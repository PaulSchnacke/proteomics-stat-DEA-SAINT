#!/usr/bin/env Rscript
# ==============================================================================
# export_exploreDE.R — Build FGCZ exploreDE-compatible SummarizedExperiment
#
# Creates a SummarizedExperiment .rds file from DiaNN data + prolfqua DEA,
# structured to match the FGCZ exploreDE Shiny app expectations.
#
# Usage:
#   Rscript export_exploreDE.R <target_directory>
#
# The target directory must contain:
#   - report.tsv or *_report.tsv   (DiaNN output)
#   - dataset.csv or *dataset*.csv (sample annotations with G_ and CONTROL)
#   - *.fasta or *.fa              (protein database)
#   - config.yaml                  (FGCZ configuration — for contrast defs)
#
# Output:
#   <target_directory>/SummarizedExperiment.rds
#
# ==============================================================================

suppressPackageStartupMessages({
  library(prolfqua)
  library(prolfquapp)
  library(dplyr)
  library(tidyr)
  library(yaml)
  library(SummarizedExperiment)
  library(S4Vectors)
})

# ---- Logging helpers ---------------------------------------------------------

log_section <- function(...) {
  msg <- paste0(...)
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("  ", msg, "\n")
  cat(strrep("=", 60), "\n")
}

log_info <- function(...) {
  message(paste0("[exploreDE] ", paste0(...)))
}

log_ok <- function(...) {
  message(paste0("  \u2713 ", paste0(...)))
}

log_warn <- function(...) {
  message(paste0("[WARN] ", paste0(...)))
}

# ---- Flexible file discovery -------------------------------------------------

discover_file <- function(dir, patterns, label, pick_largest = FALSE) {
  matches <- character(0)
  for (pat in patterns) {
    found <- list.files(dir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
    matches <- c(matches, found)
  }
  matches <- unique(matches)

  if (length(matches) == 0) {
    stop("[ERROR] No ", label, " found in: ", dir,
      "\n        Searched patterns: ", paste(patterns, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(matches) > 1 && pick_largest) {
    sizes <- file.size(matches)
    return(matches[which.max(sizes)])
  }

  return(matches[1])
}


# ==============================================================================
# PARSE ARGUMENTS
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  cat("Usage: Rscript export_exploreDE.R <target_directory>\n")
  quit(status = 1)
}

target_dir <- normalizePath(args[1], mustWork = TRUE)
setwd(target_dir)

log_section("ExploreDE Export — SummarizedExperiment Builder")
log_info("Working directory: ", target_dir)


# ==============================================================================
# STEP 1: Discover input files
# ==============================================================================
log_section("Step 1: Input Discovery")

dataset_file <- discover_file(
  target_dir,
  patterns = c("^dataset\\.csv$", "dataset.*\\.csv$"),
  label    = "dataset annotation"
)
log_ok("Dataset: ", basename(dataset_file))

report_file <- discover_file(
  target_dir,
  patterns     = c("_report\\.tsv$", "^report\\.tsv$"),
  label        = "DiaNN report",
  pick_largest = TRUE
)
log_ok("Report: ", basename(report_file))

if (file.exists(file.path(target_dir, "proteins.fasta"))) {
  fasta_file <- file.path(target_dir, "proteins.fasta")
} else {
  fasta_file <- discover_file(
    target_dir,
    patterns = c("\\.(fasta|fa)$"),
    label    = "FASTA database"
  )
}
log_ok("FASTA: ", basename(fasta_file))

config_yaml <- discover_file(
  target_dir,
  patterns = c("^config\\.yaml$"),
  label    = "config YAML"
)
log_ok("Config: ", basename(config_yaml))


# ==============================================================================
# STEP 2: Read config and extract parameters
# ==============================================================================
log_section("Step 2: Configuration")

yml <- yaml::read_yaml(config_yaml)
params <- yml$application$parameters

normalization_param <- params[["3|Normalization"]] %||% "robscale"
diff_threshold <- as.numeric(params[["4|Difference_threshold"]] %||% "1")
fdr_threshold  <- as.numeric(params[["5|FDR_threshold"]] %||% "0.05")
order_id       <- yml$job_configuration$order_id %||% "local"
workunit_id    <- yml$job_configuration$workunit_id %||% "local"

log_info("Normalization: ", normalization_param)
log_info("Diff threshold: ", diff_threshold)
log_info("FDR threshold: ", fdr_threshold)


# ==============================================================================
# STEP 3: Read annotation
# ==============================================================================
log_section("Step 3: Read Annotation")

annot <- read.csv(dataset_file)
annot <- data.frame(lapply(annot, as.character))

# Extract raw file names
annot <- annot |> dplyr::mutate(
  raw.file = gsub("^x|.d.zip$|.raw$", "", basename(Relative.Path))
)

# Ensure G_ column exists
if (!"G_" %in% colnames(annot)) {
  gv_col <- NULL
  for (col in colnames(annot)) {
    if (tolower(col) %in% c("grouping.var", "grouping_var", "groupingvar")) {
      gv_col <- col
      break
    }
  }
  if (!is.null(gv_col)) {
    annot$G_ <- annot[[gv_col]]
    log_ok("Created G_ column from ", gv_col)
  } else {
    stop("[ERROR] No Grouping.Var or G_ column found in dataset.csv", call. = FALSE)
  }
} else {
  log_ok("G_ column already present")
}

# Ensure CONTROL column exists
if (!"CONTROL" %in% colnames(annot)) {
  annot$CONTROL <- ifelse(
    grepl("control|beads", annot$G_, ignore.case = TRUE),
    "C", "T"
  )
  log_ok("Auto-assigned CONTROL column: ",
    sum(annot$CONTROL == "C"), " controls, ",
    sum(annot$CONTROL == "T"), " test samples")
} else {
  log_ok("CONTROL column already present")
}

log_ok("Loaded ", nrow(annot), " samples")


# ==============================================================================
# STEP 4: Read DiaNN data (using DEA-compatible settings: nrPeptides = 1)
# ==============================================================================
log_section("Step 4: Read DiaNN Data")

# For DEA we use nrPeptides = 1 (less strict than SAINT's nrPeptides = 2)
peptide <- prolfquasaint::read_DIANN_output(
  diann.path = report_file,
  fasta.file = fasta_file,
  nrPeptides = 1,
  q_value    = 0.01
)

log_ok("Peptide data: ", nrow(peptide), " rows, ",
  length(unique(peptide$Protein.Group)), " protein groups")


# ==============================================================================
# STEP 5: Merge annotation and build prolfqua objects
# ==============================================================================
log_section("Step 5: Build Prolfqua Objects")

annot$Relative.Path <- NULL
peptide <- dplyr::inner_join(annot, peptide, multiple = "all")
log_ok("Merged data: ", nrow(peptide), " rows")

# Set up analysis table annotation for DEA (not SAINT)
atable <- prolfqua::AnalysisTableAnnotation$new()
atable$fileName <- "raw.file"
atable$hierarchy[["protein_Id"]] <- c("Protein.Group")
atable$hierarchy[["peptide_Id"]] <- c("Stripped.Sequence")
atable$set_response("Peptide.Quantity")
atable$hierarchyDepth <- 1

# For DEA: set the grouping factor to G_
atable$factors[["G_"]] <- "G_"
atable$factorDepth <- 1

config <- prolfqua::AnalysisConfiguration$new(atable)
adata <- prolfqua::setup_analysis(peptide, config)
lfqdata <- prolfqua::LFQData$new(adata, config)
lfqdata$remove_small_intensities()

log_ok("LFQ data: ", nrow(lfqdata$data), " observations")

# Build protein annotation
prot_annot <- prolfquapp::build_protein_annot(
  lfqdata,
  peptide,
  idcol               = c("protein_Id" = "Protein.Group"),
  cleaned_protein_id  = "Protein.Group.2",
  protein_description = "fasta.header",
  exp_nr_children     = "nrPeptides",
  more_columns        = c("fasta.id", "protein_length")
)

log_ok("Protein annotations: ", nrow(prot_annot$row_annot), " proteins")


# ==============================================================================
# STEP 6: Aggregate, normalize, and run DEA
# ==============================================================================
log_section("Step 6: Aggregate, Normalize & DEA")

# Aggregate peptides to protein level
lfqdataProt <- prolfquapp::aggregate_data(lfqdata, agg_method = "medpolish")
log_ok("Protein-level data: ", nrow(lfqdataProt$data), " observations")

# Store raw protein data before normalization
lfqdata_raw <- lfqdataProt$get_copy()

# Normalize (robscale)
log_info("Applying normalization: ", normalization_param)
tr <- lfqdataProt$get_Transformer()
if (normalization_param == "robscale") {
  lfqdata_transformed <- tr$robscale()$lfq
} else if (normalization_param == "vsn") {
  lfqdata_transformed <- tr$vsn()$lfq
} else {
  lfqdata_transformed <- tr$log2()$lfq
}
log_ok("Normalization complete")

# Generate formula and contrasts
formula_str <- "normalized_abundance ~ G_"
log_info("Formula: ", formula_str)

# Determine unique groups
groups <- sort(unique(annot$G_))
control_groups <- groups[grepl("control|beads", groups, ignore.case = TRUE)]
test_groups <- setdiff(groups, control_groups)

if (length(control_groups) == 0) {
  log_warn("No control group auto-detected. Using first group as reference: ", groups[1])
  control_groups <- groups[1]
  test_groups <- groups[-1]
}

# Build contrast definitions: each test group vs each control group
contrasts_list <- list()
for (tg in test_groups) {
  for (cg in control_groups) {
    contrast_name <- paste0(tg, "_vs_", cg)
    contrast_formula <- paste0("G_", tg, " - G_", cg)
    contrasts_list[[contrast_name]] <- contrast_formula
  }
}

log_info("Contrasts: ", paste(names(contrasts_list), collapse = ", "))

# Build DEA model using prolfqua
modelFunction <- "strategy_lm"
mod <- prolfqua::build_model(
  lfqdata_transformed,
  model_strategy = modelFunction
)

log_ok("Linear model fitted")

# Compute contrasts
contr <- prolfqua::Contrasts$new(
  mod,
  contrasts = unlist(contrasts_list)
)
contr_mod <- prolfqua::ContrastsModerated$new(contr)
contrast_results <- contr_mod$get_contrasts()

log_ok("Contrasts computed: ", nrow(contrast_results), " rows total")

# Merge protein annotation into contrast results
contrast_results <- dplyr::left_join(
  contrast_results,
  prot_annot$row_annot,
  by = "protein_Id",
  multiple = "first"
)


# ==============================================================================
# STEP 7: Build wide abundance matrices (assays)
# ==============================================================================
log_section("Step 7: Build Assay Matrices")

# Get protein IDs (row names for the matrices)
raw_wide <- lfqdata_raw$to_wide(as.matrix = TRUE)
transformed_wide <- lfqdata_transformed$to_wide(as.matrix = TRUE)

raw_matrix <- raw_wide$data
transformed_matrix <- transformed_wide$data

# Use sample Name as column names (matching colData rownames)
sample_factors <- lfqdata_raw$factors()
sample_names <- sample_factors$Name
if (is.null(sample_names)) {
  sample_names <- sample_factors[[lfqdata_raw$config$table$sampleName]]
}

protein_ids <- rownames(raw_matrix)

log_ok("Raw matrix: ", nrow(raw_matrix), " proteins x ", ncol(raw_matrix), " samples")
log_ok("Transformed matrix: ", nrow(transformed_matrix), " proteins x ", ncol(transformed_matrix), " samples")


# ==============================================================================
# STEP 8: Build colData
# ==============================================================================
log_section("Step 8: Build colData")

col_data <- sample_factors
# Ensure essential columns
if (!"isotopeLabel" %in% colnames(col_data)) {
  col_data$isotopeLabel <- "light"
}

# Keep only the columns matching the blueprint
essential_cols <- c("Name", "raw.file", "G_", "CONTROL", "isotopeLabel")
available_cols <- intersect(essential_cols, colnames(col_data))
col_data <- col_data[, available_cols, drop = FALSE]

# Set rownames to sample names
rownames(col_data) <- col_data$Name

log_ok("colData: ", nrow(col_data), " samples, columns: ",
  paste(colnames(col_data), collapse = ", "))


# ==============================================================================
# STEP 9: Build elementMetadata (rowData)
# ==============================================================================
log_section("Step 9: Build elementMetadata (rowData)")

row_data_list <- list()

# Per-contrast DataFrames
unique_contrasts <- unique(contrast_results$contrast)
for (cn in unique_contrasts) {
  col_name <- paste0("constrast_", cn)
  contrast_df <- contrast_results |>
    dplyr::filter(contrast == cn) |>
    as.data.frame()

  # Ensure protein order matches the matrix row order
  contrast_df <- dplyr::left_join(
    data.frame(protein_Id = protein_ids, stringsAsFactors = FALSE),
    contrast_df,
    by = "protein_Id",
    multiple = "first"
  )

  # Ensure key columns exist (fill contrast name for all rows)
  contrast_df$contrast <- cn

  # Add CON and REV indicator columns if not present
  if (!"CON" %in% colnames(contrast_df)) {
    contrast_df$CON <- grepl("^zz|^CON", contrast_df$protein_Id, ignore.case = TRUE)
  }
  if (!"REV" %in% colnames(contrast_df)) {
    contrast_df$REV <- grepl("^REV|^rev", contrast_df$protein_Id, ignore.case = TRUE)
  }

  row_data_list[[col_name]] <- contrast_df
  log_ok(col_name, ": ", nrow(contrast_df), " rows")
}

# stats_normalized_wide — summary statistics on normalized data
log_info("Computing summary statistics ...")
st_norm <- lfqdata_transformed$get_Summariser()
stats_norm <- st_norm$stats_wide()
row_data_list[["stats_normalized_wide"]] <- dplyr::left_join(
  data.frame(protein_Id = protein_ids, stringsAsFactors = FALSE),
  as.data.frame(stats_norm),
  by = "protein_Id",
  multiple = "first"
)
log_ok("stats_normalized_wide: ", nrow(row_data_list[["stats_normalized_wide"]]), " rows")

# stats_raw_wide — summary statistics on raw data
st_raw <- lfqdata_raw$get_Summariser()
stats_raw <- st_raw$stats_wide()
stats_raw_df <- as.data.frame(stats_raw)

# Add CV columns if not present (CV = sd / mean * 100)
group_cols <- grep("^sd_", colnames(stats_raw_df), value = TRUE)
for (sd_col in group_cols) {
  grp_name <- sub("^sd_", "", sd_col)
  mean_col <- paste0("meanAbundance_", grp_name)
  cv_col <- paste0("CV_", grp_name)
  if (mean_col %in% colnames(stats_raw_df) && !cv_col %in% colnames(stats_raw_df)) {
    stats_raw_df[[cv_col]] <- stats_raw_df[[sd_col]] / stats_raw_df[[mean_col]] * 100
  }
}

row_data_list[["stats_raw_wide"]] <- dplyr::left_join(
  data.frame(protein_Id = protein_ids, stringsAsFactors = FALSE),
  stats_raw_df,
  by = "protein_Id",
  multiple = "first"
)
log_ok("stats_raw_wide: ", nrow(row_data_list[["stats_raw_wide"]]), " rows")


# ==============================================================================
# STEP 10: Build metadata
# ==============================================================================
log_section("Step 10: Build Metadata")

# Contrasts dataframe
contrasts_df <- data.frame(
  contrast_name = names(contrasts_list),
  contrast      = unlist(contrasts_list),
  stringsAsFactors = FALSE
)

# Formula dataframe
formula_df <- data.frame(
  formula = formula_str,
  stringsAsFactors = FALSE
)

# B-fabric URLs (local placeholders)
bfabric_urls <- list(
  orderURL    = paste0("local://order/", order_id),
  projectURL  = NULL,
  workunitURL = paste0("local://workunit/", workunit_id)
)

se_metadata <- list(
  bfabric_urls = bfabric_urls,
  contrasts    = contrasts_df,
  formula      = formula_df
)

log_ok("Metadata built with ", nrow(contrasts_df), " contrasts")
log_ok("Formula: ", formula_str)


# ==============================================================================
# STEP 11: Assemble SummarizedExperiment
# ==============================================================================
log_section("Step 11: Assemble SummarizedExperiment")

# Build the elementMetadata as a DataFrame of DataFrames
row_df <- S4Vectors::DataFrame(row_data_list)

# Build assay list
assay_list <- list(
  rawData         = raw_matrix,
  transformedData = transformed_matrix
)

# Construct the SummarizedExperiment
se <- SummarizedExperiment::SummarizedExperiment(
  assays       = assay_list,
  colData      = S4Vectors::DataFrame(col_data),
  rowData      = row_df,
  metadata     = se_metadata
)

# Set NAMES (protein IDs)
names(se) <- protein_ids

log_ok("SummarizedExperiment assembled:")
log_ok("  Dimensions: ", nrow(se), " proteins x ", ncol(se), " samples")
log_ok("  Assays: ", paste(names(assays(se)), collapse = ", "))
log_ok("  rowData columns: ", paste(names(rowData(se)), collapse = ", "))
log_ok("  colData columns: ", paste(names(colData(se)), collapse = ", "))
log_ok("  Metadata keys: ", paste(names(metadata(se)), collapse = ", "))


# ==============================================================================
# STEP 12: Save
# ==============================================================================
log_section("Step 12: Save")

output_path <- file.path(target_dir, "SummarizedExperiment.rds")
saveRDS(se, file = output_path)

log_ok("Saved: ", output_path)
log_ok("File size: ", round(file.size(output_path) / 1e6, 1), " MB")

log_section("ExploreDE Export Complete!")
cat("\n")
cat("  Output: SummarizedExperiment.rds\n")
cat("  Load in R:  se <- readRDS('SummarizedExperiment.rds')\n")
cat("  Use with:   exploreDE Shiny app, iSEE, OmicsViewer\n")
cat("\n")
