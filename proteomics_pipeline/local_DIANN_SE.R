#!/usr/bin/env Rscript
# ==============================================================================
# local_DIANN_SE.R — SAINTexpress Analysis (B-Fabric-Free, Cross-Platform)
#
# Local version of the FGCZ DIANN_SE.R script.
# All b-fabric download/fetch logic has been removed.
# SAINTexpress is called directly via a local binary (macOS/Windows)
# instead of prolfquasaint::runSaint().
#
# Usage:
#   Rscript local_DIANN_SE.R <target_directory>
#
# The target directory must contain:
#   - report.tsv or *_report.tsv    (DiaNN output, tab-separated)
#   - dataset.csv or *dataset*.csv  (FGCZ sample annotations)
#   - *.fasta or *.fa               (protein database)
#   - SAINTexpress-int (macOS) or SAINTexpress-int.exe (Windows)
#
# Original FGCZ author: Witold Wolski <wew@fgcz.ethz.ch>
# Compatible with prolfqua >= 3.0.0, prolfquasaint >= 0.1.0
# ==============================================================================

suppressPackageStartupMessages({
  library(prolfqua)
  library(prolfquapp)
  library(prolfquasaint)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(writexl)
})

# ---- Resolve HOME_DIR (directory where this script lives) --------------------
HOME_DIR <- tryCatch(
  {
    script_args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("--file=", script_args, value = TRUE)
    if (length(file_arg) > 0) {
      dirname(normalizePath(sub("--file=", "", file_arg[1])))
    } else if (exists("ofile", envir = sys.frame(1))) {
      dirname(normalizePath(sys.frame(1)$ofile))
    } else {
      getwd()
    }
  },
  error = function(e) getwd()
)

# ---- Logging helpers ---------------------------------------------------------

log_section <- function(...) {
  msg <- paste0(...)
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("  ", msg, "\n")
  cat(strrep("=", 60), "\n")
}

log_info <- function(...) {
  message(paste0("[SAINTexpress] ", paste0(...)))
}

log_ok <- function(...) {
  message(paste0("  \u2713 ", paste0(...)))
}

log_warn <- function(...) {
  message(paste0("[WARN] ", paste0(...)))
}

# ---- Flexible file discovery (mirrors run_pipeline.R logic) ------------------

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
    winner <- matches[which.max(sizes)]
    log_info(
      "Multiple ", label, " files found (", length(matches), "). ",
      "Picking largest: ", basename(winner),
      " (", round(max(sizes) / 1e6, 1), " MB)"
    )
    return(winner)
  }

  if (length(matches) > 1) {
    log_info("Multiple ", label, " files found. Using first: ", basename(matches[1]))
  }

  return(matches[1])
}


# ---- Parse command-line arguments --------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  cat("Usage: Rscript local_DIANN_SE.R <target_directory>\n")
  quit(status = 1)
}

target_dir <- normalizePath(args[1], mustWork = TRUE)
setwd(target_dir)

log_section("SAINTexpress Analysis — Local Mode")
log_info("Working directory: ", target_dir)
log_info("Home directory:    ", HOME_DIR)


# ==============================================================================
# LOCAL CONFIGURATION
# Replaces prolfquasaint::get_params_Bfabric() and apparams_Bfabric()
# ==============================================================================

# BFABRIC — Local stand-in (used for output naming and report metadata only)
BFABRIC <- list(
  workunitID  = "local",
  workunitURL = "",
  orderID     = "local",
  inputID     = "",
  inputURL    = "",
  datasetID   = ""
)

# REPORTDATA — Analytical parameters for SAINTexpress processing.
# ┌────────────────────────────────────────────────────────────────────────┐
# │  EDIT THESE VALUES if your experiment requires different thresholds.  │
# └────────────────────────────────────────────────────────────────────────┘
REPORTDATA <- list(
  spc            = FALSE, # Use intensity-based quantification (not spectral counts)
  FCthreshold    = 2.0, # Fold-change threshold for significance
  FDRthreshold   = 0.05, # BFDR threshold for significance
  Normalization  = "robscale", # Normalization method (applied to aggregated protein data)
  Transformation = "none", # Transformation after normalization
  nrPeptides     = 2 # <<< STRICT: minimum 2 peptides per protein for AP-MS
)

log_info("Parameters:")
log_info("  nrPeptides:     ", REPORTDATA$nrPeptides, " (strict AP-MS filter)")
log_info("  Normalization:  ", REPORTDATA$Normalization)
log_info("  Transformation: ", REPORTDATA$Transformation)
log_info("  FC threshold:   ", REPORTDATA$FCthreshold)
log_info("  BFDR threshold: ", REPORTDATA$FDRthreshold)
log_info("  spc mode:       ", REPORTDATA$spc)


# ==============================================================================
# STEP A: Discover inputs & setup
# ==============================================================================
log_section("Step A: Setup & Input Discovery")

# Discover dataset
dataset_file <- discover_file(
  target_dir,
  patterns = c("^dataset\\.csv$", "dataset.*\\.csv$"),
  label    = "dataset annotation"
)
log_ok("Dataset: ", basename(dataset_file))

# Discover report
report_file <- discover_file(
  target_dir,
  patterns     = c("_report\\.tsv$", "^report\\.tsv$"),
  label        = "DiaNN report",
  pick_largest = TRUE
)
log_ok("Report: ", basename(report_file))

# Discover FASTA (use proteins.fasta if it exists, else first .fasta/.fa)
if (file.exists(file.path(target_dir, "proteins.fasta"))) {
  fasta_file <- file.path(target_dir, "proteins.fasta")
  log_ok("FASTA: proteins.fasta (pre-merged)")
} else {
  fasta_file <- discover_file(
    target_dir,
    patterns = c("\\.(fasta|fa)$"),
    label    = "FASTA database"
  )
  log_ok("FASTA: ", basename(fasta_file))
}

# Copy SAINTexpress helper files (Rmd template, bibliography)
prolfquasaint::copy_SAINT_express(run_script = FALSE)
log_ok("SAINTexpress Rmd template and bibliography copied")

# Output directory
ZIPDIR <- paste0("C", BFABRIC$orderID, "WU", BFABRIC$workunitID)
if (dir.exists(ZIPDIR)) {
  log_info("SAINTexpress output directory exists — will overwrite: ", ZIPDIR)
}
dir.create(ZIPDIR, showWarnings = FALSE, recursive = TRUE)
log_ok("Output directory: ", ZIPDIR)

# File naming prefix
treat <- "DIANN_"


# ==============================================================================
# STEP B: Read dataset annotation
# ==============================================================================
log_section("Step B: Read Annotation")

annot <- read.csv(dataset_file)
annot <- data.frame(lapply(annot, as.character))

# Extract raw file names from the Relative.Path column
annot <- annot |> dplyr::mutate(
  raw.file = gsub("^x|.d.zip$|.raw$", "", basename(Relative.Path))
)

# Check if CONTROL column already exists — do not modify if so
if ("CONTROL" %in% colnames(annot)) {
  log_ok("CONTROL column already present in dataset.csv — using as-is")
} else {
  log_warn("No CONTROL column found in dataset.csv.")
  log_warn("The prolfquasaint::dataset_set_factors_deprecated() function will handle this.")
}

annotation <- annot
colnames(annotation) <- tolower(make.names(colnames(annotation)))

log_ok("Loaded ", nrow(annot), " samples from ", basename(dataset_file))

# Display sample groups
if ("Grouping.Var" %in% colnames(annot)) {
  log_info("Sample groups: ", paste(unique(annot$`Grouping.Var`), collapse = ", "))
} else if ("grouping.var" %in% tolower(colnames(annot))) {
  gv_col <- colnames(annot)[tolower(colnames(annot)) == "grouping.var"][1]
  log_info("Sample groups: ", paste(unique(annot[[gv_col]]), collapse = ", "))
}


# ==============================================================================
# STEP C: Read DiaNN output & FASTA
# ==============================================================================
log_section("Step C: Read DiaNN Data")

# Use get_files_DIANN to discover files (it looks for report.tsv and .fasta)
# But override with our discovered files if needed
log_info("Reading DiaNN output ...")
log_info("  Report: ", basename(report_file))
log_info("  FASTA:  ", basename(fasta_file))

log_info(
  "Applying strict nrPeptides = ", REPORTDATA$nrPeptides,
  " filter (proteins with < ", REPORTDATA$nrPeptides,
  " peptides will be EXCLUDED)"
)

peptide <- prolfquasaint::read_DIANN_output(
  diann.path = report_file,
  fasta.file = fasta_file,
  nrPeptides = REPORTDATA$nrPeptides, # <<< STRICT: min 2 peptides per protein
  q_value    = 0.01
)

log_ok(
  "Peptide-level data: ", nrow(peptide), " rows, ",
  length(unique(peptide$Protein.Group)), " protein groups"
)


# ==============================================================================
# STEP D: Merge annotation with peptide data
# ==============================================================================
log_section("Step D: Merge Annotation")

matched <- annot$raw.file[annot$raw.file %in% unique(peptide$raw.file)]
unmatched <- annot$raw.file[!annot$raw.file %in% unique(peptide$raw.file)]

if (length(unmatched) > 0) {
  log_warn(length(unmatched), " annotated files NOT found in DiaNN data:")
  for (u in unmatched) log_info("  - ", u)
}
log_ok(length(matched), " / ", nrow(annot), " annotated files matched to DiaNN data")

annot$Relative.Path <- NULL
peptide <- dplyr::inner_join(annot, peptide, multiple = "all")

log_ok("Merged data: ", nrow(peptide), " rows")


# ==============================================================================
# STEP E: Analysis configuration & protein aggregation
# ==============================================================================
log_section("Step E: Analysis Configuration & Protein Aggregation")

atable <- prolfqua::AnalysisTableAnnotation$new()
atable$fileName <- "raw.file"
atable$hierarchy[["protein_Id"]] <- c("Protein.Group")
atable$hierarchy[["peptide_Id"]] <- c("Stripped.Sequence")
atable$set_response("Peptide.Quantity")
atable$hierarchyDepth <- 1

# Set factors for SAINTexpress (Bait/Control structure)
res <- prolfquasaint::dataset_set_factors_deprecated(atable, peptide, SAINT = TRUE)
atable <- res$atable
peptide <- res$msdata

# Build LFQ data object
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

log_ok("Protein annotations built for ", nrow(prot_annot$row_annot), " proteins")

# Aggregate peptides to protein level
log_info("Aggregating peptide data to protein level (topN) ...")
lfqdataProt <- prolfquapp::aggregate_data(lfqdata, agg_method = "topN")
log_ok("Protein-level data: ", nrow(lfqdataProt$data), " observations")


# ==============================================================================
# STEP F: Normalize & transform
# ==============================================================================
log_section("Step F: Normalization & Transformation")

log_info("Applying normalization: ", REPORTDATA$Normalization)
lfqdataProt <- prolfquasaint::normalize_exp(
  lfqdataProt,
  normalization = REPORTDATA$Normalization
)

log_info("Applying transformation: ", REPORTDATA$Transformation)
lfqdataProt <- prolfquasaint::transform_force(
  lfqdataProt,
  transformation = REPORTDATA$Transformation
)

lfqdata <- lfqdataProt
log_ok("Normalization & transformation complete")

# Join protein annotation with intensity data for SAINTexpress input
intdata <- dplyr::inner_join(prot_annot$row_annot, lfqdata$data, multiple = "all")


# ==============================================================================
# STEP G: Run SAINTexpress (cross-platform binary call)
# ==============================================================================
log_section("Step G: SAINTexpress Execution (Cross-Platform)")

# ---- G.1: Prepare SAINTexpress input files -----------------------------------
log_info("Preparing SAINTexpress input files ...")

localSAINTinput <- prolfquasaint::protein_2localSaint(
  intdata,
  quantcolumn   = lfqdata$config$table$get_response(),
  proteinID     = "protein_Id",
  proteinLength = "protein_length",
  IP_name       = "raw.file",
  baitCol       = "Bait_",
  CorTCol       = "CONTROL"
)

log_info("Intensity summary for SAINTexpress input:")
print(summary(localSAINTinput$inter$exp_transformedIntensity))

# Write the 3 input files (inter.txt, prey.txt, bait.txt) exactly as runSaint does
inter_path <- file.path(target_dir, "inter.txt")
prey_path <- file.path(target_dir, "prey.txt")
bait_path <- file.path(target_dir, "bait.txt")

readr::write_tsv(localSAINTinput$inter, file = inter_path, col_names = FALSE)
readr::write_tsv(localSAINTinput$prey, file = prey_path, col_names = FALSE)
readr::write_tsv(localSAINTinput$bait, file = bait_path, col_names = FALSE)

log_ok("Written: inter.txt (", nrow(localSAINTinput$inter), " rows)")
log_ok("Written: prey.txt  (", nrow(localSAINTinput$prey), " rows)")
log_ok("Written: bait.txt  (", nrow(localSAINTinput$bait), " rows)")

# ---- G.2: Locate and execute SAINTexpress binary (cross-platform) ------------

# Determine which binary to use based on spc mode and OS
is_windows <- (.Platform$OS.type == "windows")
if (REPORTDATA$spc) {
  binary_name <- if (is_windows) "SAINTexpress-spc.exe" else "SAINTexpress-spc"
} else {
  binary_name <- if (is_windows) "SAINTexpress-int.exe" else "SAINTexpress-int"
}

# Search for the binary: HOME_DIR first (pipeline home), then target_dir
binary_candidates <- unique(c(
  file.path(HOME_DIR, binary_name),
  file.path(target_dir, binary_name)
))

saint_exe <- NULL
for (candidate in binary_candidates) {
  if (file.exists(candidate)) {
    saint_exe <- normalizePath(candidate)
    break
  }
}

if (is.null(saint_exe)) {
  stop(
    "[ERROR] SAINTexpress binary not found: ", binary_name, "\n",
    "        Searched in:\n",
    "          1. ", HOME_DIR, "\n",
    "          2. ", target_dir, "\n",
    "        For macOS, compile from source: https://sourceforge.net/projects/saint-apms/\n",
    "        For Windows, download the .exe from the same source.\n",
    "        Place the binary in the pipeline home directory.",
    call. = FALSE
  )
}

log_ok("SAINTexpress binary found: ", saint_exe)

# Ensure the binary is executable (macOS/Linux only)
if (!is_windows) {
  system2("chmod", args = c("+x", shQuote(saint_exe)), stdout = FALSE, stderr = FALSE)
}

# Execute SAINTexpress
log_info("Running: ", binary_name, " inter.txt prey.txt bait.txt")

saint_out <- system2(
  saint_exe,
  args   = c(inter_path, prey_path, bait_path),
  stdout = TRUE,
  stderr = TRUE,
  wait   = TRUE
)

cat(paste(saint_out, collapse = "\n"), "\n")
log_ok("SAINTexpress execution completed")

# ---- G.3: Read SAINTexpress output (list.txt) --------------------------------
Sys.sleep(2) # brief pause to ensure file is flushed

list_file <- file.path(target_dir, "list.txt")
if (!file.exists(list_file)) {
  stop("[ERROR] SAINTexpress did not produce list.txt. Check binary output above.",
    call. = FALSE
  )
}

saint_results <- read.csv(list_file, sep = "\t")
log_ok("SAINTexpress results: ", nrow(saint_results), " protein-bait interactions scored")

# Build the resSaint structure matching what runSaint() returns
resSaint <- list(
  listFile = data.frame(listFile = list_file),
  list     = saint_results,
  out      = data.frame(out = saint_out)
)

# Annotate SAINTexpress results with protein metadata
resSaint$list <- dplyr::inner_join(
  prot_annot$row_annot, resSaint$list,
  by = c(protein_Id = "Prey"),
  keep = TRUE, multiple = "all"
)

log_ok("Results annotated with protein metadata")


# ==============================================================================
# STEP H: Compile results & filter significant interactions
# ==============================================================================
log_section("Step H: Results Compilation")

RESULTS <- list()
RESULTS$annotation <- lfqdata$factors()
RESULTS <- c(RESULTS, resSaint)
names(localSAINTinput) <- paste("input", names(localSAINTinput), sep = "_")
RESULTS <- c(RESULTS, localSAINTinput)

# Filter significant interactions
cse <- prolfquasaint::ContrastsSAINTexpress$new(resSaint$list)
resContrasts <- cse$get_contrasts()

sig <- resContrasts |>
  dplyr::filter(
    .data$BFDR < REPORTDATA$FDRthreshold &
      .data$log2_EFCs > log2(REPORTDATA$FCthreshold)
  )

log_ok("Total interactions:       ", nrow(resContrasts))
log_ok(
  "Significant interactions: ", nrow(sig),
  " (BFDR < ", REPORTDATA$FDRthreshold,
  " & FC > ", REPORTDATA$FCthreshold, ")"
)

# Transformed data for PCA / downstream visualization
tt <- lfqdata$get_Transformer()$log2()
lfqdata_transformed <- tt$lfq

# Missing value statistics
REPORTDATA$pups <- prolfqua::UpSet_interaction_missing_stats(
  lfqdataProt$data,
  lfqdata$config,
  tr = 2
)

RESULTS$InputData <- lfqdata$to_wide()$data

gs <- lfqdata$get_Summariser()
RESULTS$MissingInformation <- gs$interaction_missing_stats()$data
RESULTS$MissingInformation$isotopeLabel <- NULL
RESULTS$listFile <- NULL


# ==============================================================================
# STEP I: Write Excel results & ORA / RNK files
# ==============================================================================
log_section("Step I: Export Files")

# Excel workbook
xlsx_path <- file.path(ZIPDIR, paste0(treat, "WU", BFABRIC$workunitID, "_data.xlsx"))
log_info("Writing Excel results ...")
writexl::write_xlsx(RESULTS, path = xlsx_path)
log_ok("Excel file: ", basename(xlsx_path))

# ORA background
log_info("Writing ORA files for STRING-db ...")
tmp <- REPORTDATA$pups$data
tmp$UniprotID <- sapply(tmp$protein_Id, function(x) strsplit(x, ";")[[1]][1])

write.table(
  data.frame(tmp$UniprotID),
  file = file.path(ZIPDIR, "ORA_background.txt"),
  col.names = FALSE, row.names = FALSE, quote = FALSE
)
log_ok("ORA_background.txt (", nrow(tmp), " proteins)")

# Per-bait ORA files
sig |>
  dplyr::group_by(Bait) |>
  tidyr::nest() -> sigg
if (nrow(sigg) > 0) {
  for (i in 1:nrow(sigg)) {
    bait_data <- sigg$data[[i]]
    bait_data$UniprotID <- sapply(bait_data$Prey, function(x) strsplit(x, ";")[[1]][1])
    filename <- paste0("ORA_Bait_", sigg$Bait[i], ".txt")
    write.table(
      data.frame(bait_data$UniprotID),
      file = file.path(ZIPDIR, filename),
      col.names = FALSE, row.names = FALSE, quote = FALSE
    )
    log_ok(filename, " (", nrow(bait_data), " significant preys)")
  }
}

# Per-bait .rnk files (for GSEA)
log_info("Writing .rnk files for GSEA ...")
resContrasts |>
  dplyr::group_by(Bait) |>
  tidyr::nest() -> resContr
if (nrow(resContr) > 0) {
  for (i in 1:nrow(resContr)) {
    bait_data <- resContr$data[[i]]
    bait_data$UniprotID <- sapply(bait_data$Prey, function(x) strsplit(x, ";")[[1]][1])
    filename <- paste0("Bait_", resContr$Bait[i], ".rnk")
    write.table(
      data.frame(bait_data$UniprotID, bait_data$log2_EFCs),
      file = file.path(ZIPDIR, filename),
      col.names = FALSE, row.names = FALSE, quote = FALSE
    )
    log_ok(filename, " (", nrow(bait_data), " proteins)")
  }
}

# Copy SAINTexpress documentation
prolfquasaint::copy_SAINTe_doc(workdir = ZIPDIR)
log_ok("SAINTexpress documentation copied to output")


# ==============================================================================
# STEP J: Render SAINTexpress HTML Report
# ==============================================================================
log_section("Step J: Report Rendering")

# Populate REPORTDATA with all objects the Rmd template needs
REPORTDATA$BFABRIC <- BFABRIC
REPORTDATA$lfqdata_transformed <- lfqdata_transformed
REPORTDATA$sig <- sig
REPORTDATA$resContrasts <- resContrasts
REPORTDATA$prot_annot <- dplyr::rename(prot_annot$row_annot, protein = protein_Id)

# Save REPORTDATA for potential re-rendering
saveRDS(REPORTDATA, file = "REPORTDATA.rds")
log_ok("REPORTDATA.rds saved")

# Locate the Rmd template
rmd_template <- "SaintExpressReportMsFragger.Rmd"

rmd_candidates <- c(
  file.path(HOME_DIR, rmd_template),
  file.path(target_dir, rmd_template),
  rmd_template
)

rmd_path <- NULL
for (candidate in rmd_candidates) {
  if (!is.na(candidate) && file.exists(candidate)) {
    rmd_path <- normalizePath(candidate)
    break
  }
}

if (is.null(rmd_path)) {
  log_warn(rmd_template, " not found — skipping report rendering.")
  log_warn("Place the template in: ", target_dir)
  log_warn("You can render manually later using the saved REPORTDATA.rds")
} else {
  log_info("Rendering report from: ", rmd_path)

  text <- paste(
    "The LC-MS data was processed using the",
    "DIA-NN software (Demichev et al. 2020).",
    "The quantification results were extracted from the DIANN main report,",
    "containing precursor (charged modified peptide) quantification results.",
    "The protein abundances were estimated by the sum of all the precursor",
    "abundances (Precursor.Quantity column) assigned to a protein."
  )

  fileNameHTML <- paste0("SaintExpressReportMsFragger_WU", BFABRIC$workunitID, ".html")

  # Clean environment before rendering
  SEP <- REPORTDATA
  rm(list = setdiff(ls(), c(
    "SEP", "REPORTDATA", "ZIPDIR", "treat",
    "BFABRIC", "target_dir", "rmd_path",
    "text", "fileNameHTML", "log_info", "log_ok"
  )))

  rmarkdown::render(
    rmd_path,
    params        = list(sep = REPORTDATA, textpreprocessing = text),
    output_format = bookdown::html_document2(),
    output_file   = file.path(target_dir, fileNameHTML)
  )

  file.copy(
    file.path(target_dir, fileNameHTML),
    file.path(ZIPDIR, fileNameHTML),
    overwrite = TRUE
  )

  log_ok("Report rendered: ", fileNameHTML)
}


# ==============================================================================
# DONE
# ==============================================================================
log_section("SAINTexpress Analysis Complete!")
log_info("Output directory: ", file.path(target_dir, ZIPDIR))
cat("\n")
cat("  Output files:\n")
cat("    ", paste0(treat, "WU", BFABRIC$workunitID, "_data.xlsx"), "  (all results)\n")
cat("    inter.txt / prey.txt / bait.txt   (SAINTexpress inputs)\n")
cat("    list.txt                          (SAINTexpress raw output)\n")
cat("    ORA_background.txt                (STRING-db background)\n")
cat("    ORA_Bait_*.txt                    (per-bait significant preys)\n")
cat("    Bait_*.rnk                        (per-bait GSEA rankings)\n")
cat("    SaintExpressReport*.html           (rendered report)\n")
cat("\n")
