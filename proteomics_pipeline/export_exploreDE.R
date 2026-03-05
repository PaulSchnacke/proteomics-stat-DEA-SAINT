#!/usr/bin/env Rscript
# ==============================================================================
# export_exploreDE.R — Build FGCZ-compatible SummarizedExperiment
#
# Uses prolfquapp::make_SummarizedExperiment() for guaranteed schema
# compatibility with exploreDEG and iSEE.
#
# Usage:
#   Rscript export_exploreDE.R <target_directory> [output_directory]
#
# The target directory must contain:
#   - report.tsv or *_report.tsv   (DiaNN output)
#   - dataset.csv or *dataset*.csv (sample annotations with G_ and CONTROL)
#   - *.fasta or *.fa              (protein database)
#   - config.yaml                  (FGCZ configuration)
#
# If output_directory is provided, SummarizedExperiment.rds is saved there.
# Otherwise it is saved in target_directory.
#
# Output:
#   <output_directory>/SummarizedExperiment.rds
#
# ==============================================================================

suppressPackageStartupMessages({
  library(prolfqua)
  library(prolfquapp)
  library(dplyr)
  library(yaml)
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
  cat("Usage: Rscript export_exploreDE.R <target_directory> [output_directory]\n")
  quit(status = 1)
}

target_dir <- normalizePath(args[1], mustWork = TRUE)

# Optional second argument: output directory (for saving .rds separately)
if (length(args) >= 2) {
  output_dir <- normalizePath(args[2], mustWork = FALSE)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
} else {
  output_dir <- target_dir
}

setwd(target_dir)

log_section("ExploreDE Export — SummarizedExperiment Builder")
log_info("Input directory:  ", target_dir)
log_info("Output directory: ", output_dir)


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

config_yaml <- discover_file(
  target_dir,
  patterns = c("^config\\.yaml$"),
  label    = "config YAML"
)
log_ok("Config: ", basename(config_yaml))


# ==============================================================================
# STEP 2: Read config and build GRP2 configuration object
# ==============================================================================
log_section("Step 2: Configuration")

# Build a local config YAML for prolfquapp::get_config() if needed
# Use the same approach as the DEA pipeline uses
local_config_path <- file.path(output_dir, "local_config_export.yaml")

yml <- yaml::read_yaml(config_yaml)
params <- yml$application$parameters

normalization_param <- params[["3|Normalization"]]  %||%
                       params[["11|Normalization"]] %||% "robscale"
diff_threshold <- as.numeric(
  params[["4|Difference_threshold"]]  %||%
  params[["22|FCthreshold"]]          %||% "1"
)
fdr_threshold <- as.numeric(
  params[["5|FDR_threshold"]]           %||%
  params[["21|BFDRsignificance"]]       %||% "0.05"
)
order_id    <- yml$job_configuration$order_id %||% "local"
workunit_id <- yml$job_configuration$workunit_id %||% "local"

# Build a local YAML config that prolfquapp::get_config() can parse
local_cfg <- list(
  group = "G_",
  path = file.path(output_dir, "output_export"),
  zipdir_name = paste0("EXPORT_", format(Sys.Date(), "%Y%m%d"), "_O", order_id),
  prefix = "DEA",
  software = "prolfquapp.DIANN",
  project_spec = list(
    input_URL    = "",
    workunit_Id  = paste0("local_export"),
    order_Id     = as.character(order_id),
    project_name = ""
  ),
  processing_options = list(
    model          = "prolfqua",
    model_missing  = TRUE,
    interaction    = FALSE,
    nr_peptides    = 1,
    remove_decoys  = FALSE,
    remove_cont    = FALSE,
    FDR_threshold  = fdr_threshold,
    diff_threshold = diff_threshold,
    aggregate      = "medpolish",
    transform      = normalization_param
  )
)

yaml::write_yaml(local_cfg, local_config_path)
log_info("Normalization: ", normalization_param)
log_info("Diff threshold: ", diff_threshold)
log_info("FDR threshold: ", fdr_threshold)


# ==============================================================================
# STEP 3: Build GRP2 config object and read annotation
# ==============================================================================
log_section("Step 3: Build GRP2 & Read Annotation")

GRP2 <- prolfquapp::get_config(local_config_path)

# Ensure output directory exists
dir.create(GRP2$get_zipdir(), showWarnings = FALSE, recursive = TRUE)

# Read annotation using prolfquapp's standard pipeline
annotation <- dataset_file |>
  prolfquapp::read_table_data() |>
  prolfquapp::read_annotation(prefix = GRP2$group)

log_ok("Annotation loaded: ", length(annotation$contrasts), " contrasts")
log_info("Contrasts: ", paste(names(annotation$contrasts), collapse = ", "))


# ==============================================================================
# STEP 4: Preprocess data using prolfquapp pipeline
# ==============================================================================
log_section("Step 4: Preprocess Data")

prolfqua_preprocess_functions <- prolfquapp::get_procfuncs()
software <- "prolfquapp.DIANN"

procsoft <- prolfquapp::preprocess_software(
  target_dir,
  annotation,
  preprocess_functions = prolfqua_preprocess_functions[[software]],
  pattern_contaminants = GRP2$processing_options$pattern_contaminants,
  pattern_decoys = GRP2$processing_options$pattern_decoys
)

xd <- procsoft$xd
log_ok("Preprocessing complete: ", nrow(xd$lfqdata$data), " observations")


# ==============================================================================
# STEP 5: Aggregate and run DEA
# ==============================================================================
log_section("Step 5: Aggregate & DEA")

lfqdata <- prolfquapp::aggregate_data(
  xd$lfqdata,
  agg_method = GRP2$processing_options$aggregate
)
log_ok("Protein-level data: ", nrow(lfqdata$data), " observations")

grp <- prolfquapp::generate_DEA_reports2(
  lfqdata,
  GRP2,
  xd$protein_annotation,
  annotation$contrasts
)
log_ok("DEA complete")


# ==============================================================================
# STEP 6: Build SummarizedExperiment using native prolfquapp function
# ==============================================================================
log_section("Step 6: Build & Save SummarizedExperiment")

SE <- prolfquapp::make_SummarizedExperiment(grp)

log_ok("SummarizedExperiment assembled:")
log_ok("  Dimensions: ", nrow(SE), " proteins x ", ncol(SE), " samples")
log_ok("  Assays: ", paste(names(SummarizedExperiment::assays(SE)), collapse = ", "))
log_ok("  rowData columns: ", paste(names(SummarizedExperiment::rowData(SE)), collapse = ", "))
log_ok("  colData columns: ", paste(names(SummarizedExperiment::colData(SE)), collapse = ", "))
log_ok("  Metadata keys: ", paste(names(S4Vectors::metadata(SE)), collapse = ", "))


# ==============================================================================
# STEP 7: Save
# ==============================================================================
output_path <- file.path(output_dir, "SummarizedExperiment.rds")
saveRDS(SE, file = output_path)

log_ok("Saved: ", output_path)
log_ok("File size: ", round(file.size(output_path) / 1e6, 1), " MB")

# Clean up temporary config
unlink(local_config_path)
unlink(file.path(output_dir, "output_export"), recursive = TRUE)

log_section("ExploreDE Export Complete!")
cat("\n")
cat("  Output: ", output_path, "\n")
cat("  Load in R:  se <- readRDS('", output_path, "')\n", sep = "")
cat("  Use with:   iSEE, exploreDE Shiny app, OmicsViewer\n")
cat("\n")
