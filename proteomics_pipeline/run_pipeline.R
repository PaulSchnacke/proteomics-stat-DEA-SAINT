#!/usr/bin/env Rscript
# ==============================================================================
# run_pipeline.R — Master Orchestration Script (v2)
# Local, semi-automated FGCZ prolfqua AP-MS Pipeline
#
# Usage:
#   Rscript run_pipeline.R /path/to/target_dir
#
# Expected contents of target_dir:
#   - *report.tsv or report.tsv   (DiaNN main output, tab-separated)
#   - dataset.csv or *dataset*.csv (FGCZ sample annotation)
#   - *.fasta or *.fa              (Protein FASTA database — multiple OK)
#   - config.yaml                  (FGCZ b-fabric configuration template)
#
# Pipeline Steps:
#   1. Setup — discover inputs, merge FASTAs, create local YAML configs
#   2. QC   — run CMD_QUANT_QC.R for pre-DEA quality control
#   3. DEA  — run CMD_DEA.R twice (normalization: none, robscale)
#   4. SAINTexpress — run local_DIANN_SE.R for AP-MS interaction scoring
# ==============================================================================

suppressPackageStartupMessages(library(yaml))

# ---- Resolve HOME_DIR (directory where this script lives) --------------------
# This allows reliable source() and file discovery regardless of working directory.
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

# ---- Utility: Formatted logging ---------------------------------------------

log_section <- function(...) {
  msg <- paste0(...)
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  ", msg, "\n")
  cat(strrep("=", 70), "\n")
}

log_step <- function(...) {
  msg <- paste0(...)
  cat(strrep("-", 50), "\n")
  cat("  ", msg, "\n")
  cat(strrep("-", 50), "\n")
}

log_info <- function(...) {
  msg <- paste0(...)
  message(paste0("[INFO] ", msg))
}

log_ok <- function(...) {
  msg <- paste0(...)
  message(paste0("  \u2713 ", msg))
}

log_warn <- function(...) {
  msg <- paste0(...)
  message(paste0("[WARN] ", msg))
}

log_error <- function(...) {
  msg <- paste0(...)
  stop(paste0("[ERROR] ", msg), call. = FALSE)
}

# ---- Utility: Flexible file discovery ----------------------------------------

#' Find a file in a directory by pattern, optionally picking the largest match
discover_file <- function(dir, patterns, label, pick_largest = FALSE) {
  matches <- character(0)
  for (pat in patterns) {
    found <- list.files(dir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
    matches <- c(matches, found)
  }
  matches <- unique(matches)

  if (length(matches) == 0) {
    log_error(
      "No ", label, " found in: ", dir,
      "\n         Searched patterns: ", paste(patterns, collapse = ", ")
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
    log_info("Multiple ", label, " files found. Using first match: ", basename(matches[1]))
  }

  return(matches[1])
}

# ---- Utility: FASTA merging --------------------------------------------------

#' Scan for .fasta/.fa files and merge if multiple exist
handle_fasta_files <- function(dir) {
  merged_path <- file.path(dir, "proteins.fasta")

  # If proteins.fasta already exists, use it
  if (file.exists(merged_path)) {
    log_ok("proteins.fasta already exists — using it directly")
    return(merged_path)
  }

  # Scan for all FASTA files
  fasta_files <- list.files(dir,
    pattern = "\\.(fasta|fa)$",
    full.names = TRUE, ignore.case = TRUE
  )

  if (length(fasta_files) == 0) {
    log_error("No .fasta or .fa files found in: ", dir)
  }

  if (length(fasta_files) == 1) {
    log_ok("Single FASTA found: ", basename(fasta_files[1]))
    return(fasta_files[1])
  }

  # Multiple FASTA files — merge them
  log_info("Found ", length(fasta_files), " FASTA files — merging into proteins.fasta ...")
  for (f in fasta_files) {
    log_info("  + ", basename(f), " (", round(file.size(f) / 1e6, 1), " MB)")
  }

  all_lines <- character(0)
  for (f in fasta_files) {
    all_lines <- c(all_lines, readLines(f, warn = FALSE))
  }
  writeLines(all_lines, merged_path)

  n_seqs <- sum(grepl("^>", all_lines))
  log_ok(
    "Merged ", length(fasta_files), " files -> proteins.fasta (",
    n_seqs, " sequences, ",
    round(file.size(merged_path) / 1e6, 1), " MB)"
  )

  return(merged_path)
}

# ---- Utility: Create output directory with checkpoint ------------------------

ensure_output_dir <- function(dir_path, label) {
  if (dir.exists(dir_path)) {
    log_info(label, " output directory exists — will overwrite: ", basename(dir_path))
  }
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
}

# ---- Utility: Run external Rscript, streaming output to console --------------

run_rscript <- function(script_path, cli_args, step_label) {
  log_step(step_label)
  cmd <- paste("Rscript", shQuote(script_path), cli_args)
  log_info("Executing: ", cmd)
  cat("\n")

  # system() with intern=FALSE streams stdout/stderr to the console in real time
  exit_code <- system(cmd)

  cat("\n")
  if (exit_code != 0) {
    log_error(step_label, " — command failed with exit code ", exit_code)
  }
  log_ok(step_label, " — completed successfully")
}

# ---- Utility: Fix HTML links in DEA output -----------------------------------
# CMD_DEA.R generates index.html with href paths that mimic B-Fabric server
# structure (e.g., href="DEA_20260223_Olocal_dea_robscale/report.html").
# Since locally the HTML and its sibling files live inside the zipdir_name
# folder already, these paths are broken. This function rewrites them.

fix_html_links <- function(output_dir, label = "") {
  html_files <- list.files(output_dir,
    pattern = "\\.html$",
    full.names = TRUE, recursive = TRUE
  )

  if (length(html_files) == 0) {
    log_info("No HTML files found in ", output_dir, " — skipping link fix")
    return(invisible(NULL))
  }

  # Identify the zipdir subdirectory (the deepest directory containing HTML)
  subdirs <- list.dirs(output_dir, recursive = FALSE, full.names = FALSE)

  for (html_file in html_files) {
    lines <- readLines(html_file, warn = FALSE, encoding = "UTF-8")
    modified <- FALSE

    for (subdir in subdirs) {
      # Pattern: href="<subdir>/filename" or src="<subdir>/filename"
      # Rewrite to: href="filename" (since index.html is inside <subdir>)
      pattern <- paste0('(href|src)="', subdir, '/')
      replacement <- '\\1="'

      if (any(grepl(pattern, lines, fixed = FALSE))) {
        lines <- gsub(pattern, replacement, lines)
        modified <- TRUE
      }
    }

    # Also fix any remaining absolute BFABRIC-style paths:
    # e.g., href="/bfabric/workunit/show.html?id=..." -> "#"
    if (any(grepl('href="/bfabric/', lines, fixed = TRUE))) {
      lines <- gsub('href="/bfabric/[^"]*"', 'href="#"', lines)
      modified <- TRUE
    }

    if (modified) {
      writeLines(lines, html_file, useBytes = TRUE)
      log_ok("Fixed links in: ", basename(html_file), " (", label, ")")
    }
  }
}


# ---- Parse command-line arguments --------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  cat("Usage: Rscript run_pipeline.R <target_directory>\n\n")
  cat("The target directory should contain:\n")
  cat("  - *report.tsv or report.tsv   (DiaNN output, tab-separated)\n")
  cat("  - dataset.csv or *dataset*.csv (sample annotations)\n")
  cat("  - *.fasta or *.fa              (protein database; multiple OK)\n")
  cat("  - config.yaml                  (FGCZ configuration)\n")
  quit(status = 1)
}

target_dir <- normalizePath(args[1], mustWork = TRUE)

log_section("AP-MS Pipeline Starting")
log_info("Target directory: ", target_dir)
log_info("Timestamp:        ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))


# ==============================================================================
# STEP 1: SETUP — Discover inputs, merge FASTAs, create local configs
# ==============================================================================
log_section("STEP 1: SETUP")

# ---- 1a. Flexible input discovery --------------------------------------------
log_info("Discovering input files ...")

# Report: *_report.tsv or report.tsv — pick largest if multiple
report_tsv <- discover_file(
  target_dir,
  patterns     = c("_report\\.tsv$", "^report\\.tsv$"),
  label        = "DiaNN report",
  pick_largest = TRUE
)
log_ok("Report: ", basename(report_tsv))

# Dataset: dataset.csv or *dataset*.csv
dataset_csv <- discover_file(
  target_dir,
  patterns = c("^dataset\\.csv$", "dataset.*\\.csv$"),
  label    = "dataset annotation"
)
log_ok("Dataset: ", basename(dataset_csv))

# ---- 1a.1 Ensure G_ column exists in dataset.csv ----------------------------
# prolfquapp expects a grouping column named "G_" (set in build_local_config).
# If the dataset has Grouping.Var but no G_, create it.
log_info("Checking for G_ column in dataset.csv ...")
ds <- read.csv(dataset_csv, stringsAsFactors = FALSE)

if (!"G_" %in% colnames(ds)) {
  # Find the Grouping.Var column (case-insensitive, flexible naming)
  gv_col <- NULL
  for (col in colnames(ds)) {
    if (tolower(col) %in% c("grouping.var", "grouping_var", "groupingvar")) {
      gv_col <- col
      break
    }
  }

  if (!is.null(gv_col)) {
    ds$G_ <- ds[[gv_col]]
    write.csv(ds, dataset_csv, row.names = FALSE)
    log_ok("Created G_ column from ", gv_col, " (", length(unique(ds$G_)), " groups)")
  } else {
    log_warn("No Grouping.Var column found — G_ column NOT created. PCA/Volcano plots may be missing.")
  }
} else {
  log_ok("G_ column already present in dataset.csv")
}

# Config: config.yaml
config_yaml <- discover_file(
  target_dir,
  patterns = c("^config\\.yaml$"),
  label    = "config YAML"
)
log_ok("Config: ", basename(config_yaml))

# ---- 1b. FASTA handling (auto-merge if multiple) ----------------------------
log_info("Handling FASTA files ...")
fasta_file <- handle_fasta_files(target_dir)

# ---- 1c. Locate FGCZ package scripts ----------------------------------------
log_info("Locating prolfquapp package scripts ...")

cmd_qc <- system.file("application/CMD_QUANT_QC.R", package = "prolfquapp")
cmd_dea <- system.file("application/CMD_DEA.R", package = "prolfquapp")

if (nchar(cmd_qc) == 0) log_error("Cannot locate CMD_QUANT_QC.R — is prolfquapp installed?")
if (nchar(cmd_dea) == 0) log_error("Cannot locate CMD_DEA.R — is prolfquapp installed?")
log_ok("CMD_QUANT_QC.R: ", cmd_qc)
log_ok("CMD_DEA.R:      ", cmd_dea)

# ---- 1d. Read original config.yaml and extract parameters --------------------
log_info("Reading config.yaml ...")

yml <- yaml::read_yaml(config_yaml)
params <- yml$application$parameters

normalization_param <- params[["3|Normalization"]] %||% "robscale"
diff_threshold <- as.numeric(params[["4|Difference_threshold"]] %||% "1")
fdr_threshold <- as.numeric(params[["5|FDR_threshold"]] %||% "0.05")
order_id <- yml$job_configuration$order_id %||% "local"

log_info("Extracted parameters:")
log_info("  Normalization:        ", normalization_param)
log_info("  Difference threshold: ", diff_threshold)
log_info("  FDR threshold:        ", fdr_threshold)
log_info("  Order ID:             ", order_id)

# ---- 1d.1 Override with GUI parameters if gui_params.yaml exists ------------
gui_nr_peptides <- 1  # default for DEA
gui_params_file <- file.path(target_dir, "gui_params.yaml")
if (file.exists(gui_params_file)) {
  gui_p <- yaml::read_yaml(gui_params_file)
  if (!is.null(gui_p$nr_peptides)) {
    gui_nr_peptides <- as.integer(gui_p$nr_peptides)
    log_info("  [GUI] nr_peptides overridden to: ", gui_nr_peptides)
  }
}

# ---- 1e. Generate local YAML configs ----------------------------------------
log_info("Generating local YAML configuration files ...")

build_local_config <- function(transform_method, workunit_suffix) {
  list(
    group = "G_",
    path = file.path(target_dir, paste0("output_", workunit_suffix)),
    zipdir_name = paste0(
      "DEA_", format(Sys.Date(), "%Y%m%d"),
      "_O", order_id, "_", workunit_suffix
    ),
    prefix = "DEA",
    software = "prolfquapp.DIANN",
    project_spec = list(
      input_URL    = "",
      workunit_Id  = paste0("local_", workunit_suffix),
      order_Id     = as.character(order_id),
      project_name = ""
    ),
    processing_options = list(
      model          = "prolfqua",
      model_missing  = TRUE,
      interaction    = FALSE,
      nr_peptides    = gui_nr_peptides,
      remove_decoys  = FALSE,
      remove_cont    = FALSE,
      FDR_threshold  = fdr_threshold,
      diff_threshold = diff_threshold,
      aggregate      = "medpolish",
      transform      = transform_method
    )
  )
}

# QC config
qc_config_path <- file.path(target_dir, "local_config_qc.yaml")
yaml::write_yaml(build_local_config("robscale", "qc"), qc_config_path)
log_ok("Created: ", basename(qc_config_path))

# DEA config — normalization: none
dea_none_config_path <- file.path(target_dir, "local_config_dea_none.yaml")
yaml::write_yaml(build_local_config("none", "dea_none"), dea_none_config_path)
log_ok("Created: ", basename(dea_none_config_path))

# DEA config — normalization: robscale
dea_robscale_config_path <- file.path(target_dir, "local_config_dea_robscale.yaml")
yaml::write_yaml(build_local_config("robscale", "dea_robscale"), dea_robscale_config_path)
log_ok("Created: ", basename(dea_robscale_config_path))

log_info("STEP 1 COMPLETE")


# ==============================================================================
# STEP 2: QC — Pre-DEA Quality Control
# ==============================================================================
log_section("STEP 2: QC (CMD_QUANT_QC.R)")

qc_outdir <- file.path(target_dir, "output_qc")
ensure_output_dir(qc_outdir, "QC")

qc_args <- paste(
  "-i", shQuote(target_dir),
  "-d", shQuote(dataset_csv),
  "-y", shQuote(qc_config_path),
  "-s DIANN",
  "-w local_qc",
  "-o", shQuote(qc_outdir)
)

run_rscript(cmd_qc, qc_args, "STEP 2: QC")
log_info("STEP 2 COMPLETE")


# ==============================================================================
# STEP 3: DEA — Differential Expression Analysis (Dual Run)
# ==============================================================================
log_section("STEP 3: DEA (CMD_DEA.R) — Dual Run")

# ---- 3a. DEA Run 1: transform = none ----------------------------------------
log_info("DEA Run 1: Normalization = NONE")

dea_none_outdir <- file.path(target_dir, "output_dea_none")
ensure_output_dir(dea_none_outdir, "DEA (none)")

dea_none_args <- paste(
  shQuote(dea_none_config_path),
  "-i", shQuote(target_dir),
  "-d", shQuote(dataset_csv),
  "-s prolfquapp.DIANN",
  "-w local_dea_none",
  "-o", shQuote(dea_none_outdir)
)

run_rscript(cmd_dea, dea_none_args, "STEP 3a: DEA (none)")
fix_html_links(dea_none_outdir, "DEA none")

# ---- 3b. DEA Run 2: transform = robscale ------------------------------------
log_info("DEA Run 2: Normalization = ROBSCALE")

dea_robscale_outdir <- file.path(target_dir, "output_dea_robscale")
ensure_output_dir(dea_robscale_outdir, "DEA (robscale)")

dea_robscale_args <- paste(
  shQuote(dea_robscale_config_path),
  "-i", shQuote(target_dir),
  "-d", shQuote(dataset_csv),
  "-s prolfquapp.DIANN",
  "-w local_dea_robscale",
  "-o", shQuote(dea_robscale_outdir)
)

run_rscript(cmd_dea, dea_robscale_args, "STEP 3b: DEA (robscale)")
fix_html_links(dea_robscale_outdir, "DEA robscale")

log_info("STEP 3 COMPLETE")


# ==============================================================================
# STEP 3c: ExploreDE Export — SummarizedExperiment .rds
# ==============================================================================
log_section("STEP 3c: ExploreDE Export (export_exploreDE.R)")

export_script <- file.path(HOME_DIR, "export_exploreDE.R")
if (!file.exists(export_script)) {
  for (candidate in c(file.path(target_dir, "export_exploreDE.R"), "export_exploreDE.R")) {
    if (file.exists(candidate)) {
      export_script <- normalizePath(candidate)
      break
    }
  }
}

if (!file.exists(export_script)) {
  log_warn("export_exploreDE.R not found — skipping SummarizedExperiment export.")
  log_warn("Place it next to run_pipeline.R to enable exploreDE .rds generation.")
} else {
  log_ok("Found: ", export_script)
  export_args <- shQuote(target_dir)
  run_rscript(export_script, export_args, "STEP 3c: ExploreDE Export")
}

log_info("STEP 3c COMPLETE")


# ==============================================================================
# STEP 4: SAINTexpress — AP-MS Interaction Scoring
# ==============================================================================
log_section("STEP 4: SAINTexpress (local_DIANN_SE.R)")

# Locate local_DIANN_SE.R using HOME_DIR (primary) then fallbacks
local_se_script <- file.path(HOME_DIR, "local_DIANN_SE.R")
if (!file.exists(local_se_script)) {
  # Fallback: check target directory and working directory
  for (candidate in c(file.path(target_dir, "local_DIANN_SE.R"), "local_DIANN_SE.R")) {
    if (file.exists(candidate)) {
      local_se_script <- normalizePath(candidate)
      break
    }
  }
}

if (!file.exists(local_se_script)) {
  log_error(
    "Cannot find local_DIANN_SE.R. ",
    "Place it next to run_pipeline.R or in the target directory."
  )
}
log_ok("Found: ", local_se_script)

saint_args <- shQuote(target_dir)
run_rscript(local_se_script, saint_args, "STEP 4: SAINTexpress")

log_info("STEP 4 COMPLETE")


# ==============================================================================
# PIPELINE COMPLETE
# ==============================================================================
log_section("AP-MS Pipeline — All Steps Completed!")
log_info("All output is in: ", target_dir)
cat("\n")
cat("  Output directories:\n")
cat("    output_qc/                    QC reports (protein abundances, sample size)\n")
cat("    output_dea_none/              DEA results (no normalization)\n")
cat("    output_dea_robscale/          DEA results (robscale normalization)\n")
cat("    results_SAINT/                SAINTexpress results & reports\n")
cat("    SummarizedExperiment.rds      ExploreDE-compatible data object\n")
cat("\n")

