# ==============================================================================
# app.R — Centralized R Shiny GUI for the AP-MS Pipeline
#
# Launch:  Rscript -e "shiny::runApp('app.R')"
#   or:    In RStudio, open this file and click "Run App"
#
# This app provides a graphical interface for run_pipeline.R and
# local_DIANN_SE.R. All scripts live in this "Home" directory —
# the user selects a target project folder and uploads data files.
# ==============================================================================

library(shiny)
library(yaml)

# Resolve HOME_DIR — the directory where app.R lives
HOME_DIR <- normalizePath(dirname(sys.frame(1)$ofile %||% "."), mustWork = FALSE)
if (!file.exists(file.path(HOME_DIR, "run_pipeline.R"))) {
    HOME_DIR <- getwd()
}

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
    # ---- Custom CSS for a polished, premium look ----
    tags$head(
        tags$link(
            href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap",
            rel = "stylesheet"
        ),
        tags$style(HTML("
      * { font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif; }
      body {
        background: linear-gradient(135deg, #0f0c29 0%, #1a1a3e 50%, #24243e 100%);
        color: #e0e0e0;
        min-height: 100vh;
        padding: 0;
        margin: 0;
      }

      /* ---- Header ---- */
      .app-header {
        background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
        padding: 28px 40px;
        margin: 0 0 30px 0;
        border-radius: 0 0 16px 16px;
        box-shadow: 0 4px 30px rgba(102, 126, 234, 0.3);
      }
      .app-header h1 {
        margin: 0;
        font-size: 28px;
        font-weight: 700;
        color: #ffffff;
        letter-spacing: -0.5px;
      }
      .app-header p {
        margin: 6px 0 0 0;
        font-size: 14px;
        color: rgba(255,255,255,0.75);
        font-weight: 400;
      }

      /* ---- Main container ---- */
      .main-container {
        max-width: 1100px;
        margin: 0 auto;
        padding: 0 24px 40px 24px;
      }

      /* ---- Glass cards ---- */
      .glass-card {
        background: rgba(255, 255, 255, 0.05);
        border: 1px solid rgba(255, 255, 255, 0.08);
        border-radius: 14px;
        padding: 24px 28px;
        margin-bottom: 20px;
        backdrop-filter: blur(12px);
        -webkit-backdrop-filter: blur(12px);
        transition: border-color 0.3s ease;
      }
      .glass-card:hover {
        border-color: rgba(102, 126, 234, 0.3);
      }
      .glass-card h3 {
        margin: 0 0 16px 0;
        font-size: 16px;
        font-weight: 600;
        color: #a78bfa;
        letter-spacing: 0.3px;
        text-transform: uppercase;
      }

      /* ---- Inputs ---- */
      .form-control, .shiny-input-container input[type='text'] {
        background: rgba(255,255,255,0.07) !important;
        border: 1px solid rgba(255,255,255,0.12) !important;
        border-radius: 8px !important;
        color: #e0e0e0 !important;
        padding: 10px 14px !important;
        font-size: 14px !important;
        transition: border-color 0.2s ease;
      }
      .form-control:focus {
        border-color: #667eea !important;
        box-shadow: 0 0 0 2px rgba(102, 126, 234, 0.15) !important;
      }
      label {
        color: #b0b0c0 !important;
        font-weight: 500 !important;
        font-size: 13px !important;
        margin-bottom: 5px !important;
      }

      /* ---- File uploads ---- */
      .shiny-input-container .input-group-btn .btn,
      .shiny-input-container .btn-file {
        background: linear-gradient(135deg, #667eea, #764ba2) !important;
        border: none !important;
        color: #fff !important;
        border-radius: 8px !important;
        font-weight: 500 !important;
        font-size: 13px !important;
        transition: transform 0.15s ease;
      }
      .shiny-input-container .input-group-btn .btn:hover {
        transform: scale(1.02);
      }
      .progress-bar {
        background: linear-gradient(90deg, #667eea, #764ba2) !important;
      }

      /* ---- Run button ---- */
      .run-btn {
        background: linear-gradient(135deg, #00c853, #00e676) !important;
        border: none !important;
        color: #0a0a0a !important;
        font-weight: 700 !important;
        font-size: 16px !important;
        padding: 14px 36px !important;
        border-radius: 10px !important;
        letter-spacing: 0.5px;
        box-shadow: 0 4px 20px rgba(0, 200, 83, 0.25);
        transition: all 0.2s ease;
        width: 100%;
        cursor: pointer;
      }
      .run-btn:hover {
        box-shadow: 0 6px 30px rgba(0, 200, 83, 0.4);
        transform: translateY(-1px);
      }
      .run-btn:disabled {
        background: rgba(255,255,255,0.1) !important;
        color: rgba(255,255,255,0.3) !important;
        box-shadow: none;
        transform: none;
        cursor: not-allowed;
      }

      /* ---- Console ---- */
      .console-panel {
        background: #0d0d1a;
        border: 1px solid rgba(255,255,255,0.06);
        border-radius: 10px;
        padding: 16px;
        font-family: 'JetBrains Mono', 'Fira Code', 'Consolas', monospace;
        font-size: 12px;
        line-height: 1.6;
        color: #a0ffa0;
        max-height: 450px;
        overflow-y: auto;
        white-space: pre-wrap;
        word-break: break-all;
      }
      .console-panel .error-line { color: #ff6b6b; }
      .console-panel .warn-line { color: #ffd93d; }
      .console-panel .info-line { color: #74b9ff; }
      .console-panel .ok-line { color: #00e676; }
      .console-panel .section-line { color: #a78bfa; font-weight: 600; }

      /* ---- Status badge ---- */
      .status-badge {
        display: inline-block;
        padding: 6px 16px;
        border-radius: 20px;
        font-size: 13px;
        font-weight: 600;
        letter-spacing: 0.3px;
      }
      .status-idle { background: rgba(255,255,255,0.08); color: #888; }
      .status-running {
        background: rgba(102, 126, 234, 0.2);
        color: #667eea;
        animation: pulse 2s ease-in-out infinite;
      }
      .status-success { background: rgba(0, 230, 118, 0.15); color: #00e676; }
      .status-error { background: rgba(255, 107, 107, 0.15); color: #ff6b6b; }

      @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.6; }
      }

      /* ---- Output buttons ---- */
      .output-btn {
        background: rgba(255,255,255,0.08) !important;
        border: 1px solid rgba(255,255,255,0.15) !important;
        color: #e0e0e0 !important;
        font-weight: 500 !important;
        border-radius: 8px !important;
        padding: 10px 20px !important;
        transition: all 0.2s ease;
        margin-right: 10px;
      }
      .output-btn:hover {
        background: rgba(102, 126, 234, 0.15) !important;
        border-color: #667eea !important;
      }

      /* ---- Upload grid ---- */
      .upload-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 16px;
      }
      .upload-slot {
        background: rgba(255,255,255,0.03);
        border: 1px dashed rgba(255,255,255,0.12);
        border-radius: 10px;
        padding: 16px;
        transition: border-color 0.2s ease;
      }
      .upload-slot:hover {
        border-color: rgba(102, 126, 234, 0.4);
      }
      .upload-slot .slot-label {
        font-size: 12px;
        color: #888;
        margin-bottom: 8px;
        font-weight: 500;
      }

      /* Scrollbar */
      ::-webkit-scrollbar { width: 6px; }
      ::-webkit-scrollbar-track { background: transparent; }
      ::-webkit-scrollbar-thumb { background: rgba(255,255,255,0.15); border-radius: 3px; }
      ::-webkit-scrollbar-thumb:hover { background: rgba(255,255,255,0.25); }
    ")),
        tags$script(HTML("
        Shiny.addCustomMessageHandler('toggle-run-btn', function(msg) {
          var btn = document.getElementById('run_btn');
          if (btn) { btn.disabled = msg.disabled; }
        });
      "))
    ),

    # ---- Header ----
    div(
        class = "app-header",
        h1("\U0001F9EC AP-MS Analysis Pipeline"),
        p("Centralized Shiny GUI for DiaNN \u2192 prolfqua \u2192 SAINTexpress")
    ),

    # ---- Main content ----
    div(
        class = "main-container",

        # ---- Target Directory ----
        div(
            class = "glass-card",
            h3("\U0001F4C1 Target Project Directory"),
            textInput("target_dir", NULL,
                placeholder = "Enter absolute path, e.g. C:/Data/MyExperiment",
                width = "100%"
            ),
            helpText("All outputs will be saved into this directory. The folder will be created if it doesn't exist.")
        ),

        # ---- File Uploads ----
        div(
            class = "glass-card",
            h3("\U0001F4E4 Input Files"),
            div(
                class = "upload-grid",

                # DiaNN Report
                div(
                    class = "upload-slot",
                    div(class = "slot-label", "DiaNN Report (.tsv)"),
                    fileInput("report_file", NULL,
                        accept = c(".tsv"),
                        placeholder = "report.tsv or *_report.tsv"
                    )
                ),

                # Dataset
                div(
                    class = "upload-slot",
                    div(class = "slot-label", "Dataset Annotation (.csv)"),
                    fileInput("dataset_file", NULL,
                        accept = c(".csv"),
                        placeholder = "*dataset*.csv"
                    )
                ),

                # Config
                div(
                    class = "upload-slot",
                    div(class = "slot-label", "Config (.yaml)"),
                    fileInput("config_file", NULL,
                        accept = c(".yaml", ".yml"),
                        placeholder = "config.yaml"
                    )
                ),

                # FASTA (multiple)
                div(
                    class = "upload-slot",
                    div(class = "slot-label", "FASTA Database (multiple OK)"),
                    fileInput("fasta_files", NULL,
                        accept = c(".fasta", ".fa"),
                        multiple = TRUE,
                        placeholder = "*.fasta, *.fa"
                    )
                )
            )
        ),

        # ---- Run Button ----
        div(
            class = "glass-card", style = "text-align: center;",
            actionButton("run_btn", "\u25B6  Run Analysis",
                class = "run-btn",
                icon = NULL
            ),
            tags$br(), tags$br(),
            uiOutput("status_ui")
        ),

        # ---- Console Output ----
        div(
            class = "glass-card",
            h3("\U0001F4DF Console Output"),
            div(
                id = "console_container",
                uiOutput("console_output", class = "console-panel")
            ),
            # Auto-scroll script
            tags$script(HTML("
        var consoleObserver = new MutationObserver(function(mutations) {
          var panel = document.querySelector('.console-panel');
          if (panel) { panel.scrollTop = panel.scrollHeight; }
        });
        setTimeout(function() {
          var target = document.getElementById('console_container');
          if (target) {
            consoleObserver.observe(target, { childList: true, subtree: true, characterData: true });
          }
        }, 1000);
      "))
        ),

        # ---- Post-completion buttons ----
        div(
            class = "glass-card",
            h3("\U0001F4C2 Results"),
            uiOutput("results_buttons")
        )
    ) # end main-container
)


# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
    # ---- Reactive values ----
    rv <- reactiveValues(
        status      = "idle", # idle | running | success | error
        console_log = character(0),
        process     = NULL,
        timer       = NULL,
        report_path = NULL
    )

    # ---- Status badge ----
    output$status_ui <- renderUI({
        badge_class <- paste0("status-badge status-", rv$status)
        label <- switch(rv$status,
            idle    = "\u23F8  Ready — upload files and click Run",
            running = "\u23F3  Pipeline running...",
            success = "\u2705  Pipeline completed successfully!",
            error   = "\u274C  Pipeline failed — check console for details"
        )
        div(class = badge_class, label)
    })

    # ---- Console rendering with syntax highlighting ----
    output$console_output <- renderUI({
        lines <- rv$console_log
        if (length(lines) == 0) {
            return(div(style = "color: #555;", "Console output will appear here..."))
        }

        # Apply color classes based on content
        colored <- lapply(lines, function(line) {
            cls <- ""
            if (grepl("^\\[ERROR\\]|Error|error|FAILED|failed", line)) {
                cls <- "error-line"
            } else if (grepl("^\\[WARN\\]|Warning|warning", line)) {
                cls <- "warn-line"
            } else if (grepl("^\\[INFO\\]|^\\[SAINTexpress\\]", line)) {
                cls <- "info-line"
            } else if (grepl("^\u2713|completed successfully|COMPLETE", line, useBytes = TRUE)) {
                cls <- "ok-line"
            } else if (grepl("^={5,}|^-{5,}", line)) cls <- "section-line"

            if (nchar(cls) > 0) {
                tags$div(class = cls, line)
            } else {
                tags$div(line)
            }
        })

        do.call(tagList, colored)
    })

    # ---- Helper: append to console ----
    log_to_console <- function(...) {
        msg <- paste0(...)
        rv$console_log <- c(rv$console_log, msg)
    }

    # ---- Validate CONTROL column and add if missing ----
    add_control_column <- function(dataset_path) {
        df <- read.csv(dataset_path, stringsAsFactors = FALSE)

        if ("CONTROL" %in% colnames(df)) {
            log_to_console("[INFO] CONTROL column already present in dataset.csv")
            return(invisible(NULL))
        }

        # Look for the grouping column
        gv_col <- NULL
        for (col in colnames(df)) {
            if (tolower(col) %in% c("grouping.var", "grouping_var", "groupingvar")) {
                gv_col <- col
                break
            }
        }

        if (is.null(gv_col)) {
            log_to_console("[WARN] No Grouping.Var column found — cannot auto-assign CONTROL column")
            return(invisible(NULL))
        }

        # Assign C to groups containing "Control" or "Beads" (case-insensitive), T otherwise
        df$CONTROL <- ifelse(
            grepl("control|beads", df[[gv_col]], ignore.case = TRUE),
            "C", "T"
        )

        log_to_console(
            "[INFO] Added CONTROL column: ",
            sum(df$CONTROL == "C"), " controls, ",
            sum(df$CONTROL == "T"), " test samples"
        )

        write.csv(df, dataset_path, row.names = FALSE)
        log_to_console("[INFO] Updated dataset.csv written to target directory")
    }

    # ---- Run Analysis ----
    observeEvent(input$run_btn, {
        # -- Validation --
        target <- trimws(input$target_dir)
        if (nchar(target) == 0) {
            showNotification("Please specify a target directory.", type = "error")
            return()
        }

        if (is.null(input$report_file)) {
            showNotification("Please upload a DiaNN report (.tsv).", type = "error")
            return()
        }
        if (is.null(input$dataset_file)) {
            showNotification("Please upload a dataset annotation (.csv).", type = "error")
            return()
        }
        if (is.null(input$config_file)) {
            showNotification("Please upload a config.yaml.", type = "error")
            return()
        }
        if (is.null(input$fasta_files)) {
            showNotification("Please upload at least one FASTA file.", type = "error")
            return()
        }

        # -- Reset state --
        rv$console_log <- character(0)
        rv$status <- "running"
        rv$report_path <- NULL

        # -- Create target directory --
        dir.create(target, showWarnings = FALSE, recursive = TRUE)
        log_to_console(strrep("=", 60))
        log_to_console("  AP-MS Pipeline — Shiny GUI")
        log_to_console(strrep("=", 60))
        log_to_console(paste0("[INFO] Target directory: ", target))
        log_to_console(paste0("[INFO] Home directory:   ", HOME_DIR))
        log_to_console(paste0("[INFO] Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
        log_to_console("")

        # -- Copy uploaded files into target directory --
        log_to_console("[INFO] Copying uploaded files to target directory...")

        # Report
        report_dest <- file.path(target, input$report_file$name)
        file.copy(input$report_file$datapath, report_dest, overwrite = TRUE)
        log_to_console(paste0("  \u2713 Report: ", input$report_file$name))

        # Dataset
        dataset_dest <- file.path(target, input$dataset_file$name)
        file.copy(input$dataset_file$datapath, dataset_dest, overwrite = TRUE)
        log_to_console(paste0("  \u2713 Dataset: ", input$dataset_file$name))

        # Config
        config_dest <- file.path(target, "config.yaml")
        file.copy(input$config_file$datapath, config_dest, overwrite = TRUE)
        log_to_console(paste0("  \u2713 Config: config.yaml"))

        # FASTA files — merge if multiple
        fasta_info <- input$fasta_files
        if (nrow(fasta_info) == 1) {
            fasta_dest <- file.path(target, fasta_info$name[1])
            file.copy(fasta_info$datapath[1], fasta_dest, overwrite = TRUE)
            log_to_console(paste0("  \u2713 FASTA: ", fasta_info$name[1]))
        } else {
            # Merge multiple FASTAs into proteins.fasta
            log_to_console(paste0("[INFO] Merging ", nrow(fasta_info), " FASTA files into proteins.fasta..."))
            merged_path <- file.path(target, "proteins.fasta")
            all_lines <- character(0)
            for (i in seq_len(nrow(fasta_info))) {
                all_lines <- c(all_lines, readLines(fasta_info$datapath[i], warn = FALSE))
                log_to_console(paste0("  + ", fasta_info$name[i]))
            }
            writeLines(all_lines, merged_path)
            n_seqs <- sum(grepl("^>", all_lines))
            log_to_console(paste0(
                "  \u2713 Merged -> proteins.fasta (",
                n_seqs, " sequences, ",
                round(file.size(merged_path) / 1e6, 1), " MB)"
            ))
        }

        # -- Verify / Add CONTROL column --
        log_to_console("")
        log_to_console("[INFO] Checking CONTROL column in dataset...")
        tryCatch(
            add_control_column(dataset_dest),
            error = function(e) {
                log_to_console(paste0("[WARN] Could not process CONTROL column: ", e$message))
            }
        )

        # -- Launch pipeline subprocess --
        log_to_console("")
        log_to_console(strrep("=", 60))
        log_to_console("  Launching pipeline subprocess...")
        log_to_console(strrep("=", 60))
        log_to_console("")

        pipeline_script <- file.path(HOME_DIR, "run_pipeline.R")
        if (!file.exists(pipeline_script)) {
            log_to_console(paste0("[ERROR] run_pipeline.R not found in: ", HOME_DIR))
            rv$status <- "error"
            return()
        }

        # Use processx for real-time output streaming
        # Falls back to system2 if processx is not available
        if (requireNamespace("processx", quietly = TRUE)) {
            tryCatch(
                {
                    rv$process <- processx::process$new(
                        command = "Rscript",
                        args    = c(pipeline_script, target),
                        stdout  = "|",
                        stderr  = "|",
                        wd      = HOME_DIR
                    )
                    log_to_console(paste0("[INFO] Pipeline started (PID: ", rv$process$get_pid(), ")"))
                },
                error = function(e) {
                    log_to_console(paste0("[ERROR] Failed to start pipeline: ", e$message))
                    rv$status <- "error"
                    return()
                }
            )
        } else {
            # Fallback: run synchronously (blocks the app, but works without processx)
            log_to_console("[WARN] processx package not installed — running synchronously (UI will freeze)")
            log_to_console("[TIP]  Install processx for real-time streaming: install.packages('processx')")
            log_to_console("")

            result <- tryCatch(
                {
                    out <- system2("Rscript",
                        args = c(pipeline_script, target),
                        stdout = TRUE, stderr = TRUE, wait = TRUE
                    )
                    list(output = out, status = attr(out, "status") %||% 0L)
                },
                error = function(e) {
                    list(output = paste0("[ERROR] ", e$message), status = 1L)
                }
            )

            for (line in result$output) {
                rv$console_log <- c(rv$console_log, line)
            }

            if (is.null(result$status) || result$status == 0) {
                rv$status <- "success"
            } else {
                rv$status <- "error"
            }

            # Check for report
            report_candidates <- list.files(target,
                pattern = "SaintExpressReport.*\\.html$",
                full.names = TRUE, recursive = TRUE
            )
            if (length(report_candidates) > 0) rv$report_path <- report_candidates[1]

            return()
        }
    })

    # ---- Poll subprocess output ----
    observe({
        invalidateLater(500, session)

        proc <- rv$process
        if (is.null(proc)) {
            return()
        }
        if (!inherits(proc, "process")) {
            return()
        }

        # Read new output
        tryCatch(
            {
                stdout_new <- proc$read_output_lines()
                stderr_new <- proc$read_error_lines()

                if (length(stdout_new) > 0) {
                    rv$console_log <- c(rv$console_log, stdout_new)
                }
                if (length(stderr_new) > 0) {
                    rv$console_log <- c(rv$console_log, stderr_new)
                }
            },
            error = function(e) {
                # Process might have finished
            }
        )

        # Check if process has finished
        if (!proc$is_alive()) {
            # Read any remaining output
            tryCatch(
                {
                    remaining_out <- proc$read_output_lines()
                    remaining_err <- proc$read_error_lines()
                    if (length(remaining_out) > 0) rv$console_log <- c(rv$console_log, remaining_out)
                    if (length(remaining_err) > 0) rv$console_log <- c(rv$console_log, remaining_err)
                },
                error = function(e) {}
            )

            exit_code <- proc$get_exit_status()

            if (!is.null(exit_code) && exit_code == 0) {
                rv$status <- "success"
                log_to_console("")
                log_to_console(strrep("=", 60))
                log_to_console("  Pipeline completed successfully!")
                log_to_console(strrep("=", 60))
            } else {
                rv$status <- "error"
                log_to_console("")
                log_to_console(paste0("[ERROR] Pipeline exited with code: ", exit_code))
            }

            # Look for the rendered report
            target <- trimws(isolate(input$target_dir))
            report_candidates <- list.files(target,
                pattern = "SaintExpressReport.*\\.html$",
                full.names = TRUE, recursive = TRUE
            )
            if (length(report_candidates) > 0) {
                rv$report_path <- report_candidates[1]
            }

            rv$process <- NULL
        }
    })

    # ---- Results buttons ----
    output$results_buttons <- renderUI({
        if (rv$status == "success") {
            target <- trimws(input$target_dir)
            btns <- tagList(
                actionButton("open_folder", "\U0001F4C2  Open Output Folder",
                    class = "output-btn",
                    onclick = paste0(
                        "Shiny.setInputValue('open_dir', '", gsub("\\\\", "/", target), "');"
                    )
                )
            )
            if (!is.null(rv$report_path)) {
                btns <- tagList(
                    btns,
                    actionButton("view_report", "\U0001F4CA  View SAINT Report",
                        class = "output-btn",
                        onclick = paste0(
                            "Shiny.setInputValue('open_report', '",
                            gsub("\\\\", "/", rv$report_path), "');"
                        )
                    )
                )
            }
            btns
        } else if (rv$status == "running") {
            div(
                style = "color: #667eea; font-size: 13px;",
                "\u23F3 Results will appear here when the pipeline completes..."
            )
        } else if (rv$status == "error") {
            div(
                style = "color: #ff6b6b; font-size: 13px;",
                "Pipeline encountered an error. Check the console output above."
            )
        } else {
            div(
                style = "color: #555; font-size: 13px;",
                "Run the analysis to see results."
            )
        }
    })

    # ---- Open folder / report handlers ----
    observeEvent(input$open_dir, {
        dir_path <- input$open_dir
        if (.Platform$OS.type == "windows") {
            shell.exec(dir_path)
        } else if (Sys.info()["sysname"] == "Darwin") {
            system2("open", dir_path)
        } else {
            system2("xdg-open", dir_path)
        }
    })

    observeEvent(input$open_report, {
        report <- input$open_report
        if (.Platform$OS.type == "windows") {
            shell.exec(report)
        } else if (Sys.info()["sysname"] == "Darwin") {
            system2("open", report)
        } else {
            system2("xdg-open", report)
        }
    })

    # ---- Disable run button while pipeline is running ----
    observe({
        disabled <- (rv$status == "running")
        session$sendCustomMessage("toggle-run-btn", list(disabled = disabled))
    })
}


# ==============================================================================
# Launch
# ==============================================================================

shinyApp(ui = ui, server = server)
