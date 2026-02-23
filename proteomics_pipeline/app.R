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
library(DT)

# Resolve HOME_DIR — the directory where app.R lives
HOME_DIR <- normalizePath(dirname(sys.frame(1)$ofile %||% "."), mustWork = FALSE)
if (!file.exists(file.path(HOME_DIR, "run_pipeline.R"))) {
    HOME_DIR <- getwd()
}

# Increase file upload limit to 5 GB (adjust the number if your files are larger)
options(shiny.maxRequestSize = 5000 * 1024^2)
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

      /* ---- Editable sample table ---- */
      .sample-table-container table.dataTable {
        background: transparent !important;
        color: #e0e0e0 !important;
        border-collapse: collapse !important;
      }
      .sample-table-container table.dataTable thead th {
        background: rgba(102, 126, 234, 0.15) !important;
        color: #a78bfa !important;
        border-bottom: 1px solid rgba(255,255,255,0.1) !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        padding: 10px 12px !important;
      }
      .sample-table-container table.dataTable tbody td {
        background: transparent !important;
        color: #e0e0e0 !important;
        border-bottom: 1px solid rgba(255,255,255,0.05) !important;
        padding: 8px 12px !important;
        font-size: 13px !important;
      }
      .sample-table-container table.dataTable tbody tr:hover td {
        background: rgba(102, 126, 234, 0.08) !important;
      }
      .sample-table-container .dataTables_wrapper .dataTables_length,
      .sample-table-container .dataTables_wrapper .dataTables_filter,
      .sample-table-container .dataTables_wrapper .dataTables_info,
      .sample-table-container .dataTables_wrapper .dataTables_paginate {
        color: #888 !important;
        font-size: 12px !important;
      }
      .sample-table-container .dataTables_wrapper .dataTables_filter input {
        background: rgba(255,255,255,0.07) !important;
        border: 1px solid rgba(255,255,255,0.12) !important;
        color: #e0e0e0 !important;
        border-radius: 6px !important;
        padding: 4px 8px !important;
      }
      .sample-table-container .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #888 !important;
        border: none !important;
      }
      .sample-table-container .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: rgba(102, 126, 234, 0.2) !important;
        color: #667eea !important;
      }

      /* ---- Parameter inputs row ---- */
      .param-row {
        display: flex;
        gap: 20px;
        margin-top: 16px;
      }
      .param-item {
        flex: 1;
        background: rgba(255,255,255,0.03);
        border: 1px solid rgba(255,255,255,0.08);
        border-radius: 10px;
        padding: 14px 16px;
      }
      .param-item label {
        color: #b0b0c0 !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        text-transform: uppercase;
        letter-spacing: 0.3px;
      }

      /* ---- Select inputs ---- */
      .selectize-input, .selectize-dropdown {
        background: rgba(255,255,255,0.07) !important;
        border: 1px solid rgba(255,255,255,0.12) !important;
        color: #e0e0e0 !important;
        border-radius: 8px !important;
      }
      .selectize-input.focus { border-color: #667eea !important; }
      .selectize-dropdown-content .option {
        color: #e0e0e0 !important;
      }
      .selectize-dropdown-content .option.active {
        background: rgba(102, 126, 234, 0.2) !important;
      }

      /* ---- Design section hint ---- */
      .design-hint {
        font-size: 12px;
        color: #888;
        margin-top: 8px;
        font-style: italic;
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

        # ---- Experimental Design ----
        div(
            class = "glass-card",
            h3("\U0001F9EA Experimental Design"),
            p(style = "color: #888; font-size: 13px; margin-bottom: 16px;",
              "Upload a dataset.csv above to populate the sample table. ",
              "Assign each sample as Control (C) or Treatment (T), and optionally exclude samples."
            ),
            div(
                class = "sample-table-container",
                DT::dataTableOutput("sample_design_table")
            ),
            div(class = "design-hint",
                uiOutput("design_summary")
            ),

            # ---- Pipeline Parameters ----
            div(
                class = "param-row",
                div(
                    class = "param-item",
                    numericInput("nr_peptides",
                        label = "Min. Peptides per Protein",
                        value = 2, min = 1, max = 10, step = 1
                    )
                ),
                div(
                    class = "param-item",
                    selectInput("contrast_direction",
                        label = "SAINTexpress Contrast Direction",
                        choices = c(
                            "Bait over Control (default)" = "bait_vs_control",
                            "Control over Bait"           = "control_vs_bait"
                        ),
                        selected = "bait_vs_control"
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
        status        = "idle", # idle | running | success | error
        console_log   = character(0),
        process       = NULL,
        timer         = NULL,
        report_path   = NULL,
        sample_design = NULL  # data.frame: Sample, Group, CONTROL, Include
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

    # ---- Reactive dataset preview: populate sample design table on upload ----
    observeEvent(input$dataset_file, {
        req(input$dataset_file)
        tryCatch({
            df <- read.csv(input$dataset_file$datapath, stringsAsFactors = FALSE)

            # Find sample name column
            sample_col <- NULL
            if ("Name" %in% colnames(df)) {
                sample_col <- "Name"
            } else if ("Relative.Path" %in% colnames(df)) {
                df$Sample <- gsub("^x|.d.zip$|.raw$", "",
                    basename(df$Relative.Path))
                sample_col <- "Sample"
            } else {
                df$Sample <- paste0("Sample_", seq_len(nrow(df)))
                sample_col <- "Sample"
            }

            # Find grouping column
            group_val <- rep("Unknown", nrow(df))
            for (col in colnames(df)) {
                if (tolower(col) %in% c("grouping.var", "grouping_var",
                    "groupingvar", "g_")) {
                    group_val <- as.character(df[[col]])
                    break
                }
            }

            # Auto-assign CONTROL based on group names
            ctrl <- ifelse(
                grepl("control|beads", group_val, ignore.case = TRUE),
                "C", "T"
            )

            # Existing CONTROL column overrides auto-detection
            if ("CONTROL" %in% colnames(df)) {
                ctrl <- as.character(df$CONTROL)
            }

            rv$sample_design <- data.frame(
                Sample  = df[[sample_col]],
                Group   = group_val,
                CONTROL = ctrl,
                Include = TRUE,
                stringsAsFactors = FALSE
            )
        }, error = function(e) {
            showNotification(
                paste0("Error reading dataset: ", e$message),
                type = "error"
            )
        })
    })

    # ---- Render editable sample design table ----
    output$sample_design_table <- DT::renderDataTable({
        req(rv$sample_design)
        DT::datatable(
            rv$sample_design,
            editable = list(
                target = "cell",
                disable = list(columns = c(0, 1))  # Sample & Group are read-only
            ),
            selection = "none",
            rownames = FALSE,
            options = list(
                pageLength = 20,
                dom = "tp",
                ordering = FALSE,
                columnDefs = list(
                    list(className = "dt-center", targets = c(2, 3))
                )
            )
        )
    })

    # ---- Handle cell edits in sample design table ----
    observeEvent(input$sample_design_table_cell_edit, {
        info <- input$sample_design_table_cell_edit
        row_idx <- info$row
        col_idx <- info$col + 1  # DT uses 0-based, R uses 1-based
        new_val <- info$value

        if (col_idx == 3) {
            # CONTROL column — validate C or T
            new_val <- toupper(trimws(new_val))
            if (!new_val %in% c("C", "T")) new_val <- "T"
            rv$sample_design[row_idx, col_idx] <- new_val
        } else if (col_idx == 4) {
            # Include column — coerce to logical
            rv$sample_design[row_idx, col_idx] <- as.logical(new_val)
        }
    })

    # ---- Design summary ----
    output$design_summary <- renderUI({
        req(rv$sample_design)
        sd <- rv$sample_design
        included <- sd[sd$Include == TRUE, ]
        n_ctrl <- sum(included$CONTROL == "C")
        n_test <- sum(included$CONTROL == "T")
        n_excl <- sum(sd$Include == FALSE)
        groups <- paste(unique(included$Group), collapse = ", ")

        parts <- paste0(
            n_ctrl, " controls, ", n_test, " treatments"
        )
        if (n_excl > 0) {
            parts <- paste0(parts, ", ", n_excl, " excluded")
        }
        parts <- paste0(parts, " | Groups: ", groups)

        div(style = "color: #a78bfa;", parts)
    })

    # ---- Apply sample design: write modified dataset.csv ----
    apply_sample_design <- function(dataset_path) {
        df <- read.csv(dataset_path, stringsAsFactors = FALSE)

        if (!is.null(rv$sample_design)) {
            sd <- rv$sample_design

            # Exclude unchecked samples
            excluded <- sd$Sample[sd$Include == FALSE]
            if (length(excluded) > 0) {
                # Match by raw file name extracted from Relative.Path
                if ("Relative.Path" %in% colnames(df)) {
                    raw_names <- gsub("^x|.d.zip$|.raw$", "",
                        basename(df$Relative.Path))
                    df <- df[!raw_names %in% excluded, , drop = FALSE]
                } else if ("Name" %in% colnames(df)) {
                    df <- df[!df$Name %in% excluded, , drop = FALSE]
                }
                log_to_console(paste0(
                    "[INFO] Excluded ", length(excluded),
                    " samples: ", paste(excluded, collapse = ", ")
                ))
            }

            # Apply CONTROL assignments from the design table
            included_sd <- sd[sd$Include == TRUE, ]
            if ("Relative.Path" %in% colnames(df)) {
                raw_names <- gsub("^x|.d.zip$|.raw$", "",
                    basename(df$Relative.Path))
                m <- match(raw_names, included_sd$Sample)
            } else if ("Name" %in% colnames(df)) {
                m <- match(df$Name, included_sd$Sample)
            } else {
                m <- seq_len(nrow(df))
            }
            ctrl_vals <- included_sd$CONTROL[m]
            ctrl_vals[is.na(ctrl_vals)] <- "T"
            df$CONTROL <- ctrl_vals

            # Ensure G_ column
            if (!"G_" %in% colnames(df)) {
                gv_col <- NULL
                for (col in colnames(df)) {
                    if (tolower(col) %in% c("grouping.var",
                        "grouping_var", "groupingvar")) {
                        gv_col <- col
                        break
                    }
                }
                if (!is.null(gv_col)) {
                    df$G_ <- df[[gv_col]]
                    log_to_console(paste0(
                        "[INFO] Created G_ column from ", gv_col
                    ))
                }
            }

            log_to_console(paste0(
                "[INFO] Applied design: ",
                sum(df$CONTROL == "C"), " controls, ",
                sum(df$CONTROL == "T"), " treatments"
            ))
        } else {
            # Fallback: auto-assign if no design table
            if (!"CONTROL" %in% colnames(df)) {
                gv_col <- NULL
                for (col in colnames(df)) {
                    if (tolower(col) %in% c("grouping.var",
                        "grouping_var", "groupingvar")) {
                        gv_col <- col
                        break
                    }
                }
                if (!is.null(gv_col)) {
                    df$CONTROL <- ifelse(
                        grepl("control|beads", df[[gv_col]],
                            ignore.case = TRUE),
                        "C", "T"
                    )
                    log_to_console(paste0(
                        "[INFO] Auto-assigned CONTROL: ",
                        sum(df$CONTROL == "C"), " controls, ",
                        sum(df$CONTROL == "T"), " treatments"
                    ))
                }
            }
        }

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

        # -- Apply sample design (CONTROL assignments + exclusions) --
        log_to_console("")
        log_to_console("[INFO] Applying experimental design to dataset...")
        tryCatch(
            apply_sample_design(dataset_dest),
            error = function(e) {
                log_to_console(paste0("[WARN] Could not apply design: ", e$message))
            }
        )

        # -- Write GUI parameters for downstream scripts --
        gui_params <- list(
            nr_peptides        = input$nr_peptides,
            contrast_direction = input$contrast_direction
        )
        gui_params_path <- file.path(target, "gui_params.yaml")
        yaml::write_yaml(gui_params, gui_params_path)
        log_to_console(paste0("[INFO] GUI parameters saved: gui_params.yaml"))
        log_to_console(paste0("  Min peptides: ", input$nr_peptides))
        log_to_console(paste0("  Contrast direction: ", input$contrast_direction))

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
