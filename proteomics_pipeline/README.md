# 🧬 AP-MS Analysis Pipeline

A centralized R Shiny application for running the **Affinity Purification – Mass Spectrometry (AP-MS)** analysis pipeline. Built on top of [prolfqua](https://github.com/fgcz/prolfqua), [prolfquapp](https://github.com/fgcz/prolfquapp), and [prolfquasaint](https://github.com/fgcz/prolfquasaint).

Upload your DiaNN output, dataset annotation, config, and FASTA files through a simple web interface — the pipeline handles QC, differential expression analysis, and SAINTexpress interaction scoring automatically.

---

## ✨ Features

- **Centralized architecture** — install once, analyze any project folder
- **Drag & drop file uploads** — DiaNN report, dataset CSV, config YAML, FASTA (multiple OK)
- **Automatic FASTA merging** — multiple `.fasta` files are merged into `proteins.fasta`
- **CONTROL column auto-detection** — groups containing "Control" or "Beads" → `C`, others → `T`
- **Real-time console output** — watch pipeline progress (Setup → QC → DEA → SAINT)
- **Cross-platform** — works on macOS and Windows
- **One-click results** — open the output folder or view the SAINT HTML report directly

---

## 📋 Prerequisites

### R (≥ 4.1)

Install from [https://cran.r-project.org/](https://cran.r-project.org/)

### Required R Packages

```r
install.packages(c("shiny", "yaml", "processx", "rmarkdown", "bookdown", "writexl"))

# Bioconductor / GitHub packages
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

# Install prolfqua ecosystem (check FGCZ documentation for latest instructions)
BiocManager::install("SummarizedExperiment")
remotes::install_github("fgcz/prolfqua")
remotes::install_github("fgcz/prolfquapp")
remotes::install_github("wolski/prolfquasaint")
```

### SAINTexpress Binary

Download or compile the SAINTexpress binary and place it in the pipeline home directory:

| Platform | Binary name            | Source                                                     |
|----------|----------------------- |------------------------------------------------------------|
| macOS    | `SAINTexpress-int`     | Compile from [SourceForge](https://sourceforge.net/projects/saint-apms/) |
| Windows  | `SAINTexpress-int.exe` | Download from [SourceForge](https://sourceforge.net/projects/saint-apms/) |

---

## 🚀 Installation

```bash
# Clone the repository
git clone https://github.com/YOUR_USERNAME/proteomics_pipeline.git
cd proteomics_pipeline

# Place the SAINTexpress binary in this directory
# e.g., copy SAINTexpress-int (macOS) or SAINTexpress-int.exe (Windows) here
```

No additional installation steps needed — all R dependencies are loaded at runtime.

---

## 💻 Usage

### Launch the Shiny App

```r
# From R/RStudio
shiny::runApp("path/to/proteomics_pipeline/app.R")

# Or from the command line
cd path/to/proteomics_pipeline
Rscript -e "shiny::runApp('app.R')"
```

The app will open in your default web browser.

### Step-by-step

1. **Set Target Directory** — enter the full path to your project folder (e.g., `C:/Data/MyExperiment`). The folder is created if it doesn't exist.

2. **Upload Files**:
   - **DiaNN Report** — `report.tsv` or `*_report.tsv`
   - **Dataset** — `dataset.csv` or any `.csv` containing "dataset"
   - **Config** — `config.yaml`
   - **FASTA** — one or more `.fasta` / `.fa` files (multiple are auto-merged)

3. **Click "Run Analysis"** — the pipeline executes in a background process:
   - **Step 1: Setup** — discovers inputs, merges FASTAs, creates YAML configs
   - **Step 2: QC** — runs `CMD_QUANT_QC.R` for quality control
   - **Step 3: DEA** — runs differential expression analysis (2× with different normalization)
   - **Step 4: SAINT** — runs SAINTexpress for interaction scoring

4. **View Results** — once complete, click "Open Output Folder" or "View SAINT Report".

### Command-line Mode (no GUI)

The pipeline can also be run directly from the command line:

```bash
Rscript run_pipeline.R /path/to/project_folder
```

The project folder must contain all required input files.

---

## 📁 Expected Project Structure

After running the pipeline, your target directory will contain:

```
MyExperiment/
├── report.tsv                  # DiaNN output (uploaded)
├── dataset.csv                 # Sample annotations (uploaded)
├── config.yaml                 # FGCZ config (uploaded)
├── proteins.fasta              # Merged FASTA (auto-generated)
├── local_config_qc.yaml        # Generated QC config
├── local_config_dea_none.yaml  # Generated DEA config (no normalization)
├── local_config_dea_robscale.yaml # Generated DEA config (robscale)
├── output_qc/                  # QC reports
├── output_dea_none/            # DEA results (no normalization)
├── output_dea_robscale/        # DEA results (robscale normalization)
├── ClocalWUlocal/              # SAINTexpress results
│   ├── DIANN_WUlocal_data.xlsx
│   ├── ORA_background.txt
│   ├── ORA_Bait_*.txt
│   ├── Bait_*.rnk
│   └── SaintExpressReport*.html
├── inter.txt                   # SAINTexpress input
├── prey.txt                    # SAINTexpress input
├── bait.txt                    # SAINTexpress input
└── list.txt                    # SAINTexpress raw output
```

---

## 📄 Dataset CSV Format

The dataset annotation CSV should contain at minimum:

| Column           | Description                              |
|------------------|------------------------------------------|
| `Relative.Path`  | Path to the raw file (used for matching) |
| `Name`           | Sample display name                      |
| `Grouping.Var`   | Experimental group (e.g., BaitA, Control)|

The `CONTROL` column is **optional** — if absent, the app auto-assigns:
- Groups containing **"Control"** or **"Beads"** (case-insensitive) → `C`
- All other groups → `T`

---

## 🔧 Troubleshooting

| Issue | Solution |
|-------|----------|
| `SAINTexpress binary not found` | Place `SAINTexpress-int` (macOS) or `SAINTexpress-int.exe` (Windows) in the pipeline directory |
| `Cannot locate CMD_QUANT_QC.R` | Install `prolfquapp`: `remotes::install_github("fgcz/prolfquapp")` |
| `processx not installed` warning | Install for real-time console: `install.packages("processx")` |
| Pipeline freezes in Shiny | Ensure `processx` is installed (without it, the GUI freezes during execution) |
| `Permission denied` on macOS | Run `chmod +x SAINTexpress-int` in the pipeline directory |

---

## 📜 License

This project builds on the [FGCZ prolfqua ecosystem](https://github.com/fgcz). See individual package licenses for details.

---

## 🙏 Acknowledgments

- **Witold Wolski** (FGCZ) — original DIANN_SE.R and prolfqua/prolfquasaint packages
- **DIA-NN** — Demichev et al., 2020
- **SAINTexpress** — Teo et al., 2014
