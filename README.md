# DIANN to FragPipe Converter GUI

A simple GUI application to convert DIANN output matrices (`.tsv`) into a format compatible with FragPipe-Analyst.

## Features
- **Drag & Drop** support.
- **Auto-detection** of Protein vs Peptide matrices.
- **Customizable** output filenames.
- **Modern GUI** using PyQt6.

## Installation

1.  **Install Python** (if not already installed).
2.  **Install Dependencies**:
    ```bash
    pip install -r requirements.txt
    ```

## Usage

1.  Run the application:
    ```bash
    python gui_app.py
    ```
2.  **Drag and drop** your `report.pg_matrix.tsv` (or `pr_matrix.tsv`) into the designated area, or click **Browse**.
3.  Select the **Analysis Level** (Protein or Peptide).
    - The app attempts to auto-detect this from the filename.
4.  Enter a desired **Output Filename Prefix**.
5.  Click **CONVERT**.
6.  The converted files will be saved in the same directory as the input file.

## Output Files

For a prefix of `my_experiment` and Protein analysis level:
- `my_experiment_protein.tsv`: The main data matrix formatted for FragPipe.
- `my_experiment_experiment_annotation.tsv`: The annotation file mapping files to conditions/replicates.
