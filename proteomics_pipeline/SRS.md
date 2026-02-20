### **SRS Module 1: Automated Data Ingestion & Formatting**

**Objective**: Parse DiaNN output, merge with sample metadata, and instantiate a prolfqua::LFQData object.

**Inputs**:

1.  report.tsv (Main DiaNN output, represented by your truncated CSV).
    
2.  dataset.csv (FGCZ metadata file).
    

**Implementation Steps**:

1.  **Metadata Merging**:
    
    *   Extract the base filename from the Relative Path column in dataset.csv.
        
    *   Perform an inner join between the DiaNN File.Name column and the extracted base filename.
        
    *   Map the Name column to sampleName and Grouping Var to the primary analytical factor (e.g., Condition).
        
2.  **Data Extraction & Column Mapping**:
    
    *   Isolate the essential quantitative columns from the DiaNN report:
        
        *   Protein.Group / Genes for protein-level identifiers.
            
        *   Precursor.Id for peptide/precursor-level identifiers.
            
        *   Precursor.Quantity (or Precursor.Normalised depending on downstream normalization choices) for intensities.
            
        *   Q.Value and PG.Q.Value for FDR filtering.
            
3.  **Object Instantiation**:
    
    *   Define the prolfqua::AnalysisTableAnnotation using the mapped columns (Sample: Name, Factor: Grouping Var, Hierarchy: Protein.Group -> Precursor.Id, Intensity: Precursor.Quantity).
        
    *   Initialize the LFQData R6 object, which will serve as the standardized input for all downstream modules.
        

### **SRS Module 2: Preprocessing & Transformation**

**Objective**: Filter, aggregate, and normalize the structured LFQData object while exposing key parameters (normalization, peptide counts) to the user via a local configuration file or command-line arguments.

**Input**: LFQData R6 object (from Module 1). **Output**: Preprocessed and optionally normalized LFQData object ready for DEA and SAINTexpress.

**Configurable Parameters (Exposed to User)**:

*   apply\_normalization: Boolean or String ("robscale", "none", "vsn").
    
*   min\_peptides\_protein: Integer (e.g., 1 for DEA, 2 for SAINTexpress).
    
*   qVal\_experiment\_threshold: Numeric (e.g., 0.01).
    
*   qVal\_individual\_threshold: Numeric (e.g., 0.05).
    
*   aggregate\_method: String (e.g., "medpolish").
    
*   impute\_missing: Boolean (model\_missing: yes).
    

**Implementation Steps**:

1.  **FDR Filtering**:
    
    *   Apply prolfqua filtering functions using qVal\_experiment\_threshold (0.01) and qVal\_individual\_threshold (0.05) to remove low-confidence precursors.
        
2.  **Peptide Count Filtering**:
    
    *   Implement a dynamic filter using min\_peptides\_protein.
        
    *   _Architecture Note_: The pipeline should branch or store multiple states here. You can run DEA with ≥ 1 peptide, but extract a separate data frame strictly filtered for ≥ 2 peptides specifically for the SAINTexpress prey.txt generation.
        
3.  **Aggregation**:
    
    *   Roll up precursor/peptide intensities to protein-level abundances using Tukey's median polish (aggregate: medpolish).
        
4.  **Normalization Toggle**:
    
    *   If apply\_normalization == "robscale", execute robust z-score transformation (prolfqua::normalize\_robscale()).
        
    *   If apply\_normalization == "none", execute a standard log2​ transformation without scaling.
        
5.  **Imputation (Context-Dependent)**:
    
    *   If model\_missing: yes is active, apply the internal prolfqua imputation strategy (typically replacing missing values with background/noise estimates) before pushing the data to the linear modeling module.
        

For QC plots: also execute CMD\_QUANT\_QC.R (which renders QC\_ProteinAbundances.Rmd and QCandSSE.Rmd

### **SRS Module 3: Differential Expression Analysis (DEA)**

**Objective**: Construct linear models and compute contrasts, mirroring the logic in prolfquapp/inst/application/CMD\_DEA.R. **Inputs**:

*   Preprocessed LFQData object (from Module 2).
    
*   config.yaml containing the contrast definitions and FDR thresholds.
    

**Implementation Steps**:

1.  **Configuration Parsing**: Read the config.yaml file (specifically the processing\_options and contrasts) to set the statistical thresholds (e.g., FDR\_threshold: 0.05, diff\_threshold: 1.0).
    
2.  **Model Definition**: Use prolfquapp::make\_DEA\_config\_R6() to instantiate the DEA configuration.
    
3.  **Contrasts Calculation**:
    
    *   Formulate the contrasts defined in the FGCZ workflow (e.g., Bait vs Control).
        
    *   Apply prolfqua linear modeling to calculate Log2 Fold Changes, p-values, and FDRs.
        
4.  **Report Generation (Optional)**: Render the DEA results through the \_Grp2Analysis\_V2.Rmd and \_DiffExpQC.Rmd templates to produce visual QC reports.
    

### **SRS Module 4: SAINTexpress Automation & ExploreDE Export**

**Objective**: Format quantitative outputs for AP-MS interaction scoring, automate SAINTexpress execution, and generate ExploreDE/ORA inputs. Reference implementation: DIANN\_SE.R.

**Inputs**:

*   LFQData object (normalized via robscale).
    
*   dataset.csv (Sample annotations).
    

**Dependencies**: prolfquapp, prolfquasaint.

**Implementation Steps**:

1.  **ExploreDE Export**:
    
    *   Execute prolfquapp::make\_SummarizedExperiment() on the DEA results.
        
    *   Save output as SummarizedExperiment.rds for direct loading into the FGCZ exploreDE shiny app.
        
2.  **SAINTexpress Data Preparation**:
    
    *   Utilize prolfquasaint::read\_DIANN\_output() to strictly filter the data for nrPeptides >= 2.
        
    *   Define parameters: SpcInt = Intensity, Transformation = none.
        
    *   Format the three standard inputs: bait.txt (Control/Test mapping), prey.txt (Protein metadata), and inter.txt (Sample vs. Prey intensities).
        
3.  **Execution & Post-Processing**:
    
    *   Invoke the SAINTexpress executable on the generated text files.
        
    *   Parse the resulting list.txt (SAINTexpress output).
        
    *   Apply significance thresholds: BFDR < 0.05 and FC > 2.0.
        
    *   Extract Uniprot IDs to generate ORA\_background.txt (all preys) and ORA\_Bait\_.txt (significant preys per bait) for STRING-db integration.
        
4.  **Report Generation**: Render SaintExpressReportMsFragger.Rmd passing the parsed results to produce the visual AP-MS QC report.