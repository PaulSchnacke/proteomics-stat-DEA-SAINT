import pandas as pd
import re
import os

def diann_to_fragpipe_mimic(input_file, analysis_level='protein', output_prefix=None):
    """
    Converts DIA-NN matrices to FragPipe-formatted tsv files with unique sample IDs.
    """
    print(f"Processing {input_file} as {analysis_level} data...")
    df = pd.read_csv(input_file, sep='\t')
    
    # 1. Identify sample columns (anything not standard DIA-NN metadata)
    metadata_cols = [
        'Protein.Group', 'Protein.Ids', 'Protein.Names', 'Genes', 'First.Protein.Description', 
        'Precursor.Id', 'Modified.Sequence', 'Stripped.Sequence', 'Precursor.Charge', 'Q.Value', 
        'Global.Q.Value', 'Global.PG.Q.Value', 'Proteotypic', 'PEP', 'Decoy', 'Ms1.Area'
    ]
    sample_cols = [c for c in df.columns if c not in metadata_cols]
    
    # 2. Map samples and generate unique IDs
    sample_map = {}
    annotation_rows = []
    seen_names = {}

    for i, col in enumerate(sample_cols):
        # Unique Internal ID (S1, S2, etc.) - This is the "safe" ID for FragPipe Analyst
        clean_id = f"S{i+1}"
        sample_map[col] = clean_id
        
        # Heuristic: Find all letter-number pairs and pick the LAST one (usually Beads2, NosH3, etc.)
        matches = re.findall(r'([a-zA-Z]+)(\d+)', col)
        if matches:
            cond, rep = matches[-1] # Take the last match in the filename
        else:
            cond, rep = "Sample", str(i+1)
        
        # Create a display name (sample_name) and ensure it is unique
        base_name = f"{cond}_{rep}"
        if base_name in seen_names:
            seen_names[base_name] += 1
            sample_name = f"{base_name}_{seen_names[base_name]}"
        else:
            seen_names[base_name] = 1
            sample_name = base_name

        annotation_rows.append({
            'file': col,
            'sample': clean_id,         # Links to headers in matrix
            'sample_name': sample_name, # Display name in plots
            'condition': cond,
            'replicate': rep
        })
    
    # 3. Construct the mimic Matrix
    new_df = pd.DataFrame()
    if analysis_level == 'protein':
        new_df['Protein'] = df['Protein.Group']
        new_df['Protein ID'] = df['Protein.Group']
        new_df['Entry Name'] = df.get('Protein.Names', df['Protein.Group'])
        new_df['Gene'] = df.get('Genes', '')
        new_df['Description'] = df.get('First.Protein.Description', 'Unknown')
        
        # Standard dummy columns
        dummies = {
            'Protein Length': 100, 'Organism': 'Homo sapiens', 
            'Protein Existence': '1:Experimental evidence at protein level',
            'Protein Probability': 1.0, 'Top Peptide Probability': 1.0,
            'Combined Total Peptides': 1, 'Combined Spectral Count': 1,
            'Combined Unique Spectral Count': 1, 'Combined Total Spectral Count': 1,
            'Indistinguishable Proteins': ""
        }
    else:
        # Peptide Mapping
        new_df['Peptide Sequence'] = df.get('Stripped.Sequence', df.get('Precursor.Id', ''))
        new_df['Modified Sequence'] = df.get('Modified.Sequence', df.get('Precursor.Id', ''))
        new_df['Protein'] = df.get('Protein.Group', '')
        new_df['Protein ID'] = df.get('Protein.Group', '')
        new_df['Gene'] = df.get('Genes', '')
        new_df['Protein Description'] = df.get('First.Protein.Description', 'Unknown')
        dummies = {
            'Peptide Length': 10, 'Charge': 2, 'Retention': 0.0, 'Observed Mass': 0.0,
            'Calibrated Observed Mass': 0.0, 'Observed M/Z': 0.0, 'Calibrated Observed M/Z': 0.0,
            'Calculated Peptide Mass': 0.0, 'Calculated M/Z': 0.0, 'Delta Mass': 0.0,
            'Expectation': 0.0, 'Hyperscore': 0, 'Next Score': 0, 
            'PeptideProphet Probability': 1.0, 'Prev AA': 'K', 'Next AA': 'K',
            'Purity': 1.0, 'Spectral Count': 1
        }

    for k, v in dummies.items():
        new_df[k] = v

    # 4. Add Abundance Data using the unique clean_id
    for original_col, clean_id in sample_map.items():
        intensity_data = df[original_col].fillna(0).astype(float)
        new_df[f"{clean_id} Intensity"] = intensity_data
        new_df[f"{clean_id} MaxLFQ Intensity"] = intensity_data
        new_df[f"{clean_id} Spectral Count"] = 1
        if analysis_level == 'protein':
            new_df[f"{clean_id} Unique Spectral Count"] = 1
            new_df[f"{clean_id} Total Spectral Count"] = 1

    # 5. Save files
    # Determine directory and tag
    if output_prefix:
        dir_name = os.path.dirname(output_prefix)
        # If output_prefix was just a name (no dir), dirname is empty. Use input file dir.
        if not dir_name:
            dir_name = os.path.dirname(input_file)
        tag = os.path.basename(output_prefix)
    else:
        dir_name = os.path.dirname(input_file)
        tag = "converted"

    # Construct filenames matching the mimic pattern: combined_protein_<tag>.tsv
    matrix_out = os.path.join(dir_name, f"combined_{analysis_level}_{tag}.tsv")
    annot_out = os.path.join(dir_name, f"experiment_annotation_{tag}.tsv")

    # Enforce Column Order for Protein Table to match FragPipe exactly
    if analysis_level == 'protein':
        # Base columns in specific order
        base_cols = [
            'Protein', 'Protein ID', 'Entry Name', 'Gene', 'Protein Length',
            'Organism', 'Protein Existence', 'Description', 'Protein Probability',
            'Top Peptide Probability', 'Combined Total Peptides', 
            'Combined Spectral Count', 'Combined Unique Spectral Count', 
            'Combined Total Spectral Count', 'Indistinguishable Proteins'
        ]
        
        # Collect sample-specific columns in order
        sample_data_cols = []
        for clean_id in sample_map.values():
            sample_data_cols.extend([
                f"{clean_id} Intensity",
                f"{clean_id} MaxLFQ Intensity",
                f"{clean_id} Spectral Count",
                f"{clean_id} Unique Spectral Count",
                f"{clean_id} Total Spectral Count"
            ])
            
        # Reorder DataFrame
        final_cols = base_cols + sample_data_cols
        # Ensure only columns that exist are selected (in case of logic drift), 
        # but here we constructed them so they should exist.
        new_df = new_df[final_cols]
    
    # Write to CSV
    new_df.to_csv(matrix_out, sep='\t', index=False, lineterminator='\r\n')
    
    # Enforce column order for Annotation file
    annot_df = pd.DataFrame(annotation_rows)
    annot_cols = ['file', 'sample', 'sample_name', 'condition', 'replicate']
    annot_df = annot_df[annot_cols]
    annot_df.to_csv(annot_out, sep='\t', index=False, lineterminator='\r\n')
    
    print(f"Created: {matrix_out} and {annot_out}")
    return matrix_out, annot_out

if __name__ == "__main__":
    # Example Usage:
    # diann_to_fragpipe_mimic('report.pg_matrix.tsv', 'protein')
    pass