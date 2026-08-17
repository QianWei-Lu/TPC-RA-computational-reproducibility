# Processed transcriptomic data

This directory contains the processed expression matrices used in the manuscript.

## Files

### `GSE89408_processed_expression.csv.gz`

Training cohort:
- Platform: GPL11154
- Healthy controls (HC): 28
- Rheumatoid arthritis (RA): 152
- Total: 180 samples

### `ValidationSet1_batch_corrected_expression.csv.gz`

Integrated Validation Set 1:
- GSE55235: 10 HC + 10 RA
- GSE55457: 10 HC + 13 RA
- GSE55584: 0 HC + 10 RA
- Integrated total: 20 HC + 33 RA = **53 samples**

The three GPL96 datasets were processed, harmonized, merged, and batch-corrected before being evaluated as one external validation cohort.

### `ValidationSet2_batch_corrected_expression.csv.gz`

Integrated Validation Set 2:
- GSE77298: 7 HC + 16 RA
- GSE206848: 7 HC + 2 RA
- Integrated total: 14 HC + 18 RA = **32 samples**

## Matrix format

The deposited matrices are gene-by-sample expression tables:
- the first field of the first row is `#Group`;
- the remaining entries of the first row contain phenotype labels (`Normal` or `RA`);
- the next row begins with `Symbol` and contains GEO sample identifiers;
- subsequent rows contain gene identifiers/symbols and processed expression values.

These are the processed matrices corresponding to the analyses described in the manuscript.
## 