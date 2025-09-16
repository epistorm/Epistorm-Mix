# Analysis of Contact Patterns Data

This folder contains the pipeline script that will conduct the full analyses for both the main text figures and supplementary text figures. It also contains the scripts for each type of analysis conducted.

## Requirements

Download the analysis folder. Open the `_pipeline_analysis.R` script and change the working directory.

``` r
my.folder = "/your-path/Epistorm-Mix/analysis"
setwd(my.folder)
```

Create an output folder with folders for main text analyses (`main`) and supplementary text analyses (`supplementary`). Your folder names should match the output directory code in `_pipeline_analysis.R`. Within the scripts in the pipelines, additional folders will need to be created.

``` r
output.dir = "./output/main/"
sm.dir = "./output/supmat/" 
```

## Analysis Pipeline

### Main Text Analysis

The main text analysis includes the following scripts included in the code folder in this analysis folder: `analysis_prep.R`, `tbl1_main.R`,`gam.R`, `gam_fig1.R`, `fig2.R`, and `fig3.R`.

### Supplementary Analysis

The supplementary text analysis includes the following scripts included in the code folder in this analysis folder: `tbl1_sm.R`, `tbl2_sm.R`, `by5_80.R`, `freq-m.R`, `sym-mat.R`,`inpers.R`, `inpers_barplot.R`, `unw-analysis.R`, `raw-analysis.R`, `race.R`, `race_assort.R`, `bysex.R`, `more-mat.R`, and `rel-diff.R`.

## Output

All the output will be in the `main` and `supmat` folders in `output` within this directory.

To access the contact matrices directly, go to the `csv` folders in `main` and `supmat`. The matrices will be in `.csv` format and can be read as respondent group as the row and contact group as the column. Each observation is the average number of contacts for a respondent in one group with contact in one group.
