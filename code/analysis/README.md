# Analysis of Contact Patterns Data

This folder contains the pipeline script that will conduct the full analyses for both the main text figures and supplementary text figures. It also contains the scripts for each type of analysis conducted.

## Requirements

Download the analysis folder. Open the `_pipeline_analysis.R` script and change the working directory.

``` r
my.folder = "[your preferred working directory name here]"
setwd(my.folder)
```

## Analysis Pipeline

### Main Text Analysis

The main text analysis includes the following scripts included in the code folder in this analysis folder: `analysis_prep.R`, `tbl1_main.R`,`gam.R`, `gam_fig1.R`, `fig2.R`, and `fig3.R`.

### Supplementary Analysis

The supplementary text analysis includes the following scripts included in the code folder in this analysis folder: `tbl1_sm.R`, `tbl2_sm.R`, `gam_sm1.R`, `gam_sm2.R`, `univar_regression.R`,`by5_80.R`, `freq-m.R`, `sym-mat.R`,`inpers.R`, `inpers_barplot.R`, `unw-analysis.R`, `raw-analysis.R`, `race.R`, `race_assort.R`, `bysex.R`, `more-mat.R`, and `rel-diff.R`.

