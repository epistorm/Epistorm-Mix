# Analysis of Contact Patterns Data

This folder contains the [pipeline](./_pipeline_analysis.R) script that will conduct the full analyses for both the main text figures and supplementary text figures of the [manuscript](). It also contains individual scripts for each subsection of the analysis.

It is necessary to set the directories and run the required [pre-processing code](./analysis_prep.R) to perform any of the following individual analyses. The scripts have no other dependencies unless explicitly specified in the pipeline.

## Requirements

Download the analysis folder. Open the `_pipeline_analysis.R` script and change the working directory.

``` r
my.folder = "[your preferred working directory name here]"
setwd(my.folder)
```

Comment out any analysis subsections after pre-processing that you are not interested in.

Example:
``` r
### To create only the contact matrices ###
## set these directories to save resulting figures and tables
output.dir = "./output/main/"
# sm.dir = "./output/supmat/" # directory for the output later used for supplementary materials
# gam.results = "./data/gam_specifications/"

### Creates additional variables and encodes existing variables in imputed data:
source(paste0(global.dir, "analysis_prep.R")) ### [Required for all the code after]

### Creates table 1 for the main text:
# source(paste0(global.dir, "tbl1_main.R"))

### Perform GAM analysis (also creates tables S10 ans S12 for SM)
# source(paste0(global.dir, "gam.R")) [no need to run if file "model_specs.txt" exists and fits the purpose]

### Create figure 1 for main text 
# source(paste0(global.dir, "gam_fig1.R"))

### Create age-specific contact matrices (figure 2 of main text)
source(paste0(global.dir, "fig2.R"))
```

## Manuscript Analysis Pipeline
The directory of the output is set to be different for the main text analysis (descriptive statistics table, GAM analysis figure, contact matrices by age and race/ethnicity) and supplementary analyses (additional descriptive statistics, univariate analysis, additional GAM analysis results, analysis of non-household contacts, frequency-based matrices, assortativity calculations, contacts by sex and by sex and age, comparisons with other countries and pre-pandemic matrices).

### Main Text Analysis

The main text analysis includes the following scripts in this folder: `analysis_prep.R`, `tbl1_main.R`,`gam.R`, `gam_fig1.R`, `fig2.R`, and `fig3.R`.

### Supplementary Analysis

The supplementary text analysis includes the following scripts included in this folder: `tbl1_sm.R`, `tbl2_sm.R`, `gam_sm1.R`, `gam_sm2.R`, `univar_regression.R`,`by5_80.R`, `freq-m.R`, `sym-mat.R`,`inpers.R`, `inpers_barplot.R`, `unw-analysis.R`, `raw-analysis.R`, `race.R`, `race_assort.R`, `bysex.R`, `more-mat.R`, and `rel-diff.R`.

