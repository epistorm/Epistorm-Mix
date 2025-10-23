# Analysis of Contact Patterns Data

This folder contains the pipeline script that will conduct the full analyses for both the main text figures and supplementary text figures of the [manuscript](). It also contains individual scripts for each subsection of the analysis.

It is necessary to set the directories and run the required [pre-processing code](./analysis_prep.R) to perform any of the following individual analyses. The scripts have no other dependencies unless explicitly specified in the [pipeline](./_pipeline_analysis.R).

## Requirements

Download the analysis folder. Open the `_pipeline_analysis.R` script and change the working directory.

``` r
my.folder = "[your preferred working directory name here]"
setwd(my.folder)
```

Comment any analysis subsections after pre-processing that you are not interested in.

### Example

Run the following code:

> ## Clean the environment
> rm(list = ls())

> #### Folders and Directories ####

> ## Working directory
> my.folder = "[your preferred working directory name here]"
> setwd(my.folder)

> ## Set global directory for sourcing the code
> ## (This code is insurance in case you run any code files that change working directory)
> e = new.env()
> e$global.dir = paste0(getwd(), "/code/analysis/")
> attach(e, name = 'global.dir')

> ## Output directory
> ## create needed folders if they do not exist
> if(!dir.exists("./output")){
>   dir.create("./output")
> }
> ...
> 

## Manuscript Analysis Pipeline

### Main Text Analysis

The main text analysis includes the following scripts in this folder: `analysis_prep.R`, `tbl1_main.R`,`gam.R`, `gam_fig1.R`, `fig2.R`, and `fig3.R`.

### Supplementary Analysis

The supplementary text analysis includes the following scripts included in this folder: `tbl1_sm.R`, `tbl2_sm.R`, `gam_sm1.R`, `gam_sm2.R`, `univar_regression.R`,`by5_80.R`, `freq-m.R`, `sym-mat.R`,`inpers.R`, `inpers_barplot.R`, `unw-analysis.R`, `raw-analysis.R`, `race.R`, `race_assort.R`, `bysex.R`, `more-mat.R`, and `rel-diff.R`.

