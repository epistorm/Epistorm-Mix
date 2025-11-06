# analysis pipeline
## Clean the environment
rm(list = ls())

#### Folders and Directories ####

## Working directory
my.folder = "[your preferred working directory name here]"
setwd(my.folder)

## Set global directory for sourcing the code
## (This code is insurance in case you run any code files that change working directory)
e = new.env()
e$global.dir = paste0(getwd(), "/code/analysis/")
attach(e, name = 'global.dir')

## Output directory
## create needed folders if they do not exist
if(!dir.exists("./output")){
  dir.create("./output")
}

if(!dir.exists("./output/main")){
  dir.create("./output/main") # all the main text figures
  dir.create("./output/main/csv") # used in multiple scripts
} 

if (!dir.exists("./output/supmat")){
  dir.create("./output/supmat") # all the supplementary material figures
  dir.create("./output/supmat/csv") # used in multiple scripts
  dir.create("./output/supmat/csv/by5") # used in by5_80.R
  dir.create("./output/supmat/csv/freq") # used in freq-m.R
  dir.create("./output/supmat/csv/sym") # used in sym-mat.R
  dir.create("./output/supmat/csv/compare") # used in inpers.R
  dir.create("./output/supmat/csv/difference-matrices") # used in matrix-comp.R
  dir.create("./output/supmat/csv/unw") # used in unw-analysis.R
  dir.create("./output/supmat/csv/raw") # used in raw-analysis.R
  dir.create("./output/supmat/csv/race") # used in race.R
  dir.create("./output/supmat/csv/sex") # used in bysex.R
  dir.create("./output/supmat/gam") # used in multiple scripts
}

## set these directories to save resulting figures and tables
output.dir = "./output/main/"
sm.dir = "./output/supmat/" # directory for the output later used for supplementary materials

#### Required Preprocessing ####

### Creates additional variables and encodes existing variables in imputed data:
##    Installs the following packages: "dplyr", "survey", "tidyverse", "ggplot2","flextable", "gt", "gtsummary",
##                                      "fitdistrplus", "mgcv", "sjPlot", "patchwork", "doParallel", "extrafont",
##                                      "readxl", "data.table", "tidyr", "psych", "Hmisc", "prettyGraphs"
##    Requires the following folders in working directory: "data/imputed_data", "data/external_data"
##    Requires the following code in global.dir: func_analysis.R;
##    Requires the following data:
##        imputed-contacts2.csv and imputed-resp2.csv in "imputed_data" folder
##        federal-state-holidays.csv in "external_data" folder
source(paste0(global.dir, "analysis_prep.R")) ### [Required for all the code after]

#### Analysis ####

### Creates table 1 for the main text:
##    Requires the following folders in working directory: "output/main", "output/main/csv" 
##                                                        (created in lines 20:40 of this pipeline)
##    Saves table in the following folders:
##        table1_main.docx in "main" folder
##        table1_main.csv in "main/csv" folder
source(paste0(global.dir, "tbl1_main.R"))

### Perform GAM analysis (also creates tables S10 ans S12 for SM)
##    Requires the following folders in working directory: "output/supmat"
##    Sources the data preparation code from "gam_data_prep.R" in global.dir
##    Saves best specifications in file "model_specs.txt" in sm.dir
##    Saves tables in the following folders:
##        multivariate_gam_bf.docx in "supmat" folder
# source(paste0(global.dir, "gam.R")) [no need to run if file "model_specs.txt" exists and fits the purpose]

### Create figure 1 for main text 
##    Uses best fit specification of the gam model (from "./data/gam_specifications/model_specs.txt") 
##      to plot relative ratios for total contacts
##    Sources the data preparation code from "gam_data_prep.R"
##    Saves figure in the following folder:
##         1_effects_figure_total.pdf in "main" folder
source(paste0(global.dir, "gam_fig1.R"))

### Create age-specific contact matrices (figure 2 of main text)
##    Requires the following folders in working directory: "output/main", "output/main/csv"
##    Saves matrices in the following folders:
##        X-pred-matrix.csv in "main/csv" folder (X = Setting)
##        2_age_X.pdf in "main" folder (X = Setting)
source(paste0(global.dir, "fig2.R"))

### Create frequency-based correlation contact matrices (figure 3 of main text)
##    Requires the following folders in working directory: "matrices/M_matrix", 
##        "matrices/F_matrix", "data/external_data", "data/external_data/mistry-data"
##    Requires the following data:
##        multiple csv and xlsx files in "data/external_data", "data/external_data/mistry-data",
##        "matrices/M_matrix" and "matrices/F_matrix" folders
##    Saves plots in the following folders:
##        fig3-correlations.pdf in "main" folder (X = Setting)
source(paste0(global.dir, "fig3-corr.R")) 

### Change output directory for supplementary material 
output.dir = sm.dir

### Create table S6: total contacts for respondent characteristics
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv"
##    Saves table in the following folders:
##        table1_sm.docx in "supmat" folder
##        table1_sm.csv in "supmat/csv" folder
source(paste0(global.dir, "tbl1_sm.R"))

### Creates tables for univariable regressions
##    Requires the following folders in working directory: "output/supmat"
##    Saves output in the following folders:
##        indep-glm.csv, factor_corr.csv, and agediff_by_levels.csv in "supmat" folder
##        resp_age_insightnet_by_X.pdf in "supmat" folder
source(paste0(global.dir, "univar-regressions.R"))

### Creates figure S1: marginal effects of age for total contacts
##    Sources the data preparation code from "gam_data_prep.R" in global.dir
##    Requires the following folders in working directory: "output/supmat/gam", "data/gam_specifications"
##    Requires the following data:
##        model_specs.txt in "gam_specifications" folder
##    Saves output in the following folders:
##        age_me_total.pdf in "supmat/gam" folder
source(paste0(global.dir, "gam_sm1.R"))

### Create Table S11: distribution of respondent characteristics for all contact settings
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv"
##    Saves table in the following folders:
##        table2_sm.docx in "supmat" folder
##        table2_sm.csv in "supmat/csv" folder
source(paste0(global.dir, "tbl2_sm.R"))

### Create figure S2: relative ratios + marginal effects of age for non-household contacts
##    Sources the data preparartion code from "gam_data_prep.R" in global.dir
##    Requires the following folders in working directory: "output/supmat/gam", "data/gam_specifications"
##    Requires the following data:
##        model_specs.txt in "gam_specifications" folder
##    Saves output in the following folders:
##        effects_figure_nonhh.pdf, age_me_nonhh.pdf, and combined_gam_figure_nonhh.pdf in "supmat/gam" folder
source(paste0(global.dir, "gam_sm2.R"))

### Creates age-specific contact matrices and plots for age groups and an assortativity plot
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/by5"
##    Saves matrices and plots in the following folders:
##        X-Y-matrix-cond.csv in "supmat/csv/by5" folder (X = Setting, Y = Age group)
##        5_Y-Conditional-X.pdf in "supmat" folder (X = Setting, Y = Age group)
##        X-Y-matrix-unc.csv and X-Y-matrix-df.csv in "supmat/csv/by5" folder (X = Setting, Y = Age group)
##        2_Y-Unconditional-X.pdf in "supmat" folder (X = Setting, Y = Age group)
##        3_Y-Summary-Assort-Age.pdf in "supmat" folder (Y = Age group)
source(paste0(global.dir, "by5_80.R"))

### Creates frequency-based age-specific contact matrices for age groups
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/freq"
##    Saves matrices and plots in the following folders:
##        X-F-matrix.csv and X-F-df.csv in "supmat/csv/freq" folder (X = Setting)
##        4_F-matrix-X.pdf in "supmat" folder (X = Setting)
source(paste0(global.dir, "freq-m.R"))

### Creates symmetrical age-specific contact matrices 
##    Requires the following folders in working directory: "output/supmat, "output/supmat/csv/sym"
##    Saves matrices and plots in the following folders:
##        X-matrix-cond.csv in "supmat/csv/sym" folder (X = Setting)
##        6_age-sym-X.pdf in "supmat" folder (X = Setting)
source(paste0(global.dir, "sym-mat.R"))

### Creates age-specific contact matrices stratified by mode of attendance (in person or remote)
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/compare"
##    Saves matrices and plots in the following folders:
##        7_X-Y.pdf in "supmat" folder (X = Attendance mode, Y = Age group)
##        X-Y-cond-matrix.csv in "supmat/csv/compare" folder (X = Attendance mode, Y = Age group) 
source(paste0(global.dir, "inpers.R")) 

### Creates barplot and estimates confidence intervals for number of contacts by mode of attendance
##    Requires the following folders in working directory: "output/supmat"
##    Saves barplot in the following folder:
##        8_summary-inpers-CI.pdf in "supmat" folder
source(paste0(global.dir, "inpers_barplot.R"))

### Sensitivity analysis: unweighted age matrices compared to weighted age matrices
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/unw"
##    Saves matrices and plots in the following folders:
##        Unweighted-Matrix-X-Y.csv in "supmat/csv/unw" folder (X = Setting, Y = Age group)
##        Weighted-Matrix-X-Y.csv in "supmat/csv/unw" folder (X = Setting, Y = Age group)
##        9_weighted-plots.pdf in "supmat" folder 
##        10_weighted-Total-Y.pdf and 10_unweighted-Total-Y.pdf (Y = Age group)
##        11_weighted-X-Y.pdf and 11_unweighted-X-Y.pdf (X = Setting, Y = Age group)
source(paste0(global.dir, "unw-analysis.R"))

### Sensitivity analysis: raw age matrices compared to imputed age matrices
##    Requires the following folders in working directory: "data/raw_data", "output/supmat", "output/supmat/csv/by5", "output/supmat/csv/raw"
##    Requires the following data:
##        raw_contact.csv and raw_resp.csv in "raw_data" folder
##        X-Y-matrix-df.csv in "supmat/csv/by5" (X = Setting, Y = Age group) 
##    Saves the matrices and plots in the following folders:
##        12_raw-imp-plots.pdf in "supmat" folder
##        13_Total-Y-raw-matrix.csv and 14_X-Y-raw-matrix.csv in "supmat/csv/raw" folder (X = Setting, Y = Age group)
##        13_Total-Y-imp-matrix.csv and 14_X-Y-imp-matrix.csv in "supmat/csv/raw" folder (X = Setting, Y = Age group)
source(paste0(global.dir, "raw-analysis.R"))

### Creates race-specific contact matrices
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/race", "output/main/csv/race"
##    Requires the following data:
##        assort_race.csv in "main/csv/race" folder
##    Saves the matrices and plots in the following folders:
##        X-race-matrix.csv, X-race-matrix-cond.csv in "supmat/csv/race" folder
##        3_race_X.pdf, 15_cond-reth-X.pdf in "supmat" folder (X = Setting)
##        16_Summary-Ethn-Matrix.pdf in "supmat"
source(paste0(global.dir, "fig3.R")) # [Required to run race_assort.R]
source(paste0(global.dir, "race.R")) # [Required to run race_assort.R]
source(paste0(global.dir, "race_assort.R"))

### Creates sex-specific contact matrices
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/sex"
##    Saves the matrices and plots in the following folders:
##        18_sex-unconditional-X.pdf and 19_sex-conditional-X.pdf in "supmat" folder (X = Setting)
##        X-sex-matrix-cond.csv and X-sex-matrix-unc.csv in "supmat/csv/sex" folder (X = Setting)
##        N_sex-by-age-X.pdf in "supmat" folder (N = 21:25, X = Setting)
##        Summary-Assort-Gender.pdf in "supmat" folder
source(paste0(global.dir, "bysex.R"))

### Creates additional matrices analysis: comparison to pre-pandemic US estimates 
##  for in person and estimates from other European and Chinese countries
##    Requires the following folders in working directory: "output/supmat", 
##        "output/supmat/csv/freq", "output/supmat/csv/compare", 
##        "data/external_data", "data/external_data/mistry-data"
##    Requires the following data:
##        multiple csv and xlsx files in "data/external_data", "data/external_data/mistry-data",
##        "output/supmat/csv/freq" and "output/supmat/csv/compare" folders
##    Saves the matrices in the following output folders:
##        multiple matrices in csv format in "supmat/csv/compare" folder
##        multiple plots in pdf format in "supmat/" folder
source(paste0(global.dir, "more-mat.R")) # [Required to run matrix-comp.R]
source(paste0(global.dir, "matrix-comp.R"))

