# analysis pipeline
## Clean the environment
rm(list = ls())
gc()

## Define if GAM should be run to find best-fitting specifications
# if FALSE, GAM model will be performed for the full set of variable combinations to select the best-fitting models
# if TRUE, model_specs will be read from githuband used as the best-fitting specifications
read_specs.T = T 

#### Folders and Directories ####
## Working (parent) directory
my.folder = "Epistorm-Mix" #[CHANGE TO FULL PATH ON YOUR MACHINE]
##    The the (parent) working directory should contain: 
##        Folders from GitHub: "data/imputed_data", "data/external_data", "code/analysis"
##        Output folders are created below if they are not downloaded from GitHub
setwd(my.folder)

## Set global directory for sourcing the code
if(exists("global.dir")){
  detach(global.dir)
}
e = new.env()
e$global.dir = file.path(my.folder, "code","analysis")
attach(e, name = 'global.dir') ## (This code is insurance in case working directory changes)

## Output directory

## set these directories to save resulting figures and tables
output.dir = file.path(my.folder, "output","main")
sm.dir = file.path(my.folder, "output","supmat") # directory for the output later used for supplementary materials

## Create directories and sub-directories:
source(file.path(global.dir, "output_dirs.R"))

#### Required Preprocessing ####

### Creates additional variables and encodes existing variables in imputed data:
##    Installs the following packages: "dplyr", "survey", "tidyverse", "ggplot2","flextable", "gt", "gtsummary",
##                                      "fitdistrplus", "mgcv", "sjPlot", "patchwork", "doParallel", "extrafont",
##                                      "readxl", "data.table", "tidyr", "psych", "Hmisc", "prettyGraphs"
##    Requires the following folders in working directory: "data/imputed_data", "data/external_data"
##    Requires the following code in global.dir: "code/func_analysis.R";
##    Requires the following data:
##        "imputed-contacts2.csv" and "imputed-resp2.csv" in "data/imputed_data" folder
##        "federal-state-holidays.csv" in "data/external_data" folder
source(file.path(global.dir, "analysis_prep.R")) ### [Required for all the code after]

#### Analysis ####

##### Create table 1 for the main text #####
##    Requires the following folders in working directory:
##        "output/main", "output/main/csv" (created in lines 20:40 of this pipeline)
##    Saves tables:
##        "table1_main.docx" in "output/main" folder
##        "table1_main.csv" in "output/main/csv" folder
source(file.path(global.dir, "tbl1_main.R"))

##### Perform GAM analysis (also creates tables S10 ans S12 for SM) #####
if (!read_specs.T){
  gam.results = file.path(sm.dir, "gam")
} else {
  gam.results = file.path("data", "gam_specifications")
}
# if read_specs.T == F, run GAM analysis, otherwise create tables
##    Requires: "output/supmat" folder in working directory
##               read_specs.T to be defined
##    Sources the data preparation code from "gam_data_prep.R" in global.dir
##    Saves best specifications in file "model_specs.txt" in "output/sm.dir" if read_specs.T == T
##    Saves table "multivariate_gam_bf_X.docx" in "output/supmat/gam" folder (X=1 for total contact; X=2 for non-hh contacts)
# define the directory where the selected specifications are saved
source(file.path(global.dir, "gam.R"))

##### Create figure 1 for main text #####
##    Uses best fit specification of the gam model (from "gam.results" folder defined below)
##      to plot relative ratios for total contacts
##    Sources the data preparation code from "gam_data_prep.R" in global.dir
##    Saves figure in the following folder:
##         "1_effects_figure_total.pdf" in "output/main" folder
source(file.path(global.dir, "gam_fig1.R"))

##### Create age-specific contact matrices (figure 2 of main text) #####
##    Requires the following folders in working directory: "output/main", "output/main/csv"
##    Saves matrices in the following folders:
##        "X-pred-matrix.csv" in "output/main/csv" folder (X = Setting)
##        "2_age_X.pdf" in "output/main" folder (X = Setting)
source(file.path(global.dir, "fig2.R"))
 
##### Create correlation plots for contact matrices (figure 3 of main text) #####
##    The code below calculates both M-matrix and F-matrices for total contacts
##    Requires the following folders in working directory:
##        Optional: "matrices/M_matrix", "matrices/F_matrix", #if you prefer to use provided matrices instead of calculating them
##        Necessary: "data/external_data", "data/external_data/mistry-data"
##    Requires the following data:
##        multiple .csv and .xlsx files in "data/external_data", "data/external_data/mistry-data",
##        Optional: files from "matrices/M_matrix" and "matrices/F_matrix" folders
##    Saves plots in the following folders:
##        "fig3-correlations.pdf" in "output/main" folder
source(file.path(global.dir, "fig3.R"))

##### Change output directory for supplementary material #####
output.dir = sm.dir

##### Create table S6: total contacts by respondent characteristics #####
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv"
##    Saves table in the following folders:
##        "table1_sm.docx" in "output/supmat" folder
##        "table1_sm.csv" in "output/supmat/csv" folder
source(file.path(global.dir, "tbl1_sm.R"))

##### Create tables from univariable regressions #####
##    Requires the following folders in working directory: "output/supmat"
##    Saves output in the following folders:
##        "indep-glm.csv", "factor_corr.csv", and "agediff_by_levels.csv" in "output/supmat" folder
##        "resp_age_insightnet_by_X.pdf" in "output/supmat" folder
source(file.path(global.dir, "univar-regressions.R"))

##### Create figure S1: marginal effects of age for total contacts #####
##    Sources the data preparation code from "gam_data_prep.R" in global.dir
##    Requires the following folders in working directory: "output/supmat/gam", "data/gam_specifications"
##    Requires the following data:
##        "model_specs.txt" in "gam_specifications" folder
##    Saves output in the following folders:
##        "age_me_total.pdf" in "output/supmat/gam" folder
source(file.path(global.dir, "gam_sm1.R"))

##### Create Table S11: distribution of respondent characteristics for all contact settings #####
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv"
##    Saves table in the following folders:
##        "table2_sm.docx" in "output/supmat" folder
##        "table2_sm.csv" in "output/supmat/csv" folder
source(file.path(global.dir, "tbl2_sm.R"))

##### Create figure S2: relative ratios + marginal effects of age for non-household contacts #####
##    Sources the data preparartion code from "gam_data_prep.R" in global.dir
##    Requires the following folders in working directory: "output/supmat/gam", "data/gam_specifications"
##    Requires the following data:
##        "model_specs.txt" in "gam_specifications" folder
##    Saves output in the following folders:
##        "effects_figure_nonhh.pdf", "age_me_nonhh.pdf", and "combined_gam_figure_nonhh.pdf" in "output/supmat/gam" folder
source(file.path(global.dir, "gam_sm2.R"))

##### Create age-specific contact matrices and plots for age groups and an assortativity plot #####
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/by5"
##    Saves matrices and plots in the following folders:
##        "X-Y-matrix-cond.csv" in "output/supmat/csv/by5" folder (X = Setting, Y = Age group)
##        "5_Y-Conditional-X.pdf" in "output/supmat" folder (X = Setting, Y = Age group)
##        "X-Y-matrix-unc.csv" and X-Y-matrix-df.csv in "output/supmat/csv/by5" folder (X = Setting, Y = Age group)
##        "2_Y-Unconditional-X.pdf" in "output/supmat" folder (X = Setting, Y = Age group)
##        "3_Y-Summary-Assort-Age.pdf" in "output/supmat" folder (Y = Age group)
source(file.path(global.dir, "by5_80.R"))

##### Create frequency-based age-specific contact matrices for age groups #####
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/freq"
##    Saves matrices and plots in the following folders:
##        "X-F-matrix.csv" and "X-F-df.csv" in "output/supmat/csv/freq" folder (X = Setting)
##        "4_F-matrix-X.pdf" in "output/supmat" folder (X = Setting)
source(file.path(global.dir, "freq-m.R"))

##### Create symmetrical age-specific contact matrices #####
##    Requires the following folders in working directory: "output/supmat, "output/supmat/csv/sym"
##    Saves matrices and plots in the following folders:
##        "X-matrix-cond.csv" in "output/supmat/csv/sym" folder (X = Setting)
##        "6_age-sym-X.pdf" in "output/supmat" folder (X = Setting)
source(file.path(global.dir, "sym-mat.R"))

##### Create age-specific contact matrices stratified by mode of attendance (in person or remote) #####
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/compare"
##    Saves matrices and plots in the following folders:
##        "7_X-Y.pdf" in "output/supmat" folder (X = Attendance mode, Y = Age group)
##        "X-Y-cond-matrix.csv" in "output/supmat/csv/compare" folder (X = Attendance mode, Y = Age group)
##        "8_summary-inpers-CI.pdf" in "output/supmat" folder
source(file.path(global.dir, "inpers.R"))
source(file.path(global.dir, "inpers_barplot.R"))

##### Sensitivity analysis: unweighted age matrices compared to weighted age matrices #####
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/unw"
##    Saves matrices and plots in the following folders:
##        "Unweighted-Matrix-X-Y.csv" in "output/supmat/csv/unw" folder (X = Setting, Y = Age group)
##        "Weighted-Matrix-X-Y.csv" in "output/supmat/csv/unw" folder (X = Setting, Y = Age group)
##        "9_weighted-plots.pdf" in "output/supmat" folder
##        "10_weighted-Total-Y.pdf" and "10_unweighted-Total-Y.pdf" (Y = Age group)
##        "11_weighted-X-Y.pdf" and "11_unweighted-X-Y.pdf" (X = Setting, Y = Age group)
source(file.path(global.dir, "unw-analysis.R"))

##### Sensitivity analysis: raw age matrices compared to imputed age matrices #####
##    Requires the following folders in working directory: "data/raw_data", "output/supmat", "output/supmat/csv/by5", "output/supmat/csv/raw"
##    Requires the following data:
##        "raw_contact.csv" and "raw_resp.csv" in "data/raw_data" folder
##        "X-Y-matrix-df.csv" in "output/supmat/csv/by5" (X = Setting, Y = Age group)
##    Saves the matrices and plots in the following folders:
##        "12_raw-imp-plots.pdf" in "output/supmat" folder
##        "13_Total-Y-raw-matrix.csv" and "14_X-Y-raw-matrix.csv" in "output/supmat/csv/raw" folder (X = Setting, Y = Age group)
##        "13_Total-Y-imp-matrix.csv" and "14_X-Y-imp-matrix.csv" in "output/supmat/csv/raw" folder (X = Setting, Y = Age group)
source(file.path(global.dir, "raw-analysis.R"))

##### Creates race-specific contact matrices #####
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/race", "output/main/csv/race"
##    Requires the following data:
##        "assort_race.csv" in "output/main/csv/race" folder
##    Saves the matrices and plots in the following folders:
##        "X-race-matrix.csv", "X-race-matrix-cond.csv" in "output/supmat/csv/race" folder
##        "3_race_X.pdf", "15_cond-reth-X.pdf" in "output/supmat" folder (X = Setting)
##        "3_race_summary.pdf", "16_Summary-Ethn-Matrix.pdf" in "output/supmat"
source(file.path(global.dir, "race.R")) # [Required to run race_assort.R]
source(file.path(global.dir, "race_assort.R"))

##### Creates sex-specific contact matrices #####
##    Requires the following folders in working directory: "output/supmat", "output/supmat/csv/sex"
##    Saves the matrices and plots in the following folders:
##        "18_sex-unconditional-X.pdf" and "19_sex-conditional-X.pdf" in "output/supmat" folder (X = Setting)
##        "X-sex-matrix-cond.csv" and "X-sex-matrix-unc.csv" in "output/supmat/csv/sex" folder (X = Setting)
##        "N_sex-by-age-X.pdf" in "output/supmat" folder (N = 21:25, X = Setting)
##        "Summary-Assort-Gender.pdf" in "output/supmat" folder
source(file.path(global.dir, "bysex.R"))

##### Creates additional matrices analysis: comparison to pre-pandemic US estimates #####
##  for in person and estimates from other European and Chinese countries
##    Requires the following folders in working directory: "output/supmat",
##        "output/supmat/csv/freq", "output/supmat/csv/compare",
##        "data/external_data", "data/external_data/mistry-data"
##    Requires the following data:
##        multiple csv and xlsx files in "data/external_data", "data/external_data/mistry-data",
##        "output/supmat/csv/freq" and "output/supmat/csv/compare" folders
##    Saves the matrices in the following output folders:
##        multiple matrices in csv format in "output/supmat/csv/compare" folder
##        multiple plots in pdf format in "output/supmat/" folder
source(file.path(global.dir, "more-mat.R")) # [Required to run matrix-comp.R]
source(file.path(global.dir, "matrix-comp.R")) # [Requires "data/external_data/Luchinni_bootstrap_age_group_total.zip" to be unzipped]

detach(global.dir)