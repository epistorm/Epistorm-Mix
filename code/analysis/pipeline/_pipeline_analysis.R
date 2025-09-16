# analysis pipeline
## Clean the environment
rm(list = ls())

#### Folders and Directories ####

## Working directory
my.folder = "/Users/malitv/MOBS Dropbox/Maria Litvinova/Research/Grant Projects/Epistorm/US-contact-patterns/shared-data-code"
setwd(my.folder)

## Set global directory for sourcing the code
e = new.env()
e$global.dir = paste0(getwd(), "/pipeline/")
attach(e, name = 'global.dir')

## Output directory
output.dir = "./output/main/"
sm.dir = "./output/supmat/" # directory for the output later used for supplementary materials

### creates additional variables and encodes existing variables in imputed data
source(paste0(global.dir, "analysis_prep.R")) ### needed for all the code after 

### create table 1 for the main text 
source(paste0(global.dir, "tbl1_main.R"))

### Perform GAM analysis
# source(paste0(global.dir, "gam.R"))

### Create figure 1 for main text
# source(paste0(global.dir, "gam_fig1.R"))

### create figure 2 for main text
source(paste0(global.dir, "fig2.R"))

### create figure 3 for main text
source(paste0(global.dir, "fig3.R"))

### change directory for supplementary material 
output.dir = sm.dir

### create table S1 for supp mat
source(paste0(global.dir, "tbl1_sm.R"))
source(paste0(global.dir, "tbl2_sm.R"))

### additional age analysis for supp mat
source(paste0(global.dir, "by5_80.R"))

### frequency-based matrices for supp mat
source(paste0(global.dir, "freq-m.R"))

### symmetrical age matrices for supp mat
source(paste0(global.dir, "sym-mat.R"))

### in person analysis by age for supp mat
source(paste0(global.dir, "inpers.R"))
source(paste0(global.dir, "inpers_barplot.R"))

### sensitivity analysis: unweighted age matrices compared to weighted age matrices
source(paste0(global.dir, "unw-analysis.R"))

### sensitivity analysis: raw age matrices compared to imputed age matrices
source(paste0(global.dir, "raw-analysis.R"))

### additional race analysis for supp mat
source(paste0(global.dir, "race.R"))
source(paste0(global.dir, "race_assort.R"))

### additional sex analysis for supp mat
source(paste0(global.dir, "bysex.R"))

### additional matrices analysis: comparison to pre-pandemic US estimates and 
## estimates from other European and Chinese countries
source(paste0(global.dir, "more-mat.R")) 
source(paste0(global.dir, "rel-diff.R"))

