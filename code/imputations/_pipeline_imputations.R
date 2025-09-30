# imputations pipeline
## Clean the environment
rm(list = ls())

## Working directory
my.folder = "[your preferred working directory name here]"
setwd(my.folder)

## Set global directory for sourcing the code
## (This code is insurance in case you run any code files that change working directory)
e = new.env()
e$global.dir = paste0(getwd(), "/code/imputations/")
attach(e, name = 'global.dir')

### perform multiple imputations based on raw cleaned data on missing contact data
##    Installs the following packages: "tidyverse", "ggplot2", "dplyr", "randomForest", "doRNG", "foreach", "doParallel"
##    Requires the following folders in working directory: "imputed_data", "raw_data", "external_data"
##    Requires the following code in global.dir: func_imp.R;
##    Requires the following data:
##        raw_contact.csv and raw_resp.csv in "raw_data" folder
##        student_agestr.csv and pop_agestr.csv in "external_data" folder
source(paste0(global.dir, "imputations.R"))