### Imputations pipeline ###

### Clean the environment
rm(list = ls())
gc()

### Working directory (needs to be a parent directory)
my.folder = "Epistorm-Mix" #[CHANGE TO FULL PATH ON YOUR MACHINE]
##    The (parent) working directory should contain: 
##        Folders from GitHub: "data/raw_data", "data/external_data", "code/imputations"
##        Output (potentially empty) folders: "data/imputed_data"
setwd(my.folder)

#### Options ####

if(exists("global.dir")){
  detach(global.dir)
}
e = new.env()
e$csv_only = F  # if TRUE saves .csv files (one for respondents, one for contacts), 
              # if FALSE also saves .Rdata

### Set global directory for sourcing the code

e$global.dir = paste(my.folder, "code", "imputations","", sep = "/")
attach(e, name = 'global.dir') # (This is insurance in case working directory is changed later)

### Set Seeds ###
if(exists("my_rs")){
  detach(seeding)
}
my_param = new.env()
my_param$seed = 1234
my_param$my_iseed = 5432
my_param$my_rs = 6023
attach(my_param, name = 'seeding')

### Set the threshold for distinguishing multiple from numerous contacts (baseline 30)
if(exists("mn_thr")){
  detach(choices)
}
assumpt = new.env()
assumpt$mn_thr = 30
attach(assumpt, name = 'choices')

### Perform multiple imputations based on raw cleaned data
##    Installs the following packages: 
##        "tidyverse", "ggplot2", "dplyr", "randomForest", "doRNG", "foreach", "doParallel"
##    Requires the following code in global.dir: func_imp.R
##    Requires the following data:
##        "raw_contact.csv" and "raw_resp.csv" in "raw_data" folder
##        "student_agestr.csv" and "pop_agestr.csv" in "external_data" folder
source(paste0(global.dir, "imputations.R"))

### Combine the respondent weights of the two waves
##    Requires the following code in global.dir: "imputations.R"
##    Requires the following data: "imputed-resp2.csv" in "imputed_data" folder
source(paste0(global.dir, "adjust-weights.R")) 

detach(global.dir)
detach(choices)
detach(seeding)




