# imputations pipeline

e = new.env() 
# e$global.dir = "/Users/malitv/MOBS Dropbox/Maria Litvinova/Research/Grant Projects/Epistorm/US-contact-patterns/shared-data-code/code/"
e$global.dir = "/Users/shsinc/Desktop/Epistorm-survey/code/"
attach(e, name = 'global.dir') 

### perform multiple imputations based on raw cleaned data on missing contact data
##    Installs the following packages: "tidyverse", "ggplot2", "dplyr", "randomForest", "doRNG", "foreach", "doParallel"
##    Requires the following folders in working directory: "imputed_data", "raw_data", "external_data"
##    Requires the following code in global.dir: func_imp.R;
##    Requires the following data:
##        raw_contact.csv and raw_resp.csv in "raw_data" folder
##        student_agestr.csv and pop_agestr.csv in "external_data" folder
source(paste0(global.dir, "imputations.R"))