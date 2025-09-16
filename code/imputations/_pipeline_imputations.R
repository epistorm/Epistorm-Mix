# imputations pipeline

e = new.env() 
e$global.dir = "/your-path/Epistorm-Mix/code/imputations"
attach(e, name = 'global.dir') 

### perform imputations based on raw cleaned data
source(paste0(global.dir, "imputations.R"))
