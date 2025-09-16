# imputations pipeline

e = new.env() 
e$global.dir = "/Users/malitv/MOBS Dropbox/Maria Litvinova/Research/Grant Projects/Epistorm/US-contact-patterns/shared-data-code/code/"
attach(e, name = 'global.dir') 

### perform imputations based on raw cleaned data
source(paste0(global.dir, "imputations.R"))
