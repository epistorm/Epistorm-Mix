#### Imputed Data #####
rm(list = ls())

#### Environment ####
my.folder = "/your-path/Epistorm-Mix/data/imputed_data"
setwd(my.folder)
output.dir = "./imputed_data/" 

#### Functions #### 
source("./code/func_analysis.R")

#### Load The Data ####
load("./imputed_dataall-waves.Rdata")

#### Age Groups ####

age.vars.breaks = list(c(seq(0,80, by = 5), 120),
                       c(0,5,10,18,seq(30,60, by=10),120))

age.vars.names = c(paste0("_",c("by5_80","insightnet")))

sum.res3 = create.age.groups(sum.res3, input.var = "resp_age", # resp age
                             break.list = age.vars.breaks,
                             suff.list = age.vars.names)
merged.df = create.age.groups(merged.df, input.var = "cont_age", # contact age
                              break.list = age.vars.breaks,
                              suff.list = age.vars.names)

#### Contact ####
tokeep_c = c("resp_id", 
             "cont_identifier", 
             "cont_setting", "cont_relationship",
             "cont_ethn", "cont_gender",
             "cont_age", 
             "cont_age_by5_80", "cont_age_insightnet"
             )

imputed_cont.df = merged.df[, tokeep_c]

imputed_cont.df$cont_identifier = factor(imputed_cont.df$cont_identifier)

# change the labels for relationship
imputed_cont.df$cont_relationship = factor(imputed_cont.df$cont_relationship)
levels(imputed_cont.df$cont_relationship) = tools::toTitleCase(levels(imputed_cont.df$cont_relationship))

# change to factor
imputed_cont.df$cont_ethn = factor(imputed_cont.df$cont_ethn,
                                   levels = c("White, Non-Hispanic", "Hispanic/Latino", 
                                              "Black, Non-Hispanic", "Asian, Non-Hispanic",
                                              "Other, Non-Hispanic"))
imputed_cont.df$cont_gender = as.factor(imputed_cont.df$cont_gender)

# save the data
write.csv(imputed_cont.df,
          paste0(output.dir, "imputed-contacts2.csv"),
          row.names = F)

#### Respondent ####

tokeep_r = c("resp_id",
             "cont", ## has contacts (yes/no)
             "sample_g", ## sample group (parents/children vs. adults)
             "resp_resp_sick", "resp_hhm_sick", ## sick
             "resp_industry_group", "resp_occupation_group", ## industry/occupation
             "resp_mc_job", "mc_ncont", ## multiple/numerous contacts
             "mult2_n", "add_mult_n", ## multiple/numerous contacts
             "resp_educator", "resp_educator_mc", "resp_edun_02","resp_edun_34", ## educational numerous/multiple contacts
             "resp_edun_59", "resp_edun_1014", "resp_edun_1517", "resp_edun_18", 
             "edu2_n", "add_edu_n", ## multiple/numerous educator contacts
             "resp_gender", ## gender
             "resp_age", 
             "resp_age_insightnet", "resp_age_by5_80", ## age
             "resp_ethn", "hhsize", "hh_income", ## other sociodemographics
             "ppwork","ppworka_1","ppworka_2","ppworka_3",#"ppwork2", ## main activity
             "ppschoola", "ppschoolb", ## school enrollment
             "uds1", "uds2", "uds2_1", "uds2_2", "udw", "udw_1", "udw_2", ## school/work mode (in-person vs online)
             "hh_loc_reg4", "hh_loc_reg9", ## location
             "tm_start_cdt", "tm_end_cdt", "tm_start_lt", "tm_end_lt",#"timezone", ## date, time, weekend
             "wave",# wave
             "weight",# respondent weight
             "total.contacts", "cont_setting_nothh_n" # total contacts
             ) 

## if respondent has a contact
sum.res3$cont = NA
sum.res3$cont = ifelse(sum.res3$total.contacts > 0, 1, 0)
sum.res3$cont <- factor(sum.res3$cont,
                             levels = c(0, 1),
                             labels = c("Does not have contacts", "Has contacts"))

# create wave
sum.res3$wave = NA
idx.002 = grep("002$", sum.res3$resp_id)
if(length(idx.002) > 0){
  sum.res3$wave[idx.002] = "wave2"
  sum.res3$wave[-idx.002] = "wave1"
}else{
  stop("No Wave 2 data")
}
sum.res3$wave = as.factor(sum.res3$wave)

# new data
imputed_resp.df = sum.res3[,tokeep_r]

# change caseid
# colnames(imputed_resp.df)[1] = "resp_id"

# change var type
imputed_resp.df$resp_edun_02 = as.integer(imputed_resp.df$resp_edun_02)
imputed_resp.df$resp_edun_34 = as.integer(imputed_resp.df$resp_edun_34)
imputed_resp.df$resp_edun_59 = as.integer(imputed_resp.df$resp_edun_59)
imputed_resp.df$resp_edun_1014 = as.integer(imputed_resp.df$resp_edun_1014)
imputed_resp.df$resp_edun_1517 = as.integer(imputed_resp.df$resp_edun_1517)
imputed_resp.df$resp_edun_18 = as.integer(imputed_resp.df$resp_edun_18)

# save the data
write.csv(imputed_resp.df,
          paste0(output.dir, "imputed-resp2.csv"),
          row.names = F)

