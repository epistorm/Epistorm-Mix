#### Imputed Data #####
rm(list = ls())

#### Environment ####
my.folder = "/Users/shsinc/Desktop/Epistorm-survey"
setwd(my.folder)
output.dir = "./imputed_data/" 

#### Functions #### 
source("./code/func_analysis.R")

#### Load The Data ####
# source("/Users/shsinc/Desktop/Epistorm-survey/code/4_analysis_polymod_20250507.R")
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
             #"cont_age_adhoc", 
             "cont_age_by5_80", "cont_age_insightnet"#,
             #"weight"
             )

imputed_cont.df = merged.df[, tokeep_c]

# change contact generators
# idx.notmult = which(imputed_cont.df$cont_gen != "multiple contacts")
# imputed_cont.df$cont_identifier[idx.notmult] = "Diary Contacts"
# imputed_cont.df$cont_identifier[which(imputed_cont.df$cont_identifier == "multiple contacts")] = "Multiple Contacts"
imputed_cont.df$cont_identifier = factor(imputed_cont.df$cont_identifier)

# change the labels for setting
# levels(imputed_cont.df$cont_setting) = c("Household", "School", "Work", "Other")

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
          paste0(output.dir, "imputed_contact.csv"),
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
             # "resp_age_adhoc2", 
             "resp_age_insightnet", "resp_age_by5_80", ## age
             "resp_ethn", "hhsize", "hh_income", ## other sociodemographics
             "ppwork","ppworka_1","ppworka_2","ppworka_3",#"ppwork2", ## main activity
             "ppschoola", "ppschoolb", ## school enrollment
             "uds1", "uds2", "uds2_1", "uds2_2", "udw", "udw_1", "udw_2", ## school/work mode (in-person vs online)
             "hh_loc_reg4", "hh_loc_reg9", ## location
             #"is_weekend2", 
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

# create mult2_n and add_mult_n for sum.res3
# sum.res3$mc_ncont = as.numeric(sum.res3$mc_ncont)
# sum.res3$mult2_n = ifelse(sum.res3$mc_ncont <= 30, sum.res3$mc_ncont, 0)
# sum.res3$add_mult_n = ifelse(sum.res3$mc_ncont > 30, sum.res3$mc_ncont, 0)

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
# imputed_resp.df$timezone = as.factor(imputed_resp.df$timezone)

# save the data
write.csv(imputed_resp.df,
          paste0(output.dir, "imputed_resp.csv"),
          row.names = F)

#### graveyard ####
# # insightnet
# levels(imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = as.character(imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[0,5\\)", "0-4", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[5,10\\)", "5-9", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[10,18\\)", "10-17", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[18,30\\)", "18-29", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[30,40\\)", "30-39", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[40,50\\)", "40-49", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[50,60\\)", "50-59", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[60,120\\)", "60+", imputed_cont.df$cont_age_insightnet)
# # imputed_cont.df$cont_age_insightnet = as.factor(imputed_cont.df$cont_age_insightnet)
# # levels(imputed_cont.df$cont_age_insightnet) = c("0-4", "5-9", "10-17", "18-29", 
# #                                                 "30-39", "40-49", "50-59", "60+")
# imputed_cont.df$cont_age_insightnet <- paste0("'", imputed_cont.df$cont_age_insightnet)
# 
# # adhoc2
# levels(imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = as.character(imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[0,5\\)", "0-4", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[5,10\\)", "5-9", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[10,15\\)", "10-14", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[15,18\\)", "15-17", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[18,20\\)", "18-19", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[20,25\\)", "20-24", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[25,30\\)", "25-29", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[30,35\\)", "30-34", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[35,40\\)", "35-39", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[40,45\\)", "40-44", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[45,50\\)", "45-49", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[50,55\\)", "50-54", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[55,60\\)", "55-59", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[60,65\\)", "60-64", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[65,70\\)", "65-69", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[70,75\\)", "70-74", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2 = gsub("\\[75,120\\)", "75+", imputed_cont.df$cont_age_adhoc2)
# # imputed_cont.df$cont_age_adhoc2 = as.factor(imputed_cont.df$cont_age_adhoc2)
# # levels(imputed_cont.df$cont_age_adhoc2) = c("0-4", "5-9", "10-14", "15-17", "18-19",
# #                                             "20-24", "25-29", "30-34", "35-39", 
# #                                             "40-44", "45-49", "50-54", "55-59",
# #                                             "60-64", "65-69", "70-74", "75+")
# imputed_cont.df$cont_age_adhoc2 <- paste0("'", imputed_cont.df$cont_age_adhoc2)
# 
# # by5_80
# levels(imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = as.character(imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[0,5\\)", "0-4", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[5,10\\)", "5-9", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[10,15\\)", "10-14", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[15,20\\)", "15-19", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[20,25\\)", "20-24", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[25,30\\)", "25-29", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[30,35\\)", "30-34", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[35,40\\)", "35-39", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[40,45\\)", "40-44", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[45,50\\)", "45-49", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[50,55\\)", "50-54", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[55,60\\)", "55-59", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[60,65\\)", "60-64", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[65,70\\)", "65-69", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[70,75\\)", "70-74", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[75,80\\)", "75-79", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80 = gsub("\\[80,120\\)", "80+", imputed_cont.df$cont_age_by5_80)
# # imputed_cont.df$cont_age_by5_80 = as.factor(imputed_cont.df$cont_age_by5_80)
# # levels(imputed_cont.df$cont_age_by5_80) = c("0-4", "5-9", "10-14", "15-19", "20-24",
# #                                             "25-29", "30-34", "35-39", "40-44", 
# #                                             "45-49", "50-54", "55-59", "60-64", 
# #                                             "65-69", "70-74", "75-79", "80+")
# imputed_cont.df$cont_age_by5_80 <- paste0("'", imputed_cont.df$cont_age_by5_80)

# # insightnet
# levels(imputed_resp.df$resp_age_insightnet)
# imputed_resp.df$resp_age_insightnet = as.character(imputed_resp.df$resp_age_insightnet)
# imputed_resp.df$resp_age_insightnet = gsub("\\[0,5\\)", "0-4", imputed_resp.df$resp_age_insightnet)
# imputed_resp.df$resp_age_insightnet = gsub("\\[5,10\\)", "5-9", imputed_resp.df$resp_age_insightnet)
# imputed_resp.df$resp_age_insightnet = gsub("\\[10,18\\)", "10-17", imputed_resp.df$resp_age_insightnet)
# imputed_resp.df$resp_age_insightnet = gsub("\\[18,30\\)", "18-29", imputed_resp.df$resp_age_insightnet)
# imputed_resp.df$resp_age_insightnet = gsub("\\[30,40\\)", "30-39", imputed_resp.df$resp_age_insightnet)
# imputed_resp.df$resp_age_insightnet = gsub("\\[40,50\\)", "40-49", imputed_resp.df$resp_age_insightnet)
# imputed_resp.df$resp_age_insightnet = gsub("\\[50,60\\)", "50-59", imputed_resp.df$resp_age_insightnet)
# imputed_resp.df$resp_age_insightnet = gsub("\\[60,120\\)", "60+", imputed_resp.df$resp_age_insightnet)
# # imputed_resp.df$resp_age_insightnet = as.factor(imputed_resp.df$resp_age_insightnet)
# # levels(imputed_resp.df$resp_age_insightnet) = c("0-4", "5-9", "10-17", "18-29", 
# #                                                 "30-39", "40-49", "50-59", "60+")
# imputed_resp.df$resp_age_insightnet <- paste0("'", imputed_resp.df$resp_age_insightnet)
# 
# # adhoc2
# levels(imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = as.character(imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[0,5\\)", "0-4", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[5,10\\)", "5-9", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[10,15\\)", "10-14", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[15,18\\)", "15-17", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[18,20\\)", "18-19", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[20,25\\)", "20-24", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[25,30\\)", "25-29", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[30,35\\)", "30-34", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[35,40\\)", "35-39", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[40,45\\)", "40-44", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[45,50\\)", "45-49", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[50,55\\)", "50-54", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[55,60\\)", "55-59", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[60,65\\)", "60-64", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[65,70\\)", "65-69", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[70,75\\)", "70-74", imputed_resp.df$resp_age_adhoc2)
# imputed_resp.df$resp_age_adhoc2 = gsub("\\[75,120\\)", "75+", imputed_resp.df$resp_age_adhoc2)
# # imputed_resp.df$resp_age_adhoc2 = as.factor(imputed_resp.df$resp_age_adhoc2)
# # levels(imputed_resp.df$resp_age_adhoc2) = c("0-4", "5-9", "10-14", "15-17", "18-19",
# #                                             "20-24", "25-29", "30-34", "35-39", 
# #                                             "40-44", "45-49", "50-54", "55-59",
# #                                             "60-64", "65-69", "70-74", "75+")
# imputed_resp.df$resp_age_adhoc2 <- paste0("'", imputed_resp.df$resp_age_adhoc2)
# 
# # by5_80
# levels(imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = as.character(imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[0,5\\)", "0-4", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[5,10\\)", "5-9", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[10,15\\)", "10-14", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[15,20\\)", "15-19", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[20,25\\)", "20-24", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[25,30\\)", "25-29", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[30,35\\)", "30-34", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[35,40\\)", "35-39", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[40,45\\)", "40-44", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[45,50\\)", "45-49", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[50,55\\)", "50-54", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[55,60\\)", "55-59", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[60,65\\)", "60-64", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[65,70\\)", "65-69", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[70,75\\)", "70-74", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[75,80\\)", "75-79", imputed_resp.df$resp_age_by5_80)
# imputed_resp.df$resp_age_by5_80 = gsub("\\[80,120\\)", "80+", imputed_resp.df$resp_age_by5_80)
# # imputed_resp.df$resp_age_by5_80 = as.factor(imputed_resp.df$resp_age_by5_80)
# # levels(imputed_resp.df$resp_age_by5_80) = c("0-4", "5-9", "10-14", "15-19", "20-24",
# #                                             "25-29", "30-34", "35-39", "40-44", 
# #                                             "45-49", "50-54", "55-59", "60-64", 
# #                                             "65-69", "70-74", "75-79", "80+")
# imputed_resp.df$resp_age_by5_80 <- paste0("'", imputed_resp.df$resp_age_by5_80 )
