#### Imputed Data - Codebooks ####
rm(list = ls())

#### Environment ####
my.folder = "/Users/shsinc/Desktop/Epistorm-survey"
setwd(my.folder)
data.dir = "./imputed_data/"
output.dir = "./imputed_data/" 

#### load the imputed data ####
source(paste0(data.dir,"imputed-data_v2.R"))

#### Codebook: Imputed Contact Data ####

codebook_contact = data.frame(
  variable = rep(NA, times = ncol(imputed_cont.df)),
  description = rep(NA, times = ncol(imputed_cont.df)),
  response_options = rep(NA, times = ncol(imputed_cont.df)),
  original_name = rep(NA, times = ncol(imputed_cont.df))
)

# add var name and levels if categorical
for (i in 1:ncol(imputed_cont.df)) {
  codebook_contact$variable[i] = colnames(imputed_cont.df)[i]
  codebook_contact$response_options[i] = paste0(levels(imputed_cont.df[,codebook_contact$variable[i]]),
                                                collapse = "; ")
  if (is.null(levels(imputed_cont.df[,codebook_contact$variable[i]]))) {
    codebook_contact$response_options[i] = "Numeric"
  }
}

# # insightnet
# imputed_cont.df$cont_age_insightnet = sub("^'", "", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet[which(imputed_cont.df$cont_age_insightnet == "NA")] = NA
# imputed_cont.df$cont_age_insightnet = as.character(imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[0,5\\)", "0-4", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[5,10\\)", "5-9", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[10,18\\)", "10-17", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[18,30\\)", "18-29", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[30,40\\)", "30-39", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[40,50\\)", "40-49", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[50,60\\)", "50-59", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = gsub("\\[60,120\\)", "60+", imputed_cont.df$cont_age_insightnet)
# imputed_cont.df$cont_age_insightnet = as.factor(imputed_cont.df$cont_age_insightnet)
# levels(imputed_cont.df$cont_age_insightnet) = c("0-4", "5-9", "10-17", "18-29", 
#                                                 "30-39", "40-49", "50-59", "60+")
# codebook_contact$response_options[which(codebook_contact$variable == "cont_age_insightnet")] = 
#   paste0(levels(imputed_cont.df$cont_age_insightnet), collapse = "; ")
# 
# # adhoc2
# imputed_cont.df$cont_age_adhoc2 = sub("^'", "", imputed_cont.df$cont_age_adhoc2)
# imputed_cont.df$cont_age_adhoc2[which(imputed_cont.df$cont_age_adhoc2 == "NA")] = NA
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
# imputed_cont.df$cont_age_adhoc2 = as.factor(imputed_cont.df$cont_age_adhoc2)
# levels(imputed_cont.df$cont_age_adhoc2) = c("0-4", "5-9", "10-14", "15-17", "18-19",
#                                             "20-24", "25-29", "30-34", "35-39", 
#                                             "40-44", "45-49", "50-54", "55-59",
#                                             "60-64", "65-69", "70-74", "75+")
# codebook_contact$response_options[which(codebook_contact$variable == "cont_age_adhoc2")] = 
#   paste0(levels(imputed_cont.df$cont_age_adhoc2), collapse = "; ")
# 
# # by5_80
# imputed_cont.df$cont_age_by5_80 = sub("^'", "", imputed_cont.df$cont_age_by5_80)
# imputed_cont.df$cont_age_by5_80[which(imputed_cont.df$cont_age_by5_80 == "NA")] = NA
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
# imputed_cont.df$cont_age_by5_80 = as.factor(imputed_cont.df$cont_age_by5_80)
# levels(imputed_cont.df$cont_age_by5_80) = c("0-4", "5-9", "10-14", "15-19", "20-24",
#                                             "25-29", "30-34", "35-39", "40-44", 
#                                             "45-49", "50-54", "55-59", "60-64", 
#                                             "65-69", "70-74", "75-79", "80+")
# codebook_contact$response_options[which(codebook_contact$variable == "cont_age_by5_80")] = 
#   paste0(levels(imputed_cont.df$cont_age_by5_80), collapse = "; ")

# var name from instrument  
codebook_contact$original_name = c(
  resp_id = "CaseID",
  cont_identifier = "Name_gen",
  cont_setting = "Created",
  cont_relationship = "RELATIONSHIP",
  cont_ethn = "C_RACE_ETHNIC",
  cont_gender = "C_GENDER",
  cont_age = "C_AGE",
  # cont_age_adhoc2 = "Created",
  cont_age_by5_80 = "Created",
  cont_age_insightnet = "Created"
)

# description of the vars
codebook_contact$description = c(
  resp_id = "ID of respondent",
  cont_identifier = "Mode of contact collection",
  cont_setting =  "Where did the contact occur?",
  cont_relationship = "What is the relationship of the contact to respondent?",
  cont_ethn = "What is the race/ethnicity of respondent's contact?",
  cont_gender = "What is the gender of respondent's contact?",
  cont_age = "What is the age of respondent's contact?",
  # cont_age_adhoc2 = "Age group with 17 levels",
  cont_age_by5_80 =  "Age group with 17 levels",
  cont_age_insightnet = "Age group with 8 levels"
)

write.csv(codebook_contact,
          paste0(output.dir, "/codebook_contact.csv"),
          row.names = F)

#### Codebook: Imputed Respondent Data ####
codebook_resp = data.frame(
  variable = rep(NA, times = ncol(imputed_resp.df)),
  description = rep(NA, times = ncol(imputed_resp.df)),
  response_options = rep(NA, times = ncol(imputed_resp.df)),
  original_name = rep(NA, times = ncol(imputed_resp.df))
)

# add var name and levels
for (i in 1:ncol(imputed_resp.df)) {
  codebook_resp$variable[i] = colnames(imputed_resp.df)[i]
  codebook_resp$response_options[i] = paste0(levels(imputed_resp.df[,codebook_resp$variable[i]]),
                                             collapse = "; ")
  if (is.null(levels(imputed_resp.df[,codebook_resp$variable[i]]))) {
    codebook_resp$response_options[i] = "Numeric"
  }
}

# fix levels 
for (i in 1:ncol(imputed_resp.df)) {
  if (length(levels(imputed_resp.df[,codebook_resp$variable[i]])) == 3 &
      "Refused" %in% levels(imputed_resp.df[,codebook_resp$variable[i]])) {
    sorted = c("Yes", "No", "Refused")
    imputed_resp.df[,codebook_resp$variable[i]] = factor(imputed_resp.df[,codebook_resp$variable[i]],
                                                         levels = sorted)
    codebook_resp$response_options[i] = paste0(sorted, collapse = "; ")
  }
  
  if (length(levels(imputed_resp.df[,codebook_resp$variable[i]])) == 2 &
      "Yes" %in% levels(imputed_resp.df[,codebook_resp$variable[i]])) {
    sorted = c("Yes", "No")
    imputed_resp.df[,codebook_resp$variable[i]] = factor(imputed_resp.df[,codebook_resp$variable[i]],
                                                                 levels = sorted)
    codebook_resp$response_options[i] = paste0(sorted,
                                               collapse = "; ")
  }
  
  if (length(levels(imputed_resp.df[,codebook_resp$variable[i]])) == 4 &
      "Yes" %in% levels(imputed_resp.df[,codebook_resp$variable[i]])) {
    sorted = c("Yes", "No", "Not asked", "Refused")
    imputed_resp.df[,codebook_resp$variable[i]] = factor(imputed_resp.df[,codebook_resp$variable[i]],
                                                         levels = sorted)
    codebook_resp$response_options[i] = paste0(sorted,
                                               collapse = "; ")
  }
  
  if (codebook_resp$variable[i] == "ppschoola") {
    sorted = c("Not asked", "Enrolled in school", "Enrolled in day care", "Home schooled",
               "Other")
    imputed_resp.df[,codebook_resp$variable[i]] = 
      factor(imputed_resp.df[,codebook_resp$variable[i]], levels = sorted)
    codebook_resp$response_options[i] = paste0(sorted, collapse = "; ")
  } else if (codebook_resp$variable[i] == "ppschoolb") {
    sorted = c("Not asked", "Enrolled in school", "Home schooled", "Other")
    imputed_resp.df[,codebook_resp$variable[i]] = 
      factor(imputed_resp.df[,codebook_resp$variable[i]], levels = sorted)
    codebook_resp$response_options[i] = paste0(sorted, collapse = "; ")
  }
  
  if (codebook_resp$variable[i] == "uds1") {
    sorted = c("Yes, in Person", "Yes, Online", "Not asked", "No")
    imputed_resp.df[,codebook_resp$variable[i]] = 
      factor(imputed_resp.df[,codebook_resp$variable[i]], levels = sorted)
    codebook_resp$response_options[i] = paste0(sorted, collapse = "; ")
  } else if (codebook_resp$variable[i] %in% c("uds2", "udw")) {
    sorted = c("Yes, in Person", "Yes, Online", "Yes, Hybrid","Not asked", "No")
    imputed_resp.df[,codebook_resp$variable[i]] = 
      factor(imputed_resp.df[,codebook_resp$variable[i]], levels = sorted)
    codebook_resp$response_options[i] = paste0(sorted, collapse = "; ")
  } 
  
}

# manual fixes
levels(imputed_resp.df$ppwork) = tools::toTitleCase(levels(imputed_resp.df$ppwork))

codebook_resp$response_options[which(codebook_resp$variable == "ppwork")] =
  paste0(levels(imputed_resp.df$ppwork), collapse = "; ")

levels(imputed_resp.df$uds1) = tools::toTitleCase(levels(imputed_resp.df$uds1))
codebook_resp$response_options[which(codebook_resp$variable == "uds1")] =
  paste0(levels(imputed_resp.df$uds1), collapse = "; ")

levels(imputed_resp.df$uds2) = tools::toTitleCase(levels(imputed_resp.df$uds2))
codebook_resp$response_options[which(codebook_resp$variable == "uds2")] =
  paste0(levels(imputed_resp.df$uds2), collapse = "; ")

levels(imputed_resp.df$udw) = tools::toTitleCase(levels(imputed_resp.df$udw))
codebook_resp$response_options[which(codebook_resp$variable == "udw")] =
  paste0(levels(imputed_resp.df$udw), collapse = "; ")

levels(imputed_resp.df$wave) = tools::toTitleCase(levels(imputed_resp.df$wave))
codebook_resp$response_options[which(codebook_resp$variable == "wave")] =
  paste0(levels(imputed_resp.df$wave), collapse = "; ")


# var name from instrument
codebook_resp$original_name = c(
  resp_id = "CaseID",
  cont = "Created",
  sample_g = "Xepistorm",
  resp_resp_sick = "Sick1",
  resp_hhm_sick = "Sick2",
  resp_industry_group = "IND1",
  resp_occupation_group = "ppcm0160",
  resp_mc_job = "MC1",
  mc_ncont = "MC1",
  mult2_n = "Created", 
  add_mult_n = "Created",  
  resp_educator = "EDU",
  resp_educator_mc = "EDU_MC",
  resp_edun_02 = "EDU_Ncont1",
  resp_edun_34 = "EDU_Ncont2",
  resp_edun_59 = "EDU_Ncont3",
  resp_edun_1014 = "EDU_Ncont4",
  resp_edun_1517 = "EDU_Ncont5",
  resp_edun_18 = "EDU_Ncont6",
  edu2_n = "Created",
  add_edu_n = "Created", 
  resp_gender = "ppgender",
  resp_age = "ppage",
  # resp_age_adhoc2 = "Created", 
  resp_age_insightnet = "Created",
  resp_age_by5_80 = "Created",
  resp_ethn = "ppethm",
  hhsize = "pphhsize",
  hh_income = "ppinc7",
  ppwork = "Created",
  ppworka_1 = "PPWORKA",
  ppworka_2 = "PPWORKA",
  ppworka_3 = "PPWORKA",
  ppschoola = "PPSCHOOLA",
  ppschoolb = "PPSCHOOLB",
  uds1 = "UDS1",
  uds2 = "UDS2",
  uds2_1 = "UDS2",
  uds2_2 = "UDS2",
  udw = "UDW",
  udw_1 = "UDW",
  udw_2 = "UDW",
  hh_loc_reg4 = "ppreg4", 
  hh_loc_reg9 = "ppreg9",
  # is_weekend2 = "Created",
  tm_start_cdt = "Created", 
  tm_end_cdt = "Created",
  tm_start_lt = "Created",
  tm_start_lt = "Created",
  # timezone = "Created",
  wave = "Created",
  weight = "weight",
  total.contacts = "Created",
  cont_setting_nothh_n = "Created"
)

# var descriptions
codebook_resp$description = 
  c(resp_id = "ID of respondent",
    cont = "Does respondent have contacts? (Yes or No)",
    sample_g = "Sample group. Identifies whether respondent is an adult or child (identified as parent)",
    resp_resp_sick = "Whether respondent was sick yesterday",
    resp_hhm_sick = "Whether a member of respondent's household was sick yesterday",
    resp_industry_group = "Industry in respondent's current or main job",
    resp_occupation_group = "Occupation in respondent's current or main job",
    resp_mc_job = "Whether respondent worked in a job with multiple contacts",
    mc_ncont = "If your job (non-educator) requires you to interact with more than 30 people, list how many",
    mult2_n = "Number of contacts for respondent with either 30 or less multiple contacts", 
    add_mult_n = "Number of contacts for respondent with more than 30 multiple contacts",
    resp_educator = "Whether respondent works as an educator",
    resp_educator_mc = "Whether respondent is an educator with multiple contacts", 
    resp_edun_02 = "How many student contacts aged 0-2 do educators with multiple contacts have?",
    resp_edun_34 = "How many student contacts aged 3-4 do educators with multiple contacts have?", 
    resp_edun_59 = "How many student contacts aged 5-9 do educators with multiple contacts have?",
    resp_edun_1014 = "How many student contacts aged 10-14 do educators with multiple contacts have?",
    resp_edun_1517 = "How many student contacts aged 15-17 do educators with multiple contacts have?",
    resp_edun_18 = "How many student contacts aged 18+ do educators with multiple contacts have?",
    edu2_n = "Number of contacts for educator with either 30 or less multiple contacts",
    add_edu_n = "Number of contacts for educator with more than 30 multiple contacts",
    resp_gender = "Gender of respondent",
    resp_age = "Age of respondent", 
    # resp_age_adhoc2 = "Age group with 17 levels", 
    resp_age_insightnet = "Age group with 8 levels",
    resp_age_by5_80 = "Age group with 17 levels",
    resp_ethn = "Race and ethnicity of respondent", 
    hhsize = "Household size of respondent", 
    hh_income = "Household income of respondent", 
    ppwork = "Combination of ppworka_1-3; Work or student status of respondent",
    ppworka_1 = "Is respondent a student?",
    ppworka_2 = "Is respondent employed?",
    ppworka_3 = "Is respondent self-employed?",
    ppschoola = "For children 0-8, what is the school status of respondent?", 
    ppschoolb = "For children 9-17 and adults, what is the school status of respondent?", 
    uds1 = "For children 0-8, did respondent attend in person or online?",
    uds2 = "For students 9-17 and adults, did respondent attend school in person or online?", 
    uds2_1 = "For students 9-17 and adults, did respondent attend school in person?", 
    uds2_2 = "For students 9-17 and adults, did respondent attend school online?", 
    udw = "For employed and/or self-employed, did respondent attend work in person or online?", 
    udw_1 = "For employed and/or self-employed, did respondent attend work in person?", 
    udw_2 = "For employed and/or self-employed, did respondent attend work online?", 
    hh_loc_reg4 = "What is the geographic region of respondent? 4 categories", 
    hh_loc_reg9 = "What is the geographic region of respondent? 9 categories", 
    # is_weekend2 = "Did respondent fill out the survey on the weekday or weekend?", 
    tm_start_cdt = "What is respondent's starting time of the survey? (CDT)", 
    tm_end_cdt = "What is respondent's finishing time of the survey? (CDT)",
    tm_start_lt = "What is respondent's starting time of the survey?", 
    tm_end_lt = "What is respondent's finishing time of the survey?",
    # timezone = "What is respondent's time zone?",
    wave = "Respondent survey wave",
    weight = "Respondent weight",
    total.contacts = "Total contacts for respondent",
    cont_setting_nothh_n = "Total non-household contacts for respondent"
  )

write.csv(codebook_resp,
          paste0(output.dir, "/codebook_resp.csv"),
          row.names = F)
