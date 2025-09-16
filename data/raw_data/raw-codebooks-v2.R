#### Raw Data - Codebooks #####
rm(list = ls())

#### Environment ####
my.folder = "/your-path/Epistorm-Mix"
setwd(my.folder)
data.dir = "./raw_data/"
output.dir = "./raw_data/" 

# load the raw data
source(paste0(data.dir,"raw-data-v2.R"))

#### Codebook: Raw Contact Data ####

codebook_contact = data.frame(
  variable = rep(NA, times = ncol(raw_cont.df)),
  description = rep(NA, times = ncol(raw_cont.df)),
  response_options = rep(NA, times = ncol(raw_cont.df)),
  original_name = rep(NA, times = ncol(raw_cont.df))
)

# add var name and levels if categorical
for (i in 1:ncol(raw_cont.df)) {
  codebook_contact$variable[i] = colnames(raw_cont.df)[i]
  codebook_contact$response_options[i] = paste0(levels(raw_cont.df[,codebook_contact$variable[i]]),
                                                collapse = "; ")
  if (is.null(levels(raw_cont.df[,codebook_contact$variable[i]]))) {
    codebook_contact$response_options[i] = "Numeric"
  }
}

# var name from instrument  
codebook_contact$original_name = c(
  resp_id = "CaseID",
  cont_setting = "Created",
  cont_relationship = "RELATIONSHIP",
  cont_age = "C_AGE",
  cont_age_by5_80 = "Created",
  cont_age_insightnet = "Created",
  cont_ethn = "C_RACE_ETHNIC",
  cont_gender3 = "C_GENDER"
)

# description of the vars
codebook_contact$description = c(
  resp_id = "ID of respondent",
  cont_setting =  "Where did the contact occur?",
  cont_relationship = "What is the relationship of the contact to respondent?",
  cont_age = "What is the age of respondent's contact?",
  cont_age_by5_80 = "Age group with 17 levels",
  cont_age_insightnet = "Age group with 8 levels",
  cont_ethn = "What is the race/ethnicity of respondent's contact?",
  cont_gender3 = "What is the gender of respondent's contact?"
)

# save the codebook 
write.csv(codebook_contact,
          paste0(output.dir, "/codebook_contact.csv"),
          row.names = F)

#### Codebook: Raw Respondent Data ####
codebook_resp = data.frame(
  variable = rep(NA, times = ncol(raw_resp.df)),
  description = rep(NA, times = ncol(raw_resp.df)),
  response_options = rep(NA, times = ncol(raw_resp.df)),
  original_name = rep(NA, times = ncol(raw_resp.df))
)

# add var name and levels
for (i in 1:ncol(raw_resp.df)) {
  codebook_resp$variable[i] = colnames(raw_resp.df)[i]
  codebook_resp$response_options[i] = paste0(levels(raw_resp.df[,codebook_resp$variable[i]]),
                                             collapse = "; ")
  if (is.null(levels(raw_resp.df[,codebook_resp$variable[i]]))) {
    codebook_resp$response_options[i] = "Numeric"
  }
}

# fix levels 
for (i in 1:ncol(raw_resp.df)) {
  if (length(levels(raw_resp.df[,codebook_resp$variable[i]])) == 3 &
      "Refused" %in% levels(raw_resp.df[,codebook_resp$variable[i]])) {
    raw_resp.df[,codebook_resp$variable[i]] = factor(raw_resp.df[,codebook_resp$variable[i]],
                                                     levels = c("Yes", "No", "Refused"))
    codebook_resp$response_options[i] = paste0(levels(raw_resp.df[,codebook_resp$variable[i]]),
                                               collapse = "; ")
  }
  
  if (length(levels(raw_resp.df[,codebook_resp$variable[i]])) == 2 &
      "Yes" %in% levels(raw_resp.df[,codebook_resp$variable[i]])) {
    raw_resp.df[,codebook_resp$variable[i]] = factor(raw_resp.df[,codebook_resp$variable[i]],
                                                     levels = c("Yes", "No"))
    codebook_resp$response_options[i] = paste0(levels(raw_resp.df[,codebook_resp$variable[i]]),
                                               collapse = "; ")
  }
}

# var name from instrument  
codebook_resp$original_name = c(
  caseid = "CaseID",
  sample_g = "Xepistorm",
  resp_resp_sick = "Sick1",
  resp_hhm_sick = "Sick2",
  resp_mc_job = "MC1",
  mc_ncont = "MC1",
  resp_educator = "EDU",
  resp_educator_mc = "EDU_MC",
  resp_edun_02 = "EDU_Ncont1",
  resp_edun_34 = "EDU_Ncont2",
  resp_edun_59 = "EDU_Ncont3",
  resp_edun_1014 = "EDU_Ncont4",
  resp_edun_1517 = "EDU_Ncont5",
  resp_edun_18 = "EDU_Ncont6",
  resp_gender3 = "ppgender",
  resp_age = "ppage",
  resp_age_by5_80 = "Created",
  resp_age_insightnet = "Created",
  resp_ethn = "ppethm",
  pphhsize = "pphhsize",
  hh_income = "ppinc7",
  ppworka_1 = "PPWORKA",
  ppworka_2 = "PPWORKA",
  ppworka_3 = "PPWORKA",
  ppschoola = "PPSCHOOLA",
  ppschoolb = "PPSCHOOLB",
  uds1 = "UDS1",
  uds2_1 = "UDS2",
  uds2_2 = "UDS2",
  udw_1 = "UDW",
  udw_2 = "UDW",
  hh_loc_reg4 = "ppreg4", 
  hh_loc_reg9 = "ppreg9",
  tm_start_cdt = "Created", 
  tm_end_cdt = "Created",
  timezone = "Created",
  wave = "Provided by Ipsos",
  weight = "Provided by Ipsos"
)

# var descriptions
codebook_resp$description = 
  c(caseid = "ID of respondent",
    sample_g = "Sample group. Identifies whether respondent is an adult or child (identified as parent)",
    resp_resp_sick = "Whether respondent was sick yesterday",
    resp_hhm_sick = "Whether a member of respondent's household was sick yesterday", 
    resp_mc_job = "Whether respondent worked in a job with multiple contacts",
    mc_ncont = "If your job (non-educator) requires you to interact with more than 30 people, list how many",
    resp_educator = "Whether respondent works as an educator",
    resp_educator_mc = "Whether respondent is an educator with multiple contacts", 
    resp_edun_02 = "How many student contacts aged 0-2 do educators with multiple contacts have?",
    resp_edun_34 = "How many student contacts aged 3-4 do educators with multiple contacts have?", 
    resp_edun_59 = "How many student contacts aged 5-9 do educators with multiple contacts have?",
    resp_edun_1014 = "How many student contacts aged 10-14 do educators with multiple contacts have?",
    resp_edun_1517 = "How many student contacts aged 15-17 do educators with multiple contacts have?",
    resp_edun_18 = "How many student contacts aged 18+ do educators with multiple contacts have?",
    resp_gender3 = "Gender of respondent",
    resp_age = "Age of respondent", 
    resp_age_by5_80 = "Age group with 17 levels",
    resp_age_insightnet = "Age group with 8 levels",
    resp_ethn = "Race and ethnicity of respondent", 
    pphhsize = "Household size of respondent", 
    hh_income = "Household income of respondent", 
    ppworka_1 = "Is respondent a student?",
    ppworka_2 = "Is respondent employed?",
    ppworka_3 = "Is respondent self-employed?", 
    ppschoola = "For children 0-8, what is the school status of respondent?", 
    ppschoolb = "For children 9-17 and adults, what is the school status of respondent?", 
    uds1 = "For children 0-8, did respondent attend in person or online?",
    uds2_1 = "For students 9-17 and adults, did respondent attend school in person?", 
    uds2_2 = "For students 9-17 and adults, did respondent attend school online?", 
    udw_1 = "For employed and/or self-employed, did respondent attend work in person?", 
    udw_2 = "For employed and/or self-employed, did respondent attend work online?", 
    hh_loc_reg4 = "What is the geographic region of respondent? 4 categories", 
    hh_loc_reg9 = "What is the geographic region of respondent? 9 categories", 
    tm_start_cdt = "What is respondent's starting time of the survey? (Chicago Time)", 
    tm_end_cdt = "What is respondent's finishing time of the survey? (Chicago Time)",
    timezone = "What is respondent's time zone?",
    wave = "Which wave did respondent participate in?",
    weight = "Respondent weight"
  )

# save the codebook 
write.csv(codebook_resp,
          paste0(output.dir, "/codebook_resp.csv"),
          row.names = F)

