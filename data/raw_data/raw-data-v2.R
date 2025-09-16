#### Raw Data #####
rm(list = ls())

#### Environment ####
my.folder = "/your-path/Epistorm-Mix"
setwd(my.folder)
data.dir = "./raw_data/"
output.dir = "./raw_data/" 

#### Functions #### 
source("./code/func.R")

#### Packages and Libraries ####
ps = c("tidyverse","dplyr", "tools","zipcodeR")
lapply(ps, function(x) load.myps(x))

#### Data ####
data.files = list.files(data.dir, ".Rdata")
k = 3
if(length(data.files) == 1 & grepl("all-waves",data.files)){
  load(paste0(data.dir,"/", data.files))
  orig.merged.df = merged.df
  #### temporary fixes: ####
  ## lost weight observations during cleaning
  names(res) = tolower(names(res))
  # Perform the left join using the standardized 'caseid'
  merged_result = left_join(merged.df, res, by = "caseid", keep = TRUE)
  # Ensure that 'weight' is kept after merging (if needed)
  merged_result = merged_result %>%
    mutate(weight = coalesce(weight.x, weight.y))  # Preserve 'weight' column
  merged.df$weight = merged_result$weight
  
  ## not the same encoding in ethnicity variables
  ethn.vars = grep("ethn", colnames(merged.df), value = T)
  merged.df$cont_ethn[which(merged.df$cont_ethn == "Other")] = "Other, Non-Hispanic"
  
  ## lost "cont_relationship" for household
  merged.df$cont_relationship[which(merged.df$cont_setting == "hh")] = 
    "person [my child lives/i live] with"
  df_list = split(merged.df, merged.df$wave)
  df_list$all_waves = merged.df
}else{
  stop("multiple files - > adjust code organization")
}
merged.df = df_list[[k]]

#### Age Groups ####

age.vars.breaks = list(c(seq(0,80, by = 5), 120),
                       c(0,5,10,18,seq(30,60, by=10),120))

age.vars.names = c(paste0("_",c("by5_80","insightnet")))

sum.res2 = create.age.groups(sum.res2, input.var = "resp_age", # resp age
                             break.list = age.vars.breaks,
                             suff.list = age.vars.names)
merged.df = create.age.groups(merged.df, input.var = "cont_age", # contact age
                              break.list = age.vars.breaks,
                              suff.list = age.vars.names)

#### Contact ####

# select variables 
tokeep_c = c("caseid",
           "cont_setting", "cont_relationship",
           "cont_age", 
           "cont_age_by5_80", "cont_age_insightnet",
           "cont_ethn", "cont_gender3")
raw_cont.df = merged.df[, tokeep_c]

# change column name for caseid 
colnames(raw_cont.df)[1] = "resp_id"

#### save the data ####
str(raw_cont.df)

# change the labels for setting
levels(raw_cont.df$cont_setting) = c("Household", "School", "Work", "Other", "Not Household")

## PATCH 
wrong.r = which(raw_cont.df$cont_relationship == "classmate/other schoolmate" |
                  raw_cont.df$cont_relationship == "not in my household")
if (length(wrong.r) > 0) {
  raw_cont.df = raw_cont.df[-wrong.r,]
}

## PATCH 
raw_cont.df$cont_relationship = as.factor(raw_cont.df$cont_relationship)
raw_cont.df$cont_relationship = droplevels(raw_cont.df$cont_relationship)
levels(raw_cont.df$cont_relationship) = toTitleCase(levels(raw_cont.df$cont_relationship))

# change to factor
raw_cont.df$cont_ethn = factor(raw_cont.df$cont_ethn, 
                               levels =  c("White, Non-Hispanic", "Hispanic/Latino", 
                                           "Black, Non-Hispanic", "Asian, Non-Hispanic",
                                           "Other, Non-Hispanic", "Refused"))
raw_cont.df$cont_gender3 = as.factor(raw_cont.df$cont_gender3)

write.csv(raw_cont.df,
          paste0(output.dir, "raw_contact.csv"),
          row.names = F)

#### Respondent ####

# select variables
tokeep_r = c("caseid",
           "sample_g", ## sample group (parents/children vs. adults)
           "resp_resp_sick", "resp_hhm_sick", ## sick
           "resp_mc_job", "mc_ncont", ## multiple/numerous contacts
           "resp_educator", "resp_educator_mc", "resp_edun_02","resp_edun_34", ## educational numerous/multiple contacts
           "resp_edun_59", "resp_edun_1014", "resp_edun_1517", "resp_edun_18", 
           "resp_gender3", ## gender
           "resp_age", ## age
           "resp_age_by5_80", "resp_age_insightnet",
           "resp_ethn", "pphhsize", "hh_income", ## other socio-demographics
           "ppworka_1","ppworka_2","ppworka_3", ## main ctivity
           "ppschoola", "ppschoolb", ## school enrollment
           "uds1", "uds2_1", "uds2_2", "udw_1", "udw_2", ## school/work mode (in-person vs online)
           "hh_loc_reg4", "hh_loc_reg9", ## location
           "tm_start_cdt", "tm_end_cdt", "timezone", ## date, time, weekend
           "wave", "weight") # wave

#### ipsos time zone for start/finish times ####
sum.res2$tm_start_cdt = as.POSIXct(as.character(sum.res2$tm_start),
                                   format = "%Y-%m-%d %H:%M:%S",  # Adjust if your format is different
                                   tz = "America/Chicago") # Chicago is in the Central Time Zone

sum.res2$tm_end_cdt = as.POSIXct(as.character(sum.res2$tm_finish),
                                 format = "%Y-%m-%d %H:%M:%S",  # Adjust if your format is different
                                 tz = "America/Chicago") # Chicago is in the Central Time Zone

#### respondent time zone ####
sum.res2$zip_char = ifelse(nchar(sum.res2$zip) == 4, 
                           sprintf("%05d", sum.res2$zip), # %05d means 5 digits with leading zeros
                           ifelse(nchar(sum.res2$zip) == 5,
                                  as.character(sum.res2$zip),
                                  NA))
## load zip-code- to county data + county descriptors (including time zone)
zipdata = zip_code_db
zipdata$zip_char = zipdata$zipcode

## define probability of belonging to a particular time zone
timezone.prob = zipdata[which(!is.na(zipdata$timezone)),] %>%
  group_by(state,timezone) %>% summarize(n = n())

timezone.prob = timezone.prob %>%
  group_by(state) %>%
  mutate(Prob = n/sum(n))

sum.res2 = left_join(sum.res2, zipdata[,-1], by = "zip_char")

## choose the most probable time zone
idx.nozip = which(sum.res2$zip_char == "99999" | is.na(sum.res2$timezone))
sum.res2$timezone[idx.nozip] = sapply(idx.nozip, function(x){
  myprob = timezone.prob[which(timezone.prob$state == as.character(sum.res2$hh_state[x])),]
  y = myprob$timezone[which(myprob$Prob == max(myprob$Prob))]
  return(y)
})
idx.addus = which(sum.res2$timezone %in% c("Central", "Eastern", "Mountain", "Pacific"))
sum.res2$timezone[idx.addus] = paste0("US/",sum.res2$timezone[idx.addus])
sum.res2$timezone = as.factor(sum.res2$timezone)

#### weights ####
sum.res2$weight_ipsos = sum.res2$weight
tmppp = sum.res2[,c("weight", "wave")] %>%
  group_by(wave) %>%
  mutate(weight = weight * n() / sum(weight)) %>%
  ungroup()
tmppp$weight[which(tmppp$wave == "wave2")] = ceiling(nrow(sum.res2)/2)*
  tmppp$weight[which(tmppp$wave == "wave2")]/
  sum(tmppp$weight[which(tmppp$wave == "wave2")])
tmppp$weight[which(tmppp$wave == "wave1")] = (nrow(sum.res2) -
                                                sum(tmppp$weight[which(tmppp$wave == "wave2")]))*
  tmppp$weight[which(tmppp$wave == "wave1")]/
  sum(tmppp$weight[which(tmppp$wave == "wave1")])

sum.res2$weight = tmppp$weight

#### change factor to numeric ####

sum.res2$mc_ncont = as.integer(sum.res2$mc_ncont)
sum.res2$resp_edun_02 = as.integer(sum.res2$resp_edun_02)
sum.res2$resp_edun_34 = as.integer(sum.res2$resp_edun_34)
sum.res2$resp_edun_59 = as.integer(sum.res2$resp_edun_59)
sum.res2$resp_edun_1014 = as.integer(sum.res2$resp_edun_1014)
sum.res2$resp_edun_1517 = as.integer(sum.res2$resp_edun_1517)
sum.res2$resp_edun_18 = as.integer(sum.res2$resp_edun_18)

#### relevel ####
sum.res2$resp_ethn = factor(sum.res2$resp_ethn,
                            levels = c("White, Non-Hispanic", "Hispanic/Latino", 
                               "Black, Non-Hispanic", "Asian, Non-Hispanic",
                               "Other, Non-Hispanic"))
sum.res2$hh_income = factor(sum.res2$hh_income, 
                            levels = c("Less than $10,000", "$10,000 to $24,999", 
                               "$25,000 to $49,999", "$50,000 to $74,999",
                               "$75,000 to $99,999", "$100,000 to $149,999",
                               "$150,000 or more"))
sum.res2$uds1 = factor(sum.res2$uds1, 
                       levels = c("Yes, in person", "Yes, online", "No"))
sum.res2$wave = factor(sum.res2$wave)
levels(sum.res2$wave) = c("Wave1", "Wave2")

#### save the data ####

raw_resp.df = sum.res2[, tokeep_r]
str(raw_resp.df)
write.csv(raw_resp.df,
          paste0(output.dir, "raw_resp.csv"),
          row.names = F)




