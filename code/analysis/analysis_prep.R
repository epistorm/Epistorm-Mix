#### Data preparations + Main Figures & Tables ####

## Data Directory
if (!exists("data.dir")){
  data.dir = file.path("data","imputed_data")
}

#### Functions #### 
## If the global directory for the code is set, use it for sourcing functions
if(exists("global.dir")){
  func.dir = file.path(global.dir, "func_analysis.R")
}else{
  func.dir = "func_analysis.R"
}
source(func.dir)

#### Packages and Libraries ####

ps = c("dplyr", "survey", "tidyverse", "ggplot2","flextable", "gt", "gtsummary",
       "fitdistrplus", "mgcv", "sjPlot", "patchwork", "doParallel", "extrafont",
       "data.table", "psych", "Hmisc", "readxl")

lapply(ps, function(x) load.myps(x))


##### Colors ####

mycol= c("#800080", "#5f875f", "#d75f87", "#d7afff", "#af5f5f", "#af87d7",
         "#af5f87", "#87afd7", "#87afaf", "#87af5f", "#635375", "#8fb2c7",
         "#5f5f87", "#005f87", "#5f5fd7", "#21929b")


#### Read Clean Data ####

merged.df = read.csv(file.path(data.dir, "imputed-contacts2.csv"))
sum.res3 = read.csv(file.path(data.dir, "imputed-resp2.csv"))

#### Create Age Groups ####

age.vars.breaks = list(seq(0,120, by = 5),
  c(seq(0,65, by = 5), 120),
  c(seq(0,75, by = 5), 120),
  c(seq(0,80, by = 5), 120),
  seq(0,120, by = 15),
  c(seq(0,75, by = 15), 120),
  c(seq(0,70, by = 10), 120),
  c(0,5,10,18,seq(30,60, by=10),120),
  c(0,5,10,15, 18, seq(25,75, by=5), 120),
  c(0,3,6,11,14,18,22,26,seq(30, 80, by=5) ,120),
  c(0,5,10,15, 18, seq(20,75, by=5), 120))

age.vars.names = c(paste0("_",c("by5", "by5_65", "by5_75","by5_80","by15",
               "by15_75","by10_70", "insightnet","adhoc", "schoolage","adhoc2")))

sum.res3 = create.age.groups(sum.res3, input.var = "resp_age", # resp age
                             break.list = age.vars.breaks,
                             suff.list = age.vars.names)
merged.df = create.age.groups(merged.df, input.var = "cont_age", # contact age
                              break.list = age.vars.breaks,
                              suff.list = age.vars.names)

## define Ipsos time zone
sum.res3$tm_start_cdt = as.POSIXct(as.character(sum.res3$tm_start_cdt),
                                   format = "%Y-%m-%d %H:%M:%OS",  # Adjust if your format is different
                                   tz = "America/Chicago") # Chicago is in the Central Time Zone

sum.res3$tm_end_cdt = as.POSIXct(as.character(sum.res3$tm_end_cdt),
                                 format = "%Y-%m-%d %H:%M:%OS",  # Adjust if your format is different
                                 tz = "America/Chicago") # Chicago is in the Central Time Zone

day.threshold = 4 ## time the day starts = 4 am

sum.res3$time_start = as.POSIXct(sum.res3$tm_start_lt,
                                 format = "%Y-%m-%d %H:%M")

sum.res3$time_end = as.POSIXct(sum.res3$tm_end_lt,
                               format = "%Y-%m-%d %H:%M")

sum.res3$diary_date = as.Date(sapply(seq_along(sum.res3$time_start), function(i)
  adjust_date(dt = sum.res3$time_start[i], 
              cutoff = day.threshold)))

sum.res3$diary_date_end = as.Date(sapply(seq_along(sum.res3$time_end), function(i)
  adjust_date(dt = sum.res3$time_end[i], 
              cutoff = day.threshold)))

## adjust for those starting on the edge between days
idx.edge = which(sum.res3$diary_date_end - sum.res3$diary_date < 0)
sum.res3$diary_date_end[idx.edge] = sapply(idx.edge, function(i)
  adjust_date(dt = sum.res3$tm_start_cdt[i],
              cutoff = NULL))

## define weekday
sum.res3$wd_start = weekdays(sum.res3$diary_date)
sum.res3$wd_end = weekdays(sum.res3$diary_date_end)

## calculate survey length
sum.res3$surv.length = round((sum.res3$time_end - sum.res3$time_start)/60,2)

# check the time for respondents who take long to complete the survey
idx.longsurvey = which((sum.res3$diary_date_end - sum.res3$diary_date) > 0 &
                         sum.res3$surv.length >= 7)

# define weekend
sum.res3$is_weekend2 = weekdays(sum.res3$diary_date) %in% c("Saturday", "Sunday")

# Infer/Adjust the weekend/weekday based on the distribution of sum.res3 time
# for respondents taking long time
if(length(idx.longsurvey) > 0){
  tmp.date = sum.res3[idx.longsurvey, c("resp_id","diary_date","diary_date_end")]
  tmp.date$right = as.numeric(tmp.date$diary_date_end - tmp.date$diary_date) 
  
  my.distr = fitDistr(tmp.date[,c("right")],
                      distr.names = "lnorm", #c("gamma", "weibull", "lnorm","nbinom", "geom") ## all of these distr were originally tested
                      outputdir = file.path(output.dir,"date.estimation.pdf"),
                      zero.augm=0.1)

  gof = unlist(lapply(my.distr$dist, function(x)
    if(length(x) > 1){ AIC(x) }else{ NA }))
  idx.best.fit = which(gof == min(gof, na.rm = T))
  
  ### estimate the probability of filling in the survey on a weekend for each obs
  sum.res3$is_weekend2[idx.longsurvey] = sapply(idx.longsurvey, function(i){
    nright = as.numeric(sum.res3$diary_date_end[i] - sum.res3$diary_date[i])
    i.prob = my.distr$mean.estim[[idx.best.fit]][c(0:nright+1)]
    my.days = weekdays(sum.res3$diary_date[i]+c(0:nright))
    weekend.prob = sum((my.days %in% c("Saturday", "Sunday"))*i.prob)/sum(i.prob)
    is_weekend = ifelse(weekend.prob > 0.5, T, F)
    return(is_weekend)
  })
}

### add state and federal holidays to weekends
holidays = read.csv("./data/external_data/federal-state-holidays.csv")
holidays.slist = strsplit(holidays$States, ", ")

for(i in 1:nrow(holidays)){
  if(holidays$Type[i] == "State Holiday"){
    i.states = holidays.slist[[i]]
    idx.hol = which(sum.res3$state %in% i.states &
                      sum.res3$diary_date == as.Date(holidays$Date[i], "%m/%d/%y"))
    sum.res3$is_weekend2[idx.hol] = TRUE
  }else if(holidays$Type[i] == "Federal Holiday"){
    idx.hol = which(sum.res3$diary_date == as.Date(holidays$Date[i], "%m/%d/%y"))
    sum.res3$is_weekend2[idx.hol] = TRUE
  }
}

## Define weeks

sum.res3$date.start = as.Date(sum.res3$time_start)

w1.start = min(sum.res3$date.start[which(sum.res3$wave == "Wave 1")])
w2.start = min(sum.res3$date.start[which(sum.res3$wave == "Wave 2")])

week.breaks = c(w1.start+seq(0,21, by=7),
                w2.start+seq(0,28, by=7))

sum.res3$weeks = cut(sum.res3$date.start, breaks = week.breaks)
sum.res3$weeks = factor(as.character(sum.res3$weeks),
                        levels = levels(sum.res3$weeks),
                        labels = paste0(format(week.breaks[1:(length(week.breaks)-1)], "%B,%d"),
                                        " - ",
                                        format(week.breaks[2:length(week.breaks)], "%B,%d")))

sum.res3$weeks = droplevels(sum.res3$weeks)
print(table(sum.res3$weeks, useNA = "ifany"))

### Encode properly the variables
sum.res3$hhsize2 = factor(sum.res3$hhsize,
                            levels = seq(1,max(sum.res3$hhsize)),
                            labels = c(as.character(1:6),
                                       rep("7+", max(sum.res3$hhsize)-6)))
sum.res3$hhsize3 = factor(sum.res3$hhsize,
                            levels = seq(1,max(sum.res3$hhsize)),
                            labels = c(as.character(1:8),
                                       rep("9+", max(sum.res3$hhsize)-8)))

sum.res3$is_weekend2 = factor(sum.res3$is_weekend2,
                              levels = c(FALSE, TRUE),
                              labels = c("Weekday", "Weekend"))

sum.res3$resp_resp_sick = factor(as.character(sum.res3$resp_resp_sick),
                                 levels = c("No", "Yes", "Refused"))

sum.res3$resp_hhm_sick = factor(as.character(sum.res3$resp_hhm_sick),
                                levels = c("No, not that I know of", "Yes",
                                           "Refused"))

sum.res3$hh_income = factor(as.character(sum.res3$hh_income),
                            levels = c("Less than $10,000",
                                       "$10,000 to $24,999",
                                       "$25,000 to $49,999",
                                       "$50,000 to $74,999",
                                       "$75,000 to $99,999",
                                       "$100,000 to $149,999",
                                       "$150,000 or more"))

sum.res3$resp_ethn = factor(as.character(sum.res3$resp_ethn),
                            levels = c("White, Non-Hispanic",
                                       "Hispanic/Latino",
                                       "Black, Non-Hispanic",
                                       "Asian, Non-Hispanic",
                                       "Other, Non-Hispanic"))

sum.res3$ppwork = factor(sum.res3$ppwork, str_to_title(c("Student", "Employed", "Self-Employed",
                                            "Employed & Self-Employed", 
                                            "Student & (Self-)Employed",
                                            "Other Non-Occupied")))

levels(sum.res3$ppwork)[which(levels(sum.res3$ppwork) == "Other Non-Occupied")] =
  "Other"

#### Adjust sex variable patch ####
idx.other_r = which(sum.res3$resp_gender == "Other")
idx.other_c = which(merged.df$cont_gender == "Other")

sum.res3$resp_sex = sum.res3$resp_gender
sum.res3$resp_sex[idx.other_r] = NA
sum.res3$resp_sex = as.factor(sum.res3$resp_sex)

merged.df$cont_sex = merged.df$cont_gender
merged.df$cont_sex[idx.other_c] = NA
merged.df$cont_sex = as.factor(merged.df$cont_sex)

### Create Mode of Attendance + Main Activity

sum.res3$inperson_online = NA ## were you at work yesterday
idx.empl = which(sum.res3$ppwork == "Employed" |
                   sum.res3$ppwork == "Self-Employed" |
                   sum.res3$ppwork == "Employed & Self-Employed")
sum.res3$inperson_online[idx.empl] = as.character(sum.res3$udw[idx.empl])

idx.students = which(sum.res3$ppwork == "Student")

## get mode of attendance based on the survey design 
## (those <9 years of age were asked directly about school attendance)
sum.res3$inperson_online[idx.students] = ifelse((sum.res3$uds2[idx.students] == "Not asked"| 
                                                  is.na(sum.res3$uds2[idx.students])),
                                                as.character(sum.res3$uds1[idx.students]), 
                                                as.character(sum.res3$uds2[idx.students]))

# all of those in idx.homeschooled should be defined as not students in ppwork
idx.homeschooled = which(sum.res3$ppschoola == "Home schooled" |
                           sum.res3$ppschoolb == "Home schooled" |
                           sum.res3$ppschoola == "Other" )
# check 
idx.hs.st = intersect(idx.homeschooled, which(sum.res3$ppwork == "Student"))
if(length(idx.hs.st) > 0){
  print("Some homeschooled are classified as students")
}

# Define mode of attendance for those in two main activities (e.g., students and workers)
idx.stw = which(sum.res3$ppwork == "Student & (Self-)Employed")

# those that have the same attendance mode in both work and school
sum.res3$inperson_online[intersect(idx.stw, which(sum.res3$uds2 == sum.res3$udw))] =
  as.character(sum.res3$uds2[intersect(idx.stw, which(sum.res3$uds2 == sum.res3$udw))])

# those that are in-person in at least one & are not yet defined -> "Yes, mixed"
idx.yes = setdiff(Reduce(intersect, sapply(c("uds2","udw"), function(x) 
  intersect(grep("Yes",sum.res3[,x]), which(is.na(sum.res3$inperson_online))))),
  which(sum.res3$ppwork == "Other"))
sum.res3$inperson_online[idx.yes] = "Yes, mixed"

# those that are not in person in either & are not yet defined -> "No"
idx.online_no = setdiff(intersect(which((sum.res3$uds2 == "Yes, online" & sum.res3$udw == "No") |
                                  (sum.res3$uds2 == "No" & sum.res3$udw == "Yes, online") |
                                  (sum.res3$uds1 == "Yes, online" & sum.res3$udw == "No") |
                                  (sum.res3$uds1 == "No" & sum.res3$udw == "Yes, online")),
                          which(is.na(sum.res3$inperson_online))),
                        which(sum.res3$ppwork == "Other"))
sum.res3$inperson_online[idx.online_no] = "No"

# the rest go into mixed
idx.sum.res3t = Reduce(setdiff, list(idx.stw, which(sum.res3$uds2 == sum.res3$udw), 
                                     idx.yes, idx.online_no))
sum.res3$inperson_online[idx.sum.res3t] = "Mixed"

# define levels
sum.res3$inperson_online = factor(sum.res3$inperson_online, 
                                  levels = c("No", "Yes, in person", "Yes, online", 
                                             "Yes, hybrid", "Yes, mixed", "Mixed")) 

## aggregate mode of attendance
sum.res3$inperson_online_agg = as.character(sum.res3$inperson_online)
idx.anyonline = which(sum.res3$inperson_online_agg %in% 
                        c("Yes, hybrid", "Yes, mixed", "Mixed"))
sum.res3$inperson_online_agg[idx.anyonline] = "Yes, in person"
sum.res3$inperson_online_agg = factor(sum.res3$inperson_online_agg,
                                      levels = c("No", "Yes, in person", "Yes, online")) 
## rename "Other" group
sum.res3$nothome = as.character(sum.res3$inperson_online_agg)
sum.res3$nothome[which(sum.res3$ppwork == "Other")] = "Not Student or Employed"

## re-label
sum.res3$nothome = factor(sum.res3$nothome,
                          levels = c("Yes, in person", "Yes, online",
                                     "No",  
                                     "Not Student or Employed"),
                          labels = c("At work/In school in person",
                                     "Worked/studied fully online",
                                     "Not at work/in school",
                                     "Not Student or Employed"))

### Correct data for misunderstood reference day (school in-person)
### students who said they were in-person in school on a weekend but have
### no contacts in school # 12 individuals
idx.misundrst1 = intersect(which(sum.res3$nothome == "At work/In school in person" &
                                   sum.res3$is_weekend2 == "Weekend" &
                                   sum.res3$ppwork == "Student"),
                           which(!sum.res3$resp_id %in%
                                   merged.df$resp_id[merged.df$cont_setting == 
                                                       "School"]))
sum.res3$nothome[idx.misundrst1] = "Not at work/in school"


## aggregate work status
sum.res3$ppwork2 = sum.res3$ppwork

# Employed & Self-Employed -> Employed
idx_selfemployed = which(levels(sum.res3$ppwork) == "Employed & Self-Employed")
levels(sum.res3$ppwork2)[idx_selfemployed] = "Employed"
# levels(sum.res3$ppwork2) # check that recoding levels worked

## create interaction between Main Activity and Mode of Attendance
sum.res3$ppwork_nothome = interaction(sum.res3$nothome, sum.res3$ppwork2, sep = "_")
sum.res3$ppwork_nothome = droplevels(sum.res3$ppwork_nothome)
# table(sum.res3$ppwork_nothome)

## Adjust labels to be relevant for selected Main Activity
sum.res3$ppwork_nothome = as.character(sum.res3$ppwork_nothome)
idx.st.only = setdiff(grep("Student", sum.res3$ppwork_nothome), 
                      grep("Employed", sum.res3$ppwork_nothome))
idx.w.only = setdiff(grep("Employed", sum.res3$ppwork_nothome), 
                     grep("Student", sum.res3$ppwork_nothome))

sum.res3$ppwork_nothome[idx.w.only] = gsub("Not at work/in school","Not at work", 
                                           gsub("At work/In school","At work", 
                                                gsub("Worked/studied", "Worked", 
                                                     sum.res3$ppwork_nothome[idx.w.only])))
sum.res3$ppwork_nothome[idx.st.only] = gsub("Not at work/in school","Not at school", 
                                            gsub("At work/In school","At school",
                                                 gsub("Worked/studied", "Studied", 
                                                      sum.res3$ppwork_nothome[idx.st.only])))
sum.res3$ppwork_nothome = as.factor(sum.res3$ppwork_nothome)

# re-level the interaction
sum.res3$ppwork_nothome = relevel(sum.res3$ppwork_nothome, ref = "At school in person_Student")

# new data
merged.df = left_join(merged.df, sum.res3, by = "resp_id")

# move teachers' contacts into school before tabulating number of contacts
idx.students = which(merged.df$cont_relationship == "Student" &
                       merged.df$resp_industry == "Education, Tutoring, and Child Care")
merged.df$cont_setting[idx.students] = "School"

## number of contacts
tmp = merged.df %>%
  group_by(resp_id) %>%
  summarise(cont_setting_school_n = sum(cont_setting == "School", na.rm = T),
            cont_setting_work_n = sum(cont_setting == "Work", na.rm = T),
            cont_setting_other_n = sum(cont_setting == "Other", na.rm = T),
            cont_setting_hh_n = sum(cont_setting == "Household", na.rm = T),
            .groups = "drop")
sum.res3 = left_join(sum.res3, tmp, by = "resp_id")

## the dummies identifying if respondents had contact (in a particular setting) ##
# total
cont_cols = grep("^cont_", colnames(merged.df), value = TRUE)
nocont = rowSums(!is.na(merged.df[, cont_cols])) == 0
merged.df$binary_col = ifelse(nocont, 0, 1)

# specific
merged.df$hh_col = ifelse(merged.df$cont_setting == "Household", 1, 0)
merged.df$school_col = ifelse(merged.df$cont_setting == "School", 1, 0)
merged.df$work_col = ifelse(merged.df$cont_setting == "Work", 1, 0)
merged.df$other_col = ifelse(merged.df$cont_setting == "Other", 1, 0)

# for analyses
setting_names = c("binary_col", "hh_col", "school_col", "work_col", "other_col")

