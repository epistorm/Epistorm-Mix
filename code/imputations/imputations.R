rm(list = ls())

#### Folders and Directories ####

data.dir = "../../data/raw_data"
output.dir = "../../data/imputed_data"

#### Functions #### 
## If the global directory for the code is set, use it for sourcing functions
if(length(global.dir) > 0){
  func.dir = paste0(global.dir, "func_imp.R")
}else{
  func.dir = "./func_imp.R"
}
source(func.dir)

#### Packages and Libraries ####

ps = c("tidyverse", "ggplot2", "dplyr", "randomForest", "doRNG", "foreach", 
       "doParallel")

lapply(ps, function(x) load.myps(x))

#### Read Clean Data ####

merged.df = read.csv(paste0(data.dir, "/raw_contact.csv"))
sum.res2 = read.csv(paste0(data.dir, "/raw_resp.csv"))

## save variable names from the raw datasets

varset.cont = colnames(merged.df)
varset.resp = colnames(sum.res2)

###### Define key respondent & contact (matrix) characteristics ######
key.vars = c("gender", "age", "ethn")

## contact ##
prefix = "cont_"
key.vars.cont = lapply(paste0(prefix, key.vars), function(x)
  grep(x, colnames(merged.df)))
names(key.vars.cont) = key.vars

## respondent ##

prefix = "resp_"
key.vars.resp = lapply(paste0(prefix, key.vars), function(x)
  grep(x, colnames(sum.res2)))
names(key.vars.cont) = key.vars

###### Define contact (link) characteristics  ######

add.cont.vars = paste0("cont_", c("setting", "relationship"))
table(merged.df[,add.cont.vars], useNA = "ifany")


###### Define hh characteristics   ######

hh.v = grep("hh_", colnames(sum.res2))

######  Contact age smoothing, if necessary  ########
# 
# ## redistributing the age of work and other contacts
# ## if they look rounded for multiple observations for the same respondent
# 
# # for specific resp_id of work and other contacts
# # are there > n_ppl adult contacts with the same rounded age (divisible by 5)
# # change all of those ages to be in the plus/minus "range" years range
# 
# n_ppl = 2
# range = 2
# seed = 12345
# 
# ## create the distribution of contact ages in the data
# age.dist = as.data.frame(table(merged.df$cont_age[merged.df$cont_setting %in%
#                                                c("Work", "Other")]))
# age.dist$Freq = age.dist$Freq/sum(age.dist$Freq)
# age.dist$Var1 = as.numeric(as.character(age.dist$Var1))
# 
# ## create a smoothing spline 
# smoother4 = smooth.spline(age.dist$Var1, age.dist$Freq, spar = 0.6)
# 
# ## plot resulting smoothing spline
# plot(age.dist$Var1, age.dist$Freq, type = "l", col = "lightblue")
# lines(smoother4, col = "purple")
# axis(1, at = seq(0, max(age.dist$Var1), 5))
# # smoother = smooth.spline(age.dist$Var1, age.dist$Freq, spar = 0.5)
# # smoother1 = smooth.spline(age.dist$Var1, age.dist$Freq, spar = 0.7) # low df
# # smoother2 = smooth.spline(age.dist$Var1, age.dist$Freq, spar = 0.3) # high df
# # smoother3 = smooth.spline(age.dist$Var1, age.dist$Freq, spar = 0.4)
# 
# ## for each respondent
# for (x in unique(merged.df$resp_id)) {
#   ## identify contacts in work and other setting
#   idx.conts = which(merged.df$resp_id == x &
#                       merged.df$cont_setting %in%
#                       c("Work", "Other"))
#   
#   ## create a frequency table for age of contacts in both settings
#   age.table = as.data.frame(table(merged.df$cont_age[idx.conts]))
#   ## transform age into numeric
#   age.table$Var1 = as.numeric(as.character(age.table$Var1))
#   ## find if there are more than n_ppl adults with rounded ages
#   check = age.table$Freq > n_ppl &
#     (age.table$Var1 %% 5 == 0) &
#     age.table$Var1 >= 18
#   ## if there are
#   if (any(check)) {
#     ## for each rounded age
#     for (age in age.table$Var1[check]) {
#       ## find contacts with rounded age
#       idx.repeated = idx.conts[which(merged.df$cont_age[idx.conts] == age)]
#       ## define bounds for smoothing
#       lower = age - range
#       upper = age + range
#       
#       ## use estimated smoothing spline to sample age 
#       ## from spline within defined bounds
#       age.dist1 = smoother4$y[smoother4$x >= lower &
#                                 smoother4$x <= upper]
#       ## scale spline within defined bounds (sum to 100%)
#       age.dist1 = age.dist1/sum(age.dist1)
#       ## sample age
#       new_ages = sample(c(lower:upper), 
#                          length(idx.repeated), 
#                          replace = T,
#                          prob = age.dist1)
#       # uncomment for printing the substitutions
#       # print(paste0("old ages: ", paste0(merged.df$cont_age[idx.repeated], collapse = ", ")))
#       # print(paste0("new ages: ", paste(new_ages, collapse = ", ")))
#       merged.df$cont_age[idx.repeated] = new_ages
#     }
#   }
# }


######## PERFORM MULTIPLE IMPUTATIONS (on main contact variables) ######## 

## define missing data for imputation
miss.info = c("Data loss", "Refused", "refused", "unknown", "Unknown", "-1")

## merge contact and respondent datasets

merged.df = left_join(merged.df, sum.res2, 
                      by = "resp_id",
                      keep = F)

## define variables to impute
cont.vars.mdf = colnames(merged.df)[unlist(key.vars.cont)]


## format the rest of the variables

#print(table(merged.df$resp_educator, useNA = "ifany"))
merged.df$resp_educator[is.na(merged.df$resp_educator)] = "No"
merged.df$resp_mc_job[is.na(merged.df$resp_mc_job)] = "No"
merged.df$resp_educator_mc[is.na(merged.df$resp_educator_mc)] = "No"

idx.survey.vars = grep("^pp|^ud", colnames(merged.df), value = T)
for(j in idx.survey.vars){
  print(j)
  merged.df[is.na(merged.df[,j]),j] = "Not asked"
  print(table(merged.df[,j], useNA = "ifany"))
}

## make sure all the variables are either numeric, factors, or dates
for(x in colnames(merged.df)){
  if(!is.numeric(merged.df[,x]) & !is.factor(merged.df[,x]) &
     !is.POSIXct(merged.df[,x])){
    if(grepl("^tm_", x)){
      print(x)
      merged.df[, x] = as.POSIXct(merged.df[, x])
      print(summary(merged.df[, x]))
    }else{
      tmp = table(merged.df[, x])
      if(sum(grepl("[0-9]", names(tmp))) == length(names(tmp))){
        cleaned_text = gsub("[$,]", "", names(tmp))
        numbers = sapply(str_extract_all(cleaned_text, "\\d+"), function(x) 
          as.numeric(x[1])+length(x)-2)
        my.lvls = names(tmp)[order(numbers, decreasing = F)]
      }else if(gsub("cont", "resp", x) %in% colnames(sum.res2)){
        if(is.factor(sum.res2[,gsub("cont", "resp", x)])){
          my.lvls = levels(sum.res2[,gsub("cont", "resp", x)])
          }else{
            my.lvls = names(tmp)[order(tmp, decreasing = T)]
          }
      }else{
        my.lvls = names(tmp)[order(tmp, decreasing = T)]
      }
      merged.df[, x] = factor(as.character(merged.df[, x]),
                              levels = my.lvls)
      print(table(merged.df[, x], useNA = "ifany"))
    }
  }
}

## Adding Contact ID
ids = unique(merged.df$resp_id)
merged.df$cont_id = NA
for(id in ids){
  idx = which(merged.df$resp_id == id)
  if(length(idx) > 0){
    if(sum(!is.na(merged.df[idx,c(cont.vars.mdf,add.cont.vars)])) == 0 & 
       length(idx) == 1){
      merged.df$cont_id[idx] = 0
      merged.df$cont_identifier[idx] = NA
    }else{
      merged.df$cont_id[idx] = c(1:length(idx))
    }
  }else{
    stop("IDs missing")
  }
}

## selecting the variables for imputations
vars.imp = setdiff(colnames(merged.df),
                   c("mc_ncont", 
                     grep("weight", colnames(merged.df), value = T),
                     grep("resp_edun_", colnames(merged.df), value = T)))

## defining the data for imputations
temp.data = merged.df[,vars.imp]

## removing the respondents with 0 contacts from the contact-level data
cont0.ids = which(temp.data$cont_id == 0)
if(length(cont0.ids) > 0){
  temp.data = temp.data[-cont0.ids,]
}

xxxx = lapply(miss.info, function(m) 
  sapply(c(cont.vars.mdf, "cont_relationship", "cont_setting"), function(x){
  print(paste0("Number of ", m, " in ", x, ":"))
  print(sum(merged.df[merged.df$cont_id > 0,x] == m, na.rm = T))
  return(NULL)
}))

                                    
## Checking how much missing data is present in key contact variables
all.missing = NULL
for(x in seq_along(cont.vars.mdf)){
  idx.miss = which(as.character(temp.data[[cont.vars.mdf[x]]]) %in% miss.info)
  all.missing = unique(c(all.missing, idx.miss))
  if(length(idx.miss) > 0){
    temp.data[idx.miss,cont.vars.mdf[x]] = NA
    print(paste0(cont.vars.mdf[x], ": ", 
                 round(100*length(idx.miss)/length(temp.data[[cont.vars.mdf[x]]]),1),
                 "% (", length(idx.miss), " out of ",
                 length(temp.data[[cont.vars.mdf[x]]]), ") contacts, ", 
                 round(100*length(unique(temp.data$resp_id[idx.miss]))/length(unique(temp.data$resp_id)),1),
                 "% (", length(unique(temp.data$resp_id[idx.miss])), " out of ",
                 length(unique(temp.data$resp_id)), ") respondents" ))
  }
  if(is.character(temp.data[[cont.vars.mdf[x]]])){
    print(paste(cont.vars.mdf[x], "is still a character"))
  }
} 

### removing variables that are not suitable for being used in imputations
cols.nrformt = which(sapply(colnames(temp.data), function(x)
  (!is.numeric(temp.data[[x]]) & !is.factor(temp.data[[x]])) |
    length(levels(temp.data[[x]])) >= 20 |
    sum(is.na(temp.data[[x]]))/nrow(temp.data) > 0.2))

temp.data0 = temp.data

if(length(cols.nrformt) > 0){
  temp.data = temp.data[,-cols.nrformt]
}

## summarizing the resulting data
print(summary(temp.data))
print(colnames(temp.data))
print(colnames(temp.data)[grep("^cont_",colnames(temp.data), invert = T)])

## setting up parallel computing
# Make sure implicit clusters are all released
stopImplicitCluster()
# Detect the number of cores available
num_cores = detectCores() - 1  # Use one core less to leave room for other processes
# Register the parallel back end
cl = makeCluster(num_cores)
registerDoParallel(cl)
# Update packages and set cluster seeds
update.packages(c("randomForest", "doRNG", "foreach"))
clusterSetRNGStream(cl = cl, iseed = 5432)

## Imputing missing values
imputed.data = missForest2(temp.data, variablewise = T, #xtrue = T,
                          parallelize = 'forests', verbose = T
)
stopCluster(cl)


# Access the completed dataset
completed.data = imputed.data$ximp

#### PERFORM IMPUTATIONS (on lost contact setting & contact relationship) ####

#### Perform imputations on contact setting data (in group "not hh") that was lost during cleaning ####
temp.compl = completed.data[which(completed.data$cont_setting != "Household"),]
temp.compl$cont_setting[which(temp.compl$cont_setting == "Not In My Household")] = NA
temp.compl$cont_relationship[which(temp.compl$cont_relationship == "Not In My Household")] = NA

temp.compl$cont_setting = droplevels(temp.compl$cont_setting)
temp.compl$cont_relationship = droplevels(temp.compl$cont_relationship)
print(summary(temp.compl$cont_setting))
print(summary(temp.compl$cont_relationship))

## setting up parallel computing
# Make sure implicit clusters are all released
stopImplicitCluster()
# Detect the number of cores available
num_cores = detectCores() - 1  # Use one core less to leave room for other processes
# Register the parallel backend
cl = makeCluster(num_cores)
registerDoParallel(cl)
# Update packages and set cluster seeds
update.packages(c("randomForest", "doRNG", "foreach"))
clusterSetRNGStream(cl = cl, iseed = 5432)


## Imputing missing data
set.imputed.data1 = missForest2(temp.compl,
                                variablewise = T,
                                parallelize = 'forests',
                                verbose = T
)
stopCluster(cl)

idx.match = which(is.na(temp.compl$cont_setting) | is.na(temp.compl$cont_relationship))
matching.table = matrix(NA, nrow = length(idx.match), ncol = 4)
# Insert the imputed cont_setting back into the original data using resp_id and cont_id for matching
for (i in seq_along(idx.match)) {
  # i = 1
  original_index = idx.match[i]
  case_id = temp.compl$resp_id[original_index]
  cont_id = temp.compl$cont_id[original_index]
  
  matching.table[i,1:2] = c(case_id,cont_id)
  # Find the matching row in temp.compl based on resp_id and cont_id
  match_row_new = which(set.imputed.data1$ximp$resp_id == case_id &
                          set.imputed.data1$ximp$cont_id == cont_id)
  match_row_old = which(completed.data$resp_id == case_id &
                          completed.data$cont_id == cont_id)
  
  if (length(match_row_new) > 0 & length(match_row_old) > 0 ) {
    if(length(match_row_new) > 1 | length(match_row_old) > 1 ){
      print(paste("multiple matches:", i))
    }
    completed.data$cont_setting[match_row_old] = 
      set.imputed.data1$ximp$cont_setting[match_row_new]
    completed.data$cont_relationship[match_row_old] = 
      set.imputed.data1$ximp$cont_relationship[match_row_new]
    
    matching.table[i,3:4] = c(match_row_old, match_row_new)
    
  }else{
    stop("No matching rows")
  }
}

colnames(matching.table) = c("resp_id", "cont_id", "old.idx", "new.idx")
matching.table = as.data.frame(matching.table)

completed.data$cont_setting = droplevels(completed.data$cont_setting)
print(summary(completed.data$cont_setting))

completed.data$cont_relationship = droplevels(completed.data$cont_relationship)
print(summary(completed.data$cont_relationship))

completed.data0 = completed.data

#### PERFORM IMPUTATIONS (on contact relationship = refused) ####

temp.compl = completed.data[which(completed.data$cont_relationship 
                                  != "Person [My Child Lives/I Live] With"),]
temp.compl$cont_relationship[which(temp.compl$cont_relationship == "Refused")] = NA

temp.compl$cont_relationship = droplevels(temp.compl$cont_relationship)
summary(temp.compl$cont_relationship)

## set up parallel computing
# Make sure implicit clusters are all released
stopImplicitCluster()
# Detect the number of cores available
num_cores = detectCores() - 1  # Use one core less to leave room for other processes
# Register the parallel backend
cl = makeCluster(num_cores)
registerDoParallel(cl)
# Update packages and set cluster seeds
update.packages(c("randomForest", "doRNG", "foreach"))
clusterSetRNGStream(cl = cl, iseed = 5432)

## Impute missing values
set.imputed.data2 = missForest2(temp.compl,
                              variablewise = T,
                              parallelize = 'forests', verbose = T)
stopCluster(cl)


set.imputed.data2$ximp$cont_relationship = droplevels(set.imputed.data2$ximp$cont_relationship)
idx.match = which(is.na(temp.compl$cont_relationship))

# Insert the imputed cont_setting back into the original data using resp_id and cont_id for matching
for (i in seq_along(idx.match)) {
  # i = 1
  original_index = idx.match[i]
  case_id = temp.compl$resp_id[original_index]
  cont_id = temp.compl$cont_id[original_index]

  # Find the matching row in temp.compl based on resp_id and cont_id
  match_row_new = which(set.imputed.data2$ximp$resp_id == case_id &
                          set.imputed.data2$ximp$cont_id == cont_id)
  match_row_old = which(completed.data$resp_id == case_id &
                          completed.data$cont_id == cont_id)

  if (length(match_row_new) > 0 & length(match_row_old) > 0 ) {
    completed.data$cont_relationship[match_row_old] = 
      set.imputed.data2$ximp$cont_relationship[match_row_new]
  }
}

# summary(set.imputed.data2$ximp$cont_relationship)
completed.data$cont_relationship = droplevels(completed.data$cont_relationship)
print(summary(completed.data$cont_relationship))

#see.idx = which(is.na(temp.data$cont_income))
#View(completed.data[see.idx,])


cont.vars.mdf2 = c(cont.vars.mdf, add.cont.vars)

idx.matched = match(cont.vars.mdf2, colnames(merged.df))
merged.df[,paste0("bimp_", colnames(merged.df)[idx.matched])] =
  merged.df[,colnames(merged.df)[idx.matched]]


completed.data$imputed.conts = 0
completed.data$imputed.details = ""

identical(cont.vars.mdf2[cont.vars.mdf2 %in% colnames(temp.data)],
          cont.vars.mdf2[cont.vars.mdf2 %in% colnames(completed.data)])

testing = cont.vars.mdf2[cont.vars.mdf2 %in% colnames(temp.data)]
idx.imputed = which(is.na(temp.data[, testing]),
                    arr.ind = T)

for(idr in unique(idx.imputed[, "row"])){

  # idr = 117
  resp_id = temp.data$resp_id[idr]
  contactid = temp.data$cont_id[idr]
  
  ced.idx = which(completed.data$resp_id == resp_id &
                   completed.data$cont_id == contactid)
  completed.data$imputed.conts[ced.idx] = 1

  idx.col = idx.imputed[which(idx.imputed[, "row"] == idr), "col"]
  if(length(idx.col) == length(testing)){
    completed.data$imputed.details[ced.idx] = "all cont_*"
  }else{
    completed.data$imputed.details[ced.idx] = paste0(testing[idx.col], collapse = ", ")
  }
}

completed.data1 = completed.data

unique(completed.data$imputed.details)

vars.ex = cont.vars.mdf2[cont.vars.mdf2 %in% colnames(completed.data)]
identical(vars.ex, cont.vars.mdf2)
identical(testing, vars.ex)

tmp = left_join(merged.df[, - match(vars.ex, colnames(merged.df))],
                completed.data[,c("resp_id", "cont_id", vars.ex,
                                  "imputed.conts", "imputed.details")],
                by = c("resp_id", "cont_id"))

# write.csv(tmp, paste0(output.dir, "/imputed_contacts_df.csv"))

needed.vars = c(colnames(merged.df)[match(vars.ex, colnames(merged.df))],
                "imputed.conts", "imputed.details", "cont_id")
merged.df[,needed.vars] = tmp[,needed.vars]

merged.df$cont_age = round(merged.df$cont_age)

sum.details = merged.df$imputed.details
excl.ageg = testing[setdiff(grep("cont_age", testing),
                    which(testing == "cont_age"))]
for(i in excl.ageg){
  sum.details = gsub(i, "", sum.details)
}
sum.details = gsub(paste0(rep(", ", length(excl.ageg)), collapse = ""), "", sum.details)

imputed.summary = as.data.frame(table(sum.details))
imputed.summary$Perc = round(100*imputed.summary$Freq/nrow(merged.df[!is.na(merged.df$cont_identifier),]),2)
tmp = strsplit(as.character(imputed.summary[,1]), ", ")
imputed.summary$Miss.vars = sapply(tmp, function(x) if(length(x) > 0){
  if(sum(grepl("all", x)) == 0){
    length(x)
  }else{
    length(testing)
  }
}else{
  0
})
imputed.summary$Miss.degree = round(imputed.summary$Miss.vars/length(testing), 3)

imputed.summary = imputed.summary[order(imputed.summary$Freq, decreasing = T),]
# write.csv(imputed.summary, paste0(output.dir, "/imputed_conts_summary.csv"),
#           row.names = F)

idx.missing = unlist(sapply(c("age","gender","ethn","relationship", "setting"), function(x)
  grep(x, merged.df$imputed.details)))

sum.details1 = gsub("cont_age[_a-zA-Z0-9_]+, ","", merged.df$imputed.details, perl = T)[idx.missing]
imputed.summary1 = as.data.frame(table(sum.details1))
imputed.summary1$Perc = round(100*imputed.summary1$Freq/nrow(merged.df[!is.na(merged.df$cont_identifier),]),2)
tmp = strsplit(as.character(imputed.summary1[,1]), ", ")
imputed.summary1 = imputed.summary1[order(imputed.summary1$Freq, decreasing = T),]
# write.csv(imputed.summary1, paste0(output.dir, "/imputed_conts_1paper.csv"),
#           row.names = F)

imputed.summary.indep = as.data.frame(table(unlist(strsplit(na.omit(sum.details), ", "))))

imputed.summary.indep$Perc = round(100*imputed.summary.indep$Freq/length(na.omit(sum.details)),
                                   2)
imputed.summary.indep = imputed.summary.indep[order(imputed.summary.indep$Perc, decreasing = T),]
# write.csv(imputed.summary.indep, paste0(output.dir, "/imputed_conts_indep_summary.csv"),
#           row.names = F)


merged.df.tmp = merged.df
# merged.df = merged.df.tmp

nconts.df = merged.df %>%
  group_by(resp_id, ppwork, weight) %>%
  summarise(n = sum(as.numeric(!is.na(.data[["cont_identifier"]]))*weight), .groups = "drop") %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  group_by(ppwork) %>%
  summarise(mean.n = mean(n))

for(x in varset.cont){
  if(is.factor(merged.df[,x])){
    merged.df[,x] = droplevels(merged.df[,x])
  }
}

         
#### PERFORM IMPUTATIONS (for multiple contacts) ####

levels(merged.df$hh_loc_reg9) = gsub("Mid ","Middle ", 
                                     gsub("-", " ", 
                                          levels(merged.df$hh_loc_reg9))) ## will be needed to sample from age& race distribution
levels(merged.df$cont_identifier) = c(levels(merged.df$cont_identifier), "Multiple Contacts")

merged.df = merged.df %>%
  mutate(mult_n = mc_ncont %>%
           replace_na(0) 
         %>% as.numeric(as.character(.)),
         edu_n = rowSums(
           across(
             c(resp_edun_02, resp_edun_34, resp_edun_59,
               resp_edun_1014, resp_edun_1517, resp_edun_18),
             ~ as.numeric(as.character(.))
           ),
           na.rm = TRUE  # Ignore NA values in the sum
         ))


idx.dm = which(merged.df$edu_n == merged.df$mult_n &
                 as.numeric(as.character(merged.df$resp_edun_18)) == merged.df$edu_n)
idx.de = which(merged.df$edu_n == merged.df$mult_n &
                 as.numeric(as.character(merged.df$resp_edun_18)) < merged.df$edu_n)

if(length(idx.dm) > 0){
  merged.df$edu_n[idx.dm] = 0
}
if(length(idx.de) > 0){
  merged.df$mult_n[idx.de] = 0
}

## Distinguishing multiple from numerous contacts 
## (multiple contacts = contacts reported by the respondents as a single number 
##                               but not satisfying "numerous contacts" definition;
## numerous* contacts = more than 30 work-related contacts per day during the day of the diary)
## * also referred to as additional contacts in the literature

merged.df$edu2_n = ifelse(merged.df$edu_n <= 30, merged.df$edu_n, 0)
merged.df$add_edu_n = ifelse(merged.df$edu_n > 30, merged.df$edu_n, 0)

merged.df$mult2_n = ifelse(merged.df$mult_n <= 30, merged.df$mult_n, 0)
merged.df$add_mult_n = ifelse(merged.df$mult_n > 30, merged.df$mult_n, 0)

multn.vars = c("edu2_n", "add_edu_n", "mult2_n", "add_mult_n")

summary(merged.df$edu2_n[which(merged.df$edu2_n > 0)])
summary(merged.df$mult2_n[which(merged.df$mult2_n > 0)])

# age groups for students from the instrument: 0-2, 3-4, 5-9, 10-14, 15-17, 18+
st.breaks = c(0, 3, 5, 10, 15, 18, 120)
merged.df$resp_age_std = cut(merged.df$resp_age,
                             breaks = st.breaks,
                             right = F)

# create indices that fulfill the two conditions
edu_variables = c("resp_edun_02", "resp_edun_34", "resp_edun_59",
                   "resp_edun_1014", "resp_edun_1517", "resp_edun_18",
                   "mult2_n")

### identify individuals with multiple contacts either in edu and other(mult) categories
### that require imputing

idx_age = lapply(edu_variables, function(edu) {
  idx = which((merged.df$edu2_n > 0 | merged.df$mult2_n > 0) &
                 as.numeric(as.character(merged.df[,edu])) > 0) ## will always create NA warnings due to "Refused"
})

adj.resp_id = unique(merged.df$resp_id[unlist(idx_age)])

idx_age2 = which(sum.res2$resp_id %in% adj.resp_id)
table(sum.res2$resp_educator[idx_age2], useNA = "ifany")

merged.df$cont_age_std = cut(merged.df$cont_age,
                             breaks = st.breaks,
                             right = F)

nobs.before = nrow(merged.df)

new_rows = list()
t=1
agl = levels(merged.df$resp_age_std)

for (count in 1:length(adj.resp_id)){
  
  ## create a new line with empty contact characteristics
  idx.c1 = which(merged.df$resp_id == adj.resp_id[count])[1]
  new_row = merged.df[idx.c1,]
  new_row[, grep("^cont_|^bimp|^imputed", colnames(new_row))] = NA

  for (j in 1:length(edu_variables)){
    mult.check = ifelse(is.na(merged.df$mult2_n[idx.c1]), 0, merged.df$mult2_n[idx.c1])
    edu = edu_variables[j]
    
    if((j < length(edu_variables) & merged.df$edu2_n[idx.c1] > 0) |
       (j == length(edu_variables) &  mult.check > 0)){
      
      count_value = as.numeric(as.character(merged.df[idx.c1, edu]))
      # print(count_value)
      
      if (count_value > 0 & !is.na(count_value)) {
        new_rows_to_add = new_row[rep(row.names(new_row), count_value),]
        row.names(new_rows_to_add) = NULL
        
        ## for all educational variables
        if(j <= length(agl)){
          new_rows_to_add$cont_age_std = factor(agl[j], levels = levels(merged.df$cont_age_std))
          ## may be either a student or a customer (e.g., patient for pediatricians)
          
          if(new_row$resp_industry_group == "Education, Tutoring, and Child Care" &
             new_row$resp_educator == "Yes"){
            new_rows_to_add$cont_relationship = factor("Student", 
                                                       levels = levels(merged.df$cont_relationship))
            new_rows_to_add$cont_setting = factor("School", 
                                                  levels = levels(merged.df$cont_setting))
          }else{
            new_rows_to_add$cont_relationship = factor("Customer", 
                                                       levels = levels(merged.df$cont_relationship))
            new_rows_to_add$cont_setting = factor("Work", 
                                                  levels = levels(merged.df$cont_setting))
          }
        }else{ ## for the non-educational variable
          new_rows_to_add$cont_age_std = factor(NA, levels = levels(merged.df$cont_age_std))
          new_rows_to_add$cont_relationship = factor("Customer", 
                                                      levels = levels(merged.df$cont_relationship))
          new_rows_to_add$cont_setting = factor("Work", 
                                                 levels = levels(merged.df$cont_setting))
        }
        new_rows[[t]] = new_rows_to_add
        t = t+1
      }
    }else{
      #print(cbind(edu, merged.df[idx.c1, c("edu2_n", "mult2_n")]))
      # notadded_rows[[k]] = cbind(edu, merged.df[idx.c1, c("resp_id","edu_n", "edu2_n", "mult2_n")])
      # k = k+1
    }
  }
  if(length(which(merged.df$resp_id == adj.resp_id[count])) == 1 & 
     sum(!is.na(merged.df[idx.c1,testing])) == 0){
    merged.df = merged.df[-idx.c1,]
  }
}

# make all the new rows into a unified dataframe
combined_df = Reduce(rows_append, new_rows)
combined_df$cont_identifier = factor("Multiple Contacts",
                                     levels = levels(merged.df$cont_identifier))
merged.df = rows_append(merged.df, combined_df)
merged.df = merged.df[order(merged.df$resp_id),]

nobs.after = nrow(merged.df)

print(paste(nobs.after - nobs.before, "contacts were added"))

#################################################################
###### make this a function #######

ids = unique(merged.df$resp_id)
merged.df$cont_id = NA
for(id in ids){
  idx = which(merged.df$resp_id == id)
  if(length(idx) > 0){
    if(sum(!is.na(merged.df[idx,testing])) == 0 &
       length(idx) == 1){
      merged.df$cont_id[idx] = 0
    }else{
      merged.df$cont_id[idx] = c(1:length(idx))
    }
  }else{
    stop("IDs missing")
  }
}

## read the age structure of students from ACS 2021-2023 (IPUMS)

pop.student.age = read.csv("./external_data/student_agestr.csv")

pop.student.age = pop.student.age %>%
  group_by(AGE, ethn, NAME) %>%
  summarize(npeople = sum(npeople, na.rm = T))

## estimate age structure of students from this survey
vars.imp2 = c("resp_id", "cont_id", "cont_identifier",  
              "cont_age","cont_age_std", "cont_ethn", "cont_gender",
              "cont_setting", "cont_relationship", 
              "resp_age", "resp_age_std", "resp_ethn", "resp_gender")

#define data to use for imputations
temp.data.mc = merged.df[,c(vars.imp2,  "weight")]
summary(temp.data.mc)

cont0.ids = which(temp.data.mc$cont_id == 0)

if(length(cont0.ids) > 0){
  temp.data.mc = temp.data.mc[-cont0.ids,]
}

print(summary(temp.data.mc))

idx.agegroup = which(((temp.data.mc$cont_setting == "School" &
                       (abs(temp.data.mc$cont_age - temp.data.mc$resp_age) < 20 &
                       (!temp.data.mc$cont_relationship %in%
                           c("Classmate/Teacher", "Relative", "Other") &
                          !temp.data.mc$cont_identifier %in% c("Classmate", "Teacher", "Relative")))) |
                       (temp.data.mc$cont_setting == "Work" &
                          temp.data.mc$cont_relationship == "Student"))
                       )

agedist = temp.data.mc[idx.agegroup,] %>%
  group_by(cont_age) %>%
  summarize(n = sum(weight, na.rm = T))

agedist.acs = pop.student.age %>%
  group_by(AGE) %>%
  summarize(n = sum(npeople, na.rm = T)) %>%
  mutate(n.prop = sum(agedist$n)*n/sum(n))

plot(agedist)
lines(agedist.acs$AGE, agedist.acs$n.prop, col = "red")

## create contingency table based on this survey data

temp.data.mc$cont_ethn = factor(temp.data.mc$cont_ethn, levels = levels(temp.data.mc$resp_ethn))

race.test = which((temp.data.mc$cont_setting == "Work" &
                     temp.data.mc$cont_relationship == "Student"))

idx.st.teach = intersect(which(merged.df$cont_relationship == "Classmate/Teacher"), 
                         which(merged.df$cont_identifier == "Teacher"))



contingency_table1 = as.data.frame(temp.data.mc[race.test,] %>%
                                     group_by(resp_ethn, cont_ethn) %>%
                                     summarise(Freq = sum(weight, na.rm = T))%>%
                                     filter(cont_ethn != "Refused"))

colnames(contingency_table1)[1:2] = c("teacher", "student")

contingency_table2 = as.data.frame(temp.data.mc[idx.st.teach,] %>%
                                     group_by(resp_ethn, cont_ethn) %>%
                                     summarise(Freq = sum(weight, na.rm = T)) %>%
                                     filter(!is.na(resp_ethn) & !is.na(cont_ethn) &
                                              cont_ethn != "Refused"))
colnames(contingency_table2)[1:2] = c("student", "teacher")


contingency_table = full_join(contingency_table1, contingency_table2, 
                              by = c("student", "teacher"))
contingency_table$Freq.x[is.na(contingency_table$Freq.x)] = 0
contingency_table$Freq = contingency_table$Freq.x + contingency_table$Freq.y

contingency_table = contingency_table[,c("teacher", "student", "Freq")]

contingency_table = contingency_table %>% 
  group_by(teacher) %>% 
  mutate(Frac = Freq/sum(Freq))

ggplot(as.data.frame(contingency_table), aes(x = teacher, y = student, fill = Frac)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + # Customize colors
  theme_minimal() + # Clean theme
  labs(fill = "Frequency") # Rename legend title

## perform the sampling of characteristics for Multiple Contacts

acs.race = pop.student.age %>%
  group_by(ethn) %>%
  summarise(npeople = sum(npeople)) %>%
  mutate(Freq = npeople/sum(npeople))
acs.race

imp.race = list()

# seeds = c(1:10000)#[idx.best.fit]
# gf.par = list()
# for(rs in seeds){
#   if(rs %% 100 == 0){
#     print(100*rs/length(seeds))
#   }
rs = 6023
set.seed(rs)

idx.students = which(merged.df$cont_identifier == "Multiple Contacts" & 
                       merged.df$cont_relationship == "Student")

imp.scale = 1 - length(idx.students)/
  sum(merged.df$cont_relationship == "Student", na.rm = T)

for(i in 1:length(levels(merged.df$cont_age_std))){

  idx.x = intersect(idx.students, 
                    which(merged.df$cont_age_std == levels(merged.df$cont_age_std)[i]))
  
  for(j in unique(as.character(merged.df$hh_loc_reg9[idx.x]))){
    idx.x.st = intersect(idx.x, which(as.character(merged.df$hh_loc_reg9) == j))
    idx.y = which(pop.student.age$AGE >= st.breaks[i] & pop.student.age$AGE < st.breaks[i+1] &
                    pop.student.age$NAME == j)
    if(length(idx.y) == 0){
      idx.y = which(agedist$cont_age >= st.breaks[i] & agedist$cont_age < st.breaks[i+1])
      y = agedist$n[idx.y]/sum(agedist$n[idx.y])
      v = agedist$cont_age[idx.y]
    }else{
      v = pop.student.age$AGE[idx.y]
      y = pop.student.age$npeople[idx.y]/sum(pop.student.age$npeople[idx.y])
    }
    merged.df$cont_age[idx.x.st] = sample(v, length(idx.x.st), replace = T, prob = y)
    merged.df$cont_ethn[idx.x.st] = sapply(idx.x.st,function(x) {
      #print(x)
      idx.age = which(pop.student.age$AGE == merged.df$cont_age[x] & 
                        pop.student.age$NAME ==  as.character(merged.df$hh_loc_reg9[x]))
      if(length(idx.age) == 0){
        idx.race = which(contingency_table$teacher == as.character(merged.df$resp_ethn[x]))
        prob.race = contingency_table$Frac[idx.race]
        res.ethn = sample(contingency_table$student[idx.race], 1, replace = T, 
                          prob = prob.race)
        }else{
        
        prob.race = pop.student.age$npeople[idx.age]/sum(pop.student.age$npeople[idx.age])
        
        # assortat = contingency_table$Frac[which(contingency_table$teacher == as.character(merged.df$resp_ethn[x]) &
        #                                           contingency_table$student== as.character(merged.df$resp_ethn[x]))]
        # idx.diag = which(pop.student.age$ethn[idx.age] == as.character(merged.df$resp_ethn[x]))
        # if(length(idx.diag) > 0){
        #   if(prob.race[idx.diag] != assortat){
        #     prob.race[idx.diag] = assortat
        #     prob.race[-idx.diag] = (1-assortat)*prob.race[-idx.diag]/sum(prob.race[-idx.diag])
        #   }
        # }
        
        idx.teach.ag = which(contingency_table$teacher == as.character(merged.df$resp_ethn[x]))
        assortat = contingency_table$Frac[idx.teach.ag][match(pop.student.age$ethn[idx.age],
          as.character(contingency_table$student[idx.teach.ag]))]
        assortat = if_else(is.na(assortat), 0, assortat)
        
        prob.race = (imp.scale*assortat + prob.race)/sum(imp.scale*assortat + prob.race)
        res.ethn = sample(pop.student.age$ethn[idx.age], 1, replace = T, 
                          prob = prob.race)
      }
      return(res.ethn)
    })
    
    if(sum(is.na(merged.df$cont_ethn[idx.x.st])) > 0){
      print(idx.x.st)
      print("NA created in students")
    }
  }
}

merged.df$cont_gender[idx.students] = sample(c("Male", "Female"), 
                                              length(idx.students), 
                                              replace = T, prob = c(0.5,0.5))
merged.df$cont_relationship[idx.students] = "Student"
merged.df$cont_setting[idx.students] = "Work"

## check imputed race
race.test = which((merged.df$cont_setting == "Work" &
                     merged.df$cont_relationship == "Student"))

contingency_table_res = table(teacher = merged.df$resp_ethn[race.test],
                          student = merged.df$cont_ethn[race.test])

contingency_table_res = as.data.frame(contingency_table_res) %>%
  group_by(teacher) %>%
  mutate(Frac = Freq/sum(Freq))

ggplot(as.data.frame(contingency_table_res), aes(x = teacher, y = student, fill = Frac)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + # Customize colors
  theme_minimal() + # Clean theme
  labs(fill = "Frequency") # Rename legend title

tmp.am = contingency_table_res[,-3] %>%
  pivot_wider(names_from = student, values_from = Frac)
print(assortativity.calc(tmp.am[,-1]))

imp.race[[rs]] = merged.df[idx.students,] %>%
  group_by(cont_ethn) %>%
  summarise(npeople = sum(weight)) %>%
  mutate(Freq = npeople/sum(npeople))
imp.race[[rs]]

#print(rs)
# gf.par[[rs]] = sum((imp.race[[rs]]$Freq - acs.race$Freq)^2)
# }

# fit.v = unlist(gf.par)
# idx.best.fit = which(abs(fit.v ) == min(abs(fit.v )))

## impute NOT teacher-student Multiple Contacts

pop.age = read.csv("./external_data/pop_agestr.csv")
pop.age = pop.age %>%
  group_by(AGE, ethn, NAME) %>%
  summarize(npeople = sum(npeople, na.rm = T))

idx.mimp2 = setdiff(which(merged.df$cont_identifier == "Multiple Contacts"),
                    idx.students)

for(j in unique(as.character(merged.df$hh_loc_reg9[idx.mimp2]))){
  #print(j)
  idx.x = intersect(idx.mimp2, 
                    which(merged.df$hh_loc_reg9 == j))
  #print(idx.x)
  idx.st = which(!is.na(merged.df$cont_age_std[idx.x]))
  
  if(length(idx.st) > 0){
    
    idx.kids = idx.x[idx.st]
    idx.x = idx.x[-idx.st]
    
    # print("KIDS:")
    # print(merged.df[idx.kids,"cont_age_std"])
    # print("OTHERS")
    # print(merged.df[idx.x,"cont_age_std"])
    
    for(i in 1:length(levels(merged.df$cont_age_std))){
      idx.x.st = intersect(idx.kids, 
                           which(merged.df$cont_age_std == levels(merged.df$cont_age_std)[i]))
      if(length(idx.x.st) > 0){
        idx.y = which(pop.age$AGE >= st.breaks[i] & pop.age$AGE < st.breaks[i+1] &
                        pop.age$NAME == j)
        y = pop.age$npeople[idx.y]/sum(pop.age$npeople[idx.y])
        merged.df$cont_age[idx.x.st] = sample(pop.age$AGE[idx.y], length(idx.x.st), replace = T, prob = y)
        merged.df$cont_ethn[idx.x.st] = sample(pop.age$ethn[idx.y], length(idx.x.st), replace = T, prob = y)
        
        if(sum(is.na(merged.df$cont_ethn[idx.x.st])) > 0){
          print(idx.x.st)
          print("NA created in kid customers")
        }
      }
    }
  }
  
  if(length(idx.x) > 0){
    idx.y = which(pop.age$NAME == j)
    y = pop.age$npeople[idx.y]/sum(pop.age$npeople[idx.y])
    
    merged.df$cont_age[idx.x] = sample(pop.age$AGE[idx.y], length(idx.x), replace = T, prob = y)
    merged.df$cont_ethn[idx.x] = sample(pop.age$ethn[idx.y], length(idx.x), replace = T, prob = y)
    if(sum(is.na(merged.df$cont_ethn[idx.x])) > 0){
      print(idx.x)
      print("NA created in customers")
    }
  }
}

merged.df$cont_gender[idx.mimp2] = sample(c("Male", "Female"), 
                                              length(idx.mimp2), 
                                              replace = T, prob = c(0.5,0.5))

### move all "teacher" contact with students from Work to school

idx.st.move = which((merged.df$cont_setting == "Work" &
         merged.df$cont_relationship == "Student" &
           merged.df$resp_industry_group == "Education, Tutoring, and Child Care"))

table(merged.df$cont_identifier[idx.st.move])
table(merged.df$resp_industry_group[idx.st.move], merged.df$cont_identifier[idx.st.move])
table(merged.df$resp_occupation_group[idx.st.move], merged.df$cont_identifier[idx.st.move])

View(table(merged.df$cont_age, merged.df$cont_identifier, useNA = "ifany"))
table(merged.df$cont_ethn, merged.df$cont_identifier, useNA = "ifany")
table(merged.df$cont_gender, merged.df$cont_identifier, useNA = "ifany")

write.csv(merged.df[, varset.cont], paste0(output.dir,"/imputed-contacts2.csv"))

######## NUMBER OF CONTACTS ########

###### Work with contact-level dataset ######
cont_cols = grep("^cont_", colnames(merged.df), value = TRUE)
nocont =  rowSums(!is.na(merged.df[, cont_cols])) == 0


###### Create respondent-level dataset ######

mainvars = setdiff(colnames(merged.df)[unlist(key.vars.cont)],
                   "cont_age")
allcont_var = rbind(cbind(var1 = c(mainvars, add.cont.vars),
                          var2 = 1),
                    as.matrix(expand_grid(mainvars, add.cont.vars)))

allcont_var = cbind(allcont_var, varname = unlist(lapply(c(1:nrow(allcont_var)), function(acv)
  paste0(c("cont", gsub("cont_", "",setdiff(allcont_var[acv,], "1"))),
         collapse = "_"))))


sum.res3.tmp = list()

for(acv in c(1:nrow(allcont_var))){
  group.vars = setdiff(allcont_var[acv,1:2], "1")

  sum.res3.tmp[[acv]] = merged.df %>%
    group_by(resp_id, !!!syms(group.vars)) %>%
    summarise(n = sum(!is.na(.data[[group.vars[1]]])), .groups = "drop") %>%
    pivot_wider(
      names_from = all_of(group.vars),
      names_sep = "_",
      values_from = n
    ) %>%
    mutate(across(everything(), ~ replace_na(.x, 0))) %>%
    rename_with(.fn = ~ gsub(" ","_",paste0(paste0(c("cont", gsub("cont_", "",group.vars)),
                                                   collapse = "_"), "_",
                                            gsub(', ','',.), "_n")),
                .cols = -resp_id  # Exclude resp_id from renaming
    )
}

tmp = Reduce(left_join, sum.res3.tmp)
tmp$total.contacts = rowSums(tmp[,grep("cont_setting_", colnames(tmp))])

mer.vars = c(setdiff(grep("^cont|bimp_cont_|_n$",colnames(merged.df), invert = TRUE, value = TRUE),
                   "total.contacts"), multn.vars)
sum.res3 = left_join(as.data.frame(tmp),
                     merged.df[,mer.vars],
                     by = "resp_id", multiple = "first")

########## By key respondent characteristics by link/contact char ##########

sum.res3 = sum.res3 %>%
  mutate(cont_setting_nothh_n = total.contacts - cont_setting_Household_n)

write.csv(sum.res3[,c(varset.resp, multn.vars,
                      "total.contacts",
                      "cont_setting_nothh_n")], 
          paste0(output.dir, "/imputed-resp2.csv"))

# View(merged.df[which(merged.df$cont_identifier == "Multiple Contacts"),
#                c("resp_id", "cont_identifier", "cont_age","cont_age_std", "resp_age_std",
#                   "edu_n", edu_variables)])
# View(merged.df[which(merged.df$resp_id==584265),
#                c("resp_id", "cont_identifier", "cont_age","cont_age_std", "resp_age_std",
#                  "edu_n", edu_variables)])

save(merged.df, # cleaned, merged, imputed contact-level data (obs. unit = contact)
     sum.res2, # respondent-level data before imputations
     sum.res3, # respondent-level data after imputations/calculated number of contacts
     file = paste0(output.dir, "all-waves.Rdata"))

## check preliminary number of contacts

nconts.total = merged.df %>%
  group_by(resp_id, weight) %>%
  summarise(n = sum(as.numeric(!is.na(.data[["cont_identifier"]]))*weight), .groups = "drop") %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  summarise(n.pp = sum(weight),mean.n = mean(n))
print(nconts.total)

nconts.df = merged.df %>%
  group_by(resp_id, ppwork, weight) %>%
  summarise(n = sum(as.numeric(!is.na(.data[["cont_identifier"]]))*weight), .groups = "drop") %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  group_by(ppwork) %>%
  summarise(n.pp = sum(weight),mean.n = mean(n))

print(nconts.df)

