rm(list = ls())

## Function for estimating average contacts by respondent-contact age groups
calc.m.num = function(mydata, 
                      myattr, # vector of the names of the two attributes
                      setting,
                      predict.m,
                      denumer.n){
  # obtain the weighted sum of contacts per respondent-contact group
  age_matrix = mydata %>%
    filter(!is.na(!!sym(myattr[2]))) %>%
    arrange(!!sym(myattr[2])) %>%
    group_by(!!sym(myattr[2]), !!sym(myattr[1])) %>%
    summarise(weighted_sum = sum(weight*!!sym(setting), na.rm = T))
  
  # combine weighted sum with the data for predictions
  age_matrix = left_join(predict.m, age_matrix)
  
  # change NAs
  age_matrix$weighted_sum[is.na(age_matrix$weighted_sum)] = 0
  
  # combine total weighted contact sum with weighted N of people
  age_matrix = left_join(age_matrix, denumer.n)
  
  # order
  age_matrix = age_matrix[order(age_matrix[,myattr[1]], age_matrix[,myattr[2]]),]
  
  # average number of contacts for the matrix
  age_matrix$ncont = age_matrix$weighted_sum/age_matrix$n
  
  return(age_matrix)
}

## LIBRARIES
ps = c("dplyr", "mgcv", "ggplot2", "tidyr", "fastDummies", "Hmisc")

lapply(ps, function(x){
  if(!require(x, character.only = T)){
    install.packages(x)
  }
  library(x, character.only = T)
  
  x
})

## READ IMPUTED RESPONDENT- AND CONTACT-LEVEL DATA 
# Located in the data folder in tutorials/data-analysis 
resp_imp = read.csv("imputed_part_reform.csv")
cont_imp = read.csv("imputed_part_reform.csv")

## ESTIMATING AGE-SPECIFIC CONTACT MATRICES
## Merge datasets
merged_df = left_join(resp_imp, cont_imp, by = "resp_id")

## Binary variable to denote whether respondent reported contacts
cont_cols = grep("^cont_", colnames(merged.df), value = TRUE)
nocont = rowSums(!is.na(merged.df[, cont_cols])) == 0
merged.df$binary_col = ifelse(nocont, 0, 1)

## Single-year age-specific contact matrices
# weighted number of respondents ##
age_matrix_n = merged_df %>%
  group_by(resp_age, resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(resp_age) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## prediction ages ##
nd = expand.grid(resp_age = c(0:max(merged_df$resp_age)),
                 cont_age = c(0:max(merged_df$resp_age)))

predict.m = as.data.frame(nd)

## calculating the matrices ##
setting_names = c("binary_col") ## more can be added (household, school, work, other)

## Smoothed matrices
age_matrix_ls = lapply(setting_names, function(setting) {
  
  age_matrix = calc.m.num(mydata = combined_df,
                          myattr = c("resp_age", "cont_age"), # vector of the names of the two attributes
                          setting = setting,
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)
  
  # GAM
  model = gam(ncont ~ te(as.numeric(part_age), as.numeric(cnt_age),
                         k = c(10,10), bs = c("cc","cc"), m = c(0,0)),
              family = nb(),
              data = age_matrix)
  
  # predictions
  age_matrix$pred = predict(model, newdata = age_matrix, type = "response")
  
  return(age_matrix)
  
})

## 5-year age-specific contact matrices
## Set age breaks and add age groups to df
age.breaks = list("_by5_80" = c(seq(0,80, by = 5), 120))
age.vars.names = c("_by5_80")

merged.df = create.age.groups(merged.df, input.var = "resp_age", # resp age
                              break.list = age.vars.breaks,
                              suff.list = age.vars.names)
merged.df = create.age.groups(merged.df, input.var = "cont_age", # contact age
                              break.list = age.vars.breaks,
                              suff.list = age.vars.names)


## weighted # of resp ##
age_matrix_n = merged.df %>%
  group_by(resp_age_by5_80, resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(resp_age_by5_80) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## prediction age groups ##
nd = expand.grid(resp_age_by5_80 = levels(merged.df$resp_age_by5_80),
                 cont_age_by5_80 = levels(merged.df$cont_age_by5_80))
predict.m = as.data.frame(nd)

## matrices 
# age group labels
age_by5_80  = c(paste0(seq(0, 75, by = 5), "-", seq(4, 79, by = 5)), "80+")

by5_matrix = calc.m.num(mydata = merged.df,
                          myattr = c("resp_age_by5_80", "cont_age_by5_80"), # vector of the names of the two attributes
                          setting = "binary_col",
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)
  
by5_matrix$setting = setting
  
# fix age labels
levels(by5_matrix$resp_age_by5_80) = age_by5_80
levels(by5_matrix$cont_age_by5_80) = age_by5_80

names(by5_matrix) = "Total"

matrix_age = age.matrix.by5 %>% 
  select("resp_age_by5_80", "cont_age_by5_80", "ncont") %>% 
  pivot_wider(names_from = "cont_age_by5_80", values_from = "ncont")

write.csv(matrix_age, 
          paste0("./-by5_80.csv"),
          row.names = F)








