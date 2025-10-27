## Requires accessing_and_extracting_data.R
resp_imp = read.csv("./tutorials/data-analysis/data/imputed_resp.csv")  # change if the data was saved in a different folder
cont_imp = read.csv("./tutorials/data-analysis/data/imputed_cont.csv")  # change if the data was saved in a different folder

## Function for estimating average contacts by respondent-contact age groups
calc.m.num = function(mydata, # data to be used
                      myattr, # vector of the names of the two attributes (e.g., "resp_age" and "cont_age")
                      setting, # character name of the binary variable defining the setting the contact belongs to
                      predict.m, # dataframe containing the combinations of respondent-contact groups (should be the same or a subsample of unique values in variables defined by myattr)
                      denumer.n # number of people to be used as the denominator for the "average number of contacts" (dataframe containing variable "n")
                     ){
  # obtain the weighted sum of contacts per respondent-contact group
  age_matrix = mydata %>%
    filter(!is.na(!!sym(myattr[2]))) %>%
    arrange(!!sym(myattr[2])) %>%
    group_by(!!sym(myattr[2]), !!sym(myattr[1])) %>%
    summarise(weighted_sum = sum(weight*!!sym(setting), na.rm = T))
  
  # combine weighted sum with the data for predictions
  age_matrix = left_join(predict.m, age_matrix)
  
  # change NAs to 0s
  age_matrix$weighted_sum[is.na(age_matrix$weighted_sum)] = 0
  
  # combine total weighted contact sum with weighted N of people
  age_matrix = left_join(age_matrix, denumer.n)
  
  # order
  age_matrix = age_matrix[order(age_matrix[,myattr[1]], age_matrix[,myattr[2]]),]
  
  # average number of contacts for the matrix
  age_matrix$ncont = age_matrix$weighted_sum/age_matrix$n
  
  return(age_matrix)
}

#### create age group ####
## helper function
mycut = function(var, breaks){
  maxv = breaks[length(breaks)]
  if(maxv > max(var, na.rm = T)){
    mylabels = c(paste(breaks[1:(length(breaks)-2)], 
                       breaks[2:(length(breaks)-1)]-1, sep = "-"),
                 paste0(breaks[length(breaks)-1],"+"))
  }else{
    mylabels = paste(breaks[1:(length(breaks)-1)], 
                     breaks[2:length(breaks)]-1, sep = "-")
  }
  var = cut(var,
            breaks = breaks,
            labels = mylabels,
            right = F)
}
## main age group function
create.age.groups = function(mydata,
                             input.var = "resp_age",
                             break.list = list(seq(0,120, by = 5),
                                               c(seq(0,75, by = 5), 120),
                                               c(seq(0,80, by = 5), 120),
                                               seq(0,120, by = 15),
                                               age.groups),
                             suff.list = paste0("_",c("by5", "by5_75","by5_80","by15","g"))){
  
  tmp.vars = lapply(break.list, function(x) mycut(mydata[,input.var],x))
  tmp.names = paste0(input.var, suff.list)
  names(tmp.vars) = tmp.names
  mydata[,tmp.names] = as.data.frame(tmp.vars)
  return(mydata)
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

## ESTIMATING AGE-SPECIFIC CONTACT MATRICES
                    
## Merge datasets
merged.df = left_join(resp_imp, cont_imp, by = "resp_id")

## Binary variable to denote whether respondent reported contacts (or reported contacts in a particular setting)
## 0 = no contact (in this setting); 1 = reported at least 1 contact (in this setting)

settings = unique(merged.df$cont_setting)
                    
# ## Uncomment this if using the code for the real data (these variables are already provided in tutorial data)
# for(x in settings){
#   merged.df[, paste("cont_", tolower(as.character(x)))] = ifelse(merged.df$cont_setting == x, 0, 1)
# }
                    
cont_cols = paste("cont_", tolower(as.character(settings))) # substitute with the variable(s) that denote needed setting
nocont = rowSums(!is.na(merged.df[, cont_cols])) == 0
merged.df$binary_col = ifelse(nocont, 0, 1)

## Single-year age-specific contact matrices
# weighted number of respondents ##
age_matrix_n = resp_imp %>%
  group_by(resp_age) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## prediction ages ##
nd = expand.grid(resp_age = c(0:max(merged_df$resp_age)),
                 cont_age = c(0:max(merged_df$resp_age)))

predict.m = as.data.frame(nd)

## calculating the matrices ##
setting_names = c("binary_col") ## more can be added (household, school, work, other) from "cont_cols"

## Smoothed matrices
age_matrix_ls = lapply(setting_names, function(setting) {
  
  age_matrix = calc.m.num(mydata = merged.df,
                          myattr = c("resp_age", "cont_age"), # vector of the names of the two attributes
                          setting = setting,
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)
  
  # GAM (estimating smoothing)
  model = gam(ncont ~ te(as.numeric(resp_age), as.numeric(cont_age), ## ncont is created in calc.m.num function above
                         k = c(10,10), bs = c("cc","cc"), m = c(0,0)),
              family = nb(),
              data = age_matrix)
  
  # predictions (predicting smoothed contact matrices)
  age_matrix$pred = predict(model, newdata = age_matrix, type = "response")
  
  return(age_matrix)
  
})

## Contact matrices using 5-year age brackets
## Set age brackets and add age groups to df
                    
age.breaks = list("_by5_80" = c(seq(0,80, by = 5), 120)) ## should be a named list
age.vars.names = names(age.breaks)

merged.df = create.age.groups(merged.df, input.var = "resp_age", # resp age
                              break.list = age.breaks,
                              suff.list = age.vars.names)
merged.df = create.age.groups(merged.df, input.var = "cont_age", # contact age
                              break.list = age.breaks,
                              suff.list = age.vars.names)


## weighted # of resp ##
age_matrix_n = resp_imp %>%
  group_by(resp_age_by5_80) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## prediction age groups ##
nd = expand.grid(resp_age_by5_80 = levels(merged.df$resp_age_by5_80),
                 cont_age_by5_80 = levels(merged.df$cont_age_by5_80))
predict.m = as.data.frame(nd)

## matrices 
by5_matrix = calc.m.num(mydata = merged.df,
                          myattr = c("resp_age_by5_80", "cont_age_by5_80"), # vector of the names of the two attributes
                          setting = "binary_col", ## setting-specific or "binary_col" for total contacts
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)

matrix_age = by5_matrix %>% 
  dplyr::select("resp_age_by5_80", "cont_age_by5_80", "ncont") %>% 
  pivot_wider(names_from = "cont_age_by5_80", values_from = "ncont")

write.csv(matrix_age, 
          paste0("./matrices/-M-by5_80-matrix.csv"),
          row.names = F)








