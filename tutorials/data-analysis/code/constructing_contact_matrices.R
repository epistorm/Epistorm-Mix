## Requires accessing_and_extracting_data.R
cont_imp = read.csv("./tutorials/data-analysis/data/imputed_cont.csv")

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
## main function
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
  
  age_matrix = calc.m.num(mydata = merged.df,
                          myattr = c("resp_age", "cont_age"), # vector of the names of the two attributes
                          setting = setting,
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)
  
  # GAM
  model = gam(ncont ~ te(as.numeric(resp_age), as.numeric(cont_age),
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
                              break.list = age.breaks,
                              suff.list = age.vars.names)
merged.df = create.age.groups(merged.df, input.var = "cont_age", # contact age
                              break.list = age.breaks,
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
by5_matrix = calc.m.num(mydata = merged.df,
                          myattr = c("resp_age_by5_80", "cont_age_by5_80"), # vector of the names of the two attributes
                          setting = "binary_col",
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)

matrix_age = by5_matrix %>% 
  dplyr::select("resp_age_by5_80", "cont_age_by5_80", "ncont") %>% 
  pivot_wider(names_from = "cont_age_by5_80", values_from = "ncont")

write.csv(matrix_age, 
          paste0("./matrices/-by5_80.csv"),
          row.names = F)








