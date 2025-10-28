## Set working directory
setwd("write-here.../Epistorm-Mix/")

## Requires accessing_and_extracting_data.R
resp_imp = read.csv("./tutorials/data-analysis/data/imputed_resp.csv")  # change if the data was saved in a different folder
cont_imp = read.csv("./tutorials/data-analysis/data/imputed_cont.csv")  # change if the data was saved in a different folder

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
                    
## Create dummy variables for individual settings 
for(x in settings){
  merged.df[, paste("cont_", tolower(as.character(x)))] = ifelse(merged.df$cont_setting == x, 0, 1)
}
                    
cont_cols = paste("cont_", tolower(as.character(settings))) # substitute with the variable(s) that denote needed setting
nocont = rowSums(!is.na(merged.df[, cont_cols])) == 0
merged.df$binary_col = ifelse(nocont, 0, 1)

## Single-year age-specific contact matrices
# weighted number of respondents ##
age_matrix_n = resp_imp %>%
  group_by(resp_age) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## prediction ages ##
nd = expand.grid(resp_age = c(0:max(merged.df$resp_age)),
                 cont_age = c(0:max(merged.df$resp_age)))

predict.m = as.data.frame(nd)

## calculating the matrices ##
setting_names = c("binary_col") ## more can be added (household, school, work, other) from "cont_cols"

## Smoothed matrices

# obtain the weighted sum of contacts per respondent-contact group
age_matrix_s = merged.df %>%
  filter(!is.na(cont_age)) %>%
  arrange(cont_age) %>%
  group_by(cont_age, resp_age) %>%
  summarise(weighted_sum = sum(weight*!!sym(setting_names), na.rm = T))

# combine weighted sum with the data for predictions
age_matrix_s = left_join(predict.m, age_matrix_s)

# change NAs to 0s
age_matrix_s$weighted_sum[is.na(age_matrix_s$weighted_sum)] = 0

# combine total weighted contact sum with weighted N of people
age_matrix_s = left_join(age_matrix_s, age_matrix_n)

# order
age_matrix_s = age_matrix_s[order(age_matrix_s[,"resp_age"], age_matrix_s[,"cont_age"]),]

# average number of contacts for the matrix
age_matrix_s$ncont = age_matrix_s$weighted_sum/age_matrix_s$n

# GAM (estimating smoothing)
model = gam(ncont ~ te(as.numeric(resp_age), as.numeric(cont_age),
                       k = c(10,10), bs = c("cc","cc"), m = c(0,0)),
            family = nb(),
            data = age_matrix_s)

# predictions (predicting smoothed contact matrices)
age_matrix_s$pred = predict(model, newdata = age_matrix_s, type = "response")

## Contact matrices using 5-year age brackets
## Set age brackets and add age groups to df
age.breaks = list("_by5_80" = c(seq(0,80, by = 5), 120)) ## should be a named list
age.vars.names = names(age.breaks)

## create age groups 
tmp.vars = lapply(age.breaks, function(x) {
  maxv = x[length(x)]
  if (maxv > max(merged.df[,"resp_age"], na.rm = TRUE)) {
    mylabels = c(
      paste(x[1:(length(x)-2)], x[2:(length(x)-1)] - 1, sep = "-"),
      paste0(x[length(x)-1], "+")
    )
  } else {
    mylabels = paste(
      x[1:(length(x)-1)],
      x[2:length(x)] - 1,
      sep = "-"
    )
  }
  
  cut(
    merged.df[,"resp_age"],
    breaks = x,
    labels = mylabels,
    right = FALSE
  )
})
tmp.names = paste0("resp_age", age.vars.names)
names(tmp.vars) = tmp.names
merged.df[,tmp.names] = as.data.frame(tmp.vars)

tmp.vars = lapply(age.breaks, function(x) {
  maxv = x[length(x)]
  if (maxv > max(merged.df[,"cont_age"], na.rm = TRUE)) {
    mylabels = c(
      paste(x[1:(length(x)-2)], x[2:(length(x)-1)] - 1, sep = "-"),
      paste0(x[length(x)-1], "+")
    )
  } else {
    mylabels = paste(
      x[1:(length(x)-1)],
      x[2:length(x)] - 1,
      sep = "-"
    )
  }
  
  cut(
    merged.df[,"cont_age"],
    breaks = x,
    labels = mylabels,
    right = FALSE
  )
})
tmp.names = paste0("cont_age", age.vars.names)
names(tmp.vars) = tmp.names
merged.df[,tmp.names] = as.data.frame(tmp.vars)

tmp.vars = lapply(age.breaks, function(x) {
  maxv = x[length(x)]
  if (maxv > max(resp_imp[,"resp_age"], na.rm = TRUE)) {
    mylabels = c(
      paste(x[1:(length(x)-2)], x[2:(length(x)-1)] - 1, sep = "-"),
      paste0(x[length(x)-1], "+")
    )
  } else {
    mylabels = paste(
      x[1:(length(x)-1)],
      x[2:length(x)] - 1,
      sep = "-"
    )
  }
  
  cut(
    resp_imp[,"resp_age"],
    breaks = x,
    labels = mylabels,
    right = FALSE
  )
})
tmp.names = paste0("resp_age", age.vars.names)
names(tmp.vars) = tmp.names
resp_imp[,tmp.names] = as.data.frame(tmp.vars)

## weighted # of resp ##
age_matrix_n = resp_imp %>%
  group_by(resp_age_by5_80) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## prediction age groups ##
nd = expand.grid(resp_age_by5_80 = levels(merged.df$resp_age_by5_80),
                 cont_age_by5_80 = levels(merged.df$cont_age_by5_80))
predict.m = as.data.frame(nd)

# obtain the weighted sum of contacts per respondent-contact group
age_matrix = merged.df %>%
  filter(!is.na(cont_age_by5_80)) %>%
  arrange(cont_age_by5_80) %>%
  group_by(cont_age_by5_80, resp_age_by5_80) %>%
  summarise(weighted_sum = sum(weight*!!sym("binary_col"), na.rm = T))

# combine weighted sum with the data for predictions
age_matrix = left_join(predict.m, age_matrix)

# change NAs to 0s
age_matrix$weighted_sum[is.na(age_matrix$weighted_sum)] = 0

# combine total weighted contact sum with weighted N of people
age_matrix = left_join(age_matrix, age_matrix_n)

# order
age_matrix = age_matrix[order(age_matrix[,"resp_age_by5_80"], age_matrix[,"cont_age_by5_80"]),]

# average number of contacts for the matrix
age_matrix$ncont = age_matrix$weighted_sum/age_matrix$n

matrix_age = age_matrix %>% 
  dplyr::select("resp_age_by5_80", "cont_age_by5_80", "ncont") %>% 
  pivot_wider(names_from = "cont_age_by5_80", values_from = "ncont")

write.csv(matrix_age, 
          paste0("./matrices/-M-by5_80-matrix.csv"),
          row.names = F)








