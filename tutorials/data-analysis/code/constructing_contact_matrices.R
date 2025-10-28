## Set working directory
setwd("type-your-directory-here")

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

#### ESTIMATING AGE-SPECIFIC CONTACT MATRICES ####
                    
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

## Define the settings for matrix calculations ##
setting_names = c("binary_col") ## more can be added (household, school, work, other) from "cont_cols"

#### Smoothed matrices #### 

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
                       k = c(10,10), # k=10 fits our data best
                       bs = c("cc","cc"), # cc = cyclic cubic spline (best suited for our data)
                       m = c(0,0)),
            family = nb(),
            data = age_matrix_s)
## For work contacts, the cc smoothing spline does not provide sufficient flexibility. 
## Consider using thin plate (tp) regression spline that allows for more flexibility in modeling complex multi-dimensional and non-linear relationships.  

# predictions (predicting smoothed contact matrices)
age_matrix_s$pred = predict(model, newdata = age_matrix_s, type = "response")

#### Contact matrices using 5-year age brackets #### 
## Set age brackets
age.breaks = list("_by5_80" = c(seq(0,80, by = 5), 120)) ## should be a named list, can create multiple age groups
age.vars.names = names(age.breaks)

## Create age group labels
tmp.vars = lapply(age.breaks, function(x) {
  maxv = x[length(x)]
  if (maxv > max(merged.df[,c("resp_age", "cont_age")], na.rm = TRUE)) {
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
})

## Create age group variables for respondents (in merged and respondent data) and contacts
for(i in seq_along(age.breaks)){ # the list may contain multiple age groups
  ## in merged dataset
  for(x in c("resp_age", "cont_age")){
    tmp.names = paste0(x, names(age.breaks)[i])
    merged.df[,tmp.names] = cut(merged.df[,x],
                                breaks = age.breaks[[i]],
                                labels = group.lbls[[i]],
                                right = FALSE
    )
  }
  ## in respondent dataset
  tmp.names = paste0("resp_age", names(age.breaks)[i])
  resp_imp[,tmp.names] = cut(resp_imp[,"resp_age"],
                              breaks = age.breaks[[i]],
                              labels = group.lbls[[i]],
                              right = FALSE
  )
}

## weighted N of resp ##
age_matrix_n = resp_imp %>%
  group_by(resp_age_by5_80) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## all age group combinations ##
## in case some combinations are missing from the sample ##
nd = expand.grid(resp_age_by5_80 = levels(merged.df$resp_age_by5_80),
                 cont_age_by5_80 = levels(merged.df$cont_age_by5_80))
predict.m = as.data.frame(nd)

# obtain the weighted sum of contacts per respondent-contact group
age_matrix = merged.df %>%
  filter(!is.na(cont_age_by5_80)) %>% ## we exclude NA group from the contact matrix
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

## save the matrix as a data frame
write.csv(age_matrix[, c("resp_age_by5_80", "cont_age_by5_80", "ncont")], 
          paste0("./Total-M-by5_80-df.csv"),
          row.names = F)

## transform dataframe into matrix form
matrix_age5 = age_matrix %>% 
  dplyr::select("resp_age_by5_80", "cont_age_by5_80", "ncont") %>% 
  pivot_wider(names_from = "cont_age_by5_80", values_from = "ncont")

write.csv(matrix_age5, 
          paste0("./Total-M-by5_80-matrix.csv"),
          row.names = F)
