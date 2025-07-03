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
cont_imp = read.csv("imputed_cnt_reform.csv")
resp_imp = read.csv("imputed_part_reform.csv")


## ESTIMATING AGE-SPECIFIC CONTACT MATRICES
## Merge datasets
combined_df = left_join(resp_imp, cont_imp, by = "part_id")

# adjust school contacts for educators
idx.students = which(combined_df$cnt_relationship == "Student" &
                       combined_df$part_educator == "Yes")

combined_df$cnt_school[idx.students] = TRUE


## recode ages 80+ for visualization
combined_df$cnt_age[which(combined_df$cnt_age >= 80 &
                             combined_df$cnt_age < 85)] = 81
combined_df$part_age[which(combined_df$part_age >= 80 &
                             combined_df$part_age < 85)] = 81

combined_df$cnt_age[which(combined_df$cnt_age >= 85 &
                             combined_df$cnt_age < 90)] = 82
combined_df$part_age[which(combined_df$part_age >= 85 &
                             combined_df$part_age < 90)] = 82

combined_df$cnt_age[which(combined_df$cnt_age > 90)] = 83
combined_df$part_age[which(combined_df$part_age > 90)] = 83


## Single-year age-specific contact matrices
# weighted number of respondents ##
age_matrix_n = combined_df %>%
  group_by(part_age, part_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(part_age) %>%
  summarise(n = sum(weight, na.rm = TRUE))


## the weights of respondents per contact per setting ##
# total
cnt_setting = c("cnt_home", "cnt_school", "cnt_work", "cnt_other")
cont_cols = which(colnames(combined_df) %in% cnt_setting)

nocont = rowSums(!is.na(combined_df[, cont_cols])) == 0
combined_df$binary_col = ifelse(nocont, 0, 1)


## prediction ages ##
nd = expand.grid(part_age = c(0:max(combined_df$part_age)),
                 cnt_age = c(0:max(combined_df$part_age)))

predict.m = as.data.frame(nd)

## calculating the matrices ##
setting_names = c("binary_col", cnt_setting)

## Smoothed matrices
age_matrix_ls = lapply(setting_names, function(setting) {
  
  age_matrix = calc.m.num(mydata = combined_df,
                          myattr = c("part_age", "cnt_age"), # vector of the names of the two attributes
                          setting = setting,
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)
  
  # GAM
  if(setting == "cnt_work"){
    model = gam(ncont ~ te(as.numeric(part_age), as.numeric(cnt_age),
                           k = c(4,4), bs = c("tp","tp"), m = c(0,0)),
                family = nb(),
                data = age_matrix)
  }else{
    model = gam(ncont ~ te(as.numeric(part_age), as.numeric(cnt_age),
                           k = c(10,10), bs = c("cc","cc"), m = c(0,0)),
                family = nb(),
                data = age_matrix)
  }
  summary(model)
  print(AIC(model))

  # predictions
  age_matrix$pred = predict(model, newdata = age_matrix, type = "response")
  
  return(age_matrix)
  
})




## Visualizing contact matrices
titles = c("total", "household", "school", "work", "other")
names(age_matrix_ls) = titles
color.thresh = c(0.4,0.4,0.4,0.4,0.6)
names(color.thresh) = titles
age_matrix_plots = lapply(titles, function(setting) {
  
  age_plot = ggplot(age_matrix_ls[[setting]]) +
    geom_raster(aes(x = part_age, y = cnt_age, fill = pred), interpolate = TRUE) +
    ggtitle(paste("Setting:", capitalize(setting))) + # Main title
    xlab("Respondent Age") + # X-axis title
    ylab("Contact Age") + # Y-axis title
    scale_fill_gradientn(
      colors = c("#005f87", "#87afd7", "white"),
      values = c(0, color.thresh[setting], 1),
      name = "" # Legend title
    ) +
    scale_y_continuous(breaks = c(0,20,40,60,80)) +
    scale_x_continuous(breaks = c(0,20,40,60,80)) +
    coord_fixed(ratio = 1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent", color = "black"),
          plot.background = element_rect(fill = "transparent", color = NA)#,
          #panel.grid.major.x = element_line(linetype = "dashed", color = "grey")
    )
  
  ggsave(paste0("age_", setting,".pdf"), age_plot)
  
  return(age_plot)
  
})
age_matrix_plots


## 5-year age-specific contact matrices
## Set age breaks and add age groups to df
age.breaks = list("_by5_80" = c(seq(0,80, by = 5), 120))

for(i in 1:length(age.breaks)){
  combined_df[, paste0("part_age", names(age.breaks)[i])] = cut(combined_df$part_age, 
                                                             breaks = age.breaks[[i]],
                                                             right = F)
  
  combined_df[, paste0("cnt_age", names(age.breaks)[i])] = cut(combined_df$cnt_age, 
                                                                breaks = age.breaks[[i]],
                                                                right = F)
}


## weighted # of resp ##
age_matrix_n = combined_df %>%
  group_by(part_age_by5_80, part_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(part_age_by5_80) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## prediction age groups ##
nd = expand.grid(part_age_by5_80 = levels(combined_df$part_age_by5_80),
                 cnt_age_by5_80 = levels(combined_df$cnt_age_by5_80))
predict.m = as.data.frame(nd)


## matrices 
# age group labels
age_by5_80  = c(paste0(seq(0, 75, by = 5), "-", seq(4, 79, by = 5)), "80+")

unc_by5_ls <- lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = combined_df
  }else{
    df = combined_df %>% filter(!!sym(setting) == 1)
  }
  
  by5_matrix = calc.m.num(mydata = df,
                          myattr = c("part_age_by5_80", "cnt_age_by5_80"), # vector of the names of the two attributes
                          setting = setting,
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)
  
  by5_matrix$setting = setting
  
  # fix age labels
  levels(by5_matrix$part_age_by5_80) = age_by5_80
  levels(by5_matrix$part_age_by5_80) = age_by5_80
  
  return(by5_matrix)
})


names(unc_by5_ls) = c("Total", "Household", "School", "Work", "Other")

unc.m = lapply(1:length(unc_by5_ls), function(setting){

  matrix_age = unc_by5_ls[[setting]] %>% 
    select("part_age_by5_80", "cnt_age_by5_80", "ncont") %>% 
    pivot_wider(names_from = "cnt_age_by5_80", values_from = "ncont")
  
  write.csv(matrix_age, 
            paste0("csv/by5/",names(unc_by5_ls)[setting],"-by5_80-matrix.csv"),
            row.names = F)
  
  return(matrix_age)
  
})







