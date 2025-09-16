#### Add CI ####

# define the age grouping variables
ra.var = "resp_age_by5_65"
ca.var = "cont_age_by5_65"

# define folders to save results
# output.dir = paste0(output.dir, "inpers/")

## predictions for the ages ##
nd = expand.grid(levels(merged.df[[ra.var]]),
                 levels(merged.df[[ca.var]]))
nd = setNames(nd, c(ra.var, ca.var))
predict.m = as.data.frame(nd)

## Conditional matrix ##
age_inperson_matrix_ls = lapply(levels(merged.df$nothome), function(x){
  
  df = merged.df[which(merged.df$nothome == x),] 
  
  # obtain the weighted sum of contacts per respondent-contact group
  age_matrix = df %>%
    filter(!is.na(!!sym(ca.var))) %>%
    arrange(!!sym(ca.var)) %>%
    group_by(!!sym(ca.var), !!sym(ra.var)) %>%
    # summarise(nocont = sum(binary_col, na.rm = T))
    summarise(weighted_sum = sum(weight*binary_col, na.rm = T))
  
  # combine weighted sum with the data for predictions
  age_matrix = left_join(predict.m, age_matrix)
  
  # change NAs
  age_matrix$weighted_sum[is.na(age_matrix$weighted_sum)] = 0
  
  ## weighted number of respondents ##
  npeople = df %>%
    group_by(!!sym(ra.var), resp_id) %>%
    summarise(weight = mean(weight, na.rm = TRUE)) %>%
    group_by(!!sym(ra.var)) %>%
    summarise(n = sum(weight, na.rm = TRUE))
  
  # combine total weighted contact sum with weighted N of people
  age_matrix = left_join(age_matrix, npeople)
  
  # order
  age_matrix = age_matrix[order(age_matrix[[ra.var]], age_matrix[[ca.var]]),]
  
  # average number of contacts for the matrix
  age_matrix$ncont = age_matrix$weighted_sum/age_matrix$n
  
  myattr = c(ra.var, ca.var)
  combined_ci = lapply(1:nrow(age_matrix), function(i) {
    
    data = df[which(df[,myattr[1]] == age_matrix[[ra.var]][i]),]
    data$nocont = ifelse(data[,myattr[2]] == age_matrix[[ca.var]][i] &
                           !is.na(data[,myattr[2]]), 1, 0)
    
    data2 = as.data.frame(data %>%
                            group_by(resp_id) %>%
                            summarise(n = sum(nocont, na.rm = T),
                                      # weighted_sum = sum(nocont * weight, na.rm = T),
                                      weight = mean(weight, na.rm = T), .groups = 'drop'))
    
    if(nrow(data2) >= 5){
      # Define survey design
      survey_design = svydesign(ids = ~1, data = data2, weights = ~ weight)
      # Bootstrap to adjust for negative values in confidence intervals
      bootstrap_rep_design <- svrep::as_bootstrap_design(
        design = survey_design,
        type = "Canty-Davison",
        replicates = 500,
        samp_method_by_stage = "SRSWR"
      )
      # Calculate the weighted mean
      wn = svymean(~ data2$n, bootstrap_rep_design, na.rm = TRUE,
                   return.replicates = T)
      ci = quantile(wn$replicates, p=c(0.025, 0.975))
    }else{
      wn = 0
      ci = c(0,0)
    }
    error_bars = data.frame(cont_age_65 = age_matrix[[ca.var]][i], 
                            resp_age_65 = age_matrix[[ra.var]][i],
                            wav_nc = wn[[1]], lower = ci[1], upper = ci[2])
    return(error_bars)
    
  })
  
  combined_ci_df = do.call(rbind, combined_ci)
  
  colnames(combined_ci_df)[1:2] = c(ca.var, ra.var)
  age_matrix1 = left_join(age_matrix, combined_ci_df)
  return(age_matrix1)
})
names(age_inperson_matrix_ls) = levels(merged.df$nothome)


## Barplot ##

sum_inpers = lapply(1:length(age_inperson_matrix_ls), function(nothome) {
  
  sum.inpers = age_inperson_matrix_ls[[nothome]] %>% 
    group_by(!!sym(ra.var)) %>% 
    summarise(ncont = sum(ncont, na.rm = T),
              lower = sum(lower, na.rm = T),
              upper = sum(upper, na.rm = T))
  sum.inpers$status = names(age_inperson_matrix_ls)[nothome]
  return(sum.inpers)
  
})
sum_inpers = do.call(rbind, sum_inpers)
sum_inpers = as.data.frame(sum_inpers)
sum_inpers$status = factor(sum_inpers$status,
                           levels = c("In Person", "Not In Person",
                                      "Not Student or Employed"))

# change name
colnames(sum_inpers)[1] = "resp"

# plot
sum_inpers_p = ggplot(data = sum_inpers,
                      aes(x = resp, y = ncont, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = upper, ymin = lower), color = "black",
                position = position_dodge(0.9),
                width = 0.2) +
  labs(x = "Respondent Age", y = "Average Contacts", fill = "Status") +
  ggtitle("Average Contacts by Age by In Person/Remote Status") +
  scale_fill_manual(
    values = c(mycol[c(14, 8)], "lightblue")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  coord_cartesian(expand = F)
sum_inpers_p

ggsave(filename = paste0(output.dir, "8_summary-inpers-CI.pdf"),
       plot = sum_inpers_p)

