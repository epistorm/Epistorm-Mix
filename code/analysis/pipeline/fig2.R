## adjust the data for plotting matrices ##
merged.df$cont_age_plot = merged.df$cont_age
merged.df$resp_age_plot = merged.df$resp_age

merged.df$cont_age_plot[which(merged.df$cont_age_plot >= 80 &
                             merged.df$cont_age_plot < 85)] = 81
merged.df$resp_age_plot[which(merged.df$resp_age_plot >= 80 &
                             merged.df$resp_age_plot < 85)] = 81
merged.df$cont_age_plot[which(merged.df$cont_age_plot >= 85 &
                             merged.df$cont_age_plot < 90)] = 82
merged.df$resp_age_plot[which(merged.df$resp_age_plot >= 85 &
                             merged.df$resp_age_plot < 90)] = 82
merged.df$cont_age_plot[which(merged.df$cont_age_plot > 90)] = 83
merged.df$resp_age_plot[which(merged.df$resp_age_plot > 90)] = 83

# weighted number of respondents ##
age_matrix_n = merged.df %>%
  group_by(resp_age_plot, resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(resp_age_plot) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## predictions for the ages ##
nd = expand.grid(resp_age_plot = c(0:max(merged.df$resp_age_plot)),
                 cont_age_plot = c(0:max(merged.df$resp_age_plot)))

predict.m = as.data.frame(nd)

## Smoothed matrices
age_matrix_ls = lapply(setting_names, function(setting) {
  
  age_matrix = calc.m.num(mydata = merged.df,
                          myattr = c("resp_age_plot", "cont_age_plot"), # vector of the names of the two attributes
                          setting = setting,
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)
  
  age_matrix$age_diff = as.numeric(age_matrix$resp_age_plot) - as.numeric(age_matrix$cont_age_plot)
  
  # GAM
  if(setting == "work_col"){
    model = gam(ncont ~ te(as.numeric(resp_age_plot), as.numeric(cont_age_plot),
                           k = c(4,4), bs = c("tp","tp"), m = c(0,0)) #+ s(age_diff)
                ,
                family = nb(),
                data = age_matrix)
  }else{
    model = gam(ncont ~ te(as.numeric(resp_age_plot), as.numeric(cont_age_plot),
                           k = c(10,10), bs = c("cc","cc"), m = c(0,0)) #+ s(age_diff)
                ,
                family = nb(),
                data = age_matrix)
  }
  summary(model)
  print(AIC(model))
  #gam.check(model)
  # predictions
  age_matrix$pred = predict(model, newdata = age_matrix, type = "response")
  
  # return(AIC(model))
  return(age_matrix)
  
})

## Plots
titles = c("total", "household", "school", "work", "other")
names(age_matrix_ls) = titles
color.thresh = c(0.4,0.4,0.4,0.4,0.6)
names(color.thresh) = titles
age_matrix_plots = lapply(titles, function(setting) {
  
  age_plot = ggplot(age_matrix_ls[[setting]]) +
    geom_raster(aes(x = resp_age_plot, y = cont_age_plot, fill = pred), interpolate = TRUE) +
    ggtitle(paste("Setting:", to_sentence_case(setting))) + # Main title
    xlab("Respondent Age") + # X-axis title
    ylab("Contact Age") + # Y-axis title
    scale_fill_gradientn(
      colors = c(mycol[c(14, 8)], "white"),
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
  
  ggsave(paste0(output.dir, "2_age_", setting,".pdf"),
         age_plot)
  
  return(age_plot)
  
})
age_matrix_plots

## Summary plot
# define the age grouping variables
ra.var = "resp_age_by5_80"
ca.var = "cont_age_by5_80"

## weighted # of resp ##
age_matrix_n = merged.df %>%
  group_by(!!sym(ra.var), resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(!!sym(ra.var)) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## predictions for the age groups ##
nd = expand.grid(levels(merged.df[[ra.var]]),
                 levels(merged.df[[ca.var]]))
nd = setNames(nd, c(ra.var, ca.var))
predict.m = as.data.frame(nd)

## matrices 

unc_by5_ls <- lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = merged.df
  }else{
    df = merged.df %>% filter(!!sym(setting) == 1)
  }
  
  by5_matrix = calc.m.num(mydata = df,
                          myattr = c(ra.var, ca.var), # vector of the names of the two attributes
                          setting = setting,
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)
  
  by5_matrix$setting = setting
  
  return(by5_matrix)
})
names(unc_by5_ls) = c("Total", "Household", "School", "Work", "Other")

unc.m = lapply(1:length(unc_by5_ls), function(setting){
  
  write.csv(unc_by5_ls[[setting]], 
            paste0(output.dir, "csv/by5/",names(unc_by5_ls)[setting],"-by5_80-df.csv"),
            row.names = F)
  
  matrix_age = unc_by5_ls[[setting]] %>% 
    dplyr::select("resp_age_by5_80", "cont_age_by5_80", "ncont") %>% ## conflicting packages
    pivot_wider(names_from = "cont_age_by5_80", values_from = "ncont")
  
  write.csv(matrix_age, 
            paste0(output.dir, "csv/by5/",names(unc_by5_ls)[setting],"-by5_80-matrix.csv"),
            row.names = F)
  
  return(matrix_age)
  
})
names(unc.m) =  c("Total", "Household", "School", "Work", "Other")

sum_setting = sapply(1:length(unc.m), function(setting){
  sum.setting = rowSums(unc.m[[setting]][,2:18])
})
sum_setting = as.data.frame(sum_setting)
colnames(sum_setting) = names(unc.m)
sum_setting$Age = unc.m$Total$resp_age_by5_80

sum_setting_df = sum_setting %>% 
  pivot_longer(cols = c("Total", "Household", "Other", "School", "Work"),
               names_to = "Setting", values_to = "Average")
sum_setting_df$Setting = factor(sum_setting_df$Setting,
                                levels = c("Total", "Household", "School", "Work",
                                           "Other"))

sum_setting_p = ggplot(data = sum_setting_df,
                       aes(x = Age, y = Average, 
                           color = Setting, group = Setting)) +
  geom_line(linetype = "solid") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
sum_setting_p

ggsave(filename = paste0(output.dir, "2_age_summary.pdf"),
       plot = sum_setting_p)

