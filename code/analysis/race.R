# levels
ethn_cols <- c("White, Non-Hispanic",
               "Hispanic/Latino",
               "Black, Non-Hispanic",
               "Asian, Non-Hispanic",
               "Other, Non-Hispanic")

## Unconditional
## weighted number of respondents ##
merged.df$resp_ethn = factor(merged.df$resp_ethn, ethn_cols)
merged.df$cont_ethn = factor(merged.df$cont_ethn, ethn_cols)

age_matrix_n = merged.df %>%
  group_by(resp_ethn, resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(resp_ethn) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## predictions for the racial/ethnic groups ##
nd = expand.grid(resp_ethn = levels(merged.df$resp_ethn),
                 cont_ethn = levels(merged.df$resp_ethn))
predict.m = as.data.frame(nd)

## Race/Ethnicity
race_matrix_ls = lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = merged.df
  }else{
    df = merged.df %>% filter(!!sym(setting) == 1)
  }
  
  ethn_matrix = calc.m.num(mydata = df,
                           myattr = c("resp_ethn", "cont_ethn"), # vector of the names of the two attributes
                           setting = setting,
                           predict.m = predict.m,
                           denumer.n = age_matrix_n)
  
  return(ethn_matrix)
})

## plotting the matrices ##
titles =c("total", "household", "school", "work", "other")
names(race_matrix_ls) = titles

race_matrix = lapply(1:length(race_matrix_ls), function(setting){
  
  matrix_r = race_matrix_ls[[setting]] %>% 
    dplyr::select("resp_ethn", "cont_ethn", "ncont") %>% 
    pivot_wider(names_from = "cont_ethn", values_from = "ncont")
  
  write.csv(matrix_r,
            paste0(output.dir, "csv/race/", names(race_matrix_ls)[setting],
                   "-race-matrix.csv"),
            row.names = F)
  
  return(matrix_r)
  
})
names(race_matrix) =  titles

## Race
race_matrix_plots = lapply(names(race_matrix_ls), function(setting) {
  df = race_matrix_ls[[setting]]
  
  r_plot = ggplot(df) +
    geom_tile(aes(x = resp_ethn, y = cont_ethn, fill = ncont)) +
    ggtitle(paste("Setting:", to_sentence_case(setting))) + # Main title
    xlab("Respondent") + # X-axis title
    ylab("Contact") + # Y-axis title
    scale_fill_gradientn(colors = c(mycol[c(14, 8)], "white")#,
                         #values = c(0, 0.8, 1)
    ) +
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
  
  ggsave(paste0(output.dir, "3_race_", setting,".pdf"),
         r_plot)
  
  return(r_plot)
  
})
race_matrix_plots

## bar plot
names(race_matrix) =  to_sentence_case(names(race_matrix))
sum_ethn = sapply(1:length(race_matrix), function(setting){
  sum.e = rowSums(race_matrix[[setting]][,2:6])
})
sum_ethn = as.data.frame(sum_ethn)
colnames(sum_ethn) = names(race_matrix)
sum_ethn$Ethn = race_matrix$Total$resp_ethn

sum_ethn_df = sum_ethn %>% 
  pivot_longer(cols = c("Total", "Household", "Work", "School", "Other"),
               names_to = "Setting", values_to = "Average")
sum_ethn_df$Setting = factor(sum_ethn_df$Setting, 
                             levels = c("Total", "Household", 
                                        "School", "Work", "Other"))

sum_ethn_p = ggplot(data = sum_ethn_df,
                    aes(x = Ethn, y = Average, 
                        fill = Setting)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
sum_ethn_p

ggsave(filename = paste0(output.dir, "3_race_summary.pdf"),
       plot = sum_ethn_p)

## Conditional
race_matrix_ls2 = lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = merged.df
  }else{
    df = merged.df %>% filter(!!sym(setting) == 1)
  }
  
  age_matrix_n = df %>%
    group_by(resp_ethn, resp_id) %>%
    summarise(weight = mean(weight, na.rm = TRUE)) %>%
    group_by(resp_ethn) %>%
    summarise(n = sum(weight, na.rm = TRUE))
  
  m = calc.m.num(mydata = df,
                 myattr = c("resp_ethn", "cont_ethn"), # vector of the names of the two attributes
                 setting = setting,
                 predict.m = predict.m,
                 denumer.n = age_matrix_n)
  
  return(m)
})
titles = c("Total", "Household", "School", "Work", "Other")
names(race_matrix_ls2) = titles

race_matrix_plots2 = lapply(titles, function(setting) {
  df = race_matrix_ls2[[setting]]
  df$resp_ethn = factor(df$resp_ethn, ethn_cols)
  df$cont_ethn = factor(df$cont_ethn, ethn_cols)
  
  r_plot = ggplot(df) +
    geom_tile(aes(x = resp_ethn, y = cont_ethn, fill = ncont)) +
    ggtitle(paste("Setting:", setting)) + # Main title
    xlab("Respondent Race/Ethn") + # X-axis title
    ylab("Contact Race/Ethn") + # Y-axis title
    scale_fill_gradientn(colors = c(mycol[c(14, 8)], "white")#,
                         #values = c(0, 0.8, 1)
    ) +
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
  
  ggsave(paste0(output.dir, "15_cond-reth-", setting,".pdf"),
         r_plot)
  
  return(r_plot)
  
})
race_matrix_plots

## barplot

## bar plot
sum_ethn = sapply(2:length(race_matrix_ls2), function(setting){
  
  ethn = race_matrix_ls2[[setting]] %>% 
    dplyr::select("resp_ethn", "cont_ethn", "ncont") %>% 
    pivot_wider(names_from = "cont_ethn", values_from = "ncont")
  sum.e = rowSums(ethn[,2:6])
  
})
sum_ethn = as.data.frame(sum_ethn)
colnames(sum_ethn) = names(race_matrix_ls2)[2:5]
sum_ethn$Ethn = race_matrix_ls2$Total$cont_ethn[1:5]

sum_ethn_df = sum_ethn %>% 
  pivot_longer(cols = c("Household", "Work", "School", "Other"),
               names_to = "Setting", values_to = "Average")
sum_ethn_df$Setting = factor(sum_ethn_df$Setting,
                             levels = c("Household", "School", "Work",
                                        "Other"))

sum_ethn_p = ggplot(data = sum_ethn_df,
                    aes(x = Ethn, y = Average, 
                        fill = Setting)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = seq(round(min(sum_ethn_df$Average), 0), 
                                  round(max(sum_ethn_df$Average), 0), by = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
sum_ethn_p

ggsave(filename = paste0(output.dir, "16_Summary-Ethn-Matrix.pdf"),
       plot = sum_ethn_p)
