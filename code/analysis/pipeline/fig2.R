# levels
ethn_cols <- c("White, Non-Hispanic", "Hispanic/Latino",
               "Black, Non-Hispanic",
               "Asian, Non-Hispanic",
               "Other, Non-Hispanic")

## Unconditional
## weighted number of respondents ##
combined_df$resp_ethn = factor(combined_df$resp_ethn, ethn_cols)
combined_df$cont_ethn = factor(combined_df$cont_ethn, ethn_cols)

age_matrix_n = combined_df %>%
  group_by(resp_ethn, resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(resp_ethn) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## predictions for the racial/ethnic groups ##
nd = expand.grid(resp_ethn = levels(combined_df$resp_ethn),
                 cont_ethn = levels(combined_df$resp_ethn))
predict.m = as.data.frame(nd)

## Race/Ethnicity
race_matrix_ls = lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = combined_df
  }else{
    df = combined_df %>% filter(!!sym(setting) == 1)
  }
  
  ethn_matrix = calc.m.num(mydata = df,
                           myattr = c("resp_ethn", "cont_ethn"), # vector of the names of the two attributes
                           setting = setting,
                           predict.m = predict.m,
                           denumer.n = age_matrix_n)
  
  return(ethn_matrix)
})

## plotting the matrices ##
titles = c("Total", "Household", "School", "Work", "Other")
names(race_matrix_ls) = titles

race_matrix = lapply(1:length(race_matrix_ls), function(setting){
  
  matrix_r = race_matrix_ls[[setting]] %>% 
    select("resp_ethn", "cont_ethn", "ncont") %>% 
    pivot_wider(names_from = "cont_ethn", values_from = "ncont")
  
  write.csv(matrix_r, 
            paste0(output.dir, "csv/race/", names(race_matrix_ls)[setting], 
                   "-race-matrix.csv"),
            row.names = F)
  
  return(matrix_r)
  
})
names(race_matrix) =  c("Total", "Household", "School", "Work", "Other")

## Race
race_matrix_plots = lapply(titles, function(setting) {
  df = race_matrix_ls[[setting]]
  
  r_plot = ggplot(df) +
    geom_tile(aes(x = resp_ethn, y = cont_ethn, fill = ncont)) +
    ggtitle(paste("Setting:", setting)) + # Main title
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
  
  ggsave(paste0(output.dir, "fig2/","unc-reth-", setting,".pdf"),
         r_plot)
  
  return(r_plot)
  
})
race_matrix_plots

## bar plot
sum_ethn = sapply(1:length(race_matrix), function(setting){
  sum.e = rowSums(race_matrix[[setting]][,2:6])
})
sum_ethn = as.data.frame(sum_ethn)
colnames(sum_ethn) = names(race_matrix)
sum_ethn$Ethn = race_matrix$Total$resp_ethn

sum_ethn_df = sum_ethn %>% 
  pivot_longer(cols = c("Total", "Household", "Work", "School", "Other"),
               names_to = "Setting", values_to = "Average")
sum_ethn_df$Ethn = factor(sum_ethn_df$Ethn, levels = ethn_cols)
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

ggsave(filename = paste0(output.dir, "fig2/","Summary-Ethn-Matrix.pdf"),
       plot = sum_ethn_p)

## Assortativity ##
assort.values <- lapply(names(race_matrix_ls), function(i) { #setting
  
  race_matrix = race_matrix_ls[[i]] %>% 
    select("resp_ethn", "cont_ethn", "ncont") %>% 
    pivot_wider(names_from = "cont_ethn", values_from = "ncont")
  
  x1 = assortativity.calc(race_matrix[,-1])
  return(x1)
  
})
names(assort.values) = names(race_matrix_ls)
assort.val.df1 = do.call(rbind, assort.values)
write.csv(assort.val.df1, paste0(output.dir, "csv/race/race-assort.csv"))





