## Assortativity ##
assort.values <- lapply(names(race_matrix_ls), function(i) { #setting
  
  x1 = race_matrix_ls[[i]] %>% 
    dplyr::select("resp_ethn", "cont_ethn", "ncont") %>% 
    pivot_wider(names_from = "cont_ethn", values_from = "ncont")
  x1 = assortativity.calc(x1[,-1])
  return(x1)
  
})
names(assort.values) = names(race_matrix_ls)

assort_df = as.data.frame(do.call(rbind, assort.values))
assort_df$setting = c("Total", "Household", "School", "Work", "Other")
assort_df$setting = factor(assort_df$setting,
                           levels = c("Total", "Household", "School", "Work", "Other"))
assort_df$sample = "Conditional"
rownames(assort_df) = NULL

## load in assortativity from the main fig (unconditional)
unc_assort = read.csv(paste0(output.dir, "./csv/race/assort_race.csv"))
colnames(unc_assort) = c("setting", "V1")
unc_assort$setting = factor(unc_assort$setting,
                            levels = c("Total", "Household", "School", "Work", "Other"))
unc_assort$sample = "Unconditional"

combined_assort = rbind(assort_df, unc_assort)

## barplot
assort_plot = ggplot(data = combined_assort,
                     aes(x = setting, y = V1, fill = sample)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Assortativity by Race and Ethnicity", x = "Setting", y = "Q-index",
       fill = "Sample") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
assort_plot
ggsave(filename = paste0(output.dir, "17_summary-assort-race.pdf"), 
       plot = assort_plot)
