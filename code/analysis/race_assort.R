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
assort_df$sample = "Unconditional"
rownames(assort_df) = NULL

assort.values2 <- lapply(names(race_matrix_ls2), function(i) { #setting
  
  x1 = race_matrix_ls2[[i]] %>% 
    dplyr::select("resp_ethn", "cont_ethn", "ncont") %>% 
    pivot_wider(names_from = "cont_ethn", values_from = "ncont")
  x1 = assortativity.calc(x1[,-1])
  return(x1)
  
})
names(assort.values2) = names(race_matrix_ls2)

assort_df2 = as.data.frame(do.call(rbind, assort.values2))
assort_df2$setting = c("Total", "Household", "School", "Work", "Other")
assort_df2$setting = factor(assort_df2$setting,
                           levels = c("Total", "Household", "School", "Work", "Other"))
assort_df2$sample = "Conditional"
rownames(assort_df2) = NULL

combined_assort = rbind(assort_df, assort_df2)

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
