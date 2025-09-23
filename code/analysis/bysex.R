merged.df$resp_sex = as.factor(merged.df$resp_sex)
merged.df$resp_sex = droplevels(merged.df$resp_sex)

merged.df$cont_sex = as.factor(merged.df$cont_sex)
merged.df$cont_sex = droplevels(merged.df$cont_sex)

## predictions for the gender groups ##
nd = expand.grid(resp_sex = levels(merged.df$resp_sex),
                 cont_sex = levels(merged.df$resp_sex))
predict.m = as.data.frame(nd)

#### Conditional ####

# matrices
g_ls <- lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = merged.df
  }else{
    df = merged.df %>% filter(!!sym(setting) == 1)
  }
  
  age_matrix_n = df %>%
    group_by(resp_sex, resp_id) %>%
    summarise(weight = mean(weight, na.rm = TRUE)) %>%
    group_by(resp_sex) %>%
    summarise(n = sum(weight, na.rm = TRUE))
  
  m = calc.m.num(mydata = df,
                 myattr = c("resp_sex", "cont_sex"), # vector of the names of the two attributes
                 setting = setting,
                 predict.m = predict.m,
                 denumer.n = age_matrix_n)
  
  m$setting = setting
  return(m)
})
names(g_ls) = c("Total", "Household", "School", "Work", "Other")

matrix_plots <- lapply(seq_along(g_ls), function(setting) {
  
  matrix.1 <- g_ls[[setting]] %>% 
    group_by(resp_sex) %>%
    filter(!all(is.na(ncont))) %>%
    ungroup()
  
  p = ggplot(matrix.1) +
    geom_tile(aes(x = resp_sex, y = cont_sex, fill = ncont)) +
    ggtitle(to_sentence_case(names(g_ls)[setting])) + # Main title
    xlab("Respondent Sex") + # X-axis title
    ylab("Contact Sex") + # Y-axis title
    scale_fill_gradientn(colors = c(mycol[c(14, 8)], "lightblue")#,
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
  
  if (names(g_ls)[setting] != "Total") {
  ggsave(filename = paste0(output.dir, "19_sex-conditional-", 
                           names(g_ls)[setting],".pdf"), 
         plot = p)}
  
  return(p)
})

#### Unconditional ####

## weighted # of resp ##
age_matrix_n = merged.df %>%
  group_by(resp_sex, resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(resp_sex) %>%
  summarise(n = sum(weight, na.rm = TRUE))

# matrices
g_ls2 <- lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = merged.df
  }else{
    df = merged.df %>% filter(!!sym(setting) == 1)
  }
  
  g_matrix = calc.m.num(mydata = df,
                        myattr = c("resp_sex", "cont_sex"), # vector of the names of the two attributes
                        setting = setting,
                        predict.m = predict.m,
                        denumer.n = age_matrix_n)
  
  g_matrix$setting = setting
  return(g_matrix)
})
names(g_ls2) = c("Total", "Household", "School", "Work", "Other")

matrix_plots <- lapply(seq_along(g_ls2), function(setting) {
  
  df = g_ls2[[setting]]
  
  p = ggplot(df) +
    geom_tile(aes(x = resp_sex, y = cont_sex, fill = ncont)) +
    ggtitle(to_sentence_case(names(g_ls)[setting])) + # Main title
    xlab("Respondent Sex") + # X-axis title
    ylab("Contact Sex") + # Y-axis title
    scale_fill_gradientn(colors = c(mycol[c(14, 8)], "lightblue")#,
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
  
  ggsave(filename = paste0(output.dir, 
                           "18_sex-unconditional-",names(g_ls2)[setting],".pdf"), 
         plot = p)
  
  return(p)
})

#### Unconditional Age by Gender ####

# define age group(s)
ra.var = "resp_age_by5_80"
ca.var = "cont_age_by5_80"

## weighted # of resp ##
age_matrix_n = merged.df %>%
  group_by(resp_sex, !!sym(ra.var), resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(resp_sex, !!sym(ra.var)) %>%
  summarise(n = sum(weight, na.rm = TRUE))

# matrices

## predictions for the ages ##
nd = expand.grid(levels(merged.df[[ra.var]]),
                 levels(merged.df[[ca.var]]))
nd = setNames(nd, c(ra.var, ca.var))
predict.m = as.data.frame(nd)

g_ls3 <- lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = merged.df
  }else{
    df = merged.df %>% filter(!!sym(setting) == 1)
  }
  g_matrix = setNames(lapply(unique(df$resp_sex), function(gend1)
    setNames(lapply(unique(df$cont_sex), function(gend2)
      calc.m.num(mydata = df[df$resp_sex == gend1 & df$cont_sex == gend2,],
                 myattr =  c(ra.var, ca.var),
                 setting = setting,
                 predict.m = predict.m,
                 denumer.n = age_matrix_n[age_matrix_n$resp_sex == gend1, ])
    ), unique(df$cont_sex))
  ), unique(df$resp_sex))
  
  
  # g_matrix$setting = setting
  return(g_matrix)
})
names(g_ls3) = c("Total", "Household", "School", "Work", "Other")

matrix_plots_by = lapply(seq_along(g_ls3), function(setting) {
  ggage = setNames(lapply(c(1:2), function(gend1) {
    
    # Get the names for the inner list here, before the inner lapply
    inner_names = names(g_ls3[[setting]][[gend1]])
    
    inner_plots = lapply(c(1:2), function(gend2) {
      
      # print(paste0(setting, ": ", gend1,", ", gend2))
      
      df = g_ls3[[setting]][[gend1]][[gend2]]
      df = df %>% 
        rename_with(~ case_when(
          str_detect(., "resp_age") ~ "resp",
          str_detect(., "cont_age") ~ "cont",
          TRUE ~ .
        ))
      
      level1 = names(g_ls3[[setting]])[gend1]
      level2 = names(g_ls3[[setting]][[gend1]])[gend2]
      
      p = ggplot(df) +
        geom_tile(aes(x = resp, y = cont, fill = ncont)) +
        ggtitle(paste0(to_sentence_case(names(g_ls3)[setting]), ": ",
                       level1, " with ", level2)) + # Main title
        xlab("Respondent Age") + # X-axis title
        ylab("Contact Age") + # Y-axis title
        scale_fill_gradientn(colors = c(mycol[c(14, 8)], "lightblue"),
                             name = "N of contacts") +
        coord_fixed(ratio = 1) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, size = 16),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10),
              panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent", color = "black"),
              plot.background = element_rect(fill = "transparent", color = NA))
      
      return(p)
    })
    
    # Apply names to the inner list here
    setNames(inner_plots, inner_names[1:2])
    
  }), names(g_ls3[[setting]])[1:2]) # Apply names to the outer list here
  
  # Combine them and add an overall title and panel tags
  final_figure = (ggage$Female$Female + ggage$Female$Male +
                     ggage$Male$Female + ggage$Male$Male) +
    plot_annotation(
      #title = "Adjusted Link Between Age and Number of Contacts",
      #subtitle = "Predictions from two separate Negative Binomial GAMs",
      tag_levels = 'A' # automatically labels panels 'A', 'B', 'C', etc.
    ) & theme(plot.tag = element_text(face = 'bold'))

  num = 20
  for (i in 1:setting) {
    num = num + 1
  }
  names(g_ls3)
  ggsave(
    filename = paste0(output.dir, num, 
                      "_sex-by-age-",names(g_ls3)[setting], ".pdf"),
    plot = final_figure,                 
    width = 11,                          
    height = 11,
    units = "in"
  )
  return(final_figure)
})

#### assortativity ####

cond_g = do.call(rbind, g_ls)
cond_g = cond_g[, c("resp_sex", "cont_sex", "setting", "ncont")]
colnames(cond_g) = c("respondent", "contact", "setting", "conditional")

unc_g = do.call(rbind, g_ls2)
unc_g = unc_g[, c("resp_sex", "cont_sex", "setting", "ncont")]
colnames(unc_g) = c("respondent", "contact", "setting", "unconditional")

combined_g = left_join(unc_g, cond_g)

assort.values <- lapply(setting_names, function(i) { #setting
  
  assort_unc_g = combined_g[which(combined_g$setting == i),] %>% 
    dplyr::select("respondent", "contact", "unconditional") %>%
    pivot_wider(names_from = "contact", values_from = "unconditional")
  x1 = assortativity.calc(assort_unc_g[,-1])
  
  assort_cond_g = combined_g[which(combined_g$setting == i),] %>% 
    dplyr::select("respondent", "contact", "conditional") %>%
    pivot_wider(names_from = "contact", values_from = "conditional")
  x2 = assortativity.calc(assort_cond_g[,-1])
  
  assort.val = list(x1, x2)
  names(assort.val) = c("unc", "cond")
  
  return(assort.val) 
  
})

# combine assortativity values in a dataframe
assort_df = do.call(rbind, lapply(assort.values, as.data.frame))
assort_df$setting = c("Total", "Household", "School", "Work", "Other")
assort_df = pivot_longer(assort_df,
                         cols = c("unc", "cond"), 
                         names_to = "Sample",
                         values_to = "Q")
assort_df$Sample = factor(assort_df$Sample)
levels(assort_df$Sample) = c("Conditional", "Unconditional")
assort_df$setting = factor(assort_df$setting,
                           levels = c("Total", "Household", "School", "Work", "Other"))

## barplot
assort_plot = ggplot(data = assort_df,
                     aes(x = setting, y = Q, 
                         fill = Sample)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Assortativity by Gender", x = "Setting", y = "Q-index") +
  # geom_text(aes(label = round(assort, 3)), position = position_dodge(width = 0.9),
  #           vjust = -0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
assort_plot
ggsave(filename = paste0(output.dir, "Summary-Assort-Gender.pdf"), 
       plot = assort_plot)





