## define the age grouping variables (only by5_75 and by5_80 in csv output)
ra.var = "resp_age_by5_80"
ca.var = "cont_age_by5_80"

#### Unweighted ####
## no. of resp ##
age_matrix_n = merged.df %>%
  group_by(!!sym(ra.var), resp_id) %>%
  summarise(weight = 1) %>%
  group_by(!!sym(ra.var)) %>%
  summarise(n = n())

## predictions for the age groups ##
nd = expand.grid(levels(merged.df[[ra.var]]),
                 levels(merged.df[[ca.var]]))
nd = setNames(nd, c(ra.var, ca.var))
predict.m = as.data.frame(nd)

## calculate the matrices for each setting
matrix_unw <- lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = merged.df
  }else{
    df = merged.df %>% filter(!!sym(setting) == 1)
  }
  
  # obtain the not weighted sum of contacts per respondent-contact group
  age_matrix = df %>%
    filter(!is.na(!!sym(ca.var))) %>%
    arrange(!!sym(ca.var)) %>%
    group_by(!!sym(ca.var), !!sym(ra.var)) %>%
    summarise(not_weighted_sum = sum(1*!!sym(setting), na.rm = T))
  
  # combine not weighted sum with the data for predictions
  age_matrix = left_join(predict.m, age_matrix)
  
  # change NAs
  age_matrix$not_weighted_sum[is.na(age_matrix$not_weighted_sum)] = 0
  
  # combine total not weighted contact sum with not weighted N of people
  age_matrix = left_join(age_matrix, age_matrix_n)
  
  # order
  age_matrix = age_matrix[order(age_matrix[[ra.var]], age_matrix[[ca.var]]),]
  
  # average number of contacts for the matrix
  age_matrix$ncont = age_matrix$not_weighted_sum/age_matrix$n
  
  return(age_matrix)
  
})
names(matrix_unw) = c("Total", "Household", "School", "Work", "Other")

lapply(1:length(matrix_unw), function(un) {
  
  mm = matrix_unw[[un]] %>% 
    dplyr::select(ra.var, ca.var, "ncont") %>% 
    pivot_wider(names_from = ca.var, values_from = "ncont")
  
  write.csv(mm, 
            paste0(output.dir, "csv/unw/Unweighted-Matrix-", 
                   names(matrix_unw)[un], "-",
                   gsub("resp_age_", "", ra.var), ".csv"),
            row.names = F)
})

#### Weighted ####
## weighted # of resp ##
age_matrix_n = merged.df %>%
  group_by(!!sym(ra.var), resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(!!sym(ra.var)) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## calculate the matrices ##
matrix_w <- lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = merged.df
  }else{
    df = merged.df %>% filter(!!sym(setting) == 1)
  }
  
  m = calc.m.num(mydata = df,
                 myattr = c(ra.var, ca.var), # vector of the names of the two attributes
                 setting = setting,
                 predict.m = predict.m,
                 denumer.n = age_matrix_n)
  
  return(m)
  
})
names(matrix_w) <- c("Total", "Household", "School", "Work", "Other")

lapply(1:length(matrix_w), function(w){
  
  mm = matrix_w[[w]] %>% 
    dplyr::select(ra.var, ca.var, "ncont") %>% 
    pivot_wider(names_from = ca.var, values_from = "ncont")
  
  write.csv(mm, 
            paste0(output.dir, "csv/unw/Weighted-Matrix-", 
                   names(matrix_w)[w], "-",
                   gsub("resp_age_", "", ra.var), ".csv"),
            row.names = F)
})

#### Comparison Scatterplots ####
pdf(paste0(output.dir, "9_weighted-plots.pdf"))
weighted_plots = lapply(names(matrix_w), function(setting) {
  weightedm = matrix_w[[setting]]
  unweightedm = matrix_unw[[setting]]
  
  total.weighted = data.frame(x = weightedm$ncont, 
                              y = unweightedm$ncont)
  
  # plot
  plot(total.weighted$x, total.weighted$y, 
       main = paste0("Scatterplot of Weighted and Unweighted: ", setting), 
       xlab = "Weighted", 
       ylab = "Unweighted",
       pch = 16)
  
  # stats
  lm_model = lm(y ~ x, data = total.weighted)
  abline(lm_model, col = "red")
  pearson_r = cor(total.weighted$x, total.weighted$y, method = "pearson")
  spearman_r = cor(total.weighted$x, total.weighted$y, method = "spearman")
  r_squared = summary(lm_model)$r.squared
  
  if (setting %in% c("School", "Work")) {
    pearson_r = cor(total.weighted$x, total.weighted$y, use = "complete.obs",
                    method = "pearson")
    spearman_r = cor(total.weighted$x, total.weighted$y, use = "complete.obs",
                     method = "spearman")
  }
  
  # add to plot
  legend("topleft", legend = paste0(
    "R² = ", round(r_squared, 3), "\n",
    "Pearson's r = ", round(pearson_r, 3), "\n",
    "Spearman's r = ", round(spearman_r, 3)),
    bty = "n", cex = 1.5
  )
  
})
dev.off()

#### Comparison Matrices Plots ####
matrix_plots <- lapply(names(matrix_w), function(setting) {
  
  weightedm = matrix_w[[setting]]
  unweightedm = matrix_unw[[setting]]  
  
  if (setting == "Total") {
    num = "10_"
  } else {
    num = "11_"
  }
  
  p = ggplot(weightedm) +
    geom_tile(aes(x = !!sym(ra.var), y = !!sym(ca.var), fill = ncont)) +
    ggtitle(paste0("Setting: ", setting, ", Status: Weighted")) + # Main title
    xlab("Respondent Age") + # X-axis title
    ylab("Contact Age") + # Y-axis title
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
  
  ggsave(filename = paste0(output.dir, num, "weighted-", setting, "-",
                           gsub("resp_age_", "", ra.var),".pdf"), 
         plot = p)
  
  p2 = ggplot(unweightedm) +
    geom_tile(aes(x = !!sym(ra.var), y = !!sym(ca.var), fill = ncont)) +
    ggtitle(paste0("Setting: ", setting, ", Status: Not Weighted")) + # Main title
    xlab("Respondent Age") + # X-axis title
    ylab("Contact Age") + # Y-axis title
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
          plot.background = element_rect(fill = "transparent", color = NA)
    )
  
  ggsave(filename = paste0(output.dir, num, "unweighted-", setting, "-",
                           gsub("resp_age_", "", ra.var), ".pdf"), 
         plot = p2)
  
  return(list(p, p2))
})

matrix_plots



