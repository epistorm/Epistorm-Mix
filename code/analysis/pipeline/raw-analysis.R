## define directories
raw.dir = "./raw_data/" # raw data
imp.dir = paste0(output.dir, "csv/by5") # imputed data

## define the age grouping variables (only by5_75 and by5_80 in csv output)
ra.var = "resp_age_by5_80"
ca.var = "cont_age_by5_80"

#### Read Clean Data ####
merged.df = read.csv(paste0(raw.dir, "raw_contact2.csv"))
sum.res2 = read.csv(paste0(raw.dir, "raw_resp2.csv"))

### list needed files ###
files_imp = list.files(path = imp.dir, 
                       pattern = paste0(gsub("resp_age_", "", ra.var), "-matrix-df\\.csv$"),
                       full.names = T)

## merge the contact data with respondent data

#### PATCH ####
sum.res2 = rename(sum.res2, resp_id = caseid)
merged_result = left_join(merged.df, sum.res2, by = "resp_id")
# merged_result = merged_result %>%
#   mutate(weight = coalesce(weight.x, weight.y))  # Preserve 'weight' column

# create age groups
age.vars.breaks = list(c(seq(0,80, by = 5), 120)) # add more if needed
age.vars.names = c(paste0("_",c("by5_80"))) # add more if needed
                   
merged_result = create.age.groups(merged_result, input.var = "resp_age", # resp age
                             break.list = age.vars.breaks,
                             suff.list = age.vars.names)
merged_result = create.age.groups(merged_result, input.var = "cont_age", # contact age
                              break.list = age.vars.breaks,
                              suff.list = age.vars.names)

## the weights of respondents per contact per setting ##
# total
cont_cols = grep("^cont_", colnames(merged_result), value = TRUE)
nocont =  rowSums(!is.na(merged_result[, cont_cols])) == 0
merged_result$binary_col = ifelse(nocont, 0, 1)

# specific
merged_result$hh_col = ifelse(merged_result$cont_setting == "Household", 1, 0)
merged_result$school_col = ifelse(merged_result$cont_setting == "School", 1, 0)
merged_result$work_col = ifelse(merged_result$cont_setting == "Work", 1, 0)
merged_result$other_col = ifelse(merged_result$cont_setting == "Other", 1, 0)

#### Calculate Matrices ####
## weighted # of resp ##
age_matrix_n = merged_result %>%
  group_by(!!sym(ra.var), resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(!!sym(ra.var)) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## predictions for the ages ##
nd = expand.grid(levels(merged_result[[ra.var]]),
                 levels(merged_result[[ca.var]]))
nd = setNames(nd, c(ra.var, ca.var))
predict.m = as.data.frame(nd)

matrix_raw <- lapply(setting_names, function(setting) {
  
  if(setting == "binary_col"){
    df = merged_result
  }else{
    df = merged_result %>% filter(!!sym(setting) == 1)
  }
  
  m = calc.m.num(mydata = df,
                 myattr = c(ra.var, ca.var), # vector of the names of the two attributes
                 setting = setting,
                 predict.m = predict.m,
                 denumer.n = age_matrix_n)
  
  return(m)
  
})
names(matrix_raw) <- c("Total", "Household", "School", "Work", "Other")

#### CSV ####
lapply(1:length(matrix_raw), function(m) {
  
  mm = matrix_raw[[m]] %>% 
    dplyr::select(ra.var, ca.var, "ncont") %>% 
    pivot_wider(names_from = ca.var, values_from = "ncont")
  
  write.csv(mm, 
            paste0(output.dir, "csv/raw/",names(matrix_raw)[m], "-",
                   gsub("resp_age_", "", ra.var),
                   "-raw-matrix.csv"),
            row.names = F)
})

#### Matrix Plots ####
matrix_plots <- lapply(1:length(matrix_raw), function(m) {
  
  m_setting = matrix_raw[[m]]
  
  p = ggplot(m_setting) +
    geom_tile(aes(x = !!sym(ra.var), y = !!sym(ca.var), fill = ncont)) +
    ggtitle(paste0(names(matrix_raw)[m], ": Raw")) + # Main title
    xlab("Respondent Age") + # X-axis title
    ylab("Contact Age") + # Y-axis title
    scale_fill_gradientn(colors = c(mycol[c(14, 8)], "lightblue")#,
                         #values = c(0, 0.8, 1)
    ) +
    # scale_x_discrete(labels = age_by5_80) +
    # scale_y_discrete(labels = age_by5_80) +
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
  
  if (names(matrix_raw)[m] == "Total") {
    num = "13_"
  } else {
    num = "14_"
  }
  
  ggsave(filename = paste0(output.dir, num,"raw-", 
                           names(matrix_raw)[m], "-", 
                           gsub("resp_age_", "", ra.var), ".pdf"), 
         plot = p)
  
  return(p)
})
matrix_plots

#### Load Imputed Matrices ####

matrix_imp = list()
for (imp_f in 1:length(files_imp)) {
  data = read.csv(files_imp[[imp_f]])
  matrix_imp[[imp_f]] = data
}
names(matrix_imp) = c("Household", "Other", "School", "Total", "Work")

#### Correlation Plots ####
pdf(paste0(output.dir, "12_raw-imp-plots.pdf"))
raw_imp_plots = lapply(names(matrix_imp), function(m) {
  rawm = matrix_raw[[m]]
  impm = matrix_imp[[m]]
  
  total.weighted = data.frame(x = rawm$ncont, 
                              y = impm$ncont)
  
  # plot
  plot(total.weighted$x, total.weighted$y, 
       main = paste0("Scatterplot of Raw and Imputed: ", m), 
       xlab = "Raw", 
       ylab = "Imputed",
       pch = 16)
  
  # stats
  lm_model = lm(y ~ x, data = total.weighted)
  abline(lm_model, col = "red")
  pearson_r = cor(total.weighted$x, total.weighted$y, method = "pearson")
  spearman_r = cor(total.weighted$x, total.weighted$y, method = "spearman")
  r_squared = summary(lm_model)$r.squared
  
  if (m %in% c("School", "Work")) {
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

matrix_plots <- lapply(names(matrix_imp), function(m) {
  
  rawm = matrix_raw[[m]]
  impm = matrix_imp[[m]]  
  
  p = ggplot(rawm) +
    geom_tile(aes(x = !!sym(ra.var), y = !!sym(ca.var), fill = ncont)) +
    ggtitle(paste0("Setting: ", m, ", Status: Raw")) + # Main title
    xlab("Respondent Age") + # X-axis title
    ylab("Contact Age") + # Y-axis title
    scale_fill_gradientn(colors = c(mycol[c(14, 8)], "lightblue")#,
                         #values = c(0, 0.8, 1)
    ) +
    # scale_x_discrete(labels = age_by5_80) +
    # scale_y_discrete(labels = age_by5_80) +
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
  
  # ggsave(filename = paste0(output.dir, "raw-mat/Raw-", m,
  #                          "-", gsub("resp_age_", "", ra.var), ".pdf"), 
  #        plot = p)
  
  # factor age var so it is ordered correctly in plot
  ages = paste(unique(impm$respondent), sep = ", ")
  impm$respondent = factor(impm$respondent,
                           levels = ages)
  impm$contact = factor(impm$contact,
                           levels = ages)
  
  p2 = ggplot(impm) +
    geom_tile(aes(x = respondent, y = contact, fill = ncont)) +
    ggtitle(paste0("Setting: ", m, ", Status: Imputed")) + # Main title
    xlab("Respondent Age") + # X-axis title
    ylab("Contact Age") + # Y-axis title
    scale_fill_gradientn(colors = c(mycol[c(14, 8)], "lightblue")#,
                         #values = c(0, 0.8, 1)
    ) +
    coord_fixed(ratio = 1) +
    # scale_x_discrete(labels = age_by5_80) +
    # scale_y_discrete(labels = age_by5_80) +
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
  
  if (m == "Total") {
    num = "13_"
  } else {
    num = "14_"
  }
  
  ggsave(filename = paste0(output.dir, num, "imp-", m,
                           "-",gsub("resp_age_", "", ra.var),".pdf"), 
         plot = p2)
  
  return(list(p, p2))
})
matrix_plots

