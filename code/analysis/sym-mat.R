## colors
mycol = c("#005f87", "#87afd7", "white")

## adjust age data for plotting
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

## Unconditional 
## weighted number of respondents 
age_matrix_n = merged.df %>%
  group_by(resp_age_plot, resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(resp_age_plot) %>%
  summarise(n = sum(weight, na.rm = TRUE)) %>%
  ungroup()

## predictions for the ages 
nd = expand.grid(resp_age_plot = c(0:max(merged.df$resp_age_plot)),
                 cont_age_plot = c(0:max(merged.df$resp_age_plot)))

predict.m = as.data.frame(nd)

## age
age_matrix_ls = lapply(setting_names, function(setting) {
  
  
  age_matrix = calc.m.num(mydata = merged.df,
                          myattr = c("resp_age_plot", "cont_age_plot"), # vector of the names of the two attributes
                          setting = setting,
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)
  
  ## GAM
  model <- gam(ncont ~ te(as.numeric(resp_age_plot), as.numeric(cont_age_plot),
                          k = c(10,10), bs = c("cc","cc"), m = c(0,0)),
               family = nb(),
               data = age_matrix)
  summary(model)
  
  ## predictions
  age_matrix$pred = predict(model, newdata = age_matrix, type = "response")
  
  return(age_matrix)
  
})

mynames = c("Total", "Household", "School", "Work", "Other")

sym_ls = lapply(seq_along(age_matrix_ls), function(m) {
  
  sym.m = age_matrix_ls[[m]]
  colnames(sym.m) = c("cont_age_plot", "resp_age_plot", "weighted_sum",
                      "n", "ncont", "pred")
  sym.m = left_join(sym.m, age_matrix_ls[[m]], 
                    by = c("cont_age_plot", "resp_age_plot"))
  sym.m$sn = (sym.m$pred.x+sym.m$pred.y)/2
  
  write.csv(sym.m[,c("cont_age_plot", "resp_age_plot", "sn")],
            file.path(output.dir, "csv", "sym", paste0(mynames[m],
                                                       "-Msym-smoothed-age-df.csv")),
            row.names = F)
  
  return(sym.m)
})
names(sym_ls) = mynames

color.thresh = c(0.4,0.4,0.4,0.4,0.6)
names(color.thresh) = names(sym_ls)

sym_plots = lapply(names(sym_ls), function(setting) {
  
  sym.plot = ggplot(sym_ls[[setting]]) +
    geom_raster(aes(x = resp_age_plot, y = cont_age_plot, fill = sn), interpolate = TRUE) +
    ggtitle(paste("Setting:", setting)) + # Main title
    xlab("Respondent Age") + # X-axis title
    ylab("Contact Age") + # Y-axis title
    scale_fill_gradientn(
      colors = mycol,
      values = c(0, color.thresh[setting], 1),
      name = "" # Legend title
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
  
  ggsave(file.path(output.dir, paste0("6_age-sym-", setting,".pdf")),
         sym.plot,
         width = 4,
         height = 4,
         units = "in",
         dpi = 900)
  return(sym.plot)
  
})
sym_plots
