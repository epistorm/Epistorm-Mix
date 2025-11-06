# define the age grouping variables
ra.var = c("resp_age_by5_80", "resp_age_by5_75")
ca.var = c("cont_age_by5_80", "cont_age_by5_75")

# define folders to save results
csv.dir = paste0(output.dir, "csv/compare/")

## transform the mode of attendance into three-level variable
merged.df$nothome[which(merged.df$nothome == "Not at work/in school")] = 
  "Worked/studied fully online"
merged.df$nothome = droplevels(merged.df$nothome)
levels(merged.df$nothome) = c("In Person", "Not In Person", "Not Student or Employed")

for (age in 1:length(ra.var)) {
  
  ## predictions for the ages ##
  nd = expand.grid(levels(merged.df[[ra.var[age]]]),
                   levels(merged.df[[ca.var[age]]]))
  nd = setNames(nd, c(ra.var[age], ca.var[age]))
  predict.m = as.data.frame(nd)
  
  #### Conditional ####
  age_inperson_matrix_ls = lapply(levels(merged.df$nothome), function(x){
    
    df = merged.df[which(merged.df$nothome == x),] 
    
    ## weighted number of respondents ##
    npeople = df %>%
      group_by(!!sym(ra.var[age]), resp_id) %>%
      summarise(weight = mean(weight, na.rm = TRUE)) %>%
      group_by(!!sym(ra.var[age])) %>%
      summarise(n = sum(weight, na.rm = TRUE))
    
    age_matrix = calc.m.num(mydata = df,
                            myattr = c(ra.var[age], ca.var[age]), # vector of the names of the two attributes
                            setting = "binary_col",
                            predict.m = predict.m,
                            denumer.n = npeople)
    
    return(age_matrix)
  })
  names(age_inperson_matrix_ls) = levels(merged.df$nothome)
  
  #### New weights ####
  # not std/emp (3) and in person (1) combined vs. not in person (2)
  # (ncont1 * (n1 + n2) + ncont3 * n3)/(n1 + n2 + n3) = ncont(in person)
  num1 = age_inperson_matrix_ls$`In Person`$ncont * 
    (age_inperson_matrix_ls$`In Person`$n + age_inperson_matrix_ls$`Not In Person`$n)
  num3 = age_inperson_matrix_ls$`Not Student or Employed`$ncont *
    age_inperson_matrix_ls$`Not Student or Employed`$n
  denom = age_inperson_matrix_ls$`Not Student or Employed`$n +
    age_inperson_matrix_ls$`In Person`$n + age_inperson_matrix_ls$`Not In Person`$n
  ncont_inpers = (num1 + num3)/denom
  age_inperson_matrix_ls$`In Person`$ncont_new = ncont_inpers
  
  # not std/emp and not in person combined vs. in person
  # (ncont2 * (n1 + n2) + ncont3 * n3)/(n1 + n2 + n3) = ncont(not in person)
  num2 = age_inperson_matrix_ls$`Not In Person`$ncont * 
    (age_inperson_matrix_ls$`In Person`$n + age_inperson_matrix_ls$`Not In Person`$n)
  ncont_notinpers = (num2 + num3)/denom
  age_inperson_matrix_ls$`Not In Person`$ncont_new = ncont_notinpers

  
  agepers_matrix_plots = lapply(names(age_inperson_matrix_ls)[1:2], function(x){
    
    # change age labels
    colnames(age_inperson_matrix_ls[[x]])[1:2] = c("resp", "cont")
    
    df = age_inperson_matrix_ls[[x]]
    
    if (x == "Not In Person") {
      x = "Remote"
    }
    
    age_plot = ggplot(df) +
      geom_tile(aes(x = resp, y = cont, fill = ncont_new)) +
      ggtitle(x) + # Main title
      xlab("Respondent Age") + # X-axis title
      ylab("Contact Age") + # Y-axis title
      scale_fill_gradientn(colors = c(mycol[c(14, 8)], "lightblue", "white"),
                           values = c(0, 0.3, 0.7,1),
                           limits = c(0, max(age_inperson_matrix_ls$`In Person`$ncont_new))
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
    
    ggsave(paste0(output.dir, "7_", x, "-", gsub("resp_age_","", ra.var[age]),
                  ".pdf"), age_plot)
    
    return(age_plot)
  })
  agepers_matrix_plots
  
  #### save the matrices as csv ####
  matrix_inpers = lapply(names(age_inperson_matrix_ls)[1:2], function(mat){
    # write.csv(age_inperson_matrix_ls[[mat]],
    #           paste0("csv/inpers/", mat, "-Cond.csv"),
    #           row.names = F)
    
    matrix_inpers = age_inperson_matrix_ls[[mat]] %>% 
      dplyr::select(ra.var[age], ca.var[age], "ncont_new") %>% 
      pivot_wider(names_from = ca.var[age], values_from = "ncont_new")
    
    write.csv(matrix_inpers,
              paste0(csv.dir, gsub(" ","-",mat), "-", gsub("resp_age_","", ra.var[age]), 
                     "-cond-matrix.csv"),
              row.names = F)
    
    return(matrix_inpers)
    
  }) 
  # names(matrix_inpers) = names(age_inperson_matrix_ls)[1:2]
  # matrix_inpers$`In Person`[,-1] - matrix_inpers$`Not In Person`[,-1]
  
}

