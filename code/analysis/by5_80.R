# define the age grouping variables
ra.var = c("resp_age_by5_80", "resp_age_by5_75")
ca.var = c("cont_age_by5_80", "cont_age_by5_75")
  
# define folders to save results
csv.dir = paste0(output.dir, "csv/by5/")

for (age in 1:length(ra.var)) {

  ## predictions for the age groups ##
  nd = expand.grid(levels(merged.df[[ra.var[age]]]),
                   levels(merged.df[[ca.var[age]]]))
  nd = setNames(nd, c(ra.var[age], ca.var[age]))
  predict.m = as.data.frame(nd)
  
  #### Conditional ####
  ## use a different age bracket for work (as sample for working individuals 65+ is limited)
  matrix_list <- lapply(setting_names, function(setting) {
    
    # subset the data based on contact setting
    if(setting == "binary_col"){
      df = merged.df
    }else{
      df = merged.df %>% filter(!!sym(setting) == 1)
    }
    
    df[[ra.var[age]]] = droplevels(df[[ra.var[age]]])
    df[[ca.var[age]]] = droplevels(df[[ca.var[age]]])
    
    if(setting == "work_col"){ ## adjust age brackets for work settings
      mygroups = c(ra.var[age], ca.var[age])
      nd = expand.grid(levels(df[[ra.var[age]]]),
                       levels(df[[ca.var[age]]]))
      nd = setNames(nd, c(ra.var[age], ca.var[age]))
      predict.m = as.data.frame(nd)
    }else{
      mygroups = c(ra.var[age], ca.var[age])
    }
    ## calculate the (weighted) number of respondents in each age group
    age_matrix_n = df %>%
      group_by(!!!syms(mygroups[1]), resp_id) %>%
      summarise(weight = mean(weight, na.rm = TRUE)) %>%
      group_by(!!!syms(mygroups[1])) %>%
      summarise(n = sum(weight, na.rm = TRUE))
    
    # calculate the matrix conditional on resp having contact in setting
    m = calc.m.num(mydata = df,
                   myattr = mygroups, # vector of the names of the two attributes
                   setting = setting,
                   predict.m = predict.m,
                   denumer.n = age_matrix_n)
    
    # add setting identifier
    m$setting = setting
    
    return(m)
  })
  names(matrix_list) <- c("Total", "Household", "School", "Work", "Other")
  
  # save the weighted matrices for total and setting-specific
  lapply(1:length(matrix_list), function(m) {
    
    matrix_by5 = matrix_list[[m]] %>% 
      dplyr::select(ra.var[age], ca.var[age], "ncont") %>% 
      pivot_wider(names_from = ca.var[age], values_from = "ncont")
    
    write.csv(matrix_by5,
              paste0(csv.dir, 
                     # "cond/",
                     names(matrix_list)[m], "-", 
                     gsub("resp_age_","", ra.var[age]),
                     "-matrix-cond.csv"),
              row.names = F)
  })
  
  ## plot the matrices
  matrix_plots <- lapply(seq_along(matrix_list), function(setting) {
    
    # pull out the dataframe
    m <- matrix_list[[setting]]
    
    # Use grep to find column names dynamically
    resp_col <- grep("^resp_", names(m), value = TRUE)
    cont_col <- grep("^cont_", names(m), value = TRUE)
    
    # ignore NAs
    matrix.1 <- m %>%
      group_by(across(all_of(resp_col))) %>%
      filter(!all(is.na(ncont))) %>%
      ungroup()
    
    p = ggplot(matrix.1) +
      geom_tile(aes(x = !!sym(resp_col), y = !!sym(cont_col), fill = ncont)) +
      ggtitle(to_sentence_case(names(matrix_list)[setting])) + # Main title
      xlab("Respondent Age") + # X-axis title
      ylab("Contact Age") + # Y-axis title
      scale_fill_gradientn(colors = c(mycol[c(14, 8)], "lightblue")
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
    
    ggsave(filename = paste0(output.dir, "5_",
                             # "cond/",
                             gsub("resp_age_","", ra.var[age]), 
                              "-Conditional-",
                              names(matrix_list)[setting], ".pdf"),
           plot = p)
    
    return(p)
  })
  matrix_plots
  
  
  #### Unconditional ####
  ## weighted # of resp ##
  age_matrix_n = merged.df %>%
    group_by(!!!syms(ra.var[age]), resp_id) %>%
    summarise(weight = mean(weight, na.rm = TRUE)) %>%
    group_by(!!!syms(ra.var[age])) %>%
    summarise(n = sum(weight, na.rm = TRUE))
  
  # calculate matrices for total sample
  unc_by5_ls <- lapply(setting_names, function(setting) {
    
    # subset data based on contact setting
    if(setting == "binary_col"){
      df = merged.df
    }else{
      df = merged.df %>% filter(!!sym(setting) == 1)
    }
    
    # based on total sample
    by5_matrix = calc.m.num(mydata = df,
                            myattr = c(ra.var[age], ca.var[age]), # vector of the names of the two attributes
                            setting = setting,
                            predict.m = predict.m,
                            denumer.n = age_matrix_n)
    
    by5_matrix$setting = setting
    
    return(by5_matrix)
  })
  names(unc_by5_ls) = c("Total", "Household", "School", "Work", "Other")
  
  # write file
  lapply(seq_along(unc_by5_ls), function(s) {
    m = unc_by5_ls[[s]]
    m = rename(m, respondent = !!sym(ra.var[age]),
               contact = !!sym(ca.var[age]))
    m = m[, c("respondent", "contact", "ncont")]
    write.csv(m, file = paste0(csv.dir,
                               names(unc_by5_ls)[s], "-",
                               gsub("resp_age_","", ra.var[age]),
                               "-matrix-df.csv"),
              row.names = F)
    ## transform into a matrix
    mm_df <- m %>%
      pivot_wider(
        names_from = contact,      # Values from 'contact' become column names
        values_from = ncont,       # Values from 'ncont' fill the cells
        id_cols = respondent       # 'respondent' values become the row identifiers
      )
    
    # Convert the resulting data frame to a matrix
    mm <- as.matrix(mm_df %>% dplyr::select(-respondent))
    rownames(mm) <- mm_df$respondent
    
    print(mm)
    write.csv(mm, file = paste0(csv.dir, 
                                # "unc/",
                                names(unc_by5_ls)[s], "-",
                                gsub("resp_age_","", ra.var[age]), 
                                "-matrix-unc.csv"),
              row.names = T)
  })
  
  # plots
  matrix_plots <- lapply(seq_along(unc_by5_ls), function(setting) {
    
    df = unc_by5_ls[[setting]]
    colnames(df)[1:2] = c("resp", "cont")
    
    p = ggplot(df) +
      geom_tile(aes(x = resp, y = cont, fill = ncont)) +
      ggtitle(to_sentence_case(names(unc_by5_ls)[setting])) + # Main title
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
    
    ggsave(filename = paste0(output.dir, "2_",
                             # "unc/",
                             gsub("resp_age_","", ra.var[age]), "-Unconditional-",
                             names(unc_by5_ls)[setting],".pdf"),
                             plot = p)
    
    return(p)
  })
  matrix_plots
  
  #### assortativity plot ####
  # extract dataframes from conditional matrix nested list
  matrix_list_df = lapply(1:length(matrix_list), function(df)
    matrix_list[[df]]
  )
  # one combined dataframe for conditional matrices
  tmp = lapply(matrix_list_df, function(x){
    names(x) = gsub("age_by5.*", "age_by5", names(x))
    return(x)
  })
  cond_age = do.call(rbind, tmp)
  cond_age = cond_age[, c(grep("resp_age", colnames(cond_age), value = T), 
                          grep("cont_age", colnames(cond_age), value = T),
                          "setting", "ncont")]
  colnames(cond_age) = c("respondent", "contact", "setting", "conditional")
  
  # one combined dataframe for unconditional matrices
  unc_age = do.call(rbind, unc_by5_ls)
  unc_age = unc_age[, c(grep("resp_age", colnames(unc_age), value = T), 
                        grep("cont_age", colnames(unc_age), value = T), 
                        "setting", "ncont")]
  colnames(unc_age) = c("respondent", "contact", "setting", "unconditional")
  
  work_cols = levels(cond_age$respondent)[
    sapply(strsplit(levels(cond_age$respondent), "-"), function(x){
      y = as.numeric(gsub("\\+","",x))
      check = ifelse(length(y) == 1, y[1] < 65, (y[1] >= 18 & y[2] < 65))
      return(check)
    })]
  
  school_cols = levels(cond_age$respondent)[
    sapply(strsplit(levels(cond_age$respondent), "-"), function(x){
      y = as.numeric(gsub("\\+","",x))
      check = ifelse(length(y) == 1, y[1] < 18, (y[1] >= 5 & y[2] < 18))
      return(check)
    })]
  
  # calculating assortativity
  assort.values <- lapply(setting_names, function(i) { #setting
    
    assort_unc_age = unc_age[which(unc_age$setting == i),] %>% 
      dplyr::select("respondent", "contact", "unconditional") %>%
      pivot_wider(names_from = "contact", values_from = "unconditional")
    
    if (i == "work_col") {
      assort_cond_age = cond_age[which(cond_age$setting == i & !grepl("\\+", cond_age$respondent)),] %>% 
        dplyr::select("respondent", "contact", "conditional") %>%
        pivot_wider(names_from = "contact", values_from = "conditional")
    } else {
      assort_cond_age = cond_age[which(cond_age$setting == i),] %>% 
        dplyr::select("respondent", "contact", "conditional") %>%
        pivot_wider(names_from = "contact", values_from = "conditional")
    }
    
    if (i == "work_col") {
      assort_unc_age = assort_unc_age[which(assort_unc_age$respondent %in% work_cols), 
                                      which(colnames(assort_unc_age) %in% work_cols)]
      x1 = assort_unc_age
      
      assort_cond_age = assort_cond_age[which(assort_cond_age$respondent %in% work_cols), 
                                        which(colnames(assort_cond_age) %in% work_cols)]
      x2 = assort_cond_age
      
    }else if (i == "school_col") {
      
      assort_unc_age = assort_unc_age[which(assort_unc_age$respondent %in% school_cols), 
                                      which(colnames(assort_unc_age) %in% school_cols)]
      x1 = assort_unc_age
      
      assort_cond_age = assort_cond_age[which(assort_cond_age$respondent %in% school_cols), 
                                        which(colnames(assort_cond_age) %in% school_cols)]
      x2 = assort_cond_age
      
    }else{
      x1 = assort_unc_age[,-1]
      x2 = assort_cond_age[,-1]
    }
    
    x1 = assortativity.calc(x1)
    x2 = assortativity.calc(x2)
    
    assort.age = list(x1, x2)
    names(assort.age) = c("unconditional", "conditional")
    return(assort.age)
    
  })
  names(assort.values) = setting_names
  
  # combine assortativity values in a dataframe
  assort_df = do.call(rbind, lapply(assort.values, as.data.frame))
  assort_df$setting = c("Total", "Household", "School", "Work", "Other")
  assort_df = pivot_longer(assort_df,
                           cols = c("unconditional", "conditional"), 
                           names_to = "pop",
                           values_to = "assort")
  assort_df$pop = factor(assort_df$pop)
  levels(assort_df$pop) = c("Conditional", "Unconditional")
  assort_df$setting = factor(assort_df$setting,
                             levels = c("Total", "Household", "School", "Work", "Other"))
  ## barplot
  assort_plot = ggplot(data = assort_df,
                       aes(x = setting, y = assort, 
                           fill = pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Assortativity by Age", x = "Setting", y = "Q-index") +
    # geom_text(aes(label = round(assort, 3)), position = position_dodge(width = 0.9),
    #           vjust = -0.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  assort_plot
  ggsave(filename = paste0(output.dir, "3_",gsub("resp_age_","", ra.var[age]),
                           "-Summary-Assort-Age.pdf"), 
         plot = assort_plot)
  

}

