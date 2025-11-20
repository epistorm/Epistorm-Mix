## Define output directories - ADD
csv.dir = file.path(output.dir, "csv", "freq")
matrix.dir = "matrices"
mycol = c("#005f87", "#87afd7", "lightblue")

## choose the age group to calculate the matrix 
ra.var = "resp_age_by5_75"
ca.var = "cont_age_by5_75"
single.yr.T = F # if T, single year matrices are calculated for 0-75
readF.T = F # if T, frequency based matrices are read from github (by 5 to 75 group) instead of calculated

## single year age groups
if (single.yr.T == T){
  
  ## create single year ages
  single.breaks = c(0:75,120)
  merged.df$resp_age_sy = mycut(merged.df$resp_age,
                                  breaks = single.breaks)
  merged.df$cont_age_sy = mycut(merged.df$cont_age,
                                  breaks = single.breaks)

  ## fix labels
  ages = c("resp_age_sy", "cont_age_sy")
  for (i in ages) {
    ordered = sapply(strsplit(levels(merged.df[,i]), "-"), function(x){
      if(length(x) == 2 & x[1] == x[2]){
        x[1]
      }else if(length(x) == 1){
        x
      }else{
        paste0(x, collapse = "-")
      }
    })
    merged.df[,i] = factor(merged.df[,i],levels = ordered)
  }
  
  merged.df$respag = merged.df$resp_age_sy
  merged.df$contag = merged.df$cont_age_sy
  
  ## change the age group for matrix calculation
  ra.var = "resp_age_sy"
  ca.var = "cont_age_sy"
  
}

## weighted # of resp ##
age_matrix_n = merged.df %>%
  group_by(!!sym(ra.var), resp_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(!!sym(ra.var)) %>%
  summarise(n = sum(weight, na.rm = TRUE))

## predictions for the age groups ##
nd = expand.grid(levels(merged.df[[ra.var]]),
                 levels(merged.df[[ca.var]]))
nd = setNames(nd, c(ra.var, ca.var))
predict.m = as.data.frame(nd)

#### Frequency Matrices ####
if (readF.T == T){
  ## read provided F-matrices for ages grouped by 5 to 75
  f_matrix.folder = file.path(matrix.dir, "F_matrix")
  cnt_sett = c("Household", "Work", "School", "Other")
  freq_m = lapply(cnt_sett, function(x){
    tmp = read.csv(file.path(f_matrix.folder, paste0(x, "-F-by5_75-matrix.csv")))
    tmp = tmp %>%
      pivot_longer(cols = colnames(tmp)[which(colnames(tmp) != "X")],
                   names_to = "contact",
                   values_to = "ncont")
    tmp$cont_age = gsub("\\.", "-", gsub("X", "", tmp$cont_age))
    tmp$cont_age[which(tmp$cont_age == "75-")] = "75+"
    colnames(tmp)[which(colnames(tmp) == "X")] = "respondent"
    colnames(tmp)[which(colnames(tmp) == "ncont")] = "fcont"
    tmp$epistorm[which(is.na(tmp$epistorm))] = 0
    as.data.frame(tmp)
  })
  names(freq_m) = cnt_sett
  
} else {
  ## calculate c(ij)k(r) 
  r_unique = unique(merged.df$resp_id)
  k_unique = unique(merged.df$cont_setting)
  
  f_ij = list()
  
  for (k in na.omit(k_unique)) {
    print(k)
    f_ij[[k]] = matrix(NA, ncol = length(levels(merged.df[[ra.var]])),
                       nrow = length(levels(merged.df[[ra.var]])))
    gamma_ij = matrix(NA, ncol = length(levels(merged.df[[ra.var]])),
                      nrow = length(r_unique))
    resp_age = rep(NA, length(r_unique))
    for (r in 1:length(r_unique)) {
      resp_age[r] = merged.df[[ra.var]][which(merged.df$resp_id == r_unique[r])][1]
      cont_age = merged.df[[ca.var]][which(merged.df$resp_id == r_unique[r] & 
                                             merged.df$cont_setting == k)]
      c_ij = as.data.frame(table(cont_age))
      c_ij$frac = c_ij$Freq/sum(c_ij$Freq, na.rm = T)
      idx.na = grep("NA",c_ij$cont_age)
      if(length(idx.na) > 0){
        c_ij = c_ij[-idx.na,]
      }
      gamma_ij[r,] = c_ij$frac
    }
    for (age in unique(resp_age)) {
      N_i = length(unique(merged.df$resp_id[which(merged.df[[ra.var]] == 
                                                    levels(merged.df[[ra.var]])[age])]))
      age_g = which(resp_age == age)
      freq = colSums(gamma_ij[age_g,], na.rm = T)
      f_ij[[k]][age,] = freq/N_i
    }
    print(rowSums(f_ij[[k]]))
  }
  
  freq_m = lapply(1:length(f_ij), function(m){
    colnames(f_ij[[m]]) = levels(merged.df[[ra.var]]) # contacts and respondents have same groups/levels
    rownames(f_ij[[m]]) = levels(merged.df[[ra.var]])
    write.csv(f_ij[[m]],
              file.path(output.dir, "csv", "freq", paste0(names(f_ij)[m],"-F-", gsub("resp_age_", "", ra.var), "-matrix.csv")),
              row.names = T)
    
    ## convert the matrix to a data frame and add the respondent column
    df = as.data.frame(f_ij[[m]]) %>%
      tibble::rownames_to_column(var = "respondent")
    
    ## pivot the data frame to the long format
    long_df = df %>%
      pivot_longer(
        cols = -respondent,        # Exclude the respondent column from pivoting
        names_to = "contact",      # Name of the new column for the original column names
        values_to = "fcont"        # Name of the new column for the cell values
      )
    print(long_df)
    write.csv(long_df,
              file.path(output.dir, "csv", "freq", paste0(names(f_ij)[m],"-F-", gsub("resp_age_", "", ra.var), "-df.csv")),
              row.names = F)
    return(long_df)
  })
  
  names(freq_m) = names(f_ij)
}

matrix_plots <- lapply(seq_along(freq_m), function(setting) {
  
  df = freq_m[[setting]] 
  df$resp = factor(df$respondent,
                   levels = levels(merged.df[[ra.var]]))
  df$age = factor(df$contact,
                  levels = levels(merged.df[[ra.var]]))
  
  p = ggplot(df) +
    geom_tile(aes(x = resp, y = age, fill = fcont)) +
    ggtitle(k_unique[setting]) + # Main title
    xlab("Respondent Age") +     # X-axis title
    ylab("Contact Age") +        # Y-axis title
    scale_fill_gradientn(colors = mycol, name = "F") +
    coord_fixed(ratio = 1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent", color = "black"),
          plot.background = element_rect(fill = "transparent", color = NA))
  
  ggsave(filename = file.path(output.dir, paste0("4_",
                           names(freq_m)[setting], "-F-", gsub("resp_age_","", ra.var), ".pdf")), 
         plot = p,
         width = 4,
         height = 4,
         units = "in",
         dpi = 900)

  return(p)
})
matrix_plots
