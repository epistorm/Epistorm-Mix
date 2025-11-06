## define dir
ext.data.dir = "./data/external_data/mistry-data/"

## define age groups
age.vars.breaks = list(c(0,5,12,18,30,40,50,60,70,120),
                       c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,120),
                       c(0,10,20,30,40,50,60,70,120),
                       c(0,3,7,10,15,20,25,30,35,40,45,50,55,60,65,70,75,120),
                       c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,120),
                       c(seq(0, 80, by = 5), 120))

age.vars.names = c(paste0("_",c("school-groups", "by5_75", "by10_70", 
                                "by3_10-by5_75","by5_70", "by5_80")))

merged.df = create.age.groups(merged.df, input.var = "resp_age", # resp age
                             break.list = age.vars.breaks,
                             suff.list = age.vars.names)
merged.df = create.age.groups(merged.df, input.var = "cont_age", # contact age
                              break.list = age.vars.breaks,
                              suff.list = age.vars.names)

## Matrices

age_listr = c(paste0("resp_age",age.vars.names))
age_listc = c(paste0("cont_age",age.vars.names))

matrix_list <- lapply(seq_along(age_listr), function(age_g) {
  
  ## predictions for the age groups ##
  nd = expand.grid(levels(merged.df[[age_listr[age_g]]]),
                   levels(merged.df[[age_listc[age_g]]]))
  nd = setNames(nd, c(age_listr[age_g], age_listc[age_g]))
  predict.m = as.data.frame(nd)
  
  ## weighted # of resp ##
  age_matrix_n = merged.df %>%
    group_by(!!!syms(age_listr[age_g]), resp_id) %>%
    summarise(weight = mean(weight, na.rm = TRUE)) %>%
    group_by(!!!syms(age_listr[age_g])) %>%
    summarise(n = sum(weight, na.rm = TRUE))
  
  # based on total sample
  by5_matrix = calc.m.num(mydata = merged.df,
                          myattr = c(age_listr[age_g], age_listc[age_g]), # vector of the names of the two attributes
                          setting = "binary_col",
                          predict.m = predict.m,
                          denumer.n = age_matrix_n)
  
  return(by5_matrix)
  
  
})
names(matrix_list) <- c("school-groups", "by5_75", "by10_70", "by3_10-by5_75",
                        "by5_70", "by5_80")

lapply(1:length(matrix_list), function(m) {
  
  mm = matrix_list[[m]] %>% 
    rename_with(~ case_when(
      str_detect(., "resp_age") ~ "respondent",
      str_detect(., "cont_age") ~ "contact",
      TRUE ~ .
    ))
  
  ## transform into a matrix
  mm_mat = mm %>%
    pivot_wider(
      names_from = contact,      # Values from 'contact' become column names
      values_from = ncont,       # Values from 'ncont' fill the cells
      id_cols = respondent       # 'respondent' values become the row identifiers
    )
  
  write.csv(mm_mat, 
            paste0(output.dir, "csv/compare/", names(matrix_list)[m], ".csv"),
            row.names = F)
  
})

#### Convert Mistry Matrix ####

## Mistry --> ageby5_80

# load the data
total.85 = read_csv(paste0(ext.data.dir, "United_States_country_level_M_overall_contact_matrix_85.csv"),
                    col_names = F)
total.85 = as.data.frame(total.85)

ages.df = read_csv(paste0(ext.data.dir, "United_States_country_level_age_distribution_85.csv"),
                   col_names = F)
ages.df = as.data.frame(ages.df)

# add labels
colnames(total.85) = c(0:84)
rownames(total.85) = c(0:84)
colnames(ages.df) = c("Age", "N")

# age group: by 5 to 80
ageby5_80 = c(seq(0,80, by = 5), 120)

Mres = matrix(NA, nrow = length(ageby5_80) -1, ncol = length(ageby5_80) -1)
for(row_m in 1:(length(ageby5_80) - 1)){
  i = ageby5_80[row_m]
  for (col_m in 1:(length(ageby5_80) - 1)) {
    j = ageby5_80[col_m]
    A = ages.df$N[match(c(i:(i+4)), ages.df$Age)]
    M = total.85[match(as.character(c(i:(i+4))), rownames(total.85)), 
                 match(as.character(c(j:(j+4))), colnames(total.85))]
    Mres[row_m,col_m] = sum(A*M)/sum(A)
  }
}

write.csv(Mres, 
          paste0(output.dir, "csv/compare/mistry-by5-matrix.csv"),
          row.names = F)
