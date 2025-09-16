## define dir
ext.data.dir = "./external_data/mistry-data/"

## define age groups
age.vars.breaks = list(c(0,5,12,18,30,40,50,60,70,120),
                       c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,120),
                       c(0,10,20,30,40,50,60,70,120),
                       c(0,3,7,10,15,20,25,30,35,40,45,50,55,60,65,70,75,120),
                       c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,120),
                       c(seq(0, 80, by = 5), 120))

age.vars.names = c(paste0("_",c("uk.bel.neth", "swiss.prem", "suz","hun","italy",
                                "mistry")))

merged.df = create.age.groups(merged.df, input.var = "resp_age", # resp age
                             break.list = age.vars.breaks,
                             suff.list = age.vars.names)
merged.df = create.age.groups(merged.df, input.var = "cont_age", # contact age
                              break.list = age.vars.breaks,
                              suff.list = age.vars.names)

# uk.bel.neth.breaks = c(0,5,12,18,30,40,50,60,70,120)
# merged.df$resp_age_uk.bel.neth = cut(merged.df$resp_age, 
#                               breaks = uk.bel.neth.breaks,
#                               right = F)
# merged.df$cont_age_uk.bel.neth = cut(merged.df$cont_age,
#                               breaks = uk.bel.neth.breaks,
#                               right = F)
# 
# swiss.prem.breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,120)
# merged.df$resp_age_swiss.prem = cut(merged.df$resp_age, 
#                               breaks = swiss.prem.breaks,
#                               right = F)
# merged.df$cont_age_swiss.prem = cut(merged.df$cont_age,
#                               breaks = swiss.prem.breaks,
#                               right = F)
# 
# suz.breaks = c(0,10,20,30,40,50,60,70,120)
# merged.df$resp_age_suz = cut(merged.df$resp_age, 
#                               breaks = suz.breaks,
#                               right = F)
# merged.df$cont_age_suz = cut(merged.df$cont_age,
#                               breaks = suz.breaks,
#                               right = F)
# 
# hun.breaks = c(0,3,7,10,15,20,25,30,35,40,45,50,55,60,65,70,75,120)
# merged.df$resp_age_hun = cut(merged.df$resp_age, 
#                               breaks = hun.breaks,
#                               right = F)
# merged.df$cont_age_hun = cut(merged.df$cont_age,
#                               breaks = hun.breaks,
#                               right = F)
# 
# # g5.breaks = c(0:83,84,120)
# # merged.df$resp_age_g5 = cut(merged.df$resp_age, 
# #                               breaks = g5.breaks,
# #                               right = F)
# # merged.df$cont_age_g5 = cut(merged.df$cont_age,
# #                               breaks = g5.breaks,
# #                               right = F)
# 
# italy.breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,120)
# merged.df$resp_age_italy = cut(merged.df$resp_age, 
#                                breaks = italy.breaks,
#                                right = F)
# merged.df$cont_age_italy = cut(merged.df$cont_age,
#                                breaks = italy.breaks,
#                                right = F)

# single.breaks = c(0:80,120)
# merged.df$resp_age_sy = cut(merged.df$resp_age, 
#                               breaks = single.breaks,
#                               right = F)
# merged.df$cont_age_sy = cut(merged.df$cont_age,
#                               breaks = single.breaks,
#                               right = F)
## Matrices

age_listr = c(paste0("resp_age",age.vars.names))
age_listc = c(paste0("cont_age",age.vars.names))

# calc.mycm = function(mydata = merged.df, 
#                      weight.exist = T,
#                      myattr, ## respondent first, contact second
#                      check.str = key.vars){
#   ## check that the attributes are the same
#   print(paste(myattr, colnames(mydata)[myattr]))
#   
#   ## find an overlap between the two strings
#   mysubsstr = strsplit(colnames(mydata)[myattr][1], "[^a-zA-Z0-9]+")[[1]]
#   mystr.check = unlist(lapply(mysubsstr,function(x)
#     grepl(x,colnames(mydata)[myattr][2])))
#   
#   if(sum(mystr.check) > 0 & 
#      sum(gsub("[^a-zA-Z]","",mysubsstr[mystr.check]) %in% 
#          gsub("[^a-zA-Z]","",check.str)) > 0){
#     
#     ### formatting the factor levels & dropping missing data
#     for (attr in myattr) {
#       #print(!is.factor(mydata[[attr]]))
#       miss.info = c("Data loss", "Refused", "Unknown")
#       ## Identify non-missing levels 
#       #attr = myattr[1]
#       new.lev = setdiff(levels(mydata[[attr]]), miss.info)
#       ## Identify missing information (potential NAs)
#       ref.idx = which(as.character(mydata[[attr]]) %in% miss.info)
#       ## Print the number of missing data observations
#       if(length(ref.idx) > 0){
#         message(paste0("Number of missing obs in", colnames(mydata)[attr],":", 
#                        length(ref.idx)))
#       }
#       ## (re-)factor the variables
#       if (!is.factor(mydata[[attr]])){
#         mydata[[attr]] = factor(mydata[[attr]], levels = new.lev)
#         #mydata[[myattr[2]]] = factor(mydata[[myattr[2]]], levels = new.lev)
#       }else{
#         if(length(ref.idx) > 0){
#           mydata = mydata[-ref.idx,]
#         }
#         tmp = as.character(mydata[[attr]])
#         mydata[[attr]] = factor(tmp,levels = new.lev)
#       }
#     }
#     
#     ## do the calculations
#     resp.l = union(levels(mydata[,myattr[1]]),levels(mydata[,myattr[2]]))
#     cont.l = resp.l
#     
#     combinations_grid = expand.grid(respondent = resp.l,
#                                     contact = cont.l)
#     
#     combinations_grid$ncont = sapply(c(1:nrow(combinations_grid)), function(i)
#       length(which(mydata[,myattr[1]] == combinations_grid$respondent[i] &
#                      mydata[,myattr[2]] == combinations_grid$contact[i]))
#     )
#     combinations_grid$nresp = sapply(c(1:nrow(combinations_grid)), function(i)
#       length(unique(mydata$caseid[which(mydata[,myattr[1]] == combinations_grid$respondent[i])]))
#     )
#     
#     combinations_grid$av_nc = combinations_grid$ncont/combinations_grid$nresp
#     
#     combinations_grid$median = median(combinations_grid$ncont, na.rm = T)
#     
#     # Create the wide matrix for ncont/nresp
#     m <- combinations_grid %>%
#       dplyr::select(respondent, contact, av_nc, median) %>%
#       arrange(factor(respondent, levels = resp.l)) %>%
#       pivot_wider(names_from = contact, values_from = av_nc)%>%
#       dplyr::select(respondent, all_of(cont.l))
#     
#     if(weight.exist){
#       
#       combinations_grid$wncont = sapply(c(1:nrow(combinations_grid)), function(i)
#         sum(mydata$weight[which(mydata[,myattr[1]] == combinations_grid$respondent[i] &
#                                   mydata[,myattr[2]] == combinations_grid$contact[i])])
#       )
#       
#       combinations_grid$wsum = sapply(c(1:nrow(combinations_grid)), function(i)
#         sum(mydata$weight[ 
#           match(unique(mydata$caseid[which(mydata[,myattr[1]] == combinations_grid$respondent[i])]),
#                 mydata$caseid)])
#       )
#       
#       #combinations_grid$wav_nc = combinations_grid$wncont/combinations_grid$wsum
#       
#       combinations_grid$wav_nc = sapply(c(1:nrow(combinations_grid)), function(i) {
#         var.name = ifelse(is.numeric(myattr[2]),colnames(mydata)[myattr[2]],
#                           myattr[2])
#         
#         data = mydata[which(mydata[,myattr[1]] == combinations_grid$respondent[i]),] 
#         data$nocont = ifelse(data[,myattr[2]] == combinations_grid$contact[i] &
#                                !is.na(data[,myattr[2]]), 1, 0)
#         
#         data2 = as.data.frame(data %>%
#                                 group_by(resp_id) %>%
#                                 summarise(n = sum(nocont, na.rm = T),
#                                           weight = mean(weight, na.rm = T), .groups = 'drop'))
#         if(nrow(data2) >= 5){
#           # Define survey design
#           survey_design <- svydesign(ids = ~1, data = data2, weights = ~weight)
#           # Calculate the weighted mean
#           wn = svymean(data2$n, survey_design, na.rm = TRUE)
#         }else{
#           wn = NA
#         }
#         return(wn)
#       })
#       
#       # Create the wide matrix for ncont/wsum
#       m_w <- combinations_grid %>%
#         dplyr::select(respondent, contact, wav_nc) %>%
#         arrange(factor(respondent, levels = resp.l)) %>%
#         pivot_wider(names_from = contact, values_from = wav_nc) %>%
#         dplyr::select(respondent, all_of(cont.l))
#       
#     }else{
#       m_w = NULL
#     }
#     m = list(df = combinations_grid, unweighted = m, weighted = m_w)
#   }else{
#     message("Wrong combination of attributes", color = "blue")
#   }
#   
#   return(m)
#   
# }

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
names(matrix_list) <- c("uk.bel.neth", "swiss.prem", "Suzhou,China", 
                        "AnhuaCounty,HunanProvince,China", "Italy", "US_Mistry")

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
            paste0(output.dir, "csv/compare/", names(matrix_list)[m], "-matrix.csv"),
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
          paste0(output.dir, "csv/compare/mistry-data/","mistry-by5-matrix.csv"),
          row.names = F)
