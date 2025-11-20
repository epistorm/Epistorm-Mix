#### Folders and Directories ####

output.dir = paste("data", "imputed_data", sep = "/")

#### Read Clean Data ####

if(csv_only){
  sum.res3 = read.csv(paste(output.dir, "imputed-resp2.csv", sep = "/"))
}else{
  load(paste(output.dir, "all-waves.Rdata", sep = "/"))
}

#### Adjust Weights ####

sum.res3$weight_ipsos = sum.res3$weight
tmppp = sum.res3[,c("weight", "wave")] %>%
  group_by(wave) %>%
  mutate(weight = weight * n() / sum(weight)) %>%
  ungroup()
tmppp$weight[which(tmppp$wave == "Wave 2")] = ceiling(nrow(sum.res3)/2)*
  tmppp$weight[which(tmppp$wave == "Wave 2")]/
  sum(tmppp$weight[which(tmppp$wave == "Wave 2")])
tmppp$weight[which(tmppp$wave == "Wave 1")] = (nrow(sum.res3) -
                                                sum(tmppp$weight[which(tmppp$wave == "Wave 2")]))*
  tmppp$weight[which(tmppp$wave == "Wave 1")]/
  sum(tmppp$weight[which(tmppp$wave == "Wave 1")])

sum.res3$weight = tmppp$weight

#### Save Updated Data ####

if(csv_only){
  write.csv(sum.res3, paste(output.dir, "imputed-resp2.csv", sep = "/"))
}else{
  write.csv(sum.res3, paste(output.dir, "imputed-resp2.csv", sep = "/"))
  save(merged.df, # cleaned, merged, imputed contact-level data (obs. unit = contact)
       sum.res2, # respondent-level data before imputations
       sum.res3, # respondent-level data after imputations/calculated number of contacts
       file = paste(output.dir, "all-waves.Rdata", sep = "/"))
}

