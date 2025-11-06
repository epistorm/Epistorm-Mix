matrix.dir = "./matrices/"
par(mfrow = c(1,1))

## Alterable parameters in code
PDF = T # if T, then visualizations are saved to a .pdf file
CSV = F # if T, then matrices are saved to a .csv file in the same format

## Read in Pre-pandemic matrix data for total contacts
### First author surnames, countries, and excel sheets (if applicable) 
mat.files = data.frame(author = c("Mistry", "Prem"),
                       sheet = c(NA, "United States of America"),
                       labels = c("US_Mistry", "US_Prem"))
mat.files$file.type = ifelse(is.na(mat.files$sheet), ".csv", ".xlsx")

## Read files into program
contact.mat = lapply(1:nrow(mat.files), function(x){
  ## read file 
  if(mat.files$file.type[x] == ".xlsx"){
    tmp = read_xlsx(paste0("./data/external_data/matrices-", mat.files$author[x], 
                           mat.files$file.type[x]), 
                    sheet = mat.files$sheet[x],
                    col_names = F)
    
  } else if(mat.files$file.type[x] == ".csv"){
    tmp = read.csv(paste0("./data/external_data/matrices-", mat.files$author[x], 
                          mat.files$file.type[x]))
  } else{
    print("ERROR: FILE TYPE IS NEITHER A CSV OR XLSX.")
  }
  
  ## return read file as a dataframe
  as.data.frame(tmp)
})

names(contact.mat) = mat.files$labels

#### Label columns and rows for age groups
### Mistry
colnames(contact.mat$US_Mistry) = c(paste0(seq(0, 75, by = 5), "-",
                                           c(seq(4, 79, by = 5))), "80+")

contact.mat$US_Mistry$resp_age = colnames(contact.mat$US_Mistry)

### Prem
colnames(contact.mat$US_Prem) = c(paste0(seq(0, 70, by = 5), "-",
                                         c(seq(4, 74, by = 5))), "75+")

contact.mat$US_Prem$resp_age = colnames(contact.mat$US_Prem)

#### Fix Age Labels ####

for (k in 1:length(contact.mat)){
  wrong.frmt = grepl(",", contact.mat[[k]]$resp_age)[1]
  if (wrong.frmt){
    breaks = contact.mat[[k]]$resp_age
    maxv = breaks[length(breaks)]
    for (i in breaks){
      idx.label = which(contact.mat[[k]]$resp_age == i)
      if(i == maxv){
        contact.mat[[k]]$resp_age[idx.label] = paste0(str_extract_all(i, "\\d+\\.?\\d*")[[1]][1],"+")
      }else{
        contact.mat[[k]]$resp_age[idx.label] = paste(str_extract_all(i, "\\d+\\.?\\d*")[[1]][1], 
                                                     as.numeric(str_extract_all(i, "\\d+\\.?\\d*")[[1]][2]) - 1, 
                                                     sep = "-")
      } 
    }
    colnames(contact.mat[[k]]) = c("resp_age", contact.mat[[k]]$resp_age)
  }
}

#### Read Epistorm contact matrices by different age groupings
filenames = c("Total-M-by5_75-matrix", "Total-M-by5_80-matrix")

epistorm.mat = lapply(filenames, function(x){
  tmp = read.csv(paste0(matrix.dir, "M_matrix/", x, ".csv"))
  
  tmp = tmp %>% 
    pivot_longer(cols = colnames(tmp)[which(colnames(tmp) != "X")],
                 names_to = "cont_age",
                 values_to = "ncont")
  tmp$cont_age = gsub("\\.", "-", gsub("X", "", tmp$cont_age))
  tmp$cont_age[which(tmp$cont_age == "75-")] = "75+"
  tmp$cont_age[which(tmp$cont_age == "80-")] = "80+"
  colnames(tmp)[which(colnames(tmp) == "X")] = "resp_age"
  tmp
})

names(epistorm.mat) = filenames

#### Convert dataframes to long from wide
### Prepandemic data
contact.mat.long = lapply(contact.mat, function(x){
  tmp = melt(setDT(x), id.vars = c("resp_age"), variable.name = "cont_age")
  
  tmp$resp_age = as.factor(tmp$resp_age)
  
  data.frame(resp_age = tmp$resp_age, cont_age = tmp$cont_age, contacts = tmp$value,
             xlab = paste(tmp$resp_age, tmp$cont_age, sep = "--"))
})

### Epistorm-Mix data
epistorm.mat.long = lapply(epistorm.mat, function(x){
  tmp = x
  tmp$resp_age = factor(tmp$resp_age,
                        levels = unique(tmp$resp_age))
  data.frame(resp_age = tmp$resp_age, cont_age = tmp$cont_age, epi.contacts = tmp$ncont,
             xlab = paste(tmp$resp_age, tmp$cont_age, sep = "--"))
})

#### Combine Epistorm-Mix age groupings estimates with other papers
## and compare

## different age groups "[0,5)" and "0-4"
matrix.plot.vals = lapply(1:length(contact.mat.long), function(x){
  
  for(i in 1:length(epistorm.mat.long)){
    if(length(levels(contact.mat.long[[x]]$resp_age)) == 
       length(levels(epistorm.mat.long[[i]]$resp_age)) &
       levels(contact.mat.long[[x]]$resp_age)[length(levels(contact.mat.long[[x]]$resp_age))] == 
       levels(epistorm.mat.long[[i]]$resp_age)[length(levels(epistorm.mat.long[[i]]$resp_age))]){
      
      tmp = full_join(contact.mat.long[[x]], epistorm.mat.long[[i]], 
                      by = c("resp_age", "cont_age", "xlab"))
      
      contact.mat.long[[x]]$resp_age = factor(contact.mat.long[[x]]$resp_age,
                                              levels = levels(epistorm.mat.long[[i]]$resp_age))
      
      contact.mat.long[[x]]$cont_age = factor(contact.mat.long[[x]]$cont_age,
                                              levels = levels(epistorm.mat.long[[i]]$resp_age))
      
      
      tmp$resp_age = factor(tmp$resp_age, levels = levels(epistorm.mat.long[[i]]$resp_age))
      
      tmp$cont_age = factor(tmp$cont_age, levels = levels(epistorm.mat.long[[i]]$resp_age))
      
      break
    }
  }
  
  my.corr.comp(x = tmp$epi.contacts,
               y = tmp$contacts,
               x_label = "Epistorm contacts",
               y_label = paste("Contacts in", names(contact.mat.long)[x]))
  
  as.data.frame(tmp)
  tmp$country = names(contact.mat.long)[x]
  
  tmp
  
})

names(matrix.plot.vals) = names(contact.mat.long)

#### Convert datasets from long to wide
cont.mat.wide = lapply(1:length(matrix.plot.vals), function(x){
  # for (x in 1:length(matrix.plot.vals)){
  df = matrix.plot.vals[[x]]
  tmp = as.data.frame(df[, -which(colnames(df) == "xlab" | colnames(df) == "epi.contacts" | 
                                    colnames(df) == "country")] %>%
                        spread(key = resp_age, value = contacts))
  
  tmp[, "cont_age"] = factor(tmp[, "cont_age"], levels = tmp[, "cont_age"])
  
  tmp = tmp[order(tmp[, "cont_age"], levels(tmp[, "cont_age"])), ]
  
  tmp
  # }
})

names(cont.mat.wide) = names(contact.mat.long)

#### Estimate the difference between Epistorm-Mix and all other contact matrices
matrix.diff = lapply(matrix.plot.vals, function(x){
  x$cont.diff = x$contacts - x$epi.contacts
  
  as.data.frame(x[, c("resp_age", "cont_age", "cont.diff")])
})

names(matrix.diff) = names(cont.mat.wide)

## Convert to wide data
diff.mat.wide = lapply(1:length(matrix.diff), function(x){
  tmp = matrix.diff[[x]] %>%
    spread(key = cont_age, value = cont.diff)
  tmp = tmp[order(tmp[, "resp_age"], levels(tmp[, "resp_age"])), ]
  tmp
})

#### Read setting-specific contact matrices
### Mistry
cnt_setting = c("household", "work", "school")
age.breaks = c(paste0(seq(0, 80, by = 5), "-",
                      c(seq(4, 84, by = 5))), "85+")
groups.age = c("75-79", "80-84", "85+")

## Read age distribution datasets
mistry.age.dist = read.csv("./data/external_data/mistry-data/United_States_country_level_age_distribution_18.csv",
                           header = F)
colnames(mistry.age.dist) = c("group", "n")
mistry.age.dist$age.group = age.breaks
mistry.age.dist$ages75 = ifelse(mistry.age.dist$age.group %in% groups.age,
                                "75+", mistry.age.dist$age.group)

mistry.age.dist$ages75 = factor(mistry.age.dist$ages75, 
                                levels = unique(mistry.age.dist$ages75))

mistry.age.dist$weightedsum = mistry.age.dist$n / sum(mistry.age.dist$n)

total = sum(mistry.age.dist$n)

age.dist = mistry.age.dist %>%
  group_by(ages75) %>%
  summarise(weightedsum = sum(n) / total)

## Read F matrices from Mistry and aggregate 75+ groups
mistry_setting = lapply(cnt_setting, function(x){
  tmp = read.csv(paste0("./data/external_data/mistry-data/United_States_country_level_F_", x,
                        "_setting_18.csv"), header = F)
  
  colnames(tmp) = age.breaks
  
  tmp$resp_age = age.breaks
  
  
  over75.row = rowSums(tmp[, which(colnames(tmp) %in% groups.age)] * 
                         mistry.age.dist$weightedsum) / sum(mistry.age.dist$n)
  
  tmp[, ncol(tmp) + 1] = unname(over75.row)
  colnames(tmp)[ncol(tmp)] = "75+"
  
  tmp = tmp[, -which(colnames(tmp) %in% groups.age)]
  
  
  
  over75.col = colSums(tmp[which(tmp$resp_age %in% groups.age), 
                           -which(colnames(tmp) == "resp_age")])
  
  tmp[nrow(tmp) + 1, -which(colnames(tmp) == "resp_age")] = unname(over75.col)
  tmp$resp_age[nrow(tmp)] = "75+"
  
  tmp = tmp[-which(tmp$resp_age %in% groups.age), ]
  
  
  tmp$resp_age = factor(tmp$resp_age, levels = tmp$resp_age)
  
  
  tmp.long = melt(setDT(tmp), id.vars = c("resp_age"), 
                  variable.name = "cont_age")
  
  tmp.long$cont_age = as.factor(tmp.long$cont_age)
  
  colnames(tmp.long)[which(colnames(tmp.long) == "value")] = "mistry"
  
  as.data.frame(tmp.long)
})
names(mistry_setting) = cnt_setting

lapply(1:length(mistry_setting), function(x){
  tmp = mistry_setting[[x]] %>%
    spread(key = cont_age, value = mistry)
  
  rowSums(tmp[, -which(colnames(tmp) == "resp_age")])
  
  if(CSV == T){
    write.csv(tmp,
              paste0(output.dir, "csv/",
                     names(mistry_setting)[x], ".csv"),
              row.names = F)
  }
})


### Epistorm-Mix F-matrices
f_matrix.folder = paste0(matrix.dir, "F_matrix/")

cnt_sett_Epi = cnt_setting

epi.setting = lapply(capitalize(cnt_sett_Epi), function(x){
  tmp = read.csv(paste0(f_matrix.folder, x, "-F-by5_75-matrix.csv"))
  
  tmp = tmp %>% 
    pivot_longer(cols = colnames(tmp)[which(colnames(tmp) != "X")],
                 names_to = "cont_age",
                 values_to = "ncont")
  tmp$cont_age = gsub("\\.", "-", gsub("X", "", tmp$cont_age))
  tmp$cont_age[which(tmp$cont_age == "75-")] = "75+"
  colnames(tmp)[which(colnames(tmp) == "X")] = "resp_age"
  colnames(tmp)[which(colnames(tmp) == "ncont")] = "epistorm"
  tmp$epistorm[which(is.na(tmp$epistorm))] = 0
  as.data.frame(tmp)
})

names(epi.setting) = cnt_sett_Epi

## merge Epistorm-Mix and Mistry datasets by setting
mat.comp = lapply(1:length(epi.setting), function(x){
  tmp = left_join(mistry_setting[[x]], 
                  epi.setting[[x]],
                  relationship = "many-to-many")
  
  tmp$epistorm[which(is.na(tmp$epistorm))] = 0
  
  tmp = as.data.frame(tmp)
  
  tmp$setting = cnt_sett_Epi[x]
  
  tmp
})

#### Log10 Correlations ####

if(PDF == T){
  pdf(paste0(output.dir, "fig3-correlations.pdf"))
}

full.df = do.call("rbind", matrix.plot.vals)
full.df$log10_cnt = log10(1 + full.df$contacts)
full.df$log10_epistorm_cnt = log10(1 + full.df$epi.contacts)

## Estimate pearson correlation coefficient
full.df$cor.vals = NA
for(i in 1:length(names(matrix.plot.vals))){
  idx = which(full.df$country == names(matrix.plot.vals)[i])
  
  pears.cor = corr.test(full.df$contacts[idx], 
                        full.df$epi.contacts[idx], 
                        method = "pearson")
  
  if(pears.cor$p < 0.001){
    pears.pvalue = "***"
  } else if(pears.cor$p < 0.01){
    pears.pvalue = "**"
  } else if(pears.cor$p < 0.05){
    pears.pvalue = "*"
  }
  
  full.df$cor.vals[idx] = paste0(names(matrix.plot.vals)[i], 
                                 " - Pearson's r = ",
                                 round(pears.cor$r, 2), pears.pvalue)
}


plot.groups = list(prepan = c("US_Mistry", "US_Prem"))

mycol = list(cou = c( "plum",  "#00B6EB", "#F8766D", "#A58AFF", "#53B400", 
                      "#00C094", "#C49A00"),
             US = c("#60C4B2", "#4a6894"))

## Plot US pre-pandemic contacts with Epistorm-Mix contacts
lapply(1:length(plot.groups), function(x){
  
  ## Identify indexes to plot
  idx = which(full.df$country %in% plot.groups[[x]])
  
  plot.df = full.df[idx, ]
  
  
  ## Axes values and labels
  cnt.x = seq(0, round(max(plot.df$contacts)), by = 2)
  log10_cnt.x = log10(1 + cnt.x)
  
  cnt.y = seq(0, round(max(plot.df$epi.contacts)), by = 1)
  log10_cnt.y = log10(1 + cnt.y)
  
  
  ## Full plot
  par(mar = c(5, 4, 4, 8), mfrow = c(1,1), xpd = TRUE)
  plot(plot.df$log10_cnt,  plot.df$log10_epistorm_cnt, 
       xlab = "Number of contacts for age-to-age pairs", 
       ylab = "Number of contacts for age-to-age pairs in Epistorm-Mix",
       axes = F, type = "n")
  box()
  axis(2, las = 2, at = log10_cnt.y, labels = cnt.y)
  axis(1, las = 1, at = log10_cnt.x, labels = cnt.x)
  abline(lm(plot.df$log10_epistorm_cnt~plot.df$log10_cnt), col = "black", xpd = F)
  
  for(i in 1:length(plot.groups[[x]])){
    points(plot.df$log10_cnt[which(plot.df$country == plot.groups[[x]][i])], 
           plot.df$log10_epistorm_cnt[which(plot.df$country == plot.groups[[x]][i])],
           col = add.alpha(mycol[[x]][i], alpha = 0.75),
           pch = 19, cex = 1.5)
  }
  
  legend("right", inset = c(-1.65, 0.1),                   
         legend = c(levels(factor(plot.df$cor.vals))),
         bty = "n",
         pch = 19, col = mycol[[x]])
  
  ## Estimate overall correlation and add legend
  tmp1 = corr.test(plot.df$contacts, 
                   plot.df$epi.contacts, 
                   method = "pearson")
  
  pears.pvalue = ifelse(tmp1$p < 0.001, "***",
                        ifelse(tmp1$p < 0.01, "**",
                               ifelse(tmp1$p < 0.05, "*", "")))

  legend("topleft",                   
         legend = c(unique(full.df$cor.vals)[grep("Mistry", unique(full.df$cor.vals))], 
                    unique(full.df$cor.vals)[grep("Prem", unique(full.df$cor.vals))],
                    paste0("Pearson's r = ", round(tmp1$r, 2), pears.pvalue)),
         bty = "n",
         pch = 19, col = c(add.alpha(mycol$US, alpha = 0.75), NA))
  
})


## Pre vs. post-pandemic by setting
min.cutoff = sapply(1:length(mat.comp), function(i) quantile(c(setdiff(mat.comp[[i]]$mistry,0),
                                                setdiff(mat.comp[[i]]$epistorm,0)),
                                              p = 0.05))
par(mfrow = c(1,1), xpd = T)
for(i in 1:length(mat.comp)){
  mat.comp[[i]]$log10_cnt = log10(min.cutoff[i] + mat.comp[[i]]$mistry)
  mat.comp[[i]]$log10_epistorm_cnt = log10(min.cutoff[i] + mat.comp[[i]]$epistorm)
  
  
  pears.cor = corr.test(mat.comp[[i]]$mistry, 
                        mat.comp[[i]]$epistorm, 
                        method = "pearson")
  
  pears.pvalue = ifelse(pears.cor$p < 0.001, "***",
                        ifelse(pears.cor$p < 0.01, "**",
                               ifelse(pears.cor$p < 0.05, "*", "")))
  
  # dist.val = ifelse(unique(mat.comp[[i]]$setting) == "work",
  #                   0.01, 0.05)
  
  
  
  cnt = c(0, 0.01, round(seq(0, max(mat.comp[[i]]$mistry), length.out = 4),2))
  cnt = cnt[cnt < max(mat.comp[[i]]$mistry)]
  log10_cnt = log10(cnt + min.cutoff[i])
  
  cnt.y = c(0, 0.01, round(seq(0, max(mat.comp[[i]]$epistorm), length.out = 4),2))
  cnt.y = cnt.y[cnt.y < max(mat.comp[[i]]$epistorm)]
  log10_cnt.y = log10(cnt.y + min.cutoff[i])
  
  plot(c(log10(min.cutoff[i]),max(mat.comp[[i]]$log10_cnt)), 
       c(log10(min.cutoff[i]), max(mat.comp[[i]]$log10_epistorm_cnt)),
       type = "n",
       xlab = "Per capita probability of contact age-to-age pairs", 
       ylab = "Per capita probability of contact age-to-age pairs in Epistorm", 
       main = str_to_title(unique(mat.comp[[i]]$setting)), 
       axes = F)
  points(mat.comp[[i]]$log10_cnt,  mat.comp[[i]]$log10_epistorm_cnt,
         col= add.alpha(mycol$US[1], alpha = 0.75),
         pch = 19, cex = 1.5)
  box()
  axis(2, las = 2, at = log10_cnt.y, labels = cnt.y
  )
  axis(1, las = 1, at = log10_cnt, labels = cnt
  )
  
  abline(lm(mat.comp[[i]]$log10_epistorm_cnt~mat.comp[[i]]$log10_cnt), 
         col = "black", xpd = F)
  
  legend("topleft", legend = paste0("Pearson's r = ",
                                    round(pears.cor$r, 2), pears.pvalue),
         bty = "n",
         inset = c(0.05, 0))
}

if(PDF == T){
  dev.off()
}
