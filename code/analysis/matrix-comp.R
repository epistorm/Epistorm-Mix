rm(list = ls())

par(mfrow = c(1,1))

#### Install and load libraries
lapply(c("readxl", "data.table", "dplyr", "tidyr", "psych", "Hmisc", 
         "prettyGraphs"), 
       function(x){
           if(!require(x, character.only = T)){
             install.packages(x)
           }
           library(x, character.only = T)
})


## Alterable parameters in code
PDF = F # if T, then visualizations are saved to a .pdf file
CSV = F # if T, then matrices are saved to a .csv file in the same format


## Correlation comparison plot function
my.corr.comp = function(x, y, x_label, y_label){
  
  plot(x, y, xlab = x_label, ylab = y_label, axes = F, pch = 19)
  axis(2, las = 2)
  axis(1, las = 1)
  box()
  
  ## Fit linear regression line to plot
  fit = lm(y ~ x)
  abline(fit, col = "red", lwd = 2.5)
  
  
  ## Get p-values from regression
  fit.pvalue = summary(fit)$coefficients[2, 4]
  if(fit.pvalue < 0.001){
    fit.pvalue = "< 0.001"
  } else{
    fit.pvalue = round(fit.pvalue, digits = 3)
  }
  
  ## Check pearson correlation coefficient and p-values
  pears.cor = corr.test(x, y, method = "pearson")
  if(pears.cor$p < 0.001){
    pears.pvalue = "< 0.001"
  } else{
    pears.pvalue = round(pears.cor$p, digits = 3)
  }
  
  ## Check spearman correlation coefficient and p-values
  spear.cor = corr.test(x, y, method = "spearman")
  if(spear.cor$p < 0.001){
    spear.pvalue = "< 0.001"
  } else{
    spear.pvalue = round(spear.cor$p, digits = 3)
  }
  
  ## Add legend with p-values and coefficients
  
  legend("bottomright", 
         legend = c(paste0("Pearson's r = ", round(pears.cor$r, digits = 3),
                           ", p-value = ", pears.pvalue),
                    paste0("Spearman's rs = ", round(spear.cor$r, digits = 3),
                           ", p-value = ", spear.pvalue),
                    paste0("R-squared = ", round(summary(fit)$r.squared, digits = 3), 
                           ", p-value = ", fit.pvalue)),
         bty = "n") 
}




## Read in Pre-pandemic matrix data for total contacts
### First author surnames, countries, and excel sheets (if applicable) 
mat.files = data.frame(author = c(rep("Jarvis", times = 4), "Wang", "Mistry", 
                                  "Prem", "Liang"),
                       sheet = c("UK", "BE", "NL", "CH", 
                                 "non-symmeterized_total", NA, 
                                 "United States of America", NA),
                       labels = c("United-Kingdom", "Belgium", "The-Netherlands", 
                                  "Switzerland", "Suzhou,China", "US_Mistry", 
                                  "US_Prem", "HunanProvince,China"))

mat.files$file.type = ifelse(is.na(mat.files$sheet), ".csv", ".xlsx")

## Read files into program
contact.mat = lapply(1:nrow(mat.files), function(x){
  ## read file 
  if(mat.files$file.type[x] == ".xlsx"){
    if(mat.files$author[x] == "Prem"){
      head.T = F
    } else{
      head.T = T
    }
    
    tmp = read_xlsx(paste0("./external_data/matrices/matrices-", mat.files$author[x], 
                           mat.files$file.type[x]), 
                    sheet = mat.files$sheet[x],
                    col_names = head.T)
    
  } else if(mat.files$file.type[x] == ".csv"){
    tmp = read.csv(paste0("./external_data/matrices/matrices-", mat.files$author[x], 
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
colnames(contact.mat$US_Mistry) = paste0("[", seq(0, 80, by = 5), ",",
                                         c(seq(5, 80, by = 5), 120), ")")

contact.mat$US_Mistry$resp_age = colnames(contact.mat$US_Mistry)

### Prem
colnames(contact.mat$US_Prem) = paste0("[", seq(0, 75, by = 5), ",",
                                       c(seq(5, 75, by = 5), 120), ")")

contact.mat$US_Prem$resp_age = colnames(contact.mat$US_Prem)

### Liang
colnames(contact.mat$`HunanProvince,China`)[which(colnames(contact.mat$`HunanProvince,China`) == "X")] = 
  "resp_age"

colnames(contact.mat$`HunanProvince,China`)[-1] = 
  c("[0,3)", "[3,7)", "[7,10)", "[10,15)",  "[15,20)",  "[20,25)",  "[25,30)", 
    "[30,35)", "[35,40)",  "[40,45)", "[45,50)", "[50,55)", "[55,60)", "[60,65)",  
    "[65,70)", "[70,75)", "[75,120)")

contact.mat$`HunanProvince,China`$resp_age = colnames(contact.mat$`HunanProvince,China`)[-1]


#### Process Italy matrices - unsymmeterize matrices
### age groups for matrix
italy.ages = paste0("[", seq(0, 70, by = 5), ",",
                    c(seq(5, 70, by = 5), 120), ")")

age.breaks = c(seq(0, 70, by = 5), 120)

### Read population size and estimate distribution - group 100+
italy.pop = read.csv("populations-dists/Resident population-Italy.csv")

tot.pop.Italy = italy.pop$Total[which(italy.pop$Age == "Total")]

italy.pop = italy.pop[which(italy.pop$Age != "Total"), ]

italy.pop$Age = as.numeric(ifelse(italy.pop$Age == "100 and over", 100, 
                                  italy.pop$Age))

### Read sample population distribution and normalize
italy.samp = read.csv("Italy-sample-size.csv")
colnames(italy.samp) = c("age", "dist.sample")

italy.samp$age = round(italy.samp$age)
italy.samp$dist.sample.norm = italy.samp$dist.sample / sum(italy.samp$dist.sample)

italy.samp$age.group = italy.ages

### Assign single-year age to respective age group
italy.pop$age.group = cut(italy.pop$Age, breaks = age.breaks,
                          right = F)

### Aggregate population by age group, factor, and assign levels in order
italy.pop.str = italy.pop %>%
  group_by(age.group) %>%
  summarise(n = sum(Total))

### Estimate the distribution of the population
italy.pop.str$dist = round(italy.pop.str$n / sum(italy.pop.str$n), digits = 3)


### Combine actual sample distribution by age group with distribution of the population
italy.pop.str = full_join(italy.pop.str, 
                          italy.samp[, c("age.group", "dist.sample", 
                                         "dist.sample.norm")],
                          by = "age.group")

italy.pop.str$age.group = factor(italy.pop.str$age.group,
                                 levels = italy.ages)

italy.pop.str = italy.pop.str[order(italy.pop.str$age.group), ]


### Estimate number of people by age group in sample according to the reported
### sample distribution and the distribution of the population
n_participants = 4979

italy.pop.str$sample = italy.pop.str$dist.sample * n_participants

italy.pop.str$sample.rev = italy.pop.str$dist * n_participants

### Visualize the population distributions
barplot(italy.pop.str$n ~ italy.pop.str$age.group,
        ylab = "Total population")

barplot(italy.pop.str$sample ~ italy.pop.str$age.group,
        ylab = "Participant population")

barplot(italy.pop.str$sample.rev ~ italy.pop.str$age.group,
        ylab = "Participant population - representative")


# Read Italy contact matrices and  unsymmeterize them
# unzip folder Lucchini_bootstrap_age_group_total
italy = lapply(0:999, function(x){
  tmp0 = read.csv(paste0("matrices/Lucchini_bootstrap_age_group_total/contact_matrix_age_group_bootstrap_", 
                         x, ".csv"))
  
  tmp = as.data.frame(tmp0)
  dim(tmp)
  
  tmp$contacts = italy.ages
  colnames(tmp)[-which(colnames(tmp) == "contacts")] = italy.ages

  tmp1 = tmp
  
  for(i in 2:ncol(tmp)){
    # Multiply number of contacts for age group j by number of participants in age group i
    tmp1[, i] = tmp[, i] * italy.pop.str$sample.rev
  }

  for(i in 1:nrow(tmp)){
    # Divide number of contacts for participants in age group i by number of participants in age group i
    tmp1[i, -1] = tmp1[i, -1] / italy.pop.str$sample.rev
  }

  tmp1
  
  # tmp0
})

### convert to long dataset
italy.df = lapply(italy, function(x){
  tmp = melt(setDT(x), id.vars = c("contacts"), variable.name = "resp_age")
})

tmp = do.call(rbind, italy.df)

colnames(tmp)[which(colnames(tmp) == "contacts")] = "cont_age"

### Estimate mean across all matrices
italy.mean = tmp %>%
  group_by(cont_age, resp_age) %>%
  summarise(contacts = mean(value))


italy.mean$xlab = paste(italy.mean$resp_age, italy.mean$cont_age, sep = "--")
italy.mean$resp_age = as.factor(italy.mean$resp_age)
italy.mean$cont_age = as.factor(italy.mean$cont_age)



#### Read Epistorm contact matrices by different age groupings
filenames = c("by5_70", "by10_70", "by5_75", "school-groups", 
              "by5_80-matrix", "by3_10-by5_75", 
              "inperson75", "remote75", "inperson80", "remote80")

epistorm.mat = lapply(filenames, function(x){
  tmp = read.csv(paste0("matrices/Epistorm-matrices/", x, ".csv"))
  
  colnames(tmp)[grep("resp", colnames(tmp))] = "resp_age"
  
  colnames(tmp)[-which(colnames(tmp) == "resp_age")] = tmp$resp_age
  
  lvl = tmp$resp_age
  
  tmp$resp_age = factor(tmp$resp_age, levels = lvl)
  
  print(levels(tmp$resp_age))
  
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

contact.mat.long$Italy = italy.mean[, c("resp_age", "cont_age", "contacts", "xlab")]

### Epistorm-Mix data
epistorm.mat.long = lapply(epistorm.mat, function(x){
  tmp = melt(setDT(x), id.vars = c("resp_age"), variable.name = "cont_age")
  
  tmp$resp_age = as.factor(tmp$resp_age)
  
  data.frame(resp_age = tmp$resp_age, cont_age = tmp$cont_age, epi.contacts = tmp$value,
             xlab = paste(tmp$resp_age, tmp$cont_age, sep = "--"))
})


#### Combine Epistorm-Mix age groupings estimates with other papers
## and compare
par(mar=c(5,4,4,2))


if(PDF == T){
  if(!dir.exists("visualizations")){
    dir.create("visualizations")
  }
  
  pdf("visualizations/comp-Epistorm-PrevLit-MaxAdjust.pdf")
}
matrix.plot.vals = lapply(1:length(contact.mat.long), function(x){
  
  for(i in 1:length(epistorm.mat.long)){
    if(length(levels(contact.mat.long[[x]]$resp_age)) == 
       length(levels(epistorm.mat.long[[i]]$resp_age)) &
       levels(contact.mat.long[[x]]$resp_age)[1] == 
       levels(epistorm.mat.long[[i]]$resp_age)[1]){
      
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
if(PDF == T){
  dev.off()
}


names(matrix.plot.vals) = names(contact.mat.long)


#### Comparison of in-person/remote work and school between Mistry and Prem with 
## the Epistorm-Mix data

### In-person school/work attendance
school.work = list()
school.work$mistry.inpers.comp = full_join(epistorm.mat.long$inperson80, 
                                           contact.mat.long$US_Mistry)
school.work$mistry.inpers.comp$setting = "mistry_inperson"

school.work$prem.inpers.comp = full_join(epistorm.mat.long$inperson75, 
                                         contact.mat.long$US_Prem)

school.work$prem.inpers.comp$setting = "prem_inperson"


### Not at school/work 
school.work$mistry.remote.comp = full_join(epistorm.mat.long$remote80, 
                                           contact.mat.long$US_Mistry)
school.work$mistry.remote.comp$setting = "mistry_remote"

school.work$prem.remote.comp = full_join(epistorm.mat.long$remote75, 
                                         contact.mat.long$US_Prem)

school.work$prem.remote.comp$setting = "prem_remote"


ylabels = c("Mistry", "Prem", "Mistry", "Prem")
xlabels = c("Epistorm contacts for people at work/school in person",
            "Epistorm contacts for people at work/school in person",
            "Epistorm contacts for people remote/not at work/school",
            "Epistorm contacts for people remote/not at work/school")



if(PDF == T){
  pdf("visualizations/comp-Epistorm-inperson-remote-MaxAdjust.pdf")
}

par(mfrow = c(1, 1))

lapply(1:length(school.work), function(x){
  
  my.corr.comp(x = school.work[[x]]$epi.contacts, 
               y = school.work[[x]]$contacts, 
               x_label = xlabels[x], 
               y_label = paste("Contacts in", ylabels[x]))
})

if(PDF == T){
  dev.off()
}



#### Convert datasets from long to wide
cont.mat.wide = lapply(matrix.plot.vals, function(x){
  tmp = as.data.frame(x[, -which(colnames(x) == "xlab" | colnames(x) == "epi.contacts")] %>%
    spread(key = resp_age, value = contacts))
  
  tmp[, "cont_age"] = factor(tmp[, "cont_age"], levels = tmp[, "cont_age"])
  
  tmp = tmp[order(tmp[, "cont_age"], levels(tmp[, "cont_age"])), ]
  
  tmp
})

names(cont.mat.wide) = names(contact.mat.long)


#### Estimate the difference between Epistorm-Mix and all other contact matrices
matrix.diff = lapply(matrix.plot.vals, function(x){
  x$cont.diff = x$contacts - x$epi.contacts
  
  as.data.frame(x[, c("resp_age", "cont_age", "cont.diff")])
})

names(matrix.diff) = names(cont.mat.wide)

## Calculate difference
school.work.diff = lapply(school.work, function(x){
  x$cont.diff = x$contacts - x$epi.contacts
  
  as.data.frame(x[, c("resp_age", "cont_age", "cont.diff")])
})

matrix.diff$Mistry_inperson = school.work.diff$mistry.inpers.comp
matrix.diff$Mistry_remote = school.work.diff$mistry.remote.comp

matrix.diff$Prem_inperson = school.work.diff$prem.inpers.comp
matrix.diff$Prem_remote = school.work.diff$prem.remote.comp


## Convert to wide datset and save
diff.mat.wide = lapply(1:length(matrix.diff), function(x){
  tmp = matrix.diff[[x]] %>%
    spread(key = cont_age, value = cont.diff)
  
  tmp = tmp[order(tmp[, "resp_age"], levels(tmp[, "resp_age"])), ]
  
  if(CSV == T){
    write.csv(tmp, paste0("matrices/difference-matrices/", 
                          names(matrix.diff)[x], ".csv"))
  }
  
  tmp
})


#### Read setting-specific contact matrices
### Mistry
cnt_setting = c("household", "work", "school", "community")
age.breaks =c("[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)", "[25,30)",
              "[30,35)", "[35,40)", "[40,45)", "[45,50)", "[50,55)", "[55,60)",
              "[60,65)", "[65,70)", "[70,75)", "[75,80)", "[80,85)", "85+")

groups.age = c("[75,80)", "[80,85)", "85+")

## Read age distribution datasets
mistry.age.dist = read.csv("matrices/United_States_country_level_age_distribution_18.csv",
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
  tmp = read.csv(paste0("matrices/United_States_country_level_F_", x,
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
  
  # if(CSV == T){
  #   write.csv(tmp,
  #             paste0("/Users/alkummer/Desktop/output matrices/mistry_",
  #                    names(mistry_setting)[x], ".csv"),
  #             row.names = F)
  # }
})






### Epistorm-Mix F-matrices
f_matrix.folder = "matrices/Epistorm-matrices/F-matrices/"

cnt_sett_Epi = cnt_setting
cnt_sett_Epi = gsub("community", "other", cnt_sett_Epi)

epi.setting = lapply(capitalize(cnt_sett_Epi), function(x){
  tmp = read.csv(paste0(f_matrix.folder, x, "-F-matrix.csv"))
  
  colnames(tmp) = levels(mistry_setting[[1]]$cont_age)
  tmp$resp_age = levels(mistry_setting[[1]]$resp_age)
  
  tmp.long = melt(setDT(tmp), id.vars = c("resp_age"), 
                  variable.name = "cont_age")
  
  colnames(tmp.long)[which(colnames(tmp.long) == "value")] = "epistorm"
  
  tmp.long$epistorm[which(is.na(tmp.long$epistorm))] = 0
  
  as.data.frame(tmp.long)
})

names(epi.setting) = cnt_sett_Epi


## merge Epistorm-Mix and Mistry datasets by setting
mat.comp = lapply(1:length(epi.setting), function(x){
  tmp = full_join(mistry_setting[[x]], 
                  epi.setting[[x]])
  
  tmp$epistorm[which(is.na(tmp$epistorm))] = 0
  
  tmp = as.data.frame(tmp)
  
  tmp$setting = cnt_sett_Epi[x]
  
  tmp
})




if(PDF == T){
  pdf("visualizations/Setting-Mistry.pdf")
}

lapply(1:length(mat.comp), function(x){
  
  my.corr.comp(x = mat.comp[[x]]$epistorm, 
               y = mat.comp[[x]]$mistry, 
               x_label = paste("Epistorm contacts in", cnt_sett_Epi[x]), 
               y_label =  paste("Mistry contacts in", cnt_sett_Epi[x]))
  
})
if(PDF == T){
  dev.off()
}



if(PDF == T){
  pdf("visualizations/log10-correlations.pdf")
}


#### LOG10 Visualization
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


plot.groups = list(cou = c("Belgium", "HunanProvince,China",  "Italy", 
                           "Suzhou,China", "Switzerland", "The-Netherlands", 
                           "United-Kingdom"), 
                   prepan = c("US_Mistry", "US_Prem"))

mycol = list(cou = c( "plum",  "#00B6EB", "#F8766D", "#A58AFF", "#53B400", 
                      "#00C094", "#C49A00"),
             US = c("#00cdcd", "#FB61D7"))




## Plot other countries and US pre-pandemic with Epistorm-Mix contacts
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
  par(mar = c(5, 4, 4, 8), mfrow = c(1,1), xpd = T)
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
  
  legend("bottomright", inset = c(-0.2, 0.1),                   
         legend = c(levels(factor(plot.df$cor.vals))),
         pch = 19, col = mycol[[x]])
  
  ## Estimate overall correlation and add legend
  tmp1 = corr.test(plot.df$contacts, 
                   plot.df$epi.contacts, 
                   method = "pearson")
  
  pears.pvalue = ifelse(tmp1$p < 0.001, "***",
                        ifelse(tmp1$p < 0.01, "**",
                               ifelse(tmp1$p < 0.05, "*", "")))
  
  legend("topleft", legend = paste0("Pearson's r = ", 
                                     round(tmp1$r, 2), 
                                     pears.pvalue))
  
  
  ## Single plots
  par(mar = c(5, 4, 4, 8), mfrow = c(1,1), xpd = T)
  
  for(i in 1:length(plot.groups[[x]])){
    plot(plot.df$log10_cnt,  plot.df$log10_epistorm_cnt, 
         xlab = "Number of contacts for age-to-age pairs", 
         ylab = "Number of contacts for age-to-age pairs in Epistorm-Mix",
         axes = F, type = "n")
    box()
    axis(2, las = 2, at = log10_cnt.y, labels = cnt.y)
    axis(1, las = 1, at = log10_cnt.x, labels = cnt.x)
    abline(lm(plot.df$log10_epistorm_cnt~plot.df$log10_cnt), col = "black", xpd = F)
    points(plot.df$log10_cnt[which(plot.df$country == plot.groups[[x]][i])], 
           plot.df$log10_epistorm_cnt[which(plot.df$country == plot.groups[[x]][i])],
           col = add.alpha(mycol[[x]][i], alpha = 0.75),
           pch = 19, cex = 1.5)
    legend("bottomright", legend = factor(plot.df$cor.vals)[i])
  }
})



## Pre vs. post-pandemic by setting
par(mfrow = c(1,1), xpd = T)
for(i in 1:length(mat.comp)){
  mat.comp[[i]]$log10_cnt = log10(1 + mat.comp[[i]]$mistry)
  mat.comp[[i]]$log10_epistorm_cnt = log10(1 + mat.comp[[i]]$epistorm)
  
  
  pears.cor = corr.test(mat.comp[[i]]$mistry, 
                        mat.comp[[i]]$epistorm, 
                        method = "pearson")
  
  pears.pvalue = ifelse(pears.cor$p < 0.001, "***",
                        ifelse(pears.cor$p < 0.01, "**",
                               ifelse(pears.cor$p < 0.05, "*", "")))
  
  dist.val = ifelse(unique(mat.comp[[i]]$setting) == "work",
                    0.01, 0.05)
  
  cnt = round(seq(0, round(max(mat.comp[[i]]$mistry), 2), 
                  by = dist.val), 2)
  log10_cnt = log10(1 + cnt)
  
  cnt.y = round(seq(0, round(max(mat.comp[[i]]$epistorm), 2), 
                    by = dist.val), 2)
  log10_cnt.y = log10(1 + cnt.y)
  

  plot(mat.comp[[i]]$log10_cnt,  mat.comp[[i]]$log10_epistorm_cnt, 
       xlab = "Per capita probability of contact age-to-age pairs", 
       ylab = "Per capita probability of contact age-to-age pairs in Epistorm", 
       main = unique(mat.comp[[i]]$setting),
       col= add.alpha(mycol$US[1], alpha = 0.75), 
       axes = F, pch = 19, cex = 1.5)
  box()
  axis(2, las = 2, at = log10_cnt.y, labels = cnt.y)
  axis(1, las = 1, at = log10_cnt, labels = cnt)
  
  abline(lm(mat.comp[[i]]$log10_epistorm_cnt~mat.comp[[i]]$log10_cnt), 
         col = "black", xpd = F)
  
  legend("bottomright", legend = paste0(unique(mat.comp[[i]]$setting), 
                                        " - Pearson's r = ",
                                        round(pears.cor$r, 2), pears.pvalue),
         inset = c(0.1, 0))
}



## Pre- vs. Post-pandemic remote/in-person
rem_inpers_df = do.call("rbind", school.work)
rem_inpers_df$log10_cnt = log10(1 + rem_inpers_df$contacts)
rem_inpers_df$log10_epistorm_cnt = log10(1 + rem_inpers_df$epi.contacts)

mode.work = c("inperson", "remote")
author = c("mistry", "prem")

for(i in mode.work){
  #indexes
  plot.idx = grep(i, rem_inpers_df$setting)
  m.idx = which(rem_inpers_df$setting == paste0(author[1], "_", i))
  p.idx = which(rem_inpers_df$setting == paste0(author[2], "_", i))
  
  ## Correlation for entire plot
  full.cor = corr.test(rem_inpers_df$contacts[plot.idx], 
                       rem_inpers_df$epi.contacts[plot.idx], 
                       method = "pearson")
  
  full.p = ifelse(full.cor$p < 0.001, "***",
                  ifelse(full.cor$p < 0.01, "**",
                         ifelse(full.cor$p < 0.05, "*", "")))
  ## Mistry correlation
  m.cor = corr.test(rem_inpers_df$contacts[m.idx], 
                    rem_inpers_df$epi.contacts[m.idx], 
                    method = "pearson")
  
  m.p = ifelse(m.cor$p < 0.001, "***",
               ifelse(m.cor$p < 0.01, "**",
                      ifelse(m.cor$p < 0.05, "*", "")))
  
  ## Prem correlation
  p.cor = corr.test(rem_inpers_df$contacts[p.idx], 
                    rem_inpers_df$epi.contacts[p.idx], 
                    method = "pearson")
  
  p.p = ifelse(p.cor$p < 0.001, "***",
               ifelse(p.cor$p < 0.01, "**",
                      ifelse(p.cor$p < 0.05, "*", "")))
  
  #axes and titles
  cnt.x = seq(0, round(max(rem_inpers_df$contacts[plot.idx])), by = 2)
  log10_cnt.x = log10(1 + cnt.x)
  
  cnt.y = seq(0, round(max(rem_inpers_df$epi.contacts[plot.idx])), 
              by = 0.5)
  log10_cnt.y = log10(1 + cnt.y)
  
  title.x = ifelse(i == "inperson", "Number of inferred contacts at work/school for age-to-age pairs",
                   "Number of inferred contacts not at work/school for age-to-age pairs")
  
  title.y = ifelse(i == "inperson", "Number of inferred contacts at work/school for age-to-age pairs in Epistorm-Mix",
                   "Number of inferred contacts not at work/school for age-to-age pairs in Epistorm-Mix")
  
  #plot
  plot(rem_inpers_df$log10_cnt[plot.idx],  
       rem_inpers_df$log10_epistorm_cnt[plot.idx], 
       col= add.alpha(mycol$US[factor(rem_inpers_df$setting[plot.idx])], alpha = 0.75),
       xlab = title.x, 
       ylab = title.y, 
       axes = F, pch = 19, cex = 1.5)
  box()
  axis(2, las = 2, at = log10_cnt.y, labels = cnt.y)
  axis(1, las = 1, at = log10_cnt.x, labels = cnt.x)
  abline(lm(rem_inpers_df$log10_epistorm_cnt~rem_inpers_df$log10_cnt), col = "black", xpd = F)
  legend("bottomright",                   
         legend = c(paste0("Mistry et. al., 2018 Pearson's r = ", 
                           round(m.cor$r, 2), m.p), 
                    paste0("Prem et. al., 2017 Pearson's r = ", 
                           round(p.cor$r, 2), p.p),
                    paste0("Full Pearson's r = ", round(full.cor$r, 2), full.p)),
         pch = 19, col = c(add.alpha(mycol$US, alpha = 0.75)), NA)
}

if(PDF == T){
  dev.off()
}





