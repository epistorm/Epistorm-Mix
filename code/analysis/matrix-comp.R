par(mfrow = c(1,1))

## Alterable parameters in code
PDF = T # if T, then visualizations are saved to a .pdf file
CSV = F # if T, then matrices are saved to a .csv file in the same format

## Read in Pre-pandemic matrix data for total contacts
### First author surnames, countries, and excel sheets (if applicable) 
mat.files = data.frame(author = c(rep("Jarvis", times = 4), "Wang", "Liang",
                                  "Mistry", "Prem"),
                       sheet = c("UK", "BE", "NL", "CH", 
                                 "non-symmeterized_total", NA,
                                 NA, "United States of America"),
                       labels = c("United-Kingdom", "Belgium", "The-Netherlands", 
                                  "Switzerland", "Suzhou,China", "HunanProvince,China",
                                  "US_Mistry", "US_Prem"))

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
    
    tmp = read_xlsx(paste0("./data/external_data/matrices-", mat.files$author[x], 
                           mat.files$file.type[x]), 
                    sheet = mat.files$sheet[x],
                    col_names = head.T)
    
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

### Liang
colnames(contact.mat$`HunanProvince,China`)[which(colnames(contact.mat$`HunanProvince,China`) == "X")] = 
  "resp_age"

colnames(contact.mat$`HunanProvince,China`)[-1] = 
  c("0-2", "3-6", "7-9", paste0(seq(10,70,by=5), "-", seq(14,74, by=5)), "75+")

contact.mat$`HunanProvince,China`$resp_age = colnames(contact.mat$`HunanProvince,China`)[-1]

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

#### Process Italy matrices - unsymmeterize matrices
### age groups for matrix
italy.ages = c(paste0(seq(0, 65, by = 5), "-",
                    c(seq(4, 69, by = 5))), "70+")

age.breaks = c(seq(0, 70, by = 5), 120)

### Read population size and estimate distribution - group 100+
italy.pop = read.csv("./data/external_data/resident_population_Italy.csv")
tot.pop.Italy = italy.pop$Total[which(italy.pop$Age == "Total")]

italy.pop = italy.pop[which(italy.pop$Age != "Total"), ]
italy.pop$Age = as.numeric(ifelse(italy.pop$Age == "100 and over", 100, 
                                  italy.pop$Age))

### Read sample population distribution and normalize
italy.samp = read.csv("./data/external_data/Italy-sample-size.csv")
colnames(italy.samp) = c("age", "dist.sample")

italy.samp$age = round(italy.samp$age)
italy.samp$dist.sample.norm = italy.samp$dist.sample / sum(italy.samp$dist.sample)

italy.samp$age.group = italy.ages

### Assign single-year age to respective age group
italy.pop$age.group = cut(italy.pop$Age, breaks = age.breaks,
                          right = F)

italy.pop$age.group = as.character(italy.pop$age.group)
for (i in unique(italy.pop$age.group)){
  idx.lbls = which(italy.pop$age.group == i)
  if (i == "[70,120)"){
    italy.pop$age.group[idx.lbls] = paste0(str_extract_all(i, "\\d+\\.?\\d*")[[1]][1],"+")
  }else {
    italy.pop$age.group[idx.lbls] = paste(str_extract_all(i, "\\d+\\.?\\d*")[[1]][1], 
                                          as.numeric(str_extract_all(i, "\\d+\\.?\\d*")[[1]][2]) - 1, 
                                          sep = "-")  
  }
}
italy.pop$age.group = factor(italy.pop$age.group,
                             levels = unique(italy.pop$age.group))

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
  tmp0 = read.csv(paste0("./data/external_data/Lucchini_bootstrap_age_group_total/contact_matrix_age_group_bootstrap_", 
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
              "by3_10-by5_75", 
              "In-Person-by5_75-cond-matrix", 
              "In-Person-by5_80-cond-matrix")

epistorm.mat = lapply(filenames, function(x){
  tmp = read.csv(paste0(output.dir, "csv/compare/", x, ".csv"))
  
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

#### Comparison of in-person work and school between Mistry and Prem with
## the Epistorm-Mix data

### In-person school/work attendance
school.work = list()
school.work$mistry.inpers.comp = full_join(epistorm.mat.long$`In-Person-by5_80-cond-matrix`,
                                           contact.mat.long$US_Mistry)
school.work$mistry.inpers.comp$setting = "mistry_inperson"

school.work$prem.inpers.comp = full_join(epistorm.mat.long$`In-Person-by5_75-cond-matrix`,
                                         contact.mat.long$US_Prem)

school.work$prem.inpers.comp$setting = "prem_inperson"

## Calculate difference
school.work.diff = lapply(school.work, function(x){
  x$cont.diff = x$contacts - x$epi.contacts
  
  as.data.frame(x[, c("resp_age", "cont_age", "cont.diff")])
})

matrix.diff$Mistry_inperson = school.work.diff$mistry.inpers.comp
matrix.diff$Prem_inperson = school.work.diff$prem.inpers.comp

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

## Convert to wide datset and save
diff.mat.wide = lapply(1:length(matrix.diff), function(x){
  tmp = matrix.diff[[x]] %>%
    spread(key = cont_age, value = cont.diff)
  
  tmp = tmp[order(tmp[, "resp_age"], levels(tmp[, "resp_age"])), ]
  
  if(CSV == T){
    write.csv(tmp, paste0(output.dir, "./csv/difference-matrices/", 
                          names(matrix.diff)[x], ".csv"))
  }
  
  tmp
})

#### LOG10 Visualization

if(PDF == T){
  pdf(paste0(output.dir, "log10-correlations.pdf"))
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


plot.groups = list(cou = c("Belgium", "HunanProvince,China",  "Italy", 
                           "Suzhou,China", "Switzerland", "The-Netherlands", 
                           "United-Kingdom")) #, 
                   # prepan = c("US_Mistry", "US_Prem"))

mycol = list(cou = c( "plum",  "#00B6EB", "#F8766D", "#A58AFF", "#53B400", 
                      "#00C094", "#C49A00"),
             US = c("#60C4B2", "#4a6894"))


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
  
  legend("topleft", legend = paste0("Pearson's r = ", 
                                     round(tmp1$r, 2), 
                                     pears.pvalue),
         bty = "n")
  
  
  ## Single plots
  par(mar = c(5, 4, 4, 8), mfrow = c(1,1), xpd = T)
  
  for(i in 1:length(plot.groups[[x]])){
    plot(plot.df$log10_cnt,  plot.df$log10_epistorm_cnt, 
         xlab = "Number of contacts for age-to-age pairs", 
         ylab = "Number of contacts for age-to-age pairs in Epistorm-Mix",
         main = plot.groups[[x]][i],
         axes = F, type = "n")
    box()
    axis(2, las = 2, at = log10_cnt.y, labels = cnt.y)
    axis(1, las = 1, at = log10_cnt.x, labels = cnt.x)
    abline(lm(plot.df$log10_epistorm_cnt~plot.df$log10_cnt), col = "black", xpd = F)
    points(plot.df$log10_cnt[which(plot.df$country == plot.groups[[x]][i])], 
           plot.df$log10_epistorm_cnt[which(plot.df$country == plot.groups[[x]][i])],
           col = add.alpha(mycol[[x]][i], alpha = 0.75),
           pch = 19, cex = 1.5)
    legend("bottomright", legend = gsub(paste0(plot.groups[[x]][i], " - "), "",
                                        plot.df$cor.vals[grep(plot.groups[[x]][i], plot.df$cor.vals)][1]),
           bty = "n")
  }
})


## Pre- vs. Post-pandemic in-person
rem_inpers_df = do.call("rbind", school.work)
rem_inpers_df$log10_cnt = log10(1 + rem_inpers_df$contacts)
rem_inpers_df$log10_epistorm_cnt = log10(1 + rem_inpers_df$epi.contacts)
author = c("mistry", "prem")

#indexes
plot.idx = grep("inperson", rem_inpers_df$setting)
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

# plot
plot(rem_inpers_df$log10_cnt[plot.idx],  
     rem_inpers_df$log10_epistorm_cnt[plot.idx], 
     col= add.alpha(mycol$US[factor(rem_inpers_df$setting[plot.idx])], alpha = 0.75),
     xlab = title.x, 
     ylab = title.y, 
     axes = F, pch = 19, cex = 1.5)
box()
idx.check = log10_cnt.y < max(rem_inpers_df$log10_epistorm_cnt[plot.idx])
axis(2, las = 2, at = log10_cnt.y[idx.check], labels = cnt.y[idx.check])
axis(1, las = 1, at = log10_cnt.x, labels = cnt.x)
abline(lm(rem_inpers_df$log10_epistorm_cnt~rem_inpers_df$log10_cnt), col = "black", xpd = F)
legend("topleft",                   
       legend = c(paste0("Mistry et. al., 2018 Pearson's r = ", 
                         round(m.cor$r, 2), m.p), 
                  paste0("Prem et. al., 2017 Pearson's r = ", 
                         round(p.cor$r, 2), p.p),
                  paste0("Full Pearson's r = ", round(full.cor$r, 2), full.p)),
       bty = "n",
       pch = 19, col = c(add.alpha(mycol$US, alpha = 0.75), NA))


if(PDF == T){
  dev.off()
}

