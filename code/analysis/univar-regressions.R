#### Set up ####

## set the threshold for the number of observations per group 
g.thrshld = 3

## set the upper limit for the smoothing parameter
#potential.ks = c(4:40) ## k selected based on multiple refitting of the data
smooth.k = 21

## do glm and gam regressions
gam.ys = c("total.contacts", "cont_setting_nothh_n")

#### Univariable Analysis ####

terms_covariates = c("resp_age", "ppwork2",
                     "nothome","resp_ethn",
                     "resp_sex", 
                     "resp_resp_sick", "resp_hhm_sick",
                     "hhsize2", "hh_income",
                     "is_weekend2", "wave",
                     "resp_age_insightnet")

## labels of the variables
mytb.vars.labels = list(resp_ethn = "Race/Ethnicity",
                        resp_sex = "Sex",
                        ppwork2 = "Main Activity",
                        ppwork_nothome = "Main Activity and Mode",
                        hh_income = "Household Income",
                        resp_age = "Age",
                        resp_age_by5_80 = "5-year Age Group",
                        resp_age_insightnet = "Ad-hoc Age Group",
                        hhsize2 = "Household Size",
                        is_weekend2 = "Day",
                        resp_resp_sick = "Sick",
                        resp_hhm_sick = "Household Member Sick",
                        hh_loc_reg4 = "Region",
                        weeks = "Survey Week",
                        wave = "Wave")

## check difference in means of both y's by factor variables
y.corr = expand.grid(setdiff(terms_covariates, "resp_age"), gam.ys)
mypairs = list()

for(i in 1:nrow(y.corr)){
  x = as.character(y.corr[i,1])
  y = as.character(y.corr[i,2])
  #print(y.corr[i,])
  
  # create a dataset specifically for this analysis
  tmp.compl = sum.res3[which(!is.na(sum.res3[[x]]) & !is.na(sum.res3[[y]])), ]
  tmp.compl$x = tmp.compl[[x]]
  tmp.compl$y = tmp.compl[[y]]
  
  if(is.factor(tmp.compl[[x]])){
    levels_factor = levels(tmp.compl[[x]])
    levels_factor = levels_factor[which(sapply(levels_factor, function(l)
      sum(tmp.compl[[x]] == l) > g.thrshld))]
    
    mypairs[[i]] = list()
    
    # select only the levels with sufficient number of observations
    subset2 = tmp.compl[which(tmp.compl[[x]] %in% levels_factor),]
    # set different levels as reference 
    for(j in levels_factor){
      subset2[[x]] = relevel(subset2[[x]], ref = j)
      nbreg = glm.nb(as.formula(paste0(y, "~", x)), weights = weight, data = subset2)
      # save results in a data frame
      mypairs[[i]][[j]] = as.data.frame(summary(nbreg)$coefficients)
      mypairs[[i]][[j]] = cbind(mypairs[[i]][[j]], confint(nbreg))
      colnames(mypairs[[i]][[j]])[4:6] = c("p_value", "95ci.ll", "95ci.ul")
      mypairs[[i]][[j]] = mypairs[[i]][[j]][which(rownames(mypairs[[i]][[j]]) != "(Intercept)"),]
      mypairs[[i]][[j]]$stars = cut(mypairs[[i]][[j]]$p_value, breaks = c(0,0.001,0.01,0.05, 0.1,1),
                                    labels = c("***","**", "*", "†", " "), include.lowest = T)
      mypairs[[i]][[j]]$level = gsub(x,"",rownames(mypairs[[i]][[j]]))
      mypairs[[i]][[j]]$ref = j
      rownames(mypairs[[i]][[j]]) = NULL
      # save regression "goodness-of-fit" 
      observed_quantiles = sort(nbreg$y)
      theoretical_quantiles = qnbinom(ppoints(length(observed_quantiles)), 
                                      mu = fitted(nbreg), 
                                      size = nbreg$theta)
      mypairs[[i]][[j]]$ks_deviation = sum(abs(observed_quantiles - theoretical_quantiles))/
        length(observed_quantiles)
      mypairs[[i]][[j]]$rmse = (sqrt(mean((nbreg$y - fitted(nbreg))^2))/ mean(nbreg$y)) * 100
      mypairs[[i]][[j]]$AIC = nbreg$aic
      mypairs[[i]][[j]]$theta = summary(nbreg)$theta
      mypairs[[i]][[j]]$xvar = x
      mypairs[[i]][[j]]$yvar = y
      
    }
  }
}
## gather all regressions in one dataframe
pairwise.comparison = Reduce(rbind, unlist(mypairs, recursive = F))
pairwise.comparison$Abs_coef = abs(pairwise.comparison$Estimate)

## remove "duplicates"
idx.dupl = lapply(c(1:nrow(pairwise.comparison)), function(i)
  sort(c(which(pairwise.comparison[, "ref"] == pairwise.comparison[i, "level"] &
                 pairwise.comparison[, "level"] == pairwise.comparison[i, "ref"] &
                 pairwise.comparison[, "yvar"] == pairwise.comparison[i, "yvar"]),i))
)

idx.remove = sapply(idx.dupl[duplicated(idx.dupl)], function(x) x[1])

if(length(idx.remove) > 0){
  pairwise.comparison = pairwise.comparison[-idx.remove,]
}
# add textual column in a format for the SM
pairwise.comparison$est_text = paste0(round(exp(pairwise.comparison$Estimate), 2),
                                      pairwise.comparison$stars, " (",
                                      round(exp(pairwise.comparison$`95ci.ll`), 2),
                                      " - ", round(exp(pairwise.comparison$`95ci.ul`), 2),")")
# export the results of univariate analysis
output.uni = as.data.frame(pairwise.comparison) %>%
  filter(yvar == "total.contacts") %>%
  dplyr::select(xvar, ref, level,  est_text) 

write.csv(output.uni, paste0(sm.dir, "indep-glm.csv"))

## Check correlations between covariates:
# set levels for interpretation of Cramer's V
classify_cramers_v = function(cramers_v_value) {
  if (cramers_v_value >= 0.00 & cramers_v_value < 0.10) {
    return("Very Weak")
  } else if (cramers_v_value >= 0.10 & cramers_v_value < 0.20) {
    return("Weak")
  } else if (cramers_v_value >= 0.20 & cramers_v_value < 0.40) {
    return("Moderate")
  } else if (cramers_v_value >= 0.40 & cramers_v_value < 0.60) {
    return("Relatively Strong")
  } else if (cramers_v_value >= 0.60 & cramers_v_value < 0.80) {
    return("Strong")
  } else if (cramers_v_value >= 0.80 & cramers_v_value <= 1.00) {
    return("Very Strong")
  } else {
    return("Invalid Cramer's V value. Must be between 0 and 1.")
  }
}
# create the grid for correlation analysis
xy = matrix(unlist(combn(terms_covariates, 2, simplify = FALSE)), ncol = 2, byrow = T)
covariate.corr = list()

for(i in 1:nrow(xy)){
  x = xy[i,1]
  y = xy[i,2]
  #print(xy[i,])
  
  # check that one variable is not an interaction term of the other
  same.core = grepl(gsub("[0-9]","", x), y) | grepl(gsub("[0-9]","", y), x)
  
  # compute correlation estimates
  if(x != y & !same.core){
    # use complete case analysis
    tmp.compl = sum.res3[which(!is.na(sum.res3[[x]]) & !is.na(sum.res3[[y]])), ]
    tmp.compl$x = tmp.compl[[x]]
    tmp.compl$y = tmp.compl[[y]]
    
    # handle respondent age separately as it is numeric
    if("resp_age" %in% xy[i,]){
      
      factor_var = ifelse(x == "resp_age", y, x)
      numeric_var = "resp_age"
      
      if(is.factor(tmp.compl[[factor_var]])){
        levels_factor = levels(tmp.compl[[factor_var]])
        levels_factor = levels_factor[which(sapply(levels_factor, function(l)
          sum(tmp.compl[[factor_var]] == l) > 3))]
        
        pairs = combn(levels_factor, 2) # pairs of the factor levels 
        covariate.corr[[paste0(x, " vs. ", y)]] = cbind(t(pairs),NA, ">= 0.1")
        colnames(covariate.corr[[paste0(x, " vs. ", y)]]) = c("level1", "level2", "Diff", "p.value")
        for(j in 1:ncol(pairs)){
          level1 = pairs[1, j]
          level2 = pairs[2, j]
          
          subset1 = subset(tmp.compl,
                           tmp.compl[[factor_var]] == level1 |
                             tmp.compl[[factor_var]] == level2)
          # define survey design
          design1 = svydesign(ids = ~1, weights = ~weight, data = subset1)
          # calculate ttests for weighted survey data
          ttest_result = svyttest(as.formula(paste0(numeric_var, "~", factor_var)), design1)
          # save results
          p_value = ttest_result$p.value
          stars = cut(p_value, breaks = c(0,0.01,0.05, 0.1,1),
                      labels = c("***","**", "*", "."), include.lowest = T)
          
          diff.text = paste0(round(ttest_result$estimate, 2), stars,
                             " (95% CI:", round(ttest_result$conf.int[1])," - ",
                             round(ttest_result$conf.int[2]), ")")
          covariate.corr[[paste0(x, " vs. ", y)]][j,3] = diff.text
          
          if(p_value < 0.1){ # save only the statistically significant results
            print(paste0(numeric_var, " (", level1, " vs. ", level2, "): Diff = ",
                         diff.text,
                         ", p = ",
                         round(p_value, 3)))
            covariate.corr[[paste0(x, " vs. ", y)]][j,4] = p_value 
          }
        }
        
        idx.above0 = which(covariate.corr[[paste0(x, " vs. ", y)]][,"p.value"] != ">= 0.1")
        covariate.corr[[paste0(x, " vs. ", y)]] = covariate.corr[[paste0(x, " vs. ", y)]][idx.above0,]
        if(length(idx.above0) == 0){
          covariate.corr[[paste0(x, " vs. ", y)]] = NULL
        }else if(length(idx.above0) == 1){
          covariate.corr[[paste0(x, " vs. ", y)]] = c(x, y, covariate.corr[[paste0(x, " vs. ", y)]])
        }else{
          covariate.corr[[paste0(x, " vs. ", y)]] = cbind(x, y, covariate.corr[[paste0(x, " vs. ", y)]])
        }
      }
      
    }else{
      # special case for age groups -> only plotting
      if(sum(grepl("resp_age", xy[i,])) > 0){
        quasinumeric_var = xy[i,grepl("resp_age", xy[i,])]
        factor_var = setdiff(xy[i,], quasinumeric_var)
        plot.agediff = tmp.compl %>% group_by(x,y) %>% summarise(n = sum(weight, na.rm = T)) 
        ggplot(plot.agediff, aes(x = y, y = n, group = x, color = x)) +
          # Add the line layer. The line will break where there are NA values, which is the correct behavior.
          geom_line(linewidth = 1) +
          # Add points to show the actual data points.
          geom_point(size = 3) +
          # Improve the labels and title
          labs(
            title = paste0("Distribution by Age Groups by ", mytb.vars.labels[[factor_var]]),
            x = "Age Group",
            y = "N of observations",
            color = mytb.vars.labels[[factor_var]] # This changes the legend title
          ) +
          # Apply a clean theme
          theme_minimal() +
          # Customize the theme for better readability
          theme(
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.title = element_text(size = 12),
            legend.position = "bottom",
            legend.title = element_text(face = "bold")
          )
        ggsave(paste0(sm.dir, quasinumeric_var, "_by_", factor_var, ".pdf"))
      }else{
        # define survey design
        survey_design = svydesign(ids = ~1, weights = ~weight, data = tmp.compl)
        # for factor variables, use Chi-squared test for weighted survey data
        x2_test = svychisq(~ x + y, design = survey_design)
        if(x2_test$p.value < 0.1){
          stars = cut(x2_test$p.value, breaks = c(0,0.01,0.05, 0.1, 1),
                      labels = c("***","**", "*", "."), include.lowest = T)
          
          # Calculate Cramer's V
          n = nrow(survey_design$variables)
          z = length(unique(tmp.compl$x))
          r = length(unique(tmp.compl$y))
          cramers_v = sqrt(x2_test$statistic / (n * min(z - 1, r - 1)))
          
          print(paste0(x, " and ", y, ": Chi^2 is stat.sig.(", stars,
                       "), Cramer's V = ", round(cramers_v, 3)))
          covariate.corr[[paste0(x, " vs. ", y)]] = c(rep(x, length(cramers_v)), 
                                                      rep(y, length(cramers_v)),
                                                      cramer.v = paste0(round(as.numeric(cramers_v), 3),
                                                                        as.character(stars)),
                                                      carmer.v.interpret = classify_cramers_v(cramers_v),
                                                      #p.value = as.numeric(round(x2_test$p.value,3)), 
                                                      #stars = as.character(stars), 
                                                      method = x2_test$method)
        }
      }
    }
  }
  
}

## separate factor from numeric variables
idx.factors = which(sapply(covariate.corr, function(x) is.null(dim(x)) & 
                             "method" %in% names(x)))
idx.num = which(sapply(covariate.corr, function(x)
  length(x) > 0))
# save correlation between factors in a separate table
if(length(idx.factors) > 0){
  factor.corr = as.data.frame(Reduce(rbind, covariate.corr[idx.factors]))[-1,]
  colnames(factor.corr) = names(covariate.corr[idx.factors][[1]])
  
  idx.num = setdiff(idx.num, idx.factors)
}
write.csv(factor.corr, paste0(sm.dir, "factor_corr.csv"))

# save age difference by group in a separate table
num.factor = as.data.frame(Reduce(rbind, covariate.corr[idx.num]))
write.csv(num.factor, paste0(sm.dir, "agediff_by_levels.csv"))

