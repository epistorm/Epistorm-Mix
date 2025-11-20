##### Set up ##### 
post.est = F # if T, there will be a post-estimates analysis

# gam.results - defined in "_pipeline_analysis.R"
# read_specs.T - defined in "_pipeline_analysis.R"

## set the threshold for the number of observations per group 
g.thrshld = 3

## set the upper limit for the smoothing parameter
# potential.ks = c(4:40) ## k selected based on multiple refitting of the data
smooth.k = 21

## do glm and gam regressions
gam.ys = c("total.contacts", "cont_setting_nothh_n")

####### Run multivariable GAM analysis ########
terms_covariates = c("resp_age", "resp_age_by5_80", "resp_age_insightnet",
                     "ppwork2", "ppwork_nothome",
                     "resp_ethn",
                     "resp_sex", 
                     "resp_resp_sick", "resp_hhm_sick",
                     "hhsize2", "hh_income", "hh_loc_reg4",
                     "is_weekend2", "weeks","wave")

all_combinations = list()  # Initialize an empty list to store combinations
for (i in 1:length(terms_covariates)) {
  combinations_i = combn(terms_covariates, i, simplify = FALSE)
  check_multiple = sapply(combinations_i, function(x)
    sum(grepl("resp_age", x)) <= 1 & sum(grepl("ppwork", x)) <= 1)
  all_combinations = c(all_combinations, combinations_i[check_multiple])
}
## Print the number of combinations
length(all_combinations)

source(file.path(global.dir, "gam_data_prep.R"))

if(!read_specs.T){
  ## set up parallel computing
  ## Make sure implicit clusters are all released
  stopImplicitCluster()
  ## Detect the number of cores available
  num_cores = detectCores(logical = FALSE) - 1  # Use one core less than physical cores to leave room for other processes
  ## make sure it runs in R engine
  options(gsubfn.engine = "R")
  ## Register the parallel backend
  cl = makeCluster(num_cores)
  registerDoParallel(cl)
  ## Update packages and set cluster seeds
  update.packages(c("randomForest", "doRNG", "foreach"))
  clusterSetRNGStream(cl = cl, iseed = 5432)
  
  t1 = Sys.time()
  ## Main foreach loop:
  ## Perform GAM analysis for each y variable
  
  reg.gam = foreach(y = gam.ys, .combine = 'list') %do% {
    print(paste("Processing variable:", y))
    
    ## Inner foreach loop: handle combinations of x variables for a given y
    results_for_y = foreach(current_combination = all_combinations, 
                            .combine = 'c', 
                            .inorder = TRUE,
                            .packages = c("mgcv", "dplyr", "performance")) %dopar% {
                              my.fullgam(
                                current_combination = current_combination,
                                mydata = sum.res3.gam,
                                y = y,
                                k = smooth.k,
                                reg.type = "nb",
                                terms_covariates = terms_covariates,
                                myprint = TRUE,
                                time.step = 30
                              )
                            }
    
    results_for_y = lapply(c(1:(length(results_for_y)/2)), function(x){
      results_for_y[((x-1)*2+1):((x-1)*2+2)]
    })
    
    ## Return the list of results for the current y
    return(results_for_y)
  }
  print(Sys.time() - t1)
  
  ## stop the cluster
  stopCluster(cl)
  
  ## name outer loop results
  names(reg.gam) = gam.ys
  
  ## export goodness-of-fit values for selection
  tmp.res = lapply(reg.gam, function(x)
    as.data.frame(t(sapply(x, function(reg)
      c(AIC = reg$model$aic,
        Rsq = reg$summary$r.sq,
        Dev = reg$summary$dev.expl,
        ## make sure the models that had issues are not selected
        bad = sum(reg$model$issues != "")))))) 
  
  names(tmp.res) = gam.ys
  
  ## select best fits
  best_fit = lapply(tmp.res, function(x) c(aic = setdiff(order(x$AIC), which(x$bad == 1))[1],
                                           rsq = setdiff(order(x$Rsq, decreasing = T), which(x$bad == 1))[1],
                                           dev = setdiff(order(x$Dev, decreasing = T), which(x$bad == 1))[1]
  ))
  
  best.combin = lapply(best_fit, function(x)
    all_combinations[x]
  )
  
  ## Adding specifications with age groups and main activity 
  ## (without participation mode) if not already present
  add.agevar = setdiff(grep("resp_age", terms_covariates, value = T), "resp_age")
  
  ## Select additional specs
  add.combin = lapply(best.combin, function(x){
    v.l =   sapply(x, function(x2) length(x2))
    longest.spec = x[[which(v.l == max(v.l))[1]]]
    age.base = setdiff(order(v.l, decreasing = T),
                       which(sapply(x, function(x2) sum(grepl("resp_age", x2)) < 1)))[1]
    ## add regression with ppwork only
    if("ppwork_nothome" %in% longest.spec){
      specs = c(actvty = match(paste0(gsub("ppwork_nothome", "ppwork2", longest.spec), collapse = ","), 
                               sapply(all_combinations, function(y) paste0(y, collapse = ","))))
      ## add regression with other age vriables
    }else{
      specs = NULL
    }
    if(length(add.agevar) > 0 & length(age.base) > 0){
      specs = c(specs,
                sapply(add.agevar, function(av){
                  if(sum(sapply(x, function(x2) av %in% x2)) == 0){
                    tmp = gsub(paste0(c("^resp_age\b", setdiff(add.agevar, av)), 
                                      collapse = "|"), av, x[[age.base]])
                    if(!identical(tmp, x[[age.base]])){
                      match(paste0(tmp, collapse = ","), 
                            sapply(all_combinations, function(y) paste0(y, collapse = ",")))
                    }else{NA}
                  }else{NA}
                }))
      return(specs)
    }else if(length(add.agevar) > 0 & length(age.base) == 0){
      specs = c(specs,
                sapply(add.agevar, function(av)
                  match(paste0(sort(c(av, longest.spec)) , collapse = ","),
                        sapply(all_combinations, function(y) paste0(sort(y), collapse = ",")))))
      return(specs)
    }else{
      return(NULL)
    }
  })
  
  
  best.models = lapply(seq_along(reg.gam), function(x) {
    bm = lapply(reg.gam[[x]][best_fit[[x]]], function(m) {
      m[[1]]$dev.expl = summary(m[[1]])$dev.expl
      return(m[[1]])
    })
    mnames = names(best_fit[[x]])
    if(length(na.omit(add.combin[[x]])) > 0){
      bm = c(bm, lapply(reg.gam[[x]][na.omit(add.combin[[x]])], function(m){
        m[[1]]$dev.expl = summary(m[[1]])$dev.expl
        return(m[[1]])
      }))
      mnames = c(mnames, names(na.omit(add.combin[[x]])))
    }
    names(bm) = mnames
    return(bm)
  })
  best.models
  
  cat("Best Fit Specifications\n\n", file = file.path(gam.results, "model_specs.txt"))
  
} else {
  
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
  
  ## read the best specifications from file
  ## if you do not want to rerun all the regressions - read the selected specifications from this file:
  file_path = file.path(gam.results,"model_specs.txt")
  # read all lines from the file into a character vector
  lines = readLines(file_path)
  lines = lines[3:length(lines)]
  lines = unlist(setNames(lines[seq(2, length(lines), by=2)], lines[seq(1, length(lines), by=2)]))
  
  best.models = lapply(seq_along(gam.ys), function(x) {
    y = gam.ys[x]
    myformula = lines[grep(y, lines)]
    m = lapply(myformula, function(l)
      gam(as.formula(l),
          data = sum.res3.gam,
          weights = sum.res3.gam$weight,
          family = "nb")
    )
    return(m)
  })
  names(best.models) = c("total.contacts", "cont_setting_nothh_n")
  
}

list_of_tables = lapply(best.models, function(outcome)
  setNames(lapply(seq_along(outcome), function(i) {
    
    model = outcome[[i]]
    
    print(gam.check(model)) 
    if(!read_specs.T){
    ## This line appends to the next specification to the created file
    cat(names(outcome)[i], "\n", file = file.path(sm.dir, "gam", "model_specs.txt"), 
        append = TRUE)
    cat(deparse(formula(model)), "\n", file = file.path(sm.dir, "gam", "model_specs.txt"), 
        append = TRUE)
    }
    t1 = tbl_regression(model, 
                        label = mytb.vars.labels,
                        exponentiate = TRUE) %>%
      ## add significance stars 
      add_significance_stars(pattern = "{estimate}{stars}", 
                             hide_p = F,
                             hide_se = T) %>%
      ## merge the estimate and CI columns into a single column
      modify_column_merge(
        pattern = "{estimate}{stars}\n({ci})",
        rows = !is.na(ci)
      )
    t2 = t1 %>%
      ## And rename the estimate column
      modify_header(estimate = "**RR (95% CI)**")     %>%
      add_glance_table(
        include = c(AIC, adj.r.squared), # These are common names from broom::glance for gam models
        label = list(
          AIC = "**AIC**",
          adj.r.squared = "**$R^2$**"#,
          #dev.expl = "**Deviance Explained**"
        )
      ) %>%
      bold_p(t = 0.05)
    
    return(t2)
  }), nm = names(outcome)))

## add column labels and export results table
lapply(seq_along(list_of_tables), function(z){
  tbl = list_of_tables[[z]]
  
  main.title = names(tbl)
  main.title = case_when(
    main.title == "aic" ~ "**Best AIC**",
    main.title == "rsq" ~ "**Best $R^2$**",
    main.title == "dev" ~ "**Best Deviance Explained**",
    main.title == "actvty" ~ "**Main Activity only**",
    TRUE ~ paste0("**", str_to_title(main.title), "**")
  )
  
  merged_tables = tbl_merge(tbl, tab_spanner = main.title) %>%
    style_my_table()
  
  merged_tables |>
    as_gt() |>
    gt::tab_source_note(gt::md("Results of the best fitting model. The goodness-of-fit measure indicated in the column name"))|> 
    gt::gtsave(filename = file.path(sm.dir, "gam", paste0("multivariate_gam_bf_", z, ".docx")))
})

## Post-estimation analysis
if (post.est == T){
  ## Extract residuals
  sum.res3.gam$tot.cont.res = residuals(best.models[[1]]$aic)
  sum.res3.gam$nonhh.res = residuals(best.models[[2]]$aic)
  ## Get the terms matrix
  terms_matrix = predict(best.models[[1]]$aic, type = "terms")
  ## Extract linear predictors
  sum.res3.gam$tot.cont.lp = rowSums(terms_matrix)
  ## Get the terms matrix
  terms_matrix2 = predict(best.models[[2]]$aic, type = "terms")
  ## Extract linear predictors
  sum.res3.gam$nonhh.lp = rowSums(terms_matrix2)

  sum.res3.gam$tot.cont.pred = predict(best.models[[1]]$aic, type = "response")
  sum.res3.gam$nonhh.pred = predict(best.models[[1]]$aic, type = "response")

  ggplot(sum.res3.gam, aes(x = tot.cont.lp, y = tot.cont.res, color = ppwork_nothome)) +
    geom_point(alpha = 0.5) +  # Adjust alpha for point transparency
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a horizontal line at y=0
    labs(
      title = "Residuals vs. Linear Predictor: Total",
      x = "Linear Predictor",
      y = "Residuals",
      color = "Grouping Variable"
    ) +
    xlim(quantile(sum.res3.gam$tot.cont.lp, p=c(0.0025, 0.9975)) ) +
    theme_minimal()

  ggplot(sum.res3.gam, aes(x = nonhh.lp, y = nonhh.res, color = ppwork_nothome)) +
    geom_point(alpha = 0.5) +  # Adjust alpha for point transparency
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a horizontal line at y=0
    labs(
      title = "Residuals vs. Linear Predictor: Non-Household",
      x = "Linear Predictor",
      y = "Residuals",
      color = "Grouping Variable"
    ) +
    xlim(quantile(sum.res3.gam$nonhh.lp, p=c(0.0025, 0.9975)) ) +
    theme_minimal()

  corr_coeff = cor(sum.res3.gam$tot.cont.pred, sum.res3.gam$total.contacts)
  label_text = paste(
    "R-squared: ", round(summary(best.models[[1]]$aic)$r.sq, 3), "\n",
    "R: ", round(corr_coeff, 3), sep = ""
  )

  ggplot(sum.res3.gam, aes(x = tot.cont.pred, y = total.contacts, color = ppwork_nothome)) +
    geom_point(alpha = 0.5) +  # Adjust alpha for point transparency
    geom_smooth(method = "lm", se = F, linetype = "dashed", color = "red") +
    labs(
      title = "Prediction vs. Observed: Non-Household",
      x = "Predicted",
      y = "Observed",
      color = "Grouping Variable"
    ) +
    annotate(
      "text",
      x = quantile(sum.res3.gam$tot.cont.pred, p=0.05),
      y = quantile(sum.res3.gam$total.contacts, p=0.99),
      label = label_text,
      hjust = 0,
      vjust = 1,
      size = 4
    ) +
    xlim(quantile(sum.res3.gam$tot.cont.pred, p=c(0.0025, 0.9975)) ) +
    theme_minimal()

  corr_coeff = cor(sum.res3.gam$nonhh.pred, sum.res3.gam$cont_setting_nothh_n)
  label_text = paste(
    "R-squared: ", round(summary(best.models[[2]]$aic)$r.sq, 3), "\n",
    "R: ", round(corr_coeff, 3), sep = ""
  )
  ggplot(sum.res3.gam, aes(x = nonhh.pred, y = cont_setting_nothh_n, color = ppwork_nothome)) +
    geom_point(alpha = 0.5) +  # Adjust alpha for point transparency
    geom_smooth(method = "lm", se = F, linetype = "dashed", color = "red") +
    labs(
      title = "Prediction vs. Observed: Non-Household",
      x = "Predicted",
      y = "Observed",
      color = "Grouping Variable"
    ) +
    annotate(
      "text",
      x = quantile(sum.res3.gam$nonhh.pred, p=0.05),
      y = quantile(sum.res3.gam$cont_setting_nothh_n, p=0.95),
      label = label_text,
      hjust = 0,
      vjust = 1,
      size = 4
    ) +
    xlim(quantile(sum.res3.gam$nonhh.pred, p=c(0.0025, 0.9975)) ) +
    theme_minimal()

  library(DHARMa)

  for(i in seq_along(best.models)){
    m = best.models[[i]]$aic
    residuals_dev <- residuals(m, type = "deviance")

    cat("\n--- DHARMa Residual Diagnostics ---\n")
    simulationOutput <- simulateResiduals(fittedModel = m, plot = TRUE)
    plot(simulationOutput) # This generates a set of diagnostic plots

    ## Specific tests:
    testDispersion(simulationOutput)   # Check for over/underdispersion
    testZeroInflation(simulationOutput) # Check for zero-inflation (if applicable)
    testUniformity(simulationOutput)    # Check if residuals are uniformly distributed

    cat("\n--- Dispersion Parameter (for Negative Binomial) ---\n")
    print(m$family$getTheta(TRUE))
  }
}
