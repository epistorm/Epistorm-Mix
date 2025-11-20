#### Functions for Analysis ####

#### loading functions ####
load.myps = function(x){
  if(!require(x, character.only = T)){
    install.packages(x)
  }
  library(x, character.only = T)
  return(x)
}

#### create age group ####
## helper function
mycut = function(var, breaks){
  maxv = breaks[length(breaks)]
  if(maxv > max(var, na.rm = T)){
    mylabels = c(paste(breaks[1:(length(breaks)-2)], 
                       breaks[2:(length(breaks)-1)]-1, sep = "-"),
                 paste0(breaks[length(breaks)-1],"+"))
  }else{
    mylabels = paste(breaks[1:(length(breaks)-1)], 
                     breaks[2:length(breaks)]-1, sep = "-")
  }
  var = cut(var,
            breaks = breaks,
            labels = mylabels,
            right = F)
}

## main function
create.age.groups = function(mydata,
                             input.var = "resp_age",
                             break.list = list(seq(0,120, by = 5),
                                               c(seq(0,75, by = 5), 120),
                                               c(seq(0,80, by = 5), 120),
                                               seq(0,120, by = 15),
                                               age.groups),
                             suff.list = paste0("_",c("by5", "by5_75","by5_80","by15","g"))){
  
  tmp.vars = lapply(break.list, function(x) mycut(mydata[,input.var],x))
  tmp.names = paste0(input.var, suff.list)
  names(tmp.vars) = tmp.names
  mydata[,tmp.names] = as.data.frame(tmp.vars)
  return(mydata)
}

#### adjust to local time zone and define diary date ####
adjust_date <- function(dt, cutoff = NULL) { ## dt needs to be in local time zone
  if(!is.null(cutoff)){
    if(hour(dt) < cutoff){ # to adjust date based on 4 AM cutoff
      local_date = as.Date(dt) - 2
    }else{
      local_date = as.Date(dt) - 1
    }
  }else{
    local_date = as.Date(dt) - 1
  }
  return(local_date)
}

#### estimate distributions ####

## assist functions
putinbins = function(v,bins.m, round.mult = 10, exact = F){ ## if a matrix is given all explicit finite extremes should be given
  if(length(dim(bins.m)) == 0){
    if(max(bins.m) <= max(v) & exact==F){
      bins.m = c(bins.m, ceiling(max(v)/round.mult)*round.mult)
    }
    AA = unlist(lapply(c(2:length(bins.m)), function(x) length(which(v>=bins.m[x-1]&v<(bins.m[x])))))
    BB = cbind(bins.m[1:(length(bins.m)-1)], (bins.m[2:length(bins.m)]-1),AA)
  }else{
    AA = apply(bins.m, 1, function(x) length(which(v>=x[1]&v<=x[2])))
    BB = cbind(bins.m,AA)
  }
  return(BB)
}


proper.ddistr = function(dd,data,param){
  if(dd == "weibull"){
    xx = dweibull(data,scale = param[[1]],shape = param[[2]])
  }else if(dd == "nbinom"){
    xx =  dnbinom(data,size = param[[1]], mu=param[[2]])
  }else if(dd == "geom"){
    xx = dgeom(data,param[[1]])
  }else if(dd == "pois"){
    xx = dpois(data,param[[1]])
  }else if(dd == "norm"){
    xx = dnorm(data,mean = param[[1]], sd= param[[2]])
  }else if(dd == "gamma"){
    xx = dgamma(data,shape = param[[1]], rate =  param[[2]])
  }else if(dd == "lnorm"){
    xx = dlnorm(data, meanlog = param[[1]], sdlog =  param[[2]])
  }else if(dd == "logis"){
    xx = dlogis(data, location = param[[1]], scale =  param[[2]])
  }
  return(xx)
}

proper.pdistr = function(dd,data,param){
  if(dd == "weibull"){
    xx = pweibull(data,scale = param[[1]],shape = param[[2]])
  }else if(dd == "nbinom"){
    xx =  pnbinom(data,size = param[[1]], mu=param[[2]])
  }else if(dd == "geom"){
    xx = pgeom(data,param[[1]])
  }else if(dd == "pois"){
    xx = ppois(data,param[[1]])
  }else if(dd == "norm"){
    xx = pnorm(data,mean = param[[1]], sd= param[[2]])
  }else if(dd == "gamma"){
    xx = pgamma(data,shape = param[[1]], rate =  param[[2]])
  }else if(dd == "lnorm"){
    xx = plnorm(data, meanlog = param[[1]], sdlog =  param[[2]])
  }else if(dd == "logis"){
    xx = plogis(data, location = param[[1]], scale =  param[[2]])
  }
  return(xx)
}
proper.rdistr = function(nel,dd,param){
  if(dd == "weibull"){
    xx = rweibull(nel,scale = param[[1]],shape = param[[2]])
  }else if(dd == "nbinom"){
    xx =  rnbinom(nel,size = param[[1]], mu=param[[2]])
  }else if(dd == "geom"){
    xx = rgeom(nel,param[[1]])
  }else if(dd == "pois"){
    xx = rpois(nel,param[[1]])
  }else if(dd == "norm"){
    xx = rnorm(nel,mean = param[[1]], sd= param[[2]])
  }else if(dd == "gamma"){
    xx = rgamma(nel,shape = param[[1]], rate =  param[[2]])
  }else if(dd == "lnorm"){
    xx = rlnorm(nel,meanlog = param[[1]], sdlog =  param[[2]])
  }else if(dd == "logis"){
    xx = rlogis(nel,location = param[[1]], scale =  param[[2]])
  }
  return(xx)
}

param.inistart = function(dd,num.def){
  if(dd == "weibull"){
    xx = list(scale = num.def[1], shape = num.def[2])
  }else if(dd == "nbinom"){
    xx =  list(size = num.def[1], mu=num.def[2])
  }else if(dd == "geom"){
    xx = list(prob=num.def[1])
  }else if(dd == "pois"){
    xx =list(lambda = num.def[1])
  }else if(dd == "norm"){
    xx = list(mean = num.def[1], sd= num.def[2])
  }else if(dd == "gamma"){
    xx = list(shape = num.def[1], rate =  num.def[2])
  }else if(dd == "lnorm"){
    xx = list(meanlog = num.def[1], sdlog =  num.def[2])
  }else if(dd == "logis"){
    xx = list(location = num.def[1], scale =  num.def[2])
  }
  return(xx)
}

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

## main function
fitDistr <- function(obs, distr.names = c("norm","gamma", "weibull","lnorm","logis",
                                          "nbinom", "geom","pois"), 
                     step.fit=1, 
                     nboots = 1001, title.data.x = "N of days", 
                     outputdir, # set to NA if you don't want the plot
                     zero.augm=0, inicond = c(1,1)){
  k="mle"
  obs0 = obs
  estim.param = list()
  estim.res = list()
  if(!is.na(outputdir)){
    pdf(outputdir,width=12,height=8)
  }
  par(mfrow=c(3,2))
  for(i in 1:length(distr.names)){
    dd = distr.names[i]
    
    if(dd %in% c("gamma", "weibull")){
      if(length(which(obs == 0)) > 0){
        obs[which(obs == 0)] = zero.augm
      }
    }
    base.funzione = seq(0, max(obs, na.rm = T)+step.fit, by=step.fit)
    n.obs = length(obs)
    dist.data = putinbins(obs, base.funzione, 
                          round.mult = 1, exact = T)
    fit_g = NULL
    nt = 1
    init.cond = param.inistart(dd, inicond)
    while(length(fit_g) == 0){
      tryCatch({fit_g  <- fitdist(obs, dd, discrete = T, method=k, start = init.cond)
      },error=function(e){cat("ERROR :",i, nt,conditionMessage(e), "\n")})
      nt = nt + 1
      if(nt%%2 == 0 & length(init.cond) > 1){
        init.cond[[2]] = runif(1,init.cond[[2]], (init.cond[[2]] + 0.5))
      }else{
        init.cond[[1]] = runif(1,init.cond[[1]], init.cond[[1]] + 0.5)
      }
      if(nt >= 10){
        break
      }
    }
    
    if(length(fit_g) > 0){
      bootsample = bootdist(fit_g, niter = nboots)
      estim.res[[i]] = fit_g
      
      all.estim = matrix(unlist(lapply(c(1:nrow(bootsample$estim)), function(x) 
        proper.ddistr(dd,base.funzione,param = as.list(bootsample$estim[x,])))),
        ncol = length(base.funzione),byrow = T)
      if(sum(is.na(all.estim))>0){
        tmp = which(apply(all.estim,1,function(x) sum(is.na(x)) > 0))
        all.estim = all.estim[-tmp,]
      }
      estim.quant = apply(all.estim,2,function(x) quantile(x, probs = c(0.025,0.5,0.975), na.rm=T))
      estim.mean = apply(all.estim,2,function(x) mean(x, na.rm=T))
      
      plot(dist.data[,1],dist.data[,3]/sum(dist.data[,3]), type="h",
           ylim=c(0,max(max(estim.quant[is.finite(estim.quant)]),
                        dist.data[,3]/sum(dist.data[,3]))), 
           xlim = c(0,max(dist.data[,2])), main = dd, #paste(dd,":", k), 
           sub = paste(c(names(fit_g$estimate), "AIC"), 
                       c(round(fit_g$estimate,2), round(fit_g$aic,1)), 
                       collapse = ", "),
           xlab = title.data.x, ylab = "Frequency")
      base.graph = base.funzione
      polygon(c(base.graph,base.graph[length(base.graph):1]), 
              c(estim.quant[1,], estim.quant[3,ncol(estim.quant):1]),
              col = add.alpha("pink",0.6), border = NA)
      lines(seq(zero.augm,max(base.funzione)+1), estim.mean, col="red")
      estim.param[[i]] = fit_g
      estim.res[[i]] = estim.mean
    }else{
      estim.param[[i]] = NA
      estim.res[[i]] = NA
    }
    
  }
  if(!is.na(outputdir)){
    dev.off()
  }
  names(estim.param) = distr.names
  names(estim.res) = distr.names
  
  return(list(dist = estim.param, mean.estim = estim.res))
}

#### Exporting summary statistics ####

## assist functions
my_n <- function(data, full_data, ...) {
  my_n <- nrow(data)
  my_p <- round(100*my_n/nrow(full_data))
  dplyr::tibble(
    my_n = my_n,
    my_p = my_p
  )
}

my_weightsum <- function(data, full_data, ...) {
  weighted_n <- sum(data$weight, na.rm = TRUE)
  weighted_p <- round(100*weighted_n/sum(full_data$weight, na.rm = T))
  dplyr::tibble(
    weighted_n = weighted_n,
    weighted_p = weighted_p
  )
}

mean_iqr <- function(numerator, weighted, iqr.lim = c(0.25,0.75)) {
  if(weighted){
    function(data, ...) {
      # dots <- list(...)
      # dots <- dots[-which(names(dots) == "full_data")]
      # # Only added to print the content of `dots`
      # print(dots)
      
      # mean_contacts <- mean(data[[numerator]]*data$weight, na.rm = T)
      # iqr_contacts <- quantile(data[[numerator]]*data$weight, iqr.lim, na.rm = T)
      # Define survey design
      survey_design <- svydesign(ids = ~1, data = data, weights = ~weight)
      # Calculate the weighted mean
      mean_contacts <- svymean(as.formula(paste0("~", numerator)), 
                               survey_design, na.rm = TRUE)
      # # Calculate the weighted quantiles (IQR)
      iqr_contacts <- svyquantile(as.formula(paste0("~", numerator)),
                                  survey_design, iqr.lim, na.rm = TRUE,
                                  ci= FALSE)
      
      # Extract results
      mean_value <- as.numeric(coef(mean_contacts)) # Weighted mean
      iqr_values <- as.numeric(coef(iqr_contacts))  # Weighted 25th and 75th percentiles
      ci_values <- as.numeric(confint(mean_contacts))
      dplyr::tibble(
        mean_contacts = mean_value,
        iqr.low = iqr_values[1],
        iqr.high = iqr_values[2],
        ci.low = ci_values[1],
        ci.high = ci_values[2]
      )
    }
  }else{
    function(data, ...) {
      mean_contacts <- mean(data[[numerator]], na.rm = T)
      iqr_contacts <- quantile(data[[numerator]], iqr.lim, na.rm = T)
      dplyr::tibble(
        mean_contacts = mean_contacts,
        iqr.low = iqr_contacts[1],
        iqr.high = iqr_contacts[2]
      )
    }
  }
}

mean_p <- function(numerator, denom = "total.contacts") {
  function(data, ...) {
    
    # Define survey design
    survey_design <- svydesign(ids = ~1, data = data, weights = ~weight)
    # Calculate the weighted mean
    mean_contacts <- svymean(as.formula(paste0("~", numerator)), 
                             survey_design, na.rm = TRUE)
    mean_contacts_2 <- svymean(as.formula(paste0("~ ", denom)), 
                               survey_design, na.rm = TRUE)
    
    # Extract results
    mean_value <- as.numeric(coef(mean_contacts)) # Weighted mean
    mean_value_2 <- as.numeric(coef(mean_contacts_2))
    
    
    dplyr::tibble(
      mean_contacts = mean_value,
      weighted_p = round(100*mean_value/mean_value_2)
      
    )
  }
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

## main function
my.sumtb.func <- function(mydata, 
                          vars, variable_labels = NULL,
                          combinations, 
                          which.tables = c(1,2,3),
                          external.tbl = NA,
                          tables.formats = list(n = "{my_n} ({my_p}%)",
                                                weighted.n = "{weighted_n} ({weighted_p}%)",
                                                means = "{mean_contacts} ({iqr.low} - {iqr.high})"),
                          iqr = c(0.25,0.75),
                          patterns = c(""),
                          # spanning.header = F, 
                          indent = NULL, ## either NULL or a list with variable to indent by level (element 1 = 1 addition indentation level)
                          layer = NULL
){
  
  if(length(variable_labels) == 0){
    variable_labels = as.list(vars)
    names(variable_labels) = variable_labels
  }
  
  if(1 %in% which.tables){
    summary_table = mydata %>%
      tbl_custom_summary(
        include = all_of(vars),
        by = NULL, # Do not group by a variable
        stat_fns = ~ my_n,
        statistic = ~ tables.formats$n,
        digits = everything() ~ c(0, 0),
        missing = "ifany",
        missing_text = "(Unknown)",
        missing_stat = tables.formats$n,
        overall_row = TRUE,
        overall_row_label = "Total Population",
        label = variable_labels # Add labels here
      ) |>
      bold_labels()|>
      modify_header(all_stat_cols() ~ "**N obs**")
    #print(summary_table)
  }else{
    summary_table =  NULL
  }
  
  if(2 %in% which.tables){
    summary_table2 = mydata |>
      tbl_custom_summary(
        include = all_of(vars),
        by = NULL,
        stat_fns = ~ my_weightsum,
        statistic = ~ tables.formats$weighted.n,
        digits = everything() ~ c(0, 0),
        missing = "ifany",
        missing_text = "(Unknown)",
        missing_stat = tables.formats$weighted.n,
        overall_row = TRUE,
        overall_row_label = "Total Population",
        label = variable_labels # Add labels here
      ) |>
      bold_labels()|>
      modify_header(all_stat_cols() ~ "**Weighted N**")
    #print(summary_table2)
  }else{
    summary_table2 = NULL
  }
  
  if(3 %in% which.tables){
    listnames = gsub("\\.|_n |_", " ",
                     gsub("FALSE", "unweighted", gsub("TRUE", "weighted",
                                                      paste(combinations[,1], 
                                                            combinations[,2], 
                                                            sep = " "))))
    
    summary_table3 =  lapply(c(1:nrow(combinations)), function(i)
      mydata |>
        tbl_custom_summary(
          include = all_of(vars),
          by = NULL,
          stat_fns = everything() ~ mean_iqr(as.character(combinations[i,1]),
                                             as.logical(combinations[i,2])),
          statistic = everything() ~ tables.formats$means,
          digits = everything() ~ c(1, 1),
          missing = "ifany",
          missing_text = "(Unknown)",
          missing_stat = "-",
          overall_row = TRUE,
          overall_row_label = "Total Population",
          label = variable_labels # Add labels here
        ) |>
        bold_labels()|>
        modify_header(all_stat_cols() ~ paste0("**", listnames[i], "**"))
    )
    
    names(summary_table3) = listnames
    # Combine the two tables into one, where each is a column
  } else {
    summary_table3 = NULL
  }
  
  combined_tbl = unlist(lapply(patterns, function(f)
    lapply(unique(combinations$weighted), function(x){
      if(f == ""){
        idx = intersect(which(combinations$weighted == as.character(x)),
                        grep(paste0(patterns[2:3], collapse = "|"),
                             as.character(combinations$myvariables),
                             invert = T))
      }else{
        idx = intersect(which(combinations$weighted == as.character(x)),
                        grep(f, as.character(combinations$myvariables)))
      }
      
      if(sum(is.na(external.tbl)) > 0){
        tbls = unlist(list(list(summary_table,
                                summary_table2)[intersect(c(1,2), which.tables)],
                           summary_table3[3 %in% which.tables][idx]),
                      recursive = F)
      }else{
        tbls = c(unlist(list(list(summary_table,
                                summary_table2)[intersect(c(1,2), which.tables)],
                           summary_table3[3 %in% which.tables][idx]),
                      recursive = F), external.tbl)
      }
      
      merged_tbl = tbl_merge(tbls = tbls, 
        tab_spanner = F # Optional: Add headers
      )
      # Access and modify the table body directly to remove headers
      # if (sum(!is.na(merged_tbl$table_styling$spanning_header)) > 0 &
      #     !spanning.header) {
      #   merged_tbl$table_styling$spanning_header = NA
      # }
      
      ###needs top be added to each table function before the summary -> move into a separate function
      if(length(indent) > 0){
        for(il in length(indent)){
          rows_to_indent = which(merged_tbl$table_body$variable == indent[[il]])
          
          # Increase indent_level for those rows
          merged_tbl$table_body$label[rows_to_indent] =
            paste0(rep(" ", 4*il), merged_tbl$table_body$label[rows_to_indent])
          
          rows_to_indent_lv = intersect(rows_to_indent,
                                        which(merged_tbl$table_body$row_type == "level"))
          merged_tbl$table_body$label[rows_to_indent_lv] =
            paste0(rep(" ", 4*il), merged_tbl$table_body$label[rows_to_indent_lv])
        }
        
      }
      if(length(layer) > 0){
        layered.vars = unique(merged_tbl$table_body$variable[grep(layer, merged_tbl$table_body$variable)])
        layer.structure = lapply(layered.vars, function(ksi){
          strsplit(gsub("[0-9]","",ksi),"_")[[1]]})
        layer.structure[['main']] = which(sapply(layer.structure, function(ksi)
          length(ksi) == 1))
        layer.structure[['subset']] = setdiff(c(1:length(layered.vars)), layer.structure[['main']])
        
        df.subset = merged_tbl$table_body[which(merged_tbl$table_body$variable %in%
                                                  layered.vars[layer.structure[['subset']]]),]
        df.subset = df.subset[which(df.subset$row_type == "level"),]
        df.main = merged_tbl$table_body[which(merged_tbl$table_body$variable %in%
                                                layered.vars[layer.structure[['main']]]),]
        df.layered = lapply(df.main$label, function(l){
          idx.1 = which(df.main$label == l)
          idx.2 = grep(paste0("_",gsub("\\)", "\\\\)",gsub("\\(", "\\\\(",l)),"$"), df.subset$label)
          if(length(idx.2) == 0){
            return(df.main[idx.1,])
          }else{
            df.joined = rbind(df.main[idx.1,],
                              df.subset[idx.2,])
            df.joined$label = gsub(paste0("_", gsub("\\)", "\\\\)",gsub("\\(", "\\\\(",l))),
                                   "", df.joined$label)
            return(df.joined)
          }
        })
        
        df.layered = Reduce(rbind, df.layered)
        
        merged_tbl$table_body =  
          rbind(merged_tbl$table_body[which(merged_tbl$table_body$variable == ".overall"),],
                df.layered,
                merged_tbl$table_body[which(!merged_tbl$table_body$variable %in%
                                              c(".overall", layered.vars)),]
          )
      }
      
      idx.nonum = which(grepl("[a-z]|[A-Z]",substring(merged_tbl$table_body$label, 1,1)))
      merged_tbl$table_body$label[idx.nonum] =  sapply(idx.nonum, function(x)
        simpleCap(merged_tbl$table_body$label[x]))
      
      ### identifying duplicated levels
      idx.cols = which(colnames(merged_tbl$table_body) %in% c("label", "variable", "var_label"))
      idx.duplicates = which(duplicated(merged_tbl$table_body[,-idx.cols]) &
                               merged_tbl$table_body$row_type == "level")
      if(length(idx.duplicates) > 0){
        merged_tbl$table_body = merged_tbl$table_body[-idx.duplicates,]
      }
      return(merged_tbl)
    }
    )), recursive = F)
  
  return(combined_tbl)
}

#### GAM ####
my.fullgam = function(current_combination, mydata, terms_covariates,
                      y, smooth.term = "resp_age", reg.type = "poisson", k = 10,
                      myprint = F, time.step = 60){
  
  ## print progress
  
  if(myprint){
    if (!exists("next_print_time")) {
      next_print_time <- Sys.time() + time.step
    }
    
    if (Sys.time() >= next_print_time) {
      print(paste(length(current_combination), "out of", length(terms_covariates)))
      next_print_time <- next_print_time + time.step
    }
  }
  
  if(smooth.term %in% current_combination){
    current_combination = gsub(smooth.term, paste0("s(", smooth.term, ", k = ", k, ")"),
                               current_combination)
  }
  myformula = paste(y, "~" , paste0(current_combination, collapse = " + "))
  
  mod_gam = gam(as.formula(myformula),
                data = mydata,
                weights = mydata$weight,
                family = reg.type)
  
  ## Check concurvity (similar to multicollinearity) for smooth terms
  
  mod_gam$conc.m = if(sum(grepl(paste0("s\\(", smooth.term), current_combination)) > 0){
    concurvity(b = mod_gam, full = T)
  }else{estimate = 0}
  
  ## check for numerical instability or extreme collinearity based on the variance-covariance matrix
  D = diag(vcov.gam(mod_gam), names = FALSE)
  check.D = all(D>0) & sum(is.na(D)) == 0
  
  mod_gam$issues = ifelse(!check.D, "unstable", "")
  ## Check collinearity for parametric (non-smooth) terms
  
  mod_gam$collnrty = if(length(current_combination) > 1 & check.D){
    check_collinearity(mod_gam) #vcov(mod_gam))
  }else{estimate = 0}
  
  ## save results
  return(list(model = mod_gam,
              summary = summary(mod_gam)))
}

style_my_table = function(gtsummary_object) {
  # the function takes a gtsummary object and applies the modifications
  gtsummary_object %>%
    modify_table_body(
      ~ .x %>%
        # conditionally modify the 'label' column
        mutate(
          label = case_when(
            # check if 'label' contains an underscore
            str_detect(label, "_") ~ {
              # split the string by "_"
              parts = str_split(label, "_", n = 2) # limit split to 2 parts
              # for each element, reorder and capitalize the second part
              map_chr(parts, ~ paste(str_to_title(.x[2]), .x[1], sep = ": "))
            },
            # if no underscore, keep the label as is
            TRUE ~ label
          )
        ) %>%
        # arrange rows so that those without bolding come first.
        dplyr::arrange(stringr::str_starts(label, "\\*\\*"))
    )
}
#### Matrix calculation ####
calc.m.num = function(mydata, 
                      myattr, # vector of the names of the two attributes
                      setting,
                      predict.m,
                      denumer.n){
  # obtain the weighted sum of contacts per respondent-contact group
  age_matrix = mydata %>%
    filter(!is.na(!!sym(myattr[2]))) %>%
    arrange(!!sym(myattr[2])) %>%
    group_by(!!sym(myattr[2]), !!sym(myattr[1])) %>%
    summarise(weighted_sum = sum(weight*!!sym(setting), na.rm = T))
  
  # combine weighted sum with the data for predictions
  age_matrix = left_join(predict.m, age_matrix)
  
  # change NAs
  age_matrix$weighted_sum[is.na(age_matrix$weighted_sum)] = 0
  
  # combine total weighted contact sum with weighted N of people
  age_matrix = left_join(age_matrix, denumer.n)
  
  # order
  age_matrix = age_matrix[order(age_matrix[,myattr[1]], age_matrix[,myattr[2]]),]
  
  # average number of contacts for the matrix
  age_matrix$ncont = age_matrix$weighted_sum/age_matrix$n
  
  return(age_matrix)
}

#### Fix grammar ####
to_sentence_case = function(text) {
  paste0(toupper(substr(text, 1, 1)), tolower(substr(text, 2, nchar(text))))
}

#### Assortativity ####
assortativity.calc <- function(my.matrix) {
  
  my.matrix = as.matrix(my.matrix)
  
  my.matrix  = Reduce(rbind, lapply(c(1:nrow(my.matrix)), function(x)
    my.matrix[,x]/sum(my.matrix[,x], na.rm = T)
  ))
  
  Tr <- sum(diag(my.matrix), na.rm = T)
  Q = (Tr-1)/(nrow(my.matrix)-1)
  
  return(Q)
  
}

## relative ratio plots 

create_rr_plots = function(m, terms_to_plot, mytb.vars.labels, mydata,
                           manual_level, ## manually assign where to put reference level for hh_income
                           group_order, ## define order to display variables (should match variables in the spec)
                           plot_colors,
                           ## plot design
                           base_size = 12,
                           x_lab = "Relative Ratio",
                           strip_hjust = 0.5, 
                           strip_angle = 90,
                           dotlbl_vjust = -0.5, 
                           dotlbl_hjust = -0.2,
                           dot_size = 3.5,
                           errbr_height = 0.2, errbr_linewidth = 0.4,
                           grid_color = "#f0f0f0"
){
  # Extract the tidy data frame
  plot_data = get_model_data(m, 
                             type = "est",
                             transform = "exp",
                             ci.lvl = 0.95)
  
  plot_data = plot_data %>%
    mutate(
      stars = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*",
        TRUE ~ ""
      ),
      estimate_label = paste0(sprintf("%.2f", estimate), stars),
      color_group = case_when(
        estimate > 1 & p.value < 0.05  ~ "Positive (Significant)",
        estimate > 1 & p.value >= 0.05 ~ "Positive (Not Significant)",
        estimate < 1 & p.value < 0.05  ~ "Negative (Significant)",
        TRUE                          ~ "Negative (Not Significant)"
      )
    ) 
  
  ## Create data points for the reference categories
  # Identify which of the plotted variables are factors
  plotted_factors = as.character(plot_data$term)
  
  # Add a helper column to both data frames to identify the main variable group
  plot_data$group_var = sapply(plot_data$term, function(x) 
    terms_to_plot[which(sapply(terms_to_plot, function(p) grepl(p, x)))[1]])
  
  plot_data = plot_data %>%
    group_by(group_var) %>%
    mutate(lev_n = row_number()+ 0.5)
  
  # Build a data frame of the reference levels
  ref_rows = lapply(unique(plot_data$group_var), function(factor_name) {
    
    # The first level of a factor is always the reference level
    ref_level_name = m$xlevels[[factor_name]][1]
    
    # Construct a 'term' name that matches the model's naming convention
    ref_term = paste0(factor_name, ref_level_name)
    
    # Extract the factor original reference level
    orig_levels = levels(mydata[[as.character(factor_name)]])[1]
    
    # Create a one-row data frame for this reference level
    tibble(
      term = ref_term,
      estimate = 1.0,
      conf.low = 1.0,
      conf.high = 1.0,
      p.value = 1.0,
      stars = "",
      estimate_label = "ref.",      # The text to display on the plot
      lev_n = ifelse(factor_name == "hh_income", manual_level, 
                     ifelse(!is.na(as.numeric(orig_levels)), 
                            as.numeric(orig_levels), 1)),
      color_group = "Reference",     # The new color group
      group_var = factor_name
    )
  })
  ref_data = bind_rows(ref_rows)
  
  ## Combine, reorder, and relabel the full data set
  
  # Combine the coefficient data with the new reference level data
  full_plot_data = bind_rows(plot_data, ref_data) 
  
  ## Plot the results for the factor variables
  
  labels_plot = sapply(full_plot_data$term, function(xx){
    idx = which(sapply(terms_to_plot, function(y) grepl(y, xx)))
    if(length(idx) > 0){
      split.label = unlist(strsplit(gsub(terms_to_plot[idx],"", xx),"_"))
      if(length(split.label) > 1){
        label = paste0(str_to_title(split.label[length(split.label)]), ": ",
                       paste0(split.label[1:(length(split.label)-1)],
                              collapse = " "))
      }else{
        label = str_to_title(split.label)
      }
      return(label)
    }else{
      return(NULL)
    }
  })
  
  labels_plot = labels_plot[!sapply(labels_plot, is.null)]
  
  # Convert group_var to a factor with the specified order and then arrange
  full_plot_data = full_plot_data %>%
    mutate(group_var = factor(group_var, levels = group_order)) %>%
    arrange(group_var, lev_n)
  
  full_plot_data$group_name = unlist(mytb.vars.labels[match(as.character(full_plot_data$group_var),
                                                            names(mytb.vars.labels))])
  full_plot_data$group_name = factor(full_plot_data$group_name, levels = unlist(mytb.vars.labels[group_order]))
  
  # Create the final, ordered y-axis levels and labels
  y_axis_levels = rev(full_plot_data$term)
  y_axis_labels = rev(full_plot_data$term)
  names(y_axis_labels) = y_axis_levels
  
  # Populate the labels vector
  for(i in 1:length(y_axis_labels)){
    term_name = names(y_axis_labels)[i]
    if(term_name %in% names(labels_plot)){
      y_axis_labels[i] = unlist(labels_plot[term_name]) # Use original label
    } else {
      factor_name = full_plot_data$group_var[full_plot_data$term == term_name]
      y_axis_labels[i] = m$xlevels[[factor_name]][1] # Use ref level name
    }
  }
  
  full_plot_data$term = factor(full_plot_data$term, levels = y_axis_levels)
  
  
  # If you need a different font, import all the fonts installed on your computer into R's database.
  # This can take a few minutes.
  # font_import() 
  
  # Build the final plot with the combined data
  p = ggplot(full_plot_data, aes(x = estimate, y = term, color = color_group)) +
    # geoms
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                   height = errbr_height, 
                   linewidth = errbr_linewidth, 
                   na.rm = TRUE) +
    geom_point(size = dot_size, na.rm = TRUE) +
    geom_text(
      aes(label = estimate_label), 
      vjust = dotlbl_vjust, 
      hjust = dotlbl_hjust,
      show.legend = FALSE, 
      na.rm = TRUE
    ) +
    # scales, facets, labels, etc.
    scale_color_manual(
      values = plot_colors
    ) +
    scale_y_discrete(labels = y_axis_labels) +
    facet_grid(group_name ~ ., scales = "free_y", space = "free_y", switch = "y") +
    coord_cartesian(clip = "off") +
    guides(color = guide_legend(ncol = 1)) +
    labs(x = x_lab, y = NULL) +
    # theme
    theme_minimal(base_size = base_size) + # Set base font for plot
    theme(
      legend.position = "bottom",
      legend.justification = "left",
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = grid_color),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(face = "bold", hjust = strip_hjust, angle = strip_angle)
    )
  
  return(p)
  
}

## correlation comparison plot function
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
