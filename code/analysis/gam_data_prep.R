#### Create dataset for GAM regression ####

### Encode Variables [same as in GAM code] #### 
## include only groups that have at least 10 observations
sum.res3.gam = sum.res3[which(sum.res3$resp_resp_sick != "Refused" &
                                sum.res3$resp_sex != "Other" &
                                sum.res3$resp_hhm_sick != "Refused"),]

## relevel the income variable
sum.res3.gam$hh_income = relevel(sum.res3.gam$hh_income, ref = "$150,000 or more")

## relevel and removed empty levels for "Sick"
sum.res3.gam$resp_resp_sick = relevel(sum.res3.gam$resp_resp_sick, ref = "No")
sum.res3.gam$resp_resp_sick = droplevels(sum.res3.gam$resp_resp_sick)

## relevel and removed empty levels for "Sex"
sum.res3.gam$resp_sex = factor(sum.res3.gam$resp_sex, 
                               levels = c("Male", "Female"))
sum.res3.gam$resp_sex = droplevels(sum.res3.gam$resp_sex)

## removed empty levels for "nothome"
sum.res3.gam$nothome = droplevels(sum.res3.gam$nothome)

## drop empty levels in interaction variables
if("ppwork_nothome" %in% colnames(sum.res3.gam)){
  sum.res3.gam$ppwork_nothome = droplevels(sum.res3.gam$ppwork_nothome)
}

## relevel age groups
if("resp_age_by5_80" %in% colnames(sum.res3.gam)){
  sum.res3.gam$resp_age_by5_80  = relevel(sum.res3.gam$resp_age_by5_80, ref = "15-19")
}
if("resp_age_insightnet" %in% colnames(sum.res3.gam)){
  sum.res3.gam$resp_age_insightnet  = relevel(sum.res3.gam$resp_age_insightnet, ref = "10-17")
}

## relevel hhousehold size
if("hhsize2" %in% colnames(sum.res3.gam)){
  sum.res3.gam$hhsize2  = relevel(sum.res3.gam$hhsize2, ref = "3")
}