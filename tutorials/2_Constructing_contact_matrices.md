# Estimating age-specific contact matrices 
In this tutorial, we will explore how to estimate age-specific contact matrices using contact patterns data. These contact matrices can then be used as an input in models that simulate epidemics, as described in [Tutorial 3](tutorials/3_Compartmental_modeling_with_contact_matrices.md).  

## Requirements
This tutorial is presented with code examples in [R](https://www.r-project.org/), but the sequence of steps would be similar in other languages like [python](https://www.python.org/). In any case, it assumes some familiarity with programming language. 

In the code examples, we directly use the following R packages: 
- [`dplyr`](https://cran.r-project.org/web/packages/dplyr/) for efficient operations with dataframes.
- [`tidyr`](https://cran.r-project.org/web/packages/tidyr/) for converting between long and wide dataframes.
- [`mgcv`](https://cran.r-project.org/web/packages/mgcv/) for working with generalized additive models.
- [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/) for visualizing results.

The code was tested in Rstudio version 4.5.1. In a `.r` R code file, you can install these packages and load the libraries using:
```R
lapply(c("dplyr", "mgcv", "ggplot2", "tidyr"), function(x){
  if(!require(x, character.only = T)){
    install.packages(x)
  }
  library(x, character.only = T)
})
```
## Loading the necessary data
To estimate age-specific contact matrices, we need at least two data elements: respondent-level contact patterns data and contact-level contact patterns data.

These contact patterns data can be imported into the program by either using the `socialmixr` R package or reading a file. Accessing and extracting contact patterns data using these two methods are described in [Tutorial 1](tutorials/1_Accessing_and_extracting_data.md).

In this tutorial, we will import our data from `.csv` files stored in the working directory of the R code. The respondent-level contact patterns data should be formatted as follows:

```csv
part_id	part_age	part_gender	part_educator	weight
163925	67	Female	No	1.201913059
210129	64	Male	No	0.747552347
218353	52	Female	No	1.075171403
269795	54	Male	No	1.344802089
325231	15	Male	No	0.948429747
344409	3	Female	No	0.434861996

...
```

and can be loaded by: 
```R
resp_imp = read.csv("imputed-resp-common.csv")
```

The contact-level patterns data should be formatted as: 
```csv
part_id	cont_id	cnt_age	cnt_setting	cnt_ethn	  cnt_gender	cnt_relationship	                    cnt_home	cnt_other	cnt_school	cnt_work
163925	1066	  80	    Household	  NH White	  Male	      Person [My Child Lives/I Live] With	  TRUE	    FALSE	    FALSE	      FALSE
163925	1067	  25	    Other	      NH White	  Female	    Other	                                FALSE	    TRUE	    FALSE	      FALSE
210129	1652	  70	    Household	  NH Other	  Female	    Person [My Child Lives/I Live] With	  TRUE	    FALSE	    FALSE	      FALSE
210129	1653	  60	    Work	      NH Other	  Male	      Work Colleague	                      FALSE	    FALSE	    FALSE	      TRUE
210129	1654	  80	    Work	      NH White    Female	    Work Colleague	                      FALSE	    FALSE	    FALSE	      TRUE
210129	1655	  31	    Other	      NH Black	  Female	    Other	                                FALSE	    TRUE	    FALSE	      FALSE
...
```

and can be loaded into the program by:
```R
cont_imp = read.csv("imputed-contacts-common.csv")
```
Documentation describing the variables of both contact patterns datasets are available [here]().


## Estimating sinlge-year age-specific contact matrices 
Before we can create the contact matrices, we need to combine the respondent- and contact-level data into a single dataset. 
```R
combined_df = left_join(resp_imp, cont_imp, by = "part_id")
```
We also need to do a little preprocessing. In this analysis, we assume that contacts that are described as "Student" to respondents who have indicated that they work in a profession where they work with or around children are had in the school setting. We adjust the `cnt_school` variable to reflect this:
```R
idx.students = which(combined_df$cnt_relationship == "Student" &
                       combined_df$part_educator == "Yes")

combined_df$cnt_school[idx.students] = TRUE
```
Then, we create a new binary variable that indicates whether or not a respondent reported contacts, where `0` = no  and `1` = yes.
```R
cnt_setting = c("cnt_home", "cnt_school", "cnt_work", "cnt_other")
cont_cols = which(colnames(combined_df) %in% cnt_setting)

nocont = rowSums(!is.na(combined_df[, cont_cols])) == 0
combined_df$binary_col = ifelse(nocont, 0, 1)
```

For visualization purposes, we recode the ages of contacts and respondents that are 80 years and older as follows:
```R
combined_df$cnt_age[which(combined_df$cnt_age >= 80 &
                             combined_df$cnt_age < 85)] = 81
combined_df$part_age[which(combined_df$part_age >= 80 &
                             combined_df$part_age < 85)] = 81

combined_df$cnt_age[which(combined_df$cnt_age >= 85 &
                             combined_df$cnt_age < 90)] = 82
combined_df$part_age[which(combined_df$part_age >= 85 &
                             combined_df$part_age < 90)] = 82

combined_df$cnt_age[which(combined_df$cnt_age > 90)] = 83
combined_df$part_age[which(combined_df$part_age > 90)] = 83
```
To ensure that the contact matrices are representative of the target population (the US in this case), we must weight the number of participants by age. Here, we do this by estimating the sum of the weights for all participants of a given age. 
```R
age_matrix_n = combined_df %>%
  group_by(part_age, part_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(part_age) %>%
  summarise(n = sum(weight, na.rm = TRUE))
```

Then, we need to supply a dataframe containing the ages to include in our matrix for both the participants and contacts. We do that by: 
```R
nd = expand.grid(part_age = c(0:max(combined_df$part_age)),
                 cnt_age = c(0:max(combined_df$part_age)))

predict.m = as.data.frame(nd)
```
Now, we are ready to estimate the age-specific contact matrices. The first step requires that we estimate the weighted sum of contacts by the respondent-contact age group. (Example: the weighted sum of contacts between respondents ages 0-4 years old with contacts ages 0-4 years old). Here, we assume that `NA` is equivalent to a weighted sum of 0. Then, we divide the weighted sum of contacts by the weighted number of respondents stored in `age_matrix_n` to obtain the average number of contacts by respondent-contact age group.  
```R
# obtain the weighted sum of contacts per respondent-contact group
  age_matrix = combined_df %>%
    filter(!is.na(!!sym("cnt_age"))) %>%
    arrange(!!sym("cnt_age")) %>%
    group_by(!!sym("cnt_age"), !!sym("part_age")) %>%
    summarise(weighted_sum = sum(weight*!!sym(setting), na.rm = T))
  
  # combine weighted sum with the data for predictions
  age_matrix = left_join(predict.m, age_matrix)
  
  # change NAs
  age_matrix$weighted_sum[is.na(age_matrix$weighted_sum)] = 0
  
  # combine total weighted contact sum with weighted N of people
  age_matrix = left_join(age_matrix, age_matrix_n)
  
  # order
  age_matrix = age_matrix[order(age_matrix[,"part_age"], age_matrix[,"cnt_age"]),]
  
  # average number of contacts for the matrix
  age_matrix$ncont = age_matrix$weighted_sum/age_matrix$n
  

  age_matrix$age_diff = as.numeric(age_matrix$part_age) - as.numeric(age_matrix$cnt_age)
```
