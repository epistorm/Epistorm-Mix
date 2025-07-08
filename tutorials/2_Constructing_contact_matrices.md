# Estimating age-specific contact matrices 
In this tutorial, we will explore how to estimate age-specific contact matrices using contact patterns data. These contact matrices can then be used as an input in models that simulate epidemics, as described in [Tutorial 3](tutorials/3_Compartmental_modeling_with_contact_matrices.md).  

## Requirements
This tutorial is presented with code examples in [R](https://www.r-project.org/), but the sequence of steps would be similar in other languages like [python](https://www.python.org/). In any case, it assumes some familiarity with programming languages. 

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


## Estimating age-specific contact matrices 
### Preparing the dataset for analysis
Before we can create the contact matrices, we need to combine the respondent- and contact-level data into a single dataset, and we do this using a `left_join` function from the `dplyr` package. 
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
### Estimating the number of contacts between single-year respondent-contact age groups
To ensure that the contact matrices are representative of the target population (the US in this case), we must weight the number of participants by age. Here, we do this by using a `group-by` to group the respondent and estimate the mean of the weights to obtain the weight of the single respondent, and then we use `group_by` to group all respondents of a given age group and sum their weights to obtain the number of respondents of a given age. 
```R
age_matrix_n = combined_df %>%
  group_by(part_age, part_id) %>%
  summarise(weight = mean(weight, na.rm = TRUE)) %>%
  group_by(part_age) %>%
  summarise(n = sum(weight, na.rm = TRUE))
```

Then, we used `expand.grid` to create a dataframe containing the ages to include in our matrix for both the participants and contacts: 
```R
nd = expand.grid(part_age = c(0:max(combined_df$part_age)),
                 cnt_age = c(0:max(combined_df$part_age)))

predict.m = as.data.frame(nd)
```
Then, we estimate the weighted sum of contacts by the respondent-contact age group. (Example: the weighted sum of contacts between respondents ages 0-4 years old with contacts ages 0-4 years old). Here, we assume that `NA` is equivalent to a weighted sum of 0. Then, we divide the weighted sum of contacts by the weighted number of respondents stored in `age_matrix_n` to obtain the average number of contacts by respondent-contact age group.  
```R
# obtain the weighted sum of contacts per respondent-contact group
  age_matrix = combined_df %>%
    filter(!is.na(!!sym("cnt_age"))) %>%
    arrange(!!sym("cnt_age")) %>%
    group_by(!!sym("cnt_age"), !!sym("part_age")) %>%
    summarise(weighted_sum = sum(weight*!!sym("cnt_home"), na.rm = T))
  
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
  
```
### Using generalized additive models to predict contacts
Once we have the average number of contacts, we fit a generalized additive model (GAM) with a log link to the data where the average number of contacts is the dependent variable and the respodents age and the contacts age are the independent variables. We do this using the `gam ` function from the library `mgc`. Using a GAM allows us to capture non-linear realtionships between the respondent-contact age groups and the number of contacts they have. In the GAM, we also consider different smoothing procedures based on the setting of the contact, which were implemented using the function `te`. For contacts in workplaces, we use a thin plate regression spline which allows more flexibility in modeling complex non-linear relationships, such as those with workplace contacts. For contacts in other settings, we use a cyclic cubic smoothing spline as this method assumes the contacts to be continous and smooth across age groups. 
The GAM for contacts in the workplace is formulated as:

To continue with estimating contacts in the household, the GAM is formulated as: 
```R
model = gam(ncont ~ te(as.numeric(part_age), as.numeric(cnt_age),
                        k = c(10,10), bs = c("cc","cc"), m = c(0,0)),
                family = nb(),
                data = age_matrix)
```
That model would also be used for estimating contacts in schools and other settings. To estimate contacts in the workplace, the GAM would need to be formulated as: 
```R
model = gam(ncont ~ te(as.numeric(part_age), as.numeric(cnt_age),
                        k = c(4,4), bs = c("tp","tp"), m = c(0,0)),
                family = nb(),
                data = age_matrix)
```

Then, we predict the number of contacts by respondent-contact age group
```R
 age_matrix$pred = predict(model, newdata = age_matrix, type = "response")
```

### Visualizing the contact matrix
To visualize the age-specific household contact matrix, we plot the predicted average number of contacts by respondent-contact age using the `geom_raster` function where the x-axis is the age of the respondent and the y-axis is the age of the contact. Using the `scale_fill_gradient` function, we specify the colors to use to fill the plot, and the order of the colors indicates that as the numbers increase, the gradient moves toward "white". The code below shows the specification of plot.

```R
age_plot = ggplot(age_matrix) +
    geom_raster(aes(x = part_age, y = cnt_age, fill = pred), interpolate = TRUE) +
    ggtitle("Setting: Household") + # Main title
    xlab("Respondent Age") + # X-axis title
    ylab("Contact Age") + # Y-axis title
    scale_fill_gradientn(colors = c("#005f87", "#87afd7"), "white"),
                         values = c(0, 0.4, 1),
                         name = "") + # Legend title
    scale_y_continuous(breaks = c(0,20,40,60,80)) +
    scale_x_continuous(breaks = c(0,20,40,60,80)) +
    coord_fixed(ratio = 1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent", color = "black"),
          plot.background = element_rect(fill = "transparent", color = NA)

age_plot
```
![household-CM](https://github.com/user-attachments/assets/61ee6a64-8c29-4c6e-8738-924eb15dcc79)

### Estimating the number of contacts between 5-year respondent-contact age groups
Many times, you will opt for creating contact matrices using age groups rather than single-year ages. For the remainder of this tutorial, we will create the age-specific household contact matrix using 5-year age groups. 

To do this, we need to specify the age groups for both the respondents and their contacts.
```R
age.breaks = list("_by5_80" = c(seq(0,80, by = 5), 120))


for(i in 1:length(age.breaks)){
  resp_imp[, paste0("part_age", names(age.breaks)[i])] = cut(resp_imp$part_age, 
                                                             breaks = age.breaks[[i]],
                                                             right = F)
  
  cont_imp[, paste0("cnt_age", names(age.breaks)[i])] = cut(cont_imp$cnt_age, 
                                                            breaks = age.breaks[[i]],
                                                            right = F)
}
```
Respondent specific weights, weighted sum of contacts between respondent-contact age groups, and estimating the average number of contacts between groups follows the same procedures shown previously. For this example, we will reference these average contacts by age groups as `age.matrix.by5`. It is important to note that for the 5-year age-specific contact matrices, we do not use a GAM to predict contacts as we do not need to smooth the estimates.  After `age.matrix.by5` is obtained, we will use the `pivot_wider` function to convert this into a matrix by respondent-contact age groups. 
```R
 matrix_age = age.matrix.by5 %>% 
    select("part_age_by5_80", "cnt_age_by5_80", "ncont") %>% 
    pivot_wider(names_from = "cnt_age_by5_80", values_from = "ncont")
```
The resulting 5-year age-specific contact matrix is what will be used in [Tutorial 3](tutorials/3_Compartmental_modeling_with_contact_matrices.md) to demonstrate how contact matrices can be used in compartmental models. 

### Using `socialmixr` to estimate 5-year age-specific contact matrices
Alternatively, contact matrices can be estimated using the `contact_matrix` function in the `socialmixr` R package. Once you have loaded the data from Zenodo following the example in [Tutorial 1](tutorials/1_Accessing_and_extracting_data.md), we can estimate the contact matrices following the syntax below:
```R
contacts = contact_matrix(epistorm_mix,  age.limits = seq(0, 80, by = 5))
```
This function produces a two-element list. The first element is the 5-year age-specific contact matrix: 
```R
contacts$matrix

#               contact.age.group
# age.group     [0,5)    [5,10)   [10,15)   [15,20)   [20,25)   [25,30)   [30,35)   [35,40)   [40,45)   [45,50)   [50,55)   [55,60)
# [0,5)   
# [5,10) 
# [10,15) 
# [15,20) 
# [20,25)
# [25,30) 
...
```
The second element is a dataframe containing the frequency and proportion of participation by 5-year age group:
```R
contacts$participants

#       age.group   participants  proportion
#       <char>      <int>         <num>
# 1:      [0,5)          
# 2:     [5,10)         
# 3:    [10,15)          
# 4:    [15,20)          
# 5:    [20,25)          
# 6:    [25,30)          
...      
```

### Full code
You can check out the full code for this tutorial here: [tutorial_code.r](../code/contact_matrices/tutoral_code.r)
