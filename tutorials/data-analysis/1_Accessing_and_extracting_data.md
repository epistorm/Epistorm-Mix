# Accessing and extracting contact patterns data

In this tutorial, we demonstrate how to access contact patterns data and extract it for analysis using two methods:
- Manual download ("Downloading from GitHub")
- Direct download from R/RStudio ("Extracting data directly in RStudio")

This tutorial is designed for those with no previous experience with GitHub. Downloaded data can be used to estimate contact matrices (see [Tutorial 2](../data-analysis/2_Constructing_contact_matrices.md)), plot contact matrices (see [Tutorial 3](../data-analysis/3_Visualizing_contact_matrices.md)), and develop epidemic models for infectious disease spread (see [Tutorial 4](../mathematical-modeling/4_Compartmental_modeling_with_contact_matrices.md)).

This tutorial uses the data specifically created for these tutorials ([here](./tutorials/data_analysis/data)) and is intended as a learning resource. To reproduce the analysis on the full dataset, please use the code provided [here](.../code) and the data provided [here](.../data).

## Requirements

This tutorial is presented with code examples in R and assumes that both [R](https://www.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/) (or an alternative, like Jupiter Notebook) are installed. It also assumes some familiarity with programming languages. It is important to note that the sequence of steps would be similar in other languages (e.g., [python](https://www.python.org/)).

## Downloading from GitHub

To download the data files for this tutorial from GitHub, go to the data folder of the GitHub repository by clicking [here](./data). Within this folder, click on the file you want to download. Then, press the download button (<img width="24" height="24" src="https://github.com/user-attachments/assets/aeebfa19-f357-463f-b453-7b0f39b5443b" />) at the top right-hand side of the file, as shown below:

<img src="./figures/extract-data1.png" width=100% >

This will automatically save the data file into the pre-set folder for downloaded files on your computer, unless you specify a different location. It is recommended that you move this file into a folder specifically created for this tutorial - your project folder.

### Loading the downloaded file in R

Once you have downloaded the data and saved it in your project folder, you can load the data into your R environment (we recommend using RStudio):

``` r
## set your project folder as your working directory
setwd("project folder address")

## read downloaded data
resp_imp = read.csv("imputed_resp.csv")

## see a subsample of the data
head(resp_imp)

# resp_id resp_age resp_gender resp_educator    weight
# 163925       67      Female            No   1.2019131
# 210129       64        Male            No   0.7475523
# 218353       52      Female            No   1.0751714
# 269795       54        Male            No   1.3448021
# 325231       15        Male            No   0.9484297
# 344409        3      Female            No   0.4348620
```

## Extracting data directly in RStudio

Alternatively, you can access the data directly in R using a web link to the file in the GitHub repository. This assumes that the repository is public (this repository is) and you have the authorization to access the selected file.

To extract the data from GitHub directly into your R environment, you will need to retrieve the URL of the data file. To obtain the URL, go to the data folder of the GitHub repository by clicking [here](./data). In this folder, click on the file you want to use for analysis. Then, press the `Raw` button at the top right-hand side of the file, as shown in the figure above.

Copy the URL for the Raw data file. Note that the URL should start with `https://raw.githubusercontent.com/...`. If this is not the case, reading the data into R directly will not work. Once you have the correct URL, simply use the `read.csv` function with the URL instead of a file path:

``` r
## save the file URL
resp.url = "https://raw.githubusercontent.com/.../data/imputed_resp.csv?token=GHSAT0AAAAAADJQZFA53RC5T5BAEGBH7FGK2G375TA" ## paste the copied URL here

## read using save URL 
resp_imp = read.csv(resp.url)
```

## Full code

You can check out the full code for this tutorial here: [accessing_and_extracting_data.r](../data-analysis/code/accessing_and_extracting_data.R)
