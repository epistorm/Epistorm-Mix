# Accessing and extracting contact patterns data

In this tutorial, we demonstrate how to access contact patterns data and extract it for analysis using two methods:
- Manual download ("Downloading from GitHub")
- Direct download from R/RStudio ("Extracting data directly in RStudio")

This tutorial is designed for those with no previous experience with GitHub. Downloaded data can be used to estimate contact matrices (see [Tutorial 2](../data-analysis/2_Constructing_contact_matrices.md)), plot contact matrices (see [Tutorial 3](../data-analysis/3_Visualizing_contact_matrices.md)), and develop epidemic models for infectious disease spread (see [Tutorial 4](../mathematical-modeling/4_Compartmental_modeling_with_contact_matrices.md)).

This tutorial uses the data specifically created for these tutorials ([here](./tutorials/data_analysis/data)) and is intended as a learning resource. To reproduce the analysis on the full dataset, please use the code provided [here](.../code) and the data provided [here](.../data).

## Requirements

This tutorial is presented with code examples in [R](https://www.r-project.org/), but the sequence of steps would be similar in other languages like [python](https://www.python.org/). In any case, it assumes some familiarity with programming languages.

## Downloading the contact patterns data

### Downloading from GitHub

To download the data files for this tutorial from GitHub, go to the data folder of the GitHub repository by clicking [here](./data). Within this folder, click on the file you want to download. Then, press the download button (<img width="34" height="34" src="https://github.com/user-attachments/assets/aeebfa19-f357-463f-b453-7b0f39b5443b" />) at the top right-hand side of the file, as shown below:

<img src="./figures/extract-data1.png" width=100% >

This will automatically save the data file into the downloads folder on your computer, unless otherwise specified. It is recommended that you move this file from your download folder into a folder specifically for the analysis you are doing.

### Loading the downloaded file in R

Once you have downloaded the data and saved it into your project folder, you can load the data into your R program:

``` r
resp_imp = read.csv("imputed_resp.csv")
head(resp_imp)

# resp_id resp_age resp_gender resp_educator    weight
# 163925       67      Female            No   1.2019131
# 210129       64        Male            No   0.7475523
# 218353       52      Female            No   1.0751714
# 269795       54        Male            No   1.3448021
# 325231       15        Male            No   0.9484297
# 344409        3      Female            No   0.4348620
```

## Extracting data directly in Rstudio

Alternatively to downloading the contact patterns data, you can access the data directly in R.

### Extracting data from `github`

To extract the data from `github` directly, you will need to retrieve the url of the raw data file. To obtain the url for the raw data on `github`, go to the data folder of the `github` repository by clicking [here](https://github.com/allisandrak/Epistorm-Mix/tree/main/data). In this folder, click on the file you want to use for analysis. Then, press the `Raw` button at the top right-hand side of the file, as shown in the figure above.

Copy the url for the Raw data file. Note that the url should start with `https://raw.githubusercontent.com/...`, and if this is not the case then reading the data into R directly will not work. To load the data into your R program, simply use the `read.csv` function using the url instead of a file path:

``` r
resp.url = "https://raw.githubusercontent.com/allisandrak/Epistorm-Mix/refs/heads/main/tutorials/data-analysis/data/imputed_resp.csv?token=GHSAT0AAAAAADJQZFA53RC5T5BAEGBH7FGK2G375TA"
resp_imp = read.csv(resp.url)
```

## Full code

You can check out the full code for this tutorial here: [accessing_and_extracting_data.r](../data-analysis/code/accessing_and_extracting_data.R)
