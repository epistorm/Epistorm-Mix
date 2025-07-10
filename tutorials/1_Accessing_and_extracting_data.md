# Accessing and extracting contact patterns data
In this tutorial, we demonstrate how to access contact patterns data and extract it for analysis. This data can then be used to estimate contact matrices (see [Tutorial 2](../tutorials/2_Constructing_contact_matrices.md)) and develop epidemic models for infectious disease spread (see [Tutorial 3](../tutorials/3_Compartmental_modeling_with_contact_matrices.md)). 

Epistorm-Mix data are stored in `.csv` files following a standard format established for social contact patterns data, allowing the Epistorm-Mix data to be compatible with R packages designed for analyzing social contact patterns data. This data is publicly available through `github` and `Zenodo`. 

[Zenodo](https://zenodo.org/) is an open-source data repository built and developed by reasearchers that allows researchers to share and access data. Inside Zenodo, there is a section specifically for [contact patterns data](https://zenodo.org/communities/social_contact_data/records?q=&l=list&p=1&s=10&sort=newest) where data collected from social contact surveys conducted all over the world, including our [Epistorm-Mix data](), are available.

## Requirements
This tutorial is presented with code examples in [R](https://www.r-project.org/), but the sequence of steps would be similar in other languages like [python](https://www.python.org/). In any case, it assumes some familiarity with programming languages. 

In this tutorial, we will be using the following R packages: 
- [`socialmixr`](https://cran.r-project.org/web/packages/socialmixr/vignettes/socialmixr.html) for accessing and loading data from Zenodo. 


## Downloading the contact patterns data
### Downloading from `github`
To download the data files from `github`, go to the data folder of the `github` repository by clicking [here](https://github.com/allisandrak/Epistorm-Mix/tree/main/data). Within this folder, click on the file you want to download for analysis. Then, press the download button at the top right-hand side of the file, as shown below:

![Screenshot 2025-07-08 at 10 38 35 AM](https://github.com/user-attachments/assets/7cebea7d-a61b-4bb1-9c4f-b0898f8c751f)

This will automatically save the data file into the downloads folder on your computer, unless otherwise specified. It is recommended that you move this file from your download folder into a folder specifically for the analysis you are doing. 

### Downloading from `Zenodo`
To download the data files from `Zenodo`, go to the Epistorm-Mix data by clicking [here](). Scroll down the webpage until you get to the Files section. Locate the file you want to use for analysis and download by clicking either the name of the file or the `download` button on the right-hand side: 

[INSERT SCREENSHOT CIRCLING THE DOWNLOAD BUTTON AND THE FILE NAME)

Zenodo also offers a feature that allows you to preview the file prior to downloading, which you can use by pressing the `preview` button. 

Again, the file will automatically save to the downloads folder of your computer and will need to moved to your project folder.

### Loading the downloaded file in R
Once you have downloaded the data and saved it into your project folder, you can load the data into your R program:
```R
resp_imp = read.csv("imputed_part_reform.csv")
head(resp_imp)

# part_id part_age part_gender part_educator    weight
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
To extract the data from `github` directly, you will need to retrieve the url of the raw data file. To obtain the url for the raw data on `github`, go to the data folder of the `github` repository by clicking [here](https://github.com/allisandrak/Epistorm-Mix/tree/main/data). In this folder, click on the file you want to use for analysis. Then, press the `Raw` button at the top right-hand side of the file, as shown below:

![Screenshot 2025-07-08 at 11 33 56 AM](https://github.com/user-attachments/assets/c70a0093-bf97-4d15-9fbc-917da1648d1c)

Copy the url for the Raw data file. Note that the url should start with `https://raw.githubusercontent.com/...`, and if this is not the case then reading the data into R directly will not work. To load the data into your R program, simply use the `read.csv` function using the url instead of a file path: 
```R
resp.url = "https://raw.githubusercontent.com/username/Epistorm-Mix/refs/heads/main/code/contact_matrices/data/imputed_part_reform.csv"
resp_imp = read.csv(resp.url)
```

### Extracting data from `Zenodo` using `socialmixr`
To extract data from `Zenodo`, you will need to install and load the `socialmixr` R package:
```R
if(!require("socialmixr")){
  install.packages("socialmixr")
}
library(socialmixr)
```

This package was designed specifically for accessing social contact patterns data from `Zenodo` and estimating contact matrices (see [Tutorial 2](../tutorials/2_Constructing_contact_matrices.md)). To load the Epistorm-Mix data into your program, you will first need to obtain the url for the [Epistorm-Mix data](), which you can retrieve by right-clicking the doi found on the right-hand side of the webpage and copying the link address:

[INSERT SCREENSHOT WITH CIRCLED DOI AND RIGHT CLICKING WITH COPY]

Then, using the copied link address and the `get_survey` R function, you can load the data into your program:
```R
epistorm_survey <- get_survey("https://doi.org/XXXXXXX/zenodo.XXXXXXX")
```
This function returns a list that contains the respondent-level data, contact-level data, and information needed for citing the data. 

If you want to use the survey data repeatedly, it is recommended that you save a local copy to your computer to avoid the need for a network connection and to speed up processing. You can do this by saving the dataset using the `saveRDS` function:
```R
saveRDS(epistorm_survey, "epistorm.rds")
```
Loading the saved `.rds` file into your R program can be done as follows: 
```R
epistorm_survey <- readRDS("epistorm.rds")
```

## Full code
You can check out the full code for this tutorial here: [tutorial_code.r](../code/accessing_data/tutorial_code.R)
