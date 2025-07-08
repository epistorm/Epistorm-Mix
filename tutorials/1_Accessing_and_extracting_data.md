# Accessing and extracting contact patterns data
In this tutorial, we demonstrate how to access contact patterns data and extract it for analysis. This data can then be used to estimate contact matrices (see [Tutorial 2](tutorials/2_Constructing_contact_matrices.md)) and develop epidemic models for infectious disease spread (see [Tutorial 3](tutorials/3_Compartmental_modeling_with_contact_matrices.md)). 

Epistorm-Mix data are stored in `.csv` files following a standard format established for social contact patterns data, allowing the Epistorm-Mix data to be compatible with R packages designed for analyzing social contact patterns data. This data is publicly available through `github` and `Zenodo`. 

## Requirements
This tutorial is presented with code examples in [R](https://www.r-project.org/), but the sequence of steps would be similar in other languages like [python](https://www.python.org/). In any case, it assumes some familiarity with programming languages. 

In this tutorial, we will be using the following R packages: 
- [`socialmixr`](https://cran.r-project.org/web/packages/socialmixr/vignettes/socialmixr.html) for accessing and loading data from Zenodo. 


## Accessing data through `github` 
### Downloading the data
To download the data files from `github`, go to the data folder of the `github` repository by clicking [here](https://github.com/allisandrak/Epistorm-Mix/tree/main/data). Within this folder, click on the file you want to download for analysis. Then, press the download button at the top right-hand side of the file, as shown below:

![Screenshot 2025-07-08 at 10 38 35 AM](https://github.com/user-attachments/assets/7cebea7d-a61b-4bb1-9c4f-b0898f8c751f)

This will automatically save the data file into the downloads folder on your computer, unless otherwise specified. It is recommended that you move this file from your download folder into a folder specifically for the analysis you are doing. 

### Loading the downloaded file in R
Once the file is downloaded and saved into your project folder, you can load the data into your R program:
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

### Access directly in R
Alternatively, you can access the data directly in R by retrieving the url of the raw data file.To obtain the url for the raw data on `github`, go to the data folder of the `github` repository by clicking [here](https://github.com/allisandrak/Epistorm-Mix/tree/main/data). In this folder, click on the file you want to use for analysis. Then, press the `Raw` button at the top right-hand side of the file, as shown below:

![Screenshot 2025-07-08 at 11 33 56 AM](https://github.com/user-attachments/assets/c70a0093-bf97-4d15-9fbc-917da1648d1c)

Copy the url for the Raw data file. Note that the url should start with `https://raw.githubusercontent.com/...`, and if this is not the case then reading the data into R directly will not work. To load the data into your R program, simply use the `read.csv` function using the url instead of a file path: 
```R
resp.url = "https://raw.githubusercontent.com/username/Epistorm-Mix/refs/heads/main/code/contact_matrices/data/imputed_part_reform.csv"
resp_imp = read.csv(resp.url)
```


## Accessing data through Zenodo
[Zenodo](https://zenodo.org/) is an open-source data repository built and developed by reasearchers that allows researchers to share and access data. Inside Zenodo, there is a section specifically for [contact patterns data](https://zenodo.org/communities/social_contact_data/records?q=&l=list&p=1&s=10&sort=newest) where data collected from social contact surveys conducted all over the world, including our [Epistorm-Mix data](), are available.

### Downloading the data
###
### R package socialmixr

