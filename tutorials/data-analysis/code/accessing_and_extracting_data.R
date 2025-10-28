## set your project folder as your working directory
setwd("./Epistorm-Mix/") # change the string in quotes to your working directory/project folder

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

## Extracting data from GitHub directly 

## save the URL
resp.url = "https://raw.githubusercontent.com/.../data/imputed_resp.csv?token=GHSAT0AAAAAADJQZFA53RC5T5BAEGBH7FGK2G375TA" ## paste the copied URL here

## read using saved URL 
resp_imp = read.csv(resp.url)








