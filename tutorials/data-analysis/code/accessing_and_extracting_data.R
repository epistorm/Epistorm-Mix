setwd("/Users/shsinc/Desktop/phd/Epistorm-survey/local-git/Epistorm-Mix/")

# Loading data downloaded from github
resp_imp = read.csv("./tutorials/data-analysis/data/imputed_resp.csv")
head(resp_imp)

# resp_id resp_age resp_gender resp_educator    weight
# 163925       67      Female            No   1.2019131
# 210129       64        Male            No   0.7475523
# 218353       52      Female            No   1.0751714
# 269795       54        Male            No   1.3448021
# 325231       15        Male            No   0.9484297
# 344409        3      Female            No   0.4348620

## Extracting data from github directly 
resp.url = "https://raw.githubusercontent.com/allisandrak/Epistorm-Mix/refs/heads/main/tutorials/data-analysis/data/imputed_resp.csv?token=GHSAT0AAAAAADJQZFA53RC5T5BAEGBH7FGK2G375TA"
resp_imp = read.csv(resp.url)









