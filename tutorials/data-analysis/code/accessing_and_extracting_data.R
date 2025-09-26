
# Loading data downloaded from github
resp_imp = read.csv("Epistorm-Mix/tutorials/data-analysis/imputed_resp.csv")
head(resp_imp)


# resp_id resp_age resp_gender resp_educator    weight
# 163925       67      Female            No   1.2019131
# 210129       64        Male            No   0.7475523
# 218353       52      Female            No   1.0751714
# 269795       54        Male            No   1.3448021
# 325231       15        Male            No   0.9484297
# 344409        3      Female            No   0.4348620


## Extracting data from github directly 
resp.url = "https://github.com/allisandrak/Epistorm-Mix/blob/872878fab849276a46ca4dc839ccd1c568d4f0bd/tutorials/data-analysis/data/imputed_resp.csv"
resp_imp = read.csv(resp.url)









