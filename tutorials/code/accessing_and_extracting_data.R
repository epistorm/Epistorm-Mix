
# Loading data downloaded from github
resp_imp = read.csv("imputed_part_reform.csv")
head(resp_imp)


# part_id part_age part_gender part_educator    weight
# 163925       67      Female            No   1.2019131
# 210129       64        Male            No   0.7475523
# 218353       52      Female            No   1.0751714
# 269795       54        Male            No   1.3448021
# 325231       15        Male            No   0.9484297
# 344409        3      Female            No   0.4348620


## Extracting data from github directly (PATCH: CHANGE LINK)
resp.url = "https://raw.githubusercontent.com/username/Epistorm-Mix/refs/heads/main/code/contact_matrices/data/imputed_part_reform.csv?token=GHSAT0AAAAAADF2WTHCJL7GXM2KUORRZH5C2DNHBBA"
resp_imp = read.csv(resp.url)









