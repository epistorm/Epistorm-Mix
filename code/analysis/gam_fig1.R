
##### Regressions set-up ####

## set the threshold for the number of observations per group 
g.thrshld = 3

## set the upper limit for the smoothing parameter
#potential.ks = c(4:40) ## k selected based on multiple refitting of the data
smooth.k = 21

## do glm and gam regressions
gam.ys = c("total.contacts", "cont_setting_nothh_n")

terms_covariates = c("resp_age", "resp_age_by5_80", "resp_age_insightnet",
                     "ppwork2", "ppwork_nothome",
                     "resp_ethn",
                     "resp_sex", 
                     "resp_resp_sick", "resp_hhm_sick",
                     "hhsize2", "hh_income", "hh_loc_reg4",
                     "is_weekend2", "weeks","wave")

all_combinations = list()  # Initialize an empty list to store combinations
for (i in 1:length(terms_covariates)) {
  combinations_i = combn(terms_covariates, i, simplify = FALSE)
  check_multiple = sapply(combinations_i, function(x)
    sum(grepl("resp_age", x)) <= 1 & sum(grepl("ppwork", x)) <= 1)
  all_combinations = c(all_combinations, combinations_i[check_multiple])
}
## labels of the variables
mytb.vars.labels = list(resp_ethn = "Race/Ethnicity",
                        resp_sex = "Sex",
                        ppwork2 = "Main Activity",
                        ppwork_nothome = "Main Activity and Mode",
                        hh_income = "Household Income",
                        resp_age = "Age",
                        resp_age_by5_80 = "5-year Age Group",
                        resp_age_insightnet = "Ad-hoc Age Group",
                        hhsize2 = "Household Size",
                        is_weekend2 = "Day",
                        resp_resp_sick = "Sick",
                        resp_hhm_sick = "Household Member Sick",
                        hh_loc_reg4 = "Region",
                        weeks = "Survey Week",
                        wave = "Wave")

### Prepare a dataframe for the regression ###

source(paste0(global.dir, "gam_data_prep.R"))

#### Rate Ratio plots ####

## read the best specifications from file
## if you do not want to rerun all the regressions - read the selected specifications from this file:
file_path = paste0(gam.results,"model_specs.txt")
# read all lines from the file into a character vector
lines = readLines(file_path)
lines = lines[3:length(lines)]
lines = unlist(setNames(lines[seq(2, length(lines), by=2)], lines[seq(1, length(lines), by=2)]))

y = gam.ys[1]

## define formula and specification from file
myformula = lines[grep(y, lines)][[1]]
nvar = str_count(myformula, fixed("+")) +1
my.spec = all_combinations[[which(sapply(all_combinations, function(i)
  sum(sapply(i, function(w) grepl(w, myformula))) == nvar &
    length(i) == nvar))]]

m = gam(as.formula(myformula),
        data = sum.res3.gam,
        weights = sum.res3.gam$weight,
        family = "nb")

terms_to_plot = setdiff(my.spec, c("resp_age", 
                                   paste0("s(resp_age, k = ",smooth.k, ")"), 
                                   "resp_hhm_sick"))

# Define the desired order for the variable groups
group_order = c("ppwork_nothome", "resp_ethn", "resp_sex", 
                "resp_resp_sick", "hhsize2", "hh_income")


plot_colors = c("Positive (Significant)"     = "#1b9e77", # Dark Teal/Green
"Positive (Not Significant)" = "#99d8c9", # Light Teal/Green
"Negative (Significant)"     = "#d95f02", # Burnt Orange
"Negative (Not Significant)" = "#f8af86", # Light Orange/Amber
"Reference"                  = "#8DA0B3" # Muted Cool Grey
)

## manual reference level for income
tmp = sapply(strsplit(gsub("^\\s+", "", gsub("[^0-9 ]","", levels(sum.res3.gam$hh_income))), " "), 
             function(x) x[1])
manual_level = which(tmp[order(as.numeric(tmp))] == tmp[1])

p = create_rr_plots(m = m, 
                    terms_to_plot = terms_to_plot, 
                    mytb.vars.labels = mytb.vars.labels, 
                    mydata = sum.res3.gam,
                    manual_level = manual_level, ## manually assign where to put reference level for hh_income
                    group_order = group_order, ## define order to display variables (should match variable names in the spec)
                    plot_colors = plot_colors)

# Display the final plot
print(p)

ggsave(
  filename = paste0(output.dir, "1_effects_figure_total.pdf"),  # The name of the output file. The .pdf extension is key.
  plot = p,                 # The plot object you want to save.
  width = 8,                          # The width of the output file in inches.
  height = 12,                          # The height of the output file in inches.
  units = "in"                         # The units for width/height ('in', 'cm', 'mm').
)
