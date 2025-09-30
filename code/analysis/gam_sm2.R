
#### Rate Ratio plots ####

## prepare data for regression
source(paste0(global.dir, "gam_data_prep.R"))

## if you do not want to rerun all the regressions - read the selected specifications from this file:
file_path = paste0(gam.results,"model_specs.txt")

# read all lines from the file into a character vector
lines = readLines(file_path)
lines = lines[3:length(lines)]
lines = unlist(setNames(lines[seq(2, length(lines), by=2)], lines[seq(1, length(lines), by=2)]))

### Smooth term plots
smooth.term = "resp_age"

### Set the outcome variable
y = "cont_setting_nothh_n"

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

### Colors for RR plots
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
  filename = paste0(output.dir, "/gam/effects_figure_nonhh.pdf"),  # The name of the output file. The .pdf extension is key.
  plot = p,                 # The plot object you want to save.
  width = 8,                          # The width of the output file in inches.
  height = 12,                          # The height of the output file in inches.
  units = "in"                         # The units for width/height ('in', 'cm', 'mm').
)

#### Effects of Age Plots ####

p.a = plot_model(m, type = "pred",
                 terms = c("resp_age"
                           #, "ppwork_nothome[At school in person_student, At work in person_employed, Not at school_student, Not at work_employed, Studied fully online_student, Worked fully online_employed, Not student or employed_other]"
                 ),
                 colors = c("#397EC4","#87afd7"),
                 ci.lvl = 0.95)
y = gsub("cont_setting_nothh_n", "Non-Household Contacts", y)
p.a = p.a + 
  xlim(0, 84) + 
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"), # Mid-grey major grid lines
    panel.grid.minor = element_line(color = "grey90")  # Lighter minor grid lines
  ) + 
  labs(
    title = "", #str_to_title(gsub("\\.", " ", y)),
    x = "Respondent Age",
    y = "Predicted N of contacts"
  )

print(p.a)

ggsave(
  filename = paste0(output.dir, "/gam/age_me_nonhh.pdf"),  # The name of the output file. The .pdf extension is key.
  plot = p.a,                 # The plot object you want to save.
  width = 7,                          # The width of the output file in inches.
  height = 5,                          # The height of the output file in inches.
  units = "in"                         # The units for width/height ('in', 'cm', 'mm').
)

# Combine them and add panel tags
final_figure <- plot_grid(
  p, p.a,
  ncol = 1,          # Arrange in one column
  rel_heights = c(3, 1), # Corresponds to patchwork's heights argument
  labels = "AUTO",       # Automatically adds 'A' and 'B' labels
  align = 'v'          # The key: aligns vertically, but NOT horizontally
)


# Display the final figure
print(final_figure)

ggsave(
  filename = paste0(sm.dir, "/gam/combined_gam_figure_nonhh.pdf"),  # The name of the output file.
  plot = final_figure,                 # The plot object to save.
  width = 8,                          # The width of the output file in units below.
  height = 15,                          # The height of the output file in units below.
  units = "in"                         # The units for width/height
)
