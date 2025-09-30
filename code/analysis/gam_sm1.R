
### Prepare a dataframe for the regression ###

source(paste0(global.dir, "gam_data_prep.R"))

#### Effects of Age Plots ####

## if you do not want to rerun all the regressions - read the selected specifications from this file:
file_path = paste0(gam.results,"model_specs.txt")
# read all lines from the file into a character vector
lines = readLines(file_path)
lines = lines[3:length(lines)]
lines = unlist(setNames(lines[seq(2, length(lines), by=2)], lines[seq(1, length(lines), by=2)]))

### Smooth term plots
smooth.term = "resp_age"

### Set outcome variable
y = "total.contacts"

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

# plot the results for numeric age 
p.a = plot_model(m, type = "pred",
                 terms = c("resp_age"
                           #, "ppwork_nothome[At school in person_student, At work in person_employed, Not at school_student, Not at work_employed, Studied fully online_student, Worked fully online_employed, Not student or employed_other]"
                 ),
                 colors = c("#397EC4","#87afd7"),
                 ci.lvl = 0.95)

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
  filename = paste0(output.dir, "/gam/age_me_total.pdf"),  # The name of the output file. The .pdf extension is key.
  plot = p.a,                 # The plot object you want to save.
  width = 7,                          # The width of the output file in inches.
  height = 5,                          # The height of the output file in inches.
  units = "in"                         # The units for width/height ('in', 'cm', 'mm').
)
