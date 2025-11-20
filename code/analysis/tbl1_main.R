## Main text
mytb.vars = c("resp_age_by5_80",
              "resp_ethn",
              "resp_sex",
              "ppwork2",
              "ppwork_nothome",
              "hh_income")

mytb.vars.labels = list(
  resp_age_adhoc2 = "Age Group (years)",
  resp_ethn = "Race/Ethnicity",
  resp_sex = "Sex",
  ppwork2 = "Main Activity",
  ppwork_nothome = "Main Activity Mode",
  hh_income = "Household Income"
)

#### Table 1 (Main) ####

weighted = TRUE ## can be a vector c(T,F) to include both weighted and unweighted summaries
my.ys = c("total.contacts")

# Create the grid of all combinations
combinations = expand.grid(myvariables = my.ys, weighted = weighted)
if("ppwork_nothome" %in% mytb.vars){ ## interaction term should be created using "interaction" function
  indent = list(first = c("ppwork_nothome"))
  layer = "ppwork"
}else{
  indent = NULL
  layer = NULL
}
combined_tbl = my.sumtb.func(mydata = sum.res3, vars = mytb.vars,
                             combinations = combinations,
                             variable_labels = mytb.vars.labels,
                             indent = indent,
                             layer = layer ## conditional that the variable to subset is "layer"_"interaction variable"
)
combined_tbl

### Export to CSV
write.csv(as_tibble(combined_tbl[[1]]),
          file.path(output.dir, "csv","table1_main.csv"),
          row.names = FALSE)

### Export to .docx
mytbl = combined_tbl[[1]]
mytbl$table_styling$header$label[which(mytbl$table_styling$header$column  == "stat_0_3")] =
  "**Total contacts**"
mytbl$table_styling$header$label[which(mytbl$table_styling$header$column  == "stat_0_4")] =
  "**Non-household contacts**"

output_file = file.path(output.dir, "table1_main.docx")

mytbl%>%
  as_gt() %>%
  tab_style(
    style = cell_text(font = "Arial", size = "10pt"),
    locations = cells_body() # Apply to the table body
  ) %>%
  tab_style(
    style = cell_text(font = "Arial", size = "11pt"),
    locations = cells_column_labels()
  )%>%
  gt::gtsave(filename = output_file)
