## vars
mytb.vars = c("resp_ethn",
              "resp_sex",
              "ppwork2",
              "ppwork_nothome",
              "hh_income",
              "resp_age_by5_80",
              "resp_age_insightnet",
              "hhsize2",
              "is_weekend2",
              "resp_resp_sick",
              "resp_hhm_sick",
              # "resp_underlcond",
              "resp_industry_group",
              "resp_occupation_group",
              "hh_loc_reg4",
              "weeks",
              "wave")

mytb.vars.labels = list(resp_ethn = "Race/Ethnicity",
                        resp_sex = "Sex",
                        ppwork2 = "Main Activity",
                        ppwork_nothome = "Main Activity Mode",
                        hh_income = "Household Income",
                        resp_age_by5_80 = "5-year Age Group",
                        resp_age_insightnet = "Ad-hoc Age Group",
                        hhsize2 = "Household Size",
                        is_weekend2 = "Day",
                        resp_resp_sick = "Sick",
                        resp_hhm_sick = "Household Member Sick",
                        # resp_underlcond = "Has Underlying Conditions",
                        resp_industry_group = "Industry Group",
                        resp_occupationg_group = "Occupation Group",
                        hh_loc_reg4 = "Region",
                        weeks = "Survey Week",
                        wave = "Wave")


## Calculate summaries

weighted <- TRUE ## can be a vector c(T,F) to include both weighted and unweighted summaries
my.ys = "total.contacts" 

# Create the grid of all combinations
combinations = expand.grid(myvariables = my.ys, weighted = weighted)

if("ppwork_nothome" %in% mytb.vars){ ## interaction term should be created using "interaction" function
  indent = list(first = c("ppwork_nothome"))
  layer = "ppwork"
}else{
  indent = NULL
  layer = NULL
}
combined_tbl_total = my.sumtb.func(mydata = sum.res3, vars = mytb.vars,
                             combinations = combinations,
                             variable_labels = mytb.vars.labels,
                             indent = indent,
                             layer = layer ## conditional that the variable to subset is "layer"_"interaction variable"
)
combined_tbl_total

### Export to CSV
write.csv(as_tibble(combined_tbl_total[[1]]),
          paste0(output.dir,"csv/table1_sm.csv"),
          row.names = FALSE)

### Export to .docx

mytbl = combined_tbl_total[[1]]
mytbl$table_styling$header$label[which(mytbl$table_styling$header$column  == "stat_0_3")] =
  "**Total contacts**"

output_file = paste0(output.dir, "table1_sm.docx")

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

