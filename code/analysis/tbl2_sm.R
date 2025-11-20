## Vars
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
                        resp_industry_group = "Industry Group",
                        resp_occupationg_group = "Occupation Group",
                        hh_loc_reg4 = "Region",
                        weeks = "Survey Week",
                        wave = "Wave")


## Calculate summaries
weighted = TRUE ## can be a vector c(T,F) to include both weighted and unweighted summaries
my.ys1 = c("total.contacts", "cont_setting_nothh_n") 
my.ys2 = c("cont_setting_hh_n", "cont_setting_school_n", 
           "cont_setting_work_n", "cont_setting_other_n")

## Create the grid of all combinations
combinations = expand.grid(myvariables = my.ys1, weighted = weighted)

if("ppwork_nothome" %in% mytb.vars){ ## interaction term should be created using "interaction" function
  indent = list(first = c("ppwork_nothome"))
  layer = "ppwork"
}else{
  indent = NULL
  layer = NULL
}

## Create the grid of all combinations for additional columns
combinations2 = expand.grid(myvariables = my.ys2, weighted = weighted)
listnames = gsub("\\.|_n |_", " ",
                 gsub("FALSE", "unweighted", gsub("TRUE", "weighted",
                                                  paste(combinations2[,1], 
                                                        combinations2[,2], 
                                                        sep = " "))))
summary_table3 =  lapply(c(1:nrow(combinations2)), function(i)
  
  sum.res3 |>
    tbl_custom_summary(
      include = all_of(mytb.vars),
      by = NULL,
      stat_fns = everything() ~ mean_p(as.character(combinations2[i,1])),
      statistic = everything() ~ "{mean_contacts} ({weighted_p}%)",
      digits = everything() ~ c(1, 1),
      missing = "ifany",
      missing_text = "(Unknown)",
      missing_stat = "-",
      overall_row = TRUE,
      overall_row_label = "Total Population",
      label = mytb.vars.labels # Add labels here
    ) |>
    bold_labels()|>
    modify_header(all_stat_cols() ~ paste0("**", listnames[i], "**"))
  
)

combined_tbl_nonhh = my.sumtb.func(mydata = sum.res3, vars = mytb.vars,
                                   combinations = combinations,
                                   which.tables = c(3),
                                   external.tbl = summary_table3,
                                   variable_labels = mytb.vars.labels,
                                   indent = indent,
                                   layer = layer ## conditional that the variable to subset is "layer"_"interaction variable"
)
combined_tbl_nonhh

## Export to .docx
mytbl = combined_tbl_nonhh[[1]]
mytbl$table_styling$header$label[which(mytbl$table_styling$header$column  == "stat_0_1")] =
  "**Total**"
mytbl$table_styling$header$label[which(mytbl$table_styling$header$column  == "stat_0_2")] =
  "**Non-household**"
mytbl$table_styling$header$label[which(mytbl$table_styling$header$column  == "stat_0_3")] =
  "**Household**"
mytbl$table_styling$header$label[which(mytbl$table_styling$header$column  == "stat_0_4")] =
  "**School**"
mytbl$table_styling$header$label[which(mytbl$table_styling$header$column  == "stat_0_5")] =
  "**Work**"
mytbl$table_styling$header$label[which(mytbl$table_styling$header$column  == "stat_0_6")] =
  "**Other**"

output_file = file.path(output.dir, "table2_sm.docx")
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

## Export to CSV
write.csv(as_tibble(combined_tbl_nonhh[[1]]),
          file.path(output.dir,"csv", "table2_sm.csv"),
          row.names = FALSE)
