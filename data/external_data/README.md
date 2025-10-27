# External Data
This data has been utilized in the analysis for the comparisons with other countries and pre-pandemic estimates, the creation of variables, or the missing data imputations.

## Sources
### Comparisons with contact matrices from other countries
- [matrices-Jarvis.xlsx](./matrices-Jarvis.xlsx) (contact matrices for Netherlands, UK, Belgium, and Switzerland) 
  - Jarvis, C. I., et al. "Social contact patterns following the COVID-19 pandemic: a snapshot of post-pandemic behaviour from the CoMix study." Epidemics 48 (2024): 100778.
- [matrices-Liang.csv](./matrices-Liang.csv) (contact matrices for Anhua County, Hunan Province, China)
  - Liang, Y., et al. "Social contact patterns and their impact on the transmission of respiratory pathogens in rural China." Infectious Disease Modelling 10.2 (2025): 439-452.
- [matrices-Wang.xlsx](./matrices-Wang.xlsx) (contact matrices for administrative districts in Suzhou city, Jiangsu province, China)
  - Wang, M. et al. Social contacts patterns relevant to the transmission of infectious diseases in Suzhou, China following the COVID-19 epidemic. J. Health Popul. Nutr. 43, 58 (2024).
- [resident_population_Italy.csv](./population-Italy.csv) (population age stucture for Italy)
   - "Popolazione Residente." Istat.it, 2025
- [Lucchini_bootstrap_age_group_total.zip](./Lucchini_bootstrap_age_group_total.zip), [Italy-sample-size.csv](./Italy-sample-size.csv) (contact matrices and sample age structure for Italy)
  - Lucchini, L., et al. “Post-Pandemic Social Contacts in Italy: Implications for Social Distancing Measures on In-Person School and Work Attendance.” ArXiv.org. (2024).
  
### Comparisons with the US estimates obtained before the COVID-19 Pandemic
- [matrices-Mistry.csv](./matrices-Mistry.csv), [mistry-data](./mistry-data)
  - Mistry, D., et al. "Inferring high-resolution human mixing patterns for disease modeling." Nature Communications 12.1 (2021): 323.
- [matrices-Prem.xlsx](./matrices-Prem.xlsx)
  - Prem, K., Alex R. Cook, and Mark Jit. "Projecting social contact matrices in 152 countries using contact surveys and demographic data." PLoS computational biology 13.9 (2017): e1005697.
 
### Variable creation and imputations
- [federal-state-holidays.csv](./federal-state-holidays.csv) (for weekend & days off variable)
  - "Columbus Day, Indigenous Peoples Day or just a regular Monday? It depends on where you are". Pew Research Center. Available [here](https://www.pewresearch.org/short-reads/2023/10/05/working-on-columbus-day-or-indigenous-peoples-day-it-depends-on-where-your-job-is/)
  - "American holidays". USAGov Contact Center. Available [here](https://www.usa.gov/holidays).
- [pop_agestr.csv](pop_agestr.csv), [student_agestr.csv](./student_agestr.csv) (for sampling age and race/ethnicity of students and customers reported as multiple contacts)
  - Census Bureau, American Community Survey (ACS). Census.gov (2025). 





