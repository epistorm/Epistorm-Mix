# Epistorm-Mix: Mapping Social Contact Patterns in the Post-Pandemic United States

The **Epistorm-Mix** contains post-COVID-19 pandemic contact patterns data for a representative sample of the US population. Respondents reported all person-to-person contacts in the preceding day ("yesterday") from the time they woke up until the time they went to sleep. Using this individual survey data, we constructed contact matrices stratified by age, sex, and race/ethnicity, which can be used to simulate the spread of respiratory infectious diseases through compartmental modeling. This repository serves as an open-access resource for infectious disease modeling and public health planning. 

[**Website**](https://www.epistorm.org/data/epistorm-mix)

## Getting Started

### [Contact Patterns Data](/data/imputed_data)
Individual-level data is provided in the data folder in the form of two datasets: i) a respondent-level dataset with all characteristics of the respondent, and ii) a contact-level dataset with all the characteristics of each reported contact. Respondent ID can be used to merge the two datasets. We recommend using the imputed data for any subsequent analysis and creation of contact matrices. Codebooks are provided with the data.

The sample is nationally representative by age, sex, race/ethnicity, household income, census region, and language for the Hispanic population. For each respondent, sample weights are provided in respondent-level data.

For reproducibility and sensitivity analyses, the data folder also contains: i) "raw data" (respondent-level and contact-level data) with clean data before imputations, ii) "external data" from other studies and sources that were used for comparisons, variable creation, and missing data imputations, iii) "math-model data" with all necessary data to reproduce mathematical modeling performed for the [manuscripts](https://www.medrxiv.org/content/10.1101/2025.11.20.25340662v1), iv) "gam specifications" folder containing the files for the GAM statistical analysis of individual-level data.

The analyses in this repository are conducted using the imputed data. 

### [Contact Matrices](./matrices)
The matrix folder contains the following types of matrices:
- M_matrix: total and setting-specific contact matrices by age, where each cell represents the weighted mean number of contacts of a participant in age group i reported with individuals in age group j. 
- F_matrix: frequency-based, setting-specific contact matrices by age, where each cell represents the per capita probability of contact of an individual of age i with an individual of age j in each setting.

### [Code](./code)

Code is provided for the mathematical modeling (in Python), missing data imputations, data wrangling and analyses (in R).

### [Tutorials](./tutorials)
We provide a series of tutorials to help you get started with Epistorm-Mix. 
- [Tutorial 1](/tutorials/data-analysis/1_Accessing_and_extracting_data.md): Accessing and extracting data
- [Tutorial 2](/tutorials/data-analysis/2_Constructing_contact_matrices.md): Constructing contact matrices
- [Tutorial 3](/tutorials/data-analysis/3_Visualizing_contact_matrices.md): Visualizing contact matrices
- [Tutorial 4](/tutorials/mathematical-modeling/4_Compartmental_modeling_with_contact_matrices.md): Compartmental modeling with contact matrices 

## Publications
The preprint describing the data collection and estimation of the contact matrices:
- Litvinova M., Sinclair S., Kummer A.G., Ventura P.C., Foster T., Shioda K., Halloran M.E., Vespignani A., and Ajelli M. ***Epistorm-Mix: Mapping Social Contact Patterns for Respiratory Pathogen Spread in the Post-Pandemic United States.*** medRxiv 2025.11.20.25340662; doi: 10.1101/2025.11.20.25340662 (available [here](https://www.medrxiv.org/content/10.1101/2025.11.20.25340662v1)). 

## Citation
To reference the data and code presented in this GitHub repository, please use the following citation: 

```bibtex 
@article {Litvinova2025.11.20.25340662,
	author = {Litvinova, Maria and Sinclair, Shelly and Kummer, Allisandra G and Ventura, Paulo C and Foster, Trevor and Shioda, Kayoko and Halloran, M Elizabeth and Vespignani, Alessandro and Ajelli, Marco},
	title = {Epistorm-Mix: Mapping Social Contact Patterns for Respiratory Pathogen Spread in the Post-Pandemic United States},
	elocation-id = {2025.11.20.25340662},
	year = {2025},
	doi = {10.1101/2025.11.20.25340662},
	publisher = {Cold Spring Harbor Laboratory Press},
	URL = {https://www.medrxiv.org/content/early/2025/11/21/2025.11.20.25340662},
	eprint = {https://www.medrxiv.org/content/early/2025/11/21/2025.11.20.25340662.full.pdf},
	journal = {medRxiv}
}
```

## Funding 
Authors acknowledge support from the CDC-RFA-FT-23-0069 cooperative agreement from the CDC's Center for Forecasting and Outbreak Analytics. The findings and conclusions in this study are those of the authors and do not necessarily represent the official position of the funding agencies. Any use of trade, firm, or product names is for descriptive purposes only and does not imply endorsement by the U.S. Government. The funders had no role in study design, data collection and analysis, decision to publish, or preparation of the manuscript. 

## License
This project is licensed under the CC BY-NC-SA 4.0 license. See the [LICENSE](./LICENSE) file for more details.


## Contact
For questions or issues, please open an issue on GitHub or [contact the maintainer](mailto:malitv@iu.edu).


