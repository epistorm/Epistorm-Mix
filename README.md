# Epistorm-Mix: Mapping Social Contact Patterns in the Post-Pandemic United States

[**Website**]() | [**Tutorials**](https://github.com/allisandrak/Observatory-of-US-Contact-Patterns/tree/main/tutorials) 

The **Epistorm-Mix** contains post-COVID-19 pandemic contact patterns data for a representative sample of the US population. Respondents reported all person-to-person contacts in the preceding day ("yesterday") from the time they woke up until the time they went to sleep. Using this individual survey data, we constructed contact matrices stratified by age, sex, and race/ethnicity, which can be used to simulate the spread of respiratory infectious diseases through compartmental modeling. This repository serves as an open-access resource for infectious disease modeling and public health planning. 

## Getting Started

### Contact Patterns Data
Individual-level data is provided in the data folder ([Contact patterns data](/data)<br/>) in the form of two datasets: i) a respondent-level dataset with all characteristics of the respondent, and ii) a contact-level dataset with all the characteristics of each reported contact. Respondent ID can be used to merge the two datasets. We advise using the imputed data ([Contact patterns data](/data/imputed_data)<br/>) for any further analysis and creation of the contact matrices. Codebooks are provided with the data.

The sample is nationally representative by age, sex, race/ethnicity, household income, census region, and language for the Hispanic population. For each respondent, sample weights are provided in respondent-level data.

For reproducibility and sensitivity analyses, the data folder also contains: i) "raw data" (respondent-level and contact-level data) with clean data before missing data imputations, ii) "external data" from other studies and sources that were used for comparisons, variable creation, and missing data imputations, iii) "math-model data" with all necessary data to reproduce mathematical modeling performed for the manuscripts (LINK HERE), iv) "gam specifications" folder containing the files for GAM statatistical analysis of individual-level data.

The analyses in this repository are conducted using the imputed data. 

### Contact Matrices
For the matrices, check the matrix folder ([Contact matrices](/matrices)<br/>) containing the following types of matrices:
- M_matrix: total and setting-specific contact matrices by age
- F_matrix: frequency-based, setting-specific contact matrices by age 

### Code

Code is provided for data wrangling and analyses (in R) and for the modeling (in Python). 
For the code, check the code folder: [Code](/code)

### Tutorials
We provide a series of tutorials to help you get started with Epistorm-Mix. 
- [Tutorial 1](/tutorials/data-analysis/1_Accessing_and_extracting_data.md): Accessing and extracting data
- [Tutorial 2](/tutorials/data-analysis/2_Constructing_contact_matrices.md): Constructing contact matrices
- [Tutorial 3](/tutorials/data-analysis/3_Visualizing_contact_matrices.md): Visualizing contact matrices
- [Tutorial 4](/tutorials/mathematical-modeling/4_Compartmental_modeling_with_contact_matrices.md): Compartmental modeling with contact matrices 

## Citation
The preprint describing the data collection and estimation of the contact matrices is available [here](). To reference our work, please use the following citation: 

```bibtex 
@article{XXX,
        author = {Litvinova, Maria and Sinclair, Shelly, and Kummer, Allisandra G., and Ventura, Pauo C. and Foster, Trevor and Shioda, Kayoko and Halloran, M. Elizabeth and Vespignani, Alessandro and Ajelli, Marco},    
        title = {The Post-Pandemic Contact Landscape in the United States: Implications for Respiratory Pathogen Transmission},<br/>
        elocation-id = {},
        year = {2025},
        doi = {},
        publisher = {},
        URL = {},
        eprint = {},
        journal = {}
}
```


## License
This project is licensed under the CC BY-NC-SA 4.0 license. See the [LICENSE](/LICENSE) file for more details.


## Contact
For questions or issues, please open an issue on GitHub or contact the maintainer at XXX.


