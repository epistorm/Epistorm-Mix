# Epistorm-Mix: Mapping Social Contact Patterns in the Post-Pandemic United States

[**Documentation**]() | [**Website**]() | [**Tutorials**](https://github.com/allisandrak/Observatory-of-US-Contact-Patterns/tree/main/tutorials) 

The **Epistorm-Mix** contains post-COVID-19 pandemic contact patterns data for a sample of the US that is nationally representative by age, sex, race/ethnicity, household income, census region, and language. Respondents reported all person-to-person contacts in the preceding day, including the contact's age and setting (household, school, workplace, community). Using the survey data, we constructed contact matrices stratified by age, sex, and race/ethnicity. Contact matrices can be used to simulate the spread of respiratory infectious diseases through compartmental modeling. This repository serves as an open-access resource for modeling and public health planning. 

## Getting Started

### Contact Patterns Data

- Raw data: respondent-level and contact-level data
- Imputed data: respondent-level and contact-level data
- External data: additional contact patterns data from other studies

The analyses in this repository are conducted using the imputed data. The raw data is provided for the sensitivity analysis. Additionally, codebooks for both the respondent-level and contact-level data are provided with the raw and imputed data. For the data, check the data folder: [Contact patterns data](/data)<br/>

### Contact Matrices

- M_matrix: total and setting-specific contact matrices by age, sex, and race/ethnicity
- F_matrix: frequency-based, setting-specific contact matrices by age 

For the matrices, check the matrix folder: [Contact matrices](/matrices)<br/>

### Code

Code is provided for imputations and analyses. For the code, check the code folder: [Code](/code)

### Tutorials
We provide a series of totorials to help you get started with Epistorm-Mix. 
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
This project is licensed under the GPL-3.0 License. See the [LICENSE](/LICENSE) file for more details.


## Contact
For questions or issues, please open an issue on GitHub or contact the maintainer at XXX.


