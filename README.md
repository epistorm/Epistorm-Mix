# Epistorm-Mix: Mapping Social Contact Patterns in the Post-Pandemic United States

[**Documentation**]() | [**Website**]() | [**Tutorials**](https://github.com/allisandrak/Observatory-of-US-Contact-Patterns/tree/main/tutorials) 

The **Epistorm-Mix** contains post-COVID-19 pandemic contact patterns data for a sample of the US that is nationally representative by age, sex, and race/ethnicity as well as the respective estimated contact matrices, allowing users to simulate the spread of respiratory infectious diseases through different population groups using current US social mixing patterns.


## Downloads
[Contact patterns data](/data)<br/>
[Contact matrices](/matrices)<br/>
[Code](/code)


## Quick Start
Once downloaded, you can start using the **contact patterns data** and the **contact matrices**. Below is an example to get started using RStudio. 


### Example: 

```
# Use the contact matrices in the matrices folder.
# The analyses scripts in the code folder along with the data in the data folder can alternatively be used to
# create matrices.
hh.df = read.csv("./your-path/matrices/hh_example.csv")

# to transform into matrix format
hh.matrix <- hh.df %>%
  pivot_wider(
    names_from = cont_age_by5_80,   # Values from 'cont_age_by5_80' become column names
    values_from = ncont,            # Values from 'ncont' fill the cells
    id_cols = resp_age_by5_80       # 'resp_age_by5_80' values become the row identifiers
  )

# to sort age groups in the right order
hh.df$resp_age_by5_80 = factor(hh.df$resp_age_by5_80,
                                  levels = unique(hh.df$resp_age_by5_80))
hh.df$cont_age_by5_80 = factor(hh.df$cont_age_by5_80,
                                  levels = unique(hh.df$cont_age_by5_80))

# to create a plot
ggplot(hh.df) +
      geom_tile(aes(x = resp_age_by5_80, y = cont_age_by5_80, fill = ncont)) +
      ggtitle("Household") + # Main title
      xlab("Respondent Age") + # X-axis title
      ylab("Contact Age") + # Y-axis title
      scale_fill_gradientn(colors = c( "#005f87", "#87afd7", "lightblue")) +
      coord_fixed(ratio = 1) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent", color = "black"),
            plot.background = element_rect(fill = "transparent", color = NA)
      )

```

### Tutorials
We provide a series of totorials to help you get started with the Observatory of US Contact Patterns. 
- [Tutorial 1](/tutorials/1_Constructing_contact_matrices.md): Constructing contact matrices
- [Tutorial 2](/tutorials/2_Compartmental_modeling_with_contact_matrices.md): Modeling with contact matrices (compartmental formulation)



## Citation
The preprint describing the data collection and estimation of the contact matrices is available [here](). To reference our work, please use the following citation: 

```bibtex 
@article{XXX,
        author = {Litvinova, Maria and Sinclair, Shelly, and Kummer, Allisandra G., and Ventura, Pauo C. and Foster, Trevor and Shioda, Kayoko and Halloran, M. Elizabeth and Vespignani,                            Alessandro and Ajelli, Marco},    
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


