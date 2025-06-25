# Using contact matrices for Epidemiological Modeling

In this tutorial, we will explore how to use demographically stratified contact matrices estimated using contact patterns data to model the spread of respiratory infectious diseases. We will take a practical approach by demonstrating how to download the contact matrices, integrate them with various epidemiological models, and run simulations. 

Let's start by ...

```python
INSERT CODE
```

## Requirements in this tutorial


## Working with contact matrices

For simple transmission models, we need at least two data elements: a population distribution by age and a contact matrix. 

### Loading the population distribution by age
The population distribution can be represented as a csv file like this:

```csv
"age","n_indiv"
0,3480117
1,2,3532512,0.0106577754199443
2,3,3672703,0.0110807390769389
3,4,3797741,0.0114579853320002
4,5,3917162,0.0118182848011669
...
```

This file can be loaded in python as a pandas dataframe:

```python
import pandas as pd

age_structure_fpath = "example_age_structure.csv"
age_distribution_df = pd.read_csv(age_structure_fpath, index_col="age")
```

Where we use the `age` colum for indexing rows of the dataframe. 


### Loading a contact matrix

A contact matrix between age groups can look like this:

```csv
```


## Using age-stratified contact matrices in compartmental models
Now that we have imported our contact matrix, we can simulate an epidemic using a compartmental model considering these contact patterns. Here, we show an example using a simple SIR model. 

```python
# create a simple SIR model

# incorporate contact matrix

# simulate an epidemic

#plot results
```

## Using age-stratified contact matrices in an agent-based models

```
INSERT CODE
```
