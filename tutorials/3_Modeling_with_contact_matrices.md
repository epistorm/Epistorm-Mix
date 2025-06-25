# Using contact matrices for Epidemiological Modeling

In this tutorial, we will explore how to use demographically stratified contact matrices estimated using contact patterns data to model the spread of respiratory infectious diseases. We will take a practical approach by demonstrating how to download the contact matrices, integrate them with various epidemiological models, and run simulations. 

Let's start by ...

```python
INSERT CODE
```

## Requirements for this tutorial

[TODO]

Imports:
```python
import pandas as pd
import numpy as np
```

## Working with contact matrices

For simple transmission models, we need at least two data elements: a population distribution by age and a contact matrix. 

### Loading the population distribution by age

Here we will assume that we have a distribution with one-year resolution, 
meaning that we know the number of individuals with 0 years old, 1 years old, 2 years old, and so on until the last age considered, for example, 99+ years or older. 
Such population distribution can be represented as a csv file like this 
(full example [here](/code/compartmental_epimodel/data/age_distributions/singleyr-age-dist.csv)):

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
age_structure_fpath = "example_age_structure.csv"
age_distribution_df = pd.read_csv(age_structure_fpath, index_col="age")
```

Where we use the `age` colum for indexing rows of the data frame. 


### Loading a contact matrix

A contact matrix between age groups can be stored as a csv file and look like this 
(full example [here](/code/compartmental_epimodel/data/survey_epistorm/ageby5_80-matrix.csv)):

```csv
"respondent","[0,5)","[5,10)","[10,15)","[15,20)","[20,25)","[25,30)","[30,35)","[35,40)","[40,45)","[45,50)","[50,55)","[55,60)","[60,65)","[65,70)","[70,75)","[75,80)","[80,120)"
"[0,5)",1.86508586599277,0.710422827992886,0.273081996947719,0.183325011542076,0.36571098013979,0.516021342455166,0.753156336541077,0.678910804245603,0.507195279388791,0.272118654895447,0.134876509582079,0.117034693639199,0.204593785542344,0.2267075414241,0.0737642531904337,0.00751391123927802,0.0267572370189854
"[5,10)",0.219730014087772,4.18930845088005,0.825018278438575,0.318152718583377,0.179360522139201,0.35033168678781,0.70932735346042,0.867405531679655,0.976521568557519,0.322524386111953,0.374348663444906,0.251417693528541,0.152369063965619,0.147057617204915,0.116092158995667,0.0333211477076163,0.0550742728747972
"[10,15)",0.163278102964188,0.245108440570201,3.98525137329441,0.636658072787636,0.262067588385853,0.118611393374782,0.429770465982329,0.593030839988691,0.771168608920652,0.603068229427644,0.393953305657497,0.119870149380466,0.149446716550355,0.0797952398681339,0.0790216974450706,0.0542235509609562,0.0150005645273413
...
```

This file can be loaded as a square-shaped pandas data frame:

```python
contact_matrix_fpath = "example_contact_matrix.csv"
contact_matrix_df = pd.read_csv(
        contact_matrix_fpath,
        header=0, index_col=0,
)

```

Here we assume that the row index 
of the contact matrix represents the respondent's age group, and the column 
index represents the contact's age group.
For the transmission model, we will also assume that the respondents will take 
the infection _from_ their contacts. If you are working with a contact matrix that is transposed with respect to this logic, you can simply transpose it by adding:

```python
contact_matrix_df = contact_matrix_df.T
```

### Aggregating the population distribution into age groups

Before we can use the age distribution into an age-structured transmission model, we need a version of it that matches the age structure of the contact matrix, which is most often done by age groups instead of individual ages. 

First, let's define the age groups as an array containing the first age of each age group. For example, from the contact matrix defined above, we would define:

```python
age_groups = np.array([0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80])
```

Which reads as follows: the first age group goes from 0 to 4 years, the second ranges from 5 to 9, the third from 10 to 14, and so on. The last age group starts at 80 and represnts all ages 80 and older.

Then, we aggregate the age distribution by:

1. Assigning an age group label to each age in the distribution data frame.
2. Adding together all ages that belong to the same age group

Below is a code snippet to perform this, along other operations that will be helpful for the transmission model. You can also check and use the function `aggregate_age_distribution()` in the [utilities file](/code/compartmental_epimodel/project_utils.py) of the transmission model project.

```python
# Aggregate the population distribution from individual ages into age groups
# =============

# Convert the age index into a column for binning
df = age_distribution_df.reset_index()

# Prepare the bins and labels
bins = list(age_groups) + [np.inf]  # include the final group as open-ended
labels = [f"{age_groups[i]}-{age_groups[i+1]-1}" for i in range(len(age_groups)-1)]
labels.append(f"{age_groups[-1]}+")  # last group is open-ended

# (1.) Assign group labels to each row
df["age_group_label"] = pd.cut(df["age"], bins=bins, right=False, labels=labels)

# Create a variable with the age group index for each age in the distribution
positions = np.searchsorted(age_groups, df["age"], side='right') - 1
positions = np.clip(positions, 0, len(age_groups) - 1)
df["age_group_index"] = positions

# (2.) Aggregate by age group
agg_df = df.groupby("age_group_label", observed=False).agg(
    **{"n_indiv": ("n_indiv", "sum")}
)

# Recompute the distribution as normalized counts
agg_df["distribution"] = agg_df["n_indiv"] / agg_df["n_indiv"].sum()
agg_df["age_min"] = age_groups_array  # Add the column with the lower bound of each age group

# Update the distribution df so it contains age group information
age_distribution_df = df.set_index("age")
```


## Using age-stratified contact matrices in compartmental models
Now that we have imported a contact matrix and have both aggregated and year-wise population distributions, we can simulate an epidemic using a compartmental model with age-structured contacts. Here, we show an example using a simple *Susceptible Infectious Removed* (SIR) model. 

Let's start by defining some parameters for the model and the simulation:

```python
dt = 1.0  # Length of the time step, in days
beta = 0.090  # Per-contact transmission risk
gamma = 0.333  # Rate of removal from the infectious compartment
total_pop = data.age_distribution_df["n_indiv"].sum()  # Total number of individuals

```

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
