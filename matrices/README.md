# Contact Matrices

## Matrix

Contact matrices (M~ij~) are estimated where each cell represents the weighted 
mean number of contacts of a participant in group *i* 
(e.g., age, race/ethnicity, and sex) reported with individuals in group *j*.

$$
M_{ij} = \frac{\sum_{r = 1}^{N_i} C_{ij}^r}{N_i}
$$

- *i* is the group of the respondent
- *j* is the group of the contact
- *r* is the respondent in group *i*
- $C_{ij}^r$ is the number of contacts reported by respondent *r* in group *i* with a contact in group *j*
- ${N_i}$ is the number of respondents in group *i*

Matrices used in the analysis are available: [M_matrix](./M_matrix)

## Conditional matrix

Contact matrices representing the average number of reported contacts given 
that a respondent is present (has non-zero contacts) in a specific setting for age,
race/ethnicity, and sex. 

$$
\theta_{ij}^k = \frac{\sum_{r=1}^{N_{ik}}C_{ij}^{r(k)}}{N_{ik}}
$$

- $C_{ij}^{r(k)}$ is the number of contacts reported by respondent *r* of age 
group *i* with a contact of age group *j* in a social setting *k*
- ${N_{ik}}$ is the number of respondents in age group *i* who reported 
non-zero contacts in setting *k*

Matrices used in the analysis are available: [M_conditional](./M_conditional)

## Symmetrized matrix

Contact matrices where each entry represents the number of reciprocal 
contacts between the two respondent and contact groups.

$$
M_{ij}^k = \frac{M_{ij}^k + M_{ij}^{T^k}}{2}
$$

- *i* is the group of the respondent
- *j* is the group of the contact
- *k* is the setting
- *T* means transposed 

Matrices used in the analysis are available: [M_reciprocal](./M_reciprocal)

## Frequency-based matrix

Contact matrices calculated by using the relative abundance of contacts between 
individuals of age *i* and individuals of age *j* reported by each respondent *r*
in setting *k*. 

$$
\Gamma_{ij}^{r(k)} = \frac{C_{ij}^{r(k)}}{\sum_{r=1}^{N_{ik}}C_{ij}^{r(k)}}
$$

- *i* is the group of the respondent
- *j* is the group of the contact
- *r(k)* is the respondent who reported having the contact in the social setting *k*
- $C_{ij}^{r(k)}$ is the number of contacts reported by respondent *r* of age group *i* with contact of age group *j* in social setting *k*
- $N_{ik}$ is the number of respondents in age group *i* who reported non-zero contacts in social setting *k*

Matrices used in the analysis are available: [F_matrix](./F_matrix)



