# Contact Matrices

This folder contains M-matrices and F-matrices for mathematical modeling and visualization of contact patterns. Contact matrices are based on imputed data and can be recreated using the scripts provided in the code folder.

## Matrix (M-matrices)

Each cell of the contact matrices $M_{ij}$ represents the weighted mean number of contacts with individuals in age group *j* reported by a participant in age group *i*.

$$
M_{ij} = \frac{\sum_{r = 1}^{N_i} C_{ij}^r}{N_i}
$$

-   *i* is the age group of the respondent
-   *j* is the age group of the contact
-   *r* is the respondent in age group *i*
-   $C_{ij}^r$ is the number of contacts reported by respondent *r* in age group *i* with a contact in age group *j*
-   ${N_i}$ is the number of respondents in age group *i*

The folder [M_matrix](./M_matrix) contains these matrices for age groups in 5-year brackets. Contact matrices for other socio-demographic groups and other age groups can be generated following the same procedure. Examples of such an approach can be found in the provided [code pipeline](../code/analysis/_pipeline_analysis.R).

## Frequency-based matrix (F-matrices)

Frequency-based (F) matrices reflect the per-capita probability of non-zero contact between individuals of age *i* and individuals of age *j*.

To obtain F-matrices, we first calculate the relative abundance of contacts between individuals of age *i* and individuals of age *j* reported by each respondent *r* in setting *k*:

$$
\Gamma_{ij}^{r(k)} = \frac{C_{ij}^{r(k)}}{\sum_{j}C_{ij}^{r(k)}},
$$

where
-   *i* is the group of the respondent
-   *j* is the group of the contact
-   *r(k)* is the respondent who reported having the contact in the social setting *k*
-   $C_{ij}^{r(k)}$ is the number of contacts in each age group *j* a respondent *r* of age group *i* reported to have in social setting *k*

Second, we calculate the per-capita probability of contact:

$$
F_{ij}^k = \frac{\sum_{r=1}^{N_{ik}}\Gamma_{ij}^{r(k)}}{N_i},
$$

where
- $N_{ik}$ is the number of respondents in age group *i* who reported non-zero contacts in social setting *k*
- $N_i$ is the total number of respondents of age group *i*

The folder [F_matrix](./F_matrix) contains F-matrices for age groups in 5-year brackets. F-matrices for other age groups can be generated using the code "freq-m.R" in [code pipeline](../code/analysis/_pipeline_analysis.R).
