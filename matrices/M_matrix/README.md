# M-matrix

The calculation process is described [here](../README.md).

Each file contains a matrix with column names representing the contact group and row names representing the respondent group.

The name of the file follows the same naming pattern, 'setting-matrix-group-format.csv', where 
- setting is "Household", "School", "Work", or "Other"
- matrix is either "F" or "M"
- group is described depending on the dimension.
  For age, it follows the pattern "step_last cutoff", where step is usually "by5" and last cutoff defines the last age group, e.g., "75" for age group "75+", "80" for "80+"
- format is always "matrix" 
