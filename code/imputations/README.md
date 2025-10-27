# Missing Data Strategy

This folder contains the pipeline script that will conduct the full imputations to create imputed data based on the clean raw data. Imputations include random forest multiple imputations of the missing data on individual contacts and sampling of age, race/ethnicity, and sex for missing data on multiple contacts reported as a group.

## Requirements

Download the imputations folder. Open the `_pipeline_imputations.R` script and change the working directory.

``` r
my.folder = "[your preferred working directory name here]"
setwd(my.folder)
```

The imputations used in this code are computationally heavy and require parallel processing. Ensure the following packages are loaded into your library:
- `randomForest`
- `doRNG`
- `foreach`
- `doParallel` 
