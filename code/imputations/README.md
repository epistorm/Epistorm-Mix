# Imputations of Missing Contact Patterns Data

This folder contains the pipeline script that will conduct the full imputations for the raw data.

## Requirements

Download the imputations folder. Open the `_pipeline_imputations.R` script and change the working directory.

``` r
my.folder = "[your preferred working directory name here]"
setwd(my.folder)
```

The imputations used in this code is computationally heavy and requires parallel processing. Ensure the following packages are loaded into your library:
- `randomForest`
- `doRNG`
- `foreach`
- `doParallel` 
