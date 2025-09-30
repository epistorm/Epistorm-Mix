# Imputations of Missing Contact Patterns Data

This folder contains the pipeline script that will conduct the full imputations for the raw data.

## Requirements

Download the imputations folder. Open the `_pipeline_imputations.R` script and change the global directory.

``` r
e = new.env() 
e$global.dir = "[your preferred working directory name here]"
attach(e, name = 'global.dir') 
```

The imputations used in this code is computationally heavy and requires parallel processing. Ensure the following packages are loaded into your library:
- `randomForest`
- `doRNG`
- `foreach`
- `doParallel` 
