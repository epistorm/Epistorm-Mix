# Imputations of Missing Contact Patterns Data

This folder contains the pipeline script that will conduct the full imputations for the raw data.

## Requirements

Download the imputations folder. Open the `_pipeline_imputations.R` script and change the global directory.

``` r
e = new.env() 
e$global.dir = "your-path/Epistorm-Mix/code/imputations/"
attach(e, name = 'global.dir') 
```

