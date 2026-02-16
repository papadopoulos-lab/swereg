# Impute missing confounders by sampling from observed values

For each confounder variable, identifies trials with missing values and
replaces them by sampling (with replacement) from observed values across
all trials. Operates at the trial level (one value per trial_id), then
merges imputed values back into the full panel data.

## Usage

``` r
tte_impute_confounders(trial, confounder_vars, seed = 4L)
```

## Arguments

- trial:

  A TTETrial object

- confounder_vars:

  character vector of confounder column names to impute

- seed:

  integer seed for reproducibility (default: 4L)

## Value

Modified TTETrial object with imputed confounder values
