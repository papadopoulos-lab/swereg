# Loop 1: Create trial panels from skeleton files

For each file_id (follow-up x age group combination), processes all
skeleton files in parallel using callr::r_bg() subprocesses. Each
subprocess starts with a clean OpenMP state, avoiding the
fork+data.table segfault problem.

## Usage

``` r
tte_generate_trials(
  ett,
  files,
  confounder_vars,
  global_max_isoyearweek,
  process_fn,
  output_dir,
  period_width = 4L,
  impute_fn = tte_impute_confounders,
  n_workers = 3L,
  swereg_dev_path = NULL
)
```

## Arguments

- ett:

  data.table from tte_grid()

- files:

  character vector of skeleton file paths

- confounder_vars:

  character vector of confounder column names

- global_max_isoyearweek:

  integer administrative censoring boundary

- process_fn:

  callback function with signature function(file_path, design, file_id,
  age_range, n_threads) returning a TTETrial

- output_dir:

  directory for output files

- period_width:

  integer collapse period width (default: 4L)

- impute_fn:

  imputation callback, or NULL to skip imputation. Default:
  \[tte_impute_confounders\]. Called as \`impute_fn(trial,
  confounder_vars)\`.

- n_workers:

  integer number of concurrent subprocesses (default: 3L)

- swereg_dev_path:

  path to local swereg dev copy, or NULL for library(swereg)

## Details

After all files are processed, combines trial objects via tte_rbind +
tte_collapse, optionally imputes missing confounders, and saves raw +
imp files.
