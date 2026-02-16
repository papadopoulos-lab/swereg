# Build the full ETT (Emulated Target Trial) grid

Takes project-specific inputs and returns a data.table with one row per
outcome x follow-up x age-group combination, including file naming
columns.

## Usage

``` r
tte_grid(outcomes_dt, follow_up_weeks, age_groups, project_prefix)
```

## Arguments

- outcomes_dt:

  data.table with columns \`outcome_var\` and \`outcome_name\`

- follow_up_weeks:

  integer vector of follow-up durations (e.g., c(52, 104, 156))

- age_groups:

  named list mapping group labels to c(min_age, max_age)

- project_prefix:

  string used for file naming (e.g., "project002_ozel_psychosis")

## Value

data.table with columns: file_id, ett_id, age_group, follow_up,
outcome_var, outcome_name, description, file_raw, file_imp,
file_analysis
