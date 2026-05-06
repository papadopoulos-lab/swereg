# Variable types: rd vs ri

``` r
library(swereg)
#> swereg 26.5.6
#> https://papadopoulos-lab.github.io/swereg/
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
```

## Introduction

In longitudinal registry data analysis with swereg, every derived
variable is either **row-dependent** (can change value across rows for
the same person) or **row-independent** (fixed per person). The
distinction is so important that swereg uses short name prefixes to make
it visible at the call site:

- **`rd_`** – row-dependent (also called “rowdep”). Variables that can
  change over time for a person.
- **`ri_`** – row-independent (also called “rowind”). Variables that are
  fixed per person.

Understanding this distinction is crucial when preparing analysis-ready
datasets: many transformations convert `rd_` variables into `ri_`
variables by capturing a value at a specific moment (“age at first
diagnosis”, “year of death”).

## The concept

### Row-dependent (`rd_*`) variables

These variables can have different values across time periods for the
same person:

- **Education level** (`rd_education`): can improve over time
- **Income** (`rd_income_inflation_adjusted`): changes annually
- **Had diagnosis this week** (`f20_diag`): TRUE/FALSE depending on the
  specific week
- **Current age** (`rd_age_continuous`): increases continuously
- **Civil status** (`rd_civil_status`): can change on marriage, divorce,
  bereavement
- **Region of residence** (`rd_region`): changes on relocation

### Row-independent (`ri_*`) variables

These variables have the same value across all time periods for a
person:

- **Age at first diagnosis** (`ri_age_first_dx`): fixed once it happens
- **Year of first diagnosis** (`ri_isoyear_first_dx`): fixed historical
  fact
- **Birth country** (`ri_birthcountry`): never changes
- **Cohort classification** (`ri_register_tag`): person’s role in the
  study (case, control, sibling, etc.)
- **Age at death** (`ri_age_death`): fixed once death occurs
- **Sex at birth** (`ri_sex`): constant

## Why this matters

In longitudinal data, you often need to:

1.  **Identify when something first happened** (first diagnosis, first
    prescription).
2.  **Capture characteristics at specific time points** (education level
    when first diagnosed).
3.  **Create person-level summaries** that don’t vary by time period.

Converting `rd_` -\> `ri_` lets you create stable, person-level
characteristics for downstream analysis – for example, to use as
baseline covariates in a target trial emulation, where the enrollment
time zero fixes which row’s `rd_` values you care about.

## Common transformation patterns

### Pattern 1: First-occurrence transformations

The most common transformation finds the first time a condition is TRUE
and extracts a value from that time point. This works for any condition
– time-based, value-based, or complex combinations.

``` r
# Age at first F64 diagnosis restricted to AFAB individuals
make_rowind_first_occurrence(skeleton,
                             condition = "f64_diag == TRUE & ri_is_amab == FALSE",
                             value_var = "rd_age_continuous",
                             new_var   = "ri_age_first_f64_afab")

# Education level at the time of first diagnosis
make_rowind_first_occurrence(skeleton,
                             condition = "isoyearweek == ri_isoyearweek_first_dx",
                             value_var = "rd_education",
                             new_var   = "ri_education_at_first_dx")
```

#### Manual approach (for understanding)

[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
is a thin wrapper. The manual equivalent is:

``` r
skeleton[f64_diag == TRUE & ri_is_amab == FALSE, temp := rd_age_continuous]
skeleton[, ri_age_first_f64_afab := swereg::first_non_na(temp), by = .(id)]
skeleton[, temp := NULL]
```

The helper function just automates the three-line pattern and guarantees
the temporary column is cleaned up.

### Pattern 2: Simple renaming (already row-independent)

When a variable is already row-independent but needs consistent naming:

``` r
# If date of birth is truly the same for all rows (it should be)
data.table::setnames(skeleton, "dob", "ri_dob")

# If birth country was added via add_onetime() and is person-level
data.table::setnames(skeleton, "birth_country", "ri_birthcountry")
```

**Important**: only use this when the variable is genuinely the same
across all rows for each person. If values differ, you have a
data-integration problem to fix first, not a renaming one.

## Practical example with fake data

``` r
# Create a small skeleton for demonstration
ids <- swereg::fake_demographics$lopnr[1:5]
skeleton <- create_skeleton(ids, "2020-01-01", "2020-03-31")

# Add demographic data (creates ri_ variables)
fake_demographics <- swereg::fake_demographics |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "doddatum")
#> Found additional date columns not in date_columns: fodelseman. Consider adding them for automatic date parsing.
add_onetime(skeleton, fake_demographics, "lopnr")

# Add diagnosis data (creates rd_ variables — the diag column
# varies week-by-week)
fake_diagnoses <- swereg::fake_diagnoses |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.
add_diagnoses(skeleton, fake_diagnoses, "lopnr",
              codes = list(f64_diag = "^F64"))

head(skeleton[id == ids[1]], 8)
#>       id isoyear isoyearweek is_isoyear isoyearweeksun personyears fodelseman
#>    <int>   <int>      <char>     <lgcl>         <Date>       <num>     <char>
#> 1:     1    1900     1900-**       TRUE     1900-07-01           1       1959
#> 2:     1    1901     1901-**       TRUE     1901-06-30           1       1959
#> 3:     1    1902     1902-**       TRUE     1902-06-29           1       1959
#> 4:     1    1903     1903-**       TRUE     1903-06-28           1       1959
#> 5:     1    1904     1904-**       TRUE     1904-07-03           1       1959
#> 6:     1    1905     1905-**       TRUE     1905-07-02           1       1959
#> 7:     1    1906     1906-**       TRUE     1906-07-01           1       1959
#> 8:     1    1907     1907-**       TRUE     1907-06-30           1       1959
#>    doddatum f64_diag
#>      <Date>   <lgcl>
#> 1:     <NA>    FALSE
#> 2:     <NA>    FALSE
#> 3:     <NA>    FALSE
#> 4:     <NA>    FALSE
#> 5:     <NA>    FALSE
#> 6:     <NA>    FALSE
#> 7:     <NA>    FALSE
#> 8:     <NA>    FALSE
```

Now create `ri_` variables from the row-dependent `f64_diag`:

``` r
# ISO year of first F64 diagnosis
make_rowind_first_occurrence(skeleton,
                             condition = "f64_diag == TRUE",
                             value_var = "isoyear",
                             new_var   = "ri_isoyear_first_f64")

# ISO year-week of first F64 diagnosis
make_rowind_first_occurrence(skeleton,
                             condition = "f64_diag == TRUE",
                             value_var = "isoyearweek",
                             new_var   = "ri_isoyearweek_first_f64")

head(skeleton[id == ids[1], .(id, isoyear, isoyearweek, f64_diag,
                              ri_isoyear_first_f64,
                              ri_isoyearweek_first_f64)], 8)
#>       id isoyear isoyearweek f64_diag ri_isoyear_first_f64
#>    <int>   <int>      <char>   <lgcl>                <int>
#> 1:     1    1900     1900-**    FALSE                   NA
#> 2:     1    1901     1901-**    FALSE                   NA
#> 3:     1    1902     1902-**    FALSE                   NA
#> 4:     1    1903     1903-**    FALSE                   NA
#> 5:     1    1904     1904-**    FALSE                   NA
#> 6:     1    1905     1905-**    FALSE                   NA
#> 7:     1    1906     1906-**    FALSE                   NA
#> 8:     1    1907     1907-**    FALSE                   NA
#>    ri_isoyearweek_first_f64
#>                      <char>
#> 1:                     <NA>
#> 2:                     <NA>
#> 3:                     <NA>
#> 4:                     <NA>
#> 5:                     <NA>
#> 6:                     <NA>
#> 7:                     <NA>
#> 8:                     <NA>
```

Notice how `f64_diag` varies by week (`rd_` character even though the
actual column name here is just `f64_diag` because it came from a
code-registry entry) while `ri_isoyear_first_f64` and
`ri_isoyearweek_first_f64` are constant for each person.

## Best practices

### 1. Use prefixes consistently

- `rd_` for time-varying variables
- `ri_` for time-invariant variables

Code-registry columns (`ov_*`, `sv_*`, `os_*`, `osd_*`, `rx_*`, `op_*`,
etc.) are de-facto row-dependent but keep their source prefix for
clarity; you don’t need to prefix them a second time with `rd_`.

### 2. Always clean up temporary variables

When using manual transformations, always remove temp variables:

``` r
skeleton[condition == TRUE, temp := value]
skeleton[, new_ri_var := first_non_na(temp), by = .(id)]
skeleton[, temp := NULL]  # critical
```

### 3. Use helper functions when possible

[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
handles temp variable management automatically and reduces errors.

### 4. Validate your transformations

Always check that your `ri_` variables are actually row-independent:

``` r
# Should return 1 for every person -- all rows have the same value
skeleton[, .(unique_values = uniqueN(ri_isoyear_first_f64)), by = .(id)]
```

## How this fits into the swereg pipeline

In the three-phase `RegistryStudy$process_skeletons()` pipeline (see
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md)),
the `rd_` -\> `ri_` conversion happens inside phase 3 (randvars) steps.
A typical pattern:

1.  **Phase 1 – framework** produces the base time grid, including
    `rd_age_continuous` (derived from `isoyearweeksun - dob`).
2.  **Phase 2 – codes** produces the code-derived columns like `os_f20`,
    `osd_i21_to_i24`, `rx_n05a`.
3.  **Phase 3 – randvars** produces both:
    - `rd_*` columns from LISA (annual demographics).
    - `ri_*` columns from first-occurrence transformations.

The `ri_*` columns are then the baseline covariates consumed by target
trial emulation in
[`vignette("tte-workflow")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md).

## Conclusion

The `rd_` / `ri_` distinction is the scaffolding that makes person-week
skeletons usable as TTE inputs: the `ri_` columns become your stable
per-person baseline characteristics, and the `rd_` columns are the
time-varying surface the enrollment procedure slices at candidate
time-zeros.
[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
is the workhorse that bridges the two.
