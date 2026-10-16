# Variable types: rd vs ri

``` r
library(swereg)
#> swereg 27.2.4
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
variable is either **row-dependent** (its value can change across rows
for the same person) or **row-independent** (fixed per person). swereg
uses short name prefixes to make the distinction visible at the call
site:

- **`rd_`**: row-dependent (also called “rowdep”). Variables that can
  change over time for a person.
- **`ri_`**: row-independent (also called “rowind”). Variables that are
  fixed per person.

Many transformations for analysis-ready datasets convert `rd_` variables
into `ri_` variables. Each one takes the value at a specific moment
(“age at first diagnosis”, “year of death”).

## The concept

### Row-dependent (`rd_*`) variables

These variables can have different values across time periods for the
same person:

- **Education level** (`rd_education`): can improve over time
- **Income** (`rd_income_inflation_adjusted`): changes annually
- **Had diagnosis this week** (`e11_diag`): TRUE/FALSE depending on the
  specific week
- **Current age** (`rd_age_continuous`): increases continuously
- **Civil status** (`rd_civil_status`): can change on marriage, divorce,
  bereavement
- **Region of residence** (`rd_region`): changes on relocation

### Row-independent (`ri_*`) variables

These variables have the same value across all time periods for a
person:

- **Age at first diagnosis** (`ri_age_first_dx`): fixed once it happens
- **Year of first diagnosis** (`ri_isoyear_first_dx`): fixed once it
  happens
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

A conversion from `rd_` to `ri_` gives a person-level characteristic
that has the same value on every row. Later analyses use it, for example
as a baseline covariate in a target trial emulation. There, the
enrollment time zero sets which row’s `rd_` values apply.

## Common transformation patterns

### Pattern 1: First-occurrence transformations

The most common transformation finds the first time a condition is TRUE
and takes a value from that time point. The condition can be time-based,
value-based, or any combination of the two.

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

The helper function runs this three-line pattern and always removes the
temporary column.

### Pattern 2: Simple renaming (already row-independent)

When a variable is already row-independent but needs consistent naming:

``` r
# If date of birth is truly the same for all rows (it should be)
data.table::setnames(skeleton, "dob", "ri_dob")

# If birth country was added via add_onetime() and is person-level
data.table::setnames(skeleton, "birth_country", "ri_birthcountry")
```

Use this only when the variable has the same value on all rows for each
person. If values differ, fix the data integration first.

## Practical example with fake data

``` r
# Create a small skeleton for demonstration
ids <- swereg::fake_demographics$lopnr[1:5]
skeleton <- create_skeleton(ids, 2020, "2020-14")

# Add demographic data (creates ri_ variables)
fake_demographics <- swereg::fake_demographics |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "doddatum")
#> Found additional date columns not in date_columns: fodelseman. Consider adding them for automatic date parsing.
add_onetime(skeleton, fake_demographics, "lopnr")

# Add diagnosis data (creates rd_ variables: the diag column
# varies week-by-week)
fake_diagnoses <- swereg::fake_diagnoses |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.
add_diagnoses(skeleton, fake_diagnoses, "lopnr",
              codes = list(f64_diag = "F64"))

head(skeleton[id == ids[1]], 8)
#>       id isoyear isoyearweek is_isoyear isoyearweeksun personyears fodelseman
#>    <int>   <int>      <char>     <lgcl>         <Date>       <num>     <char>
#> 1:     1    1900     1900-**       TRUE     1900-07-01           1       1956
#> 2:     1    1901     1901-**       TRUE     1901-06-30           1       1956
#> 3:     1    1902     1902-**       TRUE     1902-06-29           1       1956
#> 4:     1    1903     1903-**       TRUE     1903-06-28           1       1956
#> 5:     1    1904     1904-**       TRUE     1904-07-03           1       1956
#> 6:     1    1905     1905-**       TRUE     1905-07-02           1       1956
#> 7:     1    1906     1906-**       TRUE     1906-07-01           1       1956
#> 8:     1    1907     1907-**       TRUE     1907-06-30           1       1956
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
#> 1:     1    1900     1900-**    FALSE                 2000
#> 2:     1    1901     1901-**    FALSE                 2000
#> 3:     1    1902     1902-**    FALSE                 2000
#> 4:     1    1903     1903-**    FALSE                 2000
#> 5:     1    1904     1904-**    FALSE                 2000
#> 6:     1    1905     1905-**    FALSE                 2000
#> 7:     1    1906     1906-**    FALSE                 2000
#> 8:     1    1907     1907-**    FALSE                 2000
#>    ri_isoyearweek_first_f64
#>                      <char>
#> 1:                  2000-**
#> 2:                  2000-**
#> 3:                  2000-**
#> 4:                  2000-**
#> 5:                  2000-**
#> 6:                  2000-**
#> 7:                  2000-**
#> 8:                  2000-**
```

`f64_diag` varies by week, so it is row-dependent. Its name has no `rd_`
prefix because it comes from a code-registry entry.
`ri_isoyear_first_f64` and `ri_isoyearweek_first_f64` are constant for
each person.

## Best practices

### 1. Use prefixes consistently

- `rd_` for time-varying variables
- `ri_` for time-invariant variables

Code-registry columns (`ov_*`, `sv_*`, `os_*`, `osd_*`, `rx_*`, `op_*`,
etc.) are row-dependent, but they keep their source prefix. You do not
need to add an `rd_` prefix to them.

### 2. Always clean up temporary variables

When you transform by hand, always remove the temporary variables:

``` r
skeleton[condition == TRUE, temp := value]
skeleton[, new_ri_var := first_non_na(temp), by = .(id)]
skeleton[, temp := NULL]  # critical
```

### 3. Use helper functions when possible

[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
manages the temporary variable for you, which reduces errors.

### 4. Validate your transformations

Check that your `ri_` variables are row-independent:

``` r
# Should return 1 for every person -- all rows have the same value
skeleton[, .(unique_values = uniqueN(ri_isoyear_first_f64)), by = .(id)]
```

## How this fits into the swereg pipeline

In the four-phase `RegistryStudy$process_skeletons()` pipeline (see
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md)),
the `rd_` -\> `ri_` conversion happens inside phase 3 (randvars) steps.
A typical pattern:

1.  **Phase 1 – framework** produces the base time grid, including
    `rd_age_continuous` (derived from `isoyearweeksun - dob`).
2.  **Phase 1b – trim** is the one phase that MAY delete skeleton rows.
3.  **Phase 2 – codes** produces the code-derived columns such as
    `os_e11`, `osd_i21_to_i24`, `rx_c10aa`.
4.  **Phase 3 – randvars** produces both:
    - `rd_*` columns from LISA (annual demographics).
    - `ri_*` columns from first-occurrence transformations.

The `ri_*` columns are the baseline covariates that target trial
emulation uses in
[`vignette("tte-workflow")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md).

## Conclusion

The `rd_` / `ri_` distinction makes person-week skeletons usable as TTE
inputs. The `ri_` columns hold the per-person baseline characteristics.
The enrollment procedure reads the time-varying `rd_` columns at each
candidate time zero.
[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
creates `ri_` columns from `rd_` columns.
