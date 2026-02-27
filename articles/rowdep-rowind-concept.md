# Variable types: rowdep vs rowind

``` r
library(swereg)
#> swereg 26.2.27
#> https://papadopoulos-lab.github.io/swereg/
library(data.table)
```

## Introduction

In longitudinal registry data analysis with swereg, variables can be
classified into two fundamental types:

- **rowdep** (row-dependent): Variables that can change over time for a
  person
- **rowind** (row-independent): Variables that cannot change over time
  for a person

Understanding this distinction is crucial for effective analysis of
Swedish registry data, particularly during the `skeleton2_clean` phase
where many transformations convert `rowdep` variables into `rowind`
variables.

## The concept

### Row-dependent (rowdep) variables

These variables can have different values across time periods for the
same person:

- **Education level** (`rowdep_edu_cat`): A person’s education can
  improve over time
- **Income** (`rowdep_income_inflation_adjusted`): Income changes
  annually
- **Had diagnosis this week** (`diag_gd_icd10_F64_089`): TRUE/FALSE
  depending on the specific week
- **Current age** (`age`): Increases continuously over time

### Row-independent (rowind) variables

These variables have the same value across all time periods for a
person:

- **Age at first diagnosis** (`rowind_age_first_gd`): Fixed once the
  diagnosis occurs
- **Year of first diagnosis** (`rowind_isoyear_first_gd`): Fixed
  historical fact
- **Birth country** (`rowind_birthcountry`): Never changes
- **Register classification** (`rowind_register_tag`): Person’s role in
  the study (case, control, etc.)
- **Age at death** (`rowind_age_death`): Fixed once death occurs

## Why this matters

In longitudinal data, you often need to:

1.  **Identify when something first happened** (e.g., first diagnosis,
    first prescription)
2.  **Capture characteristics at specific time points** (e.g., education
    level when first diagnosed)
3.  **Create person-level summaries** that don’t vary by time period

Converting `rowdep` → `rowind` allows you to create stable, person-level
characteristics for analysis.

## Common transformation patterns

### Pattern 1: First occurrence transformations

The most common transformation finds the first time a condition is TRUE
and extracts a value from that time point. This works for any
condition - time-based, value-based, or complex combinations.

#### Time-based conditions

``` r
# Age at first GD diagnosis for AFAB individuals
make_rowind_first_occurrence(skeleton_gd,
                            condition = "diag_gd_icd10_F64_089 == TRUE & is_amab == FALSE",
                            value_var = "age", 
                            new_var = "rowind_age_first_gd_afab")

# Education level at time of first diagnosis
make_rowind_first_occurrence(skeleton_gd,
                            condition = "isoyearweek == rowind_isoyearweek_first_gd",
                            value_var = "rowdep_edu_cat", 
                            new_var = "rowind_edu_at_first_dx")
```

#### Manual approach (for understanding)

``` r
# The helper function does this automatically:
skeleton_gd[diag_gd_icd10_F64_089 == TRUE & is_amab == FALSE, temp := age]
skeleton_gd[, rowind_age_first_gd_afab := swereg::first_non_na(temp), by = .(id)]
skeleton_gd[, temp := NULL]  # Always clean up temp variable
```

### Pattern 2: Simple renaming (already rowind)

When variables are already row-independent but need consistent naming:

``` r
# If date of birth is truly the same for all rows (as it should be)
setnames(skeleton_gd, "dob", "rowind_dob")

# If birth country was added from demographics (already person-level)
setnames(skeleton_gd, "birth_country", "rowind_birth_country")
```

**Important**: Only use this when the variable is genuinely the same
across all rows for each person. If values differ, you have a data
integration problem to fix first.

## Practical example with fake data

Let’s demonstrate these concepts using the fake datasets included in
swereg:

``` r
# Create a small skeleton for demonstration
ids <- swereg::fake_demographics$lopnr[1:5]
skeleton <- create_skeleton(ids, "2020-01-01", "2020-03-31")

# Add demographic data (creates rowind variables)
fake_demographics <- swereg::fake_demographics |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "doddatum")
#> Found additional date columns not in date_columns: fodelseman. Consider adding them for automatic date parsing.
add_onetime(skeleton, fake_demographics, "lopnr")

# Add diagnosis data (creates rowdep variables)
fake_diagnoses <- swereg::fake_diagnoses |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.
add_diagnoses(skeleton, fake_diagnoses, "lopnr", 
             diags = list("f64_diag" = "^F64"))

# Examine the structure
print("Skeleton structure:")
#> [1] "Skeleton structure:"
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

Now let’s create rowind variables:

``` r
# Example 1: ISO year of first F64 diagnosis  
make_rowind_first_occurrence(skeleton,
                            condition = "f64_diag == TRUE",
                            value_var = "isoyear", 
                            new_var = "rowind_isoyear_first_f64")

# Example 2: ISO year-week of first F64 diagnosis
make_rowind_first_occurrence(skeleton,
                            condition = "f64_diag == TRUE",
                            value_var = "isoyearweek", 
                            new_var = "rowind_isoyearweek_first_f64")

# View results
print("Results with rowind variables:")
#> [1] "Results with rowind variables:"
head(skeleton[id == ids[1], .(id, isoyear, isoyearweek, f64_diag, 
                              rowind_isoyear_first_f64, rowind_isoyearweek_first_f64)], 8)
#>       id isoyear isoyearweek f64_diag rowind_isoyear_first_f64
#>    <int>   <int>      <char>   <lgcl>                    <int>
#> 1:     1    1900     1900-**    FALSE                       NA
#> 2:     1    1901     1901-**    FALSE                       NA
#> 3:     1    1902     1902-**    FALSE                       NA
#> 4:     1    1903     1903-**    FALSE                       NA
#> 5:     1    1904     1904-**    FALSE                       NA
#> 6:     1    1905     1905-**    FALSE                       NA
#> 7:     1    1906     1906-**    FALSE                       NA
#> 8:     1    1907     1907-**    FALSE                       NA
#>    rowind_isoyearweek_first_f64
#>                          <char>
#> 1:                         <NA>
#> 2:                         <NA>
#> 3:                         <NA>
#> 4:                         <NA>
#> 5:                         <NA>
#> 6:                         <NA>
#> 7:                         <NA>
#> 8:                         <NA>
```

Notice how: - `f64_diag` varies by week (rowdep) -
`rowind_isoyear_first_f64` and `rowind_isoyearweek_first_f64` are
constant for each person (rowind)

## Best practices

### 1. Naming conventions

Use clear prefixes to distinguish variable types: - `rowdep_*` for
time-varying variables - `rowind_*` for time-invariant variables

### 2. Always clean up temporary variables

When using manual transformations, always remove temp variables:

``` r
skeleton[condition == TRUE, temp := value]
skeleton[, new_rowind_var := first_non_na(temp), by = .(id)]
skeleton[, temp := NULL]  # Critical: clean up
```

### 3. Use helper functions when possible

The
[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
function handles temp variable management automatically and reduces
errors.

### 4. Validate your transformations

Always check that your rowind variables are actually row-independent:

``` r
# This should return 1 for all persons (meaning all rows have same value)
skeleton[, .(unique_values = uniqueN(rowind_isoyear_first_f64)), by = .(id)]
```

## Integration with swereg workflow

The rowdep/rowind concept fits into the standard swereg workflow:

1.  **skeleton1_create**: Focus on data integration, creates mostly
    rowdep variables
2.  **skeleton2_clean**: Heavy focus on rowdep → rowind
    transformations  
3.  **skeleton3_analyze**: Work with clean rowind variables for analysis

The `skeleton2_clean` phase is where most rowdep → rowind
transformations occur, as you prepare stable person-level
characteristics for downstream analysis.

## Conclusion

Understanding the distinction between rowdep and rowind variables is
fundamental to effective longitudinal registry data analysis. The
transformation patterns shown here, especially when combined with helper
functions like
[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md),
make it easier to create robust, analysis-ready datasets from complex
Swedish registry data.

For more advanced workflows, see the other swereg vignettes on skeleton
creation, data integration, and memory-efficient processing.
