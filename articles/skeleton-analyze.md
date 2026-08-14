# Analysing the skeleton

``` r
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
```

## Introduction

This vignette demonstrates how to work with a finished skeleton –
collapsing weekly data to yearly summaries, creating analysis datasets,
and running common epidemiological queries.

**Prerequisites**: Complete
[`vignette("skeleton-create")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-create.md)
first, or use the `RegistryStudy` pipeline from
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md).

## Build a skeleton for this vignette

We create a small skeleton with the same steps as
[`vignette("skeleton-create")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-create.md),
condensed here for convenience:

``` r
skeleton <- swereg::create_skeleton(swereg::fake_person_ids, "2015-01-01", "2020-12-31")

# Demographics
fake_demographics <- swereg::fake_demographics |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "fodelseman")
swereg::add_onetime(skeleton, fake_demographics, id_name = "lopnr")

# Diagnoses
fake_diagnoses <- swereg::fake_diagnoses |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.
swereg::add_diagnoses(skeleton, fake_diagnoses, id_name = "lopnr",
  diags = list(
    "depression" = c("F32", "F33"),
    "anxiety" = c("F40", "F41"),
    "gender_dysphoria" = c("F64"),
    "diabetes" = c("E10", "E11")
  ))
#> Warning: 'diags' is deprecated, use 'codes' instead.

# Prescriptions
fake_prescriptions <- swereg::fake_prescriptions |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "edatum")
swereg::add_rx(skeleton, fake_prescriptions, id_name = "p444_lopnr_personnr",
  rxs = list(
    "antidepressants" = c("N06A"),
    "lipid_lowering" = c("C10AA"),
    "hormones" = c("G03")
  ))
#> Warning: 'rxs' is deprecated, use 'codes' instead.

# Cause of death
fake_cod <- swereg::fake_cod |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "dodsdat")
swereg::add_cods(skeleton, fake_cod, id_name = "lopnr",
  cods = list(
    "external_death" = c("X60", "X70"),
    "cardiovascular_death" = c("I21", "I22")
  ))
#> Warning: 'cods' is deprecated, use 'codes' instead.

# Derived variables
skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
skeleton[, age := isoyear - birth_year]
skeleton[, any_mental_health := depression | anxiety]
skeleton[, depression_treated := depression & antidepressants]
skeleton[, death_any := external_death | cardiovascular_death]
skeleton[, life_stage := fcase(
  age < 18, "child",
  age >= 18 & age < 65, "adult",
  age >= 65, "elderly",
  default = "unknown"
)]
skeleton[, c("fodelseman", "birth_year") := NULL]

# Filter
skeleton <- skeleton[age >= 0 & age <= 100 & isoyear >= 2015]

cat("Skeleton ready:", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")
#> Skeleton ready: 315000 rows, 21 columns
```

## Weekly vs yearly rows

The skeleton contains both weekly and yearly rows. The `is_isoyear`
column distinguishes them:

``` r
cat("Weekly rows:", sum(!skeleton$is_isoyear), "\n")
#> Weekly rows: 314000
cat("Yearly rows:", sum(skeleton$is_isoyear), "\n")
#> Yearly rows: 1000
```

Weekly rows carry event-level precision (diagnosis this week,
prescription active this week). Yearly rows carry annually-updated data
(income, education, family type). Most analyses need the data collapsed
to one granularity.

## Collapsing weekly data to person-years

The most common aggregation: collapse weekly boolean indicators into
yearly summaries answering “did anything happen this year?”

Use
[`swereg::max_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/max_with_infinite_as_na.md)
for boolean/logical columns – it returns `TRUE` if any week was `TRUE`,
`FALSE` if all were `FALSE`, and `NA` if all were `NA`:

``` r
person_years <- skeleton[, .(
  age            = swereg::first_non_na(age),
  life_stage     = swereg::first_non_na(life_stage),

  # "Did this happen at any point during the year?"
  depression     = swereg::max_with_infinite_as_na(depression),
  anxiety        = swereg::max_with_infinite_as_na(anxiety),
  diabetes       = swereg::max_with_infinite_as_na(diabetes),
  any_mental_health   = swereg::max_with_infinite_as_na(any_mental_health),
  gender_dysphoria    = swereg::max_with_infinite_as_na(gender_dysphoria),
  antidepressants     = swereg::max_with_infinite_as_na(antidepressants),
  lipid_lowering      = swereg::max_with_infinite_as_na(lipid_lowering),
  hormones            = swereg::max_with_infinite_as_na(hormones),
  depression_treated  = swereg::max_with_infinite_as_na(depression_treated),
  death_any           = swereg::max_with_infinite_as_na(death_any)
), by = .(id, isoyear)]

cat("Person-year dataset:", nrow(person_years), "rows\n")
#> Person-year dataset: 6000 rows
head(person_years)
#>       id isoyear   age life_stage depression anxiety diabetes any_mental_health
#>    <int>   <int> <num>     <char>      <int>   <int>    <int>             <int>
#> 1:     1    2015    59      adult          0       0        0                 0
#> 2:     1    2016    60      adult          0       0        0                 0
#> 3:     1    2017    61      adult          0       0        0                 0
#> 4:     1    2018    62      adult          0       0        0                 0
#> 5:     1    2019    63      adult          0       0        0                 0
#> 6:     1    2020    64      adult          0       0        0                 0
#>    gender_dysphoria antidepressants lipid_lowering hormones depression_treated
#>               <int>           <int>          <int>    <int>              <int>
#> 1:                0               0              0        0                  0
#> 2:                0               0              0        0                  0
#> 3:                0               0              0        0                  0
#> 4:                0               0              0        1                  0
#> 5:                0               0              0        1                  0
#> 6:                0               0              0        1                  0
#>    death_any
#>        <int>
#> 1:         0
#> 2:         0
#> 3:         0
#> 4:         0
#> 5:         0
#> 6:         0
```

## Example analyses on the person-year dataset

### Prevalence by year

``` r
prevalence <- person_years[, .(
  n = .N,
  depression_prev = mean(depression, na.rm = TRUE),
  anxiety_prev    = mean(anxiety, na.rm = TRUE),
  treatment_rate  = ifelse(
    sum(depression, na.rm = TRUE) > 0,
    mean(depression_treated[depression == TRUE], na.rm = TRUE),
    NA_real_
  )
), by = .(isoyear)]

print(prevalence[order(isoyear)])
#>    isoyear     n depression_prev anxiety_prev treatment_rate
#>      <int> <int>           <num>        <num>          <num>
#> 1:    2015  1000           0.010        0.009              0
#> 2:    2016  1000           0.010        0.007              0
#> 3:    2017  1000           0.004        0.009              0
#> 4:    2018  1000           0.010        0.003              0
#> 5:    2019  1000           0.008        0.007              0
#> 6:    2020  1000           0.004        0.007              0
```

### Prevalence by life stage

``` r
by_stage <- person_years[, .(
  n = .N,
  depression_prev     = mean(depression, na.rm = TRUE),
  antidepressant_use  = mean(antidepressants, na.rm = TRUE),
  mean_age            = mean(age, na.rm = TRUE)
), by = .(life_stage)]

print(by_stage)
#>    life_stage     n depression_prev antidepressant_use mean_age
#>        <char> <int>           <num>              <num>    <num>
#> 1:      adult  5062     0.008297116         0.07506914 40.70802
#> 2:    elderly   422     0.000000000         0.06161137 66.76303
#> 3:      child   516     0.007751938         0.09108527 14.70155
```

### Treatment patterns among those with any mental health condition

``` r
treatment <- person_years[any_mental_health == TRUE, .(
  antidepressant_use = mean(antidepressants, na.rm = TRUE),
  lipid_lowering_use = mean(lipid_lowering, na.rm = TRUE),
  hormone_use        = mean(hormones, na.rm = TRUE),
  mean_age           = mean(age, na.rm = TRUE),
  n                  = .N
), by = .(isoyear)]

print(treatment[order(isoyear)])
#>    isoyear antidepressant_use lipid_lowering_use hormone_use mean_age     n
#>      <int>              <num>              <num>       <num>    <num> <int>
#> 1:    2015         0.16666667          0.0000000   0.2222222 38.88889    18
#> 2:    2016         0.00000000          0.0625000   0.1250000 34.68750    16
#> 3:    2017         0.07692308          0.0000000   0.2307692 36.92308    13
#> 4:    2018         0.23076923          0.0000000   0.1538462 40.76923    13
#> 5:    2019         0.00000000          0.0000000   0.3571429 43.92857    14
#> 6:    2020         0.09090909          0.2727273   0.2727273 38.45455    11
```

## Working directly with weekly data

Some analyses need weekly resolution – for example, time-to-event models
or exposure-lag analyses. Use the weekly rows directly:

``` r
weekly <- skeleton[is_isoyear == FALSE]
cat("Weekly dataset:", nrow(weekly), "rows\n")
#> Weekly dataset: 314000 rows

# Weeks with active depression treatment
cat("Person-weeks on antidepressants:",
    sum(weekly$antidepressants, na.rm = TRUE), "\n")
#> Person-weeks on antidepressants: 3774
```

## Creating a person-level baseline table

Collapse to one row per person for baseline characteristics:

``` r
baseline <- person_years[, .(
  first_year     = min(isoyear),
  last_year      = max(isoyear),
  follow_up_years = .N,
  ever_depression = any(depression, na.rm = TRUE),
  ever_anxiety    = any(anxiety, na.rm = TRUE),
  ever_gd         = any(gender_dysphoria, na.rm = TRUE),
  died            = any(death_any, na.rm = TRUE)
), by = .(id)]

cat("Baseline table:", nrow(baseline), "individuals\n")
#> Baseline table: 1000 individuals
print(head(baseline))
#>       id first_year last_year follow_up_years ever_depression ever_anxiety
#>    <int>      <int>     <int>           <int>          <lgcl>       <lgcl>
#> 1:     1       2015      2020               6           FALSE        FALSE
#> 2:     2       2015      2020               6           FALSE        FALSE
#> 3:     3       2015      2020               6           FALSE        FALSE
#> 4:     4       2015      2020               6           FALSE        FALSE
#> 5:     5       2015      2020               6           FALSE        FALSE
#> 6:     6       2015      2020               6           FALSE        FALSE
#>    ever_gd   died
#>     <lgcl> <lgcl>
#> 1:   FALSE  FALSE
#> 2:   FALSE  FALSE
#> 3:    TRUE  FALSE
#> 4:   FALSE  FALSE
#> 5:   FALSE  FALSE
#> 6:   FALSE  FALSE
```

## Summary

The two-step manual workflow:

1.  **Create the skeleton**
    ([`vignette("skeleton-create")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-create.md)):
    build the time grid, integrate registry data, derive variables
2.  **Analyse the skeleton** (this vignette): collapse to the right
    granularity and run queries

For production-scale pipelines with incremental rebuilds, see
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md).
