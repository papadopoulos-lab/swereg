# Building the data skeleton (skeleton1_create)

``` r
library(data.table)
```

## Introduction

This vignette demonstrates **skeleton1_create** - the first stage of the
swereg workflow where raw registry data is integrated into a
time-structured framework.

**Prerequisites**: If you’re new to swereg, start with the “Skeleton
concept” vignette to learn the conceptual foundation.

## What is skeleton1_create?

The skeleton1_create stage provides a time-structured framework with:

- **Individual IDs**: Each person in your study
- **Time structure**: Both ISO years and ISO year-weeks
- **Data integration points**: Framework for merging registry data

This stage focuses on **data integration**, not cleaning - raw registry
data from multiple sources is attached to create the foundation for all
subsequent analysis.

## Step 1: create the basic skeleton

Start by creating a skeleton with your study population and time period:

``` r
# Load example data
data("fake_person_ids", package = "swereg")

# Create skeleton covering 2015-2020
skeleton <- swereg::create_skeleton(
  ids = fake_person_ids,
  date_min = "2015-01-01",
  date_max = "2020-12-31"
)

# Examine the structure
head(skeleton)
#>       id isoyear isoyearweek is_isoyear isoyearweeksun personyears
#>    <int>   <int>      <char>     <lgcl>         <Date>       <num>
#> 1:     1    1900     1900-**       TRUE     1900-07-01           1
#> 2:     1    1901     1901-**       TRUE     1901-06-30           1
#> 3:     1    1902     1902-**       TRUE     1902-06-29           1
#> 4:     1    1903     1903-**       TRUE     1903-06-28           1
#> 5:     1    1904     1904-**       TRUE     1904-07-03           1
#> 6:     1    1905     1905-**       TRUE     1905-07-02           1
cat("Skeleton dimensions:", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")
#> Skeleton dimensions: 430000 rows, 6 columns
```

The skeleton contains:

- `id`: Individual identifier
- `isoyear`: ISO year (for annual data)
- `isoyearweek`: ISO year-week (for weekly data, format “YYYY-WW” or
  “YYYY-\*\*” for annual rows)
- `is_isoyear`: Boolean indicating annual vs weekly rows
- `isoyearweeksun`: Date representing the Sunday (last day) of the ISO
  week/year

## Step 2: add demographic data (one-time)

Demographics don’t change over time, so we add them once per person:

``` r
# Load and prepare demographic data
fake_demographics <- swereg::fake_demographics |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "fodelseman")

# Add to skeleton
swereg::add_onetime(skeleton, fake_demographics, id_name = "lopnr")

# Check what was added
new_vars <- setdiff(names(skeleton), c("id", "isoyear", "isoyearweek", "is_isoyear", "isoyearweeksun"))
cat("Added demographic variables:", paste(new_vars, collapse = ", "), "\n")
#> Added demographic variables: personyears, fodelseman, doddatum
```

## Step 3: add annual data

Some data varies by year (e.g., family status, income):

``` r
# Load annual family data
fake_annual_family <- swereg::fake_annual_family |>
  data.table::copy() |>
  swereg::make_lowercase_names()

# Add annual data for 2015
swereg::add_annual(skeleton, fake_annual_family, id_name = "lopnr", isoyear = 2015)

cat("Annual data added for 2015\n")
#> Annual data added for 2015
```

## Step 4: add diagnosis data

Hospital diagnoses drive most epidemiological studies:

``` r
# Load and prepare diagnosis data
fake_diagnoses <- swereg::fake_diagnoses |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.
fake_diagnoses <- swereg::fake_diagnoses |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.

diagnoses_combined <- data.table::rbindlist(list(
  fake_diagnoses,
  fake_diagnoses
), use.names = TRUE, fill = TRUE)

# Define diagnosis patterns to search for (^ prefix automatically added)
diagnosis_patterns <- list(
  "depression" = c("F32", "F33"),
  "anxiety" = c("F40", "F41"), 
  "gender_dysphoria" = c("F64"),
  "cardiovascular" = c("I10", "I20", "I21")
)

# Add diagnoses to skeleton
swereg::add_diagnoses(
  skeleton,
  diagnoses_combined,
  id_name = "lopnr",
  diags = diagnosis_patterns
)

# Check results
diag_vars <- names(diagnosis_patterns)
for(var in diag_vars) {
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "positive cases\n")
}
#> - depression : 322 positive cases
#> - anxiety : 314 positive cases
#> - gender_dysphoria : 461 positive cases
#> - cardiovascular : 208 positive cases
```

## Step 5: add prescription data

Medication data with treatment duration and ATC code patterns:

``` r
# Load prescription data
fake_prescriptions <- swereg::fake_prescriptions |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "edatum")

# Define drug patterns (ATC codes, ^ prefix automatically added)
drug_patterns <- list(
  "antidepressants" = c("N06A"),
  "hormones" = c("G03"),
  "cardiovascular_drugs" = c("C07", "C08", "C09")
)

# Add prescriptions to skeleton
swereg::add_rx(
  skeleton,
  fake_prescriptions,
  id_name = "p444_lopnr_personnr",
  rxs = drug_patterns
)
#> 2026-02-04 08:55:57.557027 antidepressants
#> 2026-02-04 08:55:57.958767 hormones
#> 2026-02-04 08:55:58.595629 cardiovascular_drugs

# Check prescription usage
rx_vars <- names(drug_patterns)
for(var in rx_vars) {
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "prescription periods\n")
}
#> - antidepressants : 4066 prescription periods
#> - hormones : 15124 prescription periods
#> - cardiovascular_drugs : 2165 prescription periods
```

## Step 6: add surgical operation data

Surgical procedures from hospital records:

``` r
# Add operations (using default gender-affirming surgery codes)
swereg::add_operations(skeleton, fake_diagnoses, "lopnr")

# Check operation counts
operation_vars <- grep("^op_", names(skeleton), value = TRUE)
cat("Operation variables added:", length(operation_vars), "\n")
#> Operation variables added: 9
for(var in operation_vars[1:3]) {  # Show first 3
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "procedures\n")
}
#> - op_afab_mastectomy : 250 procedures
#> - op_afab_breast_reconst_and_other_breast_ops : 0 procedures
#> - op_afab_penis_test_prosth : 0 procedures
```

## Step 7: add cause of death data

For mortality studies:

``` r
# Load cause of death data
fake_cod <- swereg::fake_cod |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "dodsdat")

# Define cause of death patterns (^ prefix automatically added)
cod_patterns <- list(
  "cardiovascular_death" = c("I21", "I22"),
  "external_causes" = c("X60", "X70")
)

# Add to skeleton
swereg::add_cods(
  skeleton,
  fake_cod,
  id_name = "lopnr",
  cods = cod_patterns
)

# Check mortality
cod_vars <- names(cod_patterns)
for(var in cod_vars) {
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "deaths\n")
}
#> - cardiovascular_death : 16 deaths
#> - external_causes : 9 deaths
```

## Completed skeleton1_create

The completed **skeleton1_create** now contains all the raw registry
data integrated into the time structure:

``` r
cat("Final skeleton dimensions:", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")
#> Final skeleton dimensions: 430000 rows, 27 columns

# Example: Show data for one person
person_data <- skeleton[id == fake_person_ids[1] & isoyear == 2018 & !is.na(isoyearweek)]
cat("\nExample data for person", fake_person_ids[1], "in 2018 (first 6 weeks):\n")
#> 
#> Example data for person 1 in 2018 (first 6 weeks):
print(head(person_data[, .(id, isoyearweek, depression, antidepressants, cardiovascular)]))
#> Key: <id, isoyearweek>
#>       id isoyearweek depression antidepressants cardiovascular
#>    <int>      <char>     <lgcl>          <lgcl>         <lgcl>
#> 1:     1     2018-01      FALSE           FALSE          FALSE
#> 2:     1     2018-02      FALSE           FALSE          FALSE
#> 3:     1     2018-03      FALSE           FALSE          FALSE
#> 4:     1     2018-04      FALSE           FALSE          FALSE
#> 5:     1     2018-05      FALSE           FALSE          FALSE
#> 6:     1     2018-06      FALSE           FALSE          FALSE
```

## Key principles for skeleton1_create

1.  **Always use
    [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)**
    after reading registry data
2.  **Sequential integration**: Add data types in logical order
3.  **Pattern matching**: Use regex patterns for medical codes (^ prefix
    automatically added)
4.  **Time structure**: Leverage ISO year-weeks for precise temporal
    analysis
5.  **Raw data focus**: skeleton1_create is about integration, not
    cleaning

## Understanding the output

The skeleton1_create output has several important characteristics:

- **Mixed time granularity**: Both weekly (`is_isoyear == FALSE`) and
  yearly (`is_isoyear == TRUE`) rows
- **Boolean variables**: All diagnosis, prescription, and operation
  variables are TRUE/FALSE
- **Time alignment**: Events are aligned to ISO year-weeks for
  consistency

## Next steps

This **skeleton1_create** provides the foundation for all subsequent
analysis. The raw data is now integrated into a time-structured
framework, ready for cleaning and variable derivation.

**Next stage**: See the “Cleaning and deriving variables
(skeleton2_clean)” vignette to learn how to clean this data and create
analysis-ready variables.

**Production workflows**: For large-scale processing, see the
“Production analysis workflows (skeleton3_analyze)” vignette.
