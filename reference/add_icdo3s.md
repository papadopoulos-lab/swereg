# Add ICD-O-3 oncology codes to skeleton

Searches for specific ICD-O-3 (International Classification of Diseases
for Oncology, 3rd edition) codes in Swedish cancer registry data and
adds corresponding boolean variables to the skeleton. ICD-O-3 is used to
classify malignant neoplasms (cancers) by histological type (morphology)
and anatomical site (topography).

## Usage

``` r
add_icdo3s(skeleton, dataset, id_name, icdo3s = list())
```

## Arguments

- skeleton:

  A data.table containing the main skeleton structure created by
  [`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)

- dataset:

  A data.table containing cancer registry data with ICD-O-3 codes. Must
  have columns for person ID, date variables, and ICD-O-3 code column
  (icdo3)

- id_name:

  Character string specifying the name of the ID variable in the dataset

- icdo3s:

  Named list of ICD-O-3 code patterns to search for. Names become
  variable names in skeleton. ICD-O-3 codes combine morphology (4
  digits + behavior code) and topography (C codes). Examples of pattern
  matching:

  - `"^8140"` - Adenocarcinoma, NOS (morphology code)

  - `"^C50"` - Breast cancer (topography code)

  - `"8500/3"` - Infiltrating duct carcinoma (morphology with behavior)

## Value

The skeleton data.table is modified by reference with ICD-O-3 variables
added. New boolean variables are created for each ICD-O-3 pattern, TRUE
when code is present.

## See also

[`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
for creating the skeleton structure,
[`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
for ICD-10 diagnosis codes,
[`add_operations`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md)
for surgical procedure codes,
[`make_lowercase_names`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
for data preprocessing

Other data_integration:
[`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
[`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
[`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# ICD-O-3 data requires a dataset with an 'icdo3' column
# (not included in fake_inpatient_diagnoses)
data("fake_person_ids", package = "swereg")

# Load your ICD-O-3 cancer registry data
cancer_data <- your_cancer_registry_data
swereg::make_lowercase_names(cancer_data, date_columns = "indatum")

# Create skeleton
skeleton <- create_skeleton(fake_person_ids[1:10], "2020-01-01", "2020-12-31")

# Add ICD-O-3 codes for specific cancer types
cancer_codes <- list(
  "adenocarcinoma" = c("^8140"),
  "breast_cancer" = c("^C50")
)
add_icdo3s(skeleton, cancer_data, "lopnr", cancer_codes)
} # }
```
