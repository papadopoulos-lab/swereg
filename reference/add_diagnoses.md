# Add diagnosis data to skeleton

Searches for specific ICD diagnosis codes in Swedish hospital registry
data and adds corresponding boolean variables to the skeleton. Can
search in main diagnoses only or both main and secondary diagnoses.

## Usage

``` r
add_diagnoses(
  skeleton,
  dataset,
  id_name,
  diag_type = "both",
  diags = list(icd10_F64_0 = c("F640"), icd10_F64_89 = c("F6489"), icd10_F64_089 =
    c("F640", "F648", "F649"))
)
```

## Arguments

- skeleton:

  A data.table containing the main skeleton structure created by
  [`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)

- dataset:

  A data.table containing hospital registry data with diagnosis codes.
  Must have columns for person ID, admission date (`indatum`), and at
  least one diagnosis code column. Expected diagnosis columns after
  [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md):
  `hdia` (main), `dia1`/`dia2`/etc (secondary), `ekod1`/`ekod2`/etc
  (external causes), `icd7*`, `icd9*`

- id_name:

  Character string specifying the name of the ID variable in the dataset

- diag_type:

  Character string specifying which diagnosis types to search:

  - "both" (default) - Search in main (`hdia`), secondary (`dia*`),
    external cause (`ekod*`), and historical ICD version columns
    (`icd7*`, `icd9*`)

  - "main" - Search only in main diagnosis column (`hdia`)

- diags:

  Named list of ICD code patterns to search for. Names become variable
  names in skeleton. Patterns should NOT include "^" prefix
  (automatically added). Use exclusions with "!" prefix. Example:
  `list("depression" = c("F32", "F33"), "anxiety" = c("F40", "F41"))`

## Value

The skeleton data.table is modified by reference with diagnosis
variables added. New boolean variables are created for each diagnosis
pattern, TRUE when diagnosis is present.

## Details

The function searches across different diagnosis code column types based
on the `diag_type` parameter:

- When `diag_type = "both"`: Searches in `hdia` (main diagnosis),
  `dia1, dia2, ...` (secondary diagnoses), `ekod1, ekod2, ...` (external
  cause codes), `icd7*` (ICD-7 codes), and `icd9*` (ICD-9 codes)

- When `diag_type = "main"`: Searches only in `hdia` (main diagnosis)

## See also

[`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
for creating the skeleton structure,
[`add_operations`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md)
for surgical procedures,
[`add_rx`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
for prescription data,
[`make_lowercase_names`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
for data preprocessing

Other data_integration:
[`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
[`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
[`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)

## Examples

``` r
# Load fake data
data("fake_person_ids", package = "swereg")
data("fake_inpatient_diagnoses", package = "swereg")
swereg::make_lowercase_names(fake_inpatient_diagnoses, date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.

# Create skeleton
skeleton <- create_skeleton(fake_person_ids[1:10], "2020-01-01", "2020-12-31")

# Add diagnoses
diag_patterns <- list(
  "depression" = c("F32", "F33"),
  "anxiety" = c("F40", "F41")
)
add_diagnoses(skeleton, fake_inpatient_diagnoses, "lopnr", "both", diag_patterns)
```
