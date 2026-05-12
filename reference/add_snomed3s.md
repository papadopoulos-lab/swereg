# Add SNOMED-CT version 3 codes to skeleton

Searches for specific SNOMED-CT (Systematized Nomenclature of Medicine -
Clinical Terms) version 3 codes in Swedish hospital registry data and
adds corresponding boolean variables to the skeleton. SNOMED-CT v3
provides standardized clinical terminology for procedures, findings, and
diagnoses used in Swedish healthcare records.

## Usage

``` r
add_snomed3s(skeleton, dataset, id_name, codes = list(), snomed3s = NULL)
```

## Arguments

- skeleton:

  A data.table containing the main skeleton structure created by
  [`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)

- dataset:

  A data.table containing hospital registry data with SNOMED-CT v3
  codes. Must have columns for person ID, date variables, and SNOMED-CT
  v3 code column (snomed3)

- id_name:

  Character string specifying the name of the ID variable in the dataset

- codes:

  Named list of SNOMED-CT v3 code patterns. Names become column names in
  the skeleton; values are character vectors of code prefixes. Matching
  is prefix-only via
  [`startsWith()`](https://rdrr.io/r/base/startsWith.html);
  `"!"`-prefixed patterns act as row-level vetoes. See
  [`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  for the full pattern-syntax description.

  Examples:

  - `"80146002"` – appendectomy procedure.

  - `"44054006"` – diabetes mellitus type 2.

- snomed3s:

  Deprecated. Use `codes` instead.

## Value

The skeleton data.table is modified by reference with SNOMED-CT v3
variables added. New boolean variables are created for each SNOMED-CT
pattern, TRUE when code is present.

## See also

[`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
for creating the skeleton structure,
[`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
for ICD-10 diagnosis codes,
[`add_snomedo10s`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)
for SNOMED-CT version 10 codes,
[`make_lowercase_names`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
for data preprocessing

Other data_integration:
[`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_quality_registry()`](https://papadopoulos-lab.github.io/swereg/reference/add_quality_registry.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
[`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)

## Examples

``` r
# Load fake data
data("fake_person_ids", package = "swereg")
data("fake_diagnoses", package = "swereg")
swereg::make_lowercase_names(fake_diagnoses, date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.

# Create skeleton
skeleton <- create_skeleton(fake_person_ids[1:10], "2020-01-01", "2020-12-31")

# Add SNOMED-CT v3 codes for specific clinical concepts
snomed_codes <- list(
  "appendectomy" = c("80146002"),
  "diabetes_t2" = c("44054006")
)
add_snomed3s(skeleton, fake_diagnoses, "lopnr", snomed_codes)
```
