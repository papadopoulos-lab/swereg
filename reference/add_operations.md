# Add surgical operation data to skeleton

Searches for specific surgical operation codes in Swedish hospital
registry data and adds corresponding boolean variables to the skeleton.
Includes predefined operation codes relevant to gender-affirming
procedures.

## Usage

``` r
add_operations(
  skeleton,
  dataset,
  id_name,
  codes = list(op_afab_mastectomy = c("HAC10", "HAC20", "HAC99", "HAC15"),
    op_afab_breast_reconst_and_other_breast_ops = c("HAD20", "HAD30", "HAD35", "HAD99",
    "HAE99"), op_afab_penis_test_prosth = c("KFH50", "KGV30", "KGW96", "KGH96"),
    op_afab_internal_genital = c("LCD00", "LCD01", "LCD04", "LCD10", "LCD11", "LCD96",
    "LCD97"), op_afab_colpectomy = c("LED00"),
    op_amab_breast_reconst_and_other_breast_ops = c("HAD00", "HAD10", "HAD99", "HAE00",
    "HAE20", "HAE99"), op_amab_reconst_vag = c("LEE10", "LEE40", "LEE96", 
     "LFE10",
    "LFE96"), op_amab_penis_amp = c("KGC10"), op_amab_larynx = c("DQD40")),
  ops = NULL
)
```

## Arguments

- skeleton:

  A data.table containing the main skeleton structure created by
  [`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)

- dataset:

  A data.table containing hospital registry data with operation codes.
  Must have columns for person ID, date variables, and operation codes
  (op1, op2, etc.)

- id_name:

  Character string specifying the name of the ID variable in the dataset

- codes:

  Named list of operation code patterns. Names become column names in
  the skeleton; values are character vectors of code prefixes. Matching
  is prefix-only via
  [`startsWith()`](https://rdrr.io/r/base/startsWith.html);
  `"!"`-prefixed patterns act as row-level vetoes. See
  [`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  for the full pattern-syntax description (the same matcher is shared
  between `add_diagnoses`, `add_operations`, `add_cods`, `add_icdo3s`,
  `add_snomed3s` and `add_snomedo10s`).

  Default includes comprehensive gender-affirming surgery codes:

  - Mastectomy procedures (HAC10, HAC20, etc.)

  - Breast reconstruction (HAD20, HAD30, etc.)

  - Genital operations (various KFH, KGV, LCD, LED, LEE codes)

  - Larynx operations (DQD40)

- ops:

  Deprecated. Use `codes` instead.

## Value

The skeleton data.table is modified by reference with operation
variables added. New boolean variables are created for each operation
pattern, TRUE when operation is present.

## See also

[`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
for creating the skeleton structure,
[`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
for diagnosis codes,
[`add_rx`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
for prescription data,
[`make_lowercase_names`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
for data preprocessing

Other data_integration:
[`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md),
[`add_quality_registry()`](https://papadopoulos-lab.github.io/swereg/reference/add_quality_registry.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
[`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
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

# Add operations (using default gender-affirming surgery codes)
add_operations(skeleton, fake_diagnoses, "lopnr")
#> Warning: [add_operations] Code(s) with zero matches in source data (pattern may not be understood, or code does not exist in registry):
#>   op_afab_mastectomy: HAC20, HAC99, HAC15
#>   op_afab_breast_reconst_and_other_breast_ops: HAD20, HAD30, HAD35, HAD99, HAE99
#>   op_afab_penis_test_prosth: KFH50, KGV30, KGW96, KGH96
#>   op_afab_internal_genital: LCD00, LCD01, LCD04, LCD10, LCD11, LCD96, LCD97
#>   op_afab_colpectomy: LED00
#>   op_amab_breast_reconst_and_other_breast_ops: HAD00, HAD10, HAD99, HAE00, HAE20, HAE99
#>   op_amab_reconst_vag: LEE10, LEE40, LEE96, LFE10, LFE96
#>   op_amab_penis_amp: KGC10
#>   op_amab_larynx: DQD40
#> Warning: [add_operations] Column(s) with zero TRUE values (8; pattern may not have been understood correctly): op_afab_breast_reconst_and_other_breast_ops, op_afab_penis_test_prosth, op_afab_internal_genital, op_afab_colpectomy, op_amab_breast_reconst_and_other_breast_ops, op_amab_reconst_vag, op_amab_penis_amp, op_amab_larynx

# Or specify custom operation codes
custom_ops <- list("mastectomy" = c("HAC10", "HAC20"))
add_operations(skeleton, fake_diagnoses, "lopnr", custom_ops)
#> Warning: [add_operations] Code(s) with zero matches in source data (pattern may not be understood, or code does not exist in registry):
#>   mastectomy: HAC20
```
