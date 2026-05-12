# Add cause of death data to skeleton

Searches for specific ICD-10 cause of death codes in Swedish death
registry data and adds corresponding boolean variables to the skeleton.
Can search in underlying cause of death, multiple causes, or both.

## Usage

``` r
add_cods(
  skeleton,
  dataset,
  id_name,
  cod_type = "both",
  codes = list(icd10_F64_0 = c("F640"), icd10_F64_89 = c("F6489"), icd10_F64_089 =
    c("F640", "F648", "F649")),
  cods = NULL
)
```

## Arguments

- skeleton:

  A data.table containing the main skeleton structure created by
  [`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)

- dataset:

  A data.table containing death registry data with cause of death codes.
  Must have columns for person ID, death date (dodsdat), and cause codes
  (ulorsak, morsak variables)

- id_name:

  Character string specifying the name of the ID variable in the dataset

- cod_type:

  Character string specifying which cause types to search:

  - "both" (default) - Search in both underlying (ulorsak) and multiple
    (morsak) causes

  - "underlying" - Search only in underlying cause of death (ulorsak)

  - "multiple" - Search only in multiple/contributing causes (morsak
    variables)

- codes:

  Named list of ICD-10 code patterns. Names become column names in the
  skeleton; values are character vectors of code prefixes.

  Matching is \*\*prefix-only\*\* via
  [`startsWith()`](https://rdrr.io/r/base/startsWith.html). A pattern
  like `"I21"` matches `"I21"`, `"I210"`, `"I219"`, etc. This is not
  regex – characters such as `^`, `$`, `*`, `[A-Z]` are taken literally
  and will not match anything.

  Prefixing a pattern with `"!"` turns it into a \*row-level veto\*: any
  source row whose code matches the (un-prefixed) pattern is masked out
  and does not contribute. The veto is applied per source row across all
  scanned columns (e.g. `ulorsak` + `morsak*`), and is reset between
  code names. Important: the veto operates on the raw source row, not on
  the (id, isoyearweek) bucket – if a person has both a vetoed code and
  a non-vetoed code in the same week, the non-vetoed code still triggers
  TRUE for that week.

  Examples:

  - `c("I21", "I22")` – any acute MI code.

  - `c("I", "!I21", "!I22")` – any chapter-I code except acute MI.

- cods:

  Deprecated alias for `codes`. If supplied, a warning is issued and the
  value is used as `codes`.

## Value

The skeleton data.table is modified by reference with cause of death
variables added. New boolean variables are created for each cause
pattern, TRUE when cause is present.

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
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_quality_registry()`](https://papadopoulos-lab.github.io/swereg/reference/add_quality_registry.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
[`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
[`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)

## Examples

``` r
# Load fake data
data("fake_person_ids", package = "swereg")
data("fake_cod", package = "swereg")
swereg::make_lowercase_names(fake_cod, date_columns = "dodsdat")

# Create skeleton
skeleton <- create_skeleton(fake_person_ids[1:10], "2020-01-01", "2020-12-31")

# Add cause of death data
cod_patterns <- list(
  "cardiovascular_death" = c("I21", "I22"),
  "external_causes" = c("X60", "X70")
)
add_cods(skeleton, fake_cod, "lopnr", "both", cod_patterns)
```
