# Add cancer diagnoses by topography (site only, ignoring morphology)

Flags cancer from the Swedish Cancer Register by *topography* (tumour
site) only, ignoring morphology/histology. Use this when the phenotype
is site-level – "any breast cancer", "any endometrial cancer" – which is
the granularity ICD-10 and the patient register encode cancer at.
Morphology / histology (cell type, behaviour) is out of scope for this
function.

## Usage

``` r
add_cancer_without_morphology(skeleton, dataset, id_name, codes = list())
```

## Arguments

- skeleton:

  The skeleton data.table created by
  [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)

- dataset:

  Cancer register data.table containing `icdo10` and/or `icdo3` columns
  and an `indatum` date column.

- id_name:

  Character string specifying the name of the ID variable in the dataset

- codes:

  Named list of topography (C-code) patterns. Names become column names
  in the skeleton; values are character vectors of code prefixes.
  Matching is prefix-only via
  [`startsWith()`](https://rdrr.io/r/base/startsWith.html);
  `"!"`-prefixed patterns act as row-level vetoes. See
  [`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  for the full pattern-syntax description.

## Value

The skeleton data.table is modified by reference; one boolean column per
pattern, TRUE when the cancer topography code is present.

## Details

Searches BOTH ICD-O topography columns and unions the result, giving
complete coverage:

- `icdo10` – ICD-O/2 topography, populated back to register start (the
  complete column).

- `icdo3` – ICD-O/3 topography, populated from ~2000 onward.

Despite its name, `icdo10` is **not** "ICD-O edition 10" (ICD-O has only
editions 1-3). It is Socialstyrelsen's (confusingly named) column for
ICD-O/2 topography, whose codes ARE the ICD-10 neoplasm site codes (e.g.
`C50` = breast). That is why ICD-10 cancer patterns match it – and why
it must not be mistaken for a stray ICD-O column and dropped.

Note: in-situ tumours carry the malignant topography code (in-situ
breast is `C50` + morphology /2, never `D05`), so a `C50` pattern here
captures both invasive and in-situ breast.

## See also

[`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
for ICD-10 patient-register codes,
[`make_lowercase_names`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
for data preprocessing

Other data_integration:
[`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_quality_registry()`](https://papadopoulos-lab.github.io/swereg/reference/add_quality_registry.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)

## Examples

``` r
data("fake_person_ids", package = "swereg")
data("fake_diagnoses", package = "swereg")
swereg::make_lowercase_names(fake_diagnoses, date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.
skeleton <- create_skeleton(fake_person_ids[1:10], "2020-01-01", "2020-12-31")
cancer_codes <- list("breast" = c("C50"), "endometrial" = c("C54", "C55"))
add_cancer_without_morphology(skeleton, fake_diagnoses, "lopnr", cancer_codes)
```
