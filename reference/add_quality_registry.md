# Add quality registry data to skeleton

Creates boolean skeleton columns from filter expressions evaluated
against quality registry data (e.g., Riksstroke, NKBC). Unlike
\[add_diagnoses()\] which matches ICD code prefixes, this function
evaluates arbitrary R expressions against the dataset columns.

## Usage

``` r
add_quality_registry(skeleton, dataset, id_name, date_col, codes = list())
```

## Arguments

- skeleton:

  A data.table containing the main skeleton structure created by
  [`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md).

- dataset:

  A data.table containing quality registry data with person ID and a
  date column.

- id_name:

  Character string specifying the name of the ID variable in the
  dataset.

- date_col:

  Character string specifying the name of the date column in the dataset
  (must be Date or IDate class).

- codes:

  Named list of filter definitions. Names become variable names in
  skeleton. Values are either `TRUE` (event flag) or
  [`quote()`](https://rdrr.io/r/base/substitute.html) expressions
  evaluated against dataset columns. Example:
  `list("stroke_event" = TRUE, "stroke_tia" = quote(tia == 1))`

## Value

The skeleton data.table is modified by reference with boolean variables
added. New columns are FALSE on non-event weeks, and FALSE when filter
expressions evaluate to NA (missing data).

## Details

Each entry in `codes` produces one boolean column on the skeleton:

- `TRUE`: event indicator — TRUE on any week with a registry event

- `quote(expr)`: filter expression evaluated against dataset columns.
  TRUE when the expression evaluates to TRUE, FALSE otherwise (including
  NA).

Multiple events in the same person-week are aggregated with
[`any()`](https://rdrr.io/r/base/any.html).

## See also

[`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
for ICD code matching,
[`add_rx`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
for prescription data

Other data_integration:
[`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md),
[`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
[`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)

## Examples

``` r
# Create fake data
data("fake_person_ids", package = "swereg")
skeleton <- create_skeleton(fake_person_ids[1:5], "2020-01-01", "2020-12-31")

fake_registry <- data.table::data.table(
  lopnr = c(fake_person_ids[1], fake_person_ids[2]),
  event_date = as.Date(c("2020-03-15", "2020-06-20")),
  severity = c(5, 18),
  treated = c(1, 2)
)

add_quality_registry(skeleton, fake_registry, "lopnr",
  date_col = "event_date",
  codes = list(
    "reg_event" = TRUE,
    "reg_severe" = quote(severity >= 15),
    "reg_treated" = quote(treated == 1)
  )
)
```
