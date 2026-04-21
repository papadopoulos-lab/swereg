# Built-in add\_\* functions

``` r
library(data.table)
library(swereg)
```

## What `add_*` functions do

`add_*` functions are the workhorse of swereg: they take raw registry
data and fold it into a skeleton created by
[`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md).
Every `add_*` function shares the same shape:

- **First argument** is the skeleton.
- **Second argument** is a registry `data.table`.
- The skeleton is **mutated in place** via
  `skeleton[data, on = ..., := ...]`. No copy, no return.
- New columns are appended to the skeleton with names you choose (for
  the pattern-matching functions, names come from the `codes` list).

This vignette walks through every `add_*` function swereg ships with,
using the synthetic datasets included in the package. For writing your
*own* `add_*` for a registry swereg doesn’t support, see
[`vignette("custom-add-functions")`](https://papadopoulos-lab.github.io/swereg/articles/custom-add-functions.md).

## Setup: a skeleton we can reuse

``` r
data("fake_person_ids", package = "swereg")

skeleton <- create_skeleton(
  ids      = fake_person_ids[1:25],
  date_min = "2019-01-01",
  date_max = "2020-12-31"
)

head(skeleton, 3)
#>       id isoyear isoyearweek is_isoyear isoyearweeksun personyears
#>    <int>   <int>      <char>     <lgcl>         <Date>       <num>
#> 1:     1    1900     1900-**       TRUE     1900-07-01           1
#> 2:     1    1901     1901-**       TRUE     1901-06-30           1
#> 3:     1    1902     1902-**       TRUE     1902-06-29           1
```

The skeleton has one row per (person, week) plus one “annual” row per
(person, year) marked by `is_isoyear == TRUE`. All `add_*` functions
respect this layout.

## `add_onetime()` — baseline / demographic data

Use
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md)
for a single value per person that never changes (date of birth, sex at
birth, country of origin, etc.). The function broadcasts the value to
every row belonging to that person.

``` r
data("fake_demographics", package = "swereg")
swereg::make_lowercase_names(fake_demographics)
#> Found potential date columns: fodelseman. Consider adding them to date_columns parameter for automatic date parsing.

add_onetime(skeleton, fake_demographics, id_name = "lopnr")

skeleton[, .(id, isoyearweek, fodelseman, doddatum)] |> head(3)
#>       id isoyearweek fodelseman doddatum
#>    <int>      <char>     <char>   <char>
#> 1:     1     1900-**       1959         
#> 2:     1     1901-**       1959         
#> 3:     1     1902-**       1959
```

Every row for a given `id` now carries the same `fodelseman` and
`doddatum` values.

**Column-name collision policy**: if the skeleton already has a column
with the same name as something in `data`,
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md)
keeps the existing skeleton column and prefixes the incoming one with
`i.` (data.table’s standard collision marker).

## `add_annual()` — yearly data for a specific year

Registries that report once per calendar year (SCB LISA income, family
composition, etc.) go through
[`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md).
You call it *once per year*, and values are attached to `(id, isoyear)`
rows.

``` r
data("fake_annual_family", package = "swereg")
swereg::make_lowercase_names(fake_annual_family)

add_annual(skeleton, fake_annual_family, id_name = "lopnr", isoyear = 2020L)

skeleton[isoyear == 2020 & is_isoyear == TRUE, .(id, isoyear, famtyp)] |> head(3)
#> Empty data.table (0 rows and 3 cols): id,isoyear,famtyp
```

**Gotcha**: the value only lands on `is_isoyear == TRUE` rows for the
matching year. The weekly rows (`is_isoyear == FALSE`) keep `NA`. If you
want the value forward-filled onto the weeks, do that explicitly with a
`nafill`-style step after all
[`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md)
calls finish.

## `add_diagnoses()` — ICD codes from the NPR

The National Patient Register holds main (`hdia`) and secondary (`dia1`,
`dia2`, …) diagnoses per hospital contact.
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
takes a named list of ICD-code prefixes and writes one boolean column
per entry, `TRUE` on every week that person had a matching diagnosis.

``` r
data("fake_diagnoses", package = "swereg")
swereg::make_lowercase_names(fake_diagnoses, date_columns = "indatum")
#> Found additional date columns not in date_columns: utdatum. Consider adding them for automatic date parsing.

add_diagnoses(
  skeleton,
  fake_diagnoses,
  id_name  = "lopnr",
  diag_type = "both",
  codes = list(
    "dep"     = c("F32", "F33"),
    "anxiety" = c("F40", "F41")
  )
)

skeleton[dep == TRUE, .(id, isoyearweek, dep)] |> head(3)
#>       id isoyearweek    dep
#>    <int>      <char> <lgcl>
#> 1:     3     1994-**   TRUE
#> 2:     3     1995-**   TRUE
#> 3:     3     1997-**   TRUE
```

**Pattern syntax** (shared with
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md),
[`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
[`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)):

- `"F32"` — prefix match (all codes starting with F32).
- `"^F320"` — the leading `^` is implicit; written out for emphasis.
- `"!F321"` — **negation**: drops matches starting with F321 from the
  positive set. Use this to carve out a specific sub-code.
- Multiple entries in one vector are OR’d together.

**`diag_type`**: `"both"` searches `hdia`, `dia*`, `ekod*`, `icd7*`, and
`icd9*`. `"main"` searches `hdia` only — useful when you want the
primary reason for admission, not every code listed.

**Splitting by source** (inpatient vs outpatient): the dataset includes
a `source` column. Filter first, then call
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
twice:

``` r
in_only <- fake_diagnoses[source == "inpatient"]
add_diagnoses(skeleton, in_only, "lopnr", codes = list("dep_inp" = c("F32", "F33")))
```

## `add_operations()` — surgical procedure codes

Same pattern as
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
but searches the `op1`, `op2`, … columns of the NPR.

``` r
add_operations(
  skeleton,
  fake_diagnoses,
  id_name = "lopnr",
  codes = list("mastectomy" = c("HAC10", "HAC20"))
)
```

## `add_rx()` — prescription drugs (LMED)

Prescriptions aren’t instantaneous: a dispense covers a treatment period
from the prescription date (`edatum`) to `edatum + fddd` days (`fddd` =
defined daily doses dispensed).
[`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
converts these intervals to ISO week ranges and flags every week the
patient was covered.

``` r
data("fake_prescriptions", package = "swereg")
swereg::make_lowercase_names(fake_prescriptions, date_columns = "edatum")

add_rx(
  skeleton,
  fake_prescriptions,
  id_name = "p444_lopnr_personnr",
  codes = list(
    "antidep" = "N06A",
    "hormones" = c("G03", "L02AE")
  ),
  source = "atc"
)

skeleton[antidep == TRUE, .(id, isoyearweek, antidep)] |> head(3)
#>       id isoyearweek antidep
#>    <int>      <char>  <lgcl>
#> 1:     2     2019-**    TRUE
#> 2:     2     2019-01    TRUE
#> 3:     2     2019-02    TRUE
```

**`source = "atc"`** (default) uses prefix matching via
[`startsWith()`](https://rdrr.io/r/base/startsWith.html) on the `atc`
column. **`source = "produkt"`** uses exact matching via `%chin%` on the
`produkt` (product name) column — useful when ATC granularity isn’t
enough (e.g. distinguishing formulations).

**Gotcha**: prescriptions with `NA` `fddd` or negative durations are
dropped with a warning. They can’t be placed on the week grid.

## `add_cods()` — cause of death

Cause-of-death records have one “underlying” cause (`ulorsak`) and
multiple contributing causes (`morsak1`, `morsak2`, …).
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md)
lets you search either or both.

``` r
data("fake_cod", package = "swereg")
swereg::make_lowercase_names(fake_cod, date_columns = "dodsdat")

add_cods(
  skeleton,
  fake_cod,
  id_name = "lopnr",
  cod_type = "both",
  codes = list(
    "cardio_death"   = c("I21", "I22"),
    "external_death" = c("X60", "X70")
  )
)
```

## `add_quality_registry()` — arbitrary event registries

Quality registries (Riksstroke, NKBC, …) don’t follow the ICD-prefix
model.
[`add_quality_registry()`](https://papadopoulos-lab.github.io/swereg/reference/add_quality_registry.md)
takes **quoted R expressions** evaluated against the dataset columns;
each expression yields one boolean flag per week.

``` r
fake_registry <- data.table::data.table(
  lopnr      = fake_person_ids[1:3],
  event_date = as.Date(c("2019-03-15", "2020-06-20", "2020-11-01")),
  severity   = c(5L, 18L, 22L),
  treated    = c(1L, 2L, 1L)
)

add_quality_registry(
  skeleton,
  fake_registry,
  id_name = "lopnr",
  date_col = "event_date",
  codes = list(
    "reg_event"   = TRUE,
    "reg_severe"  = quote(severity >= 15),
    "reg_treated" = quote(treated == 1)
  )
)
```

`TRUE` marks every week with any record. `quote(...)` flags the weeks
where the expression holds. Aggregation within a week is
[`any()`](https://rdrr.io/r/base/any.html), and `NA` is coerced to
`FALSE` (“no evidence” is not the same as “true”).

## Typical end-to-end workflow

The built-ins layer on top of each other. A realistic ordering:

``` r
skeleton <- create_skeleton(ids, "2005-01-01", "2023-12-31")

add_onetime(skeleton, demographics, "lopnr")            # baseline
for (y in 2005:2023) {
  add_annual(skeleton, lisa[year == y], "lopnr", y)     # annual
}
add_diagnoses(skeleton, npr,     "lopnr", codes = ...)  # events
add_operations(skeleton, npr,    "lopnr", codes = ...)
add_rx(skeleton, lmed, "p444_lopnr_personnr", codes = ...)
add_cods(skeleton, cod,          "lopnr", codes = ...)
```

After this you typically derive row-independent (`ri_*`) variables with
[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
— see the “rowdep vs rowind” vignette.

## If swereg doesn’t ship an `add_*` for your registry

Write your own. The contract is small and the pipeline enforces it
automatically when you register your function via
`RegistryStudy$register_codes()`. See
[`vignette("custom-add-functions")`](https://papadopoulos-lab.github.io/swereg/articles/custom-add-functions.md)
— same mechanism works for Norwegian registries, regional Swedish
cohorts, payer claims, and anything else with a longitudinal
event-per-row shape.
