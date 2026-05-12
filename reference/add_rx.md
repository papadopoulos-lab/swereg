# Add prescription drug data to skeleton

Searches for specific drug codes (ATC or product names) in Swedish
prescription registry data and adds corresponding boolean variables to
the skeleton based on prescription periods and duration of treatment.

## Usage

``` r
add_rx(
  skeleton,
  lmed,
  id_name = "lopnr",
  codes = list(rx_hormones_pubblock = c("L02AE", "H01CA")),
  source = "atc",
  rxs = NULL
)
```

## Arguments

- skeleton:

  A data.table containing the main skeleton structure created by
  [`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)

- lmed:

  A data.table containing prescription registry data (LMED). Must have
  columns for person ID, prescription date (edatum), treatment duration
  (fddd), and drug codes (atc) or product names (produkt)

- id_name:

  Character string specifying the name of the ID variable (default:
  "lopnr")

- codes:

  Named list of drug code patterns. Names become column names in the
  skeleton; values are character vectors. Matching semantics depend on
  `source` (see below).

  Prefixing a pattern with `"!"` turns it into a \*row-level veto\*: any
  prescription whose code matches the (un-prefixed) pattern is masked
  and does not contribute to the named output column. Final rule: a
  prescription row contributes to the named column iff at least one
  un-prefixed pattern matches AND no `"!"` pattern matches.

  Behaviour notes worth knowing:

  - **Vetoes are independent per named code.** A `"!"` entry inside one
    list element does not leak into any other element of the same
    `codes` list. Two named codes can produce two completely different
    views of the same prescription rows.

  - **Veto match style follows `source`.** For `source = "atc"` the veto
    is prefix-based via
    [`startsWith()`](https://rdrr.io/r/base/startsWith.html): `"!N05AA"`
    masks `N05AA01`, `N05AA02`, ... For `source = "produkt"` the veto is
    exact-match via `%chin%`: `"!Sertralin"` does NOT mask
    `"Sertralin Sandoz"` because product names are exact, not prefixes.

  - **All-negative pattern set produces an empty column.** `c("!N05AA")`
    on its own gives an all-FALSE result – without any positive pattern
    there is no set to carve from. Use a wider include + the negative,
    e.g. `c("N05A", "!N05AA")`.

  - **Per-(id, isoyearweek) aggregation respects the veto on a
    per-source-row basis.** The veto removes specific prescription rows
    from the matched set before the per-week aggregation runs. If a
    person has both a vetoed Rx and a non-vetoed Rx whose coverage
    windows overlap in the same skeleton week, the non-vetoed Rx still
    drives that week to TRUE – the veto only kills its own row's
    contribution, not the whole week.

  Examples:

  - `c("N06A")` – any antidepressant.

  - `c("N05A", "!N05AA", "!N05AB")` – any antipsychotic except
    first-generation typical agents.

  Default includes hormone therapy codes for puberty blockers (L02AE,
  H01CA). Common patterns include:

  - Antidepressants: `"N06A"`

  - Hormone therapy: `"G03"`, `"L02AE"`, `"H01CA"`

  - Cardiovascular drugs: `"C07"`, `"C08"`, `"C09"`

- source:

  Character string specifying search field and matching strategy:

  - "atc" (default) - Prefix matching in ATC codes (e.g., "N06A" matches
    "N06AB06"). Uses
    [`startsWith()`](https://rdrr.io/r/base/startsWith.html) for fast
    C-level matching.

  - "produkt" - Exact matching on product names (e.g., "Delestrogen"
    matches only "Delestrogen", not "Delestrogen Extra"). Uses `%chin%`
    for fast lookup.

- rxs:

  Deprecated. Use `codes` instead.

## Value

The skeleton data.table is modified by reference with prescription
variables added. Variables are TRUE during periods when the prescription
is active based on start/stop dates calculated from prescription date +
treatment duration

## See also

[`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
for creating the skeleton structure,
[`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
for diagnosis codes,
[`add_operations`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md)
for surgical procedures,
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
[`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md),
[`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)

## Examples

``` r
# Load fake data
data("fake_person_ids", package = "swereg")
data("fake_prescriptions", package = "swereg")
swereg::make_lowercase_names(fake_prescriptions, date_columns = "edatum")

# Create skeleton
skeleton <- create_skeleton(fake_person_ids[1:10], "2020-01-01", "2020-12-31")

# Add prescription data
rx_patterns <- list(
  "antidepressants" = c("N06A"),
  "hormones" = c("G03", "L02AE")
)
add_rx(skeleton, fake_prescriptions, "p444_lopnr_personnr", rx_patterns, "atc")
```
