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
    [`startsWith()`](https://rdrr.io/r/base/startsWith.html): `"!C10AA"`
    masks `C10AA01`, `C10AA02`, ... For `source = "produkt"` the veto is
    exact-match via `%chin%`: `"!Sertralin"` does NOT mask
    `"Sertralin Sandoz"` because product names are exact, not prefixes.

  - **All-negative pattern set produces an empty column.** `c("!C10AA")`
    on its own gives an all-FALSE result – without any positive pattern
    there is no set to carve from. Use a wider include + the negative,
    e.g. `c("C10A", "!C10AA")`.

  - **Per-(id, isoyearweek) aggregation respects the veto on a
    per-source-row basis.** The veto removes specific prescription rows
    from the matched set before the per-week aggregation runs. If a
    person has both a vetoed Rx and a non-vetoed Rx whose coverage
    windows overlap in the same skeleton week, the non-vetoed Rx still
    drives that week to TRUE – the veto only kills its own row's
    contribution, not the whole week.

  Examples:

  - `c("N06A")` – any antidepressant.

  - `c("C10A", "!C10AA", "!C10AB")` – any lipid-modifying agent except
    statins and fibrates.

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

## Coverage interval

Each endpoint of the coverage interval is resolved once, independently:

- start: supplied `start_isoyearweek`, else the ISO week of the supplied
  `start_date`, else the ISO week of `edatum`.

- stop: supplied `stop_isoyearweek`, else the ISO week of the supplied
  `stop_date`, else the ISO week of `edatum + round(fddd) - 1`.

The `- 1` is because `foverlaps(type = "any")` matches inclusively at
both endpoints: without it a duration of N days would cover N + 1 days.

Rows whose `round(fddd)` is missing, non-finite or not positive are
dropped, with one warning naming the count. This applies if and only if
the stop endpoint is actually resolved from `fddd`, that is when the
caller supplied neither `stop_isoyearweek` nor `stop_date`.

## Interval validation

The resolved pair is then validated, on one rule that every combination
of supplied columns reaches. A row is dropped, with one warning naming
the count, when any of the following holds:

- either endpoint is missing;

- either endpoint is not a well-formed ISO week (`"YYYY-WW"` with week
  01 to 53, or the annual `"YYYY-**"`);

- the start week is later than the stop week;

- both endpoints came from dates and the start date is later than the
  stop date. This catches an interval inverted by days but contained in
  one ISO week, which compares equal as week strings.

An endpoint that is well formed but outside the skeleton's weeks is
kept, not dropped: `"2020-53"` is a real week that a skeleton ending in
`"2020-52"` does not carry, and the interval still covers every week
before it.

## Rows before the weekly spine

[`create_skeleton`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
builds an annual spine (`"<year>-**"`, `is_isoyear == TRUE`) for every
ISO year before the weekly period. After validation, any endpoint that
falls before the first weekly row is remapped onto the annual row of its
ISO year, the same rule
[`add_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
uses. A prescription that starts before the weekly period and ends
inside it therefore sets both the annual rows of the pre-weekly portion
and the weekly rows it covers.

The remap applies to every endpoint, including one the caller supplied.
A supplied `start_isoyearweek` of `"2019-51"`, on a skeleton whose
weekly period starts in 2020, marks the 2019 annual row.

## The prescription table is not modified

`add_rx()` computes `start_date`, `stop_date`, `start_isoyearweek` and
`stop_isoyearweek` on a local working copy. `lmed` is read, never
written. Earlier versions wrote these helper columns back into `lmed` by
reference; because the ISO week columns depend on the skeleton, reusing
one `lmed` across two skeletons then silently reused the first
skeleton's values.

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
[`add_cancer_without_morphology()`](https://papadopoulos-lab.github.io/swereg/reference/add_cancer_without_morphology.md),
[`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md),
[`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md),
[`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md),
[`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md),
[`add_quality_registry()`](https://papadopoulos-lab.github.io/swereg/reference/add_quality_registry.md)

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
