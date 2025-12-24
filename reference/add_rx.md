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
  rxs = list(rx_hormones_pubblock = c("L02AE", "H01CA")),
  source = "atc"
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

- rxs:

  Named list of drug code patterns to search for. Names become variable
  names in skeleton. Patterns should NOT include "^" prefix
  (automatically added). Default includes hormone therapy codes for
  puberty blockers (L02AE, H01CA). Common patterns include:

  - Antidepressants: "N06A"

  - Hormone therapy: "G03", "L02AE", "H01CA"

  - Cardiovascular drugs: "C07", "C08", "C09"

- source:

  Character string specifying search field:

  - "atc" (default) - Search in ATC codes

  - "produkt" - Search in product names

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
#> 2025-12-24 07:39:04.558092 antidepressants
#> 2025-12-24 07:39:04.632555 hormones
```
