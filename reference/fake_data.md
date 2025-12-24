# Fake Swedish Registry Datasets

These datasets contain synthetic Swedish healthcare registry data for
development, testing, and vignettes. They mimic the structure and format
of real Swedish healthcare registries but contain completely fabricated
data.

## Usage

``` r
fake_person_ids

fake_demographics

fake_annual_family

fake_inpatient_diagnoses

fake_outpatient_diagnoses

fake_prescriptions

fake_cod
```

## Format

Various data structures matching real Swedish registries:

An object of class `integer` of length 1000.

An object of class `data.table` (inherits from `data.frame`) with 1000
rows and 3 columns.

An object of class `data.table` (inherits from `data.frame`) with 1000
rows and 2 columns.

An object of class `data.table` (inherits from `data.frame`) with 3000
rows and 45 columns.

An object of class `data.table` (inherits from `data.frame`) with 2000
rows and 41 columns.

An object of class `data.table` (inherits from `data.frame`) with 8000
rows and 37 columns.

An object of class `data.table` (inherits from `data.frame`) with 50
rows and 5 columns.

## Source

Generated synthetic data based on real Swedish healthcare registry
structures

## Details

These datasets are created by `dev/generate_fake_data.R` and contain:

**Key features:**

- Personal identifiers are numeric (e.g., 623334, 753064)

- Prescription data uses column name "p444_lopnr_personnr"

- ICD-10 codes include gender dysphoria (F64\*), mental health (F20\*,
  F32\*, F40\*), and physical health codes

- ATC codes include hormone therapy (G03\*), mental health medications
  (N05\*, N06\*)

- Date ranges span 1978-2021 depending on registry

- Realistic missing data patterns

**Usage requirements:**

- Always apply
  [`swereg::make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
  after loading data

- Use appropriate identifier column names (lopnr vs p444_lopnr_personnr)

- Follow Swedish registry conventions for date formats

## fake_person_ids

A numeric vector of 1000 fake personal identifiers (lopnr). Used as
reference IDs across all other datasets.

## fake_demographics

Demographics data (SCB format) with 1000 records:

- lopnr:

  Personal identifier matching fake_person_ids

- fodelseman:

  Birth year-month (YYYYMM format)

- DodDatum:

  Death date (YYYYMMDD format) or empty string

## fake_annual_family

Annual family status data (SCB format) with 1000 records:

- LopNr:

  Personal identifier (mixed case as in real data)

- FamTyp:

  Family type code (2-digit character)

## fake_inpatient_diagnoses

Inpatient hospital diagnoses (NPR format) with ~3000 records:

- LopNr:

  Personal identifier

- AR:

  Year of care

- INDATUMA:

  Admission date (YYYYMMDD character)

- INDATUM:

  Admission date (Date class)

- UTDATUMA:

  Discharge date (YYYYMMDD character)

- UTDATUM:

  Discharge date (Date class)

- HDIA:

  Main diagnosis (ICD-10 code)

- DIA1-DIA30:

  Additional diagnoses

- EKOD1-EKOD7:

  External cause codes

- OP:

  Operation codes

## fake_outpatient_diagnoses

Outpatient specialist diagnoses (NPR format) with ~2000 records. Same
structure as inpatient data but without discharge dates.

## fake_prescriptions

Prescription drug dispensing data (LMED format) with ~8000 records:

- p444_lopnr_personnr:

  Personal identifier with p444 prefix

- Fall:

  Case indicator

- Kontroll:

  Control indicator

- VARUNR:

  Product number

- ATC:

  ATC classification code

- ALDER:

  Age at prescription

- LK:

  Healthcare county code

- EDATUM:

  End date

- FDATUM:

  Start date

- OTYP:

  Origin type

- ...:

  Additional 27 columns matching real LMED structure

## fake_cod

Cause of death data with ~50 records (Swedish registry format):

- lopnr:

  Personal identifier

- dodsdat:

  Date of death

- ulorsak:

  Underlying cause of death (ICD-10) - Swedish variable name

- morsak1:

  First multiple/contributory cause of death

- morsak2:

  Second multiple/contributory cause of death

## Examples

``` r
if (FALSE) { # \dontrun{
# Load fake data
data("fake_person_ids")
data("fake_demographics")
data("fake_prescriptions")

# CRITICAL: Apply lowercase names
swereg::make_lowercase_names(fake_demographics)
swereg::make_lowercase_names(fake_prescriptions)

# Create skeleton with fake data
skeleton <- create_skeleton(
  ids = fake_person_ids[1:100],
  date_min = "2015-01-01",
  date_max = "2020-12-31"
)

# Add demographics
add_onetime(skeleton, fake_demographics[lopnr %in% fake_person_ids[1:100]], 
            id_name = "lopnr")
} # }
```
