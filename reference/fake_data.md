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

fake_diagnoses

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

An object of class `data.table` (inherits from `data.frame`) with 5000
rows and 49 columns.

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

- SOURCE column in fake_diagnoses tracks data origin

**Usage requirements:**

- Always apply
  [`swereg::make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
  after loading data

- Use appropriate identifier column names (lopnr vs p444_lopnr_personnr)

- Follow Swedish registry conventions for date formats

- Filter by SOURCE column when needed (e.g., SOURCE == "cancer" for
  ICD-O-3)

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

## fake_diagnoses

Combined diagnosis data with ~5000 records from three sources:

- SOURCE:

  Data source: "inpatient", "outpatient", or "cancer"

- LopNr:

  Personal identifier

- AR:

  Year of care

- INDATUMA:

  Admission date (YYYYMMDD character)

- INDATUM:

  Admission date (Date class)

- UTDATUMA:

  Discharge date (YYYYMMDD character, inpatient only)

- UTDATUM:

  Discharge date (Date class, inpatient only)

- HDIA:

  Main diagnosis (ICD-10 code)

- DIA1-DIA30:

  Additional diagnoses

- EKOD1-EKOD7:

  External cause codes

- OP:

  Operation codes

- ICDO3:

  ICD-O-3 morphology codes (populated for cancer source)

- SNOMED3:

  SNOMED-CT version 3 codes

- SNOMEDO10:

  SNOMED-CT version 10 codes

The SOURCE column identifies the registry origin:

- "inpatient": NPR inpatient data (~2000 records)

- "outpatient": NPR outpatient data (~2000 records)

- "cancer": Cancer registry data (~1000 records, always has ICDO3)

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
data("fake_diagnoses")

# CRITICAL: Apply lowercase names
swereg::make_lowercase_names(fake_demographics)
swereg::make_lowercase_names(fake_diagnoses, date_columns = "indatum")

# Check source distribution
table(fake_diagnoses$source)

# Filter by source
inpatient_only <- fake_diagnoses[source == "inpatient"]
cancer_only <- fake_diagnoses[source == "cancer"]

# Create skeleton with fake data
skeleton <- create_skeleton(
  ids = fake_person_ids[1:100],
  date_min = "2015-01-01",
  date_max = "2020-12-31"
)
} # }
```
