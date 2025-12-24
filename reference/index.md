# Package index

## Core functions

Main functions for creating and manipulating longitudinal skeletons

- [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
  : Create longitudinal data skeleton
- [`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md)
  : Add one-time data to skeleton
- [`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md)
  : Add annual data to skeleton

## Medical data integration

Functions for adding medical registry data

- [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md)
  : Add diagnosis data to skeleton
- [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md)
  : Add surgical operation data to skeleton
- [`add_icdo3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_icdo3s.md)
  : Add ICD-O-3 oncology codes to skeleton
- [`add_snomed3s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomed3s.md)
  : Add SNOMED-CT version 3 codes to skeleton
- [`add_snomedo10s()`](https://papadopoulos-lab.github.io/swereg/reference/add_snomedo10s.md)
  : Add SNOMED-CT version 10 codes to skeleton
- [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md)
  : Add prescription drug data to skeleton
- [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md)
  : Add cause of death data to skeleton

## Data transformation

Functions for transforming data structure and creating derived variables

- [`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
  : Transform rowdep variable to rowind variable using first occurrence

## Survival analysis

Helper functions for time-to-event and survival analysis

- [`steps_to_first()`](https://papadopoulos-lab.github.io/swereg/reference/steps_to_first.md)
  : Calculate steps until first TRUE in a forward window

## Utility functions

Helper functions for data processing

- [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
  : Convert column names to lowercase and optionally clean date columns
- [`parse_swedish_date()`](https://papadopoulos-lab.github.io/swereg/reference/parse_swedish_date.md)
  : Parse Swedish registry dates
- [`first_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/first_non_na.md)
  : Get first non-NA value from vector
- [`last_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/last_non_na.md)
  : Get last non-NA value from vector
- [`min_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/min_with_infinite_as_na.md)
  : Calculate minimum while treating infinite values as NA
- [`max_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/max_with_infinite_as_na.md)
  : Calculate maximum while treating infinite values as NA
- [`as_logical_min_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/as_logical_min_with_infinite_as_na.md)
  : Convert minimum to logical while treating infinite values as NA
- [`as_logical_max_with_infinite_as_na()`](https://papadopoulos-lab.github.io/swereg/reference/as_logical_max_with_infinite_as_na.md)
  : Convert maximum to logical while treating infinite values as NA

## Specialized functions

Functions for specific research applications

- [`x2023_mht_add_lmed()`](https://papadopoulos-lab.github.io/swereg/reference/x2023_mht_add_lmed.md)
  : Add 2023 MHT-specific prescription data to skeleton

## Datasets

Synthetic registry data for development and examples

- [`fake_person_ids`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_demographics`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_annual_family`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_inpatient_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_outpatient_diagnoses`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_prescriptions`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  [`fake_cod`](https://papadopoulos-lab.github.io/swereg/reference/fake_data.md)
  : Fake Swedish Registry Datasets
