# swereg 25.6.24

## New Features

* **NEW**: Added `isoyearweek_sunday` variable to `create_skeleton()` function - provides Date representing the Sunday (last day) of each ISO week/year for easier date calculations
* **NEW**: Added comprehensive fake Swedish registry datasets for development and vignettes:
  - `fake_person_ids`: 1000 synthetic personal identifiers
  - `fake_demographics`: Demographics data matching SCB format
  - `fake_annual_family`: Annual family status data
  - `fake_inpatient_diagnoses` and `fake_outpatient_diagnoses`: NPR diagnosis data with ICD-10 codes
  - `fake_prescriptions`: LMED prescription data with ATC codes and hormone therapy focus
  - `fake_cod`: Cause of death data
* **NEW**: Added two comprehensive vignettes:
  - `swereg.Rmd`: Basic skeleton1_create workflow tutorial
  - `swereg-advanced-workflow.Rmd`: Complete 3-phase workflow (skeleton1 → skeleton2 → skeleton3)
* **NEW**: Replaced magrittr pipe (%>%) with base pipe (|>) throughout codebase
* **NEW**: Added memory-efficient batched processing examples for large registry studies

## Bug Fixes

* **CRITICAL**: Fixed incorrect variable names in `fake_cod` dataset - changed from non-Swedish `underlying_cod/contributory_cod1/contributory_cod2` to correct Swedish registry names `ulorsak/morsak1/morsak2`
* **VERIFIED**: Confirmed all fake datasets use correct Swedish registry variable name conventions
* **VERIFIED**: All ICD-10 and ATC codes in fake datasets are properly formatted and realistic

## Documentation Improvements

* **BREAKING**: Fixed incorrect function descriptions that were copied from another package
* **NEW**: Added comprehensive roxygen2 documentation for all exported functions:
  - `add_onetime()`: Documents merging one-time/baseline data to skeleton
  - `add_annual()`: Documents merging annual data for specific ISO years
  - `add_cods()`: Documents cause of death analysis with ICD-10 codes
  - `add_diagnoses()`: Documents diagnosis analysis with main/secondary diagnoses
  - `add_operations()`: Documents surgical operation analysis including gender-affirming procedures
  - `add_rx()`: Documents prescription drug analysis with ATC/product codes
  - `create_skeleton()`: Documents longitudinal skeleton creation with detailed return structure
  - `make_lowercase_names()`: Documents generic function with S3 methods
  - `x2023_mht_add_lmed()`: Documents specialized MHT study function
* **NEW**: Added documentation for all helper functions:
  - `min_with_infinite_as_na()`, `max_with_infinite_as_na()`
  - `as_logical_min_with_infinite_as_na()`, `as_logical_max_with_infinite_as_na()`
  - `first_non_na()`, `last_non_na()`
* **NEW**: Added `@param` descriptions for all function parameters
* **NEW**: Added `@return` descriptions explaining function outputs
* **NEW**: Added `@examples` with practical usage demonstrations
* **NEW**: Added `@details` and `@note` sections for complex functions
* **IMPROVED**: Used proper roxygen2 practices including `@rdname` for S3 methods and `@seealso` cross-references

## Package Structure

* All exported functions now have complete, accurate documentation suitable for CRAN submission
* Documentation focuses on Swedish registry data analysis workflows
* Examples use `\dontrun{}` appropriately for functions requiring external data