# swereg 25.7.15

## Vignette Content Improvements

* **SIMPLIFIED**: Removed subtitle text after colons in vignette titles for cleaner presentation
* **ENHANCED**: Improved Core Concept section in basic workflow vignette with clear explanation of three data types:
  - One-time data (demographics): Added to all rows for each person
  - Annual data (income, family status): Added to all rows for specific year
  - Event-based data (diagnoses, prescriptions, deaths): Added to rows where events occurred
* **CLARIFIED**: Step 1 documentation now properly explains all skeleton columns including `isoyearweek_sunday`
* **VERIFIED**: All vignettes compile successfully with improved content

# swereg 25.7.15

## Major Documentation and Vignette Reorganization

* **RESTRUCTURED**: Complete vignette reorganization with improved naming and content flow:
  - `swereg.Rmd` → `basic-workflow.Rmd`: Focused introduction to skeleton1_create
  - `advanced-workflow.Rmd` → `complete-workflow.Rmd`: Two-stage workflow (skeleton1_create + skeleton2_clean)
  - `memory-efficient-batching.Rmd`: Maintained as comprehensive three-stage workflow guide
* **IMPROVED**: Eliminated content redundancy between vignettes for clearer learning progression
* **ENHANCED**: Updated _pkgdown.yml configuration to reflect new vignette structure

## Function Documentation Improvements

* **ENHANCED**: Comprehensive documentation improvements for all exported functions:
  - Added @family tags for logical grouping (data_integration, skeleton_creation, data_preprocessing)
  - Added @seealso sections with cross-references to related functions and vignettes
  - Replaced placeholder examples with runnable code using synthetic data
  - Improved parameter documentation with detailed descriptions and expected formats
  - Enhanced return value documentation with explicit side effects description
* **STANDARDIZED**: Consistent academic tone throughout all documentation

## Professional Presentation Updates

* **IMPROVED**: Removed informal elements and adopted academic tone across all documentation
* **UPDATED**: Changed terminology from "fake data" to "synthetic data" throughout
* **ENHANCED**: More professional language in README.md and vignettes
* **STANDARDIZED**: Consistent formal tone appropriate for scientific software

## Technical Improvements

* **VERIFIED**: All vignettes compile successfully with updated content
* **TESTED**: Package passes R CMD check with all documentation improvements
* **UPDATED**: CLAUDE.md reflects new vignette structure and documentation standards

# swereg 25.7.1

## Vignette Restructuring

* **RESTRUCTURED**: Reorganized vignettes for clearer learning progression:
  - `swereg.Rmd`: Clean skeleton1_create tutorial using full datasets (removed subset filtering)
  - `advanced-workflow.Rmd`: Focused skeleton1→skeleton2 workflow (removed batching and skeleton3 content)
  - `memory-efficient-batching.Rmd`: NEW comprehensive batching vignette with complete skeleton1→skeleton2→skeleton3 workflow for large-scale studies
* **IMPROVED**: GitHub Actions workflow optimization with dependency caching and binary packages for faster CI/CD

## Batching Vignette Fixes

* **FIXED**: Updated memory-efficient-batching vignette with production-ready improvements:
  - Replace `split()` with `csutil::easy_split` for better batch handling
  - Replace `saveRDS/readRDS` with `qs::qsave/qread` for 2-10x faster file I/O
  - Fix skeleton3_analyze to properly aggregate weekly→yearly data using `swereg::max_with_infinite_as_na`
  - Remove incorrect `is_isoyear == TRUE` filter in skeleton3_analyze
  - Fix analysis results to avoid NaN outputs in treatment rate calculations
  - Add explanations for weekly→yearly data aggregation and qs package performance benefits

## New Features

* **NEW**: Added `isoyearweek_sunday` variable to `create_skeleton()` function - provides Date representing the Sunday (last day) of each ISO week/year for easier date calculations
* **NEW**: Updated package logo
* **IMPROVED**: Updated all vignettes to not assume swereg is loaded - all functions use `swereg::` prefix and `data()` calls use `package="swereg"` argument
* **IMPROVED**: Updated function documentation to clarify that pattern matching functions (`add_diagnoses`, `add_cods`, `add_rx`) automatically add "^" prefix - users should NOT include "^" in their patterns
* **NEW**: Added comprehensive fake Swedish registry datasets for development and vignettes:
  - `fake_person_ids`: 1000 synthetic personal identifiers
  - `fake_demographics`: Demographics data matching SCB format
  - `fake_annual_family`: Annual family status data
  - `fake_inpatient_diagnoses` and `fake_outpatient_diagnoses`: NPR diagnosis data with ICD-10 codes
  - `fake_prescriptions`: LMED prescription data with ATC codes and hormone therapy focus
  - `fake_cod`: Cause of death data
* **NEW**: Added two comprehensive vignettes:
  - `swereg.Rmd`: Basic skeleton1_create workflow tutorial
  - `advanced-workflow.Rmd`: Complete 3-phase workflow (skeleton1 → skeleton2 → skeleton3)
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