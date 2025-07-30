# swereg 25.7.30

## New Features

* **NEW**: `make_rowind_first_occurrence()` helper function for rowdep → rowind transformations:
  - Simplifies the common pattern of creating row-independent variables from first occurrence of conditions
  - Automatically handles temp variable creation and cleanup
  - Uses `first_non_na()` for robust aggregation across all variable types
  - Includes comprehensive input validation and clear error messages

* **NEW**: "Understanding rowdep and rowind Variables" vignette:
  - Explains the fundamental distinction between row-dependent and row-independent variables
  - Demonstrates common transformation patterns with practical examples
  - Shows integration with the swereg workflow (skeleton1_create → skeleton2_clean → skeleton3_analyze)
  - Includes best practices for longitudinal registry data analysis

## Documentation

* **ENHANCED**: Helper functions now include `@family data_integration` tags for better organization
* **IMPROVED**: Function examples use existing fake datasets for consistency

# swereg 25.7.16

## New Swedish Date Parsing and Enhanced Data Cleaning

* **NEW**: `parse_swedish_date()` function for handling Swedish registry dates with varying precision:
  - Handles 4-character (YYYY), 6-character (YYYYMM), and 8-character (YYYYMMDD) formats
  - Automatically replaces "0000" with "0701" and "00" with "15" for missing date components
  - Supports custom defaults for missing date parts
  - Includes comprehensive error handling and vectorized processing

* **ENHANCED**: `make_lowercase_names()` now supports automatic date cleaning:
  - New `date_column` parameter to specify which column contains dates
  - Automatically creates cleaned 'date' column using `parse_swedish_date()`
  - Works with both default and data.table methods
  - Maintains backward compatibility with existing code

* **ENHANCED**: All `add_*` functions now require cleaned date columns:
  - `add_diagnoses()`, `add_operations()`, `add_rx()`, `add_cods()` expect 'date' column
  - Clear error messages guide users to use `make_lowercase_names(data, date_column = "...")` 
  - Improved validation ensures data preprocessing consistency

* **ENHANCED**: `create_skeleton()` now includes `personyears` column:
  - Annual rows (is_isoyear==TRUE) have personyears = 1
  - Weekly rows (is_isoyear==FALSE) have personyears = 1/52.25
  - Facilitates person-time calculations for survival analysis

* **IMPROVED**: Survival analysis cookbook vignette updated:
  - Uses weekly data instead of yearly data for more precise analyses
  - Age calculation based on isoyearweek_sunday instead of isoyear
  - Includes person-time in descriptive statistics
  - Demonstrates proper use of new date cleaning workflow

## Enhanced error handling and validation

* **ENHANCED**: Comprehensive input validation for all `add_*` functions:
  - `add_onetime()`: Validates skeleton structure, ID column exists, checks for ID matches
  - `add_annual()`: Validates isoyear parameter, checks skeleton year coverage
  - `add_diagnoses()`: Validates diagnosis patterns, checks for diagnosis code columns
  - `add_operations()`: Validates operation patterns, checks for operation code columns  
  - `add_rx()`: Validates prescription data structure, checks source columns
  - `add_cods()`: Validates death data structure, checks cause of death columns
* **IMPROVED**: User-friendly error messages with specific guidance:
  - Clear indication when `make_lowercase_names()` is forgotten
  - Helpful suggestions for column naming issues
  - Informative ID mismatch diagnostics with sample values
* **NEW**: Internal validation helper functions for consistent error handling
* **ADDED**: Input validation for pattern lists, data structures, and parameter ranges

## New cookbook documentation

* **NEW**: Comprehensive survival analysis cookbook (`cookbook-survival-analysis.Rmd`):
  - Complete workflow from raw data to Cox proportional hazards model
  - Time-varying covariates (annual income) with heart attack outcome
  - Handles common challenges: missing data, multiple events, competing risks
  - Performance tips for large datasets
  - Practical solutions for real-world registry analysis
* **ENHANCED**: Updated `_pkgdown.yml` with new "Cookbooks" section
* **ADDED**: `survival` package to Suggests dependencies

## Bug fixes

* **FIXED**: Improved ID matching warnings and error messages across all functions
* **CORRECTED**: Better handling of missing data in time-varying covariate analysis
* **ENHANCED**: More robust parameter validation prevents common user errors

# swereg 25.7.16

## Major documentation restructuring

* **RESTRUCTURED**: Complete vignette reorganization for clear learning progression:
  - NEW "Skeleton concept" vignette: Conceptual foundation explaining the skeleton approach without technical implementation
  - "Building the data skeleton (skeleton1_create)": Pure data integration focus - raw data to time-structured skeleton
  - "Cleaning and deriving variables (skeleton2_clean)": Pure data cleaning and variable derivation focus
  - "Production analysis workflows (skeleton3_analyze)": Memory-efficient processing and final analysis datasets
* **IMPROVED**: Clear separation of concerns with focused, single-purpose tutorials
* **ENHANCED**: Systematic learning progression from concept to implementation to production
* **UPDATED**: _pkgdown.yml structure with logical vignette grouping
* **PRESERVED**: All existing technical content while improving organization

## Content improvements

* **NEW**: Comprehensive conceptual introduction based on presentation content
* **IMPROVED**: Each vignette builds systematically on the previous one
* **ENHANCED**: Better explanation of three types of data integration (one-time, annual, event-based)
* **CLARIFIED**: Production workflow patterns with memory-efficient batching strategies
* **STANDARDIZED**: Consistent academic tone and sentence case throughout

# swereg 25.7.15

## Documentation and presentation improvements

* **STANDARDIZED**: Changed all titles and headings to normal sentence case throughout:
  - Vignette titles: "Basic Workflow" → "Basic workflow", "Complete Workflow" → "Complete workflow", etc.
  - README.md section headings: "Core Functions" → "Core functions", "Data Integration" → "Data integration", etc.
  - NEWS.md section headings: "Vignette Restructuring" → "Vignette restructuring", etc.
  - CLAUDE.md section headings: "Project Overview" → "Project overview", "Development Commands" → "Development commands", etc.
* **IMPROVED**: Consistent normal sentence case for better readability and less formal appearance
* **SIMPLIFIED**: Removed subtitle text after colons in vignette titles for cleaner presentation
* **ENHANCED**: Improved Core Concept section in basic workflow vignette with clear explanation of three data types:
  - One-time data (demographics): Added to all rows for each person
  - Annual data (income, family status): Added to all rows for specific year
  - Event-based data (diagnoses, prescriptions, deaths): Added to rows where events occurred
* **CLARIFIED**: Step 1 documentation now properly explains all skeleton columns including `isoyearweek_sunday`
* **VERIFIED**: All vignettes compile successfully with improved content

## Major documentation and vignette reorganization

* **RESTRUCTURED**: Complete vignette reorganization with improved naming and content flow:
  - `swereg.Rmd` → `basic-workflow.Rmd`: Focused introduction to skeleton1_create
  - `advanced-workflow.Rmd` → `complete-workflow.Rmd`: Two-stage workflow (skeleton1_create + skeleton2_clean)
  - `memory-efficient-batching.Rmd`: Maintained as comprehensive three-stage workflow guide
* **IMPROVED**: Eliminated content redundancy between vignettes for clearer learning progression
* **ENHANCED**: Updated _pkgdown.yml configuration to reflect new vignette structure

## Function documentation improvements

* **ENHANCED**: Comprehensive documentation improvements for all exported functions:
  - Added @family tags for logical grouping (data_integration, skeleton_creation, data_preprocessing)
  - Added @seealso sections with cross-references to related functions and vignettes
  - Replaced placeholder examples with runnable code using synthetic data
  - Improved parameter documentation with detailed descriptions and expected formats
  - Enhanced return value documentation with explicit side effects description
* **STANDARDIZED**: Consistent academic tone throughout all documentation

## Professional presentation updates

* **IMPROVED**: Removed informal elements and adopted academic tone across all documentation
* **UPDATED**: Changed terminology from "fake data" to "synthetic data" throughout
* **ENHANCED**: More professional language in README.md and vignettes
* **STANDARDIZED**: Consistent formal tone appropriate for scientific software

## Technical improvements

* **VERIFIED**: All vignettes compile successfully with updated content
* **TESTED**: Package passes R CMD check with all documentation improvements
* **UPDATED**: CLAUDE.md reflects new vignette structure and documentation standards

# swereg 25.7.1

## Vignette restructuring

* **RESTRUCTURED**: Reorganized vignettes for clearer learning progression:
  - `swereg.Rmd`: Clean skeleton1_create tutorial using full datasets (removed subset filtering)
  - `advanced-workflow.Rmd`: Focused skeleton1→skeleton2 workflow (removed batching and skeleton3 content)
  - `memory-efficient-batching.Rmd`: NEW comprehensive batching vignette with complete skeleton1→skeleton2→skeleton3 workflow for large-scale studies
* **IMPROVED**: GitHub Actions workflow optimization with dependency caching and binary packages for faster CI/CD

## Batching vignette fixes

* **FIXED**: Updated memory-efficient-batching vignette with production-ready improvements:
  - Replace `split()` with `csutil::easy_split` for better batch handling
  - Replace `saveRDS/readRDS` with `qs::qsave/qread` for 2-10x faster file I/O
  - Fix skeleton3_analyze to properly aggregate weekly→yearly data using `swereg::max_with_infinite_as_na`
  - Remove incorrect `is_isoyear == TRUE` filter in skeleton3_analyze
  - Fix analysis results to avoid NaN outputs in treatment rate calculations
  - Add explanations for weekly→yearly data aggregation and qs package performance benefits

## New features

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

## Bug fixes

* **CRITICAL**: Fixed incorrect variable names in `fake_cod` dataset - changed from non-Swedish `underlying_cod/contributory_cod1/contributory_cod2` to correct Swedish registry names `ulorsak/morsak1/morsak2`
* **VERIFIED**: Confirmed all fake datasets use correct Swedish registry variable name conventions
* **VERIFIED**: All ICD-10 and ATC codes in fake datasets are properly formatted and realistic

## Documentation improvements

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

## Package structure

* All exported functions now have complete, accurate documentation suitable for CRAN submission
* Documentation focuses on Swedish registry data analysis workflows
* Examples use `\dontrun{}` appropriately for functions requiring external data