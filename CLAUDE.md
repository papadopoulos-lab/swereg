# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

**swereg** is an R package for manipulating and analyzing Swedish healthcare registry data in epidemiological research. It creates longitudinal data skeletons with ISO year-week structure and integrates multiple Swedish health registries (NPR, LMED, Cause of Death). The package is optimized for gender dysphoria research but works for general registry analysis.

## Development commands

### Core development workflow
```r
# Load package functions during development
devtools::load_all(".")

# Generate documentation
devtools::document()

# Check package integrity (includes tests, examples, documentation)
devtools::check()

# Alternative: Use R CMD check (more comprehensive, CRAN-style)
R CMD check .

# Build and install
devtools::build()
devtools::install()
```

### Testing and development scripts
```r
# Generate fake data (run once or when data structure changes)
source("dev/generate_fake_data.R")

# Quick functionality test
source("dev/quick_test.R")

# Full workflow test with all features
source("dev/test_with_fake_data.R")

# Memory-efficient batched workflow (production-style)
source("dev/workflow_batched_memory_efficient.R")

# Load required libraries for manual testing
library(data.table)
devtools::load_all(".")
```

## Architecture and data flow

### Three-stage workflow pattern
swereg follows a systematic three-stage approach ("Good Bones, Then Muscles"):

1. **skeleton1_create**: Raw data integration - Create time skeleton and merge registry data
2. **skeleton2_clean**: Data cleaning and derived variables - Clean and process within skeleton
3. **skeleton3_analyze**: Analysis-ready dataset preparation - Final analysis preparation

### Core pattern: longitudinal skeleton + sequential data integration
1. **Skeleton Creation**: `create_skeleton()` builds time-structured framework with individual IDs and ISO weeks
2. **Sequential Data Addition**: Use specialized functions to add different data types
3. **data.table Optimization**: All operations use data.table for efficient processing of large datasets

### Key integration functions (in typical order of use)
- `add_onetime()` - Baseline/demographic data (one record per person)
- `add_annual()` - Annual data for specific years (e.g., socioeconomic status)
- `add_diagnoses()` - NPR diagnosis data with ICD-10 codes (hospital visits)
- `add_operations()` - Surgical procedure codes from NPR
- `add_rx()` - LMED prescription data with ATC codes and treatment periods
- `add_cods()` - Cause of death information

### Swedish registry context
- **NPR** (National Patient Register): Specialist healthcare (inpatient `sv` and outpatient `ov`)
- **LMED** (Prescribed Drug Register): Prescription medications with ATC codes
- **SCB** (Statistics Sweden): Demographics and socioeconomic data
- **Cause of Death Register**: Mortality data with ICD-10 codes
- Uses `cstime` package for Swedish ISO time standards

## Critical data processing rules

### Always apply make_lowercase_names() with date cleaning
**ESSENTIAL**: All imported data must be processed with `swereg::make_lowercase_names()` before use. Now includes automatic date cleaning:

```r
# Read data
data <- fread("file.txt")  # or haven::read_sas() etc.

# REQUIRED: Apply lowercase transformation with date cleaning
swereg::make_lowercase_names(data, date_column = "INDATUM")

# Now safe to use with swereg functions - note that a 'date' column is created
swereg::add_diagnoses(skeleton, data, id_name = "lopnr", ...)
```

This transforms column names like `LopNr` → `lopnr`, `ATC` → `atc`, `INDATUM` → `indatum`, and converts specified date columns to Date class.

### Swedish date parsing
Swedish registry dates come in different precision levels. The `make_lowercase_names()` function with `date_columns` parameter handles:

```r
# Apply make_lowercase_names with date parsing
swereg::make_lowercase_names(data, date_columns = "INDATUM")

# Custom defaults for missing date parts
swereg::make_lowercase_names(data, date_columns = "INDATUM", 
                            default_month_day = "0101", default_day = "01")
```

**Date format handling:**
- **4 characters (YYYY)**: Year only → adds July 1st by default
- **6 characters (YYYYMM)**: Year-month → adds 15th by default  
- **8 characters (YYYYMMDD)**: Full date → uses as-is
- **Special cases**: "0000" → "0701", "00" → "15"

### Expected column names after make_lowercase_names()
- **Person IDs**: `lopnr` (SCB), `lopnr` (NPR after transformation), `p444_lopnr_personnr` (LMED)
- **Dates**: `indatum` (admission), `utdatum` (discharge), `edatum` (prescription), `dodsdat` (death)
- **Note**: Date columns are converted to Date class in place when date_columns parameter is used
- **Diagnosis codes**: `hdia` (main), `dia1`, `dia2`, etc. (secondary), `ekod1`, etc. (external causes)
- **Operation codes**: `op1`, `op2`, etc.
- **Prescription codes**: `atc` (drug code), `fddd` (treatment duration)

## Package data for development

The package includes synthetic Swedish registry data for development and examples:
- `fake_demographics` - SCB demographics (`lopnr`, `fodelseman`, `DodDatum`)
- `fake_annual_family` - SCB annual family data (`lopnr`, `FamTyp`)
- `fake_diagnoses` - Combined diagnoses with SOURCE column (inpatient/outpatient/cancer)
- `fake_prescriptions` - LMED prescription data (37 columns)
- `fake_cod` - Cause of death registry data
- `fake_person_ids` - Reference list of person identifiers

Load with: `data("fake_demographics")` etc.

**Note**: These are synthetic datasets designed to replicate the structure and characteristics of real Swedish registry data while maintaining confidentiality and privacy.

## Code patterns

### Typical analysis workflow
```r
# 1. Create skeleton (now includes personyears column)
skeleton <- create_skeleton(ids, "2001-01-01", "2020-12-31")

# 2. Add baseline data
demographics <- fread("demographics.csv")
swereg::make_lowercase_names(demographics, date_columns = "FodelseMan")
add_onetime(skeleton, demographics, "lopnr")

# 3. Add longitudinal data
hospital_data <- haven::read_sas("hospital.sas7bdat")
swereg::make_lowercase_names(hospital_data, date_columns = "INDATUM")
add_diagnoses(skeleton, hospital_data, "lopnr", diags = list(
  "depression" = c("^F32", "^F33"),
  "anxiety" = c("^F40", "^F41")
))

# 4. Add prescriptions
prescriptions <- fread("prescriptions.txt")
swereg::make_lowercase_names(prescriptions, date_columns = "EDATUM")
add_rx(skeleton, prescriptions, "lopnr", drugs = list(
  "antidepressants" = c("^N06A")
))

# 5. Create rowind variables (skeleton2_clean phase)
# Age at first depression diagnosis
make_rowind_first_occurrence(skeleton,
                            condition = "depression == TRUE",
                            value_var = "age",
                            new_var = "rowind_age_first_depression")

# Year of first antidepressant prescription  
make_rowind_first_occurrence(skeleton,
                            condition = "antidepressants == TRUE",
                            value_var = "isoyear",
                            new_var = "rowind_isoyear_first_antidep")
```

### Pattern matching for medical codes
- **Regex patterns**: Use `^` prefix for exact starts (e.g., `"^F640"`)
- **Exclusions**: Use `!` prefix to exclude (e.g., `"!F640"`)
- **Multiple patterns**: Combine in lists (e.g., `c("^F640", "^F648", "^F649")`)
- **Historical codes**: ICD-9 uses `[A-Z]` suffixes, ICD-8 uses comma delimiters

## Data Variable Types and Transformations

### Understanding rowdep vs rowind Variables

In longitudinal registry data analysis with swereg, variables are classified into two fundamental types:

- **rowdep** (row-dependent): Variables that can change over time for a person
- **rowind** (row-independent): Variables that cannot change over time for a person

This distinction is crucial for effective analysis, particularly during the `skeleton2_clean` phase where many transformations convert `rowdep` variables into `rowind` variables.

### Examples of Variable Types

**Row-dependent (rowdep) Variables:**
- `rowdep_edu_cat`: Education level (can improve over time)
- `rowdep_income_inflation_adjusted`: Annual income (changes yearly)
- `f64_diag`: Had diagnosis this week (TRUE/FALSE by time period)
- Current values that vary by isoyear/isoyearweek

**Row-independent (rowind) Variables:**
- `rowind_age_first_gd`: Age at first diagnosis (fixed once occurred)
- `rowind_isoyear_first_gd`: Year of first diagnosis (historical fact)
- `rowind_birthcountry`: Birth country (never changes)
- `rowind_register_tag`: Person's role in study (case, control, etc.)
- `rowind_age_death`: Age at death (fixed once occurred)

### Helper Function: make_rowind_first_occurrence()

The `make_rowind_first_occurrence()` function simplifies the common pattern of creating row-independent variables from the first occurrence of conditions:

```r
# Example: Create rowind variable for year of first F64 diagnosis
make_rowind_first_occurrence(skeleton,
                            condition = "f64_diag == TRUE",
                            value_var = "isoyear", 
                            new_var = "rowind_isoyear_first_f64")

# More complex condition example
make_rowind_first_occurrence(skeleton,
                            condition = "diag_gd_icd10_F64_089 == TRUE & is_amab == FALSE",
                            value_var = "age",
                            new_var = "rowind_age_first_gd_afab")
```

**Function features:**
- Automatically handles temp variable creation and cleanup
- Uses `first_non_na()` for robust aggregation across all variable types
- Includes comprehensive input validation and clear error messages
- Works with any condition that can be evaluated in data.table syntax

### Common rowdep → rowind Transformation Patterns

```r
# Manual pattern (traditional approach)
skeleton[condition_is_true, temp := value_to_capture]
skeleton[, new_rowind_var := first_non_na(temp), by = .(id)]
skeleton[, temp := NULL]

# Helper function pattern (recommended)
make_rowind_first_occurrence(skeleton, "condition_is_true", "value_to_capture", "new_rowind_var")
```

### Integration with swereg Workflow

The rowdep/rowind concept fits into the standard swereg workflow:

1. **skeleton1_create**: Focus on data integration, creates mostly rowdep variables
2. **skeleton2_clean**: Heavy focus on rowdep → rowind transformations  
3. **skeleton3_analyze**: Work with clean rowind variables for analysis

The `skeleton2_clean` phase is where most rowdep → rowind transformations occur, as you prepare stable person-level characteristics for downstream analysis.

### Naming Conventions

- Use `rowdep_*` prefix for time-varying variables
- Use `rowind_*` prefix for time-invariant variables
- Be descriptive: `rowind_age_first_gd` not `rowind_age`
- Include context: `rowind_isoyear_first_diagnosis` not `rowind_year`

### Best Practices

**Always validate rowind variables:**
```r
# Check that rowind variables are actually row-independent
skeleton[, .(unique_values = uniqueN(rowind_age_first_f64)), by = .(id)]
# Should return 1 for all persons (all rows have same value)
```

**For detailed examples and patterns**, see the "Understanding rowdep and rowind Variables" vignette:
```r
vignette("rowdep-rowind-concept", package = "swereg")
```

## Production workflow pattern

### Memory-efficient batched processing
For large datasets, use the 3-phase batched approach with helper functions in `example/R_generic_v002/`:

```r
# Phase 1: Read large datasets once
large_files <- read_large_files()

# Phase 2: Create skeleton1_create (compilation to skeleton format)
# Process in batches to manage memory
for(batch in batches) {
  skeleton1_create(batch, ids_batch, large_files)
}
rm(large_files)  # Remove from RAM

# Phase 3: Create skeleton2_clean (clean variables using only skeleton data)
for(batch in batches) {
  skeleton2_clean(batch)  # Use only data within skeleton
}
```

### Production example scripts
The `example/` directory contains production-style workflow implementations:
- `example/R_generic_v002/` - Helper functions for batched processing
- `example/Run_generic_v002.R` - Main production workflow script
- Individual project scripts showing real-world usage patterns

**Key principles:**
- **Batch processing**: Split individuals into groups (e.g., 50-100 per batch)
- **Memory management**: Remove large datasets after skeleton1_create phase
- **Self-contained cleaning**: skeleton2_clean uses only data within skeleton

## Key dependencies

**Core**: data.table, cstime, fs, stringr, dplyr, lubridate, haven  
**Development**: devtools, usethis, testthat, ggplot2
**Note**: Package now uses base pipe `|>` instead of magrittr `%>%` (requires R ≥ 4.1)

## Version control and release management

### REQUIRED: version updates
Whenever code is updated, **BOTH** of the following must be done:

**A) Update version in DESCRIPTION to YY.M.D format (remove leading zeroes):**
```r
# Example: For January 5, 2025
Version: 25.1.5

# Example: For December 25, 2024  
Version: 24.12.25
```

**B) Update NEWS.md with changes:**
```markdown
# swereg YY.M.D

## Bug Fixes
* Fixed issue with...

## New Features  
* Added function for...

## Documentation
* Updated vignette for...
```

### Version format rules
- Use `YY.M.D` format (e.g., `25.1.5` not `25.01.05`)
- Remove all leading zeroes from month and day
- Update both DESCRIPTION and NEWS.md simultaneously
- Document all user-facing changes in NEWS.md

### Git configuration requirements
Before working with this repository, ensure git is properly configured:

```bash
# Required identity configuration
git config --global user.email "hello@rwhite.no"
git config --global user.name "Richard Aubrey White"

# Required signing configuration for security
git config --global commit.gpgsign true
git config --global gpg.format ssh
git config --global user.signingkey ~/.ssh/id_ed25519.pub

# Required merge behavior
git config --global pull.rebase false
```

**Note**: Adjust the SSH key path if working in a different environment. The signing key should point to the public SSH key file.

### Git commit message format
**REQUIRED**: All commit messages must follow this format (NO Claude/AI attribution):

```
Brief description of changes

- Bullet point of change 1
- Bullet point of change 2  
- Bullet point of change 3
```

**NEVER include**:
- Claude/AI attribution
- "Generated with Claude Code" 
- "Co-Authored-By: Claude"
- Any mention of AI assistance

**Guidelines**:
- Use standard commit message format: "Add feature X", "Fix bug in Y", "Update documentation"
- Focus on what was changed, not who/what made the change
- Example: "Add fake Swedish registry datasets" (not "Claude added fake datasets")
- All commits must be signed (configured above)

## GitHub actions and documentation

### Automated pkgdown documentation
The repository includes GitHub Actions that automatically:
- Build pkgdown documentation on every push to main/master
- Deploy to GitHub Pages at: `https://papadopoulos-lab.github.io/swereg/`
- Include all vignettes, function documentation, and news updates

### Setup requirements (one-time)
1. **Enable GitHub Pages**: Go to Settings > Pages > Source: "GitHub Actions"
2. **Permissions**: Ensure Actions have write permissions (Settings > Actions > General)
3. **Branch Protection**: Configure main/master branch as default

### Local pkgdown development
```r
# Install pkgdown if not already installed
install.packages("pkgdown")

# Build site locally to preview
pkgdown::build_site()

# Open in browser
pkgdown::preview_site()
```

The site automatically includes:
- Function reference with examples
- Vignettes as "Articles" 
- News/changelog from NEWS.md
- Automatic linking between functions

## Vignettes structure

The package includes three vignettes following a progressive learning structure:

- **Basic workflow**: `vignette("basic-workflow")` - Introduction to skeleton1_create stage
- **Complete workflow**: `vignette("complete-workflow")` - Two-stage pipeline (skeleton1_create + skeleton2_clean)
- **Memory-efficient batching**: `vignette("memory-efficient-batching")` - Complete three-stage pipeline with production-scale batching

### Vignette organization
- **Getting Started**: basic-workflow (focuses on skeleton1_create only)
- **Advanced Usage**: complete-workflow and memory-efficient-batching (full workflows)
- **Clear progression**: Each vignette builds on the previous without overlap

## Function documentation improvements

All exported functions now include:
- **@family tags**: Functions grouped by purpose (data_integration, skeleton_creation, data_preprocessing)
- **@seealso sections**: Cross-references to related functions and vignettes
- **Runnable examples**: All examples use synthetic data included in the package
- **Comprehensive parameter documentation**: Clear descriptions of expected inputs and outputs
- **Academic tone**: Professional, objective language appropriate for scientific software
- **Better return value descriptions**: Explicit documentation of side effects and modifications

## CRAN submission preparation

### Critical requirements checklist
Before CRAN submission, always verify:

1. **Remove non-portable files**: Delete Synology-specific `@eaDir` directories
2. **URL consistency**: Ensure DESCRIPTION and .onAttach use same GitHub organization URLs
3. **Test coverage**: Add comprehensive tests with testthat (aim for >80% coverage)
4. **Runnable examples**: Convert all `\dontrun{}` to executable examples using fake data
5. **LICENSE year**: Update to current year
6. **Clean inst/ directory**: Only keep files referenced by package functions
7. **Dependencies**: Use `@importFrom` or verify cstime:: usage is documented

### CRAN check workflow
```r
# Standard package check
devtools::check()

# REQUIRED: CRAN compliance check
R CMD check . --as-cran

# Only run rhub when explicitly requested (requires manual review)
# rhub::rhub_check()
```

## Package organization best practices

### Conceptual function separation
Organize functions by conceptual purpose, not just technical similarity:

- **Data transformation** (`R/data_transformations.R`): Functions that change data meaning/structure (e.g., `make_rowind_first_occurrence`)
- **Utility functions** (`R/helper_functions.R`): Basic data processing helpers (e.g., `make_lowercase_names`, date parsing)
- **Core functions** (`R/skeleton_functions.R`): Main workflow functions

### Dual formatting systems approach
When creating packages that may serve different audiences (local vs international), consider implementing dual formatting systems:

- **Separate by use case**: Group functions by intended audience rather than technical similarity
- **Consistent API patterns**: Use parallel naming (`format_*_as_local` vs `format_*_as_international`) 
- **Internal helper functions**: Create shared internal functions with `@noRd` to avoid namespace clutter
- **Comprehensive testing**: Include comparison tests that verify differences between formatting systems
- **Clear documentation**: Use vignettes to demonstrate differences with side-by-side examples

**Example application to swereg**: If implementing output formatting for Swedish research (domestic) vs international publication, separate functions would improve discoverability and prevent formatting errors in different contexts.

### pkgdown structure principles
```yaml
# Group by conceptual purpose in _pkgdown.yml
- title: Data transformation
  desc: Functions for transforming data structure and creating derived variables
- contents:
  - make_rowind_first_occurrence

- title: Utility functions  
  desc: Helper functions for data processing
- contents:
  - make_lowercase_names
  - parse_swedish_date
```

## Documentation standards

### Vignette title formatting
Use sentence case for all vignette titles and subtitles:
- ✅ "Variable types: rowdep vs rowind"
- ❌ "Variable Types: Rowdep vs Rowind"
- ✅ "Understanding the concept"
- ❌ "Understanding The Concept"

### Function documentation requirements
- Use `@family` tags for logical grouping
- Include `@seealso` references to related functions
- Provide runnable examples using package fake data
- Document side effects explicitly for functions that modify by reference

## Common issues and solutions

1. **Column name errors**: Always use `make_lowercase_names()` after reading data
2. **Missing cstime functions**: Install cstime package for Swedish time standards
3. **Large dataset memory**: Use data.table operations and avoid copying large datasets
4. **Date format issues**: Ensure dates are in Date class, not character
5. **ID mismatches**: Check that ID columns match between skeleton and data after name transformation
6. **CRAN submission failures**: Review checklist above and run `R CMD check . --as-cran`
7. **Vignette build errors**: Ensure all referenced columns exist in fake data examples