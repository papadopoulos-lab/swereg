# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Communication Style

Do not oversell capabilities (e.g., static parsing of R files). When the
user pushes back on a claim or design critique, seriously re-evaluate
rather than defending the initial position. Treat pushback as signal
that the first answer was likely wrong.

## Project overview

**swereg** is an R package for manipulating and analyzing Swedish
healthcare registry data in epidemiological research. It creates
longitudinal data skeletons with ISO year-week structure and integrates
multiple Swedish health registries (NPR, LMED, Cause of Death). The
package is optimized for gender dysphoria research but works for general
registry analysis.

## Development commands

### Core development workflow

``` r
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

``` r
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

### Two-step workflow pattern

swereg follows a two-step approach:

1.  **Create the skeleton**: Build the time grid, integrate raw registry
    data, and derive analysis-ready variables
2.  **Analyse the skeleton**: Collapse to the right granularity (e.g.,
    weekly → yearly) and run analyses

### Core pattern: longitudinal skeleton + sequential data integration

1.  **Skeleton Creation**:
    [`create_skeleton()`](https://papadopoulos-lab.github.io/swereg/reference/create_skeleton.md)
    builds time-structured framework with individual IDs and ISO weeks
2.  **Sequential Data Addition**: Use specialized functions to add
    different data types
3.  **data.table Optimization**: All operations use data.table for
    efficient processing of large datasets

### Key integration functions (in typical order of use)

- [`add_onetime()`](https://papadopoulos-lab.github.io/swereg/reference/add_onetime.md) -
  Baseline/demographic data (one record per person)
- [`add_annual()`](https://papadopoulos-lab.github.io/swereg/reference/add_annual.md) -
  Annual data for specific years (e.g., socioeconomic status)
- [`add_diagnoses()`](https://papadopoulos-lab.github.io/swereg/reference/add_diagnoses.md) -
  NPR diagnosis data with ICD-10 codes (hospital visits)
- [`add_operations()`](https://papadopoulos-lab.github.io/swereg/reference/add_operations.md) -
  Surgical procedure codes from NPR
- [`add_rx()`](https://papadopoulos-lab.github.io/swereg/reference/add_rx.md) -
  LMED prescription data with ATC codes and treatment periods
- [`add_cods()`](https://papadopoulos-lab.github.io/swereg/reference/add_cods.md) -
  Cause of death information

### Swedish registry context

- **NPR** (National Patient Register): Specialist healthcare (inpatient
  `sv` and outpatient `ov`)
- **LMED** (Prescribed Drug Register): Prescription medications with ATC
  codes
- **SCB** (Statistics Sweden): Demographics and socioeconomic data
- **Cause of Death Register**: Mortality data with ICD-10 codes
- Uses `cstime` package for Swedish ISO time standards

## Critical data processing rules

### Always apply make_lowercase_names() with date cleaning

**ESSENTIAL**: All imported data must be processed with
[`swereg::make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
before use. Now includes automatic date cleaning:

``` r
# Read data
data <- fread("file.txt")  # or haven::read_sas() etc.

# REQUIRED: Apply lowercase transformation with date cleaning
swereg::make_lowercase_names(data, date_column = "INDATUM")

# Now safe to use with swereg functions - note that a 'date' column is created
swereg::add_diagnoses(skeleton, data, id_name = "lopnr", ...)
```

This transforms column names like `LopNr` → `lopnr`, `ATC` → `atc`,
`INDATUM` → `indatum`, and converts specified date columns to Date
class.

### Swedish date parsing

Swedish registry dates come in different precision levels. The
[`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
function with `date_columns` parameter handles:

``` r
# Apply make_lowercase_names with date parsing
swereg::make_lowercase_names(data, date_columns = "INDATUM")

# Custom defaults for missing date parts
swereg::make_lowercase_names(data, date_columns = "INDATUM", 
                            default_month_day = "0101", default_day = "01")
```

**Date format handling:** - **4 characters (YYYY)**: Year only → adds
July 1st by default - **6 characters (YYYYMM)**: Year-month → adds 15th
by default  
- **8 characters (YYYYMMDD)**: Full date → uses as-is - **Special
cases**: “0000” → “0701”, “00” → “15”

### Expected column names after make_lowercase_names()

- **Person IDs**: `lopnr` (SCB), `lopnr` (NPR after transformation),
  `p444_lopnr_personnr` (LMED)
- **Dates**: `indatum` (admission), `utdatum` (discharge), `edatum`
  (prescription), `dodsdat` (death)
- **Note**: Date columns are converted to Date class in place when
  date_columns parameter is used
- **Diagnosis codes**: `hdia` (main), `dia1`, `dia2`, etc. (secondary),
  `ekod1`, etc. (external causes)
- **Operation codes**: `op1`, `op2`, etc.
- **Prescription codes**: `atc` (drug code), `fddd` (treatment duration)

## Package data for development

The package includes synthetic Swedish registry data for development and
examples: - `fake_demographics` - SCB demographics (`lopnr`,
`fodelseman`, `DodDatum`) - `fake_annual_family` - SCB annual family
data (`lopnr`, `FamTyp`) - `fake_diagnoses` - Combined diagnoses with
SOURCE column (inpatient/outpatient/cancer) - `fake_prescriptions` -
LMED prescription data (37 columns) - `fake_cod` - Cause of death
registry data - `fake_person_ids` - Reference list of person identifiers

Load with: `data("fake_demographics")` etc.

**Note**: These are synthetic datasets designed to replicate the
structure and characteristics of real Swedish registry data while
maintaining confidentiality and privacy.

## Code patterns

### Typical analysis workflow

``` r
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

# 3b. Optional: Track diagnoses by source (inpatient/outpatient) separately
# Filter the dataset before calling add_diagnoses() for source-specific tracking
inpatient_data <- hospital_data[source == "inpatient"]
add_diagnoses(skeleton, inpatient_data, "lopnr", diags = list(
  "depression_inpatient" = c("^F32", "^F33")
))
outpatient_data <- hospital_data[source == "outpatient"]
add_diagnoses(skeleton, outpatient_data, "lopnr", diags = list(
  "depression_outpatient" = c("^F32", "^F33")
))

# 4. Add prescriptions
prescriptions <- fread("prescriptions.txt")
swereg::make_lowercase_names(prescriptions, date_columns = "EDATUM")
add_rx(skeleton, prescriptions, "lopnr", drugs = list(
  "antidepressants" = c("^N06A")
))

# 5. Create row-independent (ri_*) variables from first occurrences
# Age at first depression diagnosis
make_rowind_first_occurrence(skeleton,
                            condition = "depression == TRUE",
                            value_var = "age",
                            new_var = "ri_age_first_depression")

# Year of first antidepressant prescription
make_rowind_first_occurrence(skeleton,
                            condition = "antidepressants == TRUE",
                            value_var = "isoyear",
                            new_var = "ri_isoyear_first_antidep")
```

### Pattern matching for medical codes

- **Literal prefixes**: `add_*` matches via
  [`startsWith()`](https://rdrr.io/r/base/startsWith.html) on literal
  prefixes (no regex). Write `"F640"` for any code beginning with F640.
  Do NOT prefix with `^` – that is a regex anchor and will never match
  under [`startsWith()`](https://rdrr.io/r/base/startsWith.html). (Since
  26.5.9, the pre-call syntax check warns about this.)
- **Bracket expansion**: Bracket / character-class / range patterns are
  accepted directly and expanded automatically: `"I2[0-5]"` -\>
  `c("I20","I21","I22","I23","I24","I25")`; `"FN[ABCDEGW][0-9][0-9]"`;
  `"!302[A-Z]"`. Multiple bracket groups in one pattern produce the
  Cartesian product.
- **Exclusions**: Use `!` prefix to exclude (e.g., `"!F640"`).
- **Multiple patterns**: Combine in vectors (e.g.,
  `c("F640", "F648", "F649")`).
- **Historical codes**: ICD-9 uses `[A-Z]` suffixes, ICD-8 uses comma
  delimiters.

## Data Variable Types and Transformations

### Understanding rd\_ (row-dependent) vs ri\_ (row-independent) variables

In longitudinal registry data analysis with swereg, every derived
variable is classified into two fundamental shapes, distinguished by a
short name prefix:

- **`rd_`** (row-dependent): Variables that can change over time for a
  person
- **`ri_`** (row-independent): Variables that are fixed per person

This distinction is crucial for effective analysis: many phase-3
randvars steps convert `rd_` variables into `ri_` variables by capturing
a value at a specific moment (e.g., age at first diagnosis).

### Examples of Variable Types

**Row-dependent (`rd_*`) Variables:** - `rd_education`: Education level
(can improve over time) - `rd_income_inflation_adjusted`: Annual income
(changes yearly) - `rd_age_continuous`: Continuous age (increases each
week) - `rd_civil_status`: Can change on marriage, divorce,
bereavement - `f64_diag`: Had diagnosis this week (TRUE/FALSE by time
period, produced by a phase-2 code registration)

**Row-independent (`ri_*`) Variables:** - `ri_age_first_dx`: Age at
first diagnosis (fixed once occurred) - `ri_isoyear_first_dx`: Year of
first diagnosis (historical fact) - `ri_birthcountry`: Birth country
(never changes) - `ri_register_tag`: Person’s role in study (case,
control, etc.) - `ri_age_death`: Age at death (fixed once occurred)

### Helper Function: make_rowind_first_occurrence()

The
[`make_rowind_first_occurrence()`](https://papadopoulos-lab.github.io/swereg/reference/make_rowind_first_occurrence.md)
function simplifies the common pattern of creating `ri_*` variables from
the first occurrence of a condition:

``` r
# Year of first F64 diagnosis
make_rowind_first_occurrence(skeleton,
                            condition = "f64_diag == TRUE",
                            value_var = "isoyear",
                            new_var = "ri_isoyear_first_f64")

# More complex condition
make_rowind_first_occurrence(skeleton,
                            condition = "f64_diag == TRUE & ri_is_amab == FALSE",
                            value_var = "rd_age_continuous",
                            new_var = "ri_age_first_f64_afab")
```

**Function features:** - Automatically handles temp variable creation
and cleanup - Uses
[`first_non_na()`](https://papadopoulos-lab.github.io/swereg/reference/first_non_na.md)
for robust aggregation across all variable types - Includes
comprehensive input validation and clear error messages - Works with any
condition that can be evaluated in data.table syntax

### Common rd\_ → ri\_ transformation patterns

``` r
# Manual pattern (traditional approach)
skeleton[condition_is_true, temp := value_to_capture]
skeleton[, new_ri_var := first_non_na(temp), by = .(id)]
skeleton[, temp := NULL]

# Helper function pattern (recommended)
make_rowind_first_occurrence(skeleton, "condition_is_true", "value_to_capture", "new_ri_var")
```

### Integration with swereg workflow

The `rd_`/`ri_` concept maps onto the three-phase
`RegistryStudy$process_skeletons()` pipeline:

1.  **Phase 1 – framework**: produces the base time grid and structural
    censoring. Usually includes `rd_age_continuous`.
2.  **Phase 2 – codes**: produces code-derived columns (`os_*`, `osd_*`,
    `rx_*`, `op_*`) that are de-facto row-dependent.
3.  **Phase 3 – randvars**: the heavy lifting for `rd_` -\> `ri_`
    transformations. Typical randvars steps add both time-varying LISA
    demographics (`rd_education`, `rd_income_inflation_adjusted`,
    `rd_civil_status`) and row-independent first-occurrence variables
    (`ri_age_first_dx`, `ri_isoyear_first_dx`).

### Naming conventions

- Use `rd_*` prefix for time-varying variables
- Use `ri_*` prefix for time-invariant variables
- Be descriptive: `ri_age_first_f20` not `ri_age`
- Include context: `ri_isoyear_first_diagnosis` not `ri_year`

### Best Practices

**Always validate `ri_*` variables:**

``` r
# Check that ri_* variables are actually row-independent
skeleton[, .(unique_values = uniqueN(ri_age_first_f20)), by = .(id)]
# Should return 1 for all persons (all rows have same value)
```

**For detailed examples and patterns**, see the variable types vignette:

``` r
vignette("rowdep-rowind-concept", package = "swereg")
```

## TTE (Target Trial Emulation) system

### Spec-driven configuration

[`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md)
parses a YAML study specification. Required top-level sections: `study`,
`enrollments`, `outcomes`, `follow_up`. Optional: `inclusion_criteria`,
`exclusion_criteria`, `confounders`, `open_questions`.

Key spec structure: - **study**: title, PI, `description`,
`implementation.project_prefix` - **inclusion_criteria.isoyears**:
`[start, end]` — global ISO year range filter - **exclusion_criteria**:
list with `name`, `window` (numeric weeks or `"lifetime"`),
`implementation.variable` - **enrollments**: each has `id`,
`additional_inclusion` (e.g. age range), `additional_exclusion`,
`treatment` with `matching_ratio` and `implementation` (variable,
intervention/comparator values, seed)

### Key TTE functions in `R/r6_tteplan.R`

- `tteplan_read_spec(path)` — parse + validate YAML, convert windows to
  weeks
- `tteplan_apply_exclusions(skeleton, spec, enrollment_spec)` — applies
  isoyear filter from `spec$inclusion_criteria$isoyears`, then
  additional_inclusion, global exclusion_criteria, and
  additional_exclusion
- `tteplan_apply_derived_confounders(skeleton, spec)` — computes
  rolling-window indicators for `computed: true` confounders
- `tteplan_validate_spec(spec, skeleton)` — checks all spec variables
  exist in skeleton columns
- `tteplan_from_spec_and_registrystudy(spec, study)` — creates TTEPlan
  with full ETT grid

### R6 classes

- **TTEDesign** (`R/r6_tteenrollment.R`): holds confounder_vars,
  time_treatment_var, eligible_var
- **TTEEnrollment** (`R/r6_tteenrollment.R`): data + design, lifecycle
  stages (pre_enrollment → enrolled → analysis_ready). Public workflow
  methods use step-number prefixes: `$s1_collapse()`,
  `$s2_impute_confounders()`, `$s3_ipw()`, `$s4_truncate_weights()`,
  `$s5_prepare_for_analysis()`
- **TTEPlan** (`R/r6_tteplan.R`): ETT grid,
  `$s1_generate_enrollments_and_ipw()`,
  `$s2_generate_analysis_files_and_ipcw_pp()`

## Production workflow pattern

### RegistryStudy pipeline

For production-scale pipelines, use the `RegistryStudy` R6 class which
handles rawbatch creation, skeleton processing (framework + randvars +
code registry), and incremental rebuilds. See
[`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md)
for details.

### Production example scripts

The `example/` directory contains production-style workflow
implementations: - `example/R_generic_v002/` - Helper functions for
batched processing - `example/Run_generic_v002.R` - Main production
workflow script - Individual project scripts showing real-world usage
patterns

**Key principles:** - **Batch processing**: Split individuals into
groups via `RegistryStudy$set_ids()` - **Incremental rebuilds**: Only
changed pipeline steps re-run - **Memory management**: Rawbatch files
keep large datasets out of RAM during skeleton processing

## Key dependencies

**Core**: data.table, cstime, fs, stringr, dplyr, lubridate, haven  
**Development**: devtools, usethis, testthat, ggplot2 **Note**: Package
now uses base pipe `|>` instead of magrittr `%>%` (requires R ≥ 4.1)

## Version control and release management

### REQUIRED: version updates

Whenever code is updated, **BOTH** of the following must be done:

**A) Update version in DESCRIPTION to YY.M.D format (remove leading
zeroes):**

``` r
# Example: For January 5, 2025
Version: 25.1.5

# Example: For December 25, 2024  
Version: 24.12.25
```

**B) Update NEWS.md with changes:**

``` markdown
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

``` bash
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

**Note**: Adjust the SSH key path if working in a different environment.
The signing key should point to the public SSH key file.

### Git commit message format

**REQUIRED**: All commit messages must follow this format (NO Claude/AI
attribution):

    Brief description of changes

    - Bullet point of change 1
    - Bullet point of change 2
    - Bullet point of change 3

**NEVER include**: - Claude/AI attribution - “Generated with Claude
Code” - “Co-Authored-By: Claude” - Any mention of AI assistance

**Guidelines**: - Use standard commit message format: “Add feature X”,
“Fix bug in Y”, “Update documentation” - Focus on what was changed, not
who/what made the change - Example: “Add fake Swedish registry datasets”
(not “Claude added fake datasets”) - All commits must be signed
(configured above)

## GitHub actions and documentation

### Automated pkgdown documentation

The repository includes GitHub Actions that automatically: - Build
pkgdown documentation on every push to main/master - Deploy to GitHub
Pages at: `https://papadopoulos-lab.github.io/swereg/` - Include all
vignettes, function documentation, and news updates

### Setup requirements (one-time)

1.  **Enable GitHub Pages**: Go to Settings \> Pages \> Source: “GitHub
    Actions”
2.  **Permissions**: Ensure Actions have write permissions (Settings \>
    Actions \> General)
3.  **Branch Protection**: Configure main/master branch as default

### Local pkgdown development

``` r
# Install pkgdown if not already installed
install.packages("pkgdown")

# Build site locally to preview
pkgdown::build_site()

# Open in browser
pkgdown::preview_site()
```

The site automatically includes: - Function reference with examples -
Vignettes as “Articles” - News/changelog from NEWS.md - Automatic
linking between functions

## Vignettes structure

The package vignettes follow a progressive learning structure:

### Concept

- **Skeleton concept**:
  [`vignette("skeleton-concept")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-concept.md) -
  Why the person-week time grid
- **R6 class overview**:
  [`vignette("r6-class-overview")`](https://papadopoulos-lab.github.io/swereg/articles/r6-class-overview.md) -
  Overview of R6 classes
- **Variable types**:
  [`vignette("rowdep-rowind-concept")`](https://papadopoulos-lab.github.io/swereg/articles/rowdep-rowind-concept.md) -
  rd\_ vs ri\_ variable conventions

### Manual workflow (2 steps)

- **Creating the skeleton**:
  [`vignette("skeleton-create")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-create.md) -
  Build the time grid, integrate data, derive variables
- **Analysing the skeleton**:
  [`vignette("skeleton-analyze")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-analyze.md) -
  Collapse to the right granularity and run analyses

### Pipeline (production)

- **Skeleton pipeline**:
  [`vignette("skeleton-pipeline")`](https://papadopoulos-lab.github.io/swereg/articles/skeleton-pipeline.md) -
  R6-based RegistryStudy with incremental rebuilds
- **TTE workflow**:
  [`vignette("tte-workflow")`](https://papadopoulos-lab.github.io/swereg/articles/tte-workflow.md) -
  Target trial emulation workflow

## Function documentation improvements

All exported functions now include: - **@family tags**: Functions
grouped by purpose (data_integration, skeleton_creation,
data_preprocessing) - **@seealso sections**: Cross-references to related
functions and vignettes - **Runnable examples**: All examples use
synthetic data included in the package - **Comprehensive parameter
documentation**: Clear descriptions of expected inputs and outputs -
**Academic tone**: Professional, objective language appropriate for
scientific software - **Better return value descriptions**: Explicit
documentation of side effects and modifications

## CRAN submission preparation

### Critical requirements checklist

Before CRAN submission, always verify:

1.  **Remove non-portable files**: Delete Synology-specific `@eaDir`
    directories
2.  **URL consistency**: Ensure DESCRIPTION and .onAttach use same
    GitHub organization URLs
3.  **Test coverage**: Add comprehensive tests with testthat (aim for
    \>80% coverage)
4.  **Runnable examples**: Convert all `\dontrun{}` to executable
    examples using fake data
5.  **LICENSE year**: Update to current year
6.  **Clean inst/ directory**: Only keep files referenced by package
    functions
7.  **Dependencies**: Use `@importFrom` or verify cstime:: usage is
    documented

### CRAN check workflow

``` r
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

- **Data transformation** (`R/data_transformations.R`): Functions that
  change data meaning/structure (e.g., `make_rowind_first_occurrence`)
- **Utility functions** (`R/helper_functions.R`): Basic data processing
  helpers (e.g., `make_lowercase_names`, date parsing)
- **Core functions** (`R/skeleton_functions.R`): Main workflow functions

### Dual formatting systems approach

When creating packages that may serve different audiences (local vs
international), consider implementing dual formatting systems:

- **Separate by use case**: Group functions by intended audience rather
  than technical similarity
- **Consistent API patterns**: Use parallel naming (`format_*_as_local`
  vs `format_*_as_international`)
- **Internal helper functions**: Create shared internal functions with
  `@noRd` to avoid namespace clutter
- **Comprehensive testing**: Include comparison tests that verify
  differences between formatting systems
- **Clear documentation**: Use vignettes to demonstrate differences with
  side-by-side examples

**Example application to swereg**: If implementing output formatting for
Swedish research (domestic) vs international publication, separate
functions would improve discoverability and prevent formatting errors in
different contexts.

### pkgdown structure principles

``` yaml
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

Use sentence case for all vignette titles and subtitles: - ✅ “Variable
types: rowdep vs rowind” - ❌ “Variable Types: Rowdep vs Rowind” - ✅
“Understanding the concept” - ❌ “Understanding The Concept”

### Function documentation requirements

- Use `@family` tags for logical grouping
- Include `@seealso` references to related functions
- Provide runnable examples using package fake data
- Document side effects explicitly for functions that modify by
  reference

## Common issues and solutions

1.  **Column name errors**: Always use
    [`make_lowercase_names()`](https://papadopoulos-lab.github.io/swereg/reference/make_lowercase_names.md)
    after reading data
2.  **Missing cstime functions**: Install cstime package for Swedish
    time standards
3.  **Large dataset memory**: Use data.table operations and avoid
    copying large datasets
4.  **Date format issues**: Ensure dates are in Date class, not
    character
5.  **ID mismatches**: Check that ID columns match between skeleton and
    data after name transformation
6.  **CRAN submission failures**: Review checklist above and run
    `R CMD check . --as-cran`
7.  **Vignette build errors**: Ensure all referenced columns exist in
    fake data examples
