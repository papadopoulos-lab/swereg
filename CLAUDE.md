# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Communication Style
Do not oversell capabilities (e.g., static parsing of R files). When the user pushes back on a claim or design critique, seriously re-evaluate rather than defending the initial position. Treat pushback as signal that the first answer was likely wrong.

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

### Two-step workflow pattern
swereg follows a two-step approach:

1. **Create the skeleton**: Build the time grid, integrate raw registry data, and derive analysis-ready variables
2. **Analyse the skeleton**: Collapse to the right granularity (e.g., weekly → yearly) and run analyses

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
- **Literal prefixes**: `add_*` matches via `startsWith()` on literal prefixes (no regex). Write `"F640"` for any code beginning with F640. Do NOT prefix with `^` -- that is a regex anchor and will never match under `startsWith()`. (Since 26.5.9, the pre-call syntax check warns about this.)
- **Bracket expansion**: Bracket / character-class / range patterns are accepted directly and expanded automatically: `"I2[0-5]"` -> `c("I20","I21","I22","I23","I24","I25")`; `"FN[ABCDEGW][0-9][0-9]"`; `"!302[A-Z]"`. Multiple bracket groups in one pattern produce the Cartesian product.
- **Exclusions**: Use `!` prefix to exclude (e.g., `"!F640"`).
- **Multiple patterns**: Combine in vectors (e.g., `c("F640", "F648", "F649")`).
- **Historical codes**: ICD-9 uses `[A-Z]` suffixes, ICD-8 uses comma delimiters.

## Data Variable Types and Transformations

### Understanding rd_ (row-dependent) vs ri_ (row-independent) variables

In longitudinal registry data analysis with swereg, every derived variable
is classified into two fundamental shapes, distinguished by a short name
prefix:

- **`rd_`** (row-dependent): Variables that can change over time for a person
- **`ri_`** (row-independent): Variables that are fixed per person

This distinction is crucial for effective analysis: many phase-3 randvars
steps convert `rd_` variables into `ri_` variables by capturing a value at
a specific moment (e.g., age at first diagnosis).

### Examples of Variable Types

**Row-dependent (`rd_*`) Variables:**
- `rd_education`: Education level (can improve over time)
- `rd_income_inflation_adjusted`: Annual income (changes yearly)
- `rd_age_continuous`: Continuous age (increases each week)
- `rd_civil_status`: Can change on marriage, divorce, bereavement
- `f64_diag`: Had diagnosis this week (TRUE/FALSE by time period, produced
  by a phase-2 code registration)

**Row-independent (`ri_*`) Variables:**
- `ri_age_first_dx`: Age at first diagnosis (fixed once occurred)
- `ri_isoyear_first_dx`: Year of first diagnosis (historical fact)
- `ri_birthcountry`: Birth country (never changes)
- `ri_register_tag`: Person's role in study (case, control, etc.)
- `ri_age_death`: Age at death (fixed once occurred)

### Helper Function: make_rowind_first_occurrence()

The `make_rowind_first_occurrence()` function simplifies the common
pattern of creating `ri_*` variables from the first occurrence of a
condition:

```r
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

**Function features:**
- Automatically handles temp variable creation and cleanup
- Uses `first_non_na()` for robust aggregation across all variable types
- Includes comprehensive input validation and clear error messages
- Works with any condition that can be evaluated in data.table syntax

### Common rd_ → ri_ transformation patterns

```r
# Manual pattern (traditional approach)
skeleton[condition_is_true, temp := value_to_capture]
skeleton[, new_ri_var := first_non_na(temp), by = .(id)]
skeleton[, temp := NULL]

# Helper function pattern (recommended)
make_rowind_first_occurrence(skeleton, "condition_is_true", "value_to_capture", "new_ri_var")
```

### Integration with swereg workflow

The `rd_`/`ri_` concept maps onto the `RegistryStudy$process_skeletons()`
pipeline. That pipeline runs four numbered phases, and phase 1b writes no
`rd_` or `ri_` column:

1. **Phase 1 -- framework**: produces the base time grid and
   structural censoring. Usually includes `rd_age_continuous`.
2. **Phase 1b -- trim**: the one declared place that may delete skeleton
   rows. It runs on a fresh base only.
3. **Phase 2 -- codes**: produces code-derived columns (`os_*`,
   `osd_*`, `rx_*`, `op_*`) that are de-facto row-dependent.
4. **Phase 3 -- randvars**: the heavy lifting for `rd_` -> `ri_`
   transformations. Typical randvars steps add both time-varying
   LISA demographics (`rd_education`, `rd_income_inflation_adjusted`,
   `rd_civil_status`) and row-independent first-occurrence variables
   (`ri_age_first_dx`, `ri_isoyear_first_dx`).

`.PHASE_ORDER` holds three names and `Skeleton$trim_fn_hash` pins the trim
separately, so the order carries three entries while the pipeline runs four
phases. See "Refactoring: what breaks silently".

### Naming conventions

- Use `rd_*` prefix for time-varying variables
- Use `ri_*` prefix for time-invariant variables
- Be descriptive: `ri_age_first_e11` not `ri_age`
- Include context: `ri_isoyear_first_diagnosis` not `ri_year`

### Best Practices

**Always validate `ri_*` variables:**
```r
# Check that ri_* variables are actually row-independent
skeleton[, .(unique_values = uniqueN(ri_age_first_e11)), by = .(id)]
# Should return 1 for all persons (all rows have same value)
```

**For detailed examples and patterns**, see the variable types vignette:
```r
vignette("rowdep-rowind-concept", package = "swereg")
```

## TTE (Target Trial Emulation) system

### Spec-driven configuration
`tteplan_read_spec()` parses a YAML study specification. Required top-level sections: `study`, `enrollments`, `outcomes`, `follow_up`. Optional: `inclusion_criteria`, `exclusion_criteria`, `confounders`, `open_questions`.

Key spec structure:
- **study**: title, PI, `description`, `implementation.project_prefix`
- **inclusion_criteria.isoyears**: `[start, end]` — global ISO year range filter
- **exclusion_criteria**: list with `name`, `window` (numeric weeks or `"lifetime"`), `implementation.variable`
- **enrollments**: each has `id`, `additional_inclusion` (e.g. age range), `additional_exclusion`, `treatment` with `comparator_to_intervention_ratio` and `implementation` (variable, intervention/comparator values, seed)

### Key TTE functions, one per file

- `tteplan_read_spec(path)` in `R/tteplan_read_spec.R`: parses and validates the YAML, converts windows to weeks
- `tteplan_apply_exclusions(skeleton, spec, enrollment_spec)` in `R/tteplan_apply_exclusions.R`: applies the isoyear filter from `spec$inclusion_criteria$isoyears`, then additional_inclusion, global exclusion_criteria, and additional_exclusion
- `tteplan_apply_derived_confounders(skeleton, spec)` in `R/tteplan_apply_derived_confounders.R`: computes rolling-window indicators for `computed: true` confounders
- `tteplan_validate_spec(spec, skeleton)` in `R/tteplan_validate_spec.R`: checks that every spec variable exists in the skeleton columns
- `tteplan_from_spec_and_registrystudy(spec, study)` in `R/tteplan_from_spec.R`: creates a TTEPlan with the full ETT grid

### R6 classes

Each class is defined in one file and extended with `Class$set()` in siblings.

- **TTEDesign** (`R/r6_ttedesign.R`): holds `confounder_vars`, `time_treatment_var`, `eligible_var`
- **TTEEnrollment** (`R/r6_tteenrollment.R`): data plus design, lifecycle stages `pre_enrollment` -> `enrolled` -> `analysis_ready`. Four public methods carry step-number prefixes: `$s1_impute_confounders()`, `$s2_ipw()` and `$s3_truncate_weights()` in `R/r6_tteenrollment_weighting.R`, then `$s4_prepare_for_analysis()` in `R/r6_tteenrollment.R`
- **TTEPlan** (`R/r6_tteplan.R`): ETT grid. The three pipeline methods `$s1_generate_enrollments_and_ipw()`, `$s2_generate_analysis_files_and_ipcw_pp()` and `$s3_analyze()` live in `R/r6_tteplan_pipeline.R`

## Production workflow pattern

### RegistryStudy pipeline
For production-scale pipelines, use the `RegistryStudy` R6 class which handles
rawbatch creation, skeleton processing (framework, trim, codes, randvars),
and incremental rebuilds. See `vignette("skeleton-pipeline")` for details.

### Production example scripts
The `example/` directory contains production-style workflow implementations:
- `example/R_generic_v002/` - Helper functions for batched processing
- `example/Run_generic_v002.R` - Main production workflow script
- Individual project scripts showing real-world usage patterns

**Key principles:**
- **Batch processing**: Split individuals into groups via `RegistryStudy$set_ids()`
- **Incremental rebuilds**: Only changed pipeline steps re-run
- **Memory management**: Rawbatch files keep large datasets out of RAM during skeleton processing

## Key dependencies

**Core**: data.table, cstime, fs, stringr, dplyr, lubridate, haven  
**Development**: devtools, usethis, testthat, ggplot2
**Note**: Package now uses base pipe `|>` instead of magrittr `%>%` (requires R ≥ 4.1)

## Version control and release management

### REQUIRED: version updates
Whenever code is updated, **BOTH** of the following must be done:

**A) Update the version in `DESCRIPTION`. The scheme is `YY.M.D`, a two-digit-year CalVer that
runs AHEAD of the calendar.**

**Read this before you bump. Never derive a version from today's date.** swereg started on
`YY.M.D`: `26.6.22` was minted on 2026-06-22. It drifted ahead, because a second release on one
day bumps past the published number rather than reuse it. `26.8.7` through `26.8.11` were all
minted on 2026-08-03. Today's date is therefore BEHIND the version, so a date-derived bump is a
downgrade.

To bump:

1. Read the current `Version:` line from `DESCRIPTION`. Never hardcode a version in `CLAUDE.md`
   or in a brief.
2. Increment the third component by one. The month MAY roll early instead, and it has:
   `26.8.21` became `26.9.0` on 2026-08-15, and `26.9.2` became `26.10.0` the same day.
3. Verify with `package_version()` that the new version is greater than the old one. That is the
   one hard requirement. A check by eye does not meet it.

A version that decreases is a silent downgrade, and R then refuses to install it.

```r
old <- package_version(read.dcf("DESCRIPTION")[1, "Version"])
new <- package_version("<the version you are about to write>")
new > old   # MUST be TRUE
```

The sibling packages in the `cs*` family use `YYYY.M.D`, with a four-digit year. **Do not carry
their scheme here, and do not carry this one there.** Mixing them is a silent downgrade:
`package_version("26.8.6") > package_version("2026.8.3")` is FALSE.

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

The package vignettes follow a progressive learning structure:

### Concept
- **Skeleton concept**: `vignette("skeleton-concept")` - Why the person-week time grid
- **R6 class overview**: `vignette("r6-class-overview")` - Overview of R6 classes
- **Variable types**: `vignette("rowdep-rowind-concept")` - rd_ vs ri_ variable conventions

### Manual workflow (2 steps)
- **Creating the skeleton**: `vignette("skeleton-create")` - Build the time grid, integrate data, derive variables
- **Analysing the skeleton**: `vignette("skeleton-analyze")` - Collapse to the right granularity and run analyses

### Pipeline (production)
- **Skeleton pipeline**: `vignette("skeleton-pipeline")` - R6-based RegistryStudy with incremental rebuilds
- **TTE workflow**: `vignette("tte-workflow")` - Target trial emulation workflow

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

- **Data transformation** (`R/data_transformations.R`): functions that change data meaning or structure, such as `make_rowind_first_occurrence`
- **Column helpers** (`R/helper_functions.R`): the `first_non_na` and `min_with_infinite_as_na` family
- **One file per exported workflow function**: `R/create_skeleton.R`, `R/add_onetime.R`, `R/add_annual.R`, `R/add_diagnoses_and_operations.R`, `R/add_rx.R`, `R/make_lowercase_names.R`, `R/parse_swedish_date.R`

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

## Refactoring: what breaks silently

A refactor can break each invariant below with no error and no warning. Read the named file
before you change it. Some of these carry a pinning test and some do not. Read the subsection
rather than assume. Invariants 1, 2 and 4 name their tests. Invariant 7 identifies which parts
of the generated text carry no test.

Every item names a file and a symbol, checked against the tree on 2026-08-21.

### 1. Hash a function only after `utils::removeSource()`

`.hash_function()` in `R/code_identity.R` runs `fn <- utils::removeSource(fn)` before it calls
`digest::digest()`. `body(fn)` carries a srcref when R parses the function with
`keep.source = TRUE`, and carries none under `Rscript`. So one unchanged function gives two
digests across the two sessions, and every batch rebuilds.

The `R-CMD-check` job of the shared pptemplate workflow sets `R_KEEP_PKG_SOURCE: yes`. So CI
matches an interactive session, and both differ from a plain `Rscript` run.

`tests/testthat/test-registrystudy.R` pins it, under the label
`a function's hash does not depend on keep.source`.

### 2. `.fingerprint_entry()` reads `fn` with `[[`, never `$`

`.fingerprint_entry()` in `R/code_identity.R` reads `reg[["fn"]]`. `$` does partial name
matching on a list. A derived code entry carries `fn_args` and no `fn`, so `reg$fn` resolves to
`fn_args`, and the fingerprint hashes the wrong object without stopping. Any dynamic field
access in the fingerprint path MUST use `[[`.

`tests/testthat/test-code_identity.R` pins the guard, under the label
`.fingerprint_entry stops on an entry with fn_args but no fn`. The guard gained that test in
26.10.3.

### 3. `.PHASE_ORDER` holds three names on purpose

`.PHASE_ORDER` in `R/code_identity.R` is `c("framework", "codes", "randvars")`. The trim is
phase 1b, and `Skeleton$trim_fn_hash` pins it separately, so it stays out of the order. The
pipeline runs four numbered phases: 1 framework, 1b trim, 2 codes, 3 randvars.

A skeleton written before the `phase_order` field existed reads `NULL`. It MUST NOT compare
equal to the current order. The answer there is a full rebuild, never a replay.

### 4. The trim runs inside the rebuild block deliberately

`.process_one_batch()` in `R/pipeline_batch_workers.R` calls `study$trim_fn()` inside the
rebuild block. The gate above that block rebuilds whenever the trim identity changes, so the
trim only ever sees a fresh base.

Move that call outside the block and an unrelated code-registry edit re-runs the trim on data it
already trimmed. A trim that is not a pure predicate filter then deletes more rows on every such
edit, with no error.

`tests/testthat/test-trim-phase.R` pins the placement, under the label
`an unrelated edit does not re-run the trim on trimmed data`.

### 5. Five schema constants, all independent

| Constant | Value | File |
|---|---|---|
| `.REGISTRY_STUDY_SCHEMA_VERSION` | `6L` | `R/r6_registrystudy.R` |
| `.SKELETON_SCHEMA_VERSION` | `1L` | `R/r6_skeleton.R` |
| `.TTE_DESIGN_SCHEMA_VERSION` | `3L` | `R/r6_tteenrollment.R` |
| `.TTE_ENROLLMENT_SCHEMA_VERSION` | `3L` | `R/r6_tteenrollment.R` |
| `.TTE_PLAN_SCHEMA_VERSION` | `3L` | `R/r6_tteplan.R` |

Bump only the constant whose stored shape changed. The study version and the skeleton version
never merge. Three constants share the value `3L` by coincidence, so do not move them together.

### 6. `add_rx()` reads the skeleton's row set

`add_rx()` in `R/add_rx.R` derives `weekly_isoyearweek` from the skeleton it is handed. It then
marks only the `(id, isoyearweek)` pairs that `skel_pts` already holds. Its output therefore
depends on which rows exist when it runs, and row deletion has one declared home for that
reason. Check any change to phase ordering against `add_rx()`, and not against the hashes alone.

### 7. Generated manuscript prose carries methods claims no test reads

`.plan_print_target_checklist()` in `R/tteplan_reporting.R` generates the TARGET checklist
methods text. `.build_consort_dot()` in `R/consort.R` generates the CONSORT diagram node labels.
Both reach a paper.

**Review generated prose by generating it.** Build a small plan. Print the checklist and the
labels. Read the sentences. The `paste0()` that builds them does not show you the sentence a
reader gets. A wrong sentence there is a false methods claim, produced automatically.

Three test files pin parts of the generated text:

- `tests/testthat/test-comparator-draw-naming.R` pins the assignment sentences of items 6c and 7c.
- `tests/testthat/test-tte_spec.R` pins the Item 8 attrition counts.
- `tests/testthat/test-cohort_flow.R` pins the CONSORT box labels.

Nothing pins the Causal contrasts sentence, and that sentence is wrong today.
`R/tteplan_reporting.R` prints the fixed string
`"Intention-to-treat and as-treated analyses were not conducted. "` for every plan, which is
false for an ITT plan. `grep -r "were not conducted" tests/` returns nothing. The follow-up is
papadopoulos-lab/swereg#24.

A design word is a claim about the whole pipeline. Matching, stratification, weighting and
censoring each oblige something downstream. Check the obligation rather than re-read the step
that names it.

### 8. Three test-suite shapes to watch while you move code

**A test that reads the source tree skips under `R CMD check`.** Tests run from a built tarball
there, so the source tree is absent and the `skip_if()` fires. The file is then red locally and
green in CI. Two current instances:

- `tests/testthat/test-comparator-ratio-rename-complete.R`, through `.crr_root()`. It asserts
  that the retired comparator-ratio key survives in exactly four files, and that every `R/`
  occurrence sits inside `tteplan_read_spec()` in `R/tteplan_read_spec.R`. Move that function to
  another file and the test breaks. That test file holds the literal key. Do not copy the key
  into `CLAUDE.md`: this file is not one of the four, and the test then fails.
- `tests/testthat/test-batch_lockdown.R`, through
  `skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")`.

**Never match a short digit pattern against printed output.** `$print_spec_summary()` prints a
clock in `%H:%M:%S`. A clock holds `3:1` when the hour ends in 3 and the minute starts with 1,
as in `13:15:00`. It also holds `3:1` when the minute ends in 3 and the second starts with 1, as
in `00:23:10`. `1:3` follows the same two rules with the digits swapped. An enumeration of all
86400 clock values of a day gives 3210 matches in each direction, or 3.72%.

`tests/testthat/test-comparator-ratio-direction.R` matched a ratio with `fixed = TRUE` and was
flaky at that rate. The positive assertion failed worse: a clock alone satisfied it, so a
reversed ratio passed on any of those 3210 values. Commit `1199f2d` fixed it with the
digit-boundary matcher `.crd_ratio_hits()`.

**A green local suite is not a green CI run.** CI runs the package from the installed tarball,
and `pkgload::load_all()` runs it from source. Issue #11 records that shape.

swereg's own git history records one instance. Commit `12579f41` (2026-07-18) reports that
`R CMD check` on CI failed. Every dispatched item resolved to "a DIFFERENT code version". The
parent ran the installed package with no srcref, and the worker ran `load_all()` with one. That
commit message states that local tests missed it, because both ends called `load_all()`
symmetrically. It is the same srcref defect invariant 1 covers.

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