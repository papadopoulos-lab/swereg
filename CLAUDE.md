# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**swereg** is an R package for manipulating and analyzing Swedish healthcare registry data in epidemiological research. It creates longitudinal data skeletons with ISO year-week structure and integrates multiple Swedish health registries (NPR, LMED, Cause of Death). The package is optimized for gender dysphoria research but works for general registry analysis.

## Development Commands

### Core Development Workflow
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

### Testing and Development Scripts
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

## Architecture and Data Flow

### Three-Stage Workflow Pattern
swereg follows a systematic three-stage approach ("Good Bones, Then Muscles"):

1. **skeleton1_create**: Raw data integration - Create time skeleton and merge registry data
2. **skeleton2_clean**: Data cleaning and derived variables - Clean and process within skeleton
3. **skeleton3_analyze**: Analysis-ready dataset preparation - Final analysis preparation

### Core Pattern: Longitudinal Skeleton + Sequential Data Integration
1. **Skeleton Creation**: `create_skeleton()` builds time-structured framework with individual IDs and ISO weeks
2. **Sequential Data Addition**: Use specialized functions to add different data types
3. **data.table Optimization**: All operations use data.table for efficient processing of large datasets

### Key Integration Functions (in typical order of use)
- `add_onetime()` - Baseline/demographic data (one record per person)
- `add_annual()` - Annual data for specific years (e.g., socioeconomic status)
- `add_diagnoses()` - NPR diagnosis data with ICD-10 codes (hospital visits)
- `add_operations()` - Surgical procedure codes from NPR
- `add_rx()` - LMED prescription data with ATC codes and treatment periods
- `add_cods()` - Cause of death information

### Swedish Registry Context
- **NPR** (National Patient Register): Specialist healthcare (inpatient `sv` and outpatient `ov`)
- **LMED** (Prescribed Drug Register): Prescription medications with ATC codes
- **SCB** (Statistics Sweden): Demographics and socioeconomic data
- **Cause of Death Register**: Mortality data with ICD-10 codes
- Uses `cstime` package for Swedish ISO time standards

## Critical Data Processing Rules

### Always Apply make_lowercase_names()
**ESSENTIAL**: All imported data must be processed with `swereg::make_lowercase_names()` before use:

```r
# Read data
data <- fread("file.txt")  # or haven::read_sas() etc.

# REQUIRED: Apply lowercase transformation
swereg::make_lowercase_names(data)

# Now safe to use with swereg functions
swereg::add_diagnoses(skeleton, data, id_name = "lopnr", ...)
```

This transforms column names like `LopNr` → `lopnr`, `ATC` → `atc`, `INDATUM` → `indatum`.

### Expected Column Names After make_lowercase_names()
- **Person IDs**: `lopnr` (SCB), `lopnr` (NPR after transformation), `p444_lopnr_personnr` (LMED)
- **Dates**: `indatum` (admission), `utdatum` (discharge), `edatum` (prescription), `dodsdat` (death)
- **Diagnosis codes**: `hdia` (main), `dia1`, `dia2`, etc. (secondary), `ekod1`, etc. (external causes)
- **Operation codes**: `op1`, `op2`, etc.
- **Prescription codes**: `atc` (drug code), `fddd` (treatment duration)

## Package Data for Development

The package includes synthetic Swedish registry data for development and examples:
- `fake_demographics` - SCB demographics (`lopnr`, `fodelseman`, `DodDatum`)
- `fake_annual_family` - SCB annual family data (`lopnr`, `FamTyp`)
- `fake_inpatient_diagnoses` - NPR inpatient data (full 43-column structure)
- `fake_outpatient_diagnoses` - NPR outpatient data
- `fake_prescriptions` - LMED prescription data (37 columns)
- `fake_cod` - Cause of death registry data
- `fake_person_ids` - Reference list of person identifiers

Load with: `data("fake_demographics")` etc.

**Note**: These are synthetic datasets designed to replicate the structure and characteristics of real Swedish registry data while maintaining confidentiality and privacy.

## Code Patterns

### Typical Analysis Workflow
```r
# 1. Create skeleton
skeleton <- create_skeleton(ids, "2001-01-01", "2020-12-31")

# 2. Add baseline data
demographics <- fread("demographics.csv")
swereg::make_lowercase_names(demographics)
add_onetime(skeleton, demographics, "lopnr")

# 3. Add longitudinal data
hospital_data <- haven::read_sas("hospital.sas7bdat")
swereg::make_lowercase_names(hospital_data)
add_diagnoses(skeleton, hospital_data, "lopnr", diags = list(
  "depression" = c("^F32", "^F33"),
  "anxiety" = c("^F40", "^F41")
))

# 4. Add prescriptions
prescriptions <- fread("prescriptions.txt")
swereg::make_lowercase_names(prescriptions)
add_rx(skeleton, prescriptions, "lopnr", drugs = list(
  "antidepressants" = c("^N06A")
))
```

### Pattern Matching for Medical Codes
- **Regex patterns**: Use `^` prefix for exact starts (e.g., `"^F640"`)
- **Exclusions**: Use `!` prefix to exclude (e.g., `"!F640"`)
- **Multiple patterns**: Combine in lists (e.g., `c("^F640", "^F648", "^F649")`)
- **Historical codes**: ICD-9 uses `[A-Z]` suffixes, ICD-8 uses comma delimiters

## Production Workflow Pattern

### Memory-Efficient Batched Processing
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

### Production Example Scripts
The `example/` directory contains production-style workflow implementations:
- `example/R_generic_v002/` - Helper functions for batched processing
- `example/Run_generic_v002.R` - Main production workflow script
- Individual project scripts showing real-world usage patterns

**Key principles:**
- **Batch processing**: Split individuals into groups (e.g., 50-100 per batch)
- **Memory management**: Remove large datasets after skeleton1_create phase
- **Self-contained cleaning**: skeleton2_clean uses only data within skeleton

## Key Dependencies

**Core**: data.table, cstime, fs, stringr, dplyr, lubridate, haven  
**Development**: devtools, usethis, testthat, ggplot2
**Note**: Package now uses base pipe `|>` instead of magrittr `%>%` (requires R ≥ 4.1)

## Version Control and Release Management

### REQUIRED: Version Updates
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

### Version Format Rules
- Use `YY.M.D` format (e.g., `25.1.5` not `25.01.05`)
- Remove all leading zeroes from month and day
- Update both DESCRIPTION and NEWS.md simultaneously
- Document all user-facing changes in NEWS.md

### Git Configuration Requirements
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

### Git Commit Guidelines
- **NEVER mention Claude/AI** in commit messages
- Use standard commit message format: "Add feature X", "Fix bug in Y", "Update documentation"
- Focus on what was changed, not who/what made the change
- Example: "Add fake Swedish registry datasets" (not "Claude added fake datasets")
- All commits must be signed (configured above)

## GitHub Actions and Documentation

### Automated pkgdown Documentation
The repository includes GitHub Actions that automatically:
- Build pkgdown documentation on every push to main/master
- Deploy to GitHub Pages at: `https://papadopoulos-lab.github.io/swereg/`
- Include all vignettes, function documentation, and news updates

### Setup Requirements (One-time)
1. **Enable GitHub Pages**: Go to Settings > Pages > Source: "GitHub Actions"
2. **Permissions**: Ensure Actions have write permissions (Settings > Actions > General)
3. **Branch Protection**: Configure main/master branch as default

### Local pkgdown Development
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

## Vignettes Structure

The package includes three vignettes following a progressive learning structure:

- **Basic Workflow**: `vignette("basic-workflow")` - Introduction to skeleton1_create stage
- **Complete Workflow**: `vignette("complete-workflow")` - Two-stage pipeline (skeleton1_create + skeleton2_clean)
- **Memory-Efficient Batching**: `vignette("memory-efficient-batching")` - Complete three-stage pipeline with production-scale batching

### Vignette Organization
- **Getting Started**: basic-workflow (focuses on skeleton1_create only)
- **Advanced Usage**: complete-workflow and memory-efficient-batching (full workflows)
- **Clear progression**: Each vignette builds on the previous without overlap

## Function Documentation Improvements

All exported functions now include:
- **@family tags**: Functions grouped by purpose (data_integration, skeleton_creation, data_preprocessing)
- **@seealso sections**: Cross-references to related functions and vignettes
- **Runnable examples**: All examples use synthetic data included in the package
- **Comprehensive parameter documentation**: Clear descriptions of expected inputs and outputs
- **Academic tone**: Professional, objective language appropriate for scientific software
- **Better return value descriptions**: Explicit documentation of side effects and modifications

## Common Issues and Solutions

1. **Column name errors**: Always use `make_lowercase_names()` after reading data
2. **Missing cstime functions**: Install cstime package for Swedish time standards
3. **Large dataset memory**: Use data.table operations and avoid copying large datasets
4. **Date format issues**: Ensure dates are in Date class, not character
5. **ID mismatches**: Check that ID columns match between skeleton and data after name transformation