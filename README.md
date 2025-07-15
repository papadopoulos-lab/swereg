# swereg

**swereg** is an R package for manipulating and analyzing Swedish healthcare registry data in epidemiological research. It provides a systematic three-stage framework for creating longitudinal data skeletons and integrating multiple Swedish health registries.

## The "Good Bones, Then Muscles" Approach

swereg uses a **skeleton concept**: you build strong 'bones' (time structure) then attach 'muscles' (data) systematically through three stages:

1. **skeleton1_create** - Raw data integration 
2. **skeleton2_clean** - Data cleaning and derived variables
3. **skeleton3_analyze** - Analysis-ready dataset preparation

## Features

- **Three-Stage Workflow**: Systematic skeleton1→skeleton2→skeleton3 progression
- **Time Structure**: ISO year-weeks with automatic Sunday date calculation
- **Healthcare Integration**: NPR diagnosis and surgical procedures with automatic pattern matching
- **Prescription Analysis**: LMED data with treatment duration and ATC code patterns  
- **Mortality Data**: Swedish death registry with underlying and multiple causes
- **Synthetic Data**: Realistic synthetic Swedish registry data for development and testing
- **Scalable Processing**: Memory-efficient batching for large populations
- **Reproducible Workflow**: Standardized methodology for registry-based epidemiological research

## Core Functions

### Data Structure
- `create_skeleton()` - Create longitudinal data skeleton with individual IDs and time periods
- `make_lowercase_names()` - Standardize column names across datasets

### Data Integration
- `add_onetime()` - Merge baseline/demographic data
- `add_annual()` - Add annual data for specific years
- `add_diagnoses()` - Integrate specialist healthcare diagnosis data (ICD-10)
- `add_operations()` - Add surgical procedure data
- `add_rx()` - Include prescription drug data with treatment periods
- `add_cods()` - Merge cause of death information

### Specialized Functions
- `x2023_mht_add_lmed()` - Process menopausal hormone therapy prescription data

## Installation

You can install the development version of **swereg** from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install swereg
devtools::install_github("papadopoulos-lab/swereg")
```

## Quick Start: skeleton1_create

```r
library(data.table)

# Load synthetic Swedish registry data (included with package)
data("fake_person_ids", package = "swereg")
data("fake_demographics", package = "swereg")
data("fake_inpatient_diagnoses", package = "swereg")

# Step 1: Create the skeleton (good bones)
skeleton <- swereg::create_skeleton(
  ids = fake_person_ids[1:100],
  date_min = "2020-01-01",
  date_max = "2022-12-31"
)

# Step 2: Apply standardization (required for all Swedish data)
swereg::make_lowercase_names(fake_demographics)
swereg::make_lowercase_names(fake_inpatient_diagnoses)

# Step 3: Attach demographic data (muscles)
demographics_subset <- fake_demographics[lopnr %in% fake_person_ids[1:100]]
swereg::add_onetime(skeleton, demographics_subset, id_name = "lopnr")

# Step 4: Add diagnosis patterns (^ prefix automatically added)
diagnosis_patterns <- list(
  "diabetes" = c("E10", "E11"),
  "depression" = c("F32", "F33")
)
diagnoses_subset <- fake_inpatient_diagnoses[lopnr %in% fake_person_ids[1:100]]
swereg::add_diagnoses(skeleton, diagnoses_subset, "lopnr", "both", diagnosis_patterns)

# Result: skeleton1_create ready for skeleton2_clean stage
head(skeleton)
```

## Swedish Registry Integration

swereg is specifically designed for Swedish healthcare registries with realistic fake data included:

- **NPR (National Patient Register)**: Diagnosis codes (ICD-10) and surgical procedures
- **LMED (Prescribed Drug Register)**: Prescription data with ATC codes and treatment duration  
- **Cause of Death Register**: Underlying and multiple causes (ulorsak/morsak variables)
- **SCB (Statistics Sweden)**: Demographics and socioeconomic data
- **Synthetic datasets**: `fake_demographics`, `fake_inpatient_diagnoses`, `fake_prescriptions`, `fake_cod`, etc.

## Vignettes

- **Basic Workflow**: `vignette("basic-workflow")` - Introduction to skeleton1_create
- **Complete Workflow**: `vignette("complete-workflow")` - Two-stage pipeline (skeleton1_create + skeleton2_clean)
- **Memory-Efficient Batching**: `vignette("memory-efficient-batching")` - Complete three-stage pipeline with production-scale batching

## Citation

If you use **swereg** in your research, please cite:

```
[Citation information to be added]
```
