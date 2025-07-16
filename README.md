# swereg

**swereg** is an R package for manipulating and analyzing healthcare registry data in epidemiological research. It provides a systematic three-stage framework for creating longitudinal data skeletons and integrating multiple health registries.

## Registry integration

swereg is designed for healthcare registries with realistic synthetic data included:

- **Hospital registers**: Diagnosis codes (ICD-10) and surgical procedures
- **Prescription registers**: Medication data with ATC codes and treatment duration  
- **Death registers**: Underlying and multiple causes with proper variable names
- **Administrative data**: Demographics and socioeconomic data
- **Synthetic datasets**: `fake_demographics`, `fake_inpatient_diagnoses`, `fake_prescriptions`, `fake_cod`, etc.

## The skeleton approach

swereg uses a **skeleton concept**: you build strong 'bones' (time structure) then attach 'muscles' (data) systematically through three stages:

1. **skeleton1_create** - Raw data integration 
2. **skeleton2_clean** - Data cleaning and derived variables
3. **skeleton3_analyze** - Analysis-ready dataset preparation

## Features

- **Three-stage workflow**: Systematic skeleton1→skeleton2→skeleton3 progression
- **Time structure**: ISO year-weeks with automatic Sunday date calculation
- **Healthcare integration**: Hospital diagnosis and surgical procedures with automatic pattern matching
- **Prescription analysis**: Prescription data with treatment duration and ATC code patterns  
- **Mortality data**: Death registry with underlying and multiple causes
- **Synthetic data**: Realistic synthetic registry data for development and testing
- **Scalable processing**: Memory-efficient batching for large populations
- **Reproducible workflow**: Standardized methodology for registry-based epidemiological research

## Core functions

### Data structure
- `create_skeleton()` - Create longitudinal data skeleton with individual IDs and time periods
- `make_lowercase_names()` - Standardize column names across datasets

### Data integration
- `add_onetime()` - Merge baseline/demographic data
- `add_annual()` - Add annual data for specific years
- `add_diagnoses()` - Integrate healthcare diagnosis data (ICD-10)
- `add_operations()` - Add surgical procedure data
- `add_rx()` - Include prescription drug data with treatment periods
- `add_cods()` - Merge cause of death information

### Specialized functions
- `x2023_mht_add_lmed()` - Process menopausal hormone therapy prescription data

## Installation

You can install the development version of **swereg** from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install swereg
devtools::install_github("papadopoulos-lab/swereg")
```

## Getting started

Follow the vignettes in order to learn the skeleton approach:

### 1. Understand the concept
```r
vignette("skeleton-concept")
```
Learn the foundational skeleton approach and why it works for registry data analysis.

### 2. Build your skeleton (skeleton1_create)
```r
vignette("skeleton1-create")
```
Create the time structure and integrate raw registry data:

```r
library(data.table)

# Load synthetic registry data (included with package)
data("fake_person_ids", package = "swereg")
data("fake_demographics", package = "swereg")
data("fake_inpatient_diagnoses", package = "swereg")

# Step 1: Create the skeleton (good bones)
skeleton <- swereg::create_skeleton(
  ids = fake_person_ids[1:100],
  date_min = "2020-01-01",
  date_max = "2022-12-31"
)

# Step 2: Apply standardization (required for all registry data)
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
swereg::add_diagnoses(skeleton, diagnoses_subset, "lopnr", diags = diagnosis_patterns)

# Result: skeleton1_create ready for skeleton2_clean stage
head(skeleton)
```

### 3. Clean your data (skeleton2_clean)
```r
vignette("skeleton2-clean")
```
Clean variables and create derived clinical indicators using only data within the skeleton.

### 4. Prepare for analysis (skeleton3_analyze)
```r
vignette("skeleton3-analyze")
```
Learn memory-efficient processing and create final analysis datasets for production-scale studies.

## Key principles

1. **Sequential stages**: Each stage builds on the previous one
2. **Time structure**: ISO year-weeks provide precise temporal alignment
3. **Pattern matching**: Automatic prefix handling for medical codes
4. **Memory efficiency**: Batching strategies for large datasets
5. **Self-contained cleaning**: skeleton2_clean uses only skeleton data

## Documentation

Complete documentation and tutorials available at: https://papadopoulos-lab.github.io/swereg/

## Citation

If you use **swereg** in your research, please cite:

```
[Citation information to be added]
```