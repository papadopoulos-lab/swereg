# swereg

**swereg** is an R package developed for manipulating and analyzing Swedish registry data in epidemiological research. It provides a comprehensive framework for creating longitudinal data skeletons and integrating multiple Swedish health registries including specialist healthcare, prescriptions, causes of death, and surgical procedures.

## Features

- 🏗️ **Longitudinal Skeleton Creation**: Build time-structured data frameworks with ISO years and weeks
- 🏥 **Specialist Healthcare Integration**: Add diagnosis and surgical procedure data with ICD-10 and operation codes
- 💊 **Prescription Data Analysis**: Integrate LMED prescription data with ATC codes and treatment duration
- ⚰️ **Mortality Data**: Include cause of death information from Swedish death registries
- 📦 **data.table Optimized**: Designed for efficient processing of large Swedish registry datasets

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

## Quick Start

```r
library(swereg)
library(data.table)

# Create a longitudinal skeleton
skeleton <- create_skeleton(
  ids = c("123456", "789012", "345678"),
  date_min = as.Date("2020-01-01"),
  date_max = as.Date("2022-12-31")
)

# Add baseline demographic data
demographics <- data.table(
  lopnr = c("123456", "789012", "345678"),
  birth_year = c(1990, 1985, 1995),
  sex = c("F", "M", "F")
)
add_onetime(skeleton, demographics, "lopnr")

# Add diagnosis data
diagnosis_codes <- list(
  "diabetes" = c("^E10", "^E11"),
  "hypertension" = c("^I10")
)
add_diagnoses(skeleton, hospital_data, "lopnr", "both", diagnosis_codes)
```

## Swedish Registry Integration

This package is specifically designed for Swedish healthcare registries:

- **NPR (National Patient Register)**: Specialist healthcare (inpatient and outpatient care)
- **LMED (Prescribed Drug Register)**: Prescription medications  
- **Cause of Death Register**: Mortality and underlying causes
- **Population registers**: Demographics and baseline characteristics

## Citation

If you use **swereg** in your research, please cite:

```
[Citation information to be added]
```
