# swereg

**swereg** is an R package developed for handling and analyzing Swedish register data in epidemiological research. It provides tools for data cleaning, linkage, and transformation, enabling researchers to efficiently work with complex population-based datasets.

## Features

- 🧹 **Data Cleaning**: Standardize and clean register inputs.
- 🔗 **Data Linkage**: Join datasets across different registers using unique identifiers.
- ⏱ **Time-to-Event Data Preparation**: Format data for survival and longitudinal analyses.
- 📦 **data.table Compatibility**: Designed to integrate well with the `data.table` ecosystem.

## Installation

You can install the development version of **swereg** from GitHub using [`devtools`](https://cran.r-project.org/package=devtools):

```
# Install devtools if not already installed
install.packages("devtools")

# Install swereg
devtools::install_github("papadopoulos-lab/swereg")
```
