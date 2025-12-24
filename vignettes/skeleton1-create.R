## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(data.table)

## -----------------------------------------------------------------------------
# Load example data
data("fake_person_ids", package = "swereg")

# Create skeleton covering 2015-2020
skeleton <- swereg::create_skeleton(
  ids = fake_person_ids,
  date_min = "2015-01-01",
  date_max = "2020-12-31"
)

# Examine the structure
head(skeleton)
cat("Skeleton dimensions:", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")

## -----------------------------------------------------------------------------
# Load and prepare demographic data
fake_demographics <- swereg::fake_demographics |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "fodelseman")

# Add to skeleton
swereg::add_onetime(skeleton, fake_demographics, id_name = "lopnr")

# Check what was added
new_vars <- setdiff(names(skeleton), c("id", "isoyear", "isoyearweek", "is_isoyear", "isoyearweeksun"))
cat("Added demographic variables:", paste(new_vars, collapse = ", "), "\n")

## -----------------------------------------------------------------------------
# Load annual family data
fake_annual_family <- swereg::fake_annual_family |>
  data.table::copy() |>
  swereg::make_lowercase_names()

# Add annual data for 2015
swereg::add_annual(skeleton, fake_annual_family, id_name = "lopnr", isoyear = 2015)

cat("Annual data added for 2015\n")

## -----------------------------------------------------------------------------
# Load and prepare diagnosis data
fake_diagnoses <- swereg::fake_diagnoses |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "indatum")
fake_diagnoses <- swereg::fake_diagnoses |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "indatum")

diagnoses_combined <- data.table::rbindlist(list(
  fake_diagnoses,
  fake_diagnoses
), use.names = TRUE, fill = TRUE)

# Define diagnosis patterns to search for (^ prefix automatically added)
diagnosis_patterns <- list(
  "depression" = c("F32", "F33"),
  "anxiety" = c("F40", "F41"), 
  "gender_dysphoria" = c("F64"),
  "cardiovascular" = c("I10", "I20", "I21")
)

# Add diagnoses to skeleton
swereg::add_diagnoses(
  skeleton,
  diagnoses_combined,
  id_name = "lopnr",
  diags = diagnosis_patterns
)

# Check results
diag_vars <- names(diagnosis_patterns)
for(var in diag_vars) {
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "positive cases\n")
}

## -----------------------------------------------------------------------------
# Load prescription data
fake_prescriptions <- swereg::fake_prescriptions |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "edatum")

# Define drug patterns (ATC codes, ^ prefix automatically added)
drug_patterns <- list(
  "antidepressants" = c("N06A"),
  "hormones" = c("G03"),
  "cardiovascular_drugs" = c("C07", "C08", "C09")
)

# Add prescriptions to skeleton
swereg::add_rx(
  skeleton,
  fake_prescriptions,
  id_name = "p444_lopnr_personnr",
  rxs = drug_patterns
)

# Check prescription usage
rx_vars <- names(drug_patterns)
for(var in rx_vars) {
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "prescription periods\n")
}

## -----------------------------------------------------------------------------
# Add operations (using default gender-affirming surgery codes)
swereg::add_operations(skeleton, fake_diagnoses, "lopnr")

# Check operation counts
operation_vars <- grep("^op_", names(skeleton), value = TRUE)
cat("Operation variables added:", length(operation_vars), "\n")
for(var in operation_vars[1:3]) {  # Show first 3
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "procedures\n")
}

## -----------------------------------------------------------------------------
# Load cause of death data
fake_cod <- swereg::fake_cod |>
  data.table::copy() |>
  swereg::make_lowercase_names(date_columns = "dodsdat")

# Define cause of death patterns (^ prefix automatically added)
cod_patterns <- list(
  "cardiovascular_death" = c("I21", "I22"),
  "external_causes" = c("X60", "X70")
)

# Add to skeleton
swereg::add_cods(
  skeleton,
  fake_cod,
  id_name = "lopnr",
  cods = cod_patterns
)

# Check mortality
cod_vars <- names(cod_patterns)
for(var in cod_vars) {
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "deaths\n")
}

## -----------------------------------------------------------------------------
cat("Final skeleton dimensions:", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")

# Example: Show data for one person
person_data <- skeleton[id == fake_person_ids[1] & isoyear == 2018 & !is.na(isoyearweek)]
cat("\nExample data for person", fake_person_ids[1], "in 2018 (first 6 weeks):\n")
print(head(person_data[, .(id, isoyearweek, depression, antidepressants, cardiovascular)]))

