## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(data.table)

## -----------------------------------------------------------------------------
# Quick skeleton setup for demonstration
data("fake_person_ids", package = "swereg")
data("fake_demographics", package = "swereg")
data("fake_inpatient_diagnoses", package = "swereg")

# Create basic skeleton
skeleton <- swereg::create_skeleton(fake_person_ids, "2015-01-01", "2020-12-31")

# Add minimal data for demonstration
fake_demographics_copy <- data.table::copy(fake_demographics)
swereg::make_lowercase_names(fake_demographics_copy, date_columns = "fodelseman") 
swereg::add_onetime(skeleton, fake_demographics_copy, id_name = "lopnr")

fake_diag_copy <- data.table::copy(fake_inpatient_diagnoses)
swereg::make_lowercase_names(fake_diag_copy, date_columns = "INDATUM")
swereg::add_diagnoses(skeleton, fake_diag_copy, id_name = "lopnr", 
                     diags = list(
                       "depression" = c("F32", "F33"),
                       "anxiety" = c("F40", "F41"), 
                       "gender_dysphoria" = c("F64"),
                       "psychosis" = c("F20", "F25")
                     ))

# Add prescriptions for treatment variables
data("fake_prescriptions", package = "swereg")
fake_prescriptions_copy <- data.table::copy(fake_prescriptions)
swereg::make_lowercase_names(fake_prescriptions_copy, date_columns = "EDATUM")
swereg::add_rx(skeleton, fake_prescriptions_copy, id_name = "p444_lopnr_personnr",
               rxs = list(
                 "antidepressants" = c("N06A"),
                 "antipsychotics" = c("N05A"),
                 "hormones" = c("G03")
               ))

# Add cause of death data 
data("fake_cod", package = "swereg")
fake_cod_copy <- data.table::copy(fake_cod)
swereg::make_lowercase_names(fake_cod_copy, date_columns = "dodsdat")
swereg::add_cods(skeleton, fake_cod_copy, id_name = "lopnr",
                 cods = list(
                   "external_death" = c("X60", "X70"),
                   "cardiovascular_death" = c("I21", "I22")
                 ))

cat("skeleton1_create completed:", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")

## -----------------------------------------------------------------------------
# Create age variable
skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
skeleton[, age := isoyear - birth_year]

cat("Age variable created\n")

## -----------------------------------------------------------------------------
# Create mental health composite variables
skeleton[, any_mental_health := depression | anxiety | psychosis]
skeleton[, severe_mental_illness := psychosis | gender_dysphoria]

# Check mental health prevalence
cat("Any mental health condition:", sum(skeleton$any_mental_health, na.rm = TRUE), "person-periods\n")
cat("Severe mental illness:", sum(skeleton$severe_mental_illness, na.rm = TRUE), "person-periods\n")

## -----------------------------------------------------------------------------
# Create medication concordance variables
skeleton[, depression_treated := depression & antidepressants]
skeleton[, psychosis_treated := psychosis & antipsychotics]

# Check treatment patterns
cat("Depression with treatment:", sum(skeleton$depression_treated, na.rm = TRUE), "periods\n")
cat("Psychosis with treatment:", sum(skeleton$psychosis_treated, na.rm = TRUE), "periods\n")

## -----------------------------------------------------------------------------
# Create life stage variables
skeleton[, life_stage := fcase(
  age < 18, "child",
  age >= 18 & age < 65, "adult", 
  age >= 65, "elderly",
  default = "unknown"
)]

# Check life stage distribution
cat("Life stage distribution:\n")
print(table(skeleton[is_isoyear == TRUE]$life_stage, useNA = "ifany"))

## -----------------------------------------------------------------------------
# Create outcome variables
skeleton[, death_any := external_death | cardiovascular_death]

cat("Any death:", sum(skeleton$death_any, na.rm = TRUE), "deaths\n")

## -----------------------------------------------------------------------------
# Filter to valid ages and reasonable time periods
cat("Before filtering:", nrow(skeleton), "rows\n")

skeleton <- skeleton[age >= 0 & age <= 100]
skeleton <- skeleton[isoyear >= 2015]  # Remove historical rows

cat("After filtering:", nrow(skeleton), "rows\n")

## -----------------------------------------------------------------------------
# Create registry tag variables (simulate case-control study)
skeleton[, register_tag := fcase(
  gender_dysphoria == TRUE, "case",
  id %% 3 == 0, "control_matched",
  default = "control_population"
)]

# Create shared case variables (for matched studies)
# Find first gender dysphoria diagnosis for cases
gd_first <- skeleton[gender_dysphoria == TRUE & register_tag == "case", 
                     .(first_gd_year = min(isoyear, na.rm = TRUE)), 
                     by = .(id)]

# Add to skeleton
skeleton[gd_first, on = "id", first_gd_year := first_gd_year]

# For controls, assign their matched case's first GD year (simplified)
skeleton[register_tag != "case", first_gd_year := 2016]  # Simplified for demo

cat("Study design variables created\n")
print(table(skeleton[is_isoyear == TRUE]$register_tag))

## -----------------------------------------------------------------------------
# Remove temporary variables
skeleton[, c("fodelseman", "birth_year") := NULL]

cat("skeleton2_clean completed:", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")

## -----------------------------------------------------------------------------
# Show structure
cat("Variables:", paste(names(skeleton), collapse = ", "), "\n")

# Example analysis: Depression prevalence by life stage (filter to years with data)
depression_summary <- skeleton[is_isoyear == TRUE & isoyear >= 2015, .(
  n_person_years = .N,
  depression_prev = mean(depression, na.rm = TRUE),
  treatment_rate = ifelse(sum(depression, na.rm = TRUE) > 0,
                         mean(depression_treated[depression == TRUE], na.rm = TRUE),
                         NA_real_)
), by = .(life_stage, register_tag)]

print(depression_summary[n_person_years > 0])  # Only show non-empty groups

## -----------------------------------------------------------------------------
# Example: Mental health treatment patterns (filter to valid data)
treatment_summary <- skeleton[any_mental_health == TRUE & is_isoyear == TRUE & !is.na(register_tag), .(
  antidepressant_use = mean(antidepressants, na.rm = TRUE),
  antipsychotic_use = mean(antipsychotics, na.rm = TRUE),
  hormone_use = mean(hormones, na.rm = TRUE),
  mean_age = mean(age, na.rm = TRUE),
  n_observations = .N
), by = register_tag]

print(treatment_summary[n_observations > 0])  # Only show groups with data

