## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(data.table)

## -----------------------------------------------------------------------------
# Load all fake data
data("fake_person_ids", package = "swereg")
data("fake_demographics", package = "swereg")
data("fake_annual_family", package = "swereg") 
data("fake_inpatient_diagnoses", package = "swereg")
data("fake_outpatient_diagnoses", package = "swereg")
data("fake_prescriptions", package = "swereg")
data("fake_cod", package = "swereg")

# Apply make_lowercase_names to all datasets
swereg::make_lowercase_names(fake_demographics)
swereg::make_lowercase_names(fake_annual_family)
swereg::make_lowercase_names(fake_inpatient_diagnoses)
swereg::make_lowercase_names(fake_outpatient_diagnoses)
swereg::make_lowercase_names(fake_prescriptions)
swereg::make_lowercase_names(fake_cod)

# Create skeleton covering 2015-2020
skeleton <- swereg::create_skeleton(
  ids = fake_person_ids,
  date_min = "2015-01-01",
  date_max = "2020-12-31"
)

# Add demographics
swereg::add_onetime(skeleton, fake_demographics, id_name = "lopnr")

# Add annual data
swereg::add_annual(skeleton, fake_annual_family, id_name = "lopnr", isoyear = 2015)

# Add diagnoses
diagnoses_combined <- rbindlist(list(
  fake_inpatient_diagnoses,
  fake_outpatient_diagnoses
), use.names = TRUE, fill = TRUE)

swereg::add_diagnoses(
  skeleton,
  diagnoses_combined,
  id_name = "lopnr",
  diags = list(
    "depression" = c("F32", "F33"),
    "anxiety" = c("F40", "F41"),
    "gender_dysphoria" = c("F64"),
    "psychosis" = c("F20", "F25")
  )
)

# Add prescriptions
swereg::add_rx(
  skeleton,
  fake_prescriptions,
  id_name = "p444_lopnr_personnr",
  rxs = list(
    "antidepressants" = c("N06A"),
    "antipsychotics" = c("N05A"),
    "hormones" = c("G03")
  )
)

# Add cause of death
swereg::add_cods(
  skeleton,
  fake_cod,
  id_name = "lopnr",
  cods = list(
    "external_death" = c("X60", "X70"),
    "cardiovascular_death" = c("I21", "I22")
  )
)

cat("skeleton1_create completed:", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")

## -----------------------------------------------------------------------------
  
  # CLEANING OPERATIONS (using only data within skeleton)
  
  # 1. Create age variable
  skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
  skeleton[, age := isoyear - birth_year]
  
  # 2. Create mental health composite variables
  skeleton[, any_mental_health := depression | anxiety | psychosis]
  skeleton[, severe_mental_illness := psychosis | gender_dysphoria]
  
  # 3. Create medication concordance variables
  skeleton[, depression_treated := depression & antidepressants]
  skeleton[, psychosis_treated := psychosis & antipsychotics]
  
  # 4. Create life stage variables
  skeleton[, life_stage := fcase(
    age < 18, "child",
    age >= 18 & age < 65, "adult", 
    age >= 65, "elderly",
    default = "unknown"
  )]
  
  # 5. Create outcome variables
  skeleton[, death_any := external_death | cardiovascular_death]
  
  # 6. Filter to valid ages and reasonable time periods
  skeleton <- skeleton[age >= 0 & age <= 100]
  skeleton <- skeleton[isoyear >= 2015]  # Remove historical rows
  
  # 7. Create person-level summaries for annual data
  if (skeleton[, any(is_isoyear == TRUE)]) {
    skeleton[is_isoyear == TRUE, n_mental_health_year := sum(c(depression, anxiety, psychosis), na.rm = TRUE), by = .(id, isoyear)]
    skeleton[is_isoyear == TRUE, treatment_adherence := mean(c(depression_treated, psychosis_treated), na.rm = TRUE), by = .(id, isoyear)]
  }
  
  # 8. Create registry tag variables (simulate case-control study)
  skeleton[, register_tag := fcase(
    gender_dysphoria == TRUE, "case",
    id %% 3 == 0, "control_matched",
    default = "control_population"
  )]
  
  # 9. Create shared case variables (for matched studies)
  # Find first gender dysphoria diagnosis for cases
  gd_first <- skeleton[gender_dysphoria == TRUE & register_tag == "case", 
                       .(first_gd_year = min(isoyear, na.rm = TRUE)), 
                       by = .(id)]
  
  # Add to skeleton
  skeleton[gd_first, on = "id", first_gd_year := first_gd_year]
  
  # For controls, assign their matched case's first GD year (simplified)
  skeleton[register_tag != "case", first_gd_year := 2016]  # Simplified for demo
  
  # 10. Remove temporary variables
  skeleton[, c("fodelseman", "birth_year") := NULL]
  
cat("skeleton2_clean completed:", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")

## -----------------------------------------------------------------------------
# Show structure
cat("Variables:", paste(names(skeleton), collapse = ", "), "\n")

# Example analysis: Depression prevalence by life stage
depression_summary <- skeleton[is_isoyear == TRUE, .(
  n_person_years = .N,
  depression_prev = mean(depression, na.rm = TRUE),
  treatment_rate = mean(depression_treated[depression == TRUE], na.rm = TRUE)
), by = .(life_stage, register_tag)]

print(depression_summary)

# Example: Mental health treatment patterns
treatment_summary <- skeleton[any_mental_health == TRUE & is_isoyear == TRUE, .(
  antidepressant_use = mean(antidepressants, na.rm = TRUE),
  antipsychotic_use = mean(antipsychotics, na.rm = TRUE),
  hormone_use = mean(hormones, na.rm = TRUE),
  mean_age = mean(age, na.rm = TRUE)
), by = register_tag]

print(treatment_summary)

