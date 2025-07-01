# Development script using fake Swedish registry data
# This demonstrates the full swereg workflow with synthetic data

library(data.table)

# Load package functions
devtools::load_all(".")

# Load fake data (created by generate_fake_data.R)
cat("Loading fake Swedish registry data...\n")

# Load fake data sets
data("fake_person_ids")
data("fake_demographics")
data("fake_annual_family")
data("fake_inpatient_diagnoses")
data("fake_outpatient_diagnoses")
data("fake_prescriptions")
data("fake_cod")

cat("Loaded datasets:\n")
cat("- fake_person_ids:", length(fake_person_ids), "individuals\n")
cat("- fake_demographics:", nrow(fake_demographics), "records\n")
cat("- fake_annual_family:", nrow(fake_annual_family), "records\n")
cat("- fake_inpatient_diagnoses:", nrow(fake_inpatient_diagnoses), "records\n")
cat("- fake_outpatient_diagnoses:", nrow(fake_outpatient_diagnoses), "records\n")
cat("- fake_prescriptions:", nrow(fake_prescriptions), "records\n")
cat("- fake_cod:", nrow(fake_cod), "records\n\n")

# Use subset for testing (faster)
ids <- fake_person_ids[1:500]

# 1. CREATE SKELETON
cat("=== CREATING SKELETON ===\n")
skeleton <- swereg::create_skeleton(
  ids = ids,
  date_min = "2001-01-01",
  date_max = "2015-12-31"
)
cat("Skeleton created:", nrow(skeleton), "rows\n")
cat("Unique individuals:", length(unique(skeleton$id)), "\n")
cat("Date range:", min(skeleton$isoyearweek), "to", max(skeleton$isoyearweek), "\n\n")

# 2. ADD ONE-TIME DEMOGRAPHIC INFORMATION
cat("=== ADDING DEMOGRAPHICS ===\n")
demographics_subset <- fake_demographics[lopnr %in% ids]
cat("Demographics available for", nrow(demographics_subset), "individuals\n")

swereg::add_onetime(
  skeleton,
  demographics_subset,
  id_name = "lopnr"
)
cat("Added demographic variables:", setdiff(names(skeleton), c("id", "isoyear", "isoyearweek", "is_isoyear")), "\n\n")

# 3. ADD ANNUAL FAMILY INFORMATION  
cat("=== ADDING ANNUAL FAMILY DATA ===\n")
annual_subset <- fake_annual_family[LopNr %in% ids]
cat("Annual family data available for", nrow(annual_subset), "individuals\n")

swereg::add_annual(
  skeleton,
  annual_subset,
  id_name = "LopNr",
  isoyear = 2001
)
cat("Added annual variables for 2001\n\n")

# 4. ADD DIAGNOSIS INFORMATION
cat("=== ADDING DIAGNOSES ===\n")

# Combine inpatient and outpatient data
diagnoses_combined <- rbindlist(list(
  fake_inpatient_diagnoses[LopNr %in% ids],
  fake_outpatient_diagnoses[LopNr %in% ids]
), use.names = TRUE, fill = TRUE)

# Apply make_lowercase_names as required by swereg
swereg::make_lowercase_names(diagnoses_combined)

cat("Total diagnosis records:", nrow(diagnoses_combined), "\n")

# Define diagnosis patterns (including gender dysphoria research focus)
diagnosis_patterns <- list(
  "diag_gd_icd10_F64_0" = c("^F640"),
  "diag_gd_icd10_F64_89" = c("^F6489"),
  "diag_gd_icd10_F64_089" = c("^F640", "^F648", "^F649"),
  
  "diag_psychiatric_not_gd" = c(
    "^F", "!F640", "!F648", "!F649" # ICD10: F00-F99 excluding gender dysphoria
  ),
  
  "diag_depression" = c("^F32", "^F33"),
  "diag_anxiety" = c("^F40", "^F41"),
  "diag_psychotic" = c("^F20", "^F25"),
  "diag_bipolar" = c("^F30", "^F31"),
  
  # Physical health conditions
  "diag_cardiovascular" = c("^I10", "^I20", "^I21", "^I25"),
  "diag_diabetes" = c("^E10", "^E11", "^E14"),
  "diag_respiratory" = c("^J44", "^J45", "^J46")
)

swereg::add_diagnoses(
  skeleton,
  diagnoses_combined,
  id_name = "lopnr",  # Now lowercase after make_lowercase_names
  diags = diagnosis_patterns
)

# Check results
diag_vars <- names(diagnosis_patterns)
cat("Added diagnosis variables:\n")
for(var in diag_vars) {
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "positive cases\n")
}
cat("\n")

# 5. ADD PRESCRIPTION INFORMATION
cat("=== ADDING PRESCRIPTIONS ===\n")
prescriptions_subset <- fake_prescriptions[P1193_LopNr_PersonNr %in% ids]
cat("Prescription records:", nrow(prescriptions_subset), "\n")

# Apply make_lowercase_names as in actual dev scripts
swereg::make_lowercase_names(prescriptions_subset)

# Rename ID column to match skeleton
setnames(prescriptions_subset, "p1193_lopnr_personnr", "id")

# Define prescription patterns (hormone therapy focus)
prescription_patterns <- list(
  "rx_estrogens" = c("^G03CA"), # Estrogens
  "rx_progestins" = c("^G03BA"), # Progestins  
  "rx_combined_ht" = c("^G03FA"), # Combined hormone therapy
  "rx_antiandrogens" = c("^G03AA"), # Antiandrogens
  "rx_antidepressants" = c("^N06A"), # Antidepressants
  "rx_anxiolytics" = c("^N05B"), # Anxiolytics
  "rx_cardiovascular" = c("^C07", "^C08", "^C09") # CV medications
)

swereg::add_rx(
  skeleton,
  prescriptions_subset,
  id_name = "id",
  drugs = prescription_patterns
)

# Check prescription results
rx_vars <- names(prescription_patterns)
cat("Added prescription variables:\n")
for(var in rx_vars) {
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "positive cases\n")
}
cat("\n")

# 6. ADD CAUSE OF DEATH INFORMATION
cat("=== ADDING CAUSE OF DEATH ===\n")
cod_subset <- fake_cod[lopnr %in% ids]
cat("Cause of death records:", nrow(cod_subset), "\n")

cod_patterns <- list(
  "cod_cardiovascular" = c("^I21", "^I22"),
  "cod_cancer" = c("^C78", "^C80"),
  "cod_external" = c("^X60", "^X61", "^X70"),
  "cod_respiratory" = c("^J44")
)

swereg::add_cods(
  skeleton,
  cod_subset,
  id_name = "lopnr",
  cods = cod_patterns
)

# Check CoD results  
cod_vars <- names(cod_patterns)
cat("Added cause of death variables:\n")
for(var in cod_vars) {
  count <- sum(skeleton[[var]], na.rm = TRUE)
  cat("-", var, ":", count, "positive cases\n")
}
cat("\n")

# 7. SUMMARY AND VALIDATION
cat("=== FINAL SUMMARY ===\n")
cat("Final skeleton dimensions:", nrow(skeleton), "rows x", ncol(skeleton), "columns\n")
cat("Variables added:", setdiff(names(skeleton), c("id", "isoyear", "isoyearweek", "is_isoyear")), "\n")

# Show some examples
cat("\nExample: Gender dysphoria cases by year\n")
gd_summary <- skeleton[is_isoyear == TRUE & diag_gd_icd10_F64_089 == TRUE, .N, by = isoyear]
if(nrow(gd_summary) > 0) {
  print(gd_summary)
} else {
  cat("No gender dysphoria cases found in annual summary\n")
}

cat("\nExample: Hormone therapy usage\n")  
ht_summary <- skeleton[is_isoyear == TRUE, .(
  estrogens = sum(rx_estrogens, na.rm = TRUE),
  combined_ht = sum(rx_combined_ht, na.rm = TRUE),
  antiandrogens = sum(rx_antiandrogens, na.rm = TRUE)
), by = isoyear]
print(head(ht_summary))

cat("\nDevelopment test completed successfully!\n")
cat("The swereg package is working correctly with fake data.\n")