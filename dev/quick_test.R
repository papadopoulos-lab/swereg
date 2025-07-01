# Quick test script for basic swereg functionality
# Minimal test with fake data for rapid development

library(data.table)
devtools::load_all(".")

# Load fake data
data("fake_person_ids")
data("fake_demographics") 
data("fake_inpatient_diagnoses")

# Quick test with small subset
ids <- fake_person_ids[1:50]

cat("Quick test with", length(ids), "individuals\n")

# Create skeleton
skeleton <- swereg::create_skeleton(
  ids = ids,
  date_min = "2010-01-01", 
  date_max = "2012-12-31"
)

cat("Skeleton created:", nrow(skeleton), "rows\n")

# Add demographics
demographics_subset <- fake_demographics[lopnr %in% ids]
swereg::add_onetime(skeleton, demographics_subset, id_name = "lopnr")

cat("Demographics added. Sample:\n")
print(head(skeleton[, .(id, isoyear, isoyearweek, fodelseman, DodDatum)]))

# Add simple diagnosis
diagnoses_subset <- fake_inpatient_diagnoses[LopNr %in% ids]
swereg::make_lowercase_names(diagnoses_subset)
swereg::add_diagnoses(
  skeleton, 
  diagnoses_subset,
  id_name = "lopnr",  # Now lowercase after make_lowercase_names
  diags = list("gd_any" = c("^F64"))
)

cat("\nGender dysphoria cases:", sum(skeleton$gd_any, na.rm = TRUE), "\n")
cat("Quick test completed successfully!\n")