## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(swereg)
library(data.table)

## ----eval=FALSE---------------------------------------------------------------
# # Age at first GD diagnosis for AFAB individuals
# make_rowind_first_occurrence(skeleton_gd,
#                             condition = "diag_gd_icd10_F64_089 == TRUE & is_amab == FALSE",
#                             value_var = "age",
#                             new_var = "rowind_age_first_gd_afab")
# 
# # Education level at time of first diagnosis
# make_rowind_first_occurrence(skeleton_gd,
#                             condition = "isoyearweek == rowind_isoyearweek_first_gd",
#                             value_var = "rowdep_edu_cat",
#                             new_var = "rowind_edu_at_first_dx")

## ----eval=FALSE---------------------------------------------------------------
# # The helper function does this automatically:
# skeleton_gd[diag_gd_icd10_F64_089 == TRUE & is_amab == FALSE, temp := age]
# skeleton_gd[, rowind_age_first_gd_afab := swereg::first_non_na(temp), by = .(id)]
# skeleton_gd[, temp := NULL]  # Always clean up temp variable

## ----eval=FALSE---------------------------------------------------------------
# # If date of birth is truly the same for all rows (as it should be)
# setnames(skeleton_gd, "dob", "rowind_dob")
# 
# # If birth country was added from demographics (already person-level)
# setnames(skeleton_gd, "birth_country", "rowind_birth_country")

## -----------------------------------------------------------------------------
# Load fake data
data("fake_demographics")
data("fake_inpatient_diagnoses")

# Create a small skeleton for demonstration
ids <- fake_demographics$lopnr[1:5]
skeleton <- create_skeleton(ids, "2020-01-01", "2020-03-31")

# Add demographic data (creates rowind variables)
swereg::make_lowercase_names(fake_demographics, date_columns = "DodDatum")
add_onetime(skeleton, fake_demographics, "lopnr")

# Add diagnosis data (creates rowdep variables)
swereg::make_lowercase_names(fake_inpatient_diagnoses, date_columns = "INDATUM")
add_diagnoses(skeleton, fake_inpatient_diagnoses, "lopnr", 
              diags = list("f64_diag" = "^F64"))

# Examine the structure
print("Skeleton structure:")
head(skeleton[id == ids[1]], 8)

## -----------------------------------------------------------------------------
# Example 1: ISO year of first F64 diagnosis  
make_rowind_first_occurrence(skeleton,
                            condition = "f64_diag == TRUE",
                            value_var = "isoyear", 
                            new_var = "rowind_isoyear_first_f64")

# Example 2: ISO year-week of first F64 diagnosis
make_rowind_first_occurrence(skeleton,
                            condition = "f64_diag == TRUE",
                            value_var = "isoyearweek", 
                            new_var = "rowind_isoyearweek_first_f64")

# View results
print("Results with rowind variables:")
head(skeleton[id == ids[1], .(id, isoyear, isoyearweek, f64_diag, 
                              rowind_isoyear_first_f64, rowind_isoyearweek_first_f64)], 8)

## ----eval=FALSE---------------------------------------------------------------
# skeleton[condition == TRUE, temp := value]
# skeleton[, new_rowind_var := first_non_na(temp), by = .(id)]
# skeleton[, temp := NULL]  # Critical: clean up

## ----eval=FALSE---------------------------------------------------------------
# # This should return 1 for all persons (meaning all rows have same value)
# skeleton[, .(unique_values = uniqueN(rowind_isoyear_first_f64)), by = .(id)]

