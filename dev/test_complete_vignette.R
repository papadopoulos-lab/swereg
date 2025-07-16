library(data.table)
library(survival)
library(ggplot2)

# Load synthetic registry data
data("fake_person_ids", package = "swereg")
data("fake_demographics", package = "swereg")
data("fake_annual_family", package = "swereg")
data("fake_inpatient_diagnoses", package = "swereg")
data("fake_outpatient_diagnoses", package = "swereg")

# Apply make_lowercase_names to all datasets (REQUIRED)
swereg::make_lowercase_names(fake_demographics)
swereg::make_lowercase_names(fake_annual_family)
swereg::make_lowercase_names(fake_inpatient_diagnoses)
swereg::make_lowercase_names(fake_outpatient_diagnoses)

# Use subset for demonstration (need adequate sample size for survival analysis)
# Filter to include only people who were adults (18+) in 1995
demographics_all <- fake_demographics[lopnr %in% fake_person_ids[1:500]]
demographics_all[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
demographics_all[, age_1995 := 1995 - birth_year]
study_ids <- demographics_all[age_1995 >= 18 & age_1995 <= 90]$lopnr
cat("Study population:", length(study_ids), "individuals (adults in 1995)\n")

# Create skeleton for 11-year follow-up
skeleton <- swereg::create_skeleton(
  ids = study_ids,
  date_min = "1995-01-01",
  date_max = "2005-12-31"
)

cat("Skeleton created:", nrow(skeleton), "rows\n")
cat("Time structure: weeks =", sum(!skeleton$is_isoyear), ", years =", sum(skeleton$is_isoyear), "\n")

# Add demographics (one-time data)
demographics_subset <- fake_demographics[lopnr %in% study_ids]
swereg::add_onetime(skeleton, demographics_subset, id_name = "lopnr")
cat("Demographics added for", nrow(demographics_subset), "individuals\n")

# Add annual income for each year 1995-2005
annual_subset <- fake_annual_family[lopnr %in% study_ids]

# We'll add income data for each year separately
for(year in 1995:2005) {
  # In real data, you'd have separate files for each year
  # Here we simulate by reusing the same data with different years
  annual_data_year <- copy(annual_subset)
  
  # Add some year-to-year variation in income (for demonstration)
  annual_data_year[, income := round(runif(.N, 150000, 600000) * (year - 1994) / 11)]
  
  swereg::add_annual(skeleton, annual_data_year, id_name = "lopnr", isoyear = year)
}

cat("Annual income data added for years 1995-2005\n")

# Combine inpatient and outpatient diagnoses
diagnoses_combined <- rbindlist(list(
  fake_inpatient_diagnoses[lopnr %in% study_ids],
  fake_outpatient_diagnoses[lopnr %in% study_ids]
), use.names = TRUE, fill = TRUE)

# Add heart attack diagnoses
swereg::add_diagnoses(
  skeleton,
  diagnoses_combined,
  id_name = "lopnr",
  diag_type = "both",
  diags = list(
    "heart_attack" = c("I21", "I22"),  # Acute myocardial infarction
    "stroke" = c("I63", "I64"),       # Cerebrovascular disease (competing risk)
    "diabetes" = c("E10", "E11")      # Diabetes (confounder)
  )
)

cat("Diagnosis data added\n")
cat("Heart attacks detected:", sum(skeleton$heart_attack, na.rm = TRUE), "events\n")

# Create age variable
skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
skeleton[, age := isoyear - birth_year]

# Create income categories (time-varying)
skeleton[, income_category := fcase(
  income < 250000, "Low",
  income >= 250000 & income < 400000, "Medium",
  income >= 400000, "High",
  default = "Unknown"
)]

# Create baseline characteristics (use first available year)
skeleton[, baseline_age := age[which.min(isoyear)], by = id]
skeleton[, baseline_income := income[which.min(isoyear)], by = id]

# Filter to non-missing income (age already filtered during ID selection)
skeleton <- skeleton[!is.na(income)]

cat("After filtering:", nrow(skeleton), "person-time periods\n")
cat("Unique individuals:", uniqueN(skeleton$id), "\n")

# Step 7a: Create yearly person-time data
survival_data <- skeleton[is_isoyear == TRUE, .(
  id = id,
  year = isoyear,
  age = age,
  income = income,
  income_category = income_category,
  diabetes = diabetes,
  heart_attack = heart_attack,
  stroke = stroke
)]

cat("Survival data created:", nrow(survival_data), "person-years\n")

# Step 7b: Create time-to-event variables
survival_data <- survival_data[order(id, year)]

# Calculate follow-up time (years from baseline)
survival_data[, time_start := year - 1995]
survival_data[, time_end := year - 1995 + 1]

# Identify first event
survival_data[, first_heart_attack := min(year[heart_attack == TRUE], na.rm = TRUE), by = id]
survival_data[, first_stroke := min(year[stroke == TRUE], na.rm = TRUE), by = id]

# Create event indicator and event time
survival_data[, event := fcase(
  year == first_heart_attack, 1,  # Heart attack
  year == first_stroke, 2,        # Stroke (competing risk)
  default = 0                     # No event
)]

# Adjust time_end for events
survival_data[event > 0, time_end := time_start + 0.5]  # Assume mid-year event

# Remove follow-up after first event
survival_data <- survival_data[year <= pmin(first_heart_attack, first_stroke, 2005, na.rm = TRUE)]

cat("Survival dataset created:", nrow(survival_data), "person-years\n")
cat("Heart attack events:", sum(survival_data$event == 1, na.rm = TRUE), "\n")
cat("Stroke events:", sum(survival_data$event == 2, na.rm = TRUE), "\n")

# Prepare data for Cox regression
# Create survival object with time-varying covariates
cox_data <- survival_data[, .(
  id = id,
  time_start = time_start,
  time_end = time_end,
  event = as.numeric(event == 1),  # Focus on heart attack
  age = age,
  income_log = log(income),
  income_category = factor(income_category, levels = c("Low", "Medium", "High")),
  diabetes = diabetes
)]

# Remove any rows with missing values
cox_data <- cox_data[complete.cases(cox_data)]

cat("Cox data ready:", nrow(cox_data), "rows\n")
cat("Events:", sum(cox_data$event), "\n")

if(nrow(cox_data) > 0 && sum(cox_data$event) > 0) {
  # Fit Cox model with time-varying covariates
  cox_model <- coxph(
    Surv(time_start, time_end, event) ~ 
      income_log + 
      age + 
      diabetes + 
      cluster(id),
    data = cox_data
  )
  
  # Print results
  print("Cox Proportional Hazards Model Results:")
  print(summary(cox_model))
  
  # Test proportional hazards assumption
  tryCatch({
    ph_test <- cox.zph(cox_model)
    print("Proportional Hazards Test:")
    print(ph_test)
  }, error = function(e) {
    cat("cox.zph error:", conditionMessage(e), "\n")
  })
} else {
  cat("Not enough data for Cox model\n")
  cat("Cox data details:\n")
  print(str(cox_data))
}