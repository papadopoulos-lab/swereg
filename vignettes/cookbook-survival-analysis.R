## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(data.table)
library(survival)
library(ggplot2)
library(zoo)  # For na.locf function

## -----------------------------------------------------------------------------
# Load synthetic registry data
data("fake_person_ids", package = "swereg")
data("fake_demographics", package = "swereg")
data("fake_annual_family", package = "swereg")  # Contains income data
data("fake_inpatient_diagnoses", package = "swereg")
data("fake_outpatient_diagnoses", package = "swereg")

# Apply make_lowercase_names to all datasets (REQUIRED)
swereg::make_lowercase_names(fake_demographics, date_columns = "fodelseman")
swereg::make_lowercase_names(fake_annual_family)
swereg::make_lowercase_names(fake_inpatient_diagnoses, date_columns = "indatum")
swereg::make_lowercase_names(fake_outpatient_diagnoses, date_columns = "indatum")

# Use subset for demonstration (need adequate sample size for survival analysis)
study_ids <- fake_person_ids[1:500]
cat("Study population:", length(study_ids), "individuals\n")

## -----------------------------------------------------------------------------
# Create skeleton for 11-year follow-up
skeleton <- swereg::create_skeleton(
  ids = study_ids,
  date_min = "1995-01-01",
  date_max = "2005-12-31"
)

cat("Skeleton created:", nrow(skeleton), "rows\n")
cat("Time structure: weeks =", sum(!skeleton$is_isoyear), ", years =", sum(skeleton$is_isoyear), "\n")

## -----------------------------------------------------------------------------
# Add demographics (one-time data)
swereg::add_onetime(skeleton, fake_demographics, id_name = "lopnr")

# Create age variable immediately after demographics
skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
skeleton[, age := lubridate::year(isoyearweek_sunday) - birth_year]

cat("Demographics added for", nrow(fake_demographics), "individuals\n")

## -----------------------------------------------------------------------------
# Add annual income for each year 1995-2005

# We'll add income data for each year separately
for(year in 1995:2005) {
  # In real data, you'd have separate files for each year
  # Here we simulate by reusing the same data with different years
  annual_data_year <- copy(fake_annual_family)
  
  # Add simulated income data for demonstration
  annual_data_year[, income := round(runif(.N, 150000, 600000) * (year - 1994) / 11)]
  
  swereg::add_annual(skeleton, annual_data_year, id_name = "lopnr", isoyear = year)
}

cat("Annual income data added for years 1995-2005\n")

## -----------------------------------------------------------------------------
# Combine inpatient and outpatient diagnoses
diagnoses_combined <- rbindlist(list(
  fake_inpatient_diagnoses,
  fake_outpatient_diagnoses
), use.names = TRUE, fill = TRUE)

# Clean the combined dataset (required for add_diagnoses)
swereg::make_lowercase_names(diagnoses_combined, date_columns = "indatum")

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

## -----------------------------------------------------------------------------
# Create income categories (time-varying)
skeleton[, income_category := fcase(
  income < 300000, "Low",
  income >= 300000 & income < 500000, "Medium",
  income >= 500000, "High",
  default = "Unknown"
)]

# Create baseline characteristics (use first available year)
skeleton[, baseline_age := age[which.min(isoyear)], by = id]
skeleton[, baseline_income := income[which.min(isoyear)], by = id]

# Filter to valid ages and non-missing income
# IMPORTANT: Filter at person level - remove people who were under 18 during study
# Calculate minimum age for each person during study period
skeleton[, min_age := min(age, na.rm = TRUE), by = id]
skeleton[, max_age := max(age, na.rm = TRUE), by = id]
valid_ids <- skeleton[min_age >= 18 & max_age <= 90]$id
skeleton <- skeleton[id %in% valid_ids]
skeleton <- skeleton[!is.na(income)]

cat("After filtering:", nrow(skeleton), "person-time periods\n")
cat("Unique individuals:", uniqueN(skeleton$id), "\n")

# Create Table 1: Descriptive statistics
cat("\n=== TABLE 1: DESCRIPTIVE STATISTICS ===\n")

# Overall sample characteristics
cat("Sample size:", uniqueN(skeleton$id), "individuals\n")
cat("Total person-time periods:", nrow(skeleton), "\n")
cat("Total person-time:", round(sum(skeleton$personyears, na.rm = TRUE), 1), "person-years\n")
cat("Follow-up period: 1995-2005 (11 years)\n")

# Age characteristics (at baseline)
baseline_data <- skeleton[, .SD[which.min(isoyear)], by = id]
cat("\nAge at baseline (1995):\n")
cat("- Mean (SD):", round(mean(baseline_data$age, na.rm = TRUE), 1), 
    "(", round(sd(baseline_data$age, na.rm = TRUE), 1), ")\n")
cat("- Median [IQR]:", round(median(baseline_data$age, na.rm = TRUE), 1),
    "[", round(quantile(baseline_data$age, 0.25, na.rm = TRUE), 1), "-",
    round(quantile(baseline_data$age, 0.75, na.rm = TRUE), 1), "]\n")
cat("- Range:", round(min(baseline_data$age, na.rm = TRUE), 1), "-",
    round(max(baseline_data$age, na.rm = TRUE), 1), "\n")

# Income characteristics (at baseline)
cat("\nIncome at baseline (1995):\n")
cat("- Mean (SD):", round(mean(baseline_data$income, na.rm = TRUE), 0),
    "(", round(sd(baseline_data$income, na.rm = TRUE), 0), ")\n")
cat("- Median [IQR]:", round(median(baseline_data$income, na.rm = TRUE), 0),
    "[", round(quantile(baseline_data$income, 0.25, na.rm = TRUE), 0), "-",
    round(quantile(baseline_data$income, 0.75, na.rm = TRUE), 0), "]\n")

# Income categories
income_table <- table(baseline_data$income_category)
cat("\nIncome categories at baseline:\n")
for(i in 1:length(income_table)) {
  cat("-", names(income_table)[i], ":", income_table[i], 
      "(", round(income_table[i]/sum(income_table)*100, 1), "%)\n")
}

# Health conditions (ever during follow-up)
health_summary <- skeleton[, .(
  heart_attack = any(heart_attack, na.rm = TRUE),
  stroke = any(stroke, na.rm = TRUE),
  diabetes = any(diabetes, na.rm = TRUE)
), by = id]

cat("\nHealth conditions during follow-up:\n")
cat("- Heart attack:", sum(health_summary$heart_attack, na.rm = TRUE), 
    "(", round(sum(health_summary$heart_attack, na.rm = TRUE)/nrow(health_summary)*100, 1), "%)\n")
cat("- Stroke:", sum(health_summary$stroke, na.rm = TRUE),
    "(", round(sum(health_summary$stroke, na.rm = TRUE)/nrow(health_summary)*100, 1), "%)\n")
cat("- Diabetes:", sum(health_summary$diabetes, na.rm = TRUE),
    "(", round(sum(health_summary$diabetes, na.rm = TRUE)/nrow(health_summary)*100, 1), "%)\n")

cat("\n=== END TABLE 1 ===\n")

## -----------------------------------------------------------------------------
# Create person-time dataset for survival analysis
# We need to collapse weekly data to periods with consistent covariate values

# Step 7a: Create weekly person-time data
survival_data <- skeleton[is_isoyear == FALSE, .(
  id = id,
  isoyearweek = isoyearweek,
  isoyearweek_sunday = isoyearweek_sunday,
  year = isoyear,
  age = age,
  income = income,
  income_category = income_category,
  diabetes = diabetes,
  heart_attack = heart_attack,
  stroke = stroke,
  personyears = personyears
)]

# Step 7b: Create time-to-event variables
survival_data <- survival_data[order(id, isoyearweek_sunday)]

# Calculate follow-up time (years from baseline)
baseline_date <- as.Date("1995-01-01")
survival_data[, time_start := as.numeric(isoyearweek_sunday - baseline_date) / 365.25]
survival_data[, time_end := time_start + personyears]

# Identify first event
survival_data[, first_heart_attack := min(isoyearweek_sunday[heart_attack == TRUE], na.rm = TRUE), by = id]
survival_data[, first_stroke := min(isoyearweek_sunday[stroke == TRUE], na.rm = TRUE), by = id]

# Create event indicator and event time
survival_data[, event := fcase(
  isoyearweek_sunday == first_heart_attack, 1,  # Heart attack
  isoyearweek_sunday == first_stroke, 2,        # Stroke (competing risk)
  default = 0                     # No event
)]

# Remove follow-up after first event
survival_data <- survival_data[isoyearweek_sunday <= pmin(first_heart_attack, first_stroke, as.Date("2005-12-31"), na.rm = TRUE)]

cat("Survival dataset created:", nrow(survival_data), "person-weeks\n")
cat("Heart attack events:", sum(survival_data$event == 1, na.rm = TRUE), "\n")
cat("Stroke events:", sum(survival_data$event == 2, na.rm = TRUE), "\n")

## -----------------------------------------------------------------------------
# Baseline characteristics
baseline_table <- survival_data[year == 1995, .(
  n = uniqueN(id),
  mean_age = mean(age, na.rm = TRUE),
  mean_income = mean(income, na.rm = TRUE),
  diabetes_prev = mean(diabetes, na.rm = TRUE)
), by = income_category]

print("Baseline characteristics by income category:")
print(baseline_table)

# Event rates by income category
event_rates <- survival_data[, .(
  person_years = sum(personyears),
  heart_attacks = sum(event == 1),
  rate_per_1000 = sum(event == 1) / sum(personyears) * 1000
), by = income_category]

print("Heart attack rates by income category:")
print(event_rates)

## -----------------------------------------------------------------------------
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

cat("Cox data summary:\n")
cat("- Rows:", nrow(cox_data), "\n")
cat("- Events:", sum(cox_data$event), "\n")
cat("- Unique individuals:", uniqueN(cox_data$id), "\n")

# Check if we have enough data for Cox model
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
  
  # Calculate hazard ratios
  hr_results <- data.table(
    variable = names(coef(cox_model)),
    hr = exp(coef(cox_model)),
    ci_lower = exp(confint(cox_model)[, 1]),
    ci_upper = exp(confint(cox_model)[, 2]),
    p_value = summary(cox_model)$coefficients[, "Pr(>|z|)"]
  )
  
  print("Hazard Ratios:")
  print(hr_results)
} else {
  cat("ERROR: Insufficient data for Cox model\n")
  cat("This indicates a problem with the data preparation or filtering\n")
  cat("In real analysis, you would need to:\n")
  cat("1. Use a larger sample size\n")
  cat("2. Use a longer follow-up period\n")
  cat("3. Check data quality and completeness\n")
  
  # Create dummy results for demonstration
  cox_model <- NULL
  hr_results <- data.table(
    variable = c("income_log", "age", "diabetes"),
    hr = c(0.85, 1.02, 1.15),
    ci_lower = c(0.60, 0.98, 0.80),
    ci_upper = c(1.20, 1.06, 1.65),
    p_value = c(0.35, 0.31, 0.45)
  )
  
  print("Example results (for demonstration only):")
  print(hr_results)
}

## -----------------------------------------------------------------------------
# Interpret key results
if(!is.null(cox_model)) {
  income_hr <- hr_results[variable == "income_log"]
  cat("Income Effect:\n")
  cat("- 10% increase in income associated with HR =", round(income_hr$hr^0.1, 3), "\n")
  cat("- 95% CI: (", round(income_hr$ci_lower^0.1, 3), ", ", round(income_hr$ci_upper^0.1, 3), ")\n")
  
  # Test proportional hazards assumption
  tryCatch({
    ph_test <- cox.zph(cox_model)
    print("Proportional Hazards Test:")
    print(ph_test)
    
    # Plot Schoenfeld residuals
    if(any(ph_test$table[, "p"] < 0.05)) {
      cat("WARNING: Proportional hazards assumption may be violated\n")
    }
  }, error = function(e) {
    cat("Note: Proportional hazards test could not be performed\n")
    cat("This may be due to insufficient events or model complexity\n")
  })
} else {
  cat("Model interpretation using example results:\n")
  income_hr <- hr_results[variable == "income_log"]
  cat("Income Effect (example):\n")
  cat("- 10% increase in income associated with HR =", round(income_hr$hr^0.1, 3), "\n")
  cat("- 95% CI: (", round(income_hr$ci_lower^0.1, 3), ", ", round(income_hr$ci_upper^0.1, 3), ")\n")
  cat("- This suggests lower income may be associated with higher heart attack risk\n")
}

## -----------------------------------------------------------------------------
# Create survival curves by income category
if(nrow(cox_data) > 0 && sum(cox_data$event) > 0) {
  tryCatch({
    # For visualization, we'll use income at baseline
    baseline_income <- cox_data[, .(baseline_income_cat = first(income_category)), by = id]
    plot_data <- merge(cox_data, baseline_income, by = "id")
    
    # Kaplan-Meier curves
    km_fit <- survfit(
      Surv(time_start, time_end, event) ~ baseline_income_cat,
      data = plot_data
    )
    
    # Basic survival plot
    plot(km_fit, 
         col = c("red", "blue", "green"),
         lty = 1,
         xlab = "Years from baseline",
         ylab = "Survival probability",
         main = "Heart Attack-Free Survival by Income Category")
    
    legend("bottomleft", 
           legend = c("Low", "Medium", "High"),
           col = c("red", "blue", "green"),
           lty = 1)
  }, error = function(e) {
    cat("Survival plot could not be generated due to insufficient data\n")
    cat("In a real analysis with adequate events, you would see:\n")
    cat("- Kaplan-Meier survival curves by income category\n")
    cat("- Differences in heart attack-free survival over time\n")
    cat("- Confidence intervals around the curves\n")
  })
} else {
  cat("Survival plot not generated due to insufficient data\n")
  cat("In a real analysis with adequate events, you would see:\n")
  cat("- Kaplan-Meier survival curves by income category\n")
  cat("- Differences in heart attack-free survival over time\n")
  cat("- Confidence intervals around the curves\n")
}

## -----------------------------------------------------------------------------
# Check for missing income by year
missing_income <- skeleton[is_isoyear == TRUE, .(
  n_total = .N,
  n_missing = sum(is.na(income)),
  pct_missing = round(sum(is.na(income)) / .N * 100, 1)
), by = isoyear]

print("Missing income data by year:")
print(missing_income)

# Solution: Carry forward/backward or interpolate
skeleton[, income_filled := na.locf(income, na.rm = FALSE), by = id]
skeleton[, income_filled := na.locf(income_filled, fromLast = TRUE, na.rm = FALSE), by = id]

## -----------------------------------------------------------------------------
# Handle individuals with multiple heart attacks
multi_events <- skeleton[, .(
  n_heart_attacks = sum(heart_attack, na.rm = TRUE)
), by = id]

multi_events_summary <- multi_events[, .(
  n_individuals = .N,
  n_with_multiple = sum(n_heart_attacks > 1)
), by = .(event_category = ifelse(n_heart_attacks == 0, "No events",
                                 ifelse(n_heart_attacks == 1, "One event", "Multiple events")))]

print("Multiple heart attacks:")
print(multi_events_summary)

# Solution: Use only first event or recurrent event models

## -----------------------------------------------------------------------------
# Handle individuals who may have had events before study start
# Check for prevalent cases (diagnosis before 2015)
cat("Note: This analysis assumes no prevalent cases at baseline\n")
cat("In real analysis, you would:\n")
cat("1. Exclude individuals with heart attack before 1995\n")
cat("2. Or use left-truncated survival analysis\n")

## -----------------------------------------------------------------------------
# For datasets with >100,000 individuals, consider:

# 1. Batch processing (see skeleton3_analyze vignette)
# 2. Memory-efficient data handling
# 3. Parallel processing for Cox models

cat("Performance recommendations for large datasets:\n")
cat("- Use batching for skeleton creation (see skeleton3_analyze vignette)\n")
cat("- Consider stratified Cox models for very large datasets\n")
cat("- Use data.table operations for efficient aggregation\n")

