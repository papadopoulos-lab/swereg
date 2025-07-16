library(data.table)
library(survival)

# Load datasets
data('fake_person_ids', package = 'swereg')
data('fake_demographics', package = 'swereg')
data('fake_annual_family', package = 'swereg')
data('fake_inpatient_diagnoses', package = 'swereg')
data('fake_outpatient_diagnoses', package = 'swereg')

# Apply make_lowercase_names
swereg::make_lowercase_names(fake_demographics)
swereg::make_lowercase_names(fake_annual_family)
swereg::make_lowercase_names(fake_inpatient_diagnoses)
swereg::make_lowercase_names(fake_outpatient_diagnoses)

# Use subset for demonstration
study_ids <- fake_person_ids[1:100]

# Create skeleton
skeleton <- swereg::create_skeleton(
  ids = study_ids,
  date_min = '2015-01-01',
  date_max = '2020-12-31'
)

# Add demographics
demographics_subset <- fake_demographics[lopnr %in% study_ids]
swereg::add_onetime(skeleton, demographics_subset, id_name = 'lopnr')

# Add annual income
annual_subset <- fake_annual_family[lopnr %in% study_ids]
for(year in 2015:2020) {
  annual_data_year <- copy(annual_subset)
  annual_data_year[, income := round(runif(.N, 200000, 800000) * (year - 2014) / 6)]
  swereg::add_annual(skeleton, annual_data_year, id_name = 'lopnr', isoyear = year)
}

# Add diagnoses
diagnoses_combined <- rbindlist(list(
  fake_inpatient_diagnoses[lopnr %in% study_ids],
  fake_outpatient_diagnoses[lopnr %in% study_ids]
), use.names = TRUE, fill = TRUE)

swereg::add_diagnoses(
  skeleton,
  diagnoses_combined,
  id_name = 'lopnr',
  diag_type = 'both',
  diags = list(
    'heart_attack' = c('I21', 'I22'),
    'stroke' = c('I63', 'I64'),
    'diabetes' = c('E10', 'E11')
  )
)

# Create survival data
skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
skeleton[, age := isoyear - birth_year]
skeleton[, income_category := fcase(
  income < 300000, 'Low',
  income >= 300000 & income < 500000, 'Medium',
  income >= 500000, 'High',
  default = 'Unknown'
)]

# Filter and prepare survival data
skeleton <- skeleton[age >= 18 & age <= 90]
skeleton <- skeleton[!is.na(income)]

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

survival_data <- survival_data[order(id, year)]
survival_data[, time_start := year - 2015]
survival_data[, time_end := year - 2015 + 1]
survival_data[, first_heart_attack := min(year[heart_attack == TRUE], na.rm = TRUE), by = id]
survival_data[, first_stroke := min(year[stroke == TRUE], na.rm = TRUE), by = id]
survival_data[, event := fcase(
  year == first_heart_attack, 1,
  year == first_stroke, 2,
  default = 0
)]
survival_data[event > 0, time_end := time_start + 0.5]
survival_data <- survival_data[year <= pmin(first_heart_attack, first_stroke, 2020, na.rm = TRUE)]

# Create Cox data
cox_data <- survival_data[, .(
  id = id,
  time_start = time_start,
  time_end = time_end,
  event = as.numeric(event == 1),
  age = age,
  income_log = log(income),
  income_category = factor(income_category, levels = c('Low', 'Medium', 'High')),
  diabetes = diabetes
)]

cox_data <- cox_data[complete.cases(cox_data)]

cat('Cox data summary:\n')
cat('Rows:', nrow(cox_data), '\n')
cat('Events:', sum(cox_data$event), '\n')
cat('Unique individuals:', uniqueN(cox_data$id), '\n')

# Check data structure
cat('\nData structure:\n')
print(str(cox_data))

# Try Cox model
tryCatch({
  cox_model <- coxph(
    Surv(time_start, time_end, event) ~ 
      income_log + 
      age + 
      diabetes + 
      cluster(id),
    data = cox_data
  )
  
  cat('Cox model fitted successfully\n')
  cat('Model coefficients:\n')
  print(coef(cox_model))
  
  # Check model structure
  cat('\nModel structure:\n')
  print(names(cox_model))
  
  # Try cox.zph
  tryCatch({
    ph_test <- cox.zph(cox_model)
    cat('Cox.zph test successful\n')
    print(ph_test)
  }, error = function(e) {
    cat('Cox.zph error:', conditionMessage(e), '\n')
    # Try simpler model
    cat('Trying simpler model without cluster...\n')
    tryCatch({
      cox_model_simple <- coxph(
        Surv(time_start, time_end, event) ~ 
          income_log + 
          age + 
          diabetes,
        data = cox_data
      )
      ph_test_simple <- cox.zph(cox_model_simple)
      cat('Simple model cox.zph test successful\n')
      print(ph_test_simple)
    }, error = function(e2) {
      cat('Simple model cox.zph error:', conditionMessage(e2), '\n')
    })
  })
  
}, error = function(e) {
  cat('Cox model error:', conditionMessage(e), '\n')
})