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

# Use subset - NO FILTERING OF IDs
study_ids <- fake_person_ids[1:500]
cat("Study population:", length(study_ids), "individuals\n")

# Create skeleton for 1995-2005
skeleton <- swereg::create_skeleton(
  ids = study_ids,
  date_min = '1995-01-01',
  date_max = '2005-12-31'
)
cat("Skeleton created:", nrow(skeleton), "rows\n")

# Add demographics
demographics_subset <- fake_demographics[lopnr %in% study_ids]
swereg::add_onetime(skeleton, demographics_subset, id_name = 'lopnr')

# Add annual income
annual_subset <- fake_annual_family[lopnr %in% study_ids]
for(year in 1995:2005) {
  annual_data_year <- copy(annual_subset)
  annual_data_year[, income := round(runif(.N, 150000, 600000) * (year - 1994) / 11)]
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

# Create age variable
skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
skeleton[, age := isoyear - birth_year]

# Create income categories
skeleton[, income_category := fcase(
  income < 250000, 'Low',
  income >= 250000 & income < 400000, 'Medium',
  income >= 400000, 'High',
  default = 'Unknown'
)]

# FILTER ROWS (NOT IDs)
cat('Before age filter:', nrow(skeleton), 'rows\n')
skeleton <- skeleton[age >= 18 & age <= 90]
cat('After age filter:', nrow(skeleton), 'rows\n')

skeleton <- skeleton[!is.na(income)]
cat('After income filter:', nrow(skeleton), 'rows\n')

# Create survival data
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

cat('Survival data:', nrow(survival_data), 'rows\n')
cat('Heart attack events:', sum(survival_data$heart_attack, na.rm = TRUE), '\n')

# Process survival data
survival_data <- survival_data[order(id, year)]
survival_data[, time_start := year - 1995]
survival_data[, time_end := year - 1995 + 1]
survival_data[, first_heart_attack := min(year[heart_attack == TRUE], na.rm = TRUE), by = id]
survival_data[, first_stroke := min(year[stroke == TRUE], na.rm = TRUE), by = id]
survival_data[, event := fcase(
  year == first_heart_attack, 1,
  year == first_stroke, 2,
  default = 0
)]
survival_data[event > 0, time_end := time_start + 0.5]
survival_data <- survival_data[year <= pmin(first_heart_attack, first_stroke, 2005, na.rm = TRUE)]

cat('Final survival data:', nrow(survival_data), 'rows\n')
cat('Heart attack events:', sum(survival_data$event == 1, na.rm = TRUE), '\n')

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

cat('Cox data:', nrow(cox_data), 'rows with', sum(cox_data$event), 'events\n')

if(nrow(cox_data) > 0 && sum(cox_data$event) > 0) {
  cox_model <- coxph(
    Surv(time_start, time_end, event) ~ 
      income_log + 
      age + 
      diabetes,
    data = cox_data
  )
  cat('Cox model success!\n')
  print(summary(cox_model))
} else {
  cat('Cox model failed - no data\n')
}