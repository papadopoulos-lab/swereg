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

# Use larger subset
study_ids <- fake_person_ids[1:500]

# Create skeleton for 1995-2005
skeleton <- swereg::create_skeleton(
  ids = study_ids,
  date_min = '1995-01-01',
  date_max = '2005-12-31'
)

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

# Check birth year calculation
skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
skeleton[, age := isoyear - birth_year]

cat('Birth year range:', min(skeleton$birth_year, na.rm=TRUE), 'to', max(skeleton$birth_year, na.rm=TRUE), '\n')
cat('Age range in 1995:', min(skeleton[isoyear==1995]$age, na.rm=TRUE), 'to', max(skeleton[isoyear==1995]$age, na.rm=TRUE), '\n')

# Check filtering
cat('Rows before age filter:', nrow(skeleton), '\n')
skeleton <- skeleton[age >= 18 & age <= 90]
cat('Rows after age filter:', nrow(skeleton), '\n')

skeleton <- skeleton[!is.na(income)]
cat('Rows after income filter:', nrow(skeleton), '\n')

# Check events
cat('Heart attack events:', sum(skeleton$heart_attack, na.rm=TRUE), '\n')
cat('Stroke events:', sum(skeleton$stroke, na.rm=TRUE), '\n')
cat('Diabetes events:', sum(skeleton$diabetes, na.rm=TRUE), '\n')