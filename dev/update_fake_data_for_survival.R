# Script to update fake datasets with more heart attacks and cardiovascular events
# for the survival analysis cookbook

library(data.table)
library(lubridate)

# Load existing datasets
load('data/fake_inpatient_diagnoses.rda')
load('data/fake_outpatient_diagnoses.rda')
load('data/fake_person_ids.rda')
load('data/fake_demographics.rda')

# Create working copies with original structure
inpatient_orig <- copy(fake_inpatient_diagnoses)
outpatient_orig <- copy(fake_outpatient_diagnoses)
demographics_orig <- copy(fake_demographics)

# Apply make_lowercase_names for analysis
swereg::make_lowercase_names(inpatient_orig)
swereg::make_lowercase_names(outpatient_orig)
swereg::make_lowercase_names(demographics_orig)

# Combine diagnoses for analysis
combined_diag <- rbindlist(list(inpatient_orig, outpatient_orig), use.names=TRUE, fill=TRUE)

# Check current state
cat("Original dataset:\n")
cat("Total diagnoses:", nrow(combined_diag), "\n")
cat("Heart attacks (I21,I22):", sum(grepl('^I21|^I22', combined_diag$hdia)), "\n")
cat("Strokes (I63,I64):", sum(grepl('^I63|^I64', combined_diag$hdia)), "\n")
cat("Diabetes (E10,E11):", sum(grepl('^E10|^E11', combined_diag$hdia)), "\n")

# Set seed for reproducibility
set.seed(42)

# Create age data for risk-based assignment
demographics_orig[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
demographics_orig[, age_2020 := 2020 - birth_year]

# Sample individuals for different conditions based on realistic risk factors
# Higher risk for older individuals

# Heart attacks - about 2% of population over 6 years (realistic rate)
heart_attack_candidates <- demographics_orig[age_2020 >= 45]  # Focus on older adults
n_heart_attacks <- min(50, round(nrow(heart_attack_candidates) * 0.05))  # 5% of older adults
heart_attack_ids <- sample(heart_attack_candidates$lopnr, n_heart_attacks)

# Strokes - about 1.5% of population
stroke_candidates <- demographics_orig[age_2020 >= 50]
n_strokes <- min(30, round(nrow(stroke_candidates) * 0.04))
stroke_ids <- sample(stroke_candidates$lopnr, n_strokes)

# Diabetes - about 8% of population (already some in data, add more)
diabetes_candidates <- demographics_orig[age_2020 >= 30]
n_diabetes <- min(80, round(nrow(diabetes_candidates) * 0.08))
diabetes_ids <- sample(diabetes_candidates$lopnr, n_diabetes)

# Create new inpatient diagnoses (heart attacks and strokes)
new_inpatient <- data.table()

# Add heart attacks (I21, I22) to inpatient
for(i in seq_along(heart_attack_ids)) {
  # Random date between 2015-2020 for survival analysis
  random_date <- sample(seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by="day"), 1)
  
  new_row <- data.table(
    lopnr = heart_attack_ids[i],
    ar = year(random_date),
    indatuma = format(random_date, "%Y%m%d"),
    indatum = random_date,
    hdia = sample(c("I21", "I22"), 1),  # Acute MI codes
    utdatuma = format(random_date + days(sample(3:14, 1)), "%Y%m%d"),
    utdatum = random_date + days(sample(3:14, 1))
  )
  
  # Add all other columns as NA to match inpatient structure
  for(col in names(inpatient_orig)) {
    if(!col %in% names(new_row)) {
      new_row[[col]] <- NA_character_
    }
  }
  
  new_inpatient <- rbind(new_inpatient, new_row)
}

# Add strokes (I63, I64) to inpatient
for(i in seq_along(stroke_ids)) {
  random_date <- sample(seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by="day"), 1)
  
  new_row <- data.table(
    lopnr = stroke_ids[i],
    ar = year(random_date),
    indatuma = format(random_date, "%Y%m%d"),
    indatum = random_date,
    hdia = sample(c("I63", "I64"), 1),  # Stroke codes
    utdatuma = format(random_date + days(sample(5:21, 1)), "%Y%m%d"),
    utdatum = random_date + days(sample(5:21, 1))
  )
  
  # Add all other columns as NA to match inpatient structure
  for(col in names(inpatient_orig)) {
    if(!col %in% names(new_row)) {
      new_row[[col]] <- NA_character_
    }
  }
  
  new_inpatient <- rbind(new_inpatient, new_row)
}

# Create new outpatient diagnoses (diabetes)
new_outpatient <- data.table()

# Add diabetes (E10, E11) to outpatient
for(i in seq_along(diabetes_ids)) {
  random_date <- sample(seq(as.Date("2015-01-01"), as.Date("2019-12-31"), by="day"), 1)
  
  new_row <- data.table(
    lopnr = diabetes_ids[i],
    ar = year(random_date),
    indatuma = format(random_date, "%Y%m%d"),
    indatum = random_date,
    hdia = sample(c("E10", "E11"), 1)  # Diabetes codes
  )
  
  # Add all other columns as NA to match outpatient structure
  for(col in names(outpatient_orig)) {
    if(!col %in% names(new_row)) {
      new_row[[col]] <- NA_character_
    }
  }
  
  new_outpatient <- rbind(new_outpatient, new_row)
}

# Set column order to match original
if(nrow(new_inpatient) > 0) {
  setcolorder(new_inpatient, names(inpatient_orig))
}
if(nrow(new_outpatient) > 0) {
  setcolorder(new_outpatient, names(outpatient_orig))
}

# Combine with original data
enhanced_inpatient <- rbind(inpatient_orig, new_inpatient)
enhanced_outpatient <- rbind(outpatient_orig, new_outpatient)

# Convert back to original column names (reverse make_lowercase_names)
names(enhanced_inpatient) <- names(fake_inpatient_diagnoses)
names(enhanced_outpatient) <- names(fake_outpatient_diagnoses)

# Set back to original structure
fake_inpatient_diagnoses <- enhanced_inpatient
fake_outpatient_diagnoses <- enhanced_outpatient

# Check results
test_inpatient <- copy(fake_inpatient_diagnoses)
test_outpatient <- copy(fake_outpatient_diagnoses)
swereg::make_lowercase_names(test_inpatient)
swereg::make_lowercase_names(test_outpatient)
combined_check <- rbindlist(list(test_inpatient, test_outpatient), use.names=TRUE, fill=TRUE)

cat("\nEnhanced dataset:\n")
cat("Total diagnoses:", nrow(combined_check), "\n")
cat("Heart attacks (I21,I22):", sum(grepl('^I21|^I22', combined_check$hdia)), "\n")
cat("Strokes (I63,I64):", sum(grepl('^I63|^I64', combined_check$hdia)), "\n")
cat("Diabetes (E10,E11):", sum(grepl('^E10|^E11', combined_check$hdia)), "\n")

# Save the datasets
save(fake_inpatient_diagnoses, file = "data/fake_inpatient_diagnoses.rda")
save(fake_outpatient_diagnoses, file = "data/fake_outpatient_diagnoses.rda")

cat("\nDatasets saved successfully!\n")