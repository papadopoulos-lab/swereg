library(data.table)
data('fake_person_ids', package = 'swereg')
data('fake_demographics', package = 'swereg')
swereg::make_lowercase_names(fake_demographics)

# Check what happens with isoyear rows
study_ids <- fake_person_ids[1:500]
skeleton <- swereg::create_skeleton(study_ids, '1995-01-01', '2005-12-31')
demographics_subset <- fake_demographics[lopnr %in% study_ids]
swereg::add_onetime(skeleton, demographics_subset, id_name = 'lopnr')

# Check before filtering
cat('Before filtering:\n')
cat('Total rows:', nrow(skeleton), '\n')
cat('Isoyear rows:', sum(skeleton$is_isoyear), '\n')
cat('Weekly rows:', sum(!skeleton$is_isoyear), '\n')

# Add birth year and age
skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
skeleton[, age := isoyear - birth_year]

# Check age distribution for isoyear rows only
isoyear_rows <- skeleton[is_isoyear == TRUE]
cat('Age range for isoyear rows:', min(isoyear_rows$age, na.rm=TRUE), 'to', max(isoyear_rows$age, na.rm=TRUE), '\n')

# Check if isoyear rows have negative ages
negative_age_isoyear <- isoyear_rows[age < 18]
cat('Isoyear rows with age < 18:', nrow(negative_age_isoyear), '\n')

# Filter and check
skeleton <- skeleton[age >= 18 & age <= 90]
cat('After age filter:\n')
cat('Total rows:', nrow(skeleton), '\n')
cat('Isoyear rows:', sum(skeleton$is_isoyear), '\n')

# The problem is that the age filter is removing ALL isoyear rows
# Let's see what happens if we check the age at the person level
cat('The issue is that we are filtering at the row level, but age changes over time\n')
cat('Some people might have negative age in early years but positive in later years\n')