# The counts of each exclusion line of the CONSORT side box.
#
# A step removes person-trials. It removes a person only when it removes her
# last person-trial. So each line gives the person-trials first, then the
# persons the step excluded entirely. Project 011 printed a step that removed
# 48,122,520 person-trials and 1 person under the old wording "n = 1 persons /
# 48,122,520 person-trials", which reads as if the step concerned one person.

fmt <- function(x) format(x, big.mark = ",")

test_that("an exclusion line gives person-trials, then persons excluded entirely", {
  expect_identical(
    swereg:::.consort_drop_counts(1, 48122520, fmt),
    "48,122,520 person-trials; 1 person excluded entirely"
  )
  expect_identical(
    swereg:::.consort_drop_counts(9, 1, fmt),
    "1 person-trial; 9 persons excluded entirely"
  )
  expect_identical(
    swereg:::.consort_drop_counts(0, 2, fmt),
    "2 person-trials; 0 persons excluded entirely"
  )
  expect_identical(swereg:::.consort_drop_counts(NA, 2, fmt), "2 person-trials")
})

test_that("the side box uses the wording on the total, the headings and the bullets", {
  elig <- data.table::data.table(
    step = c("before_exclusions", "eligible_valid_treatment", "eligible_age"),
    n_persons = c(100, 99, 90),
    n_person_trials = c(48123620, 1100, 1000),
    change_persons = c(NA, 1, 9),
    change_person_trials = c(NA, 48122520, 100)
  )
  lab <- swereg:::.consort_excluded_label(
    elig,
    labels_inline = function(s) {
      return(c(
        eligible_valid_treatment = "No week in either treatment arm",
        eligible_age = "Outside of age range"
      )[[s]])
    },
    inclusion_steps = "eligible_age",
    fmt = fmt
  )
  lines <- strsplit(lab, "\\l", fixed = TRUE)[[1]]
  expect_identical(
    lines,
    c(
      "Excluded (n = 48,122,620 person-trials; 10 persons excluded entirely):",
      "Not meeting inclusion criteria (n = 100 person-trials; 9 persons excluded entirely):",
      "- Outside of age range (n = 100 person-trials; 9 persons excluded entirely)",
      "Other reasons (n = 48,122,520 person-trials; 1 person excluded entirely):",
      "- No week in either treatment arm (n = 48,122,520 person-trials; 1 person excluded entirely)"
    )
  )
  expect_false(any(grepl("\\b1 persons\\b|\\b1 person-trials\\b", lines)))
})
