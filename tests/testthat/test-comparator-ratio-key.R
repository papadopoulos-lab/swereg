# The spec key `treatment.implementation.matching_ratio` is retired.
#
# swereg draws comparators at random within the entry band and reads no
# covariate, so the old key named a design swereg does not run. This release
# renames it to `comparator_to_intervention_ratio` and makes the break hard.
#
# A silent fallback is the failure this pins. A spec that still carries the old
# key would parse. The ratio would reach the draw. Every generated methods
# paragraph would keep the covariate-matching claim. `tteplan_read_spec()`
# therefore stops, and the message names the new key so the reader can act.

skip_if_not_installed("yaml")

.crk_spec_yaml <- function(ratio_key) {
  paste0(
    'study:
  title: "T"
  principal_investigator: "PI"
  description: "d"
  implementation:
    project_prefix: "p"
    version: "v001"
inclusion_criteria:
  isoyears: [2010, 2020]
exclusion_criteria: []
confounders: []
outcomes:
  - name: "Outcome"
    implementation:
      variable: osd_z
follow_up:
  - { label: "1 year", weeks: 52 }
enrollments:
  - id: "01"
    name: "E01"
    observed_var:
      sentinel: row_presence
    intervention_tolerance_weeks: 0
    comparator_tolerance_weeks: 0
    additional_inclusion: []
    treatment:
      arms: { intervention: "I", comparator: "C" }
      implementation:
        ',
    ratio_key,
    ': 3
        variable: rd_tx
        intervention_value: a
        comparator_value: b
        seed: 1
'
  )
}

.crk_write <- function(ratio_key) {
  f <- tempfile(fileext = ".yaml")
  writeLines(.crk_spec_yaml(ratio_key), f)
  f
}


test_that("tteplan_read_spec refuses the retired matching_ratio key", {
  f <- .crk_write("matching_ratio")
  on.exit(unlink(f), add = TRUE)

  expect_error(
    swereg::tteplan_read_spec(f),
    "comparator_to_intervention_ratio",
    fixed = TRUE
  )
  # The message must also name the key it found, or the reader cannot locate
  # it in the file.
  expect_error(
    swereg::tteplan_read_spec(f),
    "matching_ratio",
    fixed = TRUE
  )
})


test_that("tteplan_read_spec parses comparator_to_intervention_ratio", {
  f <- .crk_write("comparator_to_intervention_ratio")
  on.exit(unlink(f), add = TRUE)

  spec <- swereg::tteplan_read_spec(f)
  impl <- spec$enrollments[[1]]$treatment$implementation
  expect_identical(impl[["comparator_to_intervention_ratio"]], 3L)
  expect_null(impl[["matching_ratio"]])
})


test_that("a spec that carries neither ratio key is refused by name", {
  f <- .crk_write("some_other_key")
  on.exit(unlink(f), add = TRUE)

  expect_error(
    swereg::tteplan_read_spec(f),
    "is missing treatment$implementation$comparator_to_intervention_ratio",
    fixed = TRUE
  )
})
