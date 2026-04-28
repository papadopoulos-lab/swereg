# Pin the window-string -> weeks conversion that drives every
# rolling-window exclusion (e.g. "Antipsychotic medication ATC N05A in
# the past 52 weeks"). The same converter feeds confounders. A bug
# here either widens or narrows the lookback uniformly across the
# pipeline -- silent and high-impact.
#
# Contract from `.convert_window()`:
#   - "lifetime_before_baseline"           -> Inf
#   - numeric (weeks)                      -> as.integer(weeks)
#   - "N year" / "N years" (legacy string) -> 52 * N
#   - anything else -> stop()

test_that(".convert_window: 'lifetime_before_baseline' -> Inf", {
  expect_identical(swereg:::.convert_window("lifetime_before_baseline"), Inf)
})

test_that(".convert_window: numeric weeks pass through as integer", {
  expect_identical(swereg:::.convert_window(52), 52L)
  expect_identical(swereg:::.convert_window(104), 104L)
  expect_identical(swereg:::.convert_window(0), 0L)
})

test_that(".convert_window: 'N year(s)' legacy strings parse to 52*N weeks", {
  expect_identical(swereg:::.convert_window("1 year"), 52L)
  expect_identical(swereg:::.convert_window("2 years"), 104L)
  expect_identical(swereg:::.convert_window("5 years"), 260L)
})

test_that(".convert_window: unrecognised string errors clearly", {
  expect_error(swereg:::.convert_window("yesterday"),
               regexp = "Cannot parse window")
  expect_error(swereg:::.convert_window("52 weeks"),
               regexp = "Cannot parse window")
})

test_that("tteplan_read_spec: numeric and 'lifetime_before_baseline' windows survive parse", {
  yaml_txt <- '
study:
  title: "T"
  principal_investigator: "PI"
  description: "d"
  implementation:
    project_prefix: "p"
    version: "v001"
inclusion_criteria:
  isoyears: [2010, 2020]
exclusion_criteria:
  - name: "Lifetime exclusion"
    implementation:
      source_variable: osd_x
      window: "lifetime_before_baseline"
      computed: true
  - name: "52-week exclusion"
    implementation:
      source_variable: rx_y
      window: 52
      computed: true
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
    additional_inclusion: []
    treatment:
      arms: { intervention: "I", comparator: "C" }
      implementation:
        matching_ratio: 1
        variable: rd_tx
        intervention_value: a
        comparator_value: b
        seed: 1
'
  tmp <- tempfile(fileext = ".yaml")
  writeLines(yaml_txt, tmp)
  on.exit(unlink(tmp), add = TRUE)

  spec <- swereg::tteplan_read_spec(tmp)
  expect_identical(
    spec$exclusion_criteria[[1]]$implementation$window_weeks,
    Inf
  )
  expect_identical(
    spec$exclusion_criteria[[2]]$implementation$window_weeks,
    52L
  )
})

test_that("tteplan_read_spec: 'lifetime_before_and_after_baseline' is person-level (no window_weeks)", {
  # Per the spec contract, this sentinel disables the rolling-window
  # logic entirely. The reader should not attach a window_weeks field
  # for this branch.
  yaml_txt <- '
study:
  title: "T"
  principal_investigator: "PI"
  description: "d"
  implementation:
    project_prefix: "p"
    version: "v001"
inclusion_criteria:
  isoyears: [2010, 2020]
exclusion_criteria:
  - name: "Person-level exclusion"
    implementation:
      source_variable: osd_x
      window: "lifetime_before_and_after_baseline"
      computed: true
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
    additional_inclusion: []
    treatment:
      arms: { intervention: "I", comparator: "C" }
      implementation:
        matching_ratio: 1
        variable: rd_tx
        intervention_value: a
        comparator_value: b
        seed: 1
'
  tmp <- tempfile(fileext = ".yaml")
  writeLines(yaml_txt, tmp)
  on.exit(unlink(tmp), add = TRUE)

  spec <- swereg::tteplan_read_spec(tmp)
  ec <- spec$exclusion_criteria[[1]]$implementation
  expect_identical(ec$window, "lifetime_before_and_after_baseline")
  expect_null(ec$window_weeks,
              info = "lifetime_before_and_after_baseline is person-level; window_weeks must NOT be set")
})
