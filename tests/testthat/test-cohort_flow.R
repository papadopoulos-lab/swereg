test_that(".build_cohort_flow assembles eligibility + matching + analysis as one ordered flow", {
  ec <- list(
    attrition = data.table::data.table(
      trial_id = NA_integer_,
      criterion = c("before_exclusions", "eligible_age", "eligible_no_x"),
      n_persons = c(1000, 800, 700),
      n_person_trials = c(5000, 4000, 3500),
      n_intervention = c(1000, 800, 700),
      n_comparator = c(4000, 3200, 2800)
    ),
    matching = data.table::data.table(
      trial_id = 1:2,
      n_intervention_enrolled = c(350, 350),
      n_comparator_enrolled = c(700, 700)
    )
  )

  flow <- swereg:::.build_cohort_flow(
    ec, analysis_n = 2050,
    analysis_n_intervention = 690, analysis_n_comparator = 1360
  )

  expect_equal(flow$step, c(
    "before_exclusions", "eligible_age", "eligible_no_x",
    "enrolled_after_matching", "analysis_dataset"
  ))
  expect_equal(flow$kind, c(
    "start", "exclusion", "exclusion", "selection", "analysis"
  ))
  # Remaining person-trials at each step.
  expect_equal(flow$n_person_trials, c(5000, 4000, 3500, 2100, 2050))
  # Matching keeps all intervention + sampled comparator person-trials.
  expect_equal(flow$n_intervention[flow$kind == "selection"], 700)
  expect_equal(flow$n_comparator[flow$kind == "selection"], 1400)
  # Per-arm analysis-set split is carried onto the analysis step.
  expect_equal(flow$n_intervention[flow$kind == "analysis"], 690)
  expect_equal(flow$n_comparator[flow$kind == "analysis"], 1360)
  # Matching/analysis are person-trial operations: n_persons is NA.
  expect_true(is.na(flow$n_persons[flow$kind == "selection"]))
  expect_true(is.na(flow$n_persons[flow$kind == "analysis"]))
  # Per-step reductions and their (correctly labelled) kind.
  expect_equal(flow$change_person_trials, c(NA, 1000, 500, 1400, 50))
  expect_equal(flow$change_kind, c(
    NA, "excluded", "excluded",
    "not selected (matching)", "censored (per-protocol)"
  ))
})

test_that(".build_consort_dot renders matching and analysis boxes from the flow", {
  ec <- list(
    attrition = data.table::data.table(
      trial_id = NA_integer_,
      criterion = c("before_exclusions", "eligible_age"),
      n_persons = c(1000, 800),
      n_person_trials = c(5000, 4000),
      n_intervention = c(1000, 800),
      n_comparator = c(4000, 3200)
    ),
    matching = data.table::data.table(
      trial_id = 1L,
      n_intervention_enrolled = 700,
      n_comparator_enrolled = 1400
    )
  )
  flow <- swereg:::.build_cohort_flow(
    ec, analysis_n = 2050,
    analysis_n_intervention = 690, analysis_n_comparator = 1360
  )
  dot <- swereg:::.build_consort_dot(
    flow = flow, eid = "M01", label = "Test enrollment",
    intervention_label = "MHT", comparator_label = "none"
  )

  expect_type(dot, "character")
  expect_true(grepl("Excluded", dot))
  expect_true(grepl("Enrolled after matching", dot))
  expect_true(grepl("2,100 person-trials", dot))
  expect_true(grepl("Analysis dataset \\(per-protocol\\)", dot))
  expect_true(grepl("2,050 person-trials", dot))
  # The analysis box must hang off the matched box, not the excluded box.
  expect_true(grepl("matched -> analysis", dot))
  # Per-arm split is rendered in the analysis box.
  expect_true(grepl("MHT: 690 person-trials", dot))
  expect_true(grepl("none: 1,360 person-trials", dot))
})

test_that(".build_cohort_flow omits analysis arm split when unavailable", {
  ec <- list(
    attrition = data.table::data.table(
      trial_id = NA_integer_,
      criterion = c("before_exclusions", "eligible_age"),
      n_persons = c(1000, 800),
      n_person_trials = c(5000, 4000),
      n_intervention = c(1000, 800),
      n_comparator = c(4000, 3200)
    ),
    matching = data.table::data.table(
      trial_id = 1L, n_intervention_enrolled = 700, n_comparator_enrolled = 1400
    )
  )
  flow <- swereg:::.build_cohort_flow(ec, analysis_n = 2050)
  expect_true(is.na(flow$n_intervention[flow$kind == "analysis"]))
  dot <- swereg:::.build_consort_dot(
    flow = flow, eid = "M01", label = "Test",
    intervention_label = "MHT", comparator_label = "none"
  )
  # Total still shown; no per-arm parenthetical after the analysis count.
  expect_true(grepl("2,050 person-trials", dot))
  expect_false(grepl("2,050 person-trials\\\\n\\(", dot))
})

test_that(".build_cohort_flow returns NULL without attrition", {
  expect_null(swereg:::.build_cohort_flow(NULL))
  expect_null(swereg:::.build_cohort_flow(list()))
})
