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
    intervention_label = "Drug X", comparator_label = "none"
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
  expect_true(grepl("Drug X: 690 person-trials", dot))
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
    intervention_label = "Drug X", comparator_label = "none"
  )
  # Total still shown; no per-arm parenthetical after the analysis count.
  expect_true(grepl("2,050 person-trials", dot))
  expect_false(grepl("2,050 person-trials\\\\n\\(", dot))
})

test_that(".build_cohort_flow returns NULL without attrition", {
  expect_null(swereg:::.build_cohort_flow(NULL))
  expect_null(swereg:::.build_cohort_flow(list()))
})


# =============================================================================
# `.attrition_overall()` reads the global rows, or it reads nothing
# =============================================================================
# A stored attrition table holds per-trial rows (`trial_id` is the trial index)
# and, since the global-row change, one global row per criterion (`trial_id` is
# NA). The two sets describe the SAME people, so a sum over both counts every
# person of a criterion twice.
#
# The fixture below is the legacy shape: `before_exclusions` and `age` carry a
# global row, `prior_disease` does not. No source set reports one unit for that
# input. Reading the global rows would put a `uniqueN` person count one row away
# from nothing. Reading the per-trial rows would put a sum over trials one row
# away from a `uniqueN` count. The second is what a per-trial fallback did until
# this file changed, and it made the CONSORT delta between the two rows
# negative. So a mixed table now yields NULL, and the enrollment gets no
# attrition sheet and no CONSORT diagram.

.attrition_mixed <- function() {
  data.table::data.table(
    trial_id = c(1L, 2L, NA, 1L, 2L, NA, 1L, 2L),
    criterion = c(
      "before_exclusions", "before_exclusions", "before_exclusions",
      "age", "age", "age",
      "prior_disease", "prior_disease"
    ),
    n_persons = c(1500, 1600, 2600, 1300, 1350, 2100, 900, 950),
    n_person_trials = c(
      12000, 12000, 24000, 10000, 10000, 20000, 7000, 7000
    ),
    n_intervention = c(250, 250, 500, 220, 220, 440, 200, 200),
    n_comparator = c(1000, 1000, 2000, 800, 800, 1600, 550, 550)
  )
}

test_that(".attrition_overall returns NULL when one criterion has no global row", {
  att <- .attrition_mixed()
  # The fixture is well formed and holds eight rows, so the NULL below reports
  # the mixed shape and not an empty table.
  expect_equal(nrow(att), 8L)
  expect_true(any(is.na(att$trial_id)))
  expect_equal(sum(is.na(att$trial_id[att$criterion == "prior_disease"])), 0L)

  expect_null(swereg:::.attrition_overall(att))
})

test_that(".attrition_overall reads the global rows when every criterion has one", {
  att <- .attrition_mixed()
  # Give `prior_disease` the global row it lacks, and every criterion then has
  # one. The result MUST be the global rows themselves, per-trial rows ignored.
  att <- rbind(
    att,
    data.table::data.table(
      trial_id = NA_integer_,
      criterion = "prior_disease",
      n_persons = 1500,
      n_person_trials = 13000,
      n_intervention = 380,
      n_comparator = 1050
    )
  )
  overall <- swereg:::.attrition_overall(att)

  expect_equal(
    overall$criterion,
    c("before_exclusions", "age", "prior_disease")
  )
  expect_equal(overall$n_persons, c(2600, 2100, 1500))
  expect_equal(overall$n_person_trials, c(24000, 20000, 13000))
  expect_equal(overall$n_intervention, c(500, 440, 380))
  expect_equal(overall$n_comparator, c(2000, 1600, 1050))
})

test_that("a mixed attrition table builds no cohort flow and no CONSORT diagram", {
  flow <- swereg:::.build_cohort_flow(list(attrition = .attrition_mixed()))
  expect_null(flow)

  # The renderer refuses the same input, so no diagram carries a per-trial sum
  # under a "persons" heading.
  dot <- swereg:::.build_consort_dot(
    flow = flow, eid = "M02", label = "Test enrollment",
    intervention_label = "Drug X", comparator_label = "none"
  )
  expect_null(dot)

  # The same table with a global row for every criterion still builds a flow.
  # The NULL above therefore reports the mixed shape, and not the fixture.
  att <- rbind(
    .attrition_mixed(),
    data.table::data.table(
      trial_id = NA_integer_,
      criterion = "prior_disease",
      n_persons = 1500,
      n_person_trials = 13000,
      n_intervention = 380,
      n_comparator = 1050
    )
  )
  complete <- swereg:::.build_cohort_flow(list(attrition = att))
  expect_equal(complete$n_persons, c(2600, 2100, 1500))
  expect_equal(complete$n_person_trials, c(24000, 20000, 13000))
})
