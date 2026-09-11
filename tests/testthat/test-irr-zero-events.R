# The whole-panel IRR path preflights the event counts by arm, the same way
# `$irr_by_subgroup()` does for one stratum. Without it, zero events in one
# arm separates the weighted Poisson fit and returns a very large ratio with a
# confidence interval, and zero events in both arms returns a ratio near 1.
# Neither is an estimate.

# Two follow-up rows per person-trial. Every event sits on the second row, so
# the fit sees person-time in both rows and events in one.
.ize_trial <- function(n_ev_intervention, n_ev_comparator, n_per_arm = 20L) {
  n <- 2L * n_per_arm
  ids <- sprintf("t%03d", seq_len(n))
  exposed <- rep(c(TRUE, FALSE), each = n_per_arm)
  d <- data.table::data.table(
    enrollment_person_trial_id = rep(ids, each = 2L),
    id = rep(sprintf("p%03d", seq_len(n)), each = 2L),
    tstart = rep(c(0L, 4L), n),
    tstop = rep(c(4L, 8L), n),
    exposed = rep(exposed, each = 2L),
    person_weeks = 4L,
    ipw = 1,
    event = 0L
  )
  hit <- c(
    utils::head(ids[exposed], n_ev_intervention),
    utils::head(ids[!exposed], n_ev_comparator)
  )
  data.table::set(
    d,
    i = which(d$enrollment_person_trial_id %in% hit & d$tstop == 8L),
    j = "event",
    value = 1L
  )
  design <- TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    outcome_vars = "event",
    confounder_vars = character(0),
    follow_up_time = 8L
  )
  return(TTEEnrollment$new(d, design))
}


test_that("irr returns the NA row and warns when the comparator arm has no events", {
  trial <- .ize_trial(5L, 0L)
  r <- NULL
  expect_warning(
    r <- trial$irr("ipw"),
    "no events in one or both treatment arms"
  )
  expect_true(is.na(r$IRR))
  expect_true(is.na(r$IRR_lower))
  expect_true(is.na(r$IRR_upper))
  expect_true(is.na(r$IRR_pvalue))
  expect_true(r$warn)
  expect_identical(attr(r, "swereg_type"), "irr")
})

test_that("irr returns the NA row and warns when neither arm has any event", {
  trial <- .ize_trial(0L, 0L)
  r <- NULL
  expect_warning(
    r <- trial$irr("ipw"),
    "no events in one or both treatment arms"
  )
  expect_true(is.na(r$IRR))
  expect_true(r$warn)
})

test_that("irr fits when each arm holds at least one event", {
  trial <- .ize_trial(5L, 1L)
  r <- expect_no_warning(trial$irr("ipw"))
  expect_false(is.na(r$IRR))
  expect_gt(r$IRR, 0)
  expect_false(is.na(r$IRR_lower))
})


# The estimability rule reads the per-arm event counts, not the ratio alone.
#
# Before 26.10.17 an all-zero outcome passed the rule with a ratio of 1.04. The
# ratio is a symptom of an empty arm and not the fact itself, so a future change
# to the fit could produce a finite ratio for an arm that held no event. The
# counts are the fact, and `$irr()` now carries them on the row.

test_that("estimability is FALSE for an empty arm even when the ratio is finite", {
  # 1.04 passes the ratio rule on its own. Zero events in the intervention arm
  # is what makes it inestimable.
  expect_false(swereg:::.tte_irr_estimable(1.04, 0, 5))
  expect_false(swereg:::.tte_irr_estimable(1.04, 5, 0))
  expect_true(swereg:::.tte_irr_estimable(1.04, 5, 5))
})

test_that("estimability falls back to the ratio rule when a count is NA", {
  # A result stored before the counts existed. The rule must still answer.
  expect_true(swereg:::.tte_irr_estimable(1.04))
  expect_true(swereg:::.tte_irr_estimable(1.04, NA_real_, 5))
  expect_false(swereg:::.tte_irr_estimable(0))
  expect_false(swereg:::.tte_irr_estimable(NA_real_, 5, 5))
})

test_that("the NA row from a zero-event fixture carries both arm counts", {
  trial <- .ize_trial(0L, 5L)
  r <- NULL
  expect_warning(
    r <- trial$irr("ipw"),
    "no events in one or both treatment arms"
  )
  expect_identical(r$events_intervention, 0)
  expect_identical(r$events_comparator, 5)
})

test_that("a fitted row carries the events each arm actually held", {
  trial <- .ize_trial(5L, 1L)
  r <- expect_no_warning(trial$irr("ipw"))
  expect_identical(r$events_intervention, 5)
  expect_identical(r$events_comparator, 1)
})

test_that("the stored flag is FALSE for an empty arm with a forced finite ratio", {
  # The producer's path: `.s3_mark_irr_estimable()` is what `$s3_analyze()`
  # calls. Force the ratio finite so only the counts can decide.
  trial <- .ize_trial(0L, 5L)
  r <- NULL
  expect_warning(r <- trial$irr("ipw"), "no events")
  data.table::set(r, j = "IRR", value = 1.04)

  marked <- swereg:::.s3_mark_irr_estimable(r)
  expect_false(marked$irr_estimable)
})
