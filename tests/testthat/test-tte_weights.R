# Tests for TTE weight/matching/collapse on TTEEnrollment
# Tests target public methods ($s2_ipw, $s3_truncate_weights,
# $s4_prepare_for_analysis) and the one remaining private method
# (.truncate_weights).

# Shared private env for .truncate_weights tests
local_priv <- local({
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10, exposed = rep(c(TRUE, FALSE), 5),
    event = 0L, age = 50
  )
  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "event",
    confounder_vars = "age",
    follow_up_time = 10L
  )
  TTEEnrollment$new(dt, design)$.__enclos_env__$private
})

# =============================================================================
# .truncate_weights tests (still a private method — used in 2 places)
# =============================================================================

test_that(".truncate_weights truncates at correct percentiles", {
  dt <- data.table::data.table(
    id = 1:100,
    weight = c(0.01, rep(1, 98), 100)
  )
  result <- local_priv$.truncate_weights(dt, weight_cols = "weight")

  expect_true("weight_trunc" %in% names(result))

  bounds <- quantile(dt$weight, c(0.01, 0.99))
  expect_true(min(result$weight_trunc) >= bounds[1])
  expect_true(max(result$weight_trunc) <= bounds[2])
})

test_that(".truncate_weights handles multiple columns", {
  dt <- data.table::data.table(
    id = 1:100,
    ipw = c(0.1, rep(1, 98), 10),
    ipcw = c(0.05, rep(1, 98), 20)
  )
  result <- local_priv$.truncate_weights(dt, weight_cols = c("ipw", "ipcw"))

  expect_true("ipw_trunc" %in% names(result))
  expect_true("ipcw_trunc" %in% names(result))
})

test_that(".truncate_weights uses custom suffix", {
  dt <- data.table::data.table(
    id = 1:100,
    weight = runif(100, 0.5, 2)
  )
  result <- local_priv$.truncate_weights(dt, weight_cols = "weight", suffix = "_winsor")

  expect_true("weight_winsor" %in% names(result))
})

test_that(".truncate_weights validates inputs", {
  dt <- data.table::data.table(id = 1:10, weight = 1:10)

  expect_error(
    local_priv$.truncate_weights(data.frame(id = 1:10), "weight"),
    "data must be a data.table"
  )

  expect_error(
    local_priv$.truncate_weights(dt, "nonexistent"),
    "Columns not found"
  )

  expect_error(
    local_priv$.truncate_weights(dt, "weight", lower = 0.5, upper = 0.3),
    "lower and upper"
  )
})

# =============================================================================
# $s2_ipw() tests (was .calculate_ipw)
# =============================================================================

test_that("$s2_ipw() calculates propensity scores and IPW", {
  set.seed(42)
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:1000,
    exposed = as.logical(rbinom(1000, 1, 0.3)),
    age_cat = factor(sample(1:4, 1000, replace = TRUE)),
    education = factor(sample(1:3, 1000, replace = TRUE)),
    event = 0L,
    tstart = 0L,
    tstop = 1L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "event",
    confounder_vars = c("age_cat", "education"),
    follow_up_time = 10L
  )
  trial <- TTEEnrollment$new(dt, design)
  trial$s2_ipw()

  expect_true("ps" %in% names(trial$data))
  expect_true("ipw" %in% names(trial$data))
  baseline <- trial$data[tstart == 0]
  expect_true(all(baseline$ps > 0 & baseline$ps < 1))
  expect_true(all(baseline$ipw > 0))
  expect_true("ipw" %in% trial$steps_completed)
})

test_that("$s2_ipw() produces stabilized weights summing near N", {
  set.seed(123)
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:500,
    exposed = as.logical(rbinom(500, 1, 0.4)),
    confounder = factor(sample(1:3, 500, replace = TRUE)),
    event = 0L,
    tstart = 0L,
    tstop = 1L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "event",
    confounder_vars = "confounder",
    follow_up_time = 10L
  )
  trial <- TTEEnrollment$new(dt, design)
  trial$s2_ipw(stabilize = TRUE)

  weight_sum <- sum(trial$data$ipw, na.rm = TRUE)
  expect_true(abs(weight_sum - nrow(dt)) / nrow(dt) < 0.1)
})

# =============================================================================
# $s3_truncate_weights() tests (public wrapper around .truncate_weights)
# =============================================================================

test_that("$s3_truncate_weights() truncates weight columns and tracks state", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:100,
    exposed = rep(c(TRUE, FALSE), 50),
    event = 0L,
    ipw = c(0.01, rep(1, 98), 100),
    tstart = 0L,
    tstop = 1L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "event",
    confounder_vars = character(0),
    follow_up_time = 1L
  )
  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "ipw"
  trial$s3_truncate_weights()

  expect_true("ipw_trunc" %in% names(trial$data))
  expect_true("ipw_trunc" %in% trial$weight_cols)
  expect_true("truncate" %in% trial$steps_completed)
})

# Note: Enrollment matching (was .match_ratio) is tested through
# TTEEnrollment$new(ratio=) in test-tte_classes.R
