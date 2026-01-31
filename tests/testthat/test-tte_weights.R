# Tests for TTE weight functions

# =============================================================================
# tte_truncate_weights tests
# =============================================================================

test_that("tte_truncate_weights truncates at correct percentiles", {
  dt <- data.table::data.table(
    id = 1:100,
    weight = c(0.01, rep(1, 98), 100)  # Extreme values at ends
  )
  result <- tte_truncate_weights(dt, weight_cols = "weight")

  # Check new column exists

expect_true("weight_trunc" %in% names(result))

  # Check extreme values are truncated
  bounds <- quantile(dt$weight, c(0.01, 0.99))
  expect_true(min(result$weight_trunc) >= bounds[1])
  expect_true(max(result$weight_trunc) <= bounds[2])
})

test_that("tte_truncate_weights handles multiple columns", {
  dt <- data.table::data.table(
    id = 1:100,
    ipw = c(0.1, rep(1, 98), 10),
    ipcw = c(0.05, rep(1, 98), 20)
  )
  result <- tte_truncate_weights(dt, weight_cols = c("ipw", "ipcw"))

  expect_true("ipw_trunc" %in% names(result))
  expect_true("ipcw_trunc" %in% names(result))
})

test_that("tte_truncate_weights uses custom suffix", {
  dt <- data.table::data.table(
    id = 1:100,
    weight = runif(100, 0.5, 2)
  )
  result <- tte_truncate_weights(dt, weight_cols = "weight", suffix = "_winsor")

  expect_true("weight_winsor" %in% names(result))
})

test_that("tte_truncate_weights validates inputs", {
  dt <- data.table::data.table(id = 1:10, weight = 1:10)

  # Not a data.table
  expect_error(
    tte_truncate_weights(data.frame(id = 1:10), "weight"),
    "data must be a data.table"
  )

  # Missing column
  expect_error(
    tte_truncate_weights(dt, "nonexistent"),
    "Columns not found"
  )

  # Invalid quantiles
  expect_error(
    tte_truncate_weights(dt, "weight", lower = 0.5, upper = 0.3),
    "lower and upper"
  )
})

# =============================================================================
# tte_calculate_ipw tests
# =============================================================================

test_that("tte_calculate_ipw calculates propensity scores and IPW", {
  set.seed(42)
  dt <- data.table::data.table(
    trial_id = 1:1000,
    exposed = as.logical(rbinom(1000, 1, 0.3)),
    age_cat = factor(sample(1:4, 1000, replace = TRUE)),
    education = factor(sample(1:3, 1000, replace = TRUE))
  )

  result <- tte_calculate_ipw(
    data = dt,
    exposure_var = "exposed",
    confounder_vars = c("age_cat", "education"),
    id_var = "trial_id"
  )

  # Check columns exist
  expect_true("ps" %in% names(result))
  expect_true("ipw" %in% names(result))

  # Propensity scores should be between 0 and 1
  expect_true(all(result$ps > 0 & result$ps < 1))

  # IPW should be positive
  expect_true(all(result$ipw > 0))
})

test_that("tte_calculate_ipw produces stabilized weights summing near N", {
  set.seed(123)
  dt <- data.table::data.table(
    trial_id = 1:500,
    exposed = as.logical(rbinom(500, 1, 0.4)),
    confounder = factor(sample(1:3, 500, replace = TRUE))
  )

  result <- tte_calculate_ipw(
    data = dt,
    exposure_var = "exposed",
    confounder_vars = "confounder",
    stabilize = TRUE
  )

  # Stabilized weights should sum approximately to sample size
  weight_sum <- sum(result$ipw)
  expect_true(abs(weight_sum - nrow(dt)) / nrow(dt) < 0.1)  # Within 10%
})

test_that("tte_calculate_ipw validates inputs", {
  dt <- data.table::data.table(
    trial_id = 1:10,
    exposed = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    age = 1:10
  )

  # Missing exposure column
  expect_error(
    tte_calculate_ipw(dt, "nonexistent", "age"),
    "not found in data"
  )

  # Missing confounder column
  expect_error(
    tte_calculate_ipw(dt, "exposed", "nonexistent"),
    "not found in data"
  )
})

# =============================================================================
# tte_identify_censoring tests
# =============================================================================

test_that("tte_identify_censoring detects protocol deviation", {
  dt <- data.table::data.table(
    trial_id = rep(1:3, each = 4),
    tstart = rep(0:3, 3),
    tstop = rep(1:4, 3),
    baseline_exposed = rep(c(TRUE, TRUE, FALSE), each = 4),
    current_exposed = c(
      TRUE, TRUE, FALSE, FALSE,  # Person 1: deviates at week 3
      TRUE, TRUE, TRUE, TRUE,    # Person 2: adheres
      FALSE, FALSE, TRUE, TRUE   # Person 3: deviates at week 3
    )
  )

  result <- tte_identify_censoring(
    data = dt,
    exposure_var = "current_exposed",
    baseline_exposure_var = "baseline_exposed"
  )

  # Check columns exist
  expect_true("protocol_deviated" %in% names(result))
  expect_true("weeks_to_protocol_deviation" %in% names(result))
  expect_true("censored" %in% names(result))

  # Person 1 should deviate (stopped treatment)
  person1 <- result[trial_id == 1]
  expect_true(any(person1$protocol_deviated))
  expect_equal(unique(person1$weeks_to_protocol_deviation), 3L)

  # Person 2 should not deviate
  person2 <- result[trial_id == 2]
  expect_false(any(person2$protocol_deviated))
  expect_true(is.na(unique(person2$weeks_to_protocol_deviation)))

  # Person 3 should deviate (started treatment)
  person3 <- result[trial_id == 3]
  expect_true(any(person3$protocol_deviated))
  expect_equal(unique(person3$weeks_to_protocol_deviation), 3L)
})

test_that("tte_identify_censoring treats NA exposure as deviation", {
  dt <- data.table::data.table(
    trial_id = rep(1, 4),
    tstart = 0:3,
    tstop = 1:4,
    baseline_exposed = TRUE,
    current_exposed = c(TRUE, TRUE, NA, NA)  # Missing = deviation
  )

  result <- tte_identify_censoring(
    data = dt,
    exposure_var = "current_exposed",
    baseline_exposure_var = "baseline_exposed"
  )

  # NA exposure should be treated as deviation
  expect_true(result$protocol_deviated[3])
  expect_equal(unique(result$weeks_to_protocol_deviation), 3L)
})

# =============================================================================
# tte_combine_weights tests
# =============================================================================

test_that("tte_combine_weights multiplies IPW and IPCW", {
  dt <- data.table::data.table(
    id = 1:5,
    ipw = c(1, 2, 3, 4, 5),
    ipcw = c(2, 2, 2, 2, 2)
  )

  result <- tte_combine_weights(dt)

  expect_true("weight_pp" %in% names(result))
  expect_equal(result$weight_pp, c(2, 4, 6, 8, 10))
})

test_that("tte_combine_weights uses custom column names", {
  dt <- data.table::data.table(
    id = 1:3,
    my_ipw = c(1, 2, 3),
    my_ipcw = c(3, 2, 1)
  )

  result <- tte_combine_weights(
    dt,
    ipw_col = "my_ipw",
    ipcw_col = "my_ipcw",
    output_col = "combined"
  )

  expect_true("combined" %in% names(result))
  expect_equal(result$combined, c(3, 4, 3))
})

test_that("tte_combine_weights validates inputs", {
  dt <- data.table::data.table(id = 1:3, ipw = 1:3)

  expect_error(
    tte_combine_weights(dt),  # missing ipcw
    "not found in data"
  )
})

# =============================================================================
# tte_calculate_ipcw tests
# =============================================================================

test_that("tte_calculate_ipcw calculates time-varying weights", {
  # Create counting process data with censoring
  set.seed(42)
  n_trials <- 100
  n_periods <- 10

  dt <- data.table::data.table(
    trial_id = rep(1:n_trials, each = n_periods),
    tstop = rep(1:n_periods, n_trials),
    baseline_exposed = rep(
      as.logical(rbinom(n_trials, 1, 0.5)),
      each = n_periods
    ),
    age_cat = factor(rep(sample(1:3, n_trials, replace = TRUE), each = n_periods))
  )
  dt[, tstart := tstop - 1L]

  # Create some censoring events (low probability)
  dt[, censor := as.integer(runif(.N) < 0.02)]

  result <- tte_calculate_ipcw(
    data = dt,
    exposure_var = "baseline_exposed",
    censoring_var = "censor",
    confounder_vars = "age_cat",
    use_gam = FALSE  # Use GLM for faster testing
  )

  # Check all expected columns exist
  expect_true("p_censor" %in% names(result))
  expect_true("p_uncensored" %in% names(result))
  expect_true("cum_p_uncensored" %in% names(result))
  expect_true("ipcw" %in% names(result))

  # Probabilities should be between 0 and 1
  expect_true(all(result$p_censor >= 0 & result$p_censor <= 1, na.rm = TRUE))
  expect_true(all(result$p_uncensored >= 0 & result$p_uncensored <= 1,
                  na.rm = TRUE))

  # IPCW should be positive
  expect_true(all(result$ipcw > 0, na.rm = TRUE))

  # Cumulative probabilities should decrease over time
  first_trial <- result[trial_id == 1]
  expect_true(all(diff(first_trial$cum_p_uncensored) <= 0))
})

test_that("tte_calculate_ipcw validates inputs", {
  dt <- data.table::data.table(
    trial_id = 1:10,
    tstop = 1:10,
    exposed = TRUE,
    censor = 0L,
    confounder = factor(1)
  )

  # Missing required column
  expect_error(
    tte_calculate_ipcw(
      dt,
      exposure_var = "nonexistent",
      censoring_var = "censor",
      confounder_vars = "confounder"
    ),
    "not found in data"
  )
})

test_that("tte_calculate_ipcw works with single model", {
  set.seed(123)
  n_trials <- 50
  n_periods <- 5

  dt <- data.table::data.table(
    trial_id = rep(1:n_trials, each = n_periods),
    tstop = rep(1:n_periods, n_trials),
    baseline_exposed = rep(
      as.logical(rbinom(n_trials, 1, 0.5)),
      each = n_periods
    ),
    confounder = factor(rep(sample(1:2, n_trials, replace = TRUE), each = n_periods))
  )
  dt[, censor := as.integer(runif(.N) < 0.03)]

  result <- tte_calculate_ipcw(
    data = dt,
    exposure_var = "baseline_exposed",
    censoring_var = "censor",
    confounder_vars = "confounder",
    separate_by_exposure = FALSE,
    use_gam = FALSE
  )

  expect_true("ipcw" %in% names(result))
  expect_true(all(result$ipcw > 0, na.rm = TRUE))
})
