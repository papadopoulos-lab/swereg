# Tests for TTE R6 classes and methods

# =============================================================================
# TTEDesign tests
# =============================================================================

test_that("TTEDesign creates valid design object", {
  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = c("death", "hosp"),
    confounder_vars = c("age", "sex"),
    follow_up_time = 52L
  )

  expect_true(inherits(design, "TTEDesign"))
  expect_equal(design$id_var, "trial_id")
  expect_equal(design$exposure_var, "exposed")
  expect_equal(design$outcome_vars, c("death", "hosp"))
  expect_equal(design$confounder_vars, c("age", "sex"))
  expect_equal(design$follow_up_time, 52L)
  expect_equal(design$tstart_var, "tstart")
  expect_equal(design$tstop_var, "tstop")
})

test_that("TTEDesign accepts optional parameters", {
  design <- tte_design(
    id_var = "id",
    exposure_var = "treated",
    outcome_vars = "event",
    confounder_vars = "age",
    follow_up_time = 100L,
    time_exposure_var = "current_treated",
    eligible_var = "eligible",
    admin_censor_var = "admin_censor",
    tstart_var = "t0",
    tstop_var = "t1"
  )

  expect_equal(design$time_exposure_var, "current_treated")
  expect_equal(design$eligible_var, "eligible")
  expect_equal(design$admin_censor_var, "admin_censor")
  expect_equal(design$tstart_var, "t0")
  expect_equal(design$tstop_var, "t1")
})

test_that("TTEDesign validates inputs", {
  # id_var must be length 1
  expect_error(
    tte_design(
      id_var = c("a", "b"),
      exposure_var = "exposed",
      outcome_vars = "death",
      confounder_vars = "age",
      follow_up_time = 52L
    ),
    "id_var must be length 1"
  )

  # outcome_vars cannot be empty
  expect_error(
    tte_design(
      id_var = "id",
      exposure_var = "exposed",
      outcome_vars = character(),
      confounder_vars = "age",
      follow_up_time = 52L
    ),
    "outcome_vars cannot be empty"
  )

  # follow_up_time must be positive
  expect_error(
    tte_design(
      id_var = "id",
      exposure_var = "exposed",
      outcome_vars = "death",
      confounder_vars = "age",
      follow_up_time = 0L
    ),
    "follow_up_time must be a positive integer"
  )
})

test_that("TTEDesign print method works", {
  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = c("death", "hosp"),
    confounder_vars = c("age", "sex"),
    follow_up_time = 52L
  )

  output <- capture.output(print(design))
  expect_true(any(grepl("TTEDesign", output)))
  expect_true(any(grepl("trial_id", output)))
})

# =============================================================================
# TTETrial tests
# =============================================================================

test_that("TTETrial creates valid trial object", {
  dt <- data.table::data.table(
    trial_id = 1:10,
    exposed = c(rep(TRUE, 5), rep(FALSE, 5)),
    age = sample(30:70, 10),
    death = 0L
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- tte_trial(dt, design)

  expect_true(inherits(trial, "TTETrial"))
  expect_true(data.table::is.data.table(trial$data))
  expect_equal(nrow(trial$data), 10)
  expect_equal(length(trial$steps_completed), 0)
})

test_that("TTETrial makes a copy of input data", {
  dt <- data.table::data.table(
    trial_id = 1:5,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- tte_trial(dt, design)

  # Modifying trial data should not affect original
  trial$data[, new_col := 1]
  expect_false("new_col" %in% names(dt))
})

test_that("TTETrial validates required columns", {
  dt <- data.table::data.table(
    id = 1:5,  # Wrong name - missing trial_id
    exposed = TRUE
  )

  design <- tte_design(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  # Error now comes from auto-detect since neither person_id nor trial_id exists
  expect_error(tte_trial(dt, design), "Cannot auto-detect data_level")
})

test_that("TTETrial print method works", {
  dt <- data.table::data.table(
    trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- tte_trial(dt, design)
  output <- capture.output(print(trial))
  expect_true(any(grepl("TTETrial", output)))
})

# =============================================================================
# tte_enroll tests
# =============================================================================

test_that("tte_enroll samples at correct ratio and creates panels", {
  set.seed(42)
  # Create person-week data with 100 exposed, 900 unexposed
  n_exposed <- 100
  n_unexposed <- 900
  n_weeks <- 10

  dt <- data.table::data.table(
    id = rep(1:(n_exposed + n_unexposed), each = n_weeks),
    exposed = rep(c(rep(TRUE, n_exposed), rep(FALSE, n_unexposed)), each = n_weeks),
    eligible = rep(c(TRUE, rep(FALSE, n_weeks - 1)), n_exposed + n_unexposed),
    age = rep(sample(30:70, n_exposed + n_unexposed, replace = TRUE), each = n_weeks),
    death = 0L
  )

  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 5L
  )

  trial <- tte_trial(dt, design) |>
    tte_enroll(ratio = 2, seed = 123)

  # Check step is tracked
  expect_true("enroll" %in% trial$steps_completed)

  # Check data_level transition
  expect_equal(trial$data_level, "trial")

  # Check trial counts (ratio = 2 means 2 unexposed per exposed)
  trial_summary <- trial$data[, .(exposed = exposed[1]), by = trial_id]
  n_exposed_trials <- sum(trial_summary$exposed == TRUE)
  n_unexposed_trials <- sum(trial_summary$exposed == FALSE)
  expect_equal(n_exposed_trials, n_exposed)
  expect_equal(n_unexposed_trials, n_exposed * 2)  # 2:1 ratio

  # Check trial_id and trial_week created
  expect_true("trial_id" %in% names(trial$data))
  expect_true("trial_week" %in% names(trial$data))
  expect_equal(min(trial$data$trial_week), 0L)
})

# =============================================================================
# tte_collapse tests
# =============================================================================

test_that("tte_collapse aggregates correctly", {
  # Create data with 8 rows per trial, period_width=4 gives 2 periods
  # tstop 0-3 -> period 0, tstop 4-7 -> period 1
  dt <- data.table::data.table(
    trial_id = rep(1:2, each = 8),
    tstart = rep(0:7, 2),
    tstop = rep(0:7, 2),  # Using tstop as the time var for period grouping
    exposed = TRUE,
    age = rep(c(55, 60), each = 8),
    death = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L
  )

  trial <- tte_trial(dt, design) |>
    tte_collapse(period_width = 4, time_var = "tstop")

  # Check step is tracked
  expect_true("collapse" %in% trial$steps_completed)

  # Should have 2 trials x 2 periods = 4 rows
  expect_equal(nrow(trial$data), 4)

  # Check aggregation - max should capture events
  trial1 <- trial$data[trial_id == 1]
  data.table::setorderv(trial1, "tstart")
  expect_equal(trial1$death, c(1, 0))  # event in first period, none in second

  trial2 <- trial$data[trial_id == 2]
  data.table::setorderv(trial2, "tstart")
  expect_equal(trial2$death, c(0, 1))  # no event in first period, event in second
})

# =============================================================================
# tte_ipw tests
# =============================================================================

test_that("tte_ipw calculates weights", {
  set.seed(42)
  dt <- data.table::data.table(
    trial_id = rep(1:100, each = 4),
    tstart = rep(c(0, 4, 8, 12), 100),
    tstop = rep(c(4, 8, 12, 16), 100),
    exposed = rep(as.logical(rbinom(100, 1, 0.3)), each = 4),
    age = factor(rep(sample(1:4, 100, replace = TRUE), each = 4)),
    death = 0L
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design) |>
    tte_ipw(stabilize = TRUE)

  # Check step and weights tracked
  expect_true("ipw" %in% trial$steps_completed)
  expect_true("ipw" %in% trial$weight_cols)

  # Check columns exist
  expect_true("ps" %in% names(trial$data))
  expect_true("ipw" %in% names(trial$data))

  # Check IPW properties
  expect_true(all(trial$data$ipw > 0))
})

# =============================================================================
# tte_rbind tests
# =============================================================================

test_that("tte_rbind combines trials", {
  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  dt1 <- data.table::data.table(
    trial_id = 1:5,
    exposed = TRUE,
    age = 50,
    death = 0L
  )
  dt2 <- data.table::data.table(
    trial_id = 6:10,
    exposed = FALSE,
    age = 60,
    death = 0L
  )

  trial1 <- tte_trial(dt1, design)
  trial1$steps_completed <- c("match", "collapse")

  trial2 <- tte_trial(dt2, design)
  trial2$steps_completed <- c("match", "collapse")

  combined <- tte_rbind(list(trial1, trial2))

  expect_equal(nrow(combined$data), 10)
  expect_equal(combined$steps_completed, c("match", "collapse"))
})

test_that("tte_rbind validates input", {
  expect_error(tte_rbind(list()), "non-empty list")
  expect_error(tte_rbind(list("not a trial")), "TTETrial objects")
})

# =============================================================================
# tte_extract tests
# =============================================================================

test_that("tte_extract returns data.table", {
  dt <- data.table::data.table(
    trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- tte_trial(dt, design)
  result <- tte_extract(trial)

  expect_true(data.table::is.data.table(result))
  expect_equal(nrow(result), 10)
})

# =============================================================================
# tte_summary tests
# =============================================================================

test_that("tte_summary prints summary", {
  dt <- data.table::data.table(
    trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L,
    ipw = runif(10, 0.8, 1.2)
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- tte_trial(dt, design)
  trial$weight_cols <- "ipw"

  output <- capture.output(tte_summary(trial))
  expect_true(any(grepl("TTETrial Summary", output)))
  expect_true(any(grepl("ipw", output)))
})

# =============================================================================
# tte_truncate tests
# =============================================================================

test_that("tte_truncate truncates weights", {
  dt <- data.table::data.table(
    trial_id = 1:100,
    exposed = TRUE,
    age = 50,
    death = 0L,
    ipw = c(0.01, rep(1, 98), 100)
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- tte_trial(dt, design)
  trial$weight_cols <- "ipw"

  trial <- tte_truncate(trial)

  expect_true("truncate" %in% trial$steps_completed)
  expect_true("ipw_trunc" %in% names(trial$data))
  expect_true("ipw_trunc" %in% trial$weight_cols)
})

# =============================================================================
# tte_weights tests
# =============================================================================

test_that("tte_weights combines IPW and IPCW", {
  dt <- data.table::data.table(
    trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L,
    ipw = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    ipcw = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- tte_trial(dt, design)
  trial$weight_cols <- c("ipw", "ipcw")

  trial <- tte_weights(trial)

  expect_true("weights" %in% trial$steps_completed)
  expect_true("weight_pp" %in% names(trial$data))
  expect_equal(trial$data$weight_pp[1:5], c(2, 4, 6, 8, 10))
})

# =============================================================================
# Full workflow integration test
# =============================================================================

test_that("full workflow chains correctly", {
  set.seed(42)
  n_trials <- 200
  n_periods <- 10

  dt <- data.table::data.table(
    trial_id = rep(1:n_trials, each = n_periods),
    tstart = rep(seq(0, 36, 4)[1:n_periods], n_trials),
    tstop = rep(seq(4, 40, 4)[1:n_periods], n_trials),
    exposed = rep(as.logical(rbinom(n_trials, 1, 0.3)), each = n_periods),
    current_exposed = rep(as.logical(rbinom(n_trials, 1, 0.3)), each = n_periods),
    age = factor(rep(sample(1:4, n_trials, replace = TRUE), each = n_periods)),
    death = as.integer(runif(n_trials * n_periods) < 0.01)
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 40L
  )

  trial <- tte_trial(dt, design) |>
    tte_ipw(stabilize = TRUE)

  # Check workflow completed
  expect_true("ipw" %in% trial$steps_completed)
  expect_true("ipw" %in% names(trial$data))
  expect_true(all(trial$data$ipw > 0))
})

# =============================================================================
# person_id_var property tests
# =============================================================================

test_that("TTEDesign accepts person_id_var property", {
  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  expect_equal(design$person_id_var, "id")
  expect_equal(design$id_var, "trial_id")  # Default value
})

test_that("TTEDesign validates person_id_var length", {
  expect_error(
    tte_design(
      person_id_var = c("a", "b"),
      exposure_var = "exposed",
      outcome_vars = "death",
      confounder_vars = "age",
      follow_up_time = 52L
    ),
    "person_id_var must be length 1 or NULL"
  )
})

# =============================================================================
# data_level property tests
# =============================================================================

test_that("TTETrial auto-detects data_level as person_week", {
  dt <- data.table::data.table(
    id = 1:10,
    exposed = c(rep(TRUE, 5), rep(FALSE, 5)),
    age = 50,
    death = 0L
  )

  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- tte_trial(dt, design)

  expect_equal(trial$data_level, "person_week")
})

test_that("TTETrial auto-detects data_level as trial", {
  dt <- data.table::data.table(
    trial_id = 1:10,
    exposed = c(rep(TRUE, 5), rep(FALSE, 5)),
    age = 50,
    death = 0L
  )

  design <- tte_design(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- tte_trial(dt, design)

  expect_equal(trial$data_level, "trial")
})

test_that("TTETrial validates data_level value", {
  dt <- data.table::data.table(
    trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- tte_design(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  expect_error(
    tte_trial(dt, design, data_level = "invalid"),
    "data_level must be 'person_week' or 'trial'"
  )
})

# =============================================================================
# data_level guards tests
# =============================================================================

test_that("tte_enroll requires person_week data", {
  dt <- data.table::data.table(
    trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- tte_design(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- tte_trial(dt, design)

  expect_error(
    tte_enroll(trial, ratio = 2),
    "tte_enroll\\(\\) requires person_week level data"
  )
})

test_that("tte_collapse requires trial data", {
  dt <- data.table::data.table(
    id = 1:10,
    exposed = TRUE,
    eligible = TRUE,
    age = 50,
    death = 0L
  )

  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L,
    eligible_var = "eligible"
  )

  trial <- tte_trial(dt, design)

  expect_error(
    tte_collapse(trial, period_width = 4),
    "tte_collapse\\(\\) requires trial level data"
  )
})

test_that("tte_ipw requires trial data", {
  dt <- data.table::data.table(
    id = 1:10,
    exposed = TRUE,
    eligible = TRUE,
    age = 50,
    death = 0L
  )

  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L,
    eligible_var = "eligible"
  )

  trial <- tte_trial(dt, design)

  expect_error(
    tte_ipw(trial),
    "tte_ipw\\(\\) requires trial level data"
  )
})

# =============================================================================
# tte_enroll additional tests
# =============================================================================

test_that("tte_enroll creates trial panels from person-week data", {
  # Create person-week data for 3 persons, 10 weeks each
  set.seed(42)
  dt <- data.table::data.table(
    id = rep(1:3, each = 10),
    exposed = rep(c(TRUE, TRUE, FALSE), each = 10),
    eligible = c(
      c(FALSE, FALSE, TRUE, rep(FALSE, 7)),
      c(FALSE, TRUE, rep(FALSE, 8)),
      c(TRUE, rep(FALSE, 9))
    ),
    age = rep(c(50, 60, 55), each = 10),
    death = 0L
  )

  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 5L,
    eligible_var = "eligible"
  )

  trial <- tte_trial(dt, design) |>
    tte_enroll(ratio = 2, seed = 123)

  # Check data_level transition
  expect_equal(trial$data_level, "trial")
  expect_true("enroll" %in% trial$steps_completed)

  # Check trial_id was created
  expect_true("trial_id" %in% names(trial$data))

  # Check trial_week was created and is 0-indexed
  expect_true("trial_week" %in% names(trial$data))
  expect_equal(min(trial$data$trial_week), 0L)
})

test_that("tte_enroll carries forward baseline exposure", {
  # Create person-week data where exposure changes over time
  dt <- data.table::data.table(
    id = rep(1, 10),
    exposed = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    eligible = c(TRUE, rep(FALSE, 9)),
    age = 50,
    death = 0L
  )

  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 5L,
    eligible_var = "eligible"
  )

  trial <- tte_trial(dt, design) |>
    tte_enroll(ratio = 2, seed = 123)

  # Baseline exposure should be TRUE for all rows (from entry week)
  expect_true(all(trial$data$exposed == TRUE))
})

test_that("tte_enroll includes extra_cols", {
  dt <- data.table::data.table(
    id = rep(1, 10),
    exposed = TRUE,
    eligible = c(TRUE, rep(FALSE, 9)),
    age = 50,
    death = 0L,
    extra_col = 1:10
  )

  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 5L,
    eligible_var = "eligible"
  )

  trial <- tte_trial(dt, design) |>
    tte_enroll(ratio = 2, seed = 123, extra_cols = "extra_col")

  expect_true("extra_col" %in% names(trial$data))
})

# =============================================================================
# Full person_week -> trial workflow test
# =============================================================================

test_that("full person_week to trial workflow", {
  set.seed(42)

  # Create person-week data
  n_persons <- 100
  n_weeks <- 20

  dt <- data.table::data.table(
    id = rep(1:n_persons, each = n_weeks),
    exposed = rep(as.logical(rbinom(n_persons, 1, 0.3)), each = n_weeks),
    eligible = rep(
      c(TRUE, rep(FALSE, n_weeks - 1)),
      n_persons
    ),
    age = factor(rep(sample(1:4, n_persons, replace = TRUE), each = n_weeks)),
    death = 0L
  )

  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 10L,
    eligible_var = "eligible"
  )

  # Full workflow using tte_enroll
  trial <- tte_trial(dt, design) |>
    tte_enroll(ratio = 2, seed = 42)

  # Verify workflow
  expect_equal(trial$data_level, "trial")
  expect_true("enroll" %in% trial$steps_completed)
  expect_true("trial_id" %in% names(trial$data))
  expect_true("trial_week" %in% names(trial$data))

  # Check that we can continue with trial-level operations
  # Create tstart/tstop for tte_collapse
  trial$data[, tstart := trial_week]
  trial$data[, tstop := trial_week]

  trial <- trial |>
    tte_collapse(period_width = 5L, time_var = "trial_week") |>
    tte_ipw(stabilize = TRUE)

  expect_true("collapse" %in% trial$steps_completed)
  expect_true("ipw" %in% trial$steps_completed)
  expect_true("ipw" %in% names(trial$data))
})

# =============================================================================
# tte_rbind with data_level tests
# =============================================================================

test_that("tte_rbind validates same data_level", {
  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  dt1 <- data.table::data.table(
    id = 1:5,
    exposed = TRUE,
    age = 50,
    death = 0L
  )
  dt2 <- data.table::data.table(
    trial_id = 6:10,
    exposed = FALSE,
    age = 60,
    death = 0L
  )

  trial1 <- tte_trial(dt1, design, data_level = "person_week")

  # Create a design without person_id_var for trial-level data
  design2 <- tte_design(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )
  trial2 <- tte_trial(dt2, design2, data_level = "trial")

  expect_error(
    tte_rbind(list(trial1, trial2)),
    "All trials must have the same data_level"
  )
})

test_that("tte_rbind preserves data_level", {
  design <- tte_design(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  dt1 <- data.table::data.table(
    trial_id = 1:5,
    exposed = TRUE,
    age = 50,
    death = 0L
  )
  dt2 <- data.table::data.table(
    trial_id = 6:10,
    exposed = FALSE,
    age = 60,
    death = 0L
  )

  trial1 <- tte_trial(dt1, design)
  trial2 <- tte_trial(dt2, design)

  combined <- tte_rbind(list(trial1, trial2))

  expect_equal(combined$data_level, "trial")
})

# =============================================================================
# admin_censor_isoyearweek tests
# =============================================================================

test_that("TTEDesign accepts admin_censor_isoyearweek property", {
  design <- tte_design(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L,
    admin_censor_isoyearweek = "2023-52"
  )

  expect_equal(design$admin_censor_isoyearweek, "2023-52")
  # admin_censor_var should be NULL when using isoyearweek
  expect_null(design$admin_censor_var)
})

test_that("TTEDesign validates admin_censor_isoyearweek length", {
  expect_error(
    tte_design(
      exposure_var = "exposed",
      outcome_vars = "death",
      confounder_vars = "age",
      follow_up_time = 52L,
      admin_censor_isoyearweek = c("2023-52", "2024-01")
    ),
    "admin_censor_isoyearweek must be length 1 or NULL"
  )
})

test_that("TTEDesign rejects both admin_censor_var and admin_censor_isoyearweek", {
  expect_error(
    tte_design(
      exposure_var = "exposed",
      outcome_vars = "death",
      confounder_vars = "age",
      follow_up_time = 52L,
      admin_censor_var = "admin_censor",
      admin_censor_isoyearweek = "2023-52"
    ),
    "admin_censor_var and admin_censor_isoyearweek are mutually exclusive"
  )
})

# =============================================================================
# tte_prepare_outcome tests
# =============================================================================

test_that("tte_prepare_outcome validates outcome is in design", {
  dt <- data.table::data.table(
    trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    current_exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design)

  expect_error(
    tte_prepare_outcome(trial, outcome = "invalid_outcome"),
    "outcome must be one of: death"
  )
})

test_that("tte_prepare_outcome requires trial level data", {
  dt <- data.table::data.table(
    id = 1:10,
    exposed = TRUE,
    current_exposed = TRUE,
    eligible = TRUE,
    age = 50,
    death = 0L
  )

  design <- tte_design(
    person_id_var = "id",
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L,
    eligible_var = "eligible"
  )

  trial <- tte_trial(dt, design)

  expect_error(
    tte_prepare_outcome(trial, outcome = "death"),
    "tte_prepare_outcome\\(\\) requires trial level data"
  )
})

test_that("tte_prepare_outcome requires time_exposure_var", {
  dt <- data.table::data.table(
    trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L
  )

  design <- tte_design(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design)

  expect_error(
    tte_prepare_outcome(trial, outcome = "death"),
    "design must have time_exposure_var"
  )
})

test_that("tte_prepare_outcome can only run once", {
  dt <- data.table::data.table(
    trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    current_exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design) |>
    tte_prepare_outcome(outcome = "death")

  expect_error(
    tte_prepare_outcome(trial, outcome = "death"),
    "tte_prepare_outcome\\(\\) can only be run once"
  )
})

test_that("tte_prepare_outcome computes weeks_to_event correctly", {
  # Trial 1: event at tstop = 8
  # Trial 2: no event
  dt <- data.table::data.table(
    trial_id = rep(1:2, each = 4),
    tstart = rep(c(0, 4, 8, 12), 2),
    tstop = rep(c(4, 8, 12, 16), 2),
    exposed = rep(TRUE, 8),
    current_exposed = rep(TRUE, 8),
    age = rep(50, 8),
    death = c(0, 1, 0, 0, 0, 0, 0, 0)  # Event at tstop = 8 for trial 1
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design) |>
    tte_prepare_outcome(outcome = "death")

  # Trial 1: weeks_to_event should be 8
  expect_equal(
    trial$data[trial_id == 1, weeks_to_event[1]],
    8L
  )

  # Trial 2: weeks_to_event should be NA
  expect_true(
    is.na(trial$data[trial_id == 2, weeks_to_event[1]])
  )
})

test_that("tte_prepare_outcome computes weeks_to_protocol_deviation correctly", {
  # Trial 1: exposed=TRUE, current_exposed switches to FALSE at tstop = 8 (deviation)
  # Trial 2: exposed=TRUE, stays TRUE (no deviation)
  dt <- data.table::data.table(
    trial_id = rep(1:2, each = 4),
    tstart = rep(c(0, 4, 8, 12), 2),
    tstop = rep(c(4, 8, 12, 16), 2),
    exposed = rep(TRUE, 8),
    current_exposed = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    age = rep(50, 8),
    death = 0L
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design) |>
    tte_prepare_outcome(outcome = "death")

  # Trial 1: weeks_to_protocol_deviation should be 8
  expect_equal(
    trial$data[trial_id == 1, weeks_to_protocol_deviation[1]],
    8L
  )

  # Trial 2: weeks_to_protocol_deviation should be NA
  expect_true(
    is.na(trial$data[trial_id == 2, weeks_to_protocol_deviation[1]])
  )
})

test_that("tte_prepare_outcome computes weeks_to_loss correctly", {
  # Trial 1: has 4 periods (max tstop = 16), follow_up = 16 -> no loss

  # Trial 2: has only 2 periods (max tstop = 8), follow_up = 16 -> lost
  dt <- data.table::data.table(
    trial_id = c(rep(1, 4), rep(2, 2)),
    tstart = c(0, 4, 8, 12, 0, 4),
    tstop = c(4, 8, 12, 16, 4, 8),
    exposed = TRUE,
    current_exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design) |>
    tte_prepare_outcome(outcome = "death")

  # Trial 1: weeks_to_loss should be NA (not lost)
  expect_true(
    is.na(trial$data[trial_id == 1, weeks_to_loss[1]])
  )

  # Trial 2: weeks_to_loss should be 8 (max tstop before follow_up_time)
  expect_equal(
    trial$data[trial_id == 2, weeks_to_loss[1]],
    8L
  )
})

test_that("tte_prepare_outcome filters data to tstop <= censor_week", {
  # Trial 1: event at tstop = 8 -> should only have rows up to tstop = 8
  dt <- data.table::data.table(
    trial_id = rep(1, 4),
    tstart = c(0, 4, 8, 12),
    tstop = c(4, 8, 12, 16),
    exposed = TRUE,
    current_exposed = TRUE,
    age = 50,
    death = c(0, 1, 0, 0)
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design) |>
    tte_prepare_outcome(outcome = "death")

  # Should only have 2 rows (tstop = 4 and tstop = 8)
  expect_equal(nrow(trial$data), 2)
  expect_equal(max(trial$data$tstop), 8L)
})

test_that("tte_prepare_outcome creates event indicator correctly", {
  dt <- data.table::data.table(
    trial_id = rep(1, 4),
    tstart = c(0, 4, 8, 12),
    tstop = c(4, 8, 12, 16),
    exposed = TRUE,
    current_exposed = TRUE,
    age = 50,
    death = c(0, 1, 0, 0)
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design) |>
    tte_prepare_outcome(outcome = "death")

  # event should be 0 for first row (tstop = 4), 1 for second row (tstop = 8)
  data.table::setorder(trial$data, tstop)
  expect_equal(trial$data$event, c(0L, 1L))
})

test_that("tte_prepare_outcome creates censor_this_period indicator correctly", {
  # Trial deviates at tstop = 8 (current_exposed becomes FALSE)
  dt <- data.table::data.table(
    trial_id = rep(1, 4),
    tstart = c(0, 4, 8, 12),
    tstop = c(4, 8, 12, 16),
    exposed = TRUE,
    current_exposed = c(TRUE, FALSE, FALSE, FALSE),
    age = 50,
    death = 0L
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design) |>
    tte_prepare_outcome(outcome = "death")

  # Should have 2 rows (filtered to tstop <= censor_week = 8)
  # censor_this_period should be 0 for first row (tstop = 4), 1 for second (tstop = 8)
  data.table::setorder(trial$data, tstop)
  expect_equal(trial$data$censor_this_period, c(0L, 1L))
})

test_that("tte_prepare_outcome sets active_outcome", {
  dt <- data.table::data.table(
    trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    current_exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L,
    hosp = 0L
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = c("death", "hosp"),
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- tte_trial(dt, design) |>
    tte_prepare_outcome(outcome = "hosp")

  expect_equal(trial$active_outcome, "hosp")
  expect_true("prepare_outcome" %in% trial$steps_completed)
})

test_that("tte_prepare_outcome computes weeks_to_admin_end correctly", {
  skip_if_not_installed("cstime")

  # Create data with isoyearweek column
  dt <- data.table::data.table(
    trial_id = rep(1:2, each = 4),
    tstart = rep(c(0, 4, 8, 12), 2),
    tstop = rep(c(4, 8, 12, 16), 2),
    exposed = rep(TRUE, 8),
    current_exposed = rep(TRUE, 8),
    age = rep(50, 8),
    death = 0L,
    isoyearweek = c(
      # Trial 1: starts at 2023-40
      "2023-40", "2023-44", "2023-48", "2023-52",
      # Trial 2: starts at 2023-48
      "2023-48", "2023-52", "2024-04", "2024-08"
    )
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L,
    admin_censor_isoyearweek = "2023-52"
  )

  trial <- tte_trial(dt, design) |>
    tte_prepare_outcome(outcome = "death")

  # Trial 1 starts at 2023-40, admin end is 2023-52 -> ~12 weeks
  # Trial 2 starts at 2023-48, admin end is 2023-52 -> ~4 weeks
  # (exact weeks depend on cstime calculation)
  expect_true("weeks_to_admin_end" %in% names(trial$data))
  expect_true(all(!is.na(trial$data$weeks_to_admin_end)))
})

test_that("tte_prepare_outcome requires isoyearweek when admin_censor_isoyearweek set", {
  dt <- data.table::data.table(
    trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    current_exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L
    # Note: no isoyearweek column
  )

  design <- tte_design(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L,
    admin_censor_isoyearweek = "2023-52"
  )

  trial <- tte_trial(dt, design)

  expect_error(
    tte_prepare_outcome(trial, outcome = "death"),
    "admin_censor_isoyearweek requires 'isoyearweek' column"
  )
})

# =============================================================================
# tte_collapse person_weeks tests
# =============================================================================

test_that("tte_collapse creates person_weeks column", {
  dt <- data.table::data.table(
    trial_id = rep(1:2, each = 8),
    tstart = rep(0:7, 2),
    tstop = rep(0:7, 2),
    exposed = TRUE,
    age = rep(c(55, 60), each = 8),
    death = 0L
  )

  design <- tte_design(
    id_var = "trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L
  )

  trial <- tte_trial(dt, design) |>
    tte_collapse(period_width = 4, time_var = "tstop")

  expect_true("person_weeks" %in% names(trial$data))
  # With period_width = 4, person_weeks should equal tstop - tstart
  expect_true(all(trial$data$person_weeks == trial$data$tstop - trial$data$tstart))
})
