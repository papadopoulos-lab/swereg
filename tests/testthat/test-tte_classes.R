# Tests for TTE R6 classes and methods

# =============================================================================
# TTEDesign tests
# =============================================================================

test_that("TTEDesign creates valid design object", {
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    outcome_vars = c("death", "hosp"),
    confounder_vars = c("age", "sex"),
    follow_up_time = 52L
  )

  expect_true(inherits(design, "TTEDesign"))
  expect_equal(design$id_var, "enrollment_person_trial_id")
  expect_equal(design$exposure_var, "exposed")
  expect_equal(design$outcome_vars, c("death", "hosp"))
  expect_equal(design$confounder_vars, c("age", "sex"))
  expect_equal(design$follow_up_time, 52L)
  expect_equal(design$tstart_var, "tstart")
  expect_equal(design$tstop_var, "tstop")
  expect_equal(design$period_width, 4L)
})

test_that("TTEDesign accepts optional parameters", {
  design <- TTEDesign$new(
    id_var = "id",
    exposure_var = "treated",
    outcome_vars = "event",
    confounder_vars = "age",
    follow_up_time = 100L,
    time_exposure_var = "current_treated",
    eligible_var = "eligible",
    admin_censor_var = "admin_censor",
    tstart_var = "t0",
    tstop_var = "t1",
    period_width = 8L
  )

  expect_equal(design$time_exposure_var, "current_treated")
  expect_equal(design$eligible_var, "eligible")
  expect_equal(design$admin_censor_var, "admin_censor")
  expect_equal(design$tstart_var, "t0")
  expect_equal(design$tstop_var, "t1")
  expect_equal(design$period_width, 8L)
})

test_that("TTEDesign validates inputs", {
  # id_var must be length 1
  expect_error(
    TTEDesign$new(
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
    TTEDesign$new(
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
    TTEDesign$new(
      id_var = "id",
      exposure_var = "exposed",
      outcome_vars = "death",
      confounder_vars = "age",
      follow_up_time = 0L
    ),
    "follow_up_time must be a positive integer"
  )

  # period_width must be positive integer
  expect_error(
    TTEDesign$new(
      id_var = "id",
      exposure_var = "exposed",
      outcome_vars = "death",
      confounder_vars = "age",
      follow_up_time = 52L,
      period_width = 0L
    ),
    "period_width must be a positive integer"
  )

  expect_error(
    TTEDesign$new(
      id_var = "id",
      exposure_var = "exposed",
      outcome_vars = "death",
      confounder_vars = "age",
      follow_up_time = 52L,
      period_width = 2.5
    ),
    "period_width must be a positive integer"
  )
})

test_that("TTEDesign print method works", {
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    outcome_vars = c("death", "hosp"),
    confounder_vars = c("age", "sex"),
    follow_up_time = 52L
  )

  output <- capture.output(print(design))
  expect_true(any(grepl("TTEDesign", output)))
  expect_true(any(grepl("enrollment_person_trial_id", output)))
  expect_true(any(grepl("Period width", output)))
})

# =============================================================================
# TTEEnrollment tests
# =============================================================================

test_that("TTEEnrollment creates valid trial object", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = c(rep(TRUE, 5), rep(FALSE, 5)),
    age = sample(30:70, 10),
    death = 0L
  )

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)

  expect_true(inherits(trial, "TTEEnrollment"))
  expect_true(data.table::is.data.table(trial$data))
  expect_equal(nrow(trial$data), 10)
  expect_equal(length(trial$steps_completed), 0)
})

test_that("TTEEnrollment makes a copy of input data", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:5,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)

  # Modifying trial data should not affect original
  trial$data[, new_col := 1]
  expect_false("new_col" %in% names(dt))
})

test_that("TTEEnrollment validates required columns", {
  dt <- data.table::data.table(
    id = 1:5,  # Wrong name - missing enrollment_person_trial_id
    exposed = TRUE
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  # Error now comes from auto-detect since neither person_id nor enrollment_person_trial_id exists
  expect_error(TTEEnrollment$new(dt, design), "Cannot auto-detect data_level")
})

test_that("TTEEnrollment print method works", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)
  output <- capture.output(print(trial))
  expect_true(any(grepl("TTEEnrollment", output)))
})

# =============================================================================
# tte_enroll tests (band-based)
# =============================================================================

# Helper: create person-week data with isoyearweek for band-based enrollment
.make_person_week_data <- function(n_exposed, n_unexposed, n_weeks,
                                   start_isoyearweek = "2020-01") {
  n_persons <- n_exposed + n_unexposed
  cstime_weeks <- cstime::dates_by_isoyearweek[, .(isoyearweek)]
  start_idx <- which(cstime_weeks$isoyearweek == start_isoyearweek)
  week_range <- cstime_weeks$isoyearweek[start_idx:(start_idx + n_weeks - 1)]

  dt <- data.table::data.table(
    id = rep(1:n_persons, each = n_weeks),
    isoyearweek = rep(week_range, n_persons),
    exposed = rep(c(rep(TRUE, n_exposed), rep(FALSE, n_unexposed)), each = n_weeks),
    eligible = rep(c(TRUE, rep(FALSE, n_weeks - 1)), n_persons),
    age = rep(sample(30:70, n_persons, replace = TRUE), each = n_weeks),
    death = 0L
  )
  dt
}

test_that("tte_enroll samples at correct ratio and creates band-level panels", {
  set.seed(42)
  dt <- .make_person_week_data(n_exposed = 50, n_unexposed = 200, n_weeks = 20)

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 12L,
    period_width = 4L
  )

  trial <- TTEEnrollment$new(dt, design, ratio = 2, seed = 123)

  # Check step is tracked
  expect_true("enroll" %in% trial$steps_completed)

  # Check data_level transition
  expect_equal(trial$data_level, "trial")

  # Check trial counts (ratio = 2 means 2 unexposed per exposed)
  trial_summary <- trial$data[, .(exposed = exposed[1]), by = enrollment_person_trial_id]
  n_exposed_trials <- sum(trial_summary$exposed == TRUE)
  n_unexposed_trials <- sum(trial_summary$exposed == FALSE)
  expect_equal(n_exposed_trials, 50)
  # Ratio is per-band, so total may vary slightly but should be approximately 2:1
  expect_true(n_unexposed_trials >= n_exposed_trials)

  # Check enrollment_person_trial_id, trial_id (band), and trial_week created
  expect_true("enrollment_person_trial_id" %in% names(trial$data))
  expect_true("trial_id" %in% names(trial$data))
  expect_true("trial_week" %in% names(trial$data))
  expect_equal(min(trial$data$trial_week), 0L)

  # Check tstart/tstop/person_weeks created
  expect_true("tstart" %in% names(trial$data))
  expect_true("tstop" %in% names(trial$data))
  expect_true("person_weeks" %in% names(trial$data))
  expect_true(all(trial$data$person_weeks >= 1L & trial$data$person_weeks <= 4L))
  expect_true(all(trial$data$tstop - trial$data$tstart == 4L))
})

test_that("tte_enroll band IDs are isoyearweek-based (calendar-based)", {
  set.seed(42)
  dt <- .make_person_week_data(n_exposed = 10, n_unexposed = 40, n_weeks = 8)

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L,
    period_width = 4L
  )

  trial <- TTEEnrollment$new(dt, design, ratio = 2, seed = 42)

  # tstart should be multiples of period_width
  expect_true(all(trial$data$tstart %% 4 == 0))
  # tstop should be tstart + period_width
  expect_true(all(trial$data$tstop == trial$data$tstart + 4L))
})

test_that("tte_enroll per-band stratified matching", {
  set.seed(42)
  # Create data where all persons are eligible in week 1 AND week 5
  # to ensure multiple bands have entries
  n_exposed <- 20
  n_unexposed <- 80
  n_weeks <- 12
  n_persons <- n_exposed + n_unexposed

  cstime_weeks <- cstime::dates_by_isoyearweek[, .(isoyearweek)]
  start_idx <- which(cstime_weeks$isoyearweek == "2020-01")
  week_range <- cstime_weeks$isoyearweek[start_idx:(start_idx + n_weeks - 1)]

  dt <- data.table::data.table(
    id = rep(1:n_persons, each = n_weeks),
    isoyearweek = rep(week_range, n_persons),
    exposed = rep(c(rep(TRUE, n_exposed), rep(FALSE, n_unexposed)), each = n_weeks),
    eligible = rep(c(TRUE, rep(FALSE, n_weeks - 1)), n_persons),
    age = rep(sample(30:70, n_persons, replace = TRUE), each = n_weeks),
    death = 0L
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L,
    period_width = 4L
  )

  trial <- TTEEnrollment$new(dt, design, ratio = 2, seed = 42)

  # With 20 exposed and ratio=2, we expect ~40 unexposed
  trial_summary <- trial$data[, .(exposed = exposed[1]), by = enrollment_person_trial_id]
  n_exp <- sum(trial_summary$exposed == TRUE)
  n_unexp <- sum(trial_summary$exposed == FALSE)
  expect_equal(n_exp, n_exposed)
  expect_true(n_unexp <= n_exposed * 2 + 5)  # Allow some slack for per-band sampling
})

test_that("tte_enroll with period_width=1 produces weekly-level data", {
  set.seed(42)
  dt <- .make_person_week_data(n_exposed = 10, n_unexposed = 40, n_weeks = 8)

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 5L,
    period_width = 1L
  )

  trial <- TTEEnrollment$new(dt, design, ratio = 2, seed = 42)

  # With period_width=1, each band is one week
  expect_true(all(trial$data$person_weeks == 1L))
  expect_true(all(trial$data$tstop - trial$data$tstart == 1L))
  # trial_week should be 0, 1, 2, ...
  trial_weeks <- trial$data[, .(max_tw = max(trial_week)), by = enrollment_person_trial_id]
  expect_true(all(trial_weeks$max_tw <= 4L))  # follow_up = 5 -> max trial_week = 4
})

test_that("tte_enroll requires isoyearweek column", {
  dt <- data.table::data.table(
    id = rep(1:10, each = 5),
    exposed = rep(c(TRUE, FALSE), each = 25),
    eligible = rep(c(TRUE, rep(FALSE, 4)), 10),
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 4L,
    period_width = 2L
  )

  expect_error(
    TTEEnrollment$new(dt, design, ratio = 2, seed = 42),
    "isoyearweek"
  )
})

test_that("tte_enroll carries forward baseline exposure", {
  # Create person-week data where exposure changes over time
  cstime_weeks <- cstime::dates_by_isoyearweek[, .(isoyearweek)]
  start_idx <- which(cstime_weeks$isoyearweek == "2020-01")
  week_range <- cstime_weeks$isoyearweek[start_idx:(start_idx + 9)]

  dt <- data.table::data.table(
    id = rep(1, 10),
    isoyearweek = week_range,
    exposed = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    eligible = c(TRUE, rep(FALSE, 9)),
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L,
    period_width = 4L
  )

  trial <- TTEEnrollment$new(dt, design, ratio = 2, seed = 123)

  # Baseline exposure should be TRUE for all rows (from entry band)
  expect_true(all(trial$data$exposed == TRUE))
})

test_that("tte_enroll includes extra_cols", {
  cstime_weeks <- cstime::dates_by_isoyearweek[, .(isoyearweek)]
  start_idx <- which(cstime_weeks$isoyearweek == "2020-01")
  week_range <- cstime_weeks$isoyearweek[start_idx:(start_idx + 9)]

  dt <- data.table::data.table(
    id = rep(1, 10),
    isoyearweek = week_range,
    exposed = TRUE,
    eligible = c(TRUE, rep(FALSE, 9)),
    age = 50,
    death = 0L,
    extra_col = 1:10
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L,
    period_width = 4L
  )

  trial <- TTEEnrollment$new(dt, design, ratio = 2, seed = 123,
                          extra_cols = "extra_col")

  expect_true("extra_col" %in% names(trial$data))
})

# =============================================================================
# tteenrollment_rbind tests
# =============================================================================

test_that("tteenrollment_rbind combines trials", {
  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  dt1 <- data.table::data.table(
    enrollment_person_trial_id = 1:5,
    exposed = TRUE,
    age = 50,
    death = 0L
  )
  dt2 <- data.table::data.table(
    enrollment_person_trial_id = 6:10,
    exposed = FALSE,
    age = 60,
    death = 0L
  )

  trial1 <- TTEEnrollment$new(dt1, design)
  trial1$steps_completed <- c("enroll")

  trial2 <- TTEEnrollment$new(dt2, design)
  trial2$steps_completed <- c("enroll")

  combined <- tteenrollment_rbind(list(trial1, trial2))

  expect_equal(nrow(combined$data), 10)
  expect_equal(combined$steps_completed, c("enroll"))
})

test_that("tteenrollment_rbind validates input", {
  expect_error(tteenrollment_rbind(list()), "non-empty list")
  expect_error(tteenrollment_rbind(list("not a trial")), "TTEEnrollment objects")
})

# =============================================================================
# tte_extract tests
# =============================================================================

test_that("tte_extract returns data.table", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)
  result <- trial$extract()

  expect_true(data.table::is.data.table(result))
  expect_equal(nrow(result), 10)
})

# =============================================================================
# tte_summary tests
# =============================================================================

test_that("tte_summary prints summary", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L,
    ipw = runif(10, 0.8, 1.2)
  )

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "ipw"

  output <- capture.output(print(trial))
  expect_true(any(grepl("TTEEnrollment", output)))
  expect_true(any(grepl("ipw", output)))
})

# =============================================================================
# tte_truncate tests
# =============================================================================

test_that("tte_truncate truncates weights", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:100,
    exposed = TRUE,
    age = 50,
    death = 0L,
    ipw = c(0.01, rep(1, 98), 100)
  )

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "ipw"

  trial$s3_truncate_weights()

  expect_true("truncate" %in% trial$steps_completed)
  expect_true("ipw_trunc" %in% names(trial$data))
  expect_true("ipw_trunc" %in% trial$weight_cols)
})

# =============================================================================
# tte_weights tests
# =============================================================================

test_that("tte_weights combines IPW and IPCW", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L,
    ipw = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    ipcw = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  )

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- c("ipw", "ipcw")

  trial$.__enclos_env__$private$combine_weights(
    ipw_col = "ipw", ipcw_col = "ipcw"
  )

  expect_true("weights" %in% trial$steps_completed)
  expect_true("analysis_weight_pp" %in% names(trial$data))
  expect_equal(trial$data$analysis_weight_pp[1:5], c(2, 4, 6, 8, 10))
})

# =============================================================================
# Full workflow integration test
# =============================================================================

test_that("full workflow chains correctly", {
  set.seed(42)
  n_trials <- 200
  n_periods <- 10

  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:n_trials, each = n_periods),
    tstart = rep(seq(0, 36, 4)[1:n_periods], n_trials),
    tstop = rep(seq(4, 40, 4)[1:n_periods], n_trials),
    exposed = rep(as.logical(rbinom(n_trials, 1, 0.3)), each = n_periods),
    current_exposed = rep(as.logical(rbinom(n_trials, 1, 0.3)), each = n_periods),
    age = factor(rep(sample(1:4, n_trials, replace = TRUE), each = n_periods)),
    death = as.integer(runif(n_trials * n_periods) < 0.01)
  )

  design <- TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 40L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$s2_ipw(stabilize = TRUE)

  # Check workflow completed
  expect_true("ipw" %in% trial$steps_completed)
  expect_true("ipw" %in% names(trial$data))
  expect_true(all(trial$data$ipw > 0))
})

# =============================================================================
# person_id_var property tests
# =============================================================================

test_that("TTEDesign accepts person_id_var property", {
  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  expect_equal(design$person_id_var, "id")
  expect_equal(design$id_var, "enrollment_person_trial_id")  # Default value
})

test_that("TTEDesign validates person_id_var length", {
  expect_error(
    TTEDesign$new(
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

test_that("TTEEnrollment auto-detects data_level as person_week", {
  dt <- data.table::data.table(
    id = 1:10,
    exposed = c(rep(TRUE, 5), rep(FALSE, 5)),
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)

  expect_equal(trial$data_level, "person_week")
})

test_that("TTEEnrollment auto-detects data_level as trial", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = c(rep(TRUE, 5), rep(FALSE, 5)),
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)

  expect_equal(trial$data_level, "trial")
})

test_that("TTEEnrollment validates data_level value", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  expect_error(
    TTEEnrollment$new(dt, design, data_level = "invalid"),
    "data_level must be 'person_week' or 'trial'"
  )
})

# =============================================================================
# data_level guards tests
# =============================================================================

test_that("tte_enroll requires person_week data", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  expect_error(
    TTEEnrollment$new(dt, design, ratio = 2),
    "enroll\\(\\) requires person_week level data"
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

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L,
    eligible_var = "eligible"
  )

  trial <- TTEEnrollment$new(dt, design)

  expect_error(
    trial$s2_ipw(),
    "s2_ipw\\(\\) requires trial level data"
  )
})

# =============================================================================
# tte_enroll additional tests
# =============================================================================

test_that("tte_enroll creates trial panels from person-week data", {
  set.seed(42)
  dt <- .make_person_week_data(n_exposed = 2, n_unexposed = 10, n_weeks = 10)
  # Make persons 1 and 2 eligible at different weeks
  dt[id == 1 & isoyearweek == dt[id == 1, isoyearweek[3]], eligible := TRUE]
  dt[id == 2 & isoyearweek == dt[id == 2, isoyearweek[2]], eligible := TRUE]

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L,
    period_width = 4L
  )

  trial <- TTEEnrollment$new(dt, design, ratio = 2, seed = 123)

  # Check data_level transition
  expect_equal(trial$data_level, "trial")
  expect_true("enroll" %in% trial$steps_completed)

  # Check enrollment_person_trial_id and trial_id (band) were created
  expect_true("enrollment_person_trial_id" %in% names(trial$data))
  expect_true("trial_id" %in% names(trial$data))

  # Check trial_week was created and is 0-indexed
  expect_true("trial_week" %in% names(trial$data))
  expect_equal(min(trial$data$trial_week), 0L)
})

# =============================================================================
# Full person_week -> trial workflow test
# =============================================================================

test_that("full person_week to trial workflow", {
  set.seed(42)
  dt <- .make_person_week_data(n_exposed = 30, n_unexposed = 70, n_weeks = 20)

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 12L,
    period_width = 4L
  )

  # Full workflow using TTEEnrollment$new(..., ratio = ...)
  trial <- TTEEnrollment$new(dt, design, ratio = 2, seed = 42)

  # Verify workflow
  expect_equal(trial$data_level, "trial")
  expect_true("enroll" %in% trial$steps_completed)
  expect_true("enrollment_person_trial_id" %in% names(trial$data))
  expect_true("trial_id" %in% names(trial$data))
  expect_true("trial_week" %in% names(trial$data))
  expect_true("person_weeks" %in% names(trial$data))

  # Continue with trial-level operations (no collapse needed!)
  trial$s2_ipw(stabilize = TRUE)

  expect_true("ipw" %in% trial$steps_completed)
  expect_true("ipw" %in% names(trial$data))
})

# =============================================================================
# tteenrollment_rbind with data_level tests
# =============================================================================

test_that("tteenrollment_rbind validates same data_level", {
  design <- TTEDesign$new(
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
    enrollment_person_trial_id = 6:10,
    exposed = FALSE,
    age = 60,
    death = 0L
  )

  trial1 <- TTEEnrollment$new(dt1, design, data_level = "person_week")

  # Create a design without person_id_var for trial-level data
  design2 <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )
  trial2 <- TTEEnrollment$new(dt2, design2, data_level = "trial")

  expect_error(
    tteenrollment_rbind(list(trial1, trial2)),
    "All trials must have the same data_level"
  )
})

test_that("tteenrollment_rbind preserves data_level", {
  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  dt1 <- data.table::data.table(
    enrollment_person_trial_id = 1:5,
    exposed = TRUE,
    age = 50,
    death = 0L
  )
  dt2 <- data.table::data.table(
    enrollment_person_trial_id = 6:10,
    exposed = FALSE,
    age = 60,
    death = 0L
  )

  trial1 <- TTEEnrollment$new(dt1, design)
  trial2 <- TTEEnrollment$new(dt2, design)

  combined <- tteenrollment_rbind(list(trial1, trial2))

  expect_equal(combined$data_level, "trial")
})

# =============================================================================
# admin_censor_isoyearweek tests
# =============================================================================

test_that("TTEDesign accepts admin_censor_isoyearweek property", {
  design <- TTEDesign$new(
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
    TTEDesign$new(
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
    TTEDesign$new(
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
# tte_s5_prepare_outcome tests
# =============================================================================

test_that("tte_s5_prepare_outcome validates outcome is in design", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    current_exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)

  expect_error(
    trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "invalid_outcome"),
    "outcome must be one of: death"
  )
})

test_that("tte_s5_prepare_outcome requires trial level data", {
  dt <- data.table::data.table(
    id = 1:10,
    exposed = TRUE,
    current_exposed = TRUE,
    eligible = TRUE,
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L,
    eligible_var = "eligible"
  )

  trial <- TTEEnrollment$new(dt, design)

  expect_error(
    trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death"),
    "s5_prepare_outcome\\(\\) requires trial level data"
  )
})

test_that("tte_s5_prepare_outcome requires time_exposure_var", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)

  expect_error(
    trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death"),
    "design must have time_exposure_var"
  )
})

test_that("tte_s5_prepare_outcome can only run once", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    current_exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death")

  expect_error(
    trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death"),
    "s5_prepare_outcome\\(\\) can only be run once"
  )
})

test_that("tte_s5_prepare_outcome computes weeks_to_event correctly", {
  # Trial 1: event at tstop = 8
  # Trial 2: no event
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:2, each = 4),
    tstart = rep(c(0, 4, 8, 12), 2),
    tstop = rep(c(4, 8, 12, 16), 2),
    exposed = rep(TRUE, 8),
    current_exposed = rep(TRUE, 8),
    age = rep(50, 8),
    death = c(0, 1, 0, 0, 0, 0, 0, 0)  # Event at tstop = 8 for trial 1
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death")

  # Trial 1: weeks_to_event should be 8
  expect_equal(
    trial$data[enrollment_person_trial_id == 1, weeks_to_event[1]],
    8L
  )

  # Trial 2: weeks_to_event should be NA
  expect_true(
    is.na(trial$data[enrollment_person_trial_id == 2, weeks_to_event[1]])
  )
})

test_that("tte_s5_prepare_outcome computes weeks_to_protocol_deviation correctly", {
  # Trial 1: exposed=TRUE, current_exposed switches to FALSE at tstop = 8 (deviation)
  # Trial 2: exposed=TRUE, stays TRUE (no deviation)
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:2, each = 4),
    tstart = rep(c(0, 4, 8, 12), 2),
    tstop = rep(c(4, 8, 12, 16), 2),
    exposed = rep(TRUE, 8),
    current_exposed = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    age = rep(50, 8),
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death")

  # Trial 1: weeks_to_protocol_deviation should be 8
  expect_equal(
    trial$data[enrollment_person_trial_id == 1, weeks_to_protocol_deviation[1]],
    8L
  )

  # Trial 2: weeks_to_protocol_deviation should be NA
  expect_true(
    is.na(trial$data[enrollment_person_trial_id == 2, weeks_to_protocol_deviation[1]])
  )
})

test_that("tte_s5_prepare_outcome computes weeks_to_loss correctly", {
  # Trial 1: has 4 periods (max tstop = 16), follow_up = 16 -> no loss

  # Trial 2: has only 2 periods (max tstop = 8), follow_up = 16 -> lost
  dt <- data.table::data.table(
    enrollment_person_trial_id = c(rep(1, 4), rep(2, 2)),
    tstart = c(0, 4, 8, 12, 0, 4),
    tstop = c(4, 8, 12, 16, 4, 8),
    exposed = TRUE,
    current_exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death")

  # Trial 1: weeks_to_loss should be NA (not lost)
  expect_true(
    is.na(trial$data[enrollment_person_trial_id == 1, weeks_to_loss[1]])
  )

  # Trial 2: weeks_to_loss should be 8 (max tstop before follow_up_time)
  expect_equal(
    trial$data[enrollment_person_trial_id == 2, weeks_to_loss[1]],
    8L
  )
})

test_that("tte_s5_prepare_outcome filters data to tstop <= censor_week", {
  # Trial 1: event at tstop = 8 -> should only have rows up to tstop = 8
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1, 4),
    tstart = c(0, 4, 8, 12),
    tstop = c(4, 8, 12, 16),
    exposed = TRUE,
    current_exposed = TRUE,
    age = 50,
    death = c(0, 1, 0, 0)
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death")

  # Should only have 2 rows (tstop = 4 and tstop = 8)
  expect_equal(nrow(trial$data), 2)
  expect_equal(max(trial$data$tstop), 8L)
})

test_that("tte_s5_prepare_outcome creates event indicator correctly", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1, 4),
    tstart = c(0, 4, 8, 12),
    tstop = c(4, 8, 12, 16),
    exposed = TRUE,
    current_exposed = TRUE,
    age = 50,
    death = c(0, 1, 0, 0)
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death")

  # event should be 0 for first row (tstop = 4), 1 for second row (tstop = 8)
  data.table::setorder(trial$data, tstop)
  expect_equal(trial$data$event, c(0L, 1L))
})

test_that("tte_s5_prepare_outcome creates censor_this_period indicator correctly", {
  # Trial deviates at tstop = 8 (current_exposed becomes FALSE)
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1, 4),
    tstart = c(0, 4, 8, 12),
    tstop = c(4, 8, 12, 16),
    exposed = TRUE,
    current_exposed = c(TRUE, FALSE, FALSE, FALSE),
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death")

  # Should have 2 rows (filtered to tstop <= censor_week = 8)
  # censor_this_period should be 0 for first row (tstop = 4), 1 for second (tstop = 8)
  data.table::setorder(trial$data, tstop)
  expect_equal(trial$data$censor_this_period, c(0L, 1L))
})

test_that("tte_s5_prepare_outcome sets active_outcome", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    current_exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L,
    hosp = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = c("death", "hosp"),
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "hosp")

  expect_equal(trial$active_outcome, "hosp")
  expect_true("prepare_outcome" %in% trial$steps_completed)
})

test_that("tte_s5_prepare_outcome computes weeks_to_admin_end correctly", {
  skip_if_not_installed("cstime")

  # Create data with isoyearweek column
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:2, each = 4),
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

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L,
    admin_censor_isoyearweek = "2023-52"
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death")

  # Trial 1 starts at 2023-40, admin end is 2023-52 -> ~12 weeks
  # Trial 2 starts at 2023-48, admin end is 2023-52 -> ~4 weeks
  # (exact weeks depend on cstime calculation)
  expect_true("weeks_to_admin_end" %in% names(trial$data))
  expect_true(all(!is.na(trial$data$weeks_to_admin_end)))
})

test_that("tte_s5_prepare_outcome requires isoyearweek when admin_censor_isoyearweek set", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    current_exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L
    # Note: no isoyearweek column
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L,
    admin_censor_isoyearweek = "2023-52"
  )

  trial <- TTEEnrollment$new(dt, design)

  expect_error(
    trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death"),
    "admin_censor_isoyearweek requires 'isoyearweek' column"
  )
})

# =============================================================================
# enrollment_stage active binding tests
# =============================================================================

test_that("enrollment_stage returns pre_enrollment for person_week data", {
  dt <- data.table::data.table(
    id = 1:10,
    exposed = TRUE,
    eligible = TRUE,
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L,
    eligible_var = "eligible"
  )

  trial <- TTEEnrollment$new(dt, design)
  expect_equal(trial$enrollment_stage, "pre_enrollment")
})

test_that("enrollment_stage returns enrolled for trial level data", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)
  expect_equal(trial$enrollment_stage, "enrolled")
})

test_that("enrollment_stage returns analysis_ready after prepare_outcome", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:10, each = 4),
    tstart = rep(c(0, 4, 8, 12), 10),
    tstop = rep(c(4, 8, 12, 16), 10),
    exposed = rep(TRUE, 40),
    current_exposed = rep(TRUE, 40),
    age = rep(50, 40),
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 16L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$.__enclos_env__$private$s5_prepare_outcome(outcome = "death")
  expect_equal(trial$enrollment_stage, "analysis_ready")
})

test_that("enrollment_stage is read-only", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    death = 0L
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)
  expect_error(trial$enrollment_stage <- "foo")
})

# =============================================================================
# prepare_for_analysis tests
# =============================================================================

test_that("prepare_for_analysis combines prepare_outcome and ipcw_pp", {
  set.seed(42)
  n_trials <- 100
  n_periods <- 10

  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:n_trials, each = n_periods),
    tstart = rep(seq(0, by = 4, length.out = n_periods), n_trials),
    tstop = rep(seq(4, by = 4, length.out = n_periods), n_trials),
    exposed = rep(as.logical(rbinom(n_trials, 1, 0.3)), each = n_periods),
    current_exposed = rep(as.logical(rbinom(n_trials, 1, 0.3)), each = n_periods),
    id = rep(1:n_trials, each = n_periods),
    age = factor(rep(sample(1:4, n_trials, replace = TRUE), each = n_periods)),
    death = as.integer(runif(n_trials * n_periods) < 0.01),
    ipw = rep(runif(n_trials, 0.5, 2), each = n_periods)
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 40L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- c(trial$weight_cols, "ipw")
  trial$s4_prepare_for_analysis(outcome = "death", estimate_ipcw_pp_with_gam = TRUE)

  # Should have completed prepare_outcome and ipcw steps
  expect_true("prepare_outcome" %in% trial$steps_completed)
  expect_true("ipcw" %in% trial$steps_completed)
  expect_equal(trial$active_outcome, "death")
  expect_equal(trial$enrollment_stage, "analysis_ready")

  # Should have analysis weight columns
  expect_true("analysis_weight_pp" %in% names(trial$data))
  expect_true("analysis_weight_pp_trunc" %in% names(trial$data))
})

# =============================================================================
# $rates() tests
# =============================================================================

test_that("rates() calculates events and person-time by exposure group", {
  set.seed(42)
  n_trials <- 100
  n_periods <- 5

  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:n_trials, each = n_periods),
    tstart = rep(seq(0, by = 4, length.out = n_periods), n_trials),
    tstop = rep(seq(4, by = 4, length.out = n_periods), n_trials),
    exposed = rep(c(rep(TRUE, 50), rep(FALSE, 50)), each = n_periods),
    id = rep(1:n_trials, each = n_periods),
    age = factor(rep(sample(1:4, n_trials, replace = TRUE), each = n_periods)),
    event = as.integer(runif(n_trials * n_periods) < 0.02),
    person_weeks = rep(4L, n_trials * n_periods),
    ipw = rep(runif(n_trials, 0.5, 2), each = n_periods)
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "event",
    confounder_vars = "age",
    follow_up_time = 20L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "ipw"
  trial$steps_completed <- c("enroll", "ipw", "prepare_outcome")

  result <- trial$rates(weight_col = "ipw")

  expect_true(data.table::is.data.table(result))
  expect_true("exposed" %in% names(result))
  expect_true("events_weighted" %in% names(result))
  expect_true("py_weighted" %in% names(result))
  expect_true("rate_per_100000py" %in% names(result))
  expect_equal(nrow(result), 2)  # one row per exposure group
  expect_true(all(result$py_weighted > 0))
  expect_equal(attr(result, "swereg_type"), "rates")
})

test_that("rates() requires event and person_weeks columns", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = TRUE,
    age = 50,
    ipw = 1
  )

  design <- TTEDesign$new(
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "ipw"

  expect_error(trial$rates(weight_col = "ipw"), "event.*column not found")
})

# =============================================================================
# $irr() tests
# =============================================================================

test_that("irr() fits Poisson model and returns expected output", {
  set.seed(42)
  n_trials <- 200
  n_periods <- 5

  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:n_trials, each = n_periods),
    tstart = rep(seq(0, by = 4, length.out = n_periods), n_trials),
    tstop = rep(seq(4, by = 4, length.out = n_periods), n_trials),
    exposed = rep(c(rep(TRUE, 100), rep(FALSE, 100)), each = n_periods),
    id = rep(1:n_trials, each = n_periods),
    age = factor(rep(sample(1:4, n_trials, replace = TRUE), each = n_periods)),
    event = as.integer(runif(n_trials * n_periods) < 0.02),
    person_weeks = rep(4L, n_trials * n_periods),
    analysis_weight_pp_trunc = rep(runif(n_trials, 0.8, 1.2), each = n_periods)
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "event",
    confounder_vars = "age",
    follow_up_time = 20L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "analysis_weight_pp_trunc"
  trial$steps_completed <- c("enroll", "ipw", "prepare_outcome", "ipcw", "weights", "truncate")

  result <- trial$irr(weight_col = "analysis_weight_pp_trunc")

  expect_true(data.table::is.data.table(result))
  expect_true("IRR" %in% names(result))
  expect_true("IRR_lower" %in% names(result))
  expect_true("IRR_upper" %in% names(result))
  expect_true("IRR_pvalue" %in% names(result))
  expect_true("warn" %in% names(result))
  expect_equal(nrow(result), 1)
  expect_true(result$IRR > 0)
  expect_true(result$IRR_lower < result$IRR_upper)
  expect_equal(attr(result, "swereg_type"), "irr")
})

test_that("irr() includes trial_id when available", {
  set.seed(42)
  n_trials <- 200
  n_periods <- 5

  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:n_trials, each = n_periods),
    tstart = rep(seq(0, by = 4, length.out = n_periods), n_trials),
    tstop = rep(seq(4, by = 4, length.out = n_periods), n_trials),
    exposed = rep(c(rep(TRUE, 100), rep(FALSE, 100)), each = n_periods),
    id = rep(1:n_trials, each = n_periods),
    age = factor(rep(sample(1:4, n_trials, replace = TRUE), each = n_periods)),
    event = as.integer(runif(n_trials * n_periods) < 0.02),
    person_weeks = rep(4L, n_trials * n_periods),
    analysis_weight_pp_trunc = rep(runif(n_trials, 0.8, 1.2), each = n_periods),
    trial_id = rep(sample(1:10, n_trials, replace = TRUE), each = n_periods)
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "event",
    confounder_vars = "age",
    follow_up_time = 20L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "analysis_weight_pp_trunc"
  trial$steps_completed <- c("enroll", "ipw", "prepare_outcome", "ipcw", "weights", "truncate")

  # Should not error — trial_id included with ns() for >= 5 unique values
  result <- trial$irr(weight_col = "analysis_weight_pp_trunc")
  expect_true(data.table::is.data.table(result))
  expect_true(result$IRR > 0)
})

test_that("irr() rejects IPW-only weights after per-protocol censoring", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = TRUE,
    id = 1:10,
    age = 50,
    event = 0L,
    person_weeks = 4L,
    ipw = 1
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "ipw"
  trial$steps_completed <- c("enroll", "ipw", "prepare_outcome")

  expect_error(
    trial$irr(weight_col = "ipw"),
    "Cannot use 'ipw' as weight_col after per-protocol censoring"
  )

  # ipw_trunc should also be rejected
  dt$ipw_trunc <- 1
  trial2 <- TTEEnrollment$new(dt, design)
  trial2$weight_cols <- c("ipw", "ipw_trunc")
  trial2$steps_completed <- c("enroll", "ipw", "prepare_outcome")

  expect_error(
    trial2$irr(weight_col = "ipw_trunc"),
    "Cannot use 'ipw_trunc' as weight_col after per-protocol censoring"
  )
})

test_that("irr() requires event and person_weeks columns", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    exposed = TRUE,
    id = 1:10,
    age = 50,
    ipw = 1
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "ipw"

  expect_error(trial$irr(weight_col = "ipw"), "event.*column not found")
})

# =============================================================================
# $km() tests
# =============================================================================

test_that("km() returns svykm object with person-level clustering", {
  set.seed(42)
  n_trials <- 100

  # Create one row per trial (final row only, as km uses .SD[.N])
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:n_trials,
    tstart = rep(0L, n_trials),
    tstop = sample(4:20, n_trials, replace = TRUE),
    exposed = c(rep(TRUE, 50), rep(FALSE, 50)),
    id = 1:n_trials,
    age = sample(30:70, n_trials, replace = TRUE),
    event = as.integer(runif(n_trials) < 0.1),
    ipw = runif(n_trials, 0.8, 1.2)
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "event",
    confounder_vars = "age",
    follow_up_time = 20L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "ipw"
  trial$steps_completed <- c("enroll", "ipw", "prepare_outcome")

  result <- trial$km(ipw_col = "ipw")

  # svykm returns a list of strata
  expect_true(is.list(result))
  expect_true(length(result) >= 2)  # at least 2 strata (exposed/unexposed)
})

test_that("km() requires event column", {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:10,
    tstop = 1:10,
    exposed = TRUE,
    id = 1:10,
    age = 50,
    ipw = 1
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- "ipw"

  expect_error(trial$km(ipw_col = "ipw"), "event.*column not found")
})

# =============================================================================
# IPCW formula includes trial_id test
# =============================================================================

test_that("IPCW censoring model includes trial_id when data has multiple trial IDs", {
  # This test verifies the IPCW formula construction logic indirectly
  # by checking that s6_ipcw_pp runs without error when trial_id is present
  set.seed(42)
  n_trials <- 200
  n_periods <- 5

  dt <- data.table::data.table(
    enrollment_person_trial_id = rep(1:n_trials, each = n_periods),
    tstart = rep(seq(0, by = 4, length.out = n_periods), n_trials),
    tstop = rep(seq(4, by = 4, length.out = n_periods), n_trials),
    exposed = rep(c(rep(TRUE, 100), rep(FALSE, 100)), each = n_periods),
    current_exposed = rep(c(rep(TRUE, 100), rep(FALSE, 100)), each = n_periods),
    id = rep(1:n_trials, each = n_periods),
    age = factor(rep(sample(1:4, n_trials, replace = TRUE), each = n_periods)),
    death = as.integer(runif(n_trials * n_periods) < 0.01),
    ipw = rep(runif(n_trials, 0.5, 2), each = n_periods),
    trial_id = rep(sample(1:20, n_trials, replace = TRUE), each = n_periods)
  )

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    time_exposure_var = "current_exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 20L
  )

  trial <- TTEEnrollment$new(dt, design)
  trial$weight_cols <- c(trial$weight_cols, "ipw")

  # Use GLM (not GAM) to avoid smooth term issues with small test datasets
  trial$s4_prepare_for_analysis(outcome = "death", estimate_ipcw_pp_with_gam = FALSE)

  expect_true("ipcw_pp" %in% names(trial$data))
  expect_true("analysis_weight_pp" %in% names(trial$data))
})


# =============================================================================
# .assign_trial_ids() tests
# =============================================================================

test_that(".assign_trial_ids() produces consistent trial IDs from isoyearweek", {
  cstime_weeks <- cstime::dates_by_isoyearweek[, .(isoyearweek)]
  start_idx <- which(cstime_weeks$isoyearweek == "2020-01")
  week_range <- cstime_weeks$isoyearweek[start_idx:(start_idx + 19)]

  dt <- data.table::data.table(
    isoyearweek = week_range
  )

  swereg:::.assign_trial_ids(dt, period_width = 4L)

  # Should have trial_id column
  expect_true("trial_id" %in% names(dt))

  # With period_width=4, there should be at most ceil(20/4)+1 = 6 unique trial_ids
  # (band boundaries are global, so first/last bands may be partial)
  n_unique <- length(unique(dt$trial_id))
  expect_true(n_unique >= 4L && n_unique <= 6L)

  # Calling twice on different data with same isoyearweek should give same trial_id
  dt2 <- data.table::data.table(isoyearweek = week_range[5:10])
  swereg:::.assign_trial_ids(dt2, period_width = 4L)
  expect_equal(dt2$trial_id, dt$trial_id[5:10])
})

test_that(".assign_trial_ids() with period_width=1 gives unique IDs per week", {
  cstime_weeks <- cstime::dates_by_isoyearweek[, .(isoyearweek)]
  start_idx <- which(cstime_weeks$isoyearweek == "2020-01")
  week_range <- cstime_weeks$isoyearweek[start_idx:(start_idx + 3)]

  dt <- data.table::data.table(isoyearweek = week_range)
  swereg:::.assign_trial_ids(dt, period_width = 1L)

  # Each week should have a unique trial_id
  expect_equal(length(unique(dt$trial_id)), 4L)
})


# =============================================================================
# .s1_eligible_tuples() tests
# =============================================================================

test_that(".s1_eligible_tuples() returns correct tuples", {
  dt <- .make_person_week_data(n_exposed = 5, n_unexposed = 10, n_weeks = 8)

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L,
    period_width = 4L
  )

  # Need rd_exposed column (normally set by .s1_prepare_skeleton)
  dt[, rd_exposed := exposed]

  tuples <- swereg:::.s1_eligible_tuples(dt, design)

  # Should have person_id, trial_id, exposed columns
  expect_true("id" %in% names(tuples))
  expect_true("trial_id" %in% names(tuples))
  expect_true("exposed" %in% names(tuples))

  # Should have one row per (person, trial) — only eligible rows
  expect_true(nrow(tuples) > 0)
  # 15 persons, each eligible once -> 15 tuples
  expect_equal(nrow(tuples), 15L)

  # Exposed status should match
  expect_equal(sum(tuples$exposed == TRUE), 5L)
  expect_equal(sum(tuples$exposed == FALSE), 10L)
})


# =============================================================================
# enrolled_ids mode in enroll() tests
# =============================================================================

test_that("enroll with enrolled_ids skips matching and uses pre-decided IDs", {
  set.seed(42)
  dt <- .make_person_week_data(n_exposed = 10, n_unexposed = 40, n_weeks = 8)

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L,
    period_width = 4L
  )

  # First, find the actual trial_id that .assign_trial_ids would assign
  # for the first eligible week (same as in the data)
  probe <- data.table::copy(dt[id == 1 & eligible == TRUE, .(isoyearweek)])
  swereg:::.assign_trial_ids(probe, period_width = 4L)
  first_trial_id <- probe$trial_id[1]

  # Create pre-matched enrolled_ids: keep all 10 exposed + only 5 unexposed
  enrolled_ids <- data.table::data.table(
    id = c(1:10, 11:15),
    trial_id = rep(first_trial_id, 15),
    exposed = c(rep(TRUE, 10), rep(FALSE, 5)),
    enrollment_person_trial_id = paste0("01.", c(1:10, 11:15), ".", first_trial_id)
  )

  trial <- TTEEnrollment$new(
    dt, design,
    enrolled_ids = enrolled_ids,
    seed = 42,
    extra_cols = "isoyearweek"
  )

  expect_equal(trial$data_level, "trial")
  expect_true("enroll" %in% trial$steps_completed)

  # Check that exactly the pre-matched persons are enrolled
  enrolled_persons <- unique(trial$data$id)
  expect_true(all(1:15 %in% enrolled_persons))
  # Persons 16-50 should NOT be enrolled
  expect_false(any(16:50 %in% enrolled_persons))
})

test_that("enroll with enrolled_ids returns empty panel when no persons match", {
  dt <- .make_person_week_data(n_exposed = 5, n_unexposed = 10, n_weeks = 8)

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L,
    period_width = 4L
  )

  # enrolled_ids with person IDs that don't exist in dt (ids 1-15)
  enrolled_ids <- data.table::data.table(
    id = c(9001L, 9002L),
    trial_id = c(1L, 1L),
    exposed = c(TRUE, FALSE),
    enrollment_person_trial_id = c("01.9001.1", "01.9002.1")
  )

  trial <- TTEEnrollment$new(
    dt, design,
    enrolled_ids = enrolled_ids,
    seed = 42,
    extra_cols = "isoyearweek"
  )

  expect_equal(trial$data_level, "trial")
  expect_true("enroll" %in% trial$steps_completed)
  expect_equal(nrow(trial$data), 0L)
})

test_that("enroll with enrolled_ids=NULL preserves old matching behavior", {
  set.seed(42)
  dt <- .make_person_week_data(n_exposed = 10, n_unexposed = 40, n_weeks = 8)

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L,
    period_width = 4L
  )

  trial <- TTEEnrollment$new(dt, design, ratio = 2, seed = 42)

  expect_equal(trial$data_level, "trial")
  expect_true("enroll" %in% trial$steps_completed)
  # Should have enrolled via matching
  expect_true(nrow(trial$data) > 0)
})


# =============================================================================
# End-to-end two-batch matching test
# =============================================================================

test_that("centralized matching across two batches produces correct global ratio", {
  set.seed(42)
  # Batch A: 10 exposed, 3 unexposed (shortage)
  # Batch B: 2 exposed, 50 unexposed (surplus)
  # With ratio=5: need 60 unexposed total, 53 available
  # Per-batch would get: batch A: 3, batch B: 10 = 13 total
  # Centralized should get: min(60, 53) = 53 total

  design <- TTEDesign$new(
    person_id_var = "id",
    exposure_var = "exposed",
    eligible_var = "eligible",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 8L,
    period_width = 4L
  )

  # Create tuples (as .s1a_worker would return)
  tuples_a <- data.table::data.table(
    id = 1:13,
    trial_id = rep(0L, 13),
    exposed = c(rep(TRUE, 10), rep(FALSE, 3)),
    enrollment_person_trial_id = paste0("01.", 1:13, ".0")
  )
  tuples_b <- data.table::data.table(
    id = 14:65,
    trial_id = rep(0L, 52),
    exposed = c(rep(TRUE, 2), rep(FALSE, 50)),
    enrollment_person_trial_id = paste0("01.", 14:65, ".0")
  )

  all_tuples <- data.table::rbindlist(list(tuples_a, tuples_b))

  set.seed(123)
  x_ratio <- 5
  enrolled_ids <- all_tuples[, {
    exp_rows <- .SD[exposed == TRUE]
    unexp_rows <- .SD[exposed == FALSE]
    n_to_sample <- min(round(x_ratio * nrow(exp_rows)), nrow(unexp_rows))
    sampled <- if (n_to_sample > 0) unexp_rows[sample(.N, n_to_sample)] else unexp_rows[0]
    data.table::rbindlist(list(exp_rows, sampled))
  }, by = trial_id]

  n_enrolled_exposed <- sum(enrolled_ids$exposed == TRUE)
  n_enrolled_unexposed <- sum(enrolled_ids$exposed == FALSE)

  # All 12 exposed should be kept
  expect_equal(n_enrolled_exposed, 12L)
  # Should sample min(60, 53) = 53 unexposed
  expect_equal(n_enrolled_unexposed, 53L)
  # Compare with per-batch: batch A would get min(50, 3)=3, batch B min(10, 50)=10 -> 13
  # Centralized gets 53, which is much better
  expect_true(n_enrolled_unexposed > 13L)
})

test_that("centralized matching handles trial with 0 exposed", {
  tuples <- data.table::data.table(
    id = 1:10,
    trial_id = c(rep(0L, 5), rep(1L, 5)),
    exposed = c(rep(TRUE, 3), rep(FALSE, 2), rep(FALSE, 5))
  )

  set.seed(42)
  enrolled_ids <- tuples[, {
    exp_rows <- .SD[exposed == TRUE]
    unexp_rows <- .SD[exposed == FALSE]
    n_to_sample <- min(round(2 * nrow(exp_rows)), nrow(unexp_rows))
    sampled <- if (n_to_sample > 0) unexp_rows[sample(.N, n_to_sample)] else unexp_rows[0]
    data.table::rbindlist(list(exp_rows, sampled))
  }, by = trial_id]

  # Trial 0: 3 exposed + 2 unexposed (ratio=2 wants 6, only 2 available)
  trial0 <- enrolled_ids[trial_id == 0]
  expect_equal(sum(trial0$exposed == TRUE), 3L)
  expect_equal(sum(trial0$exposed == FALSE), 2L)

  # Trial 1: 0 exposed, so 0 unexposed sampled
  trial1 <- enrolled_ids[trial_id == 1]
  expect_equal(nrow(trial1), 0L)
})
