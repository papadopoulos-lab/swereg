# Plan-layer ITT production (increment A): the Loop-2 worker builds BOTH a
# per-protocol and an intention-to-treat analysis file off the same file_imp.
# These exercise .s2_worker directly with a file_imp-equivalent (an enrollment
# after IPW, before s4) so we don't need a full s1 plan run.

test_that(".s2_worker builds a valid ITT analysis file (no IPCW, tagged)", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("qs2")

  dt <- tte_simulate(N = 3000, true_lor = -0.7, persist_coef = 8, seed = 2026)
  long <- tte_build_long(dt)
  trial <- TTEEnrollment$new(long, tte_make_design(long))
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)

  imp <- tempfile(fileext = ".qs2")
  itt <- tempfile(fileext = ".qs2")
  on.exit(unlink(c(imp, itt)), add = TRUE)
  qs2::qs_save(trial, imp, nthreads = 1L)

  swereg:::.s2_worker(
    outcome = "event",
    follow_up = max(long$tstop),
    file_imp_path = imp,
    file_analysis_path = itt,
    n_threads = 1L,
    sep_by_tx = TRUE,
    with_gam = TRUE,
    estimand = "itt"
  )

  out <- swereg::qs2_read(itt, nthreads = 1L)
  expect_identical(out$estimand, "itt")
  expect_false("ipcw_pp" %in% names(out$data)) # ITT skips IPCW
  expect_true("ipw_trunc" %in% names(out$data))
  res <- out$irr(weight_col = "ipw_trunc") # guard allows ITT weight
  expect_true(is.finite(res$IRR))
})

test_that(".s2_worker defaults to PP and still produces the IPCW weight", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("qs2")

  dt <- tte_simulate(N = 3000, persist_coef = 4, seed = 7) # switching -> IPCW
  long <- tte_build_long(dt)
  trial <- TTEEnrollment$new(long, tte_make_design(long))
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)

  imp <- tempfile(fileext = ".qs2")
  pp <- tempfile(fileext = ".qs2")
  on.exit(unlink(c(imp, pp)), add = TRUE)
  qs2::qs_save(trial, imp, nthreads = 1L)

  # estimand omitted -> defaults to "pp"
  swereg:::.s2_worker(
    outcome = "event",
    follow_up = max(long$tstop),
    file_imp_path = imp,
    file_analysis_path = pp,
    n_threads = 1L,
    sep_by_tx = TRUE,
    with_gam = TRUE
  )

  out <- swereg::qs2_read(pp, nthreads = 1L)
  expect_identical(out$estimand, "pp")
  expect_true("analysis_weight_pp_trunc" %in% names(out$data))
})

test_that(".s3_ett_worker returns an irr_itt slot from an ITT analysis file", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("qs2")

  dt <- tte_simulate(N = 3000, true_lor = -0.7, persist_coef = 8, seed = 2026)
  long <- tte_build_long(dt)
  trial <- TTEEnrollment$new(long, tte_make_design(long))
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)

  imp <- tempfile(fileext = ".qs2")
  itt <- tempfile(fileext = ".qs2")
  on.exit(unlink(c(imp, itt)), add = TRUE)
  qs2::qs_save(trial, imp, nthreads = 1L)
  swereg:::.s2_worker(
    outcome = "event",
    follow_up = max(long$tstop),
    file_imp_path = imp,
    file_analysis_path = itt,
    n_threads = 1L,
    sep_by_tx = TRUE,
    with_gam = TRUE,
    estimand = "itt"
  )

  # The s3 worker derives the slot from the weight: ipw_trunc -> irr_itt.
  res <- swereg:::.s3_ett_worker(
    analysis_path = itt,
    method = "irr",
    weight_col = "ipw_trunc",
    ett_id = "ETT00001",
    n_threads = 1L
  )

  expect_named(res, "irr_itt")
  expect_false(isTRUE(res$irr_itt$skipped))
  expect_true(is.finite(res$irr_itt$IRR))

  rates <- swereg:::.s3_ett_worker(
    analysis_path = itt,
    method = "rates",
    weight_col = "ipw_trunc",
    ett_id = "ETT00001",
    n_threads = 1L
  )
  expect_named(rates, "rates_itt")
  expect_false(isTRUE(rates$rates_itt$skipped))
})

test_that(".s3_ett_worker computes subgroup + effect-modification slots", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("qs2")

  set.seed(3)
  N <- 4000L
  T_periods <- 10L
  Z <- stats::rbinom(N, 1, 0.5)
  A <- stats::rbinom(N, 1, 0.5)
  rows <- lapply(seq_len(T_periods), function(t) {
    data.table::data.table(
      id = seq_len(N), tstart = t - 1L, tstop = t,
      treatment = as.logical(A), Z = Z,
      event = stats::rbinom(N, 1, stats::plogis(-4 + log(2) * A + log(2) * A * Z)),
      person_weeks = 1L, w = 1
    )
  })
  em <- data.table::rbindlist(rows)
  design <- TTEDesign$new(
    id_var = "id", person_id_var = "id", treatment_var = "treatment",
    outcome_vars = "event", confounder_vars = "Z", subgroup_vars = "Z",
    follow_up_time = T_periods
  )
  trial <- TTEEnrollment$new(em, design, data_level = "trial")
  f <- tempfile(fileext = ".qs2")
  on.exit(unlink(f), add = TRUE)
  qs2::qs_save(trial, f, nthreads = 1L)

  sg <- swereg:::.s3_ett_worker(
    analysis_path = f, method = "irr_by_subgroup", weight_col = "w",
    ett_id = "ETT00001", n_threads = 1L, subgroup_var = "Z")
  expect_named(sg, "subgroup_Z_pp")     # non-itt weight -> pp suffix
  expect_equal(sg$subgroup_Z_pp$level, c("all", "0", "1"))

  emt <- swereg:::.s3_ett_worker(
    analysis_path = f, method = "effect_modification_test", weight_col = "w",
    ett_id = "ETT00001", n_threads = 1L, subgroup_var = "Z")
  expect_named(emt, "emtest_Z_pp")
  expect_equal(emt$emtest_Z_pp$n_levels, 2L)
})
