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
    outcome = "event", follow_up = max(long$tstop),
    file_imp_path = imp, file_analysis_path = itt,
    n_threads = 1L, sep_by_tx = TRUE, with_gam = TRUE, estimand = "itt")

  out <- swereg::qs2_read(itt, nthreads = 1L)
  expect_identical(out$estimand, "itt")
  expect_false("ipcw_pp" %in% names(out$data))     # ITT skips IPCW
  expect_true("ipw_trunc" %in% names(out$data))
  res <- out$irr(weight_col = "ipw_trunc")          # guard allows ITT weight
  expect_true(is.finite(res$IRR))
})

test_that(".s2_worker defaults to PP and still produces the IPCW weight", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("qs2")

  dt <- tte_simulate(N = 3000, persist_coef = 4, seed = 7)  # switching -> IPCW
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
    outcome = "event", follow_up = max(long$tstop),
    file_imp_path = imp, file_analysis_path = pp,
    n_threads = 1L, sep_by_tx = TRUE, with_gam = TRUE)

  out <- swereg::qs2_read(pp, nthreads = 1L)
  expect_identical(out$estimand, "pp")
  expect_true("analysis_weight_pp_trunc" %in% names(out$data))
})
