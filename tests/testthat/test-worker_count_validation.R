# Worker-count and core-count validation, at the PUBLIC entry points.
#
# The dispatcher validating its own n_workers is necessary but not sufficient:
# the callers used to convert BEFORE checking. `as.integer(2.5)` is 2, silently,
# so a fractional worker count never reached the pool's guard at all -- the
# validation existed and simply never saw the bad value. Validate first, convert
# second, and prove it at the doors users actually knock on.

test_that(".validate_n_workers() accepts real worker counts and rejects the rest", {
  ok <- c(1L, 2L, 6L, 2, 1.0)          # a plain 2 is a double in R; must pass
  for (v in ok) {
    expect_equal(swereg:::.validate_n_workers(v), as.integer(v), info = format(v))
  }
  # detectCores() feeds these paths; whatever it returns must be acceptable
  expect_silent(swereg:::.validate_n_workers(swereg:::.safe_n_cores()))

  bad <- list(2.5, 0L, -1L, NA_integer_, NA_real_, NaN, Inf, c(1L, 2L),
              "2", TRUE, NULL, integer(0))
  for (v in bad) {
    expect_error(
      swereg:::.validate_n_workers(v),
      regexp = "n_workers",
      info = paste("value:", paste(format(v), collapse = ","))
    )
  }
})

test_that(".validate_n_workers() names its caller in the error", {
  # A bare "n_workers must be..." from four call sites is a worse error than it
  # needs to be.
  expect_error(swereg:::.validate_n_workers(0L, "s3_analyze()"), "s3_analyze\\(\\)")
  expect_error(swereg:::.validate_n_workers(0L, "save_rawbatch()"), "save_rawbatch\\(\\)")
})

test_that("a fractional worker count is rejected, not silently truncated", {
  # THE regression. 2.5 used to become 2 with no word said, at every entry point.
  expect_error(swereg:::.validate_n_workers(2.5), "whole number")
  expect_error(swereg:::.validate_n_workers(0.9), "whole number")
  expect_error(swereg:::.validate_n_workers(1.0001), "whole number")
})

test_that("a whole double above the integer ceiling is rejected before coercion", {
  # Not cosmetic: it passes every OTHER check, but as.integer(2^40) is NA -- and
  # that NA used to flow past validation into callers that clear the manifest
  # before their own `n_workers <= 1L` trips on it, so a rejected count DID
  # mutate state. Reject it up front, like every other bad value.
  expect_error(swereg:::.validate_n_workers(2^40), "whole number")
  expect_error(swereg:::.validate_n_workers(.Machine$integer.max + 1), "whole number")
  # ... but the ceiling itself is fine, and round-trips as an INTEGER (not a
  # double that as.integer() would have to coerce) -- expect_identical pins that.
  expect_identical(swereg:::.validate_n_workers(.Machine$integer.max),
                   .Machine$integer.max)
})

test_that(".safe_n_cores() never returns NA, and .threads_per_worker() never returns 0", {
  # detectCores() is documented to return NA when it cannot tell. Unguarded,
  # that NA propagated into floor(NA / n_workers) -> NA threads, and only
  # surfaced much later inside a worker's setDTthreads(), far from the cause.
  expect_true(is.integer(swereg:::.safe_n_cores()))
  expect_gte(swereg:::.safe_n_cores(), 1L)

  testthat::local_mocked_bindings(
    detectCores = function(...) NA_integer_,
    .package = "parallel"
  )
  expect_equal(swereg:::.safe_n_cores(), 1L)
  expect_equal(swereg:::.safe_n_cores(fallback = 4L), 4L)
  expect_equal(swereg:::.threads_per_worker(1L), 1L)
  # ... and more workers than cores must still leave each worker a thread
  expect_gte(swereg:::.threads_per_worker(64L), 1L)
})

test_that(".threads_per_worker() divides sanely when cores ARE known", {
  testthat::local_mocked_bindings(
    detectCores = function(...) 8L,
    .package = "parallel"
  )
  expect_equal(swereg:::.threads_per_worker(1L), 8L)
  expect_equal(swereg:::.threads_per_worker(2L), 4L)
  expect_equal(swereg:::.threads_per_worker(3L), 2L)   # floor, not fractional
  expect_equal(swereg:::.threads_per_worker(16L), 1L)  # never zero
})

test_that("default_n_workers() validates configured values, it does not repair them", {
  # These names are baked into a production Docker image's Renviron. The old
  # code did `max(1L, as.integer(opt))`, which silently turned 2.5 into 2, 0 and
  # -1 into 1, and "abc" into NA -- so a typo in the deployed config ran a
  # different worker count than the operator asked for, with no complaint and no
  # way for downstream validation to recover the original value.
  withr::local_options(list(swereg.n_workers.testst = NULL))
  withr::local_envvar(c(SWEREG_N_WORKERS_TESTST = NA))

  # valid config is honoured
  withr::with_options(list(swereg.n_workers.testst = 3L),
    expect_equal(swereg::default_n_workers("testst"), 3L))
  withr::with_envvar(c(SWEREG_N_WORKERS_TESTST = "4"),
    expect_equal(swereg::default_n_workers("testst"), 4L))

  # invalid config is REJECTED, not truncated/clamped/NA'd
  withr::with_options(list(swereg.n_workers.testst = 2.5),
    expect_error(swereg::default_n_workers("testst"), "whole number|n_workers"))
  withr::with_options(list(swereg.n_workers.testst = 0L),
    expect_error(swereg::default_n_workers("testst"), "n_workers"))
  withr::with_envvar(c(SWEREG_N_WORKERS_TESTST = "abc"),
    expect_error(swereg::default_n_workers("testst"), "whole number"))
  withr::with_envvar(c(SWEREG_N_WORKERS_TESTST = "-1"),
    expect_error(swereg::default_n_workers("testst"), "n_workers"))

  # no config -> the documented default
  expect_equal(swereg::default_n_workers("testst"), 1L)
})

test_that("no package code calls parallel::detectCores() outside the guarded helper", {
  # The guard is only worth having if everything routes through it. The first
  # attempt fixed detectCores() inside parallel_pool() alone and left four other
  # live paths (s1, s2, process_skeletons, serial save_rawbatch) dividing an
  # unguarded NA.
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")

  files <- setdiff(list.files(r_dir, pattern = "\\.R$", full.names = TRUE),
                   file.path(r_dir, "default_n_workers.R"))  # where the helper lives
  offenders <- character()
  for (f in files) {
    code <- grep("^\\s*#", readLines(f, warn = FALSE), invert = TRUE, value = TRUE)
    hits <- grep("parallel::detectCores\\(", code, value = TRUE)
    if (length(hits)) offenders <- c(offenders, paste0(basename(f), ": ", trimws(hits)))
  }
  expect_equal(offenders, character(0),
    info = "use .safe_n_cores() / .threads_per_worker() instead")
})
