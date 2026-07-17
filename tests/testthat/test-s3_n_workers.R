# Pin the input validation for the n_workers argument added to
# `s3_analyze()` in 26.4.27. The validation runs at the top of the
# function body, before any output_dir / file work, so a minimal plan
# is enough to exercise it.
#
# 26.7.20: s3_analyze() now delegates to the shared `.validate_n_workers()`,
# which also rejects FRACTIONAL counts -- previously `s3_analyze(2.5)` passed
# this block and was then silently truncated to 2 by `as.integer()`, so
# `parallel_pool()`'s own guard never saw the bad value. The error wording
# changed with it, so the pattern lives in ONE place here: the accept-side tests
# below assert the error is ABSENT, and a stale literal there would pass
# vacuously forever rather than fail loudly.

skip_if_not_installed("data.table")

# Matches both the old ("a single integer >= 1") and current ("a single finite
# whole number >= 1") wording: specific enough to prove it is the n_workers
# check and not some unrelated downstream error, loose enough to survive a
# rewording.
.n_workers_err <- "n_workers must be a single"

.minimal_plan <- function() {
  swereg::TTEPlan$new(
    project_prefix = "test_plan",
    skeleton_files = "skel_001.qs2",
    global_max_isoyearweek = "2020-52"
  )
}

test_that("s3_analyze: n_workers must be numeric", {
  plan <- .minimal_plan()
  expect_error(plan$s3_analyze(n_workers = "two"), .n_workers_err)
  expect_error(plan$s3_analyze(n_workers = TRUE), .n_workers_err)
})

test_that("s3_analyze: n_workers must be length 1", {
  plan <- .minimal_plan()
  expect_error(plan$s3_analyze(n_workers = c(1L, 2L)), .n_workers_err)
  expect_error(plan$s3_analyze(n_workers = integer(0)), .n_workers_err)
})

test_that("s3_analyze: n_workers must not be NA", {
  plan <- .minimal_plan()
  expect_error(plan$s3_analyze(n_workers = NA_integer_), .n_workers_err)
  expect_error(plan$s3_analyze(n_workers = NA_real_), .n_workers_err)
})

test_that("s3_analyze: n_workers must be >= 1", {
  plan <- .minimal_plan()
  expect_error(plan$s3_analyze(n_workers = 0L), .n_workers_err)
  expect_error(plan$s3_analyze(n_workers = -1L), .n_workers_err)
})

test_that("s3_analyze: a FRACTIONAL n_workers is rejected, not truncated", {
  # New in 26.7.20. `s3_analyze(2.5)` used to pass this block (it checked
  # >= 1 but never whole-ness) and then become 2 via as.integer(), so the user
  # silently got a different worker count than they asked for and
  # parallel_pool()'s validation never saw 2.5 at all.
  plan <- .minimal_plan()
  expect_error(plan$s3_analyze(n_workers = 2.5), .n_workers_err)
  expect_error(plan$s3_analyze(n_workers = 0.5), .n_workers_err)
})

test_that("s3_analyze: valid n_workers passes the validation block", {
  # Confirm valid n_workers values get past the validation by checking
  # that the failure (if any) is from the next phase (output_dir
  # resolution), not the n_workers check.
  plan <- .minimal_plan()
  for (n in c(1L, 2L, 4L, 8L, 2)) {   # a bare 2 is a double; must be accepted
    err <- tryCatch(plan$s3_analyze(n_workers = n),
                    error = function(e) conditionMessage(e))
    expect_false(grepl(.n_workers_err, err),
      info = paste0("n_workers = ", n, " was rejected by validation"))
  }
})

test_that("s3_analyze: default n_workers is accepted", {
  plan <- .minimal_plan()
  err <- tryCatch(plan$s3_analyze(),  # no n_workers arg
                  error = function(e) conditionMessage(e))
  expect_false(grepl(.n_workers_err, err))
})
