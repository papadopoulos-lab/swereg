# Pin the input validation for the n_workers argument added to
# `s3_analyze()` in 26.4.27. The validation runs at the top of the
# function body, before any output_dir / file work, so a minimal plan
# is enough to exercise it.

skip_if_not_installed("data.table")

.minimal_plan <- function() {
  swereg::TTEPlan$new(
    project_prefix = "test_plan",
    skeleton_files = "skel_001.qs2",
    global_max_isoyearweek = "2020-52"
  )
}

test_that("s3_analyze: n_workers must be numeric", {
  plan <- .minimal_plan()
  expect_error(
    plan$s3_analyze(n_workers = "two"),
    "n_workers must be a single integer"
  )
  expect_error(
    plan$s3_analyze(n_workers = TRUE),
    "n_workers must be a single integer"
  )
})

test_that("s3_analyze: n_workers must be length 1", {
  plan <- .minimal_plan()
  expect_error(
    plan$s3_analyze(n_workers = c(1L, 2L)),
    "n_workers must be a single integer"
  )
  expect_error(
    plan$s3_analyze(n_workers = integer(0)),
    "n_workers must be a single integer"
  )
})

test_that("s3_analyze: n_workers must not be NA", {
  plan <- .minimal_plan()
  expect_error(
    plan$s3_analyze(n_workers = NA_integer_),
    "n_workers must be a single integer"
  )
  expect_error(
    plan$s3_analyze(n_workers = NA_real_),
    "n_workers must be a single integer"
  )
})

test_that("s3_analyze: n_workers must be >= 1", {
  plan <- .minimal_plan()
  expect_error(
    plan$s3_analyze(n_workers = 0L),
    "n_workers must be a single integer"
  )
  expect_error(
    plan$s3_analyze(n_workers = -1L),
    "n_workers must be a single integer"
  )
})

test_that("s3_analyze: valid n_workers passes the validation block", {
  # Confirm valid n_workers values get past the validation by checking
  # that the failure (if any) is from the next phase (output_dir
  # resolution), not the n_workers check.
  plan <- .minimal_plan()
  for (n in c(1L, 2L, 4L, 8L)) {
    err <- tryCatch(plan$s3_analyze(n_workers = n),
                    error = function(e) conditionMessage(e))
    expect_false(grepl("n_workers must be a single integer", err),
      info = paste0("n_workers = ", n, " was rejected by validation"))
  }
})

test_that("s3_analyze: default n_workers (1L) is accepted", {
  plan <- .minimal_plan()
  err <- tryCatch(plan$s3_analyze(),  # no n_workers arg
                  error = function(e) conditionMessage(e))
  expect_false(grepl("n_workers must be a single integer", err))
})
