# n_workers is validated at EVERY public entry, BEFORE any destructive work.
#
# `parallel_pool()` validating its own argument is not enough: several public
# methods divide by, or clear state with, `n_workers` long before the pool ever
# sees it. A count of `1.5` selects two workers (`length(active) < 1.5`), `0`/
# `-1` silently go serial, and `NA` invalidates a committed manifest and *then*
# errors. So the guarantee that matters is ORDERING -- validation must come
# before the first irreversible step -- and it is checked here against the
# source of the functions that actually run, not a regex over a file.
#
# Behavioural tests for the shared validator live in
# test-worker_count_validation.R; test-s3_n_workers.R covers s3 end to end.
# This file's job is only "is it wired in, ahead of the damage, everywhere".

# Position (in a deparse) of the first line matching any of `patterns`, or NA.
.first_line_matching <- function(lines, patterns) {
  for (pat in patterns) {
    hit <- grep(pat, lines)
    if (length(hit)) return(hit[1L])
  }
  NA_integer_
}

.body_lines <- function(fn) {
  expect_false(is.null(fn))
  deparse(body(fn))
}

test_that("s3_analyze validates n_workers before dividing or launching", {
  lines <- .body_lines(swereg::TTEPlan$public_methods$s3_analyze)
  v <- .first_line_matching(lines, "\\.validate_n_workers\\(")
  d <- .first_line_matching(lines, c("\\.threads_per_worker\\(", "parallel_pool\\("))
  expect_false(is.na(v))
  expect_false(is.na(d))
  expect_lt(v, d)
})

test_that("s1 validates n_workers before dividing or clearing the work dir", {
  lines <- .body_lines(swereg::TTEPlan$public_methods$s1_generate_enrollments_and_ipw)
  v <- .first_line_matching(lines, "\\.validate_n_workers\\(")
  # first destructive/derived use: thread division, pool dispatch, or work-dir removal
  d <- .first_line_matching(lines, c("\\.threads_per_worker\\(", "parallel_pool\\(", "unlink\\("))
  expect_false(is.na(v))
  expect_false(is.na(d))
  expect_lt(v, d)
})

test_that("s2 validates n_workers before dividing or the resume early-return", {
  lines <- .body_lines(swereg::TTEPlan$public_methods$s2_generate_analysis_files_and_ipcw_pp)
  v <- .first_line_matching(lines, "\\.validate_n_workers\\(")
  d <- .first_line_matching(lines, c("\\.threads_per_worker\\(", "parallel_pool\\("))
  expect_false(is.na(v))
  expect_false(is.na(d))
  expect_lt(v, d)
})

test_that("save_rawbatch validates n_workers before touching daemons or writing", {
  lines <- .body_lines(swereg::RegistryStudy$public_methods$save_rawbatch)
  v <- .first_line_matching(lines, "\\.validate_n_workers\\(")
  d <- .first_line_matching(lines, c("mirai::daemons\\(", "qs2_write_atomic\\(", "as.integer\\("))
  expect_false(is.na(v))
  expect_false(is.na(d))
  expect_lt(v, d)
})

test_that("process_skeletons validates n_workers BEFORE invalidating the manifest", {
  # The worst ordering bug of the set: the manifest invalidation is destructive
  # state clearing, and it used to run before n_workers was inspected at all --
  # so NA invalidated a committed manifest and only then errored.
  lines <- .body_lines(swereg::RegistryStudy$public_methods$process_skeletons)
  v <- .first_line_matching(lines, "\\.validate_n_workers\\(")
  d <- .first_line_matching(lines, c("invalidate_skeleton_manifest\\(", "\\.threads_per_worker\\("))
  expect_false(is.na(v))
  expect_false(is.na(d))
  expect_lt(v, d)
})

test_that("every worker-count entry point routes through the shared validator", {
  # Anti-drift: if a new dispatching entry appears, or an existing one stops
  # calling the validator, this notices. Each named method's body must contain
  # a .validate_n_workers() call.
  entries <- list(
    s3_analyze = swereg::TTEPlan$public_methods$s3_analyze,
    s1 = swereg::TTEPlan$public_methods$s1_generate_enrollments_and_ipw,
    s2 = swereg::TTEPlan$public_methods$s2_generate_analysis_files_and_ipcw_pp,
    save_rawbatch = swereg::RegistryStudy$public_methods$save_rawbatch,
    process_skeletons = swereg::RegistryStudy$public_methods$process_skeletons
  )
  for (nm in names(entries)) {
    lines <- deparse(body(entries[[nm]]))
    expect_true(any(grepl("\\.validate_n_workers\\(", lines)),
      info = paste(nm, "does not validate n_workers"))
  }
})
