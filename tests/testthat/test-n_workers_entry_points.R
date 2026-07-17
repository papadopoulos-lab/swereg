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

# EARLIEST line (in a deparse) matching ANY of `patterns`, or NA.
#
# This function had a bug worth a comment: the first version returned the first
# hit of the FIRST pattern that matched, not the minimum across all patterns. So
# in save_rawbatch(), a late `mirai::daemons(` could be "found" while an earlier
# `as.integer(n_workers)` was ignored -- meaning the convert-before-validate
# regression could return and the guard stay green. The point of an "earliest
# destructive op" check is the minimum over the whole set.
.first_line_matching <- function(lines, patterns) {
  hits <- integer(0)
  for (pat in patterns) hits <- c(hits, grep(pat, lines))
  if (length(hits)) min(hits) else NA_integer_
}

.body_lines <- function(fn) {
  expect_false(is.null(fn))
  deparse(body(fn))
}

test_that("save_rawbatch rejects an invalid n_workers even on the already-saved path", {
  # BEHAVIOURAL, not a deparse ordering guard. The "group already saved" early
  # return used to sit ahead of validation, so an invalid explicit count (or a
  # bad SWEREG_N_WORKERS_RAWBATCH) could return success on a group that was
  # already done -- validation at every entry, silently skipped.
  dir <- tempfile("rs_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  study <- swereg::RegistryStudy$new(data_rawbatch_dir = dir)
  # Mark a group as already saved so we hit the early-return path.
  study$groups_saved <- "lmed"

  expect_error(study$save_rawbatch(group = "lmed", n_workers = 2.5),
               "save_rawbatch")
  expect_error(study$save_rawbatch(group = "lmed", n_workers = 0L),
               "save_rawbatch")
})

test_that("an invalid n_workers destroys no R6 state", {
  # Validation must precede destructive self$ changes, or a rejected call leaves
  # the object corrupted. The load-bearing case codex flagged: process_skeletons
  # used to invalidate the committed skeleton manifest BEFORE inspecting
  # n_workers, so an NA count wiped the manifest and only then errored.
  dir <- tempfile("rs2_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  study <- swereg::RegistryStudy$new(data_rawbatch_dir = dir)
  study$skeleton_manifest <- list(sentinel = "PRECIOUS")

  expect_error(study$process_skeletons(n_workers = 2.5), "process_skeletons")
  # the manifest must survive a rejected call untouched
  expect_identical(study$skeleton_manifest$sentinel, "PRECIOUS")

  # s1: a rejected call must not have overwritten self$output_dir first.
  plan <- swereg::TTEPlan$new(
    project_prefix = "t", skeleton_files = "s_001.qs2",
    global_max_isoyearweek = "2020-52"
  )
  before <- plan$output_dir
  expect_error(
    plan$s1_generate_enrollments_and_ipw(output_dir = "/bad/xyz", n_workers = 2.5),
    "s1_generate"
  )
  expect_identical(plan$output_dir, before)
})

test_that("s3_analyze validates n_workers before dividing or launching", {
  lines <- .body_lines(swereg::TTEPlan$public_methods$s3_analyze)
  v <- .first_line_matching(lines, "\\.validate_n_workers\\(")
  d <- .first_line_matching(lines, c("\\.threads_per_worker\\(", "parallel_pool\\("))
  expect_false(is.na(v))
  expect_false(is.na(d))
  expect_lt(v, d)
})

test_that("s1 validates n_workers before mutating self, dividing, or clearing the work dir", {
  lines <- .body_lines(swereg::TTEPlan$public_methods$s1_generate_enrollments_and_ipw)
  v <- .first_line_matching(lines, "\\.validate_n_workers\\(")
  # earliest of: self$ mutation, thread division, pool dispatch, work-dir removal
  d <- .first_line_matching(lines, c(
    "self\\$output_dir <-", "\\.threads_per_worker\\(", "parallel_pool\\(", "unlink\\("
  ))
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

test_that("save_rawbatch validates n_workers before the early return, daemons, or convert", {
  lines <- .body_lines(swereg::RegistryStudy$public_methods$save_rawbatch)
  v <- .first_line_matching(lines, "\\.validate_n_workers\\(")
  # earliest of: the already-saved early return, daemon setup, a write, or the
  # as.integer() convert-before-validate the fix removed.
  d <- .first_line_matching(lines, c(
    "groups_saved", "mirai::daemons\\(", "qs2_write_atomic\\(", "as.integer\\(n_workers\\)"
  ))
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
  # earliest of: the randvars_fns mutation, the manifest destruction, division
  d <- .first_line_matching(lines, c(
    "self\\$randvars_fns <-", "invalidate_skeleton_manifest\\(", "\\.threads_per_worker\\("
  ))
  expect_false(is.na(v))
  expect_false(is.na(d))
  expect_lt(v, d)
})

test_that("EVERY public method with an n_workers arg validates it (derived, not hard-coded)", {
  # Anti-drift, discovered from the class definitions rather than a fixed list:
  # a hard-coded inventory cannot notice a NEW dispatching entry point. Any
  # public method of either R6 generator that takes an `n_workers` formal must
  # call .validate_n_workers() in its body -- otherwise it is a new door with no
  # lock.
  methods_with_n_workers <- function(generator) {
    pm <- generator$public_methods
    Filter(function(m) is.function(m) && "n_workers" %in% names(formals(m)), pm)
  }
  entries <- c(
    methods_with_n_workers(swereg::TTEPlan),
    methods_with_n_workers(swereg::RegistryStudy)
  )
  # Not vacuous: we know there are several. If discovery breaks, fail loudly
  # rather than silently checking nothing.
  expect_gte(length(entries), 5L)

  for (nm in names(entries)) {
    lines <- deparse(body(entries[[nm]]))
    expect_true(any(grepl("\\.validate_n_workers\\(", lines)),
      info = paste(nm, "takes n_workers but never validates it"))
  }
})
