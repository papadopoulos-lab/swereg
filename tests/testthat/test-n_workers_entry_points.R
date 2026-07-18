# n_workers is validated at EVERY public entry, as the FIRST thing each does.
#
# The dispatcher validating its own argument is not enough: several public
# methods divide by, clear state with, or early-return on `n_workers` before the
# dispatcher ever sees it. A count of `1.5` selects two workers (`length(active) <
# 1.5`), `0`/`-1` silently go serial, `NA` invalidates a committed manifest, and
# a fractional value was truncated by an `as.integer()` that ran before any
# check. The guarantee that fixes all of these is: validation is the FIRST
# statement, so nothing observable happens before a bad count is rejected.
#
# This is asserted three ways, weakest to strongest:
#   1. structural  -- the first body expression IS the .validate_n_workers()
#      assignment (deterministic, fixture-free, catches ANY reordering);
#   2. derived      -- every R6 method with an n_workers formal has such a call
#      (so a NEW entry point cannot appear unguarded);
#   3. behavioural  -- a rejected call leaves observable state untouched, on
#      objects built far enough to actually reach the once-destructive path.
#
# Behavioural tests for the shared validator itself live in
# test-worker_count_validation.R; test-s3_n_workers.R covers s3 end to end.

.n_workers_entries <- function() {
  pick <- function(generator) {
    Filter(
      function(m) is.function(m) && "n_workers" %in% names(formals(m)),
      generator$public_methods
    )
  }
  c(pick(swereg::TTEPlan), pick(swereg::RegistryStudy))
}

test_that("validation is the FIRST body expression of every n_workers entry", {
  # The strongest guard, and the one that cannot be fooled by nested functions,
  # dead branches, or a destructive op missing from a pattern list: the method's
  # first statement must literally be `n_workers <- .validate_n_workers(...)`.
  # Move it down by even one line and this fails.
  entries <- .n_workers_entries()
  expect_gte(length(entries), 5L)   # not vacuous

  for (nm in names(entries)) {
    b <- body(entries[[nm]])
    expect_true(is.call(b) && identical(b[[1]], as.name("{")),
      info = paste(nm, "body is not a { } block"))
    first <- b[[2]]
    ok <- is.call(first) &&
      identical(first[[1]], as.name("<-")) &&
      identical(first[[2]], as.name("n_workers")) &&
      is.call(first[[3]]) &&
      identical(first[[3]][[1]], as.name(".validate_n_workers"))
    expect_true(ok,
      info = paste0(nm, ": first expression is `",
                    paste(deparse(first), collapse = " "),
                    "`, not `n_workers <- .validate_n_workers(...)`"))
  }
})

test_that("every R6 method with an n_workers formal calls the shared validator", {
  # Derived, not hard-coded: a new dispatching entry point cannot slip in
  # unguarded, because discovery walks the class definitions.
  entries <- .n_workers_entries()
  expect_gte(length(entries), 5L)
  for (nm in names(entries)) {
    lines <- deparse(body(entries[[nm]]))
    expect_true(any(grepl("\\.validate_n_workers\\(", lines)),
      info = paste(nm, "takes n_workers but never validates it"))
  }
})

test_that("save_rawbatch rejects an invalid n_workers on the already-saved path", {
  # Behavioural. The "group already saved" early return used to precede
  # validation, so an invalid explicit count (or a bad SWEREG_N_WORKERS_RAWBATCH)
  # could return success on a group that was already done.
  dir <- tempfile("rs_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  study <- swereg::RegistryStudy$new(data_rawbatch_dir = dir)
  study$groups_saved <- "lmed"   # force the early-return path

  expect_error(study$save_rawbatch(group = "lmed", n_workers = 2.5), "save_rawbatch")
  expect_error(study$save_rawbatch(group = "lmed", n_workers = 0L), "save_rawbatch")
})

test_that("an invalid n_workers to process_skeletons destroys no state", {
  # Behavioural, built far enough to REACH the once-destructive path. A fresh
  # study has no framework_fn, so the reverted code (validate AFTER the manifest
  # invalidation) would error at the framework check BEFORE touching the
  # manifest -- and the test would pass for the wrong reason (codex, round 5).
  # Registering a framework lets the reverted order actually reach and destroy
  # the manifest, so this genuinely distinguishes the fix from the bug.
  dir <- tempfile("rs2_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  study <- swereg::RegistryStudy$new(data_rawbatch_dir = dir)
  study$register_framework(function(bd, s) bd)      # get past the framework check
  study$skeleton_manifest <- list(sentinel = "PRECIOUS")

  expect_error(study$process_skeletons(n_workers = 2.5), "process_skeletons")
  # a rejected call must not have cleared the committed manifest
  expect_identical(study$skeleton_manifest$sentinel, "PRECIOUS")

  # ... and the same for the overflow case, which round 5 showed also reaches
  # the destruction (as.integer(2^40) is NA) unless rejected up front.
  study$skeleton_manifest <- list(sentinel = "STILL-HERE")
  expect_error(study$process_skeletons(n_workers = 2^40), "process_skeletons")
  expect_identical(study$skeleton_manifest$sentinel, "STILL-HERE")
})
