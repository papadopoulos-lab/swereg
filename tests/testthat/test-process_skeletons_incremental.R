# Integration tests for the incremental three-phase process_skeletons.
# These exercise the full orchestration path: phase 1 framework rebuild on
# hash change, phase 3 randvars divergence-point replay, phase 2 code
# registry per-entry sync.
#
# Each test builds a tiny synthetic RegistryStudy with 2 batches, a
# minimal framework fn, 0-3 randvars steps, and a few code registry
# entries. process_skeletons() is run, then edits are made and it's re-run
# to exercise the invalidation pathways.

library(data.table)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build a fresh 2-batch study, populate a trivial rawbatch, and return it.
# The rawbatch has a `grp1` group with columns (lopnr, val).
.mk_study <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:6)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:6, val = letters[1:6])
  )
  study
}

# Framework fn that builds a minimal skeleton from the grp1 rawbatch
.framework_fn_v1 <- function(batch_data, config) {
  data.table::data.table(
    id = batch_data[["grp1"]]$lopnr,
    isoyear = 2020L,
    isoyearweek = "2020-01",
    is_isoyear = FALSE
  )
}

# Randvars fn that adds a constant column. Closures capture the column
# name so we can register variants with different output columns.
.mk_randvars_fn <- function(col_name, col_value = 1L) {
  force(col_name)
  force(col_value)
  function(skeleton, batch_data, config) {
    skeleton[, (col_name) := col_value]
    invisible(skeleton)
  }
}

# ---------------------------------------------------------------------------
# First run
# ---------------------------------------------------------------------------

test_that("first run produces skeletons with the expected provenance", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  study$register_randvars("rv_a", .mk_randvars_fn("rv_a"))
  study$register_randvars("rv_b", .mk_randvars_fn("rv_b"))

  study$process_skeletons()

  expect_equal(length(study$skeleton_files), 2L)

  sk1 <- study$load_skeleton(1L)
  expect_s3_class(sk1, "Skeleton")
  expect_equal(sk1$framework_fn_hash, swereg:::.hash_function(.framework_fn_v1))
  expect_equal(names(sk1$randvars_state), c("rv_a", "rv_b"))
  expect_equal(sk1$applied_registry, list())
  expect_true(all(c("id", "isoyear", "rv_a", "rv_b") %in% names(sk1$data)))
  expect_identical(sk1$pipeline_identity(), study$pipeline_identity())
})

# ---------------------------------------------------------------------------
# No-change re-run is a no-op
# ---------------------------------------------------------------------------

test_that("re-running with no changes touches nothing", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  study$register_randvars("rv_a", .mk_randvars_fn("rv_a"))

  study$process_skeletons()
  sk1_first <- study$load_skeleton(1L)
  hash_first <- sk1_first$pipeline_identity()
  created_at_first <- sk1_first$created_at

  # Re-run; `created_at` on the Skeleton object is set in $new() which
  # runs only on a fresh rebuild. If nothing changed, the existing
  # Skeleton is loaded and re-saved without a new $new() call, so
  # created_at should stay the same.
  Sys.sleep(0.05) # ensure a different clock time if anything mis-wires
  study$process_skeletons()

  sk1_second <- study$load_skeleton(1L)
  expect_identical(sk1_second$pipeline_identity(), hash_first)
  expect_identical(sk1_second$created_at, created_at_first)
})

# ---------------------------------------------------------------------------
# Phase 2: code registry add / remove / modify
# ---------------------------------------------------------------------------

test_that("adding a code entry only applies to existing skeletons incrementally", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:6, val = letters[1:6])
  )

  # Register the framework + randvars only initially
  study$process_skeletons()
  sk1_before <- study$load_skeleton(1L)
  cols_before <- copy(names(sk1_before$data))

  # Now register a fake-fn code entry (one that just adds a TRUE column).
  # Reuse the fake_fn pattern from test-r6_skeleton.R.
  fake_fn <- function(skeleton, dataset, id_name, codes, ...) {
    for (col_name in names(codes)) {
      skeleton[, (col_name) := TRUE]
    }
    invisible(skeleton)
  }
  study$register_codes(
    codes = list(my_code = "X"),
    fn = fake_fn,
    groups = list("grp1")
  )

  study$process_skeletons()

  sk1_after <- study$load_skeleton(1L)
  expect_true("my_code" %in% names(sk1_after$data))
  # All the old columns are still there
  expect_true(all(cols_before %in% names(sk1_after$data)))
  # And the entry is recorded in applied_registry
  expect_equal(length(sk1_after$applied_registry), 1L)
})

test_that("removing a code entry drops its columns", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  fake_fn <- function(skeleton, dataset, id_name, codes, ...) {
    for (col_name in names(codes)) {
      skeleton[, (col_name) := TRUE]
    }
    invisible(skeleton)
  }
  study$register_codes(
    codes = list(my_code = "X"),
    fn = fake_fn,
    groups = list("grp1")
  )
  study$process_skeletons()

  sk1 <- study$load_skeleton(1L)
  expect_true("my_code" %in% names(sk1$data))

  # Now remove the code entry entirely by clearing code_registry. The
  # test reaches directly into the internal state since there's no
  # $deregister_codes() method.
  study$code_registry <- list()
  study$process_skeletons()

  sk1_after <- study$load_skeleton(1L)
  expect_false("my_code" %in% names(sk1_after$data))
  expect_equal(length(sk1_after$applied_registry), 0L)
})

test_that("modifying a code entry replaces its columns", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  fake_fn <- function(skeleton, dataset, id_name, codes, ...) {
    for (col_name in names(codes)) {
      skeleton[, (col_name) := TRUE]
    }
    invisible(skeleton)
  }
  # First registration: code named "foo"
  study$register_codes(
    codes = list(foo = "X"),
    fn = fake_fn,
    groups = list("grp1"),
    label = "mycode"
  )
  study$process_skeletons()
  sk1 <- study$load_skeleton(1L)
  expect_true("foo" %in% names(sk1$data))
  expect_false("bar" %in% names(sk1$data))

  # Replace the entry: same label, different code name -> different
  # fingerprint -> dropped + re-added.
  study$code_registry <- list() # reset
  study$register_codes(
    codes = list(bar = "Y"),
    fn = fake_fn,
    groups = list("grp1"),
    label = "mycode"
  )
  study$process_skeletons()

  sk1_after <- study$load_skeleton(1L)
  expect_false("foo" %in% names(sk1_after$data))
  expect_true("bar" %in% names(sk1_after$data))
})

# ---------------------------------------------------------------------------
# Phase 3: randvars add / remove / edit / reorder
# ---------------------------------------------------------------------------

test_that("editing a randvars step replays that step and everything downstream", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  # Each step is defined with a distinct body so hash changes are
  # detectable. .hash_function() hashes body(fn) + formals(fn); it does
  # NOT catch changes to closure-captured variables from an enclosing
  # environment, so we deliberately use bodies that differ by a literal.
  rv_a <- function(skeleton, batch_data, config) {
    skeleton[, rv_a := 1L]
    invisible(skeleton)
  }
  rv_b_v1 <- function(skeleton, batch_data, config) {
    skeleton[, rv_b := 1L]
    invisible(skeleton)
  }
  rv_c <- function(skeleton, batch_data, config) {
    skeleton[, rv_c := 1L]
    invisible(skeleton)
  }
  study$register_randvars("rv_a", rv_a)
  study$register_randvars("rv_b", rv_b_v1)
  study$register_randvars("rv_c", rv_c)

  study$process_skeletons()
  sk1 <- study$load_skeleton(1L)
  expect_equal(sk1$data$rv_b[[1]], 1L)
  expect_equal(sk1$data$rv_c[[1]], 1L)

  # Replace rv_b with a version that writes rv_b = 2L (literal body edit,
  # so the function hash changes).
  rv_b_v2 <- function(skeleton, batch_data, config) {
    skeleton[, rv_b := 2L]
    invisible(skeleton)
  }
  study$randvars_fns$rv_b <- rv_b_v2

  study$process_skeletons()

  sk1_after <- study$load_skeleton(1L)
  # rv_a unchanged from phase 3 step 1 (divergence point at step 2)
  expect_equal(sk1_after$data$rv_a[[1]], 1L)
  # rv_b replayed with new body -> rv_b = 2L
  expect_equal(sk1_after$data$rv_b[[1]], 2L)
  # rv_c cascade-replayed (same fn body, same output value)
  expect_equal(sk1_after$data$rv_c[[1]], 1L)
})

test_that("removing a phase-3 step drops its columns", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  study$register_randvars("rv_a", .mk_randvars_fn("rv_a"))
  study$register_randvars("rv_b", .mk_randvars_fn("rv_b"))
  study$process_skeletons()

  sk1 <- study$load_skeleton(1L)
  expect_true("rv_b" %in% names(sk1$data))

  # Remove rv_b
  study$randvars_fns$rv_b <- NULL
  study$process_skeletons()

  sk1_after <- study$load_skeleton(1L)
  expect_false("rv_b" %in% names(sk1_after$data))
  expect_true("rv_a" %in% names(sk1_after$data))
  expect_equal(names(sk1_after$randvars_state), "rv_a")
})

test_that("inserting a phase-3 step runs only the new step and any cascade", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  study$register_randvars("rv_a", .mk_randvars_fn("rv_a"))
  study$process_skeletons()

  # Insert rv_b after rv_a
  study$register_randvars("rv_b", .mk_randvars_fn("rv_b"))
  study$process_skeletons()

  sk1 <- study$load_skeleton(1L)
  expect_true(all(c("rv_a", "rv_b") %in% names(sk1$data)))
  expect_equal(names(sk1$randvars_state), c("rv_a", "rv_b"))
})

# ---------------------------------------------------------------------------
# Phase 1: framework change triggers full rebuild
# ---------------------------------------------------------------------------

test_that("editing the framework fn forces a full rebuild", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  study$register_randvars("rv_a", .mk_randvars_fn("rv_a"))
  study$process_skeletons()

  sk1 <- study$load_skeleton(1L)
  old_framework_hash <- sk1$framework_fn_hash

  # New framework fn: different code, different hash
  new_framework <- function(batch_data, config) {
    data.table::data.table(
      id = batch_data[["grp1"]]$lopnr,
      isoyear = 2021L, # changed from 2020
      isoyearweek = "2021-01",
      is_isoyear = FALSE
    )
  }
  study$framework_fn <- new_framework
  study$process_skeletons()

  sk1_after <- study$load_skeleton(1L)
  expect_false(identical(sk1_after$framework_fn_hash, old_framework_hash))
  expect_equal(sk1_after$data$isoyear[[1]], 2021L)
  # Phase 3 was re-applied on the new base (randvars_state should show it)
  expect_true("rv_a" %in% names(sk1_after$data))
  expect_equal(names(sk1_after$randvars_state), "rv_a")
})

# ---------------------------------------------------------------------------
# Pipeline hash invariant + skeleton_pipeline_hashes + assert_*
# ---------------------------------------------------------------------------

test_that("skeleton_pipeline_hashes shows uniform hash after a clean run", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  study$register_randvars("rv_a", .mk_randvars_fn("rv_a"))
  study$process_skeletons()

  h <- study$skeleton_pipeline_hashes()
  expect_s3_class(h, "data.table")
  expect_equal(nrow(h), 2L)
  expect_true(all(!is.na(h$identity_hash)))
  expect_equal(length(unique(h$identity_hash)), 1L)
  expect_equal(
    unique(h$identity_hash),
    swereg:::.pipeline_identity_hash(study$pipeline_identity())
  )
})

test_that("assert_skeletons_consistent is a no-op on clean state", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  study$process_skeletons()
  expect_identical(
    study$assert_skeletons_consistent(),
    swereg:::.pipeline_identity_hash(study$pipeline_identity())
  )
})

test_that("assert_skeletons_consistent errors on mixed hashes", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  study$process_skeletons()

  # Edit the framework out from under the skeletons (change it but don't
  # re-run process_skeletons). Now the disk skeletons have the OLD hash
  # while self$pipeline_identity() returns the NEW hash.
  study$framework_fn <- function(batch_data, config) {
    data.table::data.table(id = batch_data[["grp1"]]$lopnr, isoyear = 2099L)
  }

  expect_error(
    study$assert_skeletons_consistent(),
    "does not match this study"
  )
})

# ---------------------------------------------------------------------------
# load_skeleton errors on unknown file format
# ---------------------------------------------------------------------------

test_that("load_skeleton errors loudly when the file is not a Skeleton R6", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  study$register_randvars("rv_a", .mk_randvars_fn("rv_a"))

  # Drop a bare-data.table file in place for batch 1
  stray_dt <- data.table::data.table(id = 1:3, stray = TRUE)
  stray_path <- file.path(
    study$data_skeleton_dir,
    "skeleton_00001.qs2"
  )
  qs2::qs_save(stray_dt, stray_path)

  expect_error(
    study$load_skeleton(1L),
    "not a Skeleton R6 object"
  )
})

# ---------------------------------------------------------------------------
# Guard: framework_fn required
# ---------------------------------------------------------------------------

test_that("process_skeletons errors when framework_fn is not registered", {
  study <- .mk_study()
  expect_error(
    study$process_skeletons(),
    "framework_fn"
  )
})

# ---------------------------------------------------------------------------
# The stored registry order is normalized by the PRODUCTION path
# ---------------------------------------------------------------------------
#
# The reachability witness for the 2026-09-21 fix. It drives the real
# `process_skeletons()` path, because a unit test calling the sync helper
# directly stays green even when nothing calls it: deleting the call in
# `.process_one_batch()` broke no test until this one existed.
#
# It also covers the case a method-based fix could never reach. The skeleton
# read on the second run was DESERIALIZED, so it carries the method bodies of
# whatever swereg wrote it; only a free function called from the worker
# applies to it.

test_that("editing a non-final code entry leaves the stored order normalized", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  fake_fn <- function(skeleton, dataset, id_name, codes, ...) {
    for (col_name in names(codes)) {
      skeleton[, (col_name) := TRUE]
    }
    invisible(skeleton)
  }
  study$register_codes(
    codes = list(aa = "A"),
    fn = fake_fn,
    groups = list("grp1")
  )
  study$register_codes(
    codes = list(bb = "B"),
    fn = fake_fn,
    groups = list("grp1")
  )
  invisible(utils::capture.output(study$process_skeletons(n_workers = 1L)))

  before <- study$code_registry_fingerprints()
  expect_identical(names(study$load_skeleton(1L)$applied_registry), before)

  # Edit the FIRST entry. It is dropped and re-applied at the END, which is
  # exactly what put job 79's store out of registration order.
  study$code_registry[[1]]$codes <- list(aa = "AZ")
  after <- study$code_registry_fingerprints()
  expect_false(identical(after[[1]], before[[1]]))
  expect_identical(after[[2]], before[[2]])

  invisible(utils::capture.output(study$process_skeletons(n_workers = 1L)))

  stored <- names(study$load_skeleton(1L)$applied_registry)
  expect_identical(stored, after)

  # The whole point: the end gate accepts, and the per-batch fast path would
  # match on the next run rather than reloading every skeleton to do nothing.
  expect_identical(
    swereg:::.first_identity_difference(
      swereg:::.stored_pipeline_identity(study$load_skeleton(1L)),
      study$pipeline_identity()
    ),
    NULL
  )
})

# ---------------------------------------------------------------------------
# Phase 2 replays DEPENDENTS, not just the entry that changed
# ---------------------------------------------------------------------------
#
# `.apply_code_entry_impl()` hands a registered function the whole skeleton,
# so entry B may read a column entry A wrote. Set-difference invalidation
# re-applied only the CHANGED entry, so editing A left B holding a value
# derived from the old A. Nothing recorded that B was stale, and after the
# 2026-09-21 order fix the gate would have reported the skeleton as current.
#
# Divergence-point rewind and replay is what makes the incremental result
# equal the clean-build result.

test_that("editing an entry replays the entries that read it", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)

  # A writes `x`. B reads `x` and writes `y`. B is registered after A, so a
  # correct phase 2 must rebuild B whenever A changes.
  fn_a <- function(skeleton, dataset, id_name, codes, ...) {
    skeleton[, x := codes[[1]]]
    invisible(skeleton)
  }
  fn_b <- function(skeleton, dataset, id_name, codes, ...) {
    skeleton[, y := x * 10L]
    invisible(skeleton)
  }
  study$register_codes(codes = list(x = 1L), fn = fn_a, groups = list("grp1"))
  study$register_codes(codes = list(y = 0L), fn = fn_b, groups = list("grp1"))
  invisible(utils::capture.output(study$process_skeletons(n_workers = 1L)))

  sk <- study$load_skeleton(1L)
  expect_identical(unique(sk$data$x), 1L)
  expect_identical(unique(sk$data$y), 10L)

  # Change A. B's own fingerprint does not move.
  before <- study$code_registry_fingerprints()
  study$code_registry[[1]]$codes <- list(x = 7L)
  after <- study$code_registry_fingerprints()
  expect_false(identical(after[[1]], before[[1]]))
  expect_identical(after[[2]], before[[2]])

  invisible(utils::capture.output(study$process_skeletons(n_workers = 1L)))

  sk2 <- study$load_skeleton(1L)
  expect_identical(unique(sk2$data$x), 7L)
  # The defect: set difference left y at 10 because B never re-ran.
  expect_identical(unique(sk2$data$y), 70L)
  # And the stored order is registration order by construction.
  expect_identical(names(sk2$applied_registry), after)
})

test_that("reordering the registry re-runs the entries that moved", {
  study <- .mk_study()
  study$register_framework(.framework_fn_v1)
  fn_a <- function(skeleton, dataset, id_name, codes, ...) {
    skeleton[, x := codes[[1]]]
    invisible(skeleton)
  }
  # Reads `x` if an earlier entry wrote it, and records -1 when it did not.
  # That makes "did this entry re-run, and in what position" observable in
  # the DATA rather than only in the recorded provenance.
  fn_b <- function(skeleton, dataset, id_name, codes, ...) {
    skeleton[, y := if ("x" %in% names(skeleton)) x * 10L else -1L]
    invisible(skeleton)
  }
  study$register_codes(codes = list(x = 1L), fn = fn_a, groups = list("grp1"))
  study$register_codes(codes = list(y = 0L), fn = fn_b, groups = list("grp1"))
  invisible(utils::capture.output(study$process_skeletons(n_workers = 1L)))
  expect_identical(unique(study$load_skeleton(1L)$data$y), 10L)

  # A pure permutation: the fingerprint SET is unchanged, so set-difference
  # invalidation did no work at all and merely relabelled the provenance.
  study$code_registry <- study$code_registry[c(2L, 1L)]
  fps <- study$code_registry_fingerprints()
  invisible(utils::capture.output(study$process_skeletons(n_workers = 1L)))

  sk <- study$load_skeleton(1L)
  expect_identical(names(sk$applied_registry), fps)
  # B now runs FIRST, so it sees no `x`. Under set-difference invalidation
  # nothing re-ran and y stayed 10, with the provenance claiming otherwise.
  expect_identical(unique(sk$data$y), -1L)
  expect_null(
    swereg:::.first_identity_difference(
      swereg:::.stored_pipeline_identity(sk),
      study$pipeline_identity()
    )
  )
})
