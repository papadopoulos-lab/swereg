# Tests for the meta-file sidecar architecture introduced alongside PR #4.
# The meta sidecar (`meta_%05d.qs2`) lives next to each skeleton and carries
# the provenance hashes + per-batch code-check accumulator snapshot. It
# enables two things: (a) the meta-only fast path in .process_one_batch()
# that avoids deserialising the heavy skeleton when nothing has changed,
# and (b) cross-process aggregation of code-check warnings via disk.

library(data.table)

# Same-shaped helpers as test-process_skeletons_incremental.R
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

.framework_fn <- function(batch_data, config) {
  # create_skeleton() returns a full skeleton with all the structural
  # columns add_*() expects (is_isoyear in particular).
  swereg::create_skeleton(
    ids       = batch_data[["grp1"]]$lopnr,
    date_min  = "2020-01-01",
    date_max  = "2020-01-31"
  )
}

# ---------------------------------------------------------------------------
# Phase 1: meta sidecar roundtrip
# ---------------------------------------------------------------------------

test_that("save_skeleton() writes both skeleton_*.qs2 AND meta_*.qs2", {
  study <- .mk_study()
  study$register_framework(.framework_fn)
  study$process_skeletons()

  for (i in 1:2) {
    sk_path   <- file.path(study$data_skeleton_dir, sprintf("skeleton_%05d.qs2", i))
    meta_path <- file.path(study$data_skeleton_dir, sprintf("meta_%05d.qs2", i))
    expect_true(file.exists(sk_path))
    expect_true(file.exists(meta_path))
  }
})

test_that("meta payload carries the expected fields", {
  study <- .mk_study()
  study$register_framework(.framework_fn)
  study$process_skeletons()

  meta <- study$load_skeleton_meta(1L)
  expect_type(meta, "list")
  expect_setequal(
    names(meta),
    c("schema_version", "swereg_version", "framework_fn_hash",
      "randvars_state", "applied_registry", "code_check_state",
      "n_rows", "built_at")
  )
  expect_identical(meta$schema_version, swereg:::.REGISTRY_STUDY_SCHEMA_VERSION)
  expect_equal(meta$framework_fn_hash, swereg:::.hash_function(.framework_fn))
  expect_gt(meta$n_rows, 0L)
  expect_s3_class(meta$built_at, "POSIXct")
})

test_that("meta is a derived cache: missing meta -> next run rebuilds it", {
  study <- .mk_study()
  study$register_framework(.framework_fn)
  study$process_skeletons()

  meta_path <- file.path(study$data_skeleton_dir, "meta_00001.qs2")
  expect_true(file.exists(meta_path))
  file.remove(meta_path)
  expect_false(file.exists(meta_path))

  # Re-run: meta is missing -> .process_one_batch() falls through to the
  # slow path, loads skeleton, hashes match, save_skeleton() rewrites
  # both files.
  study$process_skeletons()
  expect_true(file.exists(meta_path))
})

# ---------------------------------------------------------------------------
# Phase 2: meta-only fast path
# ---------------------------------------------------------------------------

test_that("fast path: 2nd no-change run does NOT deserialise the skeleton", {
  study <- .mk_study()
  study$register_framework(.framework_fn)
  study$process_skeletons()

  sk_path <- file.path(study$data_skeleton_dir, "skeleton_00001.qs2")
  mtime_before <- file.mtime(sk_path)

  # 2nd run with no changes -> meta hashes all match -> early return.
  # Skeleton file should not be touched (no rewrite).
  Sys.sleep(1.1)  # ensure mtime resolution catches any rewrite
  study$process_skeletons()
  mtime_after <- file.mtime(sk_path)

  expect_equal(mtime_before, mtime_after)
})

test_that("schema-version mismatch in meta forces a reload + rewrite", {
  study <- .mk_study()
  study$register_framework(.framework_fn)
  study$process_skeletons()

  # Hand-corrupt the meta to look like an older schema.
  meta_path <- file.path(study$data_skeleton_dir, "meta_00001.qs2")
  meta <- qs2::qs_read(meta_path)
  meta$schema_version <- 4L  # one less than current
  qs2::qs_save(meta, meta_path)

  sk_path <- file.path(study$data_skeleton_dir, "skeleton_00001.qs2")
  mtime_before <- file.mtime(sk_path)
  Sys.sleep(1.1)

  study$process_skeletons()
  mtime_after <- file.mtime(sk_path)

  # Skeleton should have been rewritten because schema mismatch forced
  # the slow path (and save_skeleton overwrites the file regardless of
  # whether anything actually changed).
  expect_true(mtime_after > mtime_before)

  # And meta is back to the current schema version.
  meta_post <- qs2::qs_read(meta_path)
  expect_identical(meta_post$schema_version, swereg:::.REGISTRY_STUDY_SCHEMA_VERSION)
})

# ---------------------------------------------------------------------------
# Phase 3: cross-batch code-check aggregation via meta
# ---------------------------------------------------------------------------

# A randvars step that registers a code-check warning by calling add_diagnoses
# directly. We synthesise a tiny diagnoses table and use codes that don't
# match anything in any batch -> the consolidated warning at end of
# process_skeletons() should mention the absent literal.
.randvars_with_unmatched_codes <- function(skeleton, batch_data, config) {
  diags <- data.table::data.table(
    lopnr = batch_data[["grp1"]]$lopnr,
    indatum = as.Date("2020-01-01"),
    hdia = "X00",  # placeholder; doesn't match the absent literal
    stringsAsFactors = FALSE
  )
  add_diagnoses(skeleton, diags, "lopnr",
                diags = list(absent = "ZZ99"))
  invisible(skeleton)
}

test_that("aggregation: process_skeletons() emits one consolidated warning", {
  study <- .mk_study()
  study$register_framework(.framework_fn)
  study$register_randvars("flag_absent", .randvars_with_unmatched_codes)

  expect_warning(study$process_skeletons(), regexp = "ZZ99")
})

test_that("aggregation: 2nd no-change run still emits the consolidated warning", {
  # Even when every batch hits the fast path and re-runs nothing, the
  # stored code_check_state in each batch's meta is still the source of
  # truth for the consolidated report.
  study <- .mk_study()
  study$register_framework(.framework_fn)
  study$register_randvars("flag_absent", .randvars_with_unmatched_codes)

  suppressWarnings(study$process_skeletons())  # 1st run: do the work

  # 2nd run: no skeleton is touched, but the report still fires.
  expect_warning(study$process_skeletons(), regexp = "ZZ99")
})
