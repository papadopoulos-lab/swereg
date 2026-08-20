# Phase 1b -- the registered trim.
#
# A study may register exactly one trim function. It runs immediately
# after the framework and before the code registry. It is the one
# declared place in the pipeline that may delete skeleton rows.
#
# Its identity is PERSISTED on the Skeleton (`trim_fn_hash`) and mirrored
# into the meta sidecar. So a trim change rebuilds the base on all four
# paths. Those are the meta-only fast return, the meta-only refresh, the
# slow path and the parallel worker path.
#
# Most tests below run the pipeline over an EXPLICIT batch list. That
# makes `full_run` FALSE, so `.commit_skeleton_manifest()` writes no
# manifest and raises nothing. Without it the manifest guard sees any
# identity drift and stops the run first. No assertion here then reads
# the skeleton, and which assertion detects a defect stays hidden. The
# last test in the file covers the full-run path on its own.

library(data.table)

# ---------------------------------------------------------------------------
# Fixture: 6 persons, 4 weekly rows each -> 12 rows per batch, 2 batches.
# ---------------------------------------------------------------------------

.trim_study <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "grp1",
    batch_size = 3L
  )
  study$set_ids(1:6)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:6, val = letters[1:6])
  )
  study$register_framework(.trim_framework)
  study
}

# Run every batch without the full-run manifest commit. See the file
# header for why.
.trim_run <- function(study, ...) {
  study$process_skeletons(batches = seq_len(study$n_batches), ...)
}

# The framework and the two week-dropping trims cross a process boundary
# in the parallel test. `process_skeletons(n_workers = 2)` serialises the
# study, and the worker evaluates them in its own session. So each one is
# self-contained and reads no helper and no variable from this file.
.trim_framework <- function(batch_data, config) {
  ids <- batch_data[["grp1"]]$lopnr
  d <- data.table::CJ(
    id = ids,
    isoyearweek = c("2020-01", "2020-02", "2020-03", "2020-04")
  )
  d[, `:=`(
    isoyear = 2020L,
    is_isoyear = FALSE,
    sex = ifelse(id %% 2L == 0L, "female", "male")
  )]
  d[]
}

# Every week-dropping trim records the column names it saw, one file per
# batch, keyed by the batch's lowest id. The record is what proves the
# trim ran before the code registry: a code entry's column MUST NOT
# appear in it.
#
# Drops week 2020-04: 3 rows of 12 per batch, leaving 9.
.trim_v1 <- function(skeleton, batch_data, config) {
  writeLines(
    names(skeleton),
    file.path(
      config$data_skeleton_dir,
      sprintf("trimsaw_%03d.txt", min(skeleton$id))
    )
  )
  skeleton[isoyearweek != "2020-04"]
}

# Drops weeks 2020-03 and 2020-04: 6 rows of 12 per batch, leaving 6.
.trim_v2 <- function(skeleton, batch_data, config) {
  writeLines(
    names(skeleton),
    file.path(
      config$data_skeleton_dir,
      sprintf("trimsaw_%03d.txt", min(skeleton$id))
    )
  )
  skeleton[!isoyearweek %in% c("2020-03", "2020-04")]
}

# NOT idempotent. Run it twice and it deletes two rows, not one. That is
# legal for a trim, because a trim runs once per rebuild on a fresh base.
.trim_drop_first <- function(skeleton, batch_data, config) {
  skeleton[-1L]
}

.trim_saw_files <- function(study) {
  list.files(study$data_skeleton_dir, pattern = "^trimsaw_", full.names = TRUE)
}

.trim_clear_saw_files <- function(study) {
  unlink(.trim_saw_files(study))
}

.trim_saw_columns <- function(study) {
  unlist(lapply(.trim_saw_files(study), readLines), use.names = FALSE)
}

# Marks `my_code` TRUE on the week the trim deletes.
.trim_code_fn <- function(skeleton, dataset, id_name, codes, ...) {
  for (nm in names(codes)) {
    skeleton[, (nm) := isoyearweek == "2020-04"]
  }
  invisible(skeleton)
}

.trim_register_codes <- function(study, label = "trimcode") {
  study$register_codes(
    codes = list(my_code = "X"),
    fn = .trim_code_fn,
    groups = list("grp1"),
    label = label
  )
}

.trim_randvar_a <- function(skeleton, batch_data, config) {
  skeleton[, rv_a := 1L]
  invisible(skeleton)
}

.trim_randvar_b <- function(skeleton, batch_data, config) {
  skeleton[, rv_a := 2L]
  invisible(skeleton)
}

.trim_rows <- function(study) {
  vapply(1:2, function(i) nrow(study$load_skeleton(i)$data), integer(1))
}

# ---------------------------------------------------------------------------
# Core 1: the trim runs, and rows disappear
# ---------------------------------------------------------------------------

test_that("a registered trim deletes rows from every batch", {
  study <- .trim_study()
  .trim_run(study)
  expect_equal(.trim_rows(study), c(12L, 12L))

  study2 <- .trim_study()
  study2$register_trim(.trim_v1)
  .trim_run(study2)

  expect_equal(.trim_rows(study2), c(9L, 9L))
  sk1 <- study2$load_skeleton(1L)
  expect_false("2020-04" %in% sk1$data$isoyearweek)
  expect_identical(sk1$trim_fn_hash, swereg:::.hash_function(.trim_v1))
  expect_identical(sk1$pipeline_hash(), study2$pipeline_hash())

  out <- utils::capture.output(print(sk1))
  expect_true(any(grepl("trim_hash", out, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Core 2: the trim runs BEFORE the code registry
# ---------------------------------------------------------------------------

test_that("the trim runs before the code registry", {
  study <- .trim_study()
  study$register_trim(.trim_v1)
  .trim_register_codes(study)
  .trim_run(study)

  seen <- .trim_saw_columns(study)
  expect_true(length(seen) > 0L)
  expect_true("isoyearweek" %in% seen)
  expect_false("my_code" %in% seen)

  sk1 <- study$load_skeleton(1L)
  expect_true("my_code" %in% names(sk1$data))
  expect_equal(nrow(sk1$data), 9L)
})

# ---------------------------------------------------------------------------
# Core 3: editing the trim rebuilds the base
# ---------------------------------------------------------------------------

test_that("editing the trim rebuilds the base", {
  study <- .trim_study()
  study$register_trim(.trim_v1)
  .trim_run(study)

  sk1_before <- study$load_skeleton(1L)
  expect_equal(nrow(sk1_before$data), 9L)

  study$trim_fn <- .trim_v2
  .trim_run(study)

  sk1_after <- study$load_skeleton(1L)
  expect_equal(nrow(sk1_after$data), 6L)
  expect_identical(sk1_after$trim_fn_hash, swereg:::.hash_function(.trim_v2))
  expect_false(identical(sk1_after$created_at, sk1_before$created_at))
})

# ---------------------------------------------------------------------------
# Core 4: no trim registered still works
# ---------------------------------------------------------------------------

test_that("a study with no trim registered builds and stays in sync", {
  study <- .trim_study()
  .trim_register_codes(study)
  .trim_run(study)

  sk1 <- study$load_skeleton(1L)
  expect_equal(nrow(sk1$data), 12L)
  expect_identical(sk1$trim_fn_hash, swereg:::.TRIM_NONE)
  expect_identical(sk1$pipeline_hash(), study$pipeline_hash())
  expect_equal(length(.trim_saw_files(study)), 0L)

  # A second run with nothing changed takes the meta-only fast return.
  .trim_run(study)
  expect_identical(study$load_skeleton(1L)$created_at, sk1$created_at)
})

# ---------------------------------------------------------------------------
# The trim runs ONCE per rebuild, so it need not be idempotent
# ---------------------------------------------------------------------------

test_that("an unrelated edit does not re-run the trim on trimmed data", {
  study <- .trim_study()
  study$register_trim(.trim_drop_first)
  .trim_run(study)
  expect_equal(.trim_rows(study), c(11L, 11L))

  # A code entry is an unrelated change. It reaches the slow path and
  # rebuilds nothing, so the trim MUST NOT run a second time.
  .trim_register_codes(study)
  .trim_run(study)
  expect_equal(.trim_rows(study), c(11L, 11L))

  # A new randvars step is also unrelated.
  study$register_randvars("rv", .trim_randvar_a)
  .trim_run(study)
  expect_equal(.trim_rows(study), c(11L, 11L))
  expect_true("rv_a" %in% names(study$load_skeleton(1L)$data))

  # So is an edit to that step.
  study$randvars_fns[["rv"]] <- .trim_randvar_b
  .trim_run(study)
  expect_equal(.trim_rows(study), c(11L, 11L))
  expect_equal(unique(study$load_skeleton(1L)$data$rv_a), 2L)
})

# ---------------------------------------------------------------------------
# Adding and removing a trim
# ---------------------------------------------------------------------------

test_that("adding a trim to a study that had none rebuilds the base", {
  study <- .trim_study()
  .trim_run(study)
  expect_equal(.trim_rows(study), c(12L, 12L))

  study$trim_fn <- .trim_v1
  .trim_run(study)

  expect_equal(.trim_rows(study), c(9L, 9L))
  expect_identical(
    study$load_skeleton(1L)$trim_fn_hash,
    swereg:::.hash_function(.trim_v1)
  )
})

test_that("removing a registered trim rebuilds the base", {
  study <- .trim_study()
  study$register_trim(.trim_v1)
  .trim_run(study)
  expect_equal(.trim_rows(study), c(9L, 9L))

  study$trim_fn <- NULL
  .trim_run(study)

  expect_equal(.trim_rows(study), c(12L, 12L))
  expect_identical(study$load_skeleton(1L)$trim_fn_hash, swereg:::.TRIM_NONE)
})

test_that("a pre-trim skeleton does not compare equal to no trim registered", {
  study <- .trim_study()
  .trim_run(study)

  # Age both batches back to the shape a swereg without the trim phase
  # wrote: trim_fn_hash absent, so NULL on read.
  for (i in 1:2) {
    sk <- study$load_skeleton(i)
    sk$trim_fn_hash <- NULL
    study$save_skeleton(sk)
  }
  expect_null(study$load_skeleton(1L)$trim_fn_hash)
  expect_null(study$load_skeleton_meta(1L)$trim_fn_hash)
  created_before <- study$load_skeleton(1L)$created_at

  # Still no trim registered. The base MUST rebuild anyway, or a later
  # $register_trim() on this study would rebuild nothing.
  .trim_run(study)

  expect_identical(study$load_skeleton(1L)$trim_fn_hash, swereg:::.TRIM_NONE)
  expect_false(identical(study$load_skeleton(1L)$created_at, created_before))
})

# ---------------------------------------------------------------------------
# A trim edit reaches more than one batch
# ---------------------------------------------------------------------------

test_that("a trim edit rebuilds every batch, not just the first", {
  study <- .trim_study()
  study$register_trim(.trim_v1)
  .trim_run(study)
  expect_equal(.trim_rows(study), c(9L, 9L))

  study$trim_fn <- .trim_v2
  .trim_run(study)

  expect_equal(.trim_rows(study), c(6L, 6L))
  hashes <- study$skeleton_pipeline_hashes()
  expect_equal(nrow(hashes), 2L)
  expect_equal(
    unique(hashes$trim_fn_hash),
    swereg:::.hash_function(.trim_v2)
  )
})

# ---------------------------------------------------------------------------
# The parallel worker path
# ---------------------------------------------------------------------------

test_that("a trim edit rebuilds through the parallel worker path", {
  skip_on_cran() # spawns subprocesses
  skip_if_not_installed("mirai")

  study <- .trim_study()
  study$register_trim(.trim_v1)
  invisible(utils::capture.output(
    suppressMessages(.trim_run(study, n_workers = 2L)),
    type = "output"
  ))
  expect_equal(.trim_rows(study), c(9L, 9L))

  study$trim_fn <- .trim_v2
  invisible(utils::capture.output(
    suppressMessages(.trim_run(study, n_workers = 2L)),
    type = "output"
  ))

  expect_equal(.trim_rows(study), c(6L, 6L))
  expect_identical(
    study$load_skeleton(2L)$trim_fn_hash,
    swereg:::.hash_function(.trim_v2)
  )
  expect_identical(
    study$load_skeleton(2L)$pipeline_hash(),
    study$pipeline_hash()
  )
})

# ---------------------------------------------------------------------------
# The meta-only refresh path (pipeline_ok && !specs_ok)
# ---------------------------------------------------------------------------

test_that("a trim change invalidates through the meta-only refresh path", {
  study <- .trim_study()
  study$register_trim(.trim_v1)
  .trim_run(study)
  expect_equal(.trim_rows(study), c(9L, 9L))

  # Liveness witness for the path itself. A new population spec with an
  # unchanged pipeline takes the meta-only refresh: it reloads the
  # skeleton and rewrites the meta, and runs no phase. The trim therefore
  # writes no record.
  .trim_clear_saw_files(study)
  study$population_by_specs <- list("sex")
  .trim_run(study)

  expect_equal(length(.trim_saw_files(study)), 0L)
  expect_true(
    "sex" %in% names(study$load_skeleton_meta(1L)$population_aggregations)
  )
  expect_equal(.trim_rows(study), c(9L, 9L))

  # Now change the trim AND register a further spec, so the meta is stale
  # on both counts. The trim change MUST win: without `trim_fn_hash` in
  # the meta comparison this takes the meta-only refresh and the rows
  # stay at 9.
  study$population_by_specs <- list("sex", "is_isoyear")
  study$trim_fn <- .trim_v2
  .trim_run(study)

  expect_equal(length(.trim_saw_files(study)), 2L)
  expect_equal(.trim_rows(study), c(6L, 6L))
  expect_identical(
    study$load_skeleton_meta(1L)$trim_fn_hash,
    swereg:::.hash_function(.trim_v2)
  )
})

# ---------------------------------------------------------------------------
# Every identity surface moves together
# ---------------------------------------------------------------------------

test_that("all four identity surfaces move together on a trim change", {
  study <- .trim_study()
  .trim_register_codes(study)
  study$register_randvars("rv", .trim_randvar_a)
  study$register_trim(.trim_v1)
  .trim_run(study)

  sk_hash_1 <- study$load_skeleton(1L)$pipeline_hash()
  meta_hash_1 <- unique(study$skeleton_pipeline_hashes()$pipeline_hash)
  study_hash_1 <- study$pipeline_hash()
  rv_hash_1 <- unname(study$randvars_hashes()[["rv"]])
  expect_identical(sk_hash_1, study_hash_1)
  expect_identical(meta_hash_1, study_hash_1)

  study$trim_fn <- .trim_v2
  .trim_run(study)

  sk_hash_2 <- study$load_skeleton(1L)$pipeline_hash()
  meta_hash_2 <- unique(study$skeleton_pipeline_hashes()$pipeline_hash)
  study_hash_2 <- study$pipeline_hash()
  rv_hash_2 <- unname(study$randvars_hashes()[["rv"]])
  expect_identical(sk_hash_2, study_hash_2)
  expect_identical(meta_hash_2, study_hash_2)
  expect_false(identical(study_hash_2, study_hash_1))

  # The trim also folds into every randvars step's own hash, so a trim
  # change diverges phase 3 at step 1.
  expect_false(identical(rv_hash_2, rv_hash_1))
})

# ---------------------------------------------------------------------------
# Error cases
# ---------------------------------------------------------------------------

test_that("a second register_trim() stops and names the registered trim", {
  study <- .trim_study()
  study$register_trim(.trim_v1)

  expect_error(
    study$register_trim(.trim_v2),
    swereg:::.hash_function(.trim_v1),
    fixed = TRUE
  )
  expect_error(
    study$register_trim(.trim_v2),
    "already registered"
  )
  # The registration is unchanged by the refused call.
  expect_identical(
    swereg:::.hash_function(study$trim_fn),
    swereg:::.hash_function(.trim_v1)
  )
})

test_that("a trim returning a non-data.table stops", {
  study <- .trim_study()
  study$register_trim(function(skeleton, batch_data, config) {
    as.data.frame(skeleton)
  })

  expect_error(
    .trim_run(study),
    "trim_fn must return a data.table"
  )
})

# ---------------------------------------------------------------------------
# The full-run path, which every other test above avoids on purpose
# ---------------------------------------------------------------------------

test_that("a full run with a trim commits a skeleton manifest", {
  study <- .trim_study()
  study$register_trim(.trim_v1)
  study$process_skeletons()

  expect_false(is.null(study$skeleton_manifest))
  expect_equal(.trim_rows(study), c(9L, 9L))
  expect_identical(
    study$assert_skeletons_consistent(),
    study$pipeline_hash()
  )
})
