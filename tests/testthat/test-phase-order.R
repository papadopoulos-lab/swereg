# Phase 2 (codes) runs before phase 3 (randvars).
#
# `.process_one_batch()` in R/r6_registrystudy.R calls
# `sk$sync_with_registry()` first and `sk$sync_randvars()` second. Three things
# follow from that order, and this file pins each one.
#
# Case 1: a randvars step reads a code registry column. Under the old order the
# column did not exist yet, and the step stopped with an object-not-found error.
#
# Case 2: a batch whose raw data group holds zero rows.
# `.apply_code_entry_impl()` skips a group with no rows, so the code column
# never reaches that batch's skeleton. Every value below is measured.
#
# Case 3: a store built under the old order. `Skeleton$phase_order` records the
# order that produced each skeleton, and the rebuild gate in
# `.process_one_batch()` compares it. A mismatch reconstructs the skeleton
# DATA. A comparison in `.meta_matches_pipeline()` alone does not. That one
# only forces the slow path, which then finds every hash unchanged, no-ops both
# syncs, and writes a current meta over a stale skeleton. So case 3 asserts on
# the data, never on the meta alone.
#
# Case 3 also pins the three pipeline-hash surfaces. `phase_order` is a package
# constant on the study side and a stored value on the skeleton side. An old
# skeleton therefore MUST NOT hash equal to the current study. The three
# surfaces are `Skeleton$pipeline_hash()`, `RegistryStudy$pipeline_hash()` and
# the meta-reconstructed digest inside `$skeleton_pipeline_hashes()`.
#
# Cases 1 and 2 drive the real `RegistryStudy$process_skeletons()`. Case 3
# calls the two sync methods directly to BUILD the old store, then drives
# `$process_skeletons()` against it.
#
# Every run below passes an EXPLICIT batch list. That makes `full_run` FALSE,
# so `.commit_skeleton_manifest()` raises nothing and each assertion here is
# the thing that detects a defect. test-trim-phase.R uses the same convention
# and its file header explains it.

library(data.table)

# ---------------------------------------------------------------------------
# Fixtures: 6 persons, batch_size 3 -> 2 batches, 3 weeks each -> 9 rows.
# ---------------------------------------------------------------------------

# Two rawbatch groups, and the split is the point. "ids" always carries every
# person, so the framework builds a full grid for both batches. "codes" carries
# the rows the code entry reads, and case 2 leaves it empty for batch 2.
.po_framework <- function(batch_data, config) {
  d <- data.table::CJ(
    id = batch_data[["ids"]]$lopnr,
    isoyearweek = c("2020-01", "2020-02", "2020-03")
  )
  d[, `:=`(isoyear = 2020L, is_isoyear = FALSE)]
  d[]
}

# Marks one week per person. `$register_codes()` calls this with the code names
# already prefixed, so the loop writes exactly the columns the contract
# validator expects.
.po_code_fn <- function(skeleton, dataset, id_name, codes, ...) {
  for (nm in names(codes)) {
    skeleton[, (nm) := isoyearweek == "2020-02"]
  }
  invisible(skeleton)
}

# Reads the code column with no guard. This is impossible under the old order:
# `my_code` does not exist when randvars run first.
.po_randvar_reads_code <- function(skeleton, batch_data, config) {
  skeleton[, rv_n_code_weeks := sum(my_code), by = id]
  skeleton[, rv_code_even := my_code & (id %% 2L == 0L)]
  invisible(skeleton)
}

# Guards on the column instead of reading it blind. Cases 2 and 3 need a step
# that runs under both orders and reports which one it saw.
.po_randvar_guarded <- function(skeleton, batch_data, config) {
  saw <- "my_code" %in% names(skeleton)
  skeleton[, rv_saw_code := saw]
  skeleton[,
    rv_n_code_weeks := if (saw) sum(my_code) else NA_integer_,
    by = id
  ]
  invisible(skeleton)
}

# `full_codes = FALSE` writes the "codes" group for batch 1's persons only, so
# batch 2's slice of that group has zero rows.
.po_study <- function(full_codes = TRUE, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("ids", "codes"),
    batch_size = 3L
  )
  study$set_ids(1:6)
  study$save_rawbatch("ids", data.table::data.table(lopnr = 1:6, val = "a"))
  code_ids <- if (full_codes) 1:6 else study$batch_id_list[[1]]
  study$save_rawbatch(
    "codes",
    data.table::data.table(lopnr = code_ids, code = "X")
  )
  study$register_framework(.po_framework)
  study$register_codes(
    codes = list(my_code = "X"),
    fn = .po_code_fn,
    groups = list("codes"),
    label = "pocode"
  )
  study
}

.po_run <- function(study) {
  invisible(utils::capture.output(
    suppressMessages(study$process_skeletons(batches = seq_len(study$n_batches))),
    type = "output"
  ))
}

# ---------------------------------------------------------------------------
# Case 1 -- a randvars step reads a code column
# ---------------------------------------------------------------------------

test_that("a randvars step reads a column the code registry wrote", {
  study <- .po_study()
  study$register_randvars("reads_code", .po_randvar_reads_code)
  .po_run(study)

  sk1 <- study$load_skeleton(1L)
  d1 <- sk1$data

  # The code column is there, and the randvars columns come after it.
  expect_identical(
    names(d1),
    c(
      "id", "isoyearweek", "isoyear", "is_isoyear",
      "my_code", "rv_n_code_weeks", "rv_code_even"
    )
  )
  expect_identical(nrow(d1), 9L)

  # One marked week per person, in both batches.
  expect_identical(sum(d1$my_code), 3L)
  expect_identical(unique(d1$rv_n_code_weeks), 1L)

  # The derived column is the code column ANDed with an even person id.
  expect_identical(d1$rv_code_even, d1$my_code & (d1$id %% 2L == 0L))

  # Across both batches: 6 persons, one marked week each, and 3 of the 6 ids
  # are even. Every one of these counts is zero under the old order, because
  # the step cannot run at all.
  both <- data.table::rbindlist(lapply(1:2, function(i) study$load_skeleton(i)$data))
  expect_identical(nrow(both), 18L)
  expect_identical(sum(both$my_code), 6L)
  expect_identical(sum(both$rv_n_code_weeks), 18L)
  expect_identical(sum(both$rv_code_even), 3L)

  # Provenance: the randvars step records the two columns it added, and the
  # code column is not one of them.
  expect_identical(
    sk1$randvars_state[["reads_code"]]$added_columns,
    c("rv_n_code_weeks", "rv_code_even")
  )
  expect_identical(sk1$phase_order, c("framework", "codes", "randvars"))
  expect_identical(
    study$load_skeleton_meta(1L)$phase_order,
    c("framework", "codes", "randvars")
  )
})

# ---------------------------------------------------------------------------
# Case 2 -- a batch whose raw data group has zero rows
# ---------------------------------------------------------------------------

test_that("an empty raw group leaves the code column off that batch", {
  study <- .po_study(full_codes = FALSE)
  study$register_randvars("guarded", .po_randvar_guarded)
  .po_run(study)

  sk1 <- study$load_skeleton(1L)
  sk2 <- study$load_skeleton(2L)

  # Both batches have the full grid. Only the code group differs.
  expect_identical(nrow(sk1$data), 9L)
  expect_identical(nrow(sk2$data), 9L)
  expect_identical(sort(unique(sk1$data$id)), sort(study$batch_id_list[[1]]))
  expect_identical(sort(unique(sk2$data$id)), sort(study$batch_id_list[[2]]))

  # MEASURED: the code column does not materialise for the empty batch.
  # `.apply_code_entry_impl()` drops every group with no rows, and skips the
  # entry when none is left, so `fn` never runs and writes no column.
  expect_true("my_code" %in% names(sk1$data))
  expect_false("my_code" %in% names(sk2$data))

  # The randvars step still runs on both batches and reports what it saw.
  expect_true(all(sk1$data$rv_saw_code))
  expect_false(any(sk2$data$rv_saw_code))
  expect_identical(unique(sk1$data$rv_n_code_weeks), 1L)
  expect_true(all(is.na(sk2$data$rv_n_code_weeks)))

  # MEASURED: the entry is recorded as applied on BOTH batches, under the same
  # fingerprint. Only the per-column counts differ, because
  # `$refresh_code_entry_counts()` intersects its predicted columns with the
  # data and finds none on batch 2.
  fp <- names(sk1$applied_registry)
  expect_identical(length(fp), 1L)
  expect_identical(names(sk2$applied_registry), fp)
  expect_identical(
    names(sk1$applied_registry[[fp]]$counts),
    "my_code"
  )
  expect_identical(sk2$applied_registry[[fp]]$counts, list())
})

# ---------------------------------------------------------------------------
# Case 3 -- a store built under the old order is RECONSTRUCTED
# ---------------------------------------------------------------------------

# Writes the skeleton in the shape a pre-swap swereg wrote: framework, then
# randvars, then codes, and no `phase_order` on the object. Every hash is the
# one `$process_skeletons()` computes for this study, so nothing except the
# phase order can force the rebuild.
.po_build_old_order_store <- function(study) {
  framework_hash <- swereg:::.hash_function(study$framework_fn)
  trim_hash <- swereg:::.trim_hash(study$trim_fn)
  randvars_hashes <- study$randvars_hashes()
  current_fps <- study$code_registry_fingerprints()

  for (i in seq_len(study$n_batches)) {
    bd <- study$load_rawbatch(i)
    loader <- function() bd
    sk <- Skeleton$new(
      data = study$framework_fn(bd, study),
      batch_number = i
    )
    sk$framework_fn_hash <- framework_hash
    sk$trim_fn_hash <- trim_hash

    # THE OLD ORDER: randvars first, code registry second.
    sk$sync_randvars(
      randvars_fns = study$randvars_fns,
      randvars_hashes = randvars_hashes,
      batch_data_loader = loader,
      config = study
    )
    sk$sync_with_registry(
      current_fps = current_fps,
      registry = study$code_registry,
      batch_data_loader = loader,
      id_col = study$id_col
    )

    # A swereg without the field wrote no phase_order at all, so it reads NULL.
    # `Skeleton$new()` already leaves it NULL, exactly as it leaves
    # `framework_fn_hash` NULL. The line is explicit because the whole case
    # turns on that value.
    sk$phase_order <- NULL
    study$save_skeleton(sk)
  }
  invisible(study)
}

test_that("a store built under the old order is reconstructed, data and all", {
  study <- .po_study()
  study$register_randvars("guarded", .po_randvar_guarded)
  .po_build_old_order_store(study)

  # The store is genuinely old on both counts, or the rebuild below proves
  # nothing.
  for (i in 1:2) {
    stale <- study$load_skeleton(i)
    expect_null(stale$phase_order)
    expect_null(study$load_skeleton_meta(i)$phase_order)
    expect_true("my_code" %in% names(stale$data))
    expect_false(any(stale$data$rv_saw_code))
    expect_true(all(is.na(stale$data$rv_n_code_weeks)))

    # Surface 1 against surface 2: the stored skeleton hash MUST NOT equal the
    # study's current hash. Every other component matches, so `phase_order` is
    # the only thing that can separate them.
    expect_false(identical(stale$pipeline_hash(), study$pipeline_hash()))
  }

  # Surface 3 against surface 2: the digest rebuilt from the meta sidecar,
  # which is the one `.commit_skeleton_manifest()` reads.
  stale_hashes <- study$skeleton_pipeline_hashes()$pipeline_hash
  expect_identical(length(stale_hashes), 2L)
  expect_false(any(stale_hashes == study$pipeline_hash()))

  # The consumer-facing pre-flight check therefore stops a stale store.
  expect_error(
    study$assert_skeletons_consistent(),
    "does not match this study's current pipeline hash"
  )

  .po_run(study)

  # THE DATA CHANGED. `rv_saw_code` flips to TRUE because the rebuild replays
  # the randvars step after the code registry. A run that only rewrote the
  # meta would leave every value FALSE.
  for (i in 1:2) {
    fresh <- study$load_skeleton(i)
    expect_true(all(fresh$data$rv_saw_code))
    expect_identical(unique(fresh$data$rv_n_code_weeks), 1L)
    expect_identical(sum(fresh$data$my_code), 3L)
    expect_identical(fresh$phase_order, c("framework", "codes", "randvars"))
    expect_identical(
      study$load_skeleton_meta(i)$phase_order,
      c("framework", "codes", "randvars")
    )
    expect_identical(fresh$pipeline_hash(), study$pipeline_hash())
  }

  expect_identical(
    unique(study$skeleton_pipeline_hashes()$pipeline_hash),
    study$pipeline_hash()
  )
  expect_identical(study$assert_skeletons_consistent(), study$pipeline_hash())

  # The per-batch report carries the stored order as one collapsed string.
  ph <- study$skeleton_pipeline_hashes()
  expect_true("phase_order" %in% names(ph))
  expect_identical(unique(ph$phase_order), "framework -> codes -> randvars")

  # A second run changes nothing. The gate is satisfied now, so this takes the
  # meta-only fast return and the rebuild does not repeat.
  created <- study$load_skeleton(1L)$created_at
  .po_run(study)
  expect_identical(study$load_skeleton(1L)$created_at, created)
  expect_true(all(study$load_skeleton(1L)$data$rv_saw_code))
})
