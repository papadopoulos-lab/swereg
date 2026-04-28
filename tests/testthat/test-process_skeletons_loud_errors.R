# Pin that worker errors during `process_skeletons()` halt
# immediately with a hard `stop()` -- not a swallowed warning, and
# not a complete-then-raise pattern that would burn 4 more days of
# compute on the remaining 149 batches when one already failed.
#
# Rationale: these pipelines run unattended for days. If batch 1
# fails 10 minutes in (e.g. a systematic bug, a missing column, an
# unreadable rawbatch file), the user wants to SSH in within minutes
# and see the failure -- not at the end of a 4-day run with 149
# already-broken outputs.
#
# Successful batches up to the failure are already persisted on disk
# by .process_one_batch(). The framework-hash matching on rerun will
# skip them, so no work is lost. The error message tells the user
# how to resume via `batches = ...`.

skip_if_not_installed("data.table")
skip_if_not_installed("withr")
skip_if_not_installed("callr")

.failing_study <- function(dir, n_ids = 3L, batch_size = 3L) {
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = batch_size
  )
  study$set_ids(seq_len(n_ids))
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = seq_len(n_ids),
                           val = rep("x", n_ids))
  )
  study$register_framework(function(batch_data, config) {
    stop("synthetic_framework_error")
  })
  study
}

test_that("process_skeletons (n_workers = 1): worker error halts immediately with hard error", {
  dir <- withr::local_tempdir()
  study <- .failing_study(dir)

  err_msg <- tryCatch(
    suppressMessages(suppressWarnings(study$process_skeletons(n_workers = 1L))),
    error = function(e) conditionMessage(e)
  )
  expect_false(is.null(err_msg),
    info = "n_workers = 1 must propagate worker errors as a hard error")
  expect_true(grepl("synthetic_framework_error", err_msg, fixed = TRUE),
    info = "stop() must include the underlying error message")
  expect_true(grepl("halted on batch", err_msg),
    info = "stop() must clearly identify the function and the batch")
  expect_true(grepl("batches =", err_msg, fixed = TRUE),
    info = "stop() must hint at how to resume (rerun with `batches = ...`)")
})

test_that("process_skeletons (n_workers > 1): worker error halts immediately with hard error", {
  skip_on_cran()                          # spawns subprocesses
  skip_on_os("windows", arch = "i386")    # 32-bit lacks proper subprocess support

  dir <- withr::local_tempdir()
  study <- .failing_study(dir, n_ids = 6L, batch_size = 3L)  # 2 batches

  err_msg <- tryCatch(
    suppressMessages(suppressWarnings(study$process_skeletons(n_workers = 2L))),
    error = function(e) conditionMessage(e)
  )
  expect_false(is.null(err_msg),
    info = "n_workers > 1 must propagate worker errors as a hard error")
  expect_true(grepl("synthetic_framework_error", err_msg, fixed = TRUE),
    info = "stop() must include the underlying error message")
  expect_true(grepl("halted on batch", err_msg))
})

test_that("process_skeletons: stops on FIRST failure, does not run subsequent batches", {
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:9)  # 3 batches
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:9, val = rep("x", 9))
  )

  # Counter increments on every framework invocation.
  invocations <- 0L
  study$register_framework(function(batch_data, config) {
    invocations <<- invocations + 1L
    if (invocations == 1L) {
      stop("first_invocation_failure")
    }
    data.table::data.table(id = batch_data[["grp1"]]$lopnr, week = 1L)
  })

  tryCatch(
    suppressMessages(suppressWarnings(study$process_skeletons(n_workers = 1L))),
    error = function(e) NULL
  )
  # CRITICAL: only the failing first invocation should have run. The
  # remaining 2 batches must NOT have been attempted -- otherwise the
  # 4-days-of-wasted-compute footgun is back.
  expect_equal(invocations, 1L,
    info = paste0(
      "process_skeletons must halt on first failure -- no subsequent ",
      "batches should run. Got ", invocations, " invocations (expected 1)."))
})

test_that("process_skeletons: successful batches before the failure are persisted on disk", {
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:9)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:9, val = rep("x", 9))
  )
  # Second invocation fails; first succeeds.
  invocations <- 0L
  study$register_framework(function(batch_data, config) {
    invocations <<- invocations + 1L
    if (invocations == 2L) {
      stop("second_invocation_failure")
    }
    data.table::data.table(id = batch_data[["grp1"]]$lopnr, week = 1L)
  })
  tryCatch(
    suppressMessages(suppressWarnings(study$process_skeletons(n_workers = 1L))),
    error = function(e) NULL
  )
  skel_files <- list.files(dir, pattern = "^skeleton_\\d+\\.qs2$")
  # First invocation succeeded -> at least one skeleton file on disk.
  # Third invocation never ran (fail-fast) -> at most 1 file.
  expect_equal(length(skel_files), 1L,
    info = paste0("expected exactly 1 successful batch persisted before fail-fast halt; got ", length(skel_files)))
})
