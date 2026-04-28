# Pin that worker errors during `process_skeletons()` propagate as a
# hard `stop()`, not a silent warning. The function still attempts
# every batch (so successful batches are persisted via
# write_pipeline_snapshot()) and streams immediate. warnings live as
# failures occur, but at the end it raises if any batch failed --
# which means non-interactive callers (Rscript, CI, the s1/s2/s3
# wrapper scripts) actually see a non-zero exit / R-level error
# instead of silently appearing to succeed.

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

test_that("process_skeletons (n_workers = 1): worker error raises a hard error at the end", {
  dir <- withr::local_tempdir()
  study <- .failing_study(dir)

  # Streaming warning while the failure happens, then a hard error
  # after the loop. We discard the warning and capture the final stop().
  err_msg <- tryCatch(
    suppressMessages(suppressWarnings(study$process_skeletons(n_workers = 1L))),
    error = function(e) conditionMessage(e)
  )
  expect_false(is.null(err_msg),
    info = "n_workers = 1 must propagate worker errors as a hard error")
  expect_true(grepl("synthetic_framework_error", err_msg, fixed = TRUE),
    info = "final stop() must include the underlying error message")
  expect_true(grepl("process_skeletons.*failed", err_msg),
    info = "final stop() must clearly identify which function failed")
})

test_that("process_skeletons (n_workers > 1): worker error raises a hard error at the end", {
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
    info = "final stop() must include the underlying error message")
})

test_that("process_skeletons: error message reports per-batch failure count + underlying messages", {
  dir <- withr::local_tempdir()
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:9)  # -> 3 batches
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:9, val = rep("x", 9))
  )
  # Counter-based: framework invocations 1 and 3 fail, invocation 2
  # succeeds. Order-independent w.r.t. how set_ids partitions IDs.
  counter <- 0L
  study$register_framework(function(batch_data, config) {
    counter <<- counter + 1L
    if (counter %% 2L == 1L) {
      stop("synthetic_invocation_", counter)
    }
    data.table::data.table(id = batch_data[["grp1"]]$lopnr, week = 1L)
  })

  err_msg <- tryCatch(
    suppressMessages(suppressWarnings(study$process_skeletons(n_workers = 1L))),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("2 / 3 batch", err_msg, fixed = TRUE),
              info = paste0("error should report 2/3 failures; got: ", err_msg))
  expect_true(grepl("synthetic_invocation_1", err_msg, fixed = TRUE),
              info = "first failure's underlying message must propagate")
  expect_true(grepl("synthetic_invocation_3", err_msg, fixed = TRUE),
              info = "third failure's underlying message must propagate")
  expect_false(grepl("synthetic_invocation_2", err_msg, fixed = TRUE),
               info = "second invocation succeeded -- must not appear")
})

test_that("process_skeletons: successful batches still produce skeleton files on partial failure", {
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
  # Only the second invocation fails; other two succeed.
  counter <- 0L
  study$register_framework(function(batch_data, config) {
    counter <<- counter + 1L
    if (counter == 2L) {
      stop("middle_invocation_failure")
    }
    data.table::data.table(id = batch_data[["grp1"]]$lopnr, week = 1L)
  })

  tryCatch(
    suppressMessages(suppressWarnings(study$process_skeletons(n_workers = 1L))),
    error = function(e) NULL
  )
  skel_files <- list.files(dir, pattern = "^skeleton_\\d+\\.qs2$")
  expect_equal(length(skel_files), 2L,
    info = paste0("expected 2 successful batch files; got ",
                  length(skel_files), ": ",
                  paste(skel_files, collapse = ", ")))
})
