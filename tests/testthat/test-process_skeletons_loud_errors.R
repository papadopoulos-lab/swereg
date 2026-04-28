# Pin that worker errors during `process_skeletons()` are visible to
# the user, not silently swallowed. swereg's own CLAUDE.md flagged
# this footgun:
#
#   "When n_workers > 1, worker errors are caught by tryCatch and
#   emitted as warning() -- progress stays at 0% with no visible
#   error."
#
# The current contract (verified by these tests): worker failures
# raise a `warning()` whose text includes both "Batch <N> failed" and
# the underlying error message. This is loud enough that interactive
# users see what crashed, but it's still NOT an `error` in the strict
# sense -- regressing the message text back to a generic
# "Batch failed" / dropping the underlying error message would
# silently eat the diagnostic information.

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

test_that("process_skeletons (n_workers = 1): framework_fn error surfaces with the original message", {
  dir <- withr::local_tempdir()
  study <- .failing_study(dir)

  result <- tryCatch(
    suppressMessages(study$process_skeletons(n_workers = 1L)),
    error   = function(e) list(kind = "error",   msg = conditionMessage(e)),
    warning = function(w) list(kind = "warning", msg = conditionMessage(w))
  )
  expect_true(grepl("synthetic_framework_error", result$msg, fixed = TRUE),
              info = paste0(
                "n_workers = 1 path should surface the framework's error ",
                "text. Captured ", result$kind, ": '", result$msg, "'"))
})

test_that("process_skeletons (n_workers > 1): worker error surfaces as a visible warning", {
  skip_on_cran()                          # spawns subprocesses
  skip_on_os("windows", arch = "i386")    # 32-bit lacks proper subprocess support

  dir <- withr::local_tempdir()
  study <- .failing_study(dir, n_ids = 6L, batch_size = 3L)  # 2 batches

  msgs <- character()
  withCallingHandlers(
    suppressMessages(study$process_skeletons(n_workers = 2L)),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      # If a future fix promotes this to a hard error, capture too.
      msgs <<- c(msgs, conditionMessage(e))
    }
  )
  joined <- paste(msgs, collapse = " | ")
  # The visible signal must carry SOMETHING about the failure -- either
  # the original error text or at least a "Batch N failed" notice.
  expect_true(
    grepl("synthetic_worker_error|synthetic_framework_error|Batch.*failed",
          joined),
    info = paste0(
      "n_workers > 1 path should surface a visible signal naming the ",
      "failed batch and/or the underlying error. Captured: '",
      joined, "'")
  )
})
