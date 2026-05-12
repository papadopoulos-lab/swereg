# Tests for the internal code-check session machinery.
# The whole API is package-private (nothing exported) so these reach in via
# `swereg:::`. Always wrap session opens in withr::defer() to avoid leaking
# global session state if a test fails partway through.

test_that("session accumulates without warning, snapshot reflects state", {
  src1 <- data.frame(diag = c("F32", "F33"), stringsAsFactors = FALSE)
  src2 <- data.frame(diag = c("F12", "F32"), stringsAsFactors = FALSE)
  codes <- list(common = c("F32"), rare = c("F12"))

  swereg:::.start_code_check_session()
  withr::defer(swereg:::.end_code_check_session())

  expect_silent(swereg:::warn_unmatched_codes(src1, codes, "add_diagnoses"))
  expect_silent(swereg:::warn_unmatched_codes(src2, codes, "add_diagnoses"))

  snap <- swereg:::.code_check_snapshot()
  # Both literals matched in at least one batch -> nothing left unmatched.
  bucket <- snap$unmatched$add_diagnoses
  expect_false(any(unlist(lapply(bucket, identity))))
})

test_that(".code_check_emit() warns about literals never matched anywhere", {
  src1 <- data.frame(diag = c("F32"), stringsAsFactors = FALSE)
  src2 <- data.frame(diag = c("F33"), stringsAsFactors = FALSE)
  codes <- list(present = c("F32", "F33"), absent = c("Z9999"))

  swereg:::.start_code_check_session()
  withr::defer(swereg:::.end_code_check_session())
  swereg:::warn_unmatched_codes(src1, codes, "add_diagnoses")
  swereg:::warn_unmatched_codes(src2, codes, "add_diagnoses")
  snap <- swereg:::.code_check_snapshot()

  expect_warning(swereg:::.code_check_emit(snap), regexp = "Z9999")
})

test_that(".code_check_merge() unions snapshots with correct semantics", {
  # Two simulated batches' snapshots: F12 matches in batch 2 only;
  # Z9999 never matches.
  codes <- list(common = c("F32"), rare = c("F12"), absent = c("Z9999"))

  swereg:::.start_code_check_session()
  swereg:::warn_unmatched_codes(
    data.frame(diag = c("F32", "F33"), stringsAsFactors = FALSE),
    codes, "add_diagnoses"
  )
  snap1 <- swereg:::.code_check_snapshot()
  swereg:::.end_code_check_session()

  swereg:::.start_code_check_session()
  swereg:::warn_unmatched_codes(
    data.frame(diag = c("F12"), stringsAsFactors = FALSE),
    codes, "add_diagnoses"
  )
  snap2 <- swereg:::.code_check_snapshot()
  swereg:::.end_code_check_session()

  merged <- swereg:::.code_check_merge(list(snap1, snap2))
  bucket <- merged$unmatched$add_diagnoses

  # F32 matched in snap1 -> still-unmatched flag is FALSE
  expect_false(bucket$common[["F32"]])
  # F12 matched in snap2 -> still-unmatched flag is FALSE
  expect_false(bucket$rare[["F12"]])
  # Z9999 matched in neither -> still-unmatched flag is TRUE
  expect_true(bucket$absent[["Z9999"]])

  # Emission only mentions Z9999.
  expect_warning(swereg:::.code_check_emit(merged), regexp = "Z9999")
})

test_that("merge OR-accumulates ever_true / ever_present for empty cols", {
  codes <- list(rare = "x", never = "y")
  sk1 <- data.table::data.table(id = 1:2, rare = c(FALSE, FALSE),
                                never = c(FALSE, FALSE))
  sk2 <- data.table::data.table(id = 1:2, rare = c(TRUE, FALSE),
                                never = c(FALSE, FALSE))

  swereg:::.start_code_check_session()
  swereg:::warn_empty_logical_cols(sk1, codes, "add_diagnoses")
  snap1 <- swereg:::.code_check_snapshot()
  swereg:::.end_code_check_session()

  swereg:::.start_code_check_session()
  swereg:::warn_empty_logical_cols(sk2, codes, "add_diagnoses")
  snap2 <- swereg:::.code_check_snapshot()
  swereg:::.end_code_check_session()

  merged <- swereg:::.code_check_merge(list(snap1, snap2))
  bucket <- merged$empty$add_diagnoses

  # `rare` ever_true=TRUE in snap2 -> not flagged.
  expect_true(bucket$rare$ever_true)
  # `never` ever_true=FALSE in both -> flagged via emit.
  expect_false(bucket$never$ever_true)

  # Capture the warning to verify only `never` shows up.
  msgs <- character()
  withCallingHandlers(
    swereg:::.code_check_emit(merged),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  combined <- paste(msgs, collapse = " ")
  expect_match(combined, "never")
  expect_false(grepl("\\brare\\b", combined))
})

test_that(".end_code_check_session() with no active session is a no-op", {
  expect_silent(swereg:::.end_code_check_session())
})

test_that("sessions nest safely via reference counting", {
  src <- data.frame(diag = "F32", stringsAsFactors = FALSE)
  codes <- list(absent = c("Z9999"))

  swereg:::.start_code_check_session()   # outer
  withr::defer(swereg:::.end_code_check_session())
  swereg:::warn_unmatched_codes(src, codes, "add_diagnoses")
  swereg:::.start_code_check_session()   # inner
  swereg:::warn_unmatched_codes(src, codes, "add_diagnoses")
  swereg:::.end_code_check_session()     # inner -> no-op

  # State must still be there for outer scope.
  snap <- swereg:::.code_check_snapshot()
  expect_true(snap$unmatched$add_diagnoses$absent[["Z9999"]])
})

test_that(".code_check_emit() is silent on empty / NULL input", {
  expect_silent(swereg:::.code_check_emit(NULL))
  expect_silent(swereg:::.code_check_emit(list(unmatched = list(), empty = list())))
})

test_that("warn_unmatched_codes() with no session warns immediately (per-call)", {
  src <- data.table::data.table(code = c("I20", "I21"))
  cl  <- list(diag_typo = c("I20", "QQ99"))
  expect_warning(swereg:::warn_unmatched_codes(src, cl, "test"), "QQ99")
})
