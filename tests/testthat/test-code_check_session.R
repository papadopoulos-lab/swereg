test_that("start/end_code_check_session() aggregates across batches without false positives", {
  # Two batches: rare code F12 only appears in batch 2. Per-batch checks would
  # warn on batch 1; session aggregation should not warn at all.
  src1 <- data.frame(diag = c("F32", "F33"), stringsAsFactors = FALSE)
  src2 <- data.frame(diag = c("F12", "F32"), stringsAsFactors = FALSE)
  codes <- list(common = c("F32"), rare = c("F12"))

  swereg::start_code_check_session()
  expect_silent(swereg::warn_unmatched_codes(src1, codes, "add_diagnoses"))
  expect_silent(swereg::warn_unmatched_codes(src2, codes, "add_diagnoses"))
  expect_silent(swereg::end_code_check_session())
})

test_that("end_code_check_session() warns only about literals never matched anywhere", {
  src1 <- data.frame(diag = c("F32"), stringsAsFactors = FALSE)
  src2 <- data.frame(diag = c("F33"), stringsAsFactors = FALSE)
  # Z9999 never matches in any batch -> should be reported.
  codes <- list(present = c("F32", "F33"), absent = c("Z9999"))

  swereg::start_code_check_session()
  swereg::warn_unmatched_codes(src1, codes, "add_diagnoses")
  swereg::warn_unmatched_codes(src2, codes, "add_diagnoses")
  expect_warning(
    swereg::end_code_check_session(),
    regexp = "Z9999"
  )
})

test_that("session aggregates warn_empty_logical_cols across batches", {
  # Skeleton in batch 1 has rare=FALSE, batch 2 has rare=TRUE -> no warning
  # for `rare`; `never` is always FALSE -> single warning.
  codes <- list(rare = "x", never = "y")
  sk1 <- data.table::data.table(id = 1:2, rare = c(FALSE, FALSE),
                                never = c(FALSE, FALSE))
  sk2 <- data.table::data.table(id = 1:2, rare = c(TRUE, FALSE),
                                never = c(FALSE, FALSE))

  swereg::start_code_check_session()
  expect_silent(swereg::warn_empty_logical_cols(sk1, codes, "add_diagnoses"))
  expect_silent(swereg::warn_empty_logical_cols(sk2, codes, "add_diagnoses"))
  w <- tryCatch(
    {
      withCallingHandlers(
        swereg::end_code_check_session(),
        warning = function(cnd) {
          assign("msg", conditionMessage(cnd), envir = parent.frame(2))
          invokeRestart("muffleWarning")
        }
      )
      get0("msg", ifnotfound = "")
    }
  )
  expect_match(w, "never")
  expect_false(grepl("\\brare\\b", w))
})

test_that("end_code_check_session() with no active session is a no-op", {
  expect_silent(swereg::end_code_check_session())
})

test_that("sessions nest safely via reference counting", {
  # Outer manual session + inner auto-session (e.g. process_skeletons).
  # The inner end must NOT clobber the outer accumulators; only the
  # outermost end emits the consolidated warning.
  src <- data.frame(diag = "F32", stringsAsFactors = FALSE)
  codes <- list(absent = c("Z9999"))

  swereg::start_code_check_session()   # outer
  swereg::warn_unmatched_codes(src, codes, "add_diagnoses")
  swereg::start_code_check_session()   # inner (auto)
  swereg::warn_unmatched_codes(src, codes, "add_diagnoses")
  expect_silent(swereg::end_code_check_session())  # inner end -> no-op
  expect_warning(
    swereg::end_code_check_session(),  # outer end -> emits
    regexp = "Z9999"
  )
})
