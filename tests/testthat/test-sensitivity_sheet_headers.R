# Pin the Sensitivity sheet column-header rename (Exposure ->
# Intervention, Events (exp) -> Events (int)). The rename was made in
# 26.4.27 to align workbook output with the rest of the TTE
# vocabulary, where "treatment" denotes the assignment and
# "intervention" denotes the active arm.
#
# We pin both:
#   1. Source-level: r6_tteplan.R no longer contains the old strings.
#      Catches accidental reverts at test time.
#   2. Functional: rendering `.write_combined_sensitivity()` to a
#      workbook produces the new headers in cell text. Catches drift
#      between the constant declaration and the writer.

skip_if_not_installed("openxlsx")
skip_if_not_installed("data.table")

test_that("source: r6_tteplan.R uses Intervention / (int) labels (not Exposure / (exp))", {
  src_path <- testthat::test_path("..", "..", "R", "r6_tteplan.R")
  skip_if_not(file.exists(src_path), "R/r6_tteplan.R not found (installed pkg?)")
  src <- readLines(src_path, warn = FALSE)

  # New labels must appear (sensitivity sheet header + per-arm cols).
  expect_true(any(grepl('"Intervention"', src, fixed = TRUE)),
    info = '"Intervention" header missing from r6_tteplan.R')
  expect_true(any(grepl('"Events (int)"', src, fixed = TRUE)),
    info = '"Events (int)" column header missing from r6_tteplan.R')

  # Old user-facing labels must NOT appear -- in *string-literal* form.
  # (Plain word "Exposure" might still show up in roxygen prose; the
  # quoted-string check is what matters for what gets written into a
  # workbook cell.)
  expect_false(any(grepl('"Exposure"', src, fixed = TRUE)),
    info = '"Exposure" string literal must not be reintroduced -- was renamed to "Intervention"')
  expect_false(any(grepl('"Events (exp)"', src, fixed = TRUE)),
    info = '"Events (exp)" must not be reintroduced -- was renamed to "Events (int)"')
  expect_false(any(grepl('"PY (exp)"', src, fixed = TRUE)))
  expect_false(any(grepl('"Rate/100k (exp)"', src, fixed = TRUE)))
})

test_that("functional: .write_combined_sensitivity emits the new headers into the worksheet", {
  # Build a TTEPlan with one ETT and a results_ett entry that has both
  # rates and IRR populated. .write_combined_sensitivity reads slot
  # names off `plan$results_ett[[ett_id]]` and writes header rows + a
  # single data row.

  ett <- data.table::data.table(
    enrollment_id   = "01",
    ett_id          = "ETT00001",
    outcome_var     = "osd_a",
    outcome_name    = "Outcome A",
    follow_up       = 52L,
    age_min         = 50L,
    age_max         = 59L,
    age_group       = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var   = "lopnr",
    treatment_var   = "rd_tx",
    file_imp        = "imp_01.qs2",
    file_raw        = "raw_01.qs2",
    file_analysis   = "analysis_001.qs2",
    description     = "ETT00001"
  )

  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  # Minimal rates / IRR result. .sensitivity_row_measurements() pulls
  # named scalar fields out -- we only need the keys to exist.
  rates_dt <- data.table::data.table(
    rd_tx = c(TRUE, FALSE),
    events_weighted = c(10, 20),
    py_weighted = c(1000, 2000),
    rate_per_100000py = c(1000, 1000)
  )
  data.table::setattr(rates_dt, "treatment_var", "rd_tx")
  irr_val <- list(IRR = 1.0, IRR_lower = 0.5, IRR_upper = 2.0,
                  IRR_pvalue = 0.5, skipped = FALSE)
  plan$results_ett <- list(
    ETT00001 = list(
      enrollment_id = "01",
      description = "ETT00001",
      rates_pp_trunc = rates_dt,
      rates_pp = rates_dt,
      irr_pp_trunc = irr_val,
      irr_pp = irr_val
    )
  )

  wb <- openxlsx::createWorkbook()
  swereg:::.write_combined_sensitivity(
    wb, "Sensitivity", plan,
    trunc_rates_slot   = "rates_pp_trunc",
    trunc_irr_slot     = "irr_pp_trunc",
    untrunc_rates_slot = "rates_pp",
    untrunc_irr_slot   = "irr_pp",
    title              = "Test sensitivity"
  )

  # Read back the worksheet contents and verify the headers we expect.
  out_path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out_path), add = TRUE)
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)

  # Read all cells; the header row appears somewhere near the top.
  contents <- openxlsx::read.xlsx(out_path, sheet = "Sensitivity",
                                  colNames = FALSE, skipEmptyRows = FALSE)
  flat <- unlist(contents, use.names = FALSE)

  expect_true("Intervention" %in% flat,
              info = "Intervention header not written to sensitivity sheet")
  expect_true("Comparator"   %in% flat)
  expect_true("Events (int)" %in% flat,
              info = "Events (int) header not written to sensitivity sheet")
  expect_true("Events (cmp)" %in% flat)
  # And confirm the OLD labels are absent.
  expect_false("Exposure"     %in% flat)
  expect_false("Events (exp)" %in% flat)
})
