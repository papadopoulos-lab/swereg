# Plan-layer ITT output (increment C): the "PP vs ITT" sheet reuses the
# combined-sensitivity writer with parameterised panel labels and the
# irr_itt / rates_itt slots. Pins that the labels propagate.

skip_if_not_installed("openxlsx")
skip_if_not_installed("data.table")

test_that(".write_combined_sensitivity honors left_label/right_label with ITT slots", {
  ett <- data.table::data.table(
    enrollment_id     = "01",
    ett_id            = "ETT00001",
    outcome_var       = "osd_a",
    outcome_name      = "Outcome A",
    follow_up         = 52L,
    age_min           = 50L,
    age_max           = 59L,
    age_group         = "50_59",
    confounder_vars   = "rd_age_continuous",
    person_id_var     = "lopnr",
    treatment_var     = "rd_tx",
    file_imp          = "imp_01.qs2",
    file_raw          = "raw_01.qs2",
    file_analysis     = "analysis_001.qs2",
    file_analysis_itt = "analysis_itt_001.qs2",
    description       = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
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
      irr_pp_trunc = irr_val,
      rates_itt = rates_dt,
      irr_itt = irr_val
    )
  )

  wb <- openxlsx::createWorkbook()
  swereg:::.write_combined_sensitivity(
    wb, "PP vs ITT", plan,
    trunc_rates_slot   = "rates_pp_trunc",
    trunc_irr_slot     = "irr_pp_trunc",
    untrunc_rates_slot = "rates_itt",
    untrunc_irr_slot   = "irr_itt",
    title              = "PP vs ITT",
    left_label         = "Per-protocol",
    right_label        = "Intention-to-treat"
  )

  out <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)
  openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
  flat <- unlist(
    openxlsx::read.xlsx(out, sheet = "PP vs ITT", colNames = FALSE,
                        skipEmptyRows = FALSE),
    use.names = FALSE
  )

  expect_true("Per-protocol" %in% flat)
  expect_true("Intention-to-treat" %in% flat)
  # The default truncated/untruncated labels must NOT appear here
  expect_false("Truncated weights" %in% flat)
  expect_false("Untruncated weights" %in% flat)
})

test_that("default panel labels are unchanged (regression)", {
  # A bare call (no label args) still prints Truncated/Untruncated.
  ett <- data.table::data.table(
    enrollment_id = "01", ett_id = "ETT00001", outcome_var = "osd_a",
    outcome_name = "Outcome A", follow_up = 52L, age_min = 50L,
    age_max = 59L, age_group = "50_59", confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr", treatment_var = "rd_tx", file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2", file_analysis = "analysis_001.qs2",
    description = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test", skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52", ett = ett
  )
  rates_dt <- data.table::data.table(
    rd_tx = c(TRUE, FALSE), events_weighted = c(10, 20),
    py_weighted = c(1000, 2000), rate_per_100000py = c(1000, 1000)
  )
  data.table::setattr(rates_dt, "treatment_var", "rd_tx")
  irr_val <- list(IRR = 1.0, IRR_lower = 0.5, IRR_upper = 2.0,
                  IRR_pvalue = 0.5, skipped = FALSE)
  plan$results_ett <- list(ETT00001 = list(
    enrollment_id = "01", description = "ETT00001",
    rates_pp_trunc = rates_dt, rates_pp = rates_dt,
    irr_pp_trunc = irr_val, irr_pp = irr_val))

  wb <- openxlsx::createWorkbook()
  swereg:::.write_combined_sensitivity(
    wb, "Full results", plan,
    trunc_rates_slot = "rates_pp_trunc", trunc_irr_slot = "irr_pp_trunc",
    untrunc_rates_slot = "rates_pp", untrunc_irr_slot = "irr_pp")
  out <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)
  openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
  flat <- unlist(openxlsx::read.xlsx(out, sheet = "Full results",
    colNames = FALSE, skipEmptyRows = FALSE), use.names = FALSE)
  expect_true("Truncated weights" %in% flat)
  expect_true("Untruncated weights" %in% flat)
})
