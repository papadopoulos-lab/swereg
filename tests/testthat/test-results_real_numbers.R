# Pin the "real numbers, formatted in Excel" behaviour and the per-estimand
# workbook restructure (v008+). events / PY / rate / p-value are written as
# bare numerics -- so they sort, sum, and never trip Excel's "number stored as
# text" warning -- while IRR + 95% CI stay human-formatted display strings
# (like Table 1's "n (%)"). Also pins the single-estimand results writer and
# the PP-vs-ITT head-to-head numeric table.

skip_if_not_installed("openxlsx")
skip_if_not_installed("data.table")

make_min_plan <- function() {
  ett <- data.table::data.table(
    enrollment_id = "01", ett_id = "ETT00001",
    outcome_var = "osd_a", outcome_name = "Outcome A",
    follow_up = 52L, age_min = 50L, age_max = 59L, age_group = "50_59",
    confounder_vars = "rd_age_continuous", person_id_var = "lopnr",
    treatment_var = "rd_tx", file_imp = "imp_01.qs2", file_raw = "raw_01.qs2",
    file_analysis = "analysis_001.qs2", description = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test", skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52", ett = ett
  )
  rates_dt <- data.table::data.table(
    rd_tx = c(TRUE, FALSE),
    events_weighted = c(10.4, 20.6),
    py_weighted = c(62816, 98765),
    rate_per_100000py = c(16.9, 12.0)
  )
  data.table::setattr(rates_dt, "treatment_var", "rd_tx")
  irr_pp <- list(IRR = 0.54, IRR_lower = 0.40, IRR_upper = 0.71,
                 IRR_pvalue = 0.0000001, skipped = FALSE)
  irr_itt <- list(IRR = 0.61, IRR_lower = 0.45, IRR_upper = 0.83,
                  IRR_pvalue = 0.002, skipped = FALSE)
  plan$results_ett <- list(
    ETT00001 = list(
      enrollment_id = "01", description = "ETT00001",
      rates_pp_trunc = rates_dt, rates_pp = rates_dt, rates_itt = rates_dt,
      irr_pp_trunc = irr_pp, irr_pp = irr_pp, irr_itt = irr_itt
    )
  )
  plan
}

test_that(".sensitivity_row_fmt: counts/rates/p numeric, IRR + CI display strings", {
  m <- list(events_intervention = 10.4, py_intervention = 62816,
            rate_intervention = 16.9, events_cmp = 20.6, py_cmp = 98765,
            rate_cmp = 12.0, irr = 0.54, lo = 0.40, hi = 0.71, pvalue = 0.002)
  cells <- swereg:::.sensitivity_row_fmt(m, "")
  expect_true(is.numeric(cells[["Events (int)"]]))
  expect_true(is.numeric(cells[["PY (int)"]]))
  expect_true(is.numeric(cells[["Rate/100k (int)"]]))
  expect_true(is.numeric(cells[["p-value"]]))
  expect_true(is.character(cells[["IRR"]]))
  expect_true(is.character(cells[["95% CI"]]))
  expect_identical(cells[["IRR"]], "0.54")
  # a missing block keeps numeric columns numeric (NA_real_, not "-")
  empty <- swereg:::.sensitivity_row_fmt(NULL, "")
  expect_true(is.numeric(empty[["Events (int)"]]))
  expect_true(is.na(empty[["Events (int)"]]))
})

test_that(".write_results_single: cells round-trip as real numbers", {
  plan <- make_min_plan()
  wb <- openxlsx::createWorkbook()
  swereg:::.write_results_single(wb, "PP results", plan,
    rates_slot = "rates_pp_trunc", irr_slot = "irr_pp_trunc",
    title = "PP results")
  p <- tempfile(fileext = ".xlsx")
  on.exit(unlink(p), add = TRUE)
  openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
  # title row 1, blank row 2, header row 3, data row 4
  d <- openxlsx::read.xlsx(p, sheet = "PP results", startRow = 3)
  expect_true(is.numeric(d[[6]]))    # Events (int)
  expect_true(is.numeric(d[[7]]))    # PY (int)
  expect_true(is.numeric(d[[14]]))   # p-value
  expect_true(is.character(d[[12]])) # IRR display string
  expect_equal(d[[7]][1], 62816)
})

test_that(".build_pp_vs_itt_df merges PP and ITT IRRs onto shared rows", {
  plan <- make_min_plan()
  df <- swereg:::.build_pp_vs_itt_df(plan, keep_ett_ids = "ETT00001")
  expect_true(all(c("irr_pp", "irr_itt") %in% names(df)))
  expect_equal(df$irr_pp[1], 0.54)
  expect_equal(df$irr_itt[1], 0.61)
})

test_that(".write_pp_vs_itt_forest: PP IRR + ITT IRR columns are real numbers", {
  skip_if_not_installed("ggplot2")
  plan <- make_min_plan()
  wb <- openxlsx::createWorkbook()
  img_dir <- tempfile("img")
  dir.create(img_dir)
  on.exit(unlink(img_dir, recursive = TRUE), add = TRUE)
  swereg:::.write_pp_vs_itt_forest(wb, "PP vs ITT forest", plan,
    keep_ett_ids = "ETT00001", title = "PP vs ITT",
    img_dir = img_dir, img_basename = "pp_vs_itt")
  p <- tempfile(fileext = ".xlsx")
  on.exit(unlink(p), add = TRUE)
  openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
  d <- openxlsx::read.xlsx(p, sheet = "PP vs ITT forest", startRow = 3)
  expect_true(is.numeric(d[[4]]))  # PP IRR
  expect_true(is.numeric(d[[7]]))  # ITT IRR
  expect_equal(d[[4]][1], 0.54)
  expect_equal(d[[7]][1], 0.61)
})
