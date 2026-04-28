# Pin behaviour when every IRR result is `skipped = TRUE` (the silent
# failure mode produced by missing `survey` package, etc.). The forest
# plot module must filter those rows out and emit a sentinel message
# rather than crash, and must not produce empty PNG/PDF sidecars.
#
# This is the symptom that produced the "no figures in s4 output"
# debugging session: 135/135 ETTs skipped -> .build_forest_df returns
# NULL -> .write_forest_irr writes a "No valid IRR results" notice.

skip_if_not_installed("data.table")

.fixture_plan_skipped <- function() {
  ett <- data.table::data.table(
    enrollment_id   = c("01", "01"),
    ett_id          = c("ETT001", "ETT002"),
    outcome_var     = "osd_a",
    outcome_name    = "Outcome A",
    follow_up       = c(52L, 156L),
    age_min         = 50L,
    age_max         = 59L,
    age_group       = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var   = "lopnr",
    treatment_var   = "rd_tx",
    file_imp        = "imp_01.qs2",
    file_raw        = "raw_01.qs2",
    file_analysis   = c("analysis_001.qs2", "analysis_002.qs2"),
    description     = c("ETT001", "ETT002")
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  # Every ETT has only the skipped placeholder -- mimics the
  # missing-survey-package failure mode.
  plan$results_ett <- list(
    ETT001 = list(
      enrollment_id = "01",
      description = "ETT001",
      irr_pp_trunc = list(skipped = TRUE,
                          reason = "there is no package called 'survey'"),
      irr_pp       = list(skipped = TRUE,
                          reason = "there is no package called 'survey'")
    ),
    ETT002 = list(
      enrollment_id = "01",
      description = "ETT002",
      irr_pp_trunc = list(skipped = TRUE,
                          reason = "there is no package called 'survey'"),
      irr_pp       = list(skipped = TRUE,
                          reason = "there is no package called 'survey'")
    )
  )
  plan
}

test_that(".build_forest_df: returns NULL when every IRR is skipped", {
  plan <- .fixture_plan_skipped()
  df <- swereg:::.build_forest_df(
    plan,
    rates_slot = "rates_pp_trunc",
    irr_slot   = "irr_pp_trunc"
  )
  expect_null(df,
              info = "with all-skipped IRRs, .build_forest_df should return NULL")
})

test_that(".build_forest_df: skips entries missing IRR/IRR_lower/IRR_upper", {
  plan <- .fixture_plan_skipped()
  # Even if `skipped` flag isn't set, missing CI bounds means the row
  # cannot drive a forest plot. .build_forest_df must filter those.
  plan$results_ett[[1]]$irr_pp_trunc <- list(IRR = 1.0)  # missing lower/upper
  plan$results_ett[[2]]$irr_pp_trunc <- list(IRR = 1.0)
  df <- swereg:::.build_forest_df(
    plan,
    rates_slot = "rates_pp_trunc",
    irr_slot   = "irr_pp_trunc"
  )
  expect_null(df)
})

test_that(".build_forest_df: keeps a partial set when at least one IRR is valid", {
  plan <- .fixture_plan_skipped()
  # Promote ETT002 to a real IRR; keep ETT001 skipped.
  plan$results_ett[[2]]$irr_pp_trunc <- list(
    IRR = 1.5, IRR_lower = 0.9, IRR_upper = 2.5,
    IRR_pvalue = 0.1, skipped = FALSE
  )
  rates_dt <- data.table::data.table(
    rd_tx = c(TRUE, FALSE),
    events_weighted = c(10, 20),
    py_weighted = c(1000, 2000),
    rate_per_100000py = c(1000, 1000)
  )
  data.table::setattr(rates_dt, "treatment_var", "rd_tx")
  plan$results_ett[[2]]$rates_pp_trunc <- rates_dt

  df <- swereg:::.build_forest_df(
    plan,
    rates_slot = "rates_pp_trunc",
    irr_slot   = "irr_pp_trunc"
  )
  expect_false(is.null(df))
  expect_equal(nrow(df), 1L)
  expect_equal(df$ett_id, "ETT002")
})

test_that("source: warning helper exists for the empty-forest path", {
  # The .write_forest_irr writer falls through to writeData() with the
  # sentinel "No valid IRR results to plot." string when df is NULL.
  # If that string is renamed without updating tests / docs, downstream
  # consumers (and reviewers) lose the diagnostic. Pin the literal.
  src_path <- testthat::test_path("..", "..", "R", "forest_plot.R")
  skip_if_not(file.exists(src_path))
  src <- readLines(src_path, warn = FALSE)
  expect_true(any(grepl("No valid IRR results", src, fixed = TRUE)),
              info = "sentinel 'No valid IRR results' message missing from forest_plot.R")
})
