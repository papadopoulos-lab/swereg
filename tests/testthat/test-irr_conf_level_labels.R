# The IRR interval headers state the study level `.s3_conf_level()`, the level
# s3 computed the intervals at. A study at 90 percent MUST NOT print "95% CI".

skip_if_not_installed("openxlsx")

.lbl_plan <- function(conf_level = NULL) {
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = "ETT00001",
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    follow_up = 52L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    subgroup_vars = list("Z"),
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = "analysis_001.qs2",
    file_analysis_itt = "analysis_itt_001.qs2",
    description = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  if (!is.null(conf_level)) {
    plan$spec <- list(study = list(implementation = list(conf_level = conf_level)))
  }
  rates_dt <- data.table::data.table(
    rd_tx = c(TRUE, FALSE),
    events_weighted = c(10.4, 20.6),
    py_weighted = c(62816, 98765),
    rate_per_100000py = c(16.9, 12.0)
  )
  data.table::setattr(rates_dt, "treatment_var", "rd_tx")
  irr <- list(
    IRR = 0.54,
    IRR_lower = 0.40,
    IRR_upper = 0.71,
    IRR_pvalue = 0.001,
    skipped = FALSE
  )
  sub_tab <- data.table::data.table(
    level = c("all", "0", "1"),
    IRR = c(2, 2, 4),
    IRR_lower = c(1.5, 1.5, 3),
    IRR_upper = c(2.5, 2.5, 5),
    IRR_pvalue = c(0.01, 0.02, 0.001),
    warn = FALSE
  )
  emt <- list(p_value = 0.01, ratio_of_irrs = 2.0, n_levels = 2L)
  plan$results_ett <- list(
    ETT00001 = list(
      enrollment_id = "01",
      description = "ETT00001",
      rates_pp_trunc = rates_dt,
      rates_pp = rates_dt,
      rates_itt = rates_dt,
      irr_pp_trunc = irr,
      irr_pp = irr,
      irr_itt = irr,
      subgroup_Z_pp = sub_tab,
      subgroup_Z_itt = sub_tab,
      emtest_Z_pp = emt,
      emtest_Z_itt = emt
    )
  )
  plan
}

# Saves once. A second save of the same workbook warns from openxlsx.
.lbl_sheet_cells <- function(wb, sheets) {
  p <- tempfile(fileext = ".xlsx")
  on.exit(unlink(p))
  openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
  out <- lapply(sheets, function(sheet) {
    unlist(
      openxlsx::read.xlsx(p, sheet = sheet, colNames = FALSE),
      use.names = FALSE
    )
  })
  names(out) <- sheets
  out
}

.lbl_overlay_labels <- function(rendered) {
  out <- character(0)
  for (i in seq_len(length(rendered$plot))) {
    for (d in ggplot2::ggplot_build(rendered$plot[[i]])$data) {
      if ("label" %in% names(d)) {
        out <- c(out, as.character(d$label))
      }
    }
  }
  out
}

test_that("IRR interval labels state a non-default conf_level", {
  skip_if_not_installed("ggplot2")
  plan <- .lbl_plan(conf_level = 0.9)
  img_dir <- tempfile("img")
  dir.create(img_dir)
  on.exit(unlink(img_dir, recursive = TRUE), add = TRUE)

  wb <- openxlsx::createWorkbook()
  swereg:::.write_itt_vs_pp_forest(
    wb,
    "ITT vs PP forest",
    plan,
    keep_ett_ids = "ETT00001",
    img_dir = img_dir,
    img_basename = "itt_vs_pp"
  )
  swereg:::.write_effect_modification(wb, "Effect modification", plan)
  sheets <- c("ITT vs PP forest", "Effect modification")
  all_cells <- .lbl_sheet_cells(wb, sheets)
  for (sheet in sheets) {
    cells <- all_cells[[sheet]]
    expect_true(all(c("ITT 90% CI", "PP 90% CI") %in% cells), info = sheet)
    expect_false(any(grepl("95% CI", cells, fixed = TRUE)), info = sheet)
  }

  df <- swereg:::.build_itt_vs_pp_df(plan, keep_ett_ids = "ETT00001")
  drawn <- .lbl_overlay_labels(
    swereg:::.render_itt_vs_pp_overlay(df, conf_level = 0.9)
  )
  expect_true(all(c("PP IRR (90% CI)", "ITT IRR (90% CI)") %in% drawn))
  expect_false(any(grepl("95% CI", drawn, fixed = TRUE)))
})

test_that("IRR interval labels default to 95 when the study names no level", {
  skip_if_not_installed("ggplot2")
  plan <- .lbl_plan()
  wb <- openxlsx::createWorkbook()
  swereg:::.write_effect_modification(wb, "Effect modification", plan)
  cells <- .lbl_sheet_cells(wb, "Effect modification")[[1]]
  expect_true(all(c("ITT 95% CI", "PP 95% CI") %in% cells))

  df <- swereg:::.build_itt_vs_pp_df(plan, keep_ett_ids = "ETT00001")
  drawn <- .lbl_overlay_labels(swereg:::.render_itt_vs_pp_overlay(df))
  expect_true(all(c("PP IRR (95% CI)", "ITT IRR (95% CI)") %in% drawn))
})

test_that("the ITT vs PP forest writer passes the study level to its figure", {
  skip_if_not_installed("ggplot2")
  plan <- .lbl_plan(conf_level = 0.9)
  img_dir <- tempfile("img")
  dir.create(img_dir)
  on.exit(unlink(img_dir, recursive = TRUE), add = TRUE)
  seen <- NULL
  orig <- swereg:::.render_itt_vs_pp_overlay
  local_mocked_bindings(
    .render_itt_vs_pp_overlay = function(df, ...) {
      seen <<- list(...)$conf_level
      orig(df, ...)
    }
  )
  swereg:::.write_itt_vs_pp_forest(
    openxlsx::createWorkbook(),
    "ITT vs PP forest",
    plan,
    keep_ett_ids = "ETT00001",
    img_dir = img_dir,
    img_basename = "itt_vs_pp"
  )
  expect_identical(seen, 0.9)
})

test_that("the results and sensitivity sheets head the IRR interval at conf_level", {
  for (lvl in list(NULL, 0.9)) {
    plan <- .lbl_plan(conf_level = lvl)
    want <- if (is.null(lvl)) "95% CI" else "90% CI"
    wb <- openxlsx::createWorkbook()
    swereg:::.write_results_single(
      wb,
      "PP results",
      plan,
      rates_slot = "rates_pp_trunc",
      irr_slot = "irr_pp_trunc"
    )
    swereg:::.write_results_single(
      wb,
      "ITT results",
      plan,
      rates_slot = "rates_itt",
      irr_slot = "irr_itt"
    )
    swereg:::.write_combined_sensitivity(
      wb,
      "Full results",
      plan,
      trunc_rates_slot = "rates_pp_trunc",
      trunc_irr_slot = "irr_pp_trunc",
      untrunc_rates_slot = "rates_pp",
      untrunc_irr_slot = "irr_pp"
    )
    sheets <- c("PP results", "ITT results", "Full results")
    all_cells <- .lbl_sheet_cells(wb, sheets)
    for (sheet in sheets) {
      cells <- all_cells[[sheet]]
      expect_true(want %in% cells, info = paste(sheet, want))
      if (!is.null(lvl)) {
        expect_false("95% CI" %in% cells, info = sheet)
      }
    }
  }
})

test_that("tteenrollment_irr_combine() heads the interval at conf_level", {
  results <- list(
    ETT00001 = list(
      irr_pp = data.table::data.table(
        IRR = 0.54,
        IRR_lower = 0.40,
        IRR_upper = 0.71,
        IRR_pvalue = 0.001,
        warn = FALSE
      )
    )
  )
  d95 <- tteenrollment_irr_combine(results, "irr_pp")
  expect_identical(names(d95), c("ett_id", "IRR", "95% CI", "p-value"))
  d90 <- tteenrollment_irr_combine(results, "irr_pp", conf_level = 0.9)
  expect_identical(names(d90), c("ett_id", "IRR", "90% CI", "p-value"))
  expect_identical(d90[["90% CI"]], "0.40 to 0.71")
})

test_that("tteenrollment_combined_combine() heads the interval at conf_level", {
  rates <- data.table::data.table(
    rd_tx = c(TRUE, FALSE),
    events_weighted = c(10.4, 20.6),
    py_weighted = c(62816, 98765),
    rate_per_100000py = c(16.9, 12.0)
  )
  data.table::setattr(rates, "treatment_var", "rd_tx")
  results <- list(
    ETT00001 = list(
      rates_pp = rates,
      irr_pp = data.table::data.table(
        IRR = 0.54,
        IRR_lower = 0.40,
        IRR_upper = 0.71,
        IRR_pvalue = 0.001,
        warn = FALSE
      )
    )
  )
  d95 <- tteenrollment_combined_combine(results, "rates_pp", "irr_pp")
  expect_true("95% CI" %in% names(d95))
  d90 <- tteenrollment_combined_combine(
    results,
    "rates_pp",
    "irr_pp",
    conf_level = 0.9
  )
  expect_true("90% CI" %in% names(d90))
  expect_false("95% CI" %in% names(d90))
})

test_that("the forest summary tables carry the study level in their headers", {
  plan <- .lbl_plan(conf_level = 0.9)
  img_dir <- tempfile("img")
  dir.create(img_dir)
  on.exit(unlink(img_dir, recursive = TRUE), add = TRUE)
  # The figure is not under test here. A stub keeps it out of the run.
  local_mocked_bindings(
    .render_itt_vs_pp_overlay = function(df, ...) stop("figure not drawn")
  )
  wb <- openxlsx::createWorkbook()
  expect_warning(
    swereg:::.write_itt_vs_pp_forest(
      wb,
      "ITT vs PP forest",
      plan,
      keep_ett_ids = "ETT00001",
      img_dir = img_dir,
      img_basename = "itt_vs_pp"
    ),
    "figure not drawn"
  )
  swereg:::.write_effect_modification(wb, "Effect modification", plan)
  sheets <- c("ITT vs PP forest", "Effect modification")
  all_cells <- .lbl_sheet_cells(wb, sheets)
  expect_true(
    all(c("ITT 90% CI", "PP 90% CI") %in% all_cells[["ITT vs PP forest"]])
  )
  expect_true(
    all(c("ITT 90% CI", "PP 90% CI") %in% all_cells[["Effect modification"]])
  )
})
