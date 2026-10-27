# The two "Minimum events" sheets of `$export_tables()`, and the rule that
# the intention-to-treat estimate follows the per-protocol decision.
#
# Both sheets hold one row per outcome and comparison and one column per
# follow-up horizon, per-protocol only. The test stores horizon records on
# the export fixture plan, runs the real `$export_tables()`, and reads the
# sheets back with openxlsx. Every expected value below is computed by hand
# from the toy panel.
#
# ETT00001 (52 weeks), per-protocol, window 0-52: the intervention arm has
# 2 weighted events and the comparator arm 4, so the expected events are 2
# and 4. They reach the cut of 2 and no larger cut of the grid. The
# intention-to-treat window 0-52 has 1 and 1 weighted events, so 1 and 1
# expected events: it fails the cut of 2 on its own weights.
#
# ETT00005 (156 weeks), per-protocol, window 52-104: the intervention arm has
# 0 weighted events in 1 weighted year and the comparator arm 3 in 3 weighted
# years, so the expected events are 0.75 and 2.25. It fails at every cut of
# the grid.

# Rows are (tstart, tstop]. `w_pp` is the per-protocol weight and `w_itt` the
# intention-to-treat weight.
#
#   arm  tstart tstop w_pp w_itt event
#   int       0    52    2     1     1
#   int      52   104    1     1     0
#   int     104   130    1     1     1
#   cmp       0    52    4     1     1
#   cmp      52   104    3     1     1
#   cmp     104   156    2     1     0
.hs_panel <- function() {
  return(data.table::data.table(
    rd_tx = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    tstart = c(0, 52, 104, 0, 52, 104),
    tstop = c(52, 104, 130, 52, 104, 156),
    w_pp = c(2, 1, 1, 4, 3, 2),
    w_itt = 1,
    event = c(1L, 0L, 1L, 1L, 1L, 0L)
  ))
}

.hs_rule <- list(min_expected_events = 2)

# The export fixture plan, with horizon records on ETT00001 (52 weeks) and
# ETT00005 (156 weeks). The parent merge of `$s3_analyze()` then applies them.
.hs_plan <- function() {
  plan <- .xp_plan("new")
  plan$spec$min_expected_events_per_arm_per_year <- 2
  # `.xp_plan()` stamps each result with a record of its own rule, 0. The
  # record of this file holds the rule of this spec.
  for (id in names(plan$results_ett)) {
    for (est in c("pp", "itt")) {
      plan$results_ett[[id]][[paste0("horizon_", est)]]$rule <- .hs_rule
    }
  }
  d <- .hs_panel()
  for (id in c("ETT00001", "ETT00005")) {
    fu <- plan$ett$follow_up[plan$ett$ett_id == id]
    panel <- d[tstart < fu]
    for (est in c("pp", "itt")) {
      plan$results_ett[[id]][[paste0("horizon_", est)]] <-
        swereg:::.tte_horizon_record(
          panel,
          weight_col = paste0("w_", est),
          follow_up = fu,
          rule = .hs_rule,
          treatment_var = "rd_tx"
        )
    }
  }
  plan$results_ett <- swereg:::.s3_apply_horizon_rule(
    plan$results_ett,
    c("ETT00001", "ETT00005")
  )
  return(plan)
}

.hs_export <- function(plan) {
  dir <- tempfile("hs-export")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = parent.frame())
  path <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = path)))
  return(path)
}

.hs_sheet <- function(path, sheet) {
  return(openxlsx::read.xlsx(
    path,
    sheet = sheet,
    startRow = 4,
    check.names = FALSE,
    sep.names = " "
  ))
}

test_that("the largest passing cut is the largest grid value the smallest expected events reach", {
  f <- swereg:::.tte_horizon_max_passing_cut
  expect_identical(f(c(5, 7)), 5)
  expect_identical(f(c(4.999, 9)), 4)
  expect_identical(f(c(10, 19.99)), 10)
  expect_identical(f(c(25, 30)), 20)
  expect_identical(f(c(1, 3)), 1)
  expect_identical(f(c(0.999, 3)), NA_real_)
  expect_identical(f(c(0, 0)), NA_real_)
})

test_that("$export_tables() writes the two Minimum events sheets after Study Specification", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  path <- .hs_export(.hs_plan())
  sheets <- openxlsx::getSheetNames(path)
  expect_false("Minimum events" %in% sheets)
  spec <- match("Study Specification", sheets)
  expect_identical(
    sheets[spec + 1:2],
    c("Minimum events (Y or -)", "Minimum events (#)")
  )
  expect_identical(sheets[spec + 3L], "Target trial protocol")
})

test_that("the Y or - sheet shows the per-protocol decision, outcome first", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  d <- .hs_sheet(.hs_export(.hs_plan()), "Minimum events (Y or -)")
  expect_identical(names(d), c("Outcome", "#", "Comparison", "52 weeks", "156 weeks"))
  expect_identical(d$Outcome[1:3], c("Outcome A", NA, "Outcome B"))
  expect_equal(d$`#`[1:2], c(1, 2))
  # ETT00001: per-protocol passes at the cut of 2.
  expect_identical(d[1, "52 weeks"], "Y")
  # ETT00005: per-protocol fails in window 52-104.
  expect_identical(d[5, "156 weeks"], "-")
})

test_that("the # sheet shows the largest cut of the grid at which each estimate passes", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  d <- .hs_sheet(.hs_export(.hs_plan()), "Minimum events (#)")
  expect_identical(as.character(d[1, "52 weeks"]), "2")
  expect_identical(as.character(d[5, "156 weeks"]), "-")
})

test_that("a reported cell is filled blue and a withheld cell red", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  wb <- openxlsx::loadWorkbook(.hs_export(.hs_plan()))
  fill_of <- function(sheet, row, col) {
    for (so in wb$styleObjects) {
      if (so$sheet == sheet && any(so$rows == row & so$cols == col)) {
        f <- so$style$fill$fillFg
        if (!is.null(f)) return(toupper(sub("^FF", "#", f[[1]])))
      }
    }
    return(NA_character_)
  }
  for (sh in c("Minimum events (Y or -)", "Minimum events (#)")) {
    # Data start on row 5. ETT00001 is row 5, column 4; ETT00005 row 9,
    # column 5.
    expect_identical(fill_of(sh, 5L, 4L), "#BDD7EE")
    expect_identical(fill_of(sh, 9L, 5L), "#F4B6B6")
  }
})

test_that("the intention-to-treat estimate follows the per-protocol decision", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  plan <- .hs_plan()
  e <- plan$get_estimates()
  # ETT00001: the intention-to-treat window fails on its own weights, but the
  # per-protocol decision passes, so the intention-to-treat ratio is reported.
  itt1 <- e[ett_id == "ETT00001" & estimand == "itt"]
  expect_identical(itt1$horizon_status, swereg:::.HORIZON_STATUS_PASS)
  expect_true(is.finite(itt1$irr))
  expect_identical(plan$results_ett$ETT00001$horizon_itt$decided_by, "pp")
  expect_false(all(plan$results_ett$ETT00001$horizon_itt$windows$pass))
  # ETT00005: the per-protocol decision withholds both estimands from week 52.
  pp5 <- e[ett_id == "ETT00005" & estimand == "pp" & weights == "truncated"]
  itt5 <- e[ett_id == "ETT00005" & estimand == "itt"]
  expect_identical(
    pp5$horizon_status,
    "below minimum-events rule: intervention arm, weeks 52-104"
  )
  expect_identical(itt5$horizon_status, pp5$horizon_status)
  expect_true(is.na(itt5$irr))
  expect_identical(
    plan$results_ett$ETT00005$horizon_itt$reportable_weeks,
    52
  )
})

test_that("two ETTs that would share one cell stop the sheet writer", {
  plan <- .hs_plan()
  ett <- data.table::copy(plan$ett)
  ett[ett_id == "ETT00002", outcome_name := "Outcome A"]
  plan$ett <- ett
  expect_error(
    swereg:::.minimum_events_wide(plan, list()),
    "ETT00001, ETT00002\\) share enrollment 01, outcome 'Outcome A'"
  )
})

test_that("the exporters refuse an intention-to-treat result decided on its own", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  plan <- .hs_plan()
  plan$results_ett$ETT00001$horizon_itt$decided_by <- NULL
  expect_error(
    .hs_export(plan),
    "judged on their own, as before swereg 27.3.0"
  )
})
