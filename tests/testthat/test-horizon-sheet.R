# The Minimum events and Longest reportable sheets of `$export_tables()`, and
# the rule that the intention-to-treat estimate follows the per-protocol
# decision.
#
# Both sheets hold one row per comparison and one column group per outcome,
# per-protocol only. Minimum events has one column per follow-up year, and
# Longest reportable one per threshold. The test stores horizon records on
# the export fixture plan, runs the real `$export_tables()`, and reads the
# sheets back with openxlsx through `.ff_grid()`. Every expected value below
# is computed by hand from the toy panel.
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
# the grid. Its window 0-52 is the window of ETT00001, so year 1 of its
# `by_year` passes at the cut of 2, and years 2 and 3 fail at every cut.

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

test_that("$export_tables() writes Minimum events and Longest reportable after Attrition by arm", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  path <- .hs_export(.hs_plan())
  sheets <- openxlsx::getSheetNames(path)
  expect_false(any(
    c("Minimum events (Y or -)", "Minimum events (#)") %in% sheets
  ))
  att <- match("Attrition by arm", sheets)
  expect_identical(sheets[att + 1:2], c("Minimum events", "Longest reportable"))
  # The two sheets sit in the feasibility block, before the results sheets.
  expect_lt(att + 2L, match("Target trial protocol", sheets))
  expect_lt(match("Study Specification", sheets), att)
})

# The spec threshold of `.hs_plan()` is 2, so the column "threshold 2" of
# Longest reportable reads the stored verdict of the rule.
.HS_OWN <- "threshold 2"

test_that("Longest reportable shows the per-protocol decision at the spec threshold, one column group per outcome", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  g <- .ff_grid(.hs_export(.hs_plan()), "Longest reportable")
  expect_identical(g$label[1:2], c("ID", "Comparison"))
  expect_identical(
    unique(g$outcome[-(1:2)]),
    paste("Outcome", LETTERS[1:6])
  )
  expect_identical(
    .ff_grid_labels(g, "Outcome A"),
    paste("threshold", c(2, 5, 10, 20))
  )
  expect_identical(g$id, c("01", "02"))
  # ETT00001: per-protocol passes at the cut of 2, to its own 1 year.
  expect_identical(.ff_grid_cell(g, "01", "Outcome A", .HS_OWN), "1 year")
  # ETT00005: per-protocol fails in window 52-104, so years 2 and 3 fail
  # and the decision reports 1 year of its 3.
  expect_identical(.ff_grid_cell(g, "01", "Outcome C", .HS_OWN), "1 year")
})

test_that("Minimum events shows the largest cut of the grid at which each estimate passes", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  g <- .ff_grid(.hs_export(.hs_plan()), "Minimum events")
  expect_identical(.ff_grid_cell(g, "01", "Outcome A", "1 year"), "2")
  expect_identical(.ff_grid_cell(g, "01", "Outcome C", "3 years"), "-")
})

test_that("a cell reported to its last year is filled green and a withheld cell yellow or red", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  path <- .hs_export(.hs_plan())
  # Longest reportable, at the spec threshold: ETT00001 is reported to its
  # last year, and ETT00005 is withheld after year 1 of 3.
  sh <- "Longest reportable"
  g <- .ff_grid(path, sh)
  row <- g$first_row + match("01", g$id) - 1L
  col_a <- .ff_grid_col(g, "Outcome A", .HS_OWN)
  col_c <- .ff_grid_col(g, "Outcome C", .HS_OWN)
  expect_identical(.ff_fill(path, sh, row, col_a), "#A9D18E")
  expect_identical(.ff_fill(path, sh, row, col_c), "#FFE699")
  # Minimum events: the cut 2 takes the fill of 1 to 4, and the failed
  # year 3 of ETT00005 the fill of "-".
  sh <- "Minimum events"
  g <- .ff_grid(path, sh)
  row <- g$first_row + match("01", g$id) - 1L
  expect_identical(
    .ff_fill(path, sh, row, .ff_grid_col(g, "Outcome A", "1 year")),
    "#F8CBAD"
  )
  expect_identical(
    .ff_fill(path, sh, row, .ff_grid_col(g, "Outcome C", "3 years")),
    "#F4B6B6"
  )
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


# ---- the whole-year columns, (e) to (i) ---------------------------------------

# A one-enrollment, one-outcome plan with one ETT per follow-up of
# `follow_ups`, ids ETT00001, ETT00002, ... in that order.
.hs_toy_plan <- function(follow_ups) {
  n <- length(follow_ups)
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = sprintf("ETT%05d", seq_len(n)),
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    outcome_role = "primary",
    follow_up = as.integer(follow_ups),
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "id",
    treatment_var = "rd_tx",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = sprintf("analysis_%02d.qs2", seq_len(n)),
    description = sprintf("ETT%05d", seq_len(n))
  )
  return(swereg::TTEPlan$new(
    project_prefix = "hs",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  ))
}

# The per-protocol records of `.hs_panel()` cut at each follow-up of `plan`.
.hs_toy_records <- function(plan) {
  d <- .hs_panel()
  recs <- lapply(seq_len(nrow(plan$ett)), function(i) {
    fu <- plan$ett$follow_up[i]
    return(swereg:::.tte_horizon_record(
      d[tstart < fu],
      weight_col = "w_pp",
      follow_up = fu,
      rule = .hs_rule,
      treatment_var = "rd_tx"
    ))
  })
  names(recs) <- plan$ett$ett_id
  return(recs)
}

.hs_year_cols <- function(wide) {
  return(setdiff(names(wide$yn), c("Outcome", "#", "Comparison")))
}

test_that("(e) follow-ups 52 and 156 give one column per whole year, in both workbooks", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  path <- .hs_export(.hs_plan())
  feas <- file.path(dirname(path), "feasibility.xlsx")
  expect_true(file.exists(feas))
  for (xlsx in c(path, feas)) {
    g <- .ff_grid(xlsx, "Minimum events")
    for (o in paste("Outcome", LETTERS[1:6])) {
      expect_identical(
        .ff_grid_labels(g, o),
        c("1 year", "2 years", "3 years"),
        info = paste(basename(xlsx), o)
      )
    }
  }
})

test_that("(f) the 2-year cell equals the hand verdict from the 156-week windows", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  plan <- .hs_plan()
  # By hand: window 0-52 passes at 2 (expected events 2 and 4), window
  # 52-104 fails at every cut (0.75 and 2.25). So year 2 fails and has no
  # passing cut, and year 1 passes at 2.
  by_year <- plan$results_ett$ETT00005$horizon_pp$by_year
  expect_identical(by_year$year, 1:3)
  expect_identical(by_year$reportable, c(TRUE, FALSE, FALSE))
  expect_identical(by_year$max_passing_cut, c(2, NA, NA))
  path <- .hs_export(plan)
  lr <- .ff_grid(path, "Longest reportable")
  cut <- .ff_grid(path, "Minimum events")
  # Outcome C of enrollment 01 is the 156-week ETT00005, which has no
  # 52-week or 104-week ETT. Year 2 fails, so the spec threshold reports
  # 1 year.
  expect_identical(.ff_grid_cell(lr, "01", "Outcome C", .HS_OWN), "1 year")
  expect_identical(.ff_grid_cell(cut, "01", "Outcome C", "2 years"), "-")
  expect_identical(.ff_grid_cell(cut, "01", "Outcome C", "1 year"), "2")
  # No ETT of Outcome A reaches year 2, so its cells are empty, not "-".
  expect_true(is.na(.ff_grid_cell(cut, "01", "Outcome A", "2 years")))
  expect_true(is.na(.ff_grid_cell(cut, "01", "Outcome A", "3 years")))
  # Before swereg 27.3.13 row 3 of the Minimum events (Y or -) sheet said
  # that years without their own follow-up read the shortest longer one.
  # Neither Minimum events nor Longest reportable carries that note.
})

test_that("(g) the 1-year and 3-year cells equal the records of their own ETTs", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  plan <- .hs_plan()
  wide <- swereg:::.minimum_events_wide(
    plan,
    swereg:::.acc_horizon_records(plan, "pp")
  )
  r1 <- plan$results_ett$ETT00001$horizon_pp
  r5 <- plan$results_ett$ETT00005$horizon_pp
  as_yn <- function(rec) if (rec$withheld) "-" else "Y"
  as_cut <- function(rec) {
    if (is.na(rec$max_passing_cut)) "-" else format(rec$max_passing_cut)
  }
  expect_identical(wide$yn[1, "1 year"], as_yn(r1))
  expect_identical(wide$cut[1, "1 year"], as_cut(r1))
  expect_identical(wide$yn[5, "3 years"], as_yn(r5))
  expect_identical(wide$cut[5, "3 years"], as_cut(r5))
  expect_identical(c(wide$yn[1, "1 year"], wide$yn[5, "3 years"]), c("Y", "-"))
})

test_that("(h) an 80-week ETT gets no column, fills year 1 from by_year, and a 26-week ETT fills no cell", {
  plan <- .hs_toy_plan(c(26, 80))
  recs <- .hs_toy_records(plan)
  # The 26-week record fails; the 80-week record fails in window 52-80, but
  # year 1 of its by_year passes at 2.
  expect_true(recs$ETT00001$withheld)
  expect_true(recs$ETT00002$withheld)
  expect_identical(recs$ETT00002$by_year$reportable, c(TRUE, FALSE))
  wide <- swereg:::.minimum_events_wide(plan, recs)
  expect_identical(.hs_year_cols(wide), "1 year")
  expect_identical(wide$yn[1, "1 year"], "Y")
  expect_identical(wide$cut[1, "1 year"], "2")
  expect_true(wide$any_longer)
  expect_identical(dim(wide$withheld), c(1L, 1L))

  # A longer ETT that reaches 104 weeks adds year 2, read from that ETT.
  plan2 <- .hs_toy_plan(c(26, 80, 120))
  recs2 <- .hs_toy_records(plan2)
  wide2 <- swereg:::.minimum_events_wide(plan2, recs2)
  expect_identical(.hs_year_cols(wide2), c("1 year", "2 years"))
  expect_identical(wide2$yn[1, "1 year"], "Y")
  expect_identical(
    wide2$yn[1, "2 years"],
    if (recs2$ETT00003$by_year$reportable[2L]) "Y" else "-"
  )

  # Only follow-ups under one year: no year column at all.
  plan3 <- .hs_toy_plan(26)
  wide3 <- swereg:::.minimum_events_wide(plan3, .hs_toy_records(plan3))
  expect_identical(.hs_year_cols(wide3), character(0))
  expect_identical(nrow(wide3$yn), 1L)
})

test_that("(i) a feasibility record of 27.3.0 stops; an s3 record of 27.3.0 exports", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  plan <- .hs_toy_plan(c(52, 104))
  recs <- .hs_toy_records(plan)
  old <- swereg:::.tte_feasibility_strip(recs$ETT00002)
  old$by_year <- NULL
  expect_false("events_expected" %in% names(old$windows))
  expect_error(
    swereg:::.minimum_events_wide(
      plan,
      list(ETT00001 = recs$ETT00001, ETT00002 = old)
    ),
    "It is a feasibility record stored before swereg 27.3.1. Rerun the feasibility step",
    fixed = TRUE
  )

  # The s3 record of ETT00005 as swereg 27.3.0 stored it: no by_year, and
  # windows that hold events_expected. The sheet derives by_year.
  plan <- .hs_plan()
  for (est in c("pp", "itt")) {
    slot <- paste0("horizon_", est)
    plan$results_ett$ETT00005[[slot]]$by_year <- NULL
  }
  path <- .hs_export(plan)
  lr <- .ff_grid(path, "Longest reportable")
  cut <- .ff_grid(path, "Minimum events")
  expect_identical(.ff_grid_cell(lr, "01", "Outcome C", .HS_OWN), "1 year")
  expect_identical(
    vapply(
      c("1 year", "2 years", "3 years"),
      function(y) .ff_grid_cell(cut, "01", "Outcome C", y),
      character(1),
      USE.NAMES = FALSE
    ),
    c("2", "-", "-")
  )
})
