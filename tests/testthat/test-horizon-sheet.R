# The "Minimum events" sheet of `$export_tables()`.
#
# The sheet holds one row per ETT, estimand and follow-up year of the
# minimum-events rule. The test stores horizon records on the export fixture plan, runs the
# real `$export_tables()`, and reads the sheet back with openxlsx. Every
# expected value below is computed by hand from the toy panel.

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

.hs_headers <- c(
  "ETT",
  "Enrollment",
  "Outcome",
  "Follow-up (weeks)",
  "Estimand",
  "Window (weeks)",
  "PY (int)",
  "PY (cmp)",
  "Pooled rate per 100,000 PY",
  "Expected events (int)",
  "Expected events (cmp)",
  "Events (int)",
  "Events (cmp)",
  "Window passes",
  "Reportable horizon (weeks)"
)

test_that("$export_tables() writes the Minimum events sheet with hand-computed rows", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  path <- .hs_export(.hs_plan())
  expect_true("Minimum events" %in% openxlsx::getSheetNames(path))

  title <- openxlsx::read.xlsx(
    path,
    sheet = "Minimum events",
    colNames = FALSE,
    rows = 1L
  )[1, 1]
  expect_match(
    title,
    "min_expected_events_per_arm_per_year = 2",
    fixed = TRUE
  )

  d <- openxlsx::read.xlsx(
    path,
    sheet = "Minimum events",
    startRow = 4,
    sep.names = " ",
    check.names = FALSE
  )
  expect_identical(names(d), .hs_headers)
  # One row per ETT x estimand x window: 1 + 1 for ETT00001, 3 + 3 for
  # ETT00005. The other six trials store no record, so they have no rows.
  expect_identical(nrow(d), 8L)
  expect_identical(
    d$ETT,
    c("ETT00001", "ETT00001", rep("ETT00005", 6L))
  )
  expect_identical(
    d$Estimand,
    c(
      "per-protocol",
      "intention-to-treat",
      rep("per-protocol", 3L),
      rep("intention-to-treat", 3L)
    )
  )
  expect_identical(
    d$`Window (weeks)`,
    c("0-52", "0-52", "0-52", "52-104", "104-156", "0-52", "52-104", "104-156")
  )
  expect_identical(d$Enrollment, rep("Enrollment one", 8L))
  expect_identical(d$Outcome, c("Outcome A", "Outcome A", rep("Outcome C", 6L)))
  expect_equal(d$`Follow-up (weeks)`, c(52, 52, rep(156, 6L)))

  # Weighted person-weeks of each arm in each window, divided by 52.25.
  # PP, window 1: int 2 * 52 = 104, cmp 4 * 52 = 208.
  # PP, window 2: int 52, cmp 3 * 52 = 156. PP, window 3: int 26, cmp 104.
  # ITT: int 52, 52, 26 and cmp 52, 52, 52.
  py_int <- c(104, 52, 104, 52, 26, 52, 52, 26) / 52.25
  py_cmp <- c(208, 52, 208, 156, 104, 52, 52, 52) / 52.25
  # Weighted events. PP: int 2, 0, 1 and cmp 4, 3, 0. ITT: int 1, 0, 1 and
  # cmp 1, 1, 0.
  ev_int <- c(2, 1, 2, 0, 1, 1, 0, 1)
  ev_cmp <- c(4, 1, 4, 3, 0, 1, 1, 0)
  # Expected events = pooled rate x arm person-years. PP, window 1:
  # 6 / 312 x 104 = 2 and 6 / 312 x 208 = 4. PP, window 2: 3 / 208 x 52 =
  # 0.75 and 3 / 208 x 156 = 2.25. PP, window 3: 1 / 130 x 26 = 0.2 and
  # 1 / 130 x 104 = 0.8. ITT, window 1: 1 and 1. ITT, window 2: 0.5 and 0.5.
  # ITT, window 3: 1 / 78 x 26 = 1/3 and 1 / 78 x 52 = 2/3.
  e_int <- c(2, 1, 2, 0.75, 0.2, 1, 0.5, 1 / 3)
  e_cmp <- c(4, 1, 4, 2.25, 0.8, 1, 0.5, 2 / 3)
  rate <- c(
    6 / 312,
    2 / 104,
    6 / 312,
    3 / 208,
    1 / 130,
    2 / 104,
    1 / 104,
    1 / 78
  ) *
    52.25 *
    1e5

  expect_equal(d$`PY (int)`, py_int, tolerance = 1e-10)
  expect_equal(d$`PY (cmp)`, py_cmp, tolerance = 1e-10)
  expect_equal(d$`Events (int)`, ev_int, tolerance = 1e-10)
  expect_equal(d$`Events (cmp)`, ev_cmp, tolerance = 1e-10)
  expect_equal(d$`Pooled rate per 100,000 PY`, rate, tolerance = 1e-10)
  expect_equal(d$`Expected events (int)`, e_int, tolerance = 1e-10)
  expect_equal(d$`Expected events (cmp)`, e_cmp, tolerance = 1e-10)

  # Threshold 2, both arms: E = 2 passes. ITT window 1 fails on both arms, so
  # its reportable horizon is 0. PP of ETT00005 fails in window 2.
  expect_identical(
    d$`Window passes`,
    c("yes", "no", "yes", "no", "no", "no", "no", "no")
  )
  expect_equal(
    d$`Reportable horizon (weeks)`,
    c(52, 0, 52, 52, 52, 0, 0, 0)
  )
})

test_that("the Minimum events sheet agrees with the PP results sheet decision", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  plan <- .hs_plan()
  path <- .hs_export(plan)
  e <- plan$get_estimates()
  pp5 <- e[ett_id == "ETT00005" & estimand == "pp" & weights == "truncated"]
  expect_identical(
    pp5$horizon_status,
    "below minimum-events rule: intervention arm, weeks 52-104"
  )
  expect_true(is.na(pp5$irr))
  itt1 <- e[ett_id == "ETT00001" & estimand == "itt"]
  expect_identical(
    itt1$horizon_status,
    "below minimum-events rule: both arms, weeks 0-52"
  )
  pp1 <- e[ett_id == "ETT00001" & estimand == "pp" & weights == "truncated"]
  expect_identical(pp1$horizon_status, swereg:::.HORIZON_STATUS_PASS)
  expect_true(is.finite(pp1$irr))

  toc <- openxlsx::read.xlsx(path, sheet = "Provenance", colNames = FALSE)
  expect_true("Minimum events" %in% unlist(toc))
})

test_that("a plan with no stored window table gets the sheet with one line", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")
  path <- .hs_export(.xp_plan("new"))
  expect_true("Minimum events" %in% openxlsx::getSheetNames(path))
  m <- openxlsx::read.xlsx(
    path,
    sheet = "Minimum events",
    colNames = FALSE,
    skipEmptyRows = FALSE
  )
  expect_identical(
    m[4, 1],
    paste0(
      "No stored result holds a window table of the minimum-events rule. ",
      "Run $s3_analyze()."
    )
  )
})
