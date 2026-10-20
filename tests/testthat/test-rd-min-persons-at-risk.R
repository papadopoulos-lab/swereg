# A risk difference is withheld where an arm has too few people at risk.
#
# At a stop time where either arm has fewer than `min_persons_at_risk` people
# at risk, `.tte_rd_curve()` stores no risk difference: `rd`, `rd_lo`, `rd_hi`
# and the number-needed-to-treat columns are `NA`, and `interval_status` reads
# "too few at risk at horizon". Every renderer then prints "not estimable"
# with the reason.
#
# Fixture, 20 people in each arm, one person-trial each:
#
#   comparator   18 followed to week 10 without an event, 2 events at week 8
#   intervention 11 censored at week 4, 1 event at week 6, 8 followed to 10
#
# People at risk at the stop times 4, 6, 8 and 10:
#
#   comparator   20, 20, 20, 18
#   intervention 20,  9,  8,  8
#
# At k = 10 only week 4 keeps its risk difference. At k = 9 week 6 keeps it
# too, because the rule is "fewer than k". At week 6 the comparator has no
# event yet, so "zero-event arm" also applies there, and the sparse risk set
# wins. With k = 0, S_comparator(10) = 1 - 2/20 = 0.9 and
# S_intervention(10) = 1 - 1/9 = 8/9, so RD(10) = 0.9 - 8/9 = 1/90.

skip_if_not_installed("data.table")

.rmp_data <- function() {
  data.table::data.table(
    id = c(paste0("c", 1:20), paste0("i", 1:20)),
    ptid = c(paste0("c", 1:20, "_t"), paste0("i", 1:20, "_t")),
    exposed = rep(c(FALSE, TRUE), each = 20L),
    tstart = 0L,
    tstop = c(rep(10L, 18L), 8L, 8L, rep(4L, 11L), 6L, rep(10L, 8L)),
    event = c(rep(0L, 18L), 1L, 1L, rep(0L, 11L), 1L, rep(0L, 8L)),
    w = 1
  )
}

.rmp_curve <- function(...) {
  set.seed(3L)
  return(swereg:::.tte_rd_curve(
    data = .rmp_data(),
    person_id_var = "id",
    id_var = "ptid",
    treatment_var = "exposed",
    time_var = "tstop",
    weight_col = "w",
    n_boot = 50L,
    ...
  ))
}

few <- "too few at risk at horizon"

test_that("the head counts of the fixture are the ones the comment states", {
  out <- .rmp_curve(min_persons_at_risk = 0L)
  expect_identical(out$tstop, c(4L, 6L, 8L, 10L))
  expect_identical(as.integer(out$n_persons_at_risk_comparator), c(20L, 20L, 20L, 18L))
  expect_identical(as.integer(out$n_persons_at_risk_intervention), c(20L, 9L, 8L, 8L))
  expect_equal(out$rd[4], 0.9 - 8 / 9, tolerance = 1e-12)
  expect_false(any(out$interval_status == few))
})

test_that("a sparse risk set withholds the whole risk difference", {
  out <- .rmp_curve(min_persons_at_risk = 10L)
  withheld <- out$tstop %in% c(6L, 8L, 10L)
  expect_identical(out$interval_status[withheld], rep(few, 3L))
  for (col in c("rd", "rd_lo", "rd_hi", "nnt", "nnt_lo", "nnt_hi")) {
    expect_true(all(is.na(out[[col]][withheld])), label = col)
  }
  expect_true(all(is.na(out$nnt_direction[withheld])))
  # Week 4 keeps its point estimate. No arm has an event there yet.
  expect_identical(out$rd[1], 0)
  expect_identical(out$interval_status[1], "zero-event arm")
  # The survival columns stay.
  expect_equal(out$surv_comparator[4], 0.9, tolerance = 1e-12)
  expect_equal(out$surv_intervention[4], 8 / 9, tolerance = 1e-12)
  expect_identical(attr(out, "min_persons_at_risk"), 10L)
})

test_that("the rule is fewer than k, and it wins over zero-event arm", {
  out <- .rmp_curve(min_persons_at_risk = 9L)
  # Week 6: 9 people at risk in the intervention arm is not fewer than 9.
  expect_identical(out$interval_status[2], "zero-event arm")
  expect_true(is.finite(out$rd[2]))
  expect_identical(out$interval_status[3:4], rep(few, 2L))
  # At k = 10 week 6 is both sparse and zero-event, and it reads sparse.
  expect_identical(.rmp_curve(min_persons_at_risk = 10L)$interval_status[2], few)
})

test_that("the default threshold is 10 at every entry point", {
  expect_identical(
    .rmp_curve()$interval_status,
    .rmp_curve(min_persons_at_risk = 10L)$interval_status
  )
  expect_identical(
    formals(swereg::TTEEnrollment$public_methods$risk_difference)$min_persons_at_risk,
    10L
  )
  expect_identical(
    formals(swereg::TTEPlan$public_methods$s3_analyze)$rd_min_persons_at_risk,
    10L
  )
})

test_that("min_persons_at_risk must be one whole number of at least 0", {
  for (bad in list(-1, 2.5, NA_real_, c(1, 2), "10")) {
    expect_error(
      .rmp_curve(min_persons_at_risk = bad),
      "min_persons_at_risk must be a single whole number of at least 0"
    )
  }
})

test_that("the stored row carries the rule and the threshold", {
  out <- .rmp_curve(min_persons_at_risk = 10L)
  res <- swereg:::.s3_rd_result("rd_pp_trunc", out, "ETT00001", "tstop")
  row <- res$rd_pp_trunc
  expect_identical(row$interval_status, few)
  expect_true(is.na(row$rd))
  expect_identical(row$min_persons_at_risk, 10)
})

# --- renderers ---------------------------------------------------------------

.rmp_lookup <- function() {
  return(data.table::data.table(
    ett_id = c("E1", "E2"),
    rd = c(NA_real_, -0.001),
    rd_lo = c(NA_real_, -0.002),
    rd_hi = c(NA_real_, -0.0005),
    nnt = c(NA_real_, 1000),
    nnt_direction = c(NA_character_, "benefit"),
    n_persons_with_event_intervention = c(1, 5),
    n_persons_with_event_comparator = c(4, 9),
    interval_status = c(few, "ok"),
    min_persons_at_risk = c(10, 10)
  ))
}

test_that("the forest cells print not estimable with the reason", {
  cells <- swereg:::.forest_rd_map(c("E1", "E2"), .rmp_lookup())
  expect_identical(cells$txt_rd[1], "not estimable (< 10 at risk)")
  expect_identical(cells$txt_nnt[1], "not estimable")
  # The estimable row is untouched.
  expect_identical(cells$txt_rd[2], "-10.00 (-20.00 to -5.00)")
})

test_that("the results-sheet cells carry the reason and the withheld flag", {
  row <- .rmp_lookup()[1]
  data.table::setnames(
    row,
    c("n_persons_with_event_intervention", "n_persons_with_event_comparator"),
    c("persons_event_int", "persons_event_cmp")
  )
  cells <- swereg:::.rd_sheet_cells(row)
  expect_true(isTRUE(attr(cells, "withheld")))
  expect_true(is.na(cells[[3]]))
  expect_identical(
    cells[[4]],
    "not estimable: fewer than 10 persons at risk in an arm at horizon"
  )
  # A sheet whose only risk difference is withheld still carries the columns.
  expect_true(swereg:::.results_has_rd(list(cells)))
})

test_that("the PP results and Summary sheets print not estimable", {
  skip_if_not_installed("openxlsx")
  plan <- .xp_plan("new")
  row <- plan$results_ett[["ETT00001"]]$rd_pp_trunc
  for (col in c("rd", "rd_lo", "rd_hi", "nnt", "nnt_lo", "nnt_hi")) {
    data.table::set(row, j = col, value = NA_real_)
  }
  data.table::set(row, j = "nnt_direction", value = NA_character_)
  data.table::set(row, j = "interval_status", value = few)
  data.table::set(row, j = "min_persons_at_risk", value = 10)
  plan$results_ett[["ETT00001"]]$rd_pp_trunc <- row

  wb <- openxlsx::createWorkbook()
  swereg:::.write_results_single(
    wb,
    "PP results",
    plan,
    "rates_pp_trunc",
    "irr_pp_trunc",
    "rd_pp_trunc"
  )
  path <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, path)
  sheet <- openxlsx::read.xlsx(path, colNames = FALSE, skipEmptyRows = FALSE)
  header <- unlist(sheet[1, ])
  r1 <- which(sheet[[1]] == swereg:::.enrollment_label(plan, plan$ett$enrollment_id[1]))[1]
  rd_col <- which(header == "Risk difference per 10,000")
  ci_col <- grep("^Risk difference .*% CI$", header)
  expect_identical(sheet[r1, rd_col], "not estimable")
  expect_identical(
    sheet[r1, ci_col],
    "not estimable: fewer than 10 persons at risk in an arm at horizon"
  )

  cells <- swereg:::.summary_estimand_cells(
    plan$get_estimates(),
    plan$ett$ett_id,
    c(irr = "irr_pp_trunc", rd = "rd_pp_trunc")
  )
  expect_identical(
    cells$rd[1],
    "not estimable: fewer than 10 persons at risk in an arm at horizon"
  )
})

# The fixture plan stores risk differences from before the rule: none of them
# records a threshold.
.rmp_set_k <- function(plan, k, ett_ids = names(plan$results_ett)) {
  for (id in ett_ids) {
    for (slot in c("rd_pp_trunc", "rd_itt")) {
      row <- plan$results_ett[[id]][[slot]]
      if (data.table::is.data.table(row)) {
        data.table::set(row, j = "min_persons_at_risk", value = k)
        plan$results_ett[[id]][[slot]] <- row
      }
    }
  }
  return(plan)
}

.rmp_prose <- function(plan) {
  ctx <- swereg:::.protocol_context(plan, "ETT00001")
  tab <- paste(unlist(swereg:::.build_protocol_table(plan$spec, ctx)), collapse = " ")
  txt <- paste(
    utils::capture.output(suppressMessages(plan$print_target_checklist())),
    collapse = " "
  )
  return(list(protocol = tab, target = gsub("\\s+", " ", txt)))
}

test_that("the methods prose omits the rule when no stored result applied it", {
  plan <- .xp_plan("new")
  expect_identical(swereg:::.plan_rd_rule_text(plan), "")
  p <- .rmp_prose(plan)
  expect_false(grepl("persons at risk", p$protocol, fixed = TRUE))
  expect_false(grepl("No risk difference was reported", p$target, fixed = TRUE))
  expect_false(grepl("computed with this rule", p$target, fixed = TRUE))
})

test_that("the methods prose states the threshold every stored result used", {
  p <- .rmp_prose(.rmp_set_k(.xp_plan("new"), 20))
  # The protocol sheet is a preregistration exhibit. No s3 result reaches it,
  # so it never states the rule.
  expect_false(grepl("persons at risk", p$protocol, fixed = TRUE))
  expect_match(
    p$target,
    "No risk difference was reported at a time at which either arm had fewer than 20 persons at risk.",
    fixed = TRUE
  )
})

test_that("the methods prose limits the rule to the results computed with it", {
  plan <- .rmp_set_k(.xp_plan("new"), 20, "ETT00001")
  p <- .rmp_prose(plan)
  expect_false(grepl("persons at risk", p$protocol, fixed = TRUE))
  expect_match(
    p$target,
    "For the results computed with this rule, no risk difference was reported at a time at which either arm had fewer than 20 persons at risk.",
    fixed = TRUE
  )
})

test_that("a withheld risk difference keeps its row when the ratio is missing", {
  skip_if_not_installed("openxlsx")
  plan <- .xp_plan("new")
  row <- plan$results_ett[["ETT00001"]]$rd_pp_trunc
  for (col in c("rd", "rd_lo", "rd_hi", "nnt", "nnt_lo", "nnt_hi")) {
    data.table::set(row, j = col, value = NA_real_)
  }
  data.table::set(row, j = "nnt_direction", value = NA_character_)
  data.table::set(row, j = "interval_status", value = few)
  data.table::set(row, j = "min_persons_at_risk", value = 10)
  plan$results_ett[["ETT00001"]]$rd_pp_trunc <- row
  plan$results_ett[["ETT00001"]]$irr_pp_trunc <- NULL
  est <- plan$get_estimates()
  expect_false(isTRUE(est[ett_id == "ETT00001" & estimand == "pp" &
    weights == "truncated"]$irr_stored))

  wb <- openxlsx::createWorkbook()
  swereg:::.write_results_single(
    wb,
    "PP results",
    plan,
    "rates_pp_trunc",
    "irr_pp_trunc",
    "rd_pp_trunc"
  )
  path <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, path)
  sheet <- openxlsx::read.xlsx(path, colNames = FALSE, skipEmptyRows = FALSE)
  header <- unlist(sheet[1, ])
  r1 <- which(sheet[[4]] == plan$ett$outcome_name[1] &
    sheet[[5]] == as.character(plan$ett$follow_up[1]))[1]
  expect_false(is.na(r1))
  expect_identical(sheet[r1, which(header == "Risk difference per 10,000")], "not estimable")
  expect_true(is.na(sheet[r1, which(header == "IRR")]))

  cells <- swereg:::.summary_estimand_cells(
    est,
    plan$ett$ett_id,
    c(irr = "irr_pp_trunc", rd = "rd_pp_trunc")
  )
  expect_identical(
    cells$rd[1],
    "not estimable: fewer than 10 persons at risk in an arm at horizon"
  )
  expect_true(is.na(cells$irr_ci[1]))
})
