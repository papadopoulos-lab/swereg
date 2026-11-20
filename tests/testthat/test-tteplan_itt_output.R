# Plan-layer ITT output (increment C): the "PP vs ITT" sheet reuses the
# combined-sensitivity writer with parameterised panel labels and the
# irr_itt / rates_itt slots. Pins that the labels propagate.

skip_if_not_installed("openxlsx")
skip_if_not_installed("data.table")

test_that(".write_combined_sensitivity honors left_label/right_label with ITT slots", {
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
  rates_dt <- data.table::data.table(
    rd_tx = c(TRUE, FALSE),
    events_weighted = c(10, 20),
    py_weighted = c(1000, 2000),
    rate_per_100000py = c(1000, 1000)
  )
  data.table::setattr(rates_dt, "treatment_var", "rd_tx")
  irr_val <- list(
    IRR = 1.0,
    IRR_lower = 0.5,
    IRR_upper = 2.0,
    IRR_pvalue = 0.5,
    skipped = FALSE
  )
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
    wb,
    "PP vs ITT",
    plan,
    trunc_rates_slot = "rates_pp_trunc",
    trunc_irr_slot = "irr_pp_trunc",
    untrunc_rates_slot = "rates_itt",
    untrunc_irr_slot = "irr_itt",
    title = "PP vs ITT",
    left_label = "Per-protocol",
    right_label = "Intention-to-treat"
  )

  out <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)
  openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
  flat <- unlist(
    openxlsx::read.xlsx(
      out,
      sheet = "PP vs ITT",
      colNames = FALSE,
      skipEmptyRows = FALSE
    ),
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
    enrollment_id = "01",
    ett_id = "ETT00001",
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    follow_up = 52L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = "analysis_001.qs2",
    description = "ETT00001"
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
  irr_val <- list(
    IRR = 1.0,
    IRR_lower = 0.5,
    IRR_upper = 2.0,
    IRR_pvalue = 0.5,
    skipped = FALSE
  )
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
    wb,
    "Full results",
    plan,
    trunc_rates_slot = "rates_pp_trunc",
    trunc_irr_slot = "irr_pp_trunc",
    untrunc_rates_slot = "rates_pp",
    untrunc_irr_slot = "irr_pp"
  )
  out <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)
  openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
  flat <- unlist(
    openxlsx::read.xlsx(
      out,
      sheet = "Full results",
      colNames = FALSE,
      skipEmptyRows = FALSE
    ),
    use.names = FALSE
  )
  expect_true("Truncated weights" %in% flat)
  expect_true("Untruncated weights" %in% flat)
})

test_that(".write_effect_modification writes stratum IRRs + interaction test", {
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
  sub_tab <- function() {
    data.table::data.table(
      level = c("all", "0", "1"),
      IRR = c(2, 2, 4),
      IRR_lower = c(1.5, 1.5, 3),
      IRR_upper = c(2.5, 2.5, 5),
      IRR_pvalue = c(0.01, 0.02, 0.001),
      warn = FALSE
    )
  }
  emt <- list(p_value = 0.01, ratio_of_irrs = 2.0, n_levels = 2L)
  pass <- list(withheld = FALSE, exempt = FALSE, status = "all windows pass")
  hsub <- list(`0` = pass, `1` = pass)
  plan$results_ett <- list(
    ETT00001 = list(
      enrollment_id = "01",
      description = "ETT00001",
      subgroup_Z_pp = sub_tab(),
      subgroup_Z_itt = sub_tab(),
      emtest_Z_pp = emt,
      emtest_Z_itt = emt,
      # The per-level records of the minimum-events rule that s3 stores.
      # Every level passes.
      hsub_Z_pp = hsub,
      hsub_Z_itt = hsub
    )
  )

  wb <- openxlsx::createWorkbook()
  swereg:::.write_effect_modification(
    wb,
    "Effect modification",
    plan,
    title = "EM"
  )
  out <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)
  openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
  flat <- unlist(
    openxlsx::read.xlsx(
      out,
      sheet = "Effect modification",
      colNames = FALSE,
      skipEmptyRows = FALSE
    ),
    use.names = FALSE
  )

  expect_true("PP IRR" %in% flat)
  expect_true("ITT IRR" %in% flat)
  expect_true("Z" %in% flat) # subgroup variable
  expect_true(all(c("all", "0", "1") %in% flat)) # stratum levels
})

# ---- the Weight truncation (ITT) sheet of `$export_tables()` -----------------
# `.xp_plan("new", itt_untrunc = TRUE)` of helper-export_parity.R stores both
# intention-to-treat weightings, with values that differ. The tests below
# write the workbook from that plan and read the sheet back.

# One sheet as a character matrix, every row and column kept.
.ittx_cells <- function(xlsx, sheet) {
  m <- openxlsx::read.xlsx(
    xlsx,
    sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  m[] <- lapply(m, as.character)
  return(as.matrix(m))
}

.ittx_export <- function(plan, dir) {
  xlsx <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
  return(xlsx)
}

# The header row and the body rows of the ITT sheet. A sheet with no body
# has no header row, and gives a body of 0 rows.
.ittx_body <- function(xlsx) {
  m <- .ittx_cells(xlsx, "Weight truncation (ITT)")
  h <- which(m[, 1L] == "Enrollment")[1L]
  body <- if (is.na(h)) m[0L, , drop = FALSE] else m[-seq_len(h), , drop = FALSE]
  return(list(m = m, h = h, body = body))
}

# The 29 cells of the row of one ETT, found by its enrollment label and its
# outcome, which are unique per ETT in this fixture. A missing row gives 29
# `NA` cells, so the assertions on it fail rather than stop.
.ittx_row <- function(body, plan, eid) {
  if (nrow(body) == 0L || ncol(body) < 29L) {
    return(rep(NA_character_, 29L))
  }
  k <- match(eid, plan$ett$ett_id)
  enr <- swereg:::.enrollment_label(plan, plan$ett$enrollment_id[[k]])
  hit <- which(body[, 1L] == enr & body[, 4L] == plan$ett$outcome_name[[k]])
  if (length(hit) != 1L) {
    return(rep(NA_character_, 29L))
  }
  return(unname(body[hit, ]))
}

# The ETTs of the fixture that the sheet reports. ETT00004 has no usable
# intention-to-treat block on either weight, and ETT00006 has no result.
.ITTX_ROWS <- c(
  "ETT00001",
  "ETT00002",
  "ETT00003",
  "ETT00005",
  "ETT00007",
  "ETT00008"
)

# The 12 cells one `$get_estimates()` row MUST give, in sheet order. An empty
# number-needed-to-treat cell reads back as NA.
.ittx_want <- function(x) {
  nnt_cell <- function(x) {
    nnt <- swereg:::.tte_nntb_cell(
      x$nnt,
      x$nnt_lo,
      x$nnt_hi,
      nnt_direction = x$nnt_direction
    )
    return(if (nzchar(nnt)) nnt else NA_character_)
  }
  rd_cells <- if (isTRUE(x$rd_stored)) {
    list(
      x$rd * 10000,
      sprintf("%+.2f to %+.2f", x$rd_lo * 10000, x$rd_hi * 10000),
      nnt_cell(x)
    )
  } else {
    list("not computed", "not computed", "not computed")
  }
  return(c(
    list(x$events_int, x$py_int, x$rate_int, x$events_cmp, x$py_cmp, x$rate_cmp),
    list(
      swereg:::.ff_irr_num(x$irr),
      paste(swereg:::.ff_irr_num(x$irr_lo), "to", swereg:::.ff_irr_num(x$irr_hi)),
      x$irr_pvalue
    ),
    rd_cells
  ))
}

# Compare the 12 cells of one block with one `$get_estimates()` row.
.ittx_expect_block <- function(got, x, lab) {
  want <- .ittx_want(x)
  for (j in seq_along(want)) {
    w <- want[[j]]
    if (is.numeric(w)) {
      expect_equal(
        as.numeric(got[[j]]),
        w,
        tolerance = 1e-10,
        info = paste(lab, "cell", j)
      )
    } else {
      expect_identical(unname(got[[j]]), w, info = paste(lab, "cell", j))
    }
  }
}

test_that("export_tables writes the Weight truncation (ITT) sheet after the PP one", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  dir <- withr::local_tempdir()
  xlsx <- .ittx_export(.xp_plan("new", itt_untrunc = TRUE), dir)
  sheets <- openxlsx::getSheetNames(xlsx)
  # The sheet-exists assertion.
  expect_true("Weight truncation (ITT)" %in% sheets)
  expect_identical(
    match("Weight truncation (ITT)", sheets),
    match("Weight truncation (PP)", sheets) + 1L
  )
  # The table of contents on the Provenance sheet names it once.
  toc <- .ittx_cells(xlsx, "Provenance")
  expect_identical(sum(toc == "Weight truncation (ITT)", na.rm = TRUE), 1L)
})

test_that("the Weight truncation (ITT) cells equal $get_estimates() on both weights", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  dir <- withr::local_tempdir()
  plan <- .xp_plan("new", itt_untrunc = TRUE)
  xlsx <- .ittx_export(plan, dir)
  s <- .ittx_body(xlsx)
  expect_identical(ncol(s$m), 29L)
  expect_identical(
    unname(s$m[s$h - 1L, c(6L, 18L)]),
    c("Truncated weights", "Untruncated weights")
  )
  body <- s$body
  expect_identical(nrow(body), length(.ITTX_ROWS))

  e <- plan$get_estimates()
  # The four ETTs with an estimable ratio on both weights. ETT00001 and
  # ETT00003 also store a risk difference on both weights.
  for (eid in c("ETT00001", "ETT00002", "ETT00003", "ETT00005")) {
    row <- .ittx_row(body, plan, eid)
    t <- e[ett_id == eid & estimand == "itt" & weights == "truncated"]
    u <- e[ett_id == eid & estimand == "itt" & weights == "untruncated"]
    expect_identical(nrow(t), 1L, info = eid)
    expect_identical(nrow(u), 1L, info = eid)
    # The truncated side.
    .ittx_expect_block(row[6:17], t, paste(eid, "truncated"))
    # The untruncated side, the cell-equality assertion of mutation (2).
    .ittx_expect_block(row[18:29], u, paste(eid, "untruncated"))
    # The fixture gives the two weights different values, so a sheet that
    # read the truncated slots on both sides cannot pass the line above.
    for (nm in c("events_int", "py_int", "rate_int", "irr", "irr_pvalue")) {
      expect_false(isTRUE(all.equal(t[[nm]], u[[nm]])), info = paste(eid, nm))
    }
  }
  expect_true(all(e[ett_id %in% c("ETT00001", "ETT00003") & estimand == "itt"]$rd_stored))
})

test_that("a result saved before 27.3.16 keeps its ITT row, and its untruncated cells read not computed", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  dir_new <- file.path(withr::local_tempdir(), "new")
  dir_old <- file.path(withr::local_tempdir(), "old")
  dir.create(dir_new)
  dir.create(dir_old)
  full <- .ittx_body(.ittx_export(.xp_plan("new", itt_untrunc = TRUE), dir_new))
  # A result of swereg 27.3.15 or earlier stores no untruncated
  # intention-to-treat slot.
  plan <- .xp_plan("new", itt_untrunc = TRUE)
  plan$results_ett <- lapply(plan$results_ett, function(r) {
    r[grepl("_itt_untrunc$", names(r))] <- NULL
    return(r)
  })
  e <- plan$get_estimates()
  expect_identical(nrow(e[estimand == "itt" & weights == "untruncated"]), 0L)

  old <- .ittx_body(.ittx_export(plan, dir_old))
  expect_identical(nrow(old$body), length(.ITTX_ROWS))
  for (eid in .ITTX_ROWS) {
    row <- .ittx_row(old$body, plan, eid)
    # The "not computed" assertion of mutation (3): the row stays, and all
    # 12 untruncated cells read "not computed".
    expect_identical(row[18:29], rep("not computed", 12L), info = eid)
    # The identifiers and the truncated side are those of the full export.
    expect_identical(row[1:17], .ittx_row(full$body, plan, eid)[1:17], info = eid)
  }
  t <- e[ett_id == "ETT00001" & estimand == "itt" & weights == "truncated"]
  .ittx_expect_block(
    .ittx_row(old$body, plan, "ETT00001")[6:17],
    t,
    "ETT00001 truncated, old result"
  )
})
