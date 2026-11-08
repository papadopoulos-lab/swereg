# Protocol adherence counts: `.tte_adherence_counts()`, the s3 enrollment
# worker that stores them, `$get_adherence()` and the two refresh routes.
#
# The counts come from the source panel, the raw file `.s1e_worker()` writes
# before any outcome. A person-trial is observed at week t when
# `t <= min(r, g, a)`. It is on protocol when it is also `t <= d` or `d` is NA.
# The helpers live in `helper-adherence.R`.

skip_if_not_installed("data.table")
skip_if_not_installed("cstime")
skip_if_not_installed("qs2")
skip_if_not_installed("withr")


# --- (1) exact counts on a hand-built panel ------------------------------------

test_that("adherence counts of a hand-built panel match at every stored week", {
  trials <- .adh_test1_trials()
  enr <- .adh_enrollment(.adh_panel(trials), .adh_design(admin = "2020-30"))
  out <- swereg:::.tte_adherence_counts(enr)

  expect_identical(
    names(out),
    c("arm", "week", "n_observed", "n_on_protocol")
  )
  expect_identical(out$arm, rep(c("intervention", "comparator"), each = 24L))
  expect_identical(out$week, rep(1:24, 2L))
  expect_type(out$n_observed, "integer")
  expect_type(out$n_on_protocol, "integer")

  # Derived by hand from the boundaries in `.adh_test1_trials()`. The NA
  # treatment trial is in no arm.
  # Intervention observed: 3 always, plus I4 to 8, I5 to 14, I6 to 6, I7 to 16.
  int_obs <- c(rep(7L, 6), rep(6L, 2), rep(5L, 6), rep(4L, 2), rep(3L, 8))
  # Intervention on protocol: I1 always, I3 to 10, I4 to 8, I5 to 14, I6 to 6,
  # I7 to 16. I2 deviates at time zero and is never on protocol.
  int_pp <- c(
    rep(6L, 6),
    rep(5L, 2),
    rep(4L, 2),
    rep(3L, 4),
    rep(2L, 2),
    rep(1L, 8)
  )
  # Comparator observed: 3 always, plus C4 to 3, C5 to 1, C6 to 22, C7 to 16.
  cmp_obs <- c(7L, rep(6L, 2), rep(5L, 13), rep(4L, 6), rep(3L, 2))
  # Comparator on protocol: C1 always, C3 to 20, C4 to 3, C5 to 1, C6 to 22,
  # C7 to 16. C2 deviates at time zero.
  cmp_pp <- c(
    6L,
    rep(5L, 2),
    rep(4L, 13),
    rep(3L, 4),
    rep(2L, 2),
    rep(1L, 2)
  )

  int <- out[arm == "intervention"]
  cmp <- out[arm == "comparator"]
  expect_identical(int$n_observed, int_obs)
  expect_identical(int$n_on_protocol, int_pp)
  expect_identical(cmp$n_observed, cmp_obs)
  expect_identical(cmp$n_on_protocol, cmp_pp)
})


# --- (2) equality with the panels s5_prepare_outcome() builds -------------------

test_that("adherence counts equal the risk sets of the s5_prepare_outcome() panels", {
  trials <- .adh_test2_trials()
  panel <- .adh_panel(trials, last_week = 48L)
  design <- .adh_design(admin = "2020-40", follow_up = 48L)
  out <- swereg:::.tte_adherence_counts(.adh_enrollment(panel, design))

  # The fixture states the case. The administrative end binds for at least
  # one person-trial, and one person holds two overlapping trials.
  a <- as.integer(difftime(
    cstime::isoyearweek_to_last_date("2020-40"),
    cstime::isoyearweek_to_last_date(trials$baseline),
    units = "weeks"
  )) +
    1L
  bound <- pmin(trials$r, trials$g, na.rm = TRUE)
  expect_true(any(a < 48L & (is.na(bound) | a < bound)))
  expect_identical(sum(trials$id == "p01"), 2L)

  build <- function(estimand) {
    enr <- .adh_enrollment(panel, design)
    enr$.__enclos_env__$private$s5_prepare_outcome(
      outcome = "death",
      estimand = estimand
    )
    return(enr$data)
  }
  itt <- build("itt")
  pp <- build("pp")

  # `s5_prepare_outcome()` writes `weeks_to_admin_end` with its own formula.
  # The helper reproduces it, and here the two agree for every person-trial.
  admin_s5 <- unique(itt[, list(trial_id, weeks_to_admin_end)])
  data.table::setorder(admin_s5, trial_id)
  expect_identical(admin_s5$weeks_to_admin_end, a[order(trials$trial_id)])

  # Unweighted person-trial rows with `tstart < t <= tstop`, at every week.
  rows_at <- function(d, t, arm) {
    return(sum(d$tx == arm & d$tstart < t & t <= d$tstop))
  }
  weeks <- 1:48
  for (arm_value in c(TRUE, FALSE)) {
    label <- if (arm_value) "intervention" else "comparator"
    got <- out[arm == label]
    expect_identical(got$week, weeks)
    expect_identical(
      got$n_observed,
      vapply(weeks, function(t) rows_at(itt, t, arm_value), integer(1))
    )
    expect_identical(
      got$n_on_protocol,
      vapply(weeks, function(t) rows_at(pp, t, arm_value), integer(1))
    )
  }
})


# --- (3) not computed -----------------------------------------------------------

test_that("the worker returns adherence = NULL, with the name, when a column is missing", {
  dir <- withr::local_tempdir()
  trials <- .adh_test1_trials()
  panel <- .adh_panel(trials)
  analysis_path <- file.path(dir, "analysis.qs2")
  qs2::qs_save(
    .adh_enrollment(panel, .adh_design(admin = "2020-30")),
    analysis_path
  )

  run <- function(raw_panel, admin, raw_name) {
    raw_path <- file.path(dir, raw_name)
    if (!is.null(raw_panel)) {
      qs2::qs_save(.adh_enrollment(raw_panel, .adh_design(admin)), raw_path)
    }
    return(swereg:::.s3_enrollment_worker(
      analysis_path = analysis_path,
      raw_path = raw_path,
      enrollment_id = "01",
      n_threads = 1L
    ))
  }

  for (col in c(
    "weeks_to_protocol_deviation",
    "weeks_to_record_end",
    "weeks_to_observation_gap"
  )) {
    p <- data.table::copy(panel)
    p[, (col) := NULL]
    res <- run(p, NULL, paste0("raw_no_", col, ".qs2"))
    expect_true("adherence" %in% names(res), info = col)
    expect_null(res$adherence)
  }

  # `isoyearweek` is needed only when the administrative end is set.
  p <- data.table::copy(panel)
  p[, isoyearweek := NULL]
  res <- run(p, "2020-30", "raw_no_isoyearweek.qs2")
  expect_true("adherence" %in% names(res))
  expect_null(res$adherence)
  res <- run(p, NULL, "raw_no_isoyearweek_no_admin.qs2")
  expect_s3_class(res$adherence, "data.table")

  # No raw file at all: the name is still there.
  res <- run(NULL, NULL, "raw_absent.qs2")
  expect_false(file.exists(file.path(dir, "raw_absent.qs2")))
  expect_true("adherence" %in% names(res))
  expect_null(res$adherence)
})


# --- (4) outcome independence ---------------------------------------------------

test_that("outcome columns do not change the adherence counts", {
  trials <- .adh_test1_trials()
  panel <- .adh_panel(trials)
  design <- .adh_design(admin = "2020-30")
  ref <- swereg:::.tte_adherence_counts(.adh_enrollment(panel, design))

  p <- data.table::copy(panel)
  p[, death := 1L]
  p[, weeks_to_event_death := 2L]
  p[, event := 1L]
  p[, censor_this_period := 1L]
  p[, weeks_to_event := 1L]
  got <- swereg:::.tte_adherence_counts(.adh_enrollment(p, design))
  expect_identical(got, ref)
})


# --- (5) the accessor -----------------------------------------------------------

test_that("get_adherence() returns every stored table, with share", {
  plan <- .xp_plan("new")
  a1 <- swereg:::.tte_adherence_counts(.adh_enrollment(
    .adh_panel(.adh_test1_trials()),
    .adh_design(admin = "2020-30")
  ))
  a2 <- data.table::data.table(
    arm = rep(c("intervention", "comparator"), each = 2L),
    week = rep(1:2, 2L),
    n_observed = c(4L, 0L, 3L, 2L),
    n_on_protocol = c(2L, 0L, 3L, 1L)
  )
  plan$results_enrollment[["01"]]$adherence <- a1
  plan$results_enrollment[["02"]]$adherence <- a2

  out <- plan$get_adherence()
  expect_identical(
    names(out),
    c("enrollment_id", "arm", "week", "n_observed", "n_on_protocol", "share")
  )
  expect_identical(out$enrollment_id, rep(c("01", "02"), c(48L, 4L)))
  expect_identical(out[enrollment_id == "01", -c("enrollment_id", "share")], a1)
  expect_identical(out[enrollment_id == "02", -c("enrollment_id", "share")], a2)
  expect_identical(
    out[enrollment_id == "02"]$share,
    c(0.5, NA_real_, 1, 0.5)
  )
  expect_identical(
    out[enrollment_id == "01"]$share,
    a1$n_on_protocol / a1$n_observed
  )

  # A stored NULL gives no rows.
  plan$results_enrollment[["02"]]["adherence"] <- list(NULL)
  expect_true("adherence" %in% names(plan$results_enrollment[["02"]]))
  out <- plan$get_adherence()
  expect_identical(unique(out$enrollment_id), "01")
})


# --- (6) worker reachability ----------------------------------------------------

test_that("the s3 enrollment worker returns the adherence of its raw file", {
  dir <- withr::local_tempdir()
  enr <- .adh_enrollment(
    .adh_panel(.adh_test1_trials()),
    .adh_design(admin = "2020-30")
  )
  analysis_path <- file.path(dir, "analysis.qs2")
  raw_path <- file.path(dir, "raw.qs2")
  qs2::qs_save(enr, analysis_path)
  qs2::qs_save(enr, raw_path)

  res <- swereg:::.s3_enrollment_worker(
    analysis_path = analysis_path,
    raw_path = raw_path,
    enrollment_id = "01",
    n_threads = 1L
  )
  expect_true("adherence" %in% names(res))
  expect_identical(res$adherence, swereg:::.tte_adherence_counts(enr))
  expect_identical(nrow(res$adherence), 48L)
})


# --- (7) refresh rule 1 ---------------------------------------------------------

test_that("recompute_baselines() refreshes a result without adherence only", {
  plan <- .xp_plan("new")
  dir <- withr::local_tempdir()
  enr <- .adh_enrollment(
    .adh_panel(.adh_test1_trials()),
    .adh_design(admin = "2020-30")
  )
  .adh_write_files(plan, dir, enr)

  # Enrollment 01 holds no `adherence` name. Enrollment 02 holds a stored
  # NULL, which is present. Both have current panels.
  plan$results_enrollment[["01"]][["adherence"]] <- NULL
  plan$results_enrollment[["02"]]["adherence"] <- list(NULL)
  expect_false("adherence" %in% names(plan$results_enrollment[["01"]]))
  expect_true("adherence" %in% names(plan$results_enrollment[["02"]]))
  expect_false(swereg:::.baseline_panel_is_stale(plan$results_enrollment[[
    "01"
  ]]))
  expect_false(swereg:::.baseline_panel_is_stale(plan$results_enrollment[[
    "02"
  ]]))

  msg <- testthat::capture_messages(plan$recompute_baselines(output_dir = dir))
  expect_match(
    msg[[1L]],
    "recomputed 1, skipped 1 already current",
    fixed = TRUE
  )

  expect_identical(
    plan$results_enrollment[["01"]]$adherence,
    swereg:::.tte_adherence_counts(enr)
  )
  expect_true("adherence" %in% names(plan$results_enrollment[["02"]]))
  expect_null(plan$results_enrollment[["02"]]$adherence)
  expect_identical(plan$results_enrollment[["02"]]$n_baseline, 640L)
})


# --- (8) refresh rule 2 ---------------------------------------------------------

test_that("export_tables() refreshes a result without adherence when its raw file exists", {
  skip_if_not_installed("openxlsx")
  dir <- withr::local_tempdir()
  enr <- .adh_enrollment(
    .adh_panel(.adh_test1_trials()),
    .adh_design(admin = "2020-30")
  )

  plan <- .xp_plan("new")
  .adh_write_files(plan, dir, enr)
  plan$results_enrollment[["01"]][["adherence"]] <- NULL
  expect_false("adherence" %in% names(plan$results_enrollment[["01"]]))
  suppressMessages(plan$export_tables(
    path = file.path(dir, "tables.xlsx"),
    output_dir = dir
  ))
  expect_true("adherence" %in% names(plan$results_enrollment[["01"]]))
  expect_identical(
    plan$results_enrollment[["01"]]$adherence,
    swereg:::.tte_adherence_counts(enr)
  )

  # No output directory resolves: no refresh, no error, and no warning about
  # adherence or analysis files.
  plan <- .xp_plan("new")
  plan$results_enrollment[["01"]][["adherence"]] <- NULL
  expect_null(plan$output_dir)
  warns <- character(0)
  withCallingHandlers(
    suppressMessages(plan$export_tables(path = file.path(dir, "nodir.xlsx"))),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("adherence|analysis file", warns, ignore.case = TRUE)))
  expect_true(file.exists(file.path(dir, "nodir.xlsx")))
  expect_false("adherence" %in% names(plan$results_enrollment[["01"]]))

  # The output directory resolves and the raw file is absent: no refresh and
  # no such warning either.
  empty <- withr::local_tempdir()
  warns <- character(0)
  withCallingHandlers(
    suppressMessages(plan$export_tables(
      path = file.path(dir, "noraw.xlsx"),
      output_dir = empty
    )),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("adherence|analysis file", warns, ignore.case = TRUE)))
  expect_false("adherence" %in% names(plan$results_enrollment[["01"]]))
})


# --- (9) the milestone, label and presence helpers ------------------------------

test_that("milestone weeks, their labels and the presence rule", {
  expect_identical(swereg:::.tte_adherence_milestones(12L), integer(0))
  expect_identical(swereg:::.tte_adherence_milestones(24L), 13L)
  expect_identical(swereg:::.tte_adherence_milestones(51L), c(13L, 26L))
  expect_identical(
    swereg:::.tte_adherence_milestones(160L),
    c(13L, 26L, 52L, 104L, 156L)
  )

  expect_identical(
    swereg:::.tte_adherence_milestone_labels(c(13L, 26L, 52L, 104L), NULL),
    c("13 weeks", "26 weeks", "1 year", "2 years")
  )
  spec <- list(follow_up = list(list(label = "12 months", weeks = 52L)))
  expect_identical(
    swereg:::.tte_adherence_milestone_labels(c(13L, 52L, 104L), spec),
    c("13 weeks", "12 months", "2 years")
  )

  expect_true(swereg:::.tte_adherence_missing(list(n_baseline = 1L)))
  expect_false(swereg:::.tte_adherence_missing(list(adherence = NULL)))
})

test_that("no milestone week gives no label", {
  expect_identical(
    swereg:::.tte_adherence_milestone_labels(integer(0), NULL),
    character(0)
  )
  spec <- list(follow_up = list(list(label = "12 months", weeks = 52L)))
  expect_identical(
    swereg:::.tte_adherence_milestone_labels(integer(0), spec),
    character(0)
  )
})

test_that("a NULL stored result is skipped, and a result without adherence is missing it", {
  # swereg 27.3.2 skipped a NULL result. A result without the name
  # `adherence` is still refreshed.
  expect_false(swereg:::.tte_adherence_missing(NULL))
  expect_true(swereg:::.tte_adherence_missing(list(n_baseline = 1L)))
  expect_true(swereg:::.tte_adherence_missing(list()))
})


# =============================================================================
# The Protocol adherence sheet of tables.xlsx and feasibility.xlsx: tests 9 to
# 15 of the contract. The section above numbers its own last test (9); the
# labels below carry no number.
# =============================================================================

ADH_SHEET <- "Protocol adherence"

# Every cell of the adherence sheet of `xlsx`, as text, rows and columns as
# in the workbook.
.adh_sheet_cells <- function(xlsx) {
  m <- openxlsx::read.xlsx(
    xlsx,
    sheet = ADH_SHEET,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  m[] <- lapply(m, as.character)
  return(m)
}

# The sheet data of the adherence sheet: one row per written cell, with its
# worksheet row `r`, column `c`, openxlsx type `t` (0 is a number) and value
# `v` (a number, or the index of a shared string).
.adh_sheet_data <- function(xlsx) {
  wb <- openxlsx::loadWorkbook(xlsx)
  sd <- wb$worksheets[[match(ADH_SHEET, names(wb))]]$sheet_data
  return(data.frame(r = sd$rows, c = sd$cols, t = sd$t, v = sd$v))
}

# The merged ranges of the adherence sheet, as "D4:F4".
.adh_merges <- function(xlsx) {
  wb <- openxlsx::loadWorkbook(xlsx)
  mc <- wb$worksheets[[match(ADH_SHEET, names(wb))]]$mergeCells
  return(sub('^<mergeCell ref="([^"]+)"/>$', "\\1", mc))
}

# The table of contents of the Provenance sheet: the sheet names, in order.
.adh_toc <- function(xlsx) {
  d <- openxlsx::read.xlsx(
    xlsx,
    sheet = "Provenance",
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  d <- d[stats::complete.cases(d[, 5:6]), 5:6]
  return(d[[1]][d[[1]] != "Name"])
}

# An adherence table with `last` weeks. The counts fall by one each week and
# differ between the arms, so N (PP) < N (ITT) and the percentage is not 100.
.adh_table <- function(last, k = 1L) {
  w <- seq_len(last)
  return(data.table::data.table(
    arm = rep(c("intervention", "comparator"), each = last),
    week = rep(w, 2L),
    n_observed = c(last + 100L - w + 1L, 2L * last + 50L - w + 1L) * k,
    n_on_protocol = c(last + 50L - w + 1L, last + 20L - w + 1L) * k
  ))
}

# Export the `.xp_plan()` fixture with the given adherence tables under a new
# directory. Returns the two workbook paths, the directory and the plan.
.adh_export <- function(tables, envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  plan <- .xp_plan("new")
  for (id in names(tables)) {
    plan$results_enrollment[[id]]["adherence"] <- list(tables[[id]])
  }
  path <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = path)))
  return(list(
    dir = dir,
    plan = plan,
    tables = path,
    feasibility = file.path(dir, "feasibility.xlsx")
  ))
}


test_that("the Protocol adherence sheet is identical in tables.xlsx and feasibility.xlsx of one plan", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("survey")
  # The real s1 -> s2 -> s3 run. The raw file of each enrollment comes from
  # `.s1e_worker()`, and the enrollment worker reads it.
  run <- spr_report_pipeline()
  withr::defer(unlink(run$root, recursive = TRUE))
  plan <- run$plan
  adh <- plan$get_adherence()
  expect_identical(sort(unique(adh$enrollment_id)), c("01", "02"))

  # The $s3_feasibility() path: the sheet reads the worker results.
  dev <- swereg:::.swereg_dev_path()
  utils::capture.output(
    plan$s3_feasibility(n_workers = 2L, swereg_dev_path = dev)
  )
  dir <- withr::local_tempdir()
  feas_s3 <- file.path(dir, "feasibility_s3.xlsx")
  expect_true(file.copy(
    file.path(plan$dir_results, "feasibility.xlsx"),
    feas_s3
  ))

  # The $export_tables() path: tables.xlsx and, through
  # `.plan_export_feasibility()`, feasibility.xlsx. Both read
  # `$get_adherence()`.
  tables <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = tables)))
  feas_export <- file.path(dir, "feasibility.xlsx")

  for (xlsx in c(tables, feas_export, feas_s3)) {
    expect_true(ADH_SHEET %in% openxlsx::getSheetNames(xlsx), info = xlsx)
  }
  want <- .adh_sheet_cells(tables)
  # Rows 1 to 5 hold the title, the note and the header. Then one row per
  # enrollment and arm.
  expect_identical(nrow(want), 5L + 2L * 2L)
  expect_identical(.adh_sheet_cells(feas_export), want)
  expect_identical(.adh_sheet_cells(feas_s3), want)
})


test_that("the N (ITT), N (PP) and % cells equal get_adherence() and are numeric", {
  skip_if_not_installed("openxlsx")
  x <- .adh_export(list(`01` = .adh_table(60L), `02` = .adh_table(110L, 2L)))
  plan <- x$plan
  adh <- plan$get_adherence()
  want <- adh[enrollment_id == "02" & arm == "comparator" & week == 52L]
  expect_identical(nrow(want), 1L)
  label <- swereg:::.tte_adherence_milestone_labels(52L, plan$spec)
  arm_label <- swereg:::.lookup_arm_labels(plan$spec, "02")[["comparator"]]

  for (xlsx in c(x$tables, x$feasibility)) {
    m <- .adh_sheet_cells(xlsx)
    col <- which(unlist(m[4L, ]) == label)
    expect_length(col, 1L)
    expect_identical(
      unlist(m[5L, col + 0:2], use.names = FALSE),
      c("N (ITT)", "N (PP)", "%")
    )
    # The comparator row follows the row that names the comparison.
    first <- which(m[[2L]] == swereg:::.enrollment_label(plan, "02"))
    row <- first + 1L
    expect_identical(m[[3L]][row], arm_label)

    sd <- .adh_sheet_data(xlsx)
    cell <- function(j) {
      return(sd[sd$r == row & sd$c == col + j, ])
    }
    for (j in 0:2) {
      expect_identical(cell(j)$t, 0L, info = paste(xlsx, j))
    }
    got <- as.numeric(vapply(0:2, function(j) cell(j)$v, character(1)))
    expect_identical(
      got,
      c(
        as.numeric(want$n_observed),
        as.numeric(want$n_on_protocol),
        round(100 * want$n_on_protocol / want$n_observed)
      )
    )
    # 158 of 438, so the cell holds 36 and not 100 or a fraction.
    expect_identical(got, c(438, 158, 36))
  }
})


test_that("a 260-week panel has the milestones 13, 26, 52, 104, 156, 208 and 260", {
  skip_if_not_installed("openxlsx")
  enr <- .adh_enrollment(
    .adh_panel(.adh_test1_trials(), last_week = 260L),
    .adh_design(follow_up = 260L)
  )
  counts <- swereg:::.tte_adherence_counts(enr)
  expect_identical(max(counts$week), 260L)
  weeks <- swereg:::.tte_adherence_milestones(max(counts$week))
  expect_identical(weeks, c(13L, 26L, 52L, 104L, 156L, 208L, 260L))
  want <- c(
    "13 weeks",
    "26 weeks",
    "1 year",
    "2 years",
    "3 years",
    "4 years",
    "5 years"
  )
  expect_identical(
    swereg:::.tte_adherence_milestone_labels(weeks, NULL),
    want
  )
  spec <- list(follow_up = list(list(label = "24 months", weeks = 104L)))
  expect_identical(
    swereg:::.tte_adherence_milestone_labels(weeks, spec),
    replace(want, 4L, "24 months")
  )

  # The sheet shows the same labels in row 4, one per block of three columns.
  x <- .adh_export(list(`01` = counts, `02` = NULL))
  m <- .adh_sheet_cells(x$tables)
  row4 <- unlist(m[4L, ], use.names = FALSE)
  expect_identical(row4[1:3], c("#", "Comparison", "Arm"))
  expect_identical(row4[3L + 3L * (seq_along(want) - 1L) + 1L], want)
  expect_identical(row4[!is.na(row4)][-(1:3)], want)
})


test_that("a zero N (ITT) gives a blank % cell", {
  skip_if_not_installed("openxlsx")
  tab <- .adh_table(30L)
  tab[
    arm == "comparator" & week == 13L,
    `:=`(
      n_observed = 0L,
      n_on_protocol = 0L
    )
  ]
  x <- .adh_export(list(`01` = tab, `02` = NULL))
  m <- .adh_sheet_cells(x$tables)
  col <- which(unlist(m[4L, ]) == "13 weeks")
  row <- 7L
  expect_identical(m[[3L]][row], "Untreated")
  sd <- .adh_sheet_data(x$tables)
  itt <- sd[sd$r == row & sd$c == col, ]
  pct <- sd[sd$r == row & sd$c == col + 2L, ]
  expect_identical(itt$t, 0L)
  expect_identical(itt$v, "0")
  expect_identical(nrow(pct) == 0L || is.na(pct$v), TRUE)
  expect_identical(m[[col + 2L]][row], NA_character_)
  # The intervention arm of the same milestone keeps its percentage.
  expect_identical(m[[col + 2L]][6L], as.character(round(100 * 68 / 118)))
})


test_that("the milestone labels merge across three columns, and the sheet and its TOC row sit in place", {
  skip_if_not_installed("openxlsx")
  x <- .adh_export(list(`01` = .adh_table(60L), `02` = .adh_table(110L, 2L)))
  for (xlsx in c(x$tables, x$feasibility)) {
    m <- .adh_sheet_cells(xlsx)
    row4 <- unlist(m[4L, ], use.names = FALSE)
    cols <- which(!is.na(row4))[-(1:3)]
    expect_identical(cols, c(4L, 7L, 10L, 13L), info = xlsx)
    want <- paste0(
      openxlsx::int2col(cols),
      "4:",
      openxlsx::int2col(cols + 2L),
      "4"
    )
    expect_identical(.adh_merges(xlsx), want, info = xlsx)
  }

  # feasibility.xlsx: immediately after Attrition.
  sheets <- openxlsx::getSheetNames(x$feasibility)
  expect_identical(match(ADH_SHEET, sheets), match("Attrition", sheets) + 1L)

  # tables.xlsx: immediately after the last sheet of the per-enrollment loop.
  sheets <- openxlsx::getSheetNames(x$tables)
  loop <- grep("^(Attrition_|Love_)", sheets)
  expect_identical(sheets[max(loop)], "Love_02")
  expect_identical(match(ADH_SHEET, sheets), max(loop) + 1L)
  expect_identical(sum(sheets == ADH_SHEET), 1L)

  # One TOC row, in workbook order.
  toc <- .adh_toc(x$tables)
  expect_identical(toc[seq_along(sheets)], sheets)
  expect_identical(sum(toc == ADH_SHEET), 1L)
})


test_that("a plan with no adherence table exports with no Protocol adherence sheet", {
  skip_if_not_installed("openxlsx")
  dir <- withr::local_tempdir()
  plan <- .xp_plan("new")
  # A stored NULL in 01, and no name at all in 02. No output directory
  # resolves, so neither is refreshed.
  plan$results_enrollment[["01"]]["adherence"] <- list(NULL)
  plan$results_enrollment[["02"]][["adherence"]] <- NULL
  expect_null(plan$output_dir)
  expect_identical(nrow(plan$get_adherence()), 0L)
  path <- file.path(dir, "tables.xlsx")
  expect_no_error(suppressMessages(suppressWarnings(
    plan$export_tables(path = path)
  )))
  expect_false(ADH_SHEET %in% openxlsx::getSheetNames(path))
  expect_false(ADH_SHEET %in% .adh_toc(path))
  feas <- file.path(dir, "feasibility.xlsx")
  expect_true(file.exists(feas))
  expect_false(ADH_SHEET %in% openxlsx::getSheetNames(feas))

  # An enrollment without a table is left out when another has one.
  x <- .adh_export(list(`01` = NULL, `02` = .adh_table(30L)))
  m <- .adh_sheet_cells(x$tables)
  expect_identical(nrow(m), 7L)
  expect_identical(m[[1L]][6L], "2")
  expect_identical(m[[2L]][6L], swereg:::.enrollment_label(x$plan, "02"))
})


test_that("no file named for adherence is written beside tables.xlsx", {
  skip_if_not_installed("openxlsx")
  x <- .adh_export(list(`01` = .adh_table(60L), `02` = .adh_table(110L, 2L)))
  files <- list.files(x$dir, recursive = TRUE)
  expect_true(all(c("tables.xlsx", "feasibility.xlsx") %in% files))
  expect_true(ADH_SHEET %in% openxlsx::getSheetNames(x$tables))
  expect_false(any(grepl("adherence", files, ignore.case = TRUE)))
})


test_that("a milestone past the last stored week is blank, and # and Comparison sit on the first row only", {
  skip_if_not_installed("openxlsx")
  # Enrollment 01 stops at week 60 and 02 at week 110, so the union of the
  # milestones reaches 104 weeks.
  x <- .adh_export(list(`01` = .adh_table(60L), `02` = .adh_table(110L, 2L)))
  for (xlsx in c(x$tables, x$feasibility)) {
    m <- .adh_sheet_cells(xlsx)
    col <- which(unlist(m[4L, ]) == "2 years")
    expect_length(col, 1L)
    # Rows 6 and 7 are enrollment 01, rows 8 and 9 enrollment 02.
    expect_identical(m[[1L]][6:9], c("1", NA, "2", NA), info = xlsx)
    expect_identical(
      m[[2L]][6:9],
      c(
        swereg:::.enrollment_label(x$plan, "01"),
        NA,
        swereg:::.enrollment_label(x$plan, "02"),
        NA
      ),
      info = xlsx
    )
    for (j in 0:2) {
      expect_identical(m[[col + j]][6:7], c(NA_character_, NA_character_))
      expect_false(anyNA(m[[col + j]][8:9]))
    }
  }
})
