# =============================================================================
# smd_numeric: the unrounded standardised difference as a real column
# =============================================================================
# The fixture below is hand-computable, which is the point. Both SMDs are
# closed-form, and both differ from their three-decimal display string by more
# than 1e-5, so a value parsed back out of the `SMD` column cannot satisfy the
# assertions here.
#
#   age: |2.5 - 1.0| / sqrt((s0^2 + s1^2) / 2), s0^2 = 4, s1^2 = 5/3
#   edu: |0.75 - 0.25| / sqrt((0.75*0.25 + 0.25*0.75) / 2)
# =============================================================================

love_fixture_t1 <- function(include_smd = TRUE) {
  d <- data.table::data.table(
    exp = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    age = c(0, 0, 0, 4, 1, 2, 3, 4),
    edu = factor(c("a", "b", "b", "b", "a", "a", "a", "b"), levels = c("a", "b"))
  )
  swereg:::.swereg_table1(
    d,
    vars = c("age", "edu"),
    strata = "exp",
    include_smd = include_smd,
    show_missing = "none"
  )
}

# Hand-computed, not read back off the object under test.
SMD_AGE <- 1.5 / sqrt((4 + 5 / 3) / 2)
SMD_EDU <- 0.5 / sqrt(0.1875)


test_that("smd_numeric is a row-parallel column holding the unrounded double", {
  t1 <- love_fixture_t1()

  expect_true("smd_numeric" %in% names(t1))
  expect_type(t1$smd_numeric, "double")
  expect_identical(length(t1$smd_numeric), nrow(t1))
  expect_identical(nrow(t1), 5L)

  # Rows 1 (N), 2 (Sum of weights) and 5 (second level of edu) carry no SMD.
  expect_true(is.na(t1$smd_numeric[1]))
  expect_true(is.na(t1$smd_numeric[2]))
  expect_true(is.na(t1$smd_numeric[5]))

  expect_equal(t1$smd_numeric[3], SMD_AGE, tolerance = 1e-12)
  expect_equal(t1$smd_numeric[4], SMD_EDU, tolerance = 1e-12)

  # It sits beside the display string, it does not replace it.
  expect_identical(t1$SMD[3], "0.891")
  expect_identical(t1$SMD[4], "1.155")
})


test_that("smd_numeric is not the SMD string parsed back to a number", {
  t1 <- love_fixture_t1()
  # as.numeric("0.891") would land exactly on 0.891; the real double does not.
  expect_gt(abs(t1$smd_numeric[3] - 0.891), 1e-5)
  expect_gt(abs(t1$smd_numeric[4] - 1.155), 1e-5)
})


test_that("smd_numeric survives subsetting, reordering, rbindlist and qs2", {
  t1 <- love_fixture_t1()

  # Row subsetting: the SMD stays attached to its own row.
  sub <- t1[c(4, 3)]
  expect_equal(sub$smd_numeric[1], SMD_EDU, tolerance = 1e-12)
  expect_equal(sub$smd_numeric[2], SMD_AGE, tolerance = 1e-12)

  # Row reordering by a different column.
  reordered <- t1[order(t1$Variable)]
  expect_equal(
    reordered$smd_numeric[reordered$Variable == "edu"],
    SMD_EDU,
    tolerance = 1e-12
  )

  # rbindlist of two copies.
  stacked <- data.table::rbindlist(list(t1, t1), use.names = TRUE)
  expect_identical(nrow(stacked), 10L)
  expect_equal(stacked$smd_numeric[3], SMD_AGE, tolerance = 1e-12)
  expect_equal(stacked$smd_numeric[8], SMD_AGE, tolerance = 1e-12)

  # qs2 save/read cycle.
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "t1.qs2")
  swereg::qs2_write_atomic(t1, f)
  back <- swereg::qs2_read(f)
  expect_true("smd_numeric" %in% names(back))
  expect_equal(back$smd_numeric[3], SMD_AGE, tolerance = 1e-12)
  expect_equal(back$smd_numeric[4], SMD_EDU, tolerance = 1e-12)
})


test_that("a cached baseline panel without smd_numeric is marked stale", {
  panel <- love_fixture_t1()
  expect_true("smd_numeric" %in% names(panel))

  fresh <- list(table1_ipw_trunc = panel)
  aged <- list(
    table1_ipw_trunc = data.table::copy(panel)[, smd_numeric := NULL]
  )
  expect_false("smd_numeric" %in% names(aged$table1_ipw_trunc))
  # The aged panel still carries the class, which is what the pre-repair
  # predicate tested. Only the column test can tell it apart.
  expect_s3_class(aged$table1_ipw_trunc, "swereg_table1")

  # Reach the predicate export_tables() uses, not a copy of its expression.
  expect_false(swereg:::.baseline_panel_is_stale(fresh))
  expect_true(swereg:::.baseline_panel_is_stale(aged))

  # A pre-swereg_table1 cache stays stale, and an empty result stays current.
  plain <- list(table1_ipw_trunc = data.frame(Variable = "N"))
  expect_true(swereg:::.baseline_panel_is_stale(plain))
  expect_false(swereg:::.baseline_panel_is_stale(NULL))
  expect_false(swereg:::.baseline_panel_is_stale(list(n_baseline = 10L)))

  # And export_tables() reaches it: the vapply target is this function.
  export_src <- paste(
    deparse(body(utils::removeSource(swereg::TTEPlan$public_methods$export_tables))),
    collapse = " "
  )
  expect_match(export_src, ".baseline_panel_is_stale", fixed = TRUE)
})


test_that(".render_love_plot draws the 0.1 line and both weighting series", {
  skip_if_not_installed("ggplot2")

  unw <- love_fixture_t1()
  # A second, better-balanced panel standing in for the weighted series.
  d <- data.table::data.table(
    exp = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    age = c(1, 2, 3, 4, 1, 2, 3, 4),
    edu = factor(c("a", "b", "a", "b", "a", "b", "a", "b"), levels = c("a", "b"))
  )
  wtd <- swereg:::.swereg_table1(
    d,
    vars = c("age", "edu"),
    strata = "exp",
    include_smd = TRUE,
    show_missing = "none"
  )

  df <- swereg:::.build_love_df(unw, wtd)
  p <- swereg:::.render_love_plot(df, title = "balance")
  expect_s3_class(p, "ggplot")

  # The 0.1 reference line.
  vlines <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)
  expect_length(vlines, 1L)
  xis <- unname(unlist(lapply(vlines, function(l) l$data$xintercept)))
  expect_equal(xis, 0.1, tolerance = 1e-12)

  # Both weighting series, on the same covariates.
  expect_setequal(
    as.character(unique(p$data$weighting)),
    c("Unweighted", "IPW truncated")
  )
  expect_setequal(as.character(unique(p$data$variable)), c("age", "edu"))
  expect_identical(nrow(p$data), 4L)

  # The plotted values are the unrounded doubles, not the display strings.
  unw_age <- p$data[
    p$data$weighting == "Unweighted" & p$data$variable == "age",
  ]$smd
  expect_equal(unw_age, SMD_AGE, tolerance = 1e-12)

  # It builds without error, which is what insertImage() needs.
  expect_silent(invisible(ggplot2::ggplot_build(p)))
})


test_that(".write_love_plot adds a sheet and writes PNG + PDF sidecars", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("openxlsx")

  t1 <- love_fixture_t1()
  tmp <- withr::local_tempdir()
  wb <- openxlsx::createWorkbook()
  paths <- swereg:::.write_love_plot(
    wb,
    "Love plot",
    t1_unweighted = t1,
    t1_weighted = t1,
    title = "Love plot: covariate balance",
    img_dir = tmp,
    img_basename = "tables_love_plot"
  )
  expect_true("Love plot" %in% openxlsx::sheets(wb))
  expect_true(file.exists(file.path(tmp, "tables_love_plot.png")))
  expect_true(file.exists(file.path(tmp, "tables_love_plot.pdf")))
  expect_identical(basename(paths$png), "tables_love_plot.png")
})


test_that("smd_numeric never reaches a rendered table", {
  t1 <- love_fixture_t1()
  expect_true("smd_numeric" %in% names(t1))

  stripped <- swereg:::.t1_drop_numeric(t1)
  expect_false("smd_numeric" %in% names(stripped))
  expect_true("SMD" %in% names(stripped))
  # The input is not modified by reference.
  expect_true("smd_numeric" %in% names(t1))
  # A panel that never had the column is returned untouched.
  no_smd <- love_fixture_t1(include_smd = FALSE)
  expect_identical(swereg:::.t1_drop_numeric(no_smd), no_smd)

  wb <- openxlsx::createWorkbook()
  swereg:::.write_tableone_sheet(wb, "Table 1", t1, title = "T1")
  written <- openxlsx::readWorkbook(wb, "Table 1", startRow = 3L)
  expect_false("smd_numeric" %in% names(written))
  expect_true("SMD" %in% names(written))
})


test_that("export_tables appends a TOC name and a TOC description in lockstep", {
  # The sheet ritual is three steps: write the sheet, append to toc_names,
  # append to toc_desc. Dropping the third step misaligns every later row of
  # the table of contents and raises no error, so count the appends in the
  # method body directly. This is a syntax-tree count of assignments of the
  # form `x <- c(x, ...)`; it proves the two vectors grow the same number of
  # times, not that each pair describes the same sheet.
  count_appends <- function(expr, target) {
    n <- 0L
    sym <- as.name(target)
    walk <- function(e) {
      if (!is.call(e)) {
        return(invisible(NULL))
      }
      if (
        length(e) >= 3L &&
          identical(e[[1L]], quote(`<-`)) &&
          identical(e[[2L]], sym) &&
          is.call(e[[3L]]) &&
          identical(e[[3L]][[1L]], quote(c)) &&
          length(e[[3L]]) >= 2L &&
          identical(e[[3L]][[2L]], sym)
      ) {
        n <<- n + 1L
      }
      for (i in seq_along(e)) {
        part <- e[[i]]
        ok <- tryCatch(
          {
            is.call(part)
            TRUE
          },
          error = function(...) FALSE
        )
        if (ok) walk(part)
      }
      invisible(NULL)
    }
    walk(expr)
    n
  }

  fn <- utils::removeSource(swereg::TTEPlan$public_methods$export_tables)
  n_names <- count_appends(body(fn), "toc_names")
  n_desc <- count_appends(body(fn), "toc_desc")

  expect_gt(n_names, 10L)
  expect_identical(n_names, n_desc)
})


# =============================================================================
# The headline Table 1 panel: it carries an SMD, and a stale one is detected
# =============================================================================
# The fixture writes an enrollment to disk and calls `.s3_enrollment_worker()`.
# The worker owns `main_args`, so the tests below read the real argument list.
# A test that called `.swereg_table1()` directly would assert its own
# arguments and would pass whatever `main_args` said.
#
# Baseline rows repeat the hand-computed values at the top of this file, so
# SMD_AGE and SMD_EDU stay valid. Weights are all 1, and `.t1_wtd_mean_sd()`
# collapses to the sample SD under equal weights.
#
#   age: comparator 0,0,0,4 and intervention 1,2,3,4
#   edu: comparator a,b,b,b and intervention a,a,a,b
#
# `smoke` is missing for one person in each arm. The main panel MUST suppress
# its Missing row and MUST divide by the non-missing denominator.
# =============================================================================

main_panel_worker_result <- function(dir) {
  lv_edu <- c("a", "b")
  lv_smoke <- c("no", "yes")
  baseline <- data.table::data.table(
    id = 1:8,
    tstart = 0L,
    trt = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    age = c(0, 0, 0, 4, 1, 2, 3, 4),
    edu = factor(c("a", "b", "b", "b", "a", "a", "a", "b"), levels = lv_edu),
    smoke = factor(
      c(NA, "no", "no", "yes", "yes", "yes", NA, "no"),
      levels = lv_smoke
    )
  )
  # Follow-up rows hold different values, so a broken baseline slice moves
  # every number the tests below assert on.
  follow_up <- data.table::copy(baseline)
  follow_up[, tstart := 1L]
  follow_up[, age := 99]
  follow_up[, edu := factor("b", levels = lv_edu)]
  follow_up[, smoke := factor("no", levels = lv_smoke)]

  d <- data.table::rbindlist(list(baseline, follow_up))
  d[, ipw := 1]
  d[, ipw_trunc := 1]

  enrollment <- list(
    design = list(
      tstart_var = "tstart",
      treatment_var = "trt",
      confounder_vars = c("age", "edu", "smoke")
    ),
    data = d
  )
  path <- file.path(dir, "analysis.qs2")
  swereg::qs2_write_atomic(enrollment, path)

  swereg:::.s3_enrollment_worker(
    analysis_path = path,
    raw_path = file.path(dir, "absent-raw-file.qs2"),
    enrollment_id = "e1",
    # The same value $recompute_baselines() passes, so the worker's
    # setDTthreads() call leaves this session's thread count unchanged.
    n_threads = data.table::getDTthreads(),
    arm_labels = c(comparator = "Comparator", intervention = "Intervention")
  )
}


test_that("the headline Table 1 panel carries an SMD column", {
  res <- main_panel_worker_result(withr::local_tempdir())
  main <- res$table1_ipw_trunc_main
  expect_s3_class(main, "swereg_table1")

  # `SMD` is the display column that reaches the CSV and the worksheet.
  expect_true("SMD" %in% names(main))
  # `smd_numeric` travels beside it, for the Love plot and balance checks.
  expect_true("smd_numeric" %in% names(main))

  # age: the arms differ, so the SMD is a real value and not a blank cell.
  age_row <- which(startsWith(main$Variable, "age"))
  expect_length(age_row, 1L)
  expect_identical(main$SMD[age_row], "0.891")
  expect_equal(main$smd_numeric[age_row], SMD_AGE, tolerance = 1e-12)

  # edu: the arms differ here too.
  edu_row <- which(main$Variable == "edu")
  expect_length(edu_row, 1L)
  expect_identical(main$SMD[edu_row], "1.155")
  expect_equal(main$smd_numeric[edu_row], SMD_EDU, tolerance = 1e-12)

  # The supplementary panel keeps its own SMD; the main panel did not steal it.
  expect_true("SMD" %in% names(res$table1_ipw_trunc))
})


test_that("the headline Table 1 panel keeps its non-missing denominator", {
  res <- main_panel_worker_result(withr::local_tempdir())
  main <- res$table1_ipw_trunc_main
  supp <- res$table1_ipw_trunc

  # `smoke` is missing for one person in each arm. The supplementary panel
  # shows that row and the main panel does not.
  expect_false("Missing" %in% main$Level)
  expect_true("Missing" %in% supp$Level)

  # Every column divides by the non-missing denominator, so the two observed
  # levels of `smoke` sum to 100 per cent.
  i <- which(main$Variable == "smoke")
  expect_length(i, 1L)
  smoke <- main[i:(i + 1L)]
  expect_identical(smoke$Level, c("no", "yes"))
  as_pct <- function(x) as.numeric(sub("%$", "", x))
  expect_equal(sum(as_pct(smoke$Overall)), 100, tolerance = 1e-9)
  expect_equal(sum(as_pct(smoke[["Comparator"]])), 100, tolerance = 1e-9)
  expect_equal(sum(as_pct(smoke[["Intervention"]])), 100, tolerance = 1e-9)
})


test_that("a cached result whose main panel lacks smd_numeric is stale", {
  # The state the pre-repair predicate could not see: an earlier refresh gave
  # the supplementary panel an smd_numeric, and left the main panel without
  # one. The %||% chain stops at table1_ipw_trunc and never reads the main
  # panel, so it reports the whole result as current.
  supp <- love_fixture_t1(include_smd = TRUE)
  main_aged <- love_fixture_t1(include_smd = FALSE)

  expect_true("smd_numeric" %in% names(supp))
  expect_false("smd_numeric" %in% names(main_aged))
  expect_false("SMD" %in% names(main_aged))
  # The aged main panel still carries the class, so only the column test can
  # tell it apart.
  expect_s3_class(main_aged, "swereg_table1")

  half_refreshed <- list(
    table1_ipw_trunc = supp,
    table1_ipw_trunc_main = main_aged
  )
  expect_true(swereg:::.baseline_panel_is_stale(half_refreshed))

  # Every panel carries smd_numeric, so the result stays current.
  both_current <- list(
    table1_ipw_trunc = supp,
    table1_ipw_trunc_main = love_fixture_t1(include_smd = TRUE)
  )
  expect_false(swereg:::.baseline_panel_is_stale(both_current))

  # A stale main panel counts on its own, with no other panel present.
  expect_true(
    swereg:::.baseline_panel_is_stale(list(table1_ipw_trunc_main = main_aged))
  )

  # A stale supplementary panel still counts, whatever the main panel holds.
  expect_true(swereg:::.baseline_panel_is_stale(list(
    table1_raw = data.table::copy(supp)[, smd_numeric := NULL],
    table1_ipw_trunc_main = love_fixture_t1(include_smd = TRUE)
  )))

  # A panel the worker never produced is absent, not stale.
  expect_false(swereg:::.baseline_panel_is_stale(list(table1_ipw_trunc = supp)))
})


test_that("a freshly computed enrollment result is not stale", {
  # The predicate and the worker MUST agree, or $export_tables() recomputes
  # every enrollment on every call and never converges.
  res <- main_panel_worker_result(withr::local_tempdir())
  expect_false(swereg:::.baseline_panel_is_stale(res))
})


test_that("the exported table1 CSV carries SMD and not smd_numeric", {
  dir <- withr::local_tempdir()
  res <- main_panel_worker_result(dir)

  # Drive the real private method, with a real worker result on a real plan.
  # The method reads the panel through `$get_baselines()` and heads the two arm
  # columns from the specification, so `self` MUST be a TTEPlan and its
  # specification MUST name the arms the worker built the panel with.
  plan <- swereg::TTEPlan$new(
    project_prefix = "t1",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = data.table::data.table(
      enrollment_id = "e1",
      ett_id = "ETT00001",
      outcome_var = "osd_a",
      outcome_name = "Outcome A",
      follow_up = 52L,
      description = "ETT00001",
      age_min = 50L,
      age_max = 59L,
      confounder_vars = "age",
      person_id_var = "id",
      treatment_var = "trt"
    )
  )
  plan$spec <- list(
    study = list(implementation = list(project_prefix = "t1")),
    enrollments = list(list(
      id = "e1",
      name = "Enrollment one",
      treatment = list(arms = list(
        intervention = "Intervention",
        comparator = "Comparator"
      ))
    ))
  )
  plan$results_enrollment <- list(e1 = res)

  export_table <- swereg::TTEPlan$private_methods$.export_table
  env <- new.env(parent = environment(export_table))
  env$self <- plan
  environment(export_table) <- env

  path <- export_table(
    spec = list(type = "table1", enrollment = "e1", label = "table1"),
    dir = file.path(dir, "exhibits")
  )
  expect_true(file.exists(path))

  csv <- data.table::fread(path, colClasses = "character")
  # The display column survives; the programmatic column does not.
  expect_true("SMD" %in% names(csv))
  expect_false("smd_numeric" %in% names(csv))

  age_row <- which(startsWith(csv$Variable, "age"))
  expect_length(age_row, 1L)
  expect_identical(csv$SMD[age_row], "0.891")
})


# =============================================================================
# Table 1 reads the entry-window snapshot on both routes
# =============================================================================
# Time zero is the landmark, so the `tstart == 0` row holds the confounder
# value of the LANDMARK band. `.tte_entry__<v>` holds the value at the
# recruiting week, and Table 1 MUST describe that instant.
#
# The fixture below repeats the hand-computed values at the top of this file in
# the `.tte_entry__` columns, and puts values that would move every number into
# the plain columns. A route that reads the plain column therefore misses
# SMD_AGE and SMD_EDU.
# =============================================================================

entry_snapshot_enrollment <- function() {
  lv_edu <- c("a", "b")
  lv_smoke <- c("no", "yes")
  d <- data.table::data.table(
    id = 1:8,
    tstart = 0L,
    trt = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    # The landmark-band values: a route that reads these gets 99 everywhere.
    age = 99,
    edu = factor("b", levels = lv_edu),
    smoke = factor("no", levels = lv_smoke),
    # The entry-window snapshot: the values SMD_AGE and SMD_EDU come from.
    .tte_entry__age = c(0, 0, 0, 4, 1, 2, 3, 4),
    .tte_entry__edu = factor(
      c("a", "b", "b", "b", "a", "a", "a", "b"),
      levels = lv_edu
    ),
    .tte_entry__smoke = factor(
      c(NA, "no", "no", "yes", "yes", "yes", NA, "no"),
      levels = lv_smoke
    )
  )
  d[, ipw := 1]
  d[, ipw_trunc := 1]
  d[]
}

test_that("the plan's Table 1 worker reads the entry-window snapshot", {
  d <- entry_snapshot_enrollment()
  enrollment <- list(
    design = list(
      tstart_var = "tstart",
      treatment_var = "trt",
      confounder_vars = c("age", "edu", "smoke")
    ),
    data = d
  )
  panel <- swereg:::.s3_enrollment_table1(
    enrollment,
    ipw_col = "ipw_trunc",
    arm_labels = c(comparator = "Comparator", intervention = "Intervention"),
    show_missing = "none"
  )

  age_row <- which(startsWith(panel$Variable, "age"))
  expect_length(age_row, 1L)
  expect_equal(panel$smd_numeric[age_row], SMD_AGE, tolerance = 1e-12)

  edu_row <- which(panel$Variable == "edu")
  expect_length(edu_row, 1L)
  expect_equal(panel$smd_numeric[edu_row], SMD_EDU, tolerance = 1e-12)

  # The landmark value is 99 in both arms, so a route that read it would show
  # an SMD of zero. Assert the mean it printed instead.
  expect_true(startsWith(panel$Overall[age_row], "1.75"))
})

test_that("the $table1() method and the plan's worker agree", {
  d <- entry_snapshot_enrollment()
  design <- TTEDesign$new(
    person_id_var = "id",
    treatment_var = "trt",
    outcome_vars = "event",
    confounder_vars = c("age", "edu", "smoke"),
    follow_up_time = 4L
  )
  d[, event := 0L]
  d[, tstop := 4L]
  d[, enrollment_person_trial_id := as.character(id)]
  trial <- TTEEnrollment$new(data.table::copy(d), design)

  by_method <- trial$table1(
    ipw_col = "ipw_trunc",
    arm_labels = c(comparator = "Comparator", intervention = "Intervention"),
    show_missing = "none"
  )
  by_worker <- swereg:::.s3_enrollment_table1(
    trial,
    ipw_col = "ipw_trunc",
    arm_labels = c(comparator = "Comparator", intervention = "Intervention"),
    show_missing = "none"
  )
  expect_equal(by_method, by_worker)
})
