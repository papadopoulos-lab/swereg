# =============================================================================
# The producer stores the number-needed-to-treat interval and the numbers at
# risk; the accessors expose them; the figure reads them
# =============================================================================
#
# Two quantities a manuscript figure prints used to have no home in storage.
#
#   1. The interval of the number needed to treat. `$get_estimates()` carried
#      the point estimate alone, so a figure that wanted the interval had to
#      invert `rd_lo` and `rd_hi` itself. That would put a second estimator in
#      the analysis project.
#   2. The head count of people at risk. `$get_curves()` carried survival and
#      no count, so a survival figure could draw no numbers-at-risk row.
#
# Both are now stored by `.tte_rd_curve()` and returned by the accessors.
#
# THE ROUTE IS THE PRODUCER, AND ONE SITE COMPUTES EACH QUANTITY
# `.tte_nntb()` is the one site that maps a risk-difference interval onto the
# reciprocal scale. `.tte_rd_curve()` calls it and stores the result. Nothing
# downstream inverts a bound again.
# The head count is `uniqueN()` over the person identifier. It is neither the
# row count, which counts person-trials, nor `sum(w)`, which is the weighted
# risk set. Survival is a weighted probability, so no head count follows from
# it.
#
# THE SUBTLE CASE, AND WHY IT NEEDS ITS OWN FIXTURE
# `x -> -1/x` is undefined across zero, so an interval that contains the null
# has no reciprocal interval. Both bounds are `NA` there and
# `interval_status` reads `"spans null"`. The panel below reaches that state,
# and a separate panel reaches `"ok"`, so a mutation that drops the null guard
# reddens one and a mutation that shifts a stored bound reddens the other.

skip_if_not_installed("data.table")

# --- fixtures ---------------------------------------------------------------

# One trial-level panel. `h_int` and `h_cmp` are the per-band event
# probabilities of the two arms. Every person holds two person-trials, so the
# person and the person-trial are genuinely different columns.
.nn_panel <- function(n_persons, n_bands, h_int, h_cmp, seed) {
  set.seed(seed)
  persons <- sprintf("p%04d", seq_len(n_persons))
  arm <- rep(c(TRUE, FALSE), length.out = n_persons)
  rows <- list()
  for (k in seq_along(persons)) {
    h <- if (arm[k]) h_int else h_cmp
    for (trial in 1:2) {
      ev <- stats::rbinom(n_bands, 1L, h)
      first <- which(ev == 1L)
      keep <- if (length(first) > 0L) seq_len(first[1]) else seq_len(n_bands)
      rows[[length(rows) + 1L]] <- data.table::data.table(
        id = persons[k],
        enrollment_person_trial_id = paste0(persons[k], "_t", trial),
        rd_intervention = arm[k],
        tstart = (keep - 1L) * 4L,
        tstop = keep * 4L,
        event = ev[keep],
        w = 1 + 0.2 * (seq_along(keep) %% 3L)
      )
    }
  }
  data.table::rbindlist(rows)
}

.nn_design <- function() {
  swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "rd_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "age",
    follow_up_time = 24L
  )
}

.nn_curve <- function(h_int, h_cmp, seed, n_boot = 200L) {
  d <- .nn_panel(120L, 6L, h_int, h_cmp, seed)
  enr <- swereg::TTEEnrollment$new(d, .nn_design(), data_level = "trial")
  enr$risk_difference(weight_col = "w", n_boot = n_boot, seed = 1L)
}

# A large arm separation, so at least one band has an interval that strictly
# excludes the null and therefore a finite number-needed-to-treat interval.
.nn_curve_ok <- function() {
  .nn_curve(h_int = 0.005, h_cmp = 0.090, seed = 11L)
}

# Two arms that barely differ, so every interval contains the null.
.nn_curve_spans <- function() {
  .nn_curve(h_int = 0.030, h_cmp = 0.032, seed = 14L)
}


# --- the producer stores the interval ---------------------------------------

test_that("the stored curve carries the number-needed-to-treat interval", {
  cv <- .nn_curve_ok()

  expect_true(all(c("nnt_lo", "nnt_hi") %in% names(cv)))

  # The fixture reaches the state the assertion needs. Without an "ok" band
  # every bound would be `NA` and the comparison below would compare nothing.
  expect_true(any(cv$interval_status == "ok"))

  # The bounds are the ones `.tte_nntb()` returns from the SAME three numbers.
  # This is the assertion that says one site computes them.
  want <- swereg:::.tte_nntb(cv$rd, cv$rd_lo, cv$rd_hi)
  expect_equal(cv$nnt_lo, want$nntb_lo)
  expect_equal(cv$nnt_hi, want$nntb_hi)

  # An "ok" band has a real interval, and it is ordered.
  ok <- cv[interval_status == "ok"]
  expect_true(all(is.finite(ok$nnt_lo)))
  expect_true(all(is.finite(ok$nnt_hi)))
  expect_true(all(ok$nnt_lo < ok$nnt_hi))
})


test_that("a spans-null interval has no finite number-needed-to-treat bound", {
  cv <- .nn_curve_spans()

  # The fixture reaches the state the assertion needs.
  expect_true(all(cv$interval_status == "spans null"))

  # The point estimate stays. It is a valid descriptive quantity.
  expect_true(any(is.finite(cv$nnt)))

  # The interval does not. `x -> -1/x` is undefined across zero, so there is
  # no interval to report and the stored value says exactly that.
  expect_true(all(is.na(cv$nnt_lo)))
  expect_true(all(is.na(cv$nnt_hi)))

  # And the cell that renders it prints NOTHING rather than the point estimate
  # alone. `.tte_nntb_cell()` blanks a row whose bounds are missing, which is
  # the convention every number-needed-to-treat cell in the package follows.
  cell <- swereg:::.tte_nntb_cell(
    cv$nnt,
    cv$nnt_lo,
    cv$nnt_hi,
    cv$nnt_direction
  )
  expect_true(all(cell == ""))
})


# --- the producer stores the head count -------------------------------------

test_that("the stored curve carries the distinct-person count at risk", {
  # The canonical 9-row panel. Three people hold five person-trials, so the
  # person count is strictly below the row count in three of the four
  # arm-bands, and the two are not a constant offset apart.
  dt <- data.table::data.table(
    enrollment_person_trial_id = c(
      "p1_trialA", "p1_trialB", "p2_trialA", "p3_trialC", "p3_trialD",
      "p1_trialA", "p2_trialA", "p3_trialC", "p3_trialD"
    ),
    id = c("p1", "p1", "p2", "p3", "p3", "p1", "p2", "p3", "p3"),
    exposed = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    tstop = c(4L, 4L, 4L, 4L, 4L, 8L, 8L, 8L, 8L),
    event = c(0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L),
    w = c(0.5, 0.5, 0.5, 2, 2, 0.5, 0.5, 2, 2),
    age = 50,
    death = 0L
  )
  design <- swereg::TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )
  enr <- swereg::TTEEnrollment$new(dt, design)
  cv <- enr$risk_difference(weight_col = "w", n_boot = 20L, seed = 1L)

  # Hand-counted people, in band order. Intervention: {p1, p2} then {p1, p2}.
  # Comparator: {p3} then {p3}.
  expect_equal(cv$n_persons_at_risk_intervention, c(2L, 2L))
  expect_equal(cv$n_persons_at_risk_comparator, c(1L, 1L))

  # It is the count `$survival_curve()` reports, on the same panel.
  sc <- enr$survival_curve(weight_col = "w")
  expect_equal(
    cv$n_persons_at_risk_intervention,
    sc[exposed == TRUE][order(tstop)]$n_persons_at_risk
  )
  expect_equal(
    cv$n_persons_at_risk_comparator,
    sc[exposed == FALSE][order(tstop)]$n_persons_at_risk
  )

  # And it is NEITHER of the two numbers it is easy to confuse it with. Rows
  # are person-trials and `at_risk` is the weighted risk set.
  rows_int <- dt[exposed == TRUE, .N, keyby = tstop]$N
  expect_false(isTRUE(all.equal(
    as.numeric(cv$n_persons_at_risk_intervention),
    as.numeric(rows_int)
  )))
  expect_false(any(
    cv$n_persons_at_risk_intervention ==
      sc[exposed == TRUE][order(tstop)]$at_risk
  ))
})


# --- the accessors expose both ----------------------------------------------

# One plan holding one stored risk-difference result, built by the production
# splitter from a real curve. Nothing here is hand-written, so the accessor
# meets the shape the producer writes.
.nn_plan <- function(curve) {
  plan <- swereg::TTEPlan$new(
    project_prefix = "nn",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = data.table::data.table(
      enrollment_id = "01",
      ett_id = "ETT00001",
      outcome_var = "osd_a",
      outcome_name = "Outcome A",
      outcome_role = "primary",
      follow_up = 24L,
      age_min = 50L,
      age_max = 59L,
      age_group = "50_59",
      confounder_vars = "age",
      person_id_var = "id",
      treatment_var = "rd_intervention",
      file_imp = "imp_01.qs2",
      file_raw = "raw_01.qs2",
      # NAMED BUT NEVER WRITTEN. The survival figure reads the stored curve,
      # so a read of either file would fail and the test would say so.
      file_analysis = "analysis_missing.qs2",
      file_analysis_itt = "analysis_itt_missing.qs2",
      description = "ETT00001"
    )
  )
  plan$results_ett <- list(
    ETT00001 = c(
      list(enrollment_id = "01", description = "ETT00001"),
      swereg:::.s3_rd_result("rd_pp_trunc", curve, "ETT00001", "tstop")
    )
  )
  plan
}


test_that("get_estimates carries the stored number-needed-to-treat interval", {
  cv <- .nn_curve_ok()
  plan <- .nn_plan(cv)
  row <- plan$results_ett[["ETT00001"]][["rd_pp_trunc"]]

  e <- plan$get_estimates()[estimand == "pp" & weights == "truncated"]
  expect_identical(nrow(e), 1L)
  expect_true(all(c("nnt_lo", "nnt_hi") %in% names(e)))

  # READ from the stored row, never recomputed. The row is the last band, so
  # these are the numbers a forest figure prints.
  expect_equal(e$nnt_lo, as.numeric(row$nnt_lo))
  expect_equal(e$nnt_hi, as.numeric(row$nnt_hi))
  expect_equal(e$nnt, as.numeric(row$nnt))
})


test_that("get_curves carries the stored distinct-person count at risk", {
  cv <- .nn_curve_ok()
  plan <- .nn_plan(cv)

  d <- plan$get_curves()
  expect_true("n_persons_at_risk" %in% names(d))
  expect_identical(nrow(d), 2L * nrow(cv))

  bands <- sort(unique(cv$tstop))
  got_int <- d[arm == "intervention"][order(band)]
  got_cmp <- d[arm == "comparator"][order(band)]
  expect_equal(got_int$band, as.numeric(bands))
  expect_equal(
    got_int$n_persons_at_risk,
    as.numeric(cv$n_persons_at_risk_intervention)
  )
  expect_equal(
    got_cmp$n_persons_at_risk,
    as.numeric(cv$n_persons_at_risk_comparator)
  )

  # The two arms differ, so a melt that read one column twice cannot pass.
  expect_false(isTRUE(all.equal(
    got_int$n_persons_at_risk,
    got_cmp$n_persons_at_risk
  )))
})


# --- the survival figure reads storage and opens no analysis file -----------

test_that("the survival figure draws from storage with no analysis file", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  dir <- withr::local_tempdir()
  cv <- .nn_curve_ok()
  plan <- .nn_plan(cv)
  # The grid names an analysis file that does NOT exist, and no output
  # directory is set at all. A read of either file would error, so this
  # fixture is the witness that the export path makes none.
  expect_false(file.exists(file.path(dir, "analysis_missing.qs2")))

  captured <- NULL
  real_renderer <- swereg:::.render_survival_curve
  testthat::local_mocked_bindings(
    .render_survival_curve = function(curve, ...) {
      captured <<- data.table::copy(curve)
      real_renderer(curve, ...)
    },
    .package = "swereg"
  )

  out <- plan$.__enclos_env__$private$.export_figure(
    list(
      type = "survival",
      enrollment = "01",
      outcome = "osd_a",
      follow_up = 24L,
      age_group = "50_59",
      estimands = "pp",
      label = "surv"
    ),
    file.path(dir, "fig")
  )
  expect_length(out, 1L)
  expect_true(file.exists(out))

  # The numbers-at-risk row prints the STORED head count, per arm and band.
  # Survival is a weighted probability, so a row derived from it would carry
  # other numbers entirely.
  expect_true("n_persons_at_risk" %in% names(captured))
  int <- captured[group == "Intervention"][order(band)]
  cmp <- captured[group == "Comparator"][order(band)]
  expect_equal(
    int$n_persons_at_risk,
    as.numeric(cv$n_persons_at_risk_intervention)
  )
  expect_equal(
    cmp$n_persons_at_risk,
    as.numeric(cv$n_persons_at_risk_comparator)
  )
  # The plotted line is the stored survival of the same arm.
  expect_equal(int$surv, cv$surv_intervention)
  expect_equal(cmp$surv, cv$surv_comparator)
})


# =============================================================================
# Analysis-file reads: none to RENDER, and the producer may still make one
# =============================================================================
#
# Two different operations open an analysis panel, and only one of them is the
# defect phases 5 and 6 removed.
#
#   RENDER. A consumer opens a panel to draw a figure or to fill a sheet. That
#   is what `.export_figure()` did until this release, and it is forbidden. s3
#   computes and s4 formats.
#
#   PRODUCE. `$export_tables()` finds a baseline panel that a earlier release
#   wrote, and calls `$recompute_baselines()`, which calls s3's OWN worker
#   `.s3_enrollment_worker()`. That worker opens the analysis file, computes a
#   Table 1 panel and stores it. It is s3 running late, not s4 computing.
#
# The two tests below pin one each, on one fixture. The analysis file EXISTS on
# disk in both, so a reinstated read SUCCEEDS. The assertion catches it, not an
# error in the setup.

# One enrollment, one emulated trial, every stored result produced by the real
# s3 workers on a panel written to disk.
.nn_export_fixture <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  d <- .nn_panel(60L, 4L, h_int = 0.02, h_cmp = 0.06, seed = 21L)
  d[, rd_age_continuous := 50 + (seq_len(.N) %% 10L)]
  # 4 weeks per band, which is what `.nn_panel()` lays out. `$rates()` needs
  # it and fails loudly without it.
  d[, person_weeks := 4]
  d[, analysis_weight_pp_trunc := 1]
  d[, analysis_weight_pp := 1]
  d[, ipw := 1]
  d[, ipw_trunc := 1]
  design <- swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "rd_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "rd_age_continuous",
    follow_up_time = 16L
  )
  enr <- swereg::TTEEnrollment$new(d, design, data_level = "trial")
  for (f in c("analysis_001.qs2", "analysis_itt_001.qs2", "raw_01.qs2")) {
    qs2::qs_save(enr, file.path(dir, f))
  }
  panel_files <- c("analysis_001.qs2", "analysis_itt_001.qs2", "raw_01.qs2")

  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = "ETT00001",
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    outcome_role = "primary",
    follow_up = 16L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "id",
    treatment_var = "rd_intervention",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = "analysis_001.qs2",
    file_analysis_itt = "analysis_itt_001.qs2",
    description = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "nn",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$output_dir <- dir
  plan$spec <- list(
    study = list(
      title = "Fixture",
      implementation = list(project_prefix = "nn")
    ),
    enrollments = list(list(
      id = "01",
      name = "Enrollment one",
      treatment = list(arms = list(
        intervention = "Treated",
        comparator = "Untreated"
      ))
    ))
  )

  # EVERY stored result comes from the production worker, on the file above.
  # These reads happen in the fixture, before any recorder is installed.
  run <- function(method, weight_col) {
    swereg:::.s3_ett_worker(
      analysis_path = file.path(dir, "analysis_001.qs2"),
      method = method,
      weight_col = weight_col,
      ett_id = "ETT00001",
      n_threads = 1L,
      subgroup_var = NULL,
      conf_level = 0.95
    )
  }
  plan$results_ett <- list(
    ETT00001 = c(
      list(enrollment_id = "01", description = "ETT00001"),
      run("summary_and_rates", "analysis_weight_pp_trunc"),
      run("rates", "ipw_trunc"),
      run("irr", "analysis_weight_pp_trunc"),
      run("irr", "ipw_trunc"),
      run("risk_difference", "analysis_weight_pp_trunc")
    )
  )
  plan$results_enrollment <- list(
    `01` = swereg:::.s3_enrollment_worker(
      analysis_path = file.path(dir, "analysis_001.qs2"),
      raw_path = file.path(dir, "raw_01.qs2"),
      enrollment_id = "01",
      n_threads = 1L,
      arm_labels = c(comparator = "Untreated", intervention = "Treated")
    )
  )
  list(dir = dir, plan = plan, panel_files = panel_files)
}

# The survival exhibit spec this fixture answers.
.NN_SURV_SPEC <- list(
  type = "survival",
  enrollment = "01",
  outcome = "osd_a",
  follow_up = 16L,
  age_group = "50_59",
  estimands = "pp",
  label = "surv"
)


test_that("no consumer reads an analysis file to render", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("qs2")

  fx <- .nn_export_fixture()
  plan <- fx$plan
  # The file EXISTS. A reinstated read therefore SUCCEEDS, and the assertion
  # at the end of this test is what catches it. A fixture with the file absent
  # would catch it as an error in the call, before any assertion ran.
  expect_true(file.exists(file.path(fx$dir, "analysis_001.qs2")))
  expect_false(
    swereg:::.baseline_panel_is_stale(plan$results_enrollment[["01"]])
  )

  seen <- character(0)
  real_read <- swereg::qs2_read
  testthat::local_mocked_bindings(
    qs2_read = function(file, ...) {
      seen <<- c(seen, basename(file))
      real_read(file, ...)
    },
    .package = "swereg"
  )

  # BOTH entry points. The workbook and the figure are separate consumers and
  # a read could return to either one.
  xlsx <- file.path(fx$dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
  fig <- plan$.__enclos_env__$private$.export_figure(
    .NN_SURV_SPEC,
    file.path(fx$dir, "fig")
  )

  # Both produced their artefact, so the assertion below is measured on a run
  # that did the work rather than on one that fell over early.
  expect_true(file.exists(xlsx))
  expect_length(fig, 1L)
  expect_true(file.exists(fig))

  # THE ASSERTION. Neither entry point opened a panel.
  #
  # A MULTISET, not a set. `intersect()` and `expect_setequal()` both discard
  # duplicates, so a consumer that reopened a file the producer already reads
  # would pass either of them. `sort()` on the filtered vector keeps every
  # occurrence, so a second read of one file is a second element.
  expect_identical(sort(seen[seen %in% fx$panel_files]), character(0))
})


test_that("the stale-baseline branch refreshes through the producer", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")

  fx <- .nn_export_fixture()
  plan <- fx$plan
  # A panel an earlier release wrote: not a `swereg_table1`, so no
  # `smd_numeric`. This is the state the branch exists for.
  plan$results_enrollment[["01"]]$table1_ipw_trunc <- data.table::data.table(
    variable = "rd_age_continuous",
    level = "mean (sd)"
  )
  expect_true(
    swereg:::.baseline_panel_is_stale(plan$results_enrollment[["01"]])
  )

  called <- list()
  real_worker <- swereg:::.s3_enrollment_worker
  seen <- character(0)
  real_read <- swereg::qs2_read
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(...) {
      called[[length(called) + 1L]] <<- list(...)
      real_worker(...)
    },
    qs2_read = function(file, ...) {
      seen <<- c(seen, basename(file))
      real_read(file, ...)
    },
    .package = "swereg"
  )

  # The branch fired, and it says so on the console.
  xlsx <- file.path(fx$dir, "tables.xlsx")
  expect_message(
    suppressWarnings(plan$export_tables(path = xlsx)),
    "Refreshing 1 stale baseline table"
  )

  # THE READ ASSERTION, and it comes first because it is the one this test
  # exists for. The worker reads the analysis file ONCE and the raw file ONCE,
  # and nothing else in the export path reads either.
  #
  # A MULTISET, not a set. `intersect()` and `expect_setequal()` both discard
  # duplicates. A consumer read added inside this branch would most likely
  # reopen `analysis_001.qs2`, which the producer already reads. A set
  # comparison cannot see that second read at all. `sort()` on the filtered
  # vector keeps every occurrence, so the second read is a third element.
  expect_identical(
    sort(seen[seen %in% fx$panel_files]),
    sort(c("analysis_001.qs2", "raw_01.qs2"))
  )

  # It refreshed through s3's OWN worker. That is what makes the read a
  # producer's read and not a renderer's.
  #
  # `vapply()` over the recorded calls, never `called[[1L]]`. An empty list
  # subscripted by position ERRORS, and an error here would abort the test
  # before the assertion above ever ran. `vapply()` on an empty list returns
  # `character(0)`, which fails as an assertion.
  expect_length(called, 1L)
  expect_identical(
    vapply(called, function(a) basename(a$analysis_path), character(1)),
    "analysis_001.qs2"
  )

  # The stored panel is current afterwards, so the refresh did the work.
  refreshed <- plan$results_enrollment[["01"]]
  expect_false(swereg:::.baseline_panel_is_stale(refreshed))
  expect_true(inherits(refreshed$table1_ipw_trunc, "swereg_table1"))
  expect_true("smd_numeric" %in% names(refreshed$table1_ipw_trunc))
})
