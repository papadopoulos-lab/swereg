# The minimum detectable effect (MDE) on the PP results and ITT results sheets.
#
# `.tte_mde()` is the naive Poisson formula. Its inputs are the UNWEIGHTED
# comparator events, comparator person-years and intervention person-years
# that `$rates()` stores at s3. The expected values below are computed by hand,
# with the two normal quantiles written out, so a change to the formula cannot
# move the expectation with it.

# qnorm(0.975) + qnorm(0.8), to 15 significant digits.
.mde_k_95_80 <- 1.95996398454005 + 0.841621233572914

test_that(".tte_mde matches the hand computation", {
  r <- swereg:::.tte_mde(
    e0 = 50,
    py0 = 1000,
    py1 = 400,
    power = 0.8,
    conf_level = 0.95
  )
  # E1 = 50 / 1000 * 400 = 20, se = sqrt(1 / 20 + 1 / 50) = sqrt(0.07).
  expect_equal(r$expected_events, 20, tolerance = 1e-10)
  expect_equal(
    r$mde_protective,
    exp(-.mde_k_95_80 * sqrt(0.07)),
    tolerance = 1e-10
  )
  expect_equal(
    r$mde_harmful,
    exp(.mde_k_95_80 * sqrt(0.07)),
    tolerance = 1e-10
  )
  expect_equal(r$mde_protective, 0.476527532727025, tolerance = 1e-10)
  expect_equal(r$mde_harmful, 2.09851463204508, tolerance = 1e-10)
})

test_that(".tte_mde gives NA MDE values when e0 or E1 is 0", {
  no_cmp <- swereg:::.tte_mde(0, 1000, 400, 0.8, 0.95)
  expect_identical(no_cmp$expected_events, 0)
  expect_true(is.na(no_cmp$mde_protective))
  expect_true(is.na(no_cmp$mde_harmful))

  no_int_time <- swereg:::.tte_mde(50, 1000, 0, 0.8, 0.95)
  expect_identical(no_int_time$expected_events, 0)
  expect_true(is.na(no_int_time$mde_protective))
  expect_true(is.na(no_int_time$mde_harmful))

  expect_error(
    swereg:::.tte_mde(50, 1000, 400, power = 1, conf_level = 0.95),
    "power"
  )
})


# A weighted panel. Every intervention row weighs 3 and every comparator row
# weighs 0.5, so each weighted total differs from its unweighted total.
# 20 person-trials per arm, two 4-week rows each: 160 person-weeks per arm.
# Events: 5 in the intervention arm and 4 in the comparator arm.
.mde_weighted_trial <- function() {
  n_per_arm <- 20L
  n <- 2L * n_per_arm
  ids <- sprintf("t%03d", seq_len(n))
  exposed <- rep(c(TRUE, FALSE), each = n_per_arm)
  d <- data.table::data.table(
    enrollment_person_trial_id = rep(ids, each = 2L),
    id = rep(sprintf("p%03d", seq_len(n)), each = 2L),
    tstart = rep(c(0L, 4L), n),
    tstop = rep(c(4L, 8L), n),
    exposed = rep(exposed, each = 2L),
    person_weeks = 4L,
    ipw = rep(ifelse(exposed, 3, 0.5), each = 2L),
    event = 0L
  )
  hit <- c(utils::head(ids[exposed], 5L), utils::head(ids[!exposed], 4L))
  data.table::set(
    d,
    i = which(d$enrollment_person_trial_id %in% hit & d$tstop == 8L),
    j = "event",
    value = 1L
  )
  design <- swereg::TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    outcome_vars = "event",
    confounder_vars = character(0),
    follow_up_time = 8L
  )
  return(swereg::TTEEnrollment$new(d, design))
}

# The unweighted person-years of each arm: 160 person-weeks / 52.25.
.mde_py_arm <- 160 / 52.25

test_that("$rates() stores the unweighted events and person-years per arm", {
  r <- .mde_weighted_trial()$rates("ipw")
  int <- r[exposed == TRUE]
  cmp <- r[exposed == FALSE]
  expect_identical(int$events_unweighted, 5L)
  expect_identical(cmp$events_unweighted, 4L)
  expect_equal(int$py_unweighted, .mde_py_arm, tolerance = 1e-12)
  expect_equal(cmp$py_unweighted, .mde_py_arm, tolerance = 1e-12)
  # The weighted totals differ, so a test on these values can tell them apart.
  expect_equal(int$events_weighted, 15)
  expect_equal(cmp$py_weighted, 0.5 * .mde_py_arm, tolerance = 1e-12)
})


# A one-ETT plan whose PP and ITT rates come from the weighted panel above.
.mde_plan <- function(rates) {
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
    treatment_var = "exposed",
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
  irr <- list(IRR = 0.9, IRR_lower = 0.5, IRR_upper = 1.6, IRR_pvalue = 0.7)
  plan$results_ett <- list(
    ETT00001 = list(
      enrollment_id = "01",
      description = "ETT00001",
      rates_pp_trunc = rates,
      rates_pp = rates,
      rates_itt = rates,
      irr_pp_trunc = irr,
      irr_pp = irr,
      irr_itt = irr
    )
  )
  return(plan)
}

# Write one results sheet and read its header row and first data row back.
.mde_read_sheet <- function(plan, rates_slot, irr_slot) {
  wb <- openxlsx::createWorkbook()
  swereg:::.write_results_single(
    wb,
    "Results",
    plan,
    rates_slot = rates_slot,
    irr_slot = irr_slot,
    title = "Results"
  )
  p <- tempfile(fileext = ".xlsx")
  on.exit(unlink(p), add = TRUE)
  openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
  raw <- openxlsx::read.xlsx(p, sheet = "Results", colNames = FALSE,
    skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  # Title row 1, blank row 2, header row 3, data row 4.
  header <- as.character(unlist(raw[3, ]))
  d <- openxlsx::read.xlsx(p, sheet = "Results", startRow = 3,
    sep.names = " ", skipEmptyCols = FALSE)
  return(list(header = header, data = d))
}

test_that("the results sheet MDE reads the unweighted counts", {
  skip_if_not_installed("openxlsx")
  plan <- .mde_plan(.mde_weighted_trial()$rates("ipw"))
  for (slots in list(
    c("rates_pp_trunc", "irr_pp_trunc"),
    c("rates_itt", "irr_itt")
  )) {
    s <- .mde_read_sheet(plan, slots[[1]], slots[[2]])
    # By hand: e0 = 4 and py0 = py1, so E1 = 4 and se = sqrt(1/4 + 1/4).
    # The weighted counts give E1 = 4 / (0.5 * py) * (3 * py) = 24.
    expect_equal(s$data[["Expected events (int), null"]][1], 4)
    expect_equal(
      s$data[["MDE IRR, protective"]][1],
      exp(-.mde_k_95_80 * sqrt(0.5)),
      tolerance = 1e-10
    )
    expect_equal(
      s$data[["MDE IRR, harmful"]][1],
      exp(.mde_k_95_80 * sqrt(0.5)),
      tolerance = 1e-10
    )
  }
})

test_that("the MDE block sits right after the p-value column", {
  skip_if_not_installed("openxlsx")
  plan <- .mde_plan(.mde_weighted_trial()$rates("ipw"))
  s <- .mde_read_sheet(plan, "rates_pp_trunc", "irr_pp_trunc")
  expect_identical(
    s$header[14:17],
    c(
      "p-value",
      "Expected events (int), null",
      "MDE IRR, protective",
      "MDE IRR, harmful"
    )
  )
})

test_that("a plan stored before the unweighted counts gives three blank cells", {
  skip_if_not_installed("openxlsx")
  rates <- .mde_weighted_trial()$rates("ipw")
  rates[, c("events_unweighted", "py_unweighted") := NULL]
  s <- .mde_read_sheet(.mde_plan(rates), "rates_pp_trunc", "irr_pp_trunc")
  expect_true(is.na(s$data[["Expected events (int), null"]][1]))
  expect_true(is.na(s$data[["MDE IRR, protective"]][1]))
  expect_true(is.na(s$data[["MDE IRR, harmful"]][1]))
  # The measurement block still reports the weighted counts.
  expect_equal(s$data[["Events (int)"]][1], 15)
})


test_that("$export_tables() puts the MDE columns on PP and ITT results only", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  plan <- .xp_plan("new")
  # ETT00001 stores unweighted counts. Every other trial is a plan stored
  # before them, so its three cells are blank.
  add_unweighted <- function(rv) {
    rv <- data.table::copy(rv)
    rv[, `:=`(events_unweighted = c(12, 30), py_unweighted = c(600, 900))]
    return(rv)
  }
  for (slot in c("rates_pp_trunc", "rates_itt")) {
    plan$results_ett$ETT00001[[slot]] <- add_unweighted(
      plan$results_ett$ETT00001[[slot]]
    )
  }
  dir <- tempfile("mde-export")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- file.path(dir, "tables.xlsx")
  suppressMessages(suppressWarnings(plan$export_tables(path = path)))

  mde_headers <- c(
    "Expected events (int), null",
    "MDE IRR, protective",
    "MDE IRR, harmful"
  )
  # e0 = 30, py0 = 900, py1 = 600: E1 = 20 and se = sqrt(1/20 + 1/30).
  for (sheet in c("PP results", "ITT results")) {
    d <- openxlsx::read.xlsx(path, sheet = sheet, startRow = 3,
      sep.names = " ", skipEmptyCols = FALSE)
    expect_true(all(mde_headers %in% names(d)), label = sheet)
    # Measurement block, then MDE block, then risk-difference block.
    expect_identical(
      names(d)[14:18],
      c("p-value", mde_headers, "Persons with event (int)")
    )
    # Row 1 is ETT00001, the first trial of `plan$ett`.
    expect_identical(d$Outcome[1], "Outcome A")
    expect_equal(d[["Expected events (int), null"]][1], 20)
    expect_equal(
      d[["MDE IRR, protective"]][1],
      exp(-.mde_k_95_80 * sqrt(1 / 20 + 1 / 30)),
      tolerance = 1e-10
    )
    expect_gt(nrow(d), 1L)
    expect_true(all(is.na(d[["MDE IRR, harmful"]][-1])))
  }
  raw <- openxlsx::read.xlsx(path, sheet = "Weight truncation (PP)",
    colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  cells <- as.character(unlist(raw))
  expect_false(any(mde_headers %in% cells))
  expect_false(any(grepl("MDE", cells)))
})


test_that("tteenrollment_rates_combine() accepts old and new rates tables together", {
  # A rates table stored before 27.2.0 has no unweighted columns. A plan that
  # reran some trials holds both shapes, and combining them MUST NOT stop.
  old <- data.table::data.table(
    tx = c(TRUE, FALSE),
    n_persons = c(3, 7),
    n_trials = c(30, 70),
    events_weighted = c(10.4, 20.6),
    py_weighted = c(1000, 2000),
    rate_per_100000py = c(1040, 1030)
  )
  data.table::setattr(old, "treatment_var", "tx")
  new <- data.table::copy(old)
  new[, `:=`(events_unweighted = c(10, 20), py_unweighted = c(900, 1900))]
  data.table::setattr(new, "treatment_var", "tx")
  res <- list(
    ETT1 = list(rates_pp_trunc = old),
    ETT2 = list(rates_pp_trunc = new)
  )

  got <- NULL
  expect_no_error(
    got <- swereg::tteenrollment_rates_combine(res, "rates_pp_trunc")
  )
  expect_identical(got$ett_id, c("ETT1", "ETT2"))
  expect_identical(got$events_weighted_Intervention, c("10.4", "10.4"))
  expect_identical(got$py_weighted_Comparator, c("2,000", "2,000"))
})


test_that("a forest figure draws an estimable ratio below 0.01", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("patchwork")
  plan <- .xp_plan("new")
  plan$results_ett$ETT00001$irr_pp_trunc <- swereg:::.s3_mark_irr_estimable(
    data.table::data.table(
      IRR = 0.004,
      IRR_lower = 0.002,
      IRR_upper = 0.008,
      IRR_pvalue = 1e-9,
      warn = FALSE,
      events_intervention = 3,
      events_comparator = 500
    )
  )
  real_renderer <- swereg:::.render_combined_forest_plot
  got <- NULL
  testthat::local_mocked_bindings(
    .render_combined_forest_plot = function(...) {
      got <<- real_renderer(...)
      return(got)
    },
    .package = "swereg"
  )
  dir <- tempfile("mde-forest")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  spec <- list(
    type = "forest",
    exposures = list(X = c("ETT00001", "ETT00002")),
    label = "forest"
  )
  out <- swereg:::.plan_export(plan, list(spec), dir)
  expect_true(file.exists(out))

  # The forest panel is the last plot of the patchwork. Its layers are the
  # reference line, the intervals and the points, on a log10 x scale.
  panel <- got$plot[[length(got$plot)]]
  built <- ggplot2::ggplot_build(panel)
  geoms <- vapply(panel$layers, function(l) class(l$geom)[1], character(1))
  pts <- built$data[[which(geoms == "GeomPoint")]]
  rng <- built$data[[which(geoms == "GeomLinerange")]]
  expect_true(any(abs(pts$x - log10(0.004)) < 1e-9))
  expect_true(any(
    abs(rng$xmin - log10(0.002)) < 1e-9 & abs(rng$xmax - log10(0.008)) < 1e-9
  ))
  # The panel range holds the whole interval, so nothing is clipped.
  x_range <- built$layout$panel_params[[1]]$x.range
  expect_lte(x_range[1], log10(0.002))
  row <- got$text[got$text$ett_id %in% "ETT00001"]
  expect_identical(row$txt_irr, "0.0040 (0.0020 to 0.0080)")
})


test_that("$export_tables(power = ) sets the power of the MDE", {
  skip_if_not_installed("openxlsx")
  plan <- .xp_plan("new")
  # ETT00001 stores unweighted counts: e0 = 30, py0 = 900, py1 = 600.
  for (slot in c("rates_pp_trunc", "rates_itt")) {
    rv <- data.table::copy(plan$results_ett$ETT00001[[slot]])
    rv[, `:=`(events_unweighted = c(12, 30), py_unweighted = c(600, 900))]
    plan$results_ett$ETT00001[[slot]] <- rv
  }
  dir <- tempfile("mde-power")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  read_mde <- function(power) {
    path <- file.path(dir, paste0("tables_", power, ".xlsx"))
    suppressMessages(suppressWarnings(
      plan$export_tables(path = path, power = power)
    ))
    d <- openxlsx::read.xlsx(path, sheet = "PP results", startRow = 3,
      sep.names = " ", skipEmptyCols = FALSE)
    return(c(d[["MDE IRR, protective"]][1], d[["MDE IRR, harmful"]][1]))
  }
  at_80 <- read_mde(0.8)
  at_90 <- read_mde(0.9)
  want_90 <- swereg:::.tte_mde(30, 900, 600, power = 0.9, conf_level = 0.95)
  expect_equal(
    at_90,
    c(want_90$mde_protective, want_90$mde_harmful),
    tolerance = 1e-10
  )
  # Higher power needs a larger effect, so the protective MDE moves down.
  expect_lt(at_90[1], at_80[1])
  expect_gt(at_90[2], at_80[2])
})
