# The fixture of the minimum-events rule per subgroup level, built by the
# real `$s3_analyze()`. test-horizon-subgroup-level.R describes its ETTs.
# test-subgroup-reporting.R reads its workbook too.

# One row per 4-week interval. `rd_intervention` TRUE is the intervention
# arm. In each arm, the first `thin_events` persons of level "thin" have an
# event, at weeks 8, 12, 16, and so on. Every other thin person is followed
# to `end_wk` with no event. The "big" level and the NA level have random
# events.
.hl_panel <- function(thin_events, end_wk = 104L, seed = 11L) {
  set.seed(seed)
  n_int <- end_wk %/% 4L
  one <- function(id, arm, sg, ev) {
    keep <- if (any(ev == 1L)) seq_len(which(ev == 1L)[1L]) else seq_len(n_int)
    return(data.table::data.table(
      id = id,
      enrollment_person_trial_id = paste0(id, "_t1"),
      rd_intervention = arm,
      tstart = (keep - 1L) * 4L,
      tstop = keep * 4L,
      event = ev[keep],
      person_weeks = 4,
      rd_age_continuous = 50 + as.integer(sub("^p", "", id)) %% 10L,
      rd_sg = sg,
      rd_one = if (is.na(sg)) NA_character_ else "x"
    ))
  }
  rows <- list()
  k <- 0L
  for (arm in c(TRUE, FALSE)) {
    for (j in seq_len(100L)) {
      k <- k + 1L
      ev <- stats::rbinom(n_int, 1L, 0.02)
      rows[[k]] <- one(sprintf("p%04d", k), arm, "big", ev)
    }
    for (j in seq_len(8L)) {
      k <- k + 1L
      ev <- integer(n_int)
      if (j <= thin_events) {
        ev[j + 1L] <- 1L
      }
      rows[[k]] <- one(sprintf("p%04d", k), arm, "thin", ev)
    }
    for (j in seq_len(4L)) {
      k <- k + 1L
      ev <- stats::rbinom(n_int, 1L, 0.02)
      rows[[k]] <- one(sprintf("p%04d", k), arm, NA_character_, ev)
    }
  }
  d <- data.table::rbindlist(rows)
  for (w in c("analysis_weight_pp_trunc", "analysis_weight_pp", "ipw")) {
    data.table::set(d, j = w, value = 1)
  }
  data.table::set(d, j = "ipw_trunc", value = 1)
  return(d)
}

# Write the analysis files and return the plan, with the stage stamps set and
# nothing computed.
.hl_build <- function(dir) {
  design <- swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "rd_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "rd_age_continuous",
    follow_up_time = 104L
  )
  # Two events per arm in level "thin" fail the rule at 5. Seven pass it.
  few <- .hl_panel(thin_events = 2L)
  many <- .hl_panel(thin_events = 7L)
  save <- function(d, file) {
    enr <- swereg::TTEEnrollment$new(d, design, data_level = "trial")
    qs2::qs_save(enr, file.path(dir, file))
  }
  files <- list(
    # The per-protocol level "thin" of ETT00001 fails. Its own
    # intention-to-treat level passes.
    e1_pp = few[tstop <= 52],
    e1_itt = many[tstop <= 52],
    # The part-year ETT00002: its own level "thin" passes in both estimands.
    e2 = many[tstop <= 40],
    e3 = few[tstop <= 90],
    e4 = few,
    e5 = few[tstop <= 52],
    e6 = many[tstop <= 52]
  )
  for (nm in names(files)) {
    save(files[[nm]], paste0(nm, ".qs2"))
  }
  for (enr in c("01", "02", "03")) {
    save(if (enr == "03") many else few, paste0("raw_", enr, ".qs2"))
  }
  ett <- data.table::data.table(
    enrollment_id = c("01", "01", "01", "01", "02", "03"),
    ett_id = sprintf("ETT%05d", 1:6),
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    outcome_role = "primary",
    follow_up = c(52L, 40L, 90L, 104L, 52L, 52L),
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "id",
    treatment_var = "rd_intervention",
    file_imp = "imp.qs2",
    file_raw = paste0("raw_", c("01", "01", "01", "01", "02", "03"), ".qs2"),
    file_analysis = c(
      "e1_pp.qs2",
      "e2.qs2",
      "e3.qs2",
      "e4.qs2",
      "e5.qs2",
      "e6.qs2"
    ),
    file_analysis_itt = c(
      "e1_itt.qs2",
      "e2.qs2",
      "e3.qs2",
      "e4.qs2",
      "e5.qs2",
      "e6.qs2"
    ),
    description = sprintf("ETT%05d", 1:6),
    feasibility_horizon = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  data.table::set(
    ett,
    j = "subgroup_vars",
    value = list(rep(list(c("rd_sg", "rd_one")), 6L))
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "hl",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$output_dir <- dir
  arms <- list(arms = list(intervention = "Treated", comparator = "Untreated"))
  plan$spec <- list(
    study = list(
      title = "Fixture",
      implementation = list(project_prefix = "hl")
    ),
    enrollments = list(
      list(id = "01", name = "Enrollment one", treatment = arms),
      list(
        id = "02",
        name = "Enrollment two",
        treatment = arms,
        minimum_events_rule_exempt = TRUE
      ),
      list(id = "03", name = "Enrollment three", treatment = arms)
    ),
    min_expected_events_per_arm_per_year = 5,
    period_width = 4L,
    admin_censoring_isoyearweek = "2020-52"
  )
  tte_stamp_stages(plan)
  return(plan)
}

# s3 through the REAL batch runner (worker subprocesses), then the workbook.
# Built once per file.
.hl_cache <- new.env()
.hl_fixture <- function() {
  if (!is.null(.hl_cache$fx)) {
    return(.hl_cache$fx)
  }
  dir <- tempfile("hl_s3_")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = testthat::teardown_env())
  plan <- .hl_build(dir)
  out <- utils::capture.output(suppressWarnings(plan$s3_analyze(
    output_dir = dir,
    n_workers = 3L,
    swereg_dev_path = swereg:::.swereg_dev_path()
  )))
  # The export error is kept, not thrown. A test that reads s3 results then
  # fails at its own assertion, also when the export stops.
  xlsx <- file.path(dir, "tables.xlsx")
  export_error <- tryCatch(
    {
      suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  .hl_cache$fx <- list(
    dir = dir,
    plan = plan,
    xlsx = xlsx,
    out = out,
    export_error = export_error
  )
  return(.hl_cache$fx)
}

.hl_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("survey")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
}
