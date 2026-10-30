# The feasibility-horizon fixtures of `test-feasibility-follow-up.R`. They
# live here because `test-s3-feasibility.R` reads the same s3 run and export.
# The functions need the spec helpers of `helper-sparse_rule.R`.

FF_W <- 260L
FF_FOLLOW_UP <- list(
  list(label = "1 year", weeks = 52L),
  list(label = "2 years", weeks = 104L)
)
FF_MIN_SHEETS <- c("Minimum events (Y or -)", "Minimum events (#)")
FF_EXPORT_TIME <- as.POSIXct("2026-10-29 12:00:00", tz = "UTC")

# ---- the spec -----------------------------------------------------------------

# The spec of helper-sparse_rule.R with follow-ups 52 and 104, one subgroup and
# `n_outcomes` outcomes. `w` is the value of `feasibility_follow_up_weeks`, or
# `NULL` for no key.
.ff_spec <- function(w = FF_W, n_outcomes = 1L, min_events = 1) {
  spec <- spr_spec_list()
  spec$follow_up <- FF_FOLLOW_UP
  spec$min_expected_events_per_arm_per_year <- min_events
  spec$subgroups <- list(list(
    name = "Subgroup",
    implementation = list(variable = "rd_sg")
  ))
  if (n_outcomes > 1L) {
    spec$outcomes <- lapply(seq_len(n_outcomes), function(k) {
      return(list(
        name = paste("Outcome", LETTERS[k]),
        description = "planted constant-hazard event",
        implementation = list(variable = paste0("osd_", letters[k]))
      ))
    })
  }
  if (!is.null(w)) {
    spec$feasibility_follow_up_weeks <- w
  }
  return(spec)
}

# Write `spec` under `root` and build the plan through the s0 entry point.
# Returns the plan.
.ff_plan <- function(spec, root) {
  dirs <- file.path(root, c("spec", "tteplan", "results", "meta"))
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  spr_write_spec(spec, dirs[1])
  plan <- suppressMessages(swereg::tteplan_from_spec_and_registrystudy(
    study = list(
      skeleton_files = file.path(dirs[2], "skel_none.qs2"),
      data_meta_dir = dirs[4]
    ),
    candidate_dir_spec = dirs[1],
    candidate_dir_tteplan = dirs[2],
    candidate_dir_results = dirs[3],
    spec_version = "v001",
    global_max_isoyearweek = "2023-52",
    check_skeletons = FALSE
  ))
  return(plan)
}

# Write one YAML spec with raw text for the key, and return its path.
.ff_spec_path <- function(dir, raw_key = NULL) {
  spec <- .ff_spec(NULL)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  extra <- if (is.null(raw_key)) {
    NULL
  } else {
    paste0("feasibility_follow_up_weeks: ", raw_key, "\n")
  }
  return(spr_write_spec(spec, dir, extra_yaml = extra))
}


# ---- the s3 fixture -----------------------------------------------------------------

# One trial per person, 4-week intervals to `end_wk`. Every intervention
# person stops at week 104, except the first `tail_keep`, so the intervention
# arm holds a thin tail after the second follow-up year.
.ff_panel <- function(n = 200L, tail_keep = 6L, end_wk = FF_W, seed = 11L) {
  set.seed(seed)
  rows <- vector("list", n)
  for (k in seq_len(n)) {
    arm <- k %% 2L == 1L
    last <- if (arm && (k + 1L) / 2L > tail_keep) 104L else end_wk
    n_int <- last %/% 4L
    ev <- stats::rbinom(n_int, 1L, if (arm) 0.01 else 0.02)
    keep <- if (any(ev == 1L)) seq_len(which(ev == 1L)[1L]) else seq_len(n_int)
    rows[[k]] <- data.table::data.table(
      id = sprintf("p%04d", k),
      enrollment_person_trial_id = sprintf("p%04d_t1", k),
      rd_intervention = arm,
      tstart = (keep - 1L) * 4L,
      tstop = keep * 4L,
      event = ev[keep],
      person_weeks = 4,
      rd_age_continuous = 50 + k %% 10L,
      rd_sg = if (k %% 4L < 2L) "a" else "b"
    )
  }
  d <- data.table::rbindlist(rows)
  w <- 1 + 0.2 * (d$tstop %/% 4L %% 3L)
  d[, analysis_weight_pp_trunc := w]
  d[, analysis_weight_pp := w * 1.1]
  d[, ipw := 1]
  d[, ipw_trunc := w]
  return(d)
}

# The analysis files of every ETT of `plan`: the panel cut at the follow-up
# of the ETT, as s2 cuts it. Both enrollments read the same panel. The raw
# file of an enrollment is its shortest analysis panel.
.ff_write_files <- function(plan, dir, panel) {
  for (i in seq_len(nrow(plan$ett))) {
    r <- plan$ett[i]
    fu <- as.integer(r$follow_up)
    design <- swereg::TTEDesign$new(
      person_id_var = "id",
      id_var = "enrollment_person_trial_id",
      treatment_var = "rd_intervention",
      outcome_vars = r$outcome_var,
      confounder_vars = "rd_age_continuous",
      follow_up_time = fu
    )
    enr <- swereg::TTEEnrollment$new(
      panel[tstop <= fu],
      design,
      data_level = "trial"
    )
    for (f in c(r$file_analysis, r$file_analysis_itt)) {
      qs2::qs_save(enr, file.path(dir, f))
    }
    if (fu == min(plan$ett$follow_up)) {
      qs2::qs_save(enr, file.path(dir, r$file_raw))
    }
  }
  return(invisible(NULL))
}

# s3 through the REAL batch runner, then the workbook, for the plan with the
# key ("key") and without it ("none"). Both plans carry one set of stage
# stamps and one creation time, so a difference between their outputs comes
# from the key alone. Built once per file.
.ff_cache <- new.env()
.ff_fixture <- function(key = TRUE) {
  name <- if (key) "key" else "none"
  # The cache outlives a test file, and the teardown of that file deletes
  # the root. A deleted root is built again.
  if (!is.null(.ff_cache[[name]]) && dir.exists(.ff_cache[[name]]$root)) {
    return(.ff_cache[[name]])
  }
  root <- tempfile(paste0("ff_", name, "_"))
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = testthat::teardown_env())
  plan <- .ff_plan(.ff_spec(if (key) FF_W else NULL), root)
  dir <- file.path(root, "tteplan")
  .ff_write_files(plan, dir, .ff_panel())
  plan$output_dir <- dir
  tte_stamp_stages(plan)
  if (is.null(.ff_cache$stamps)) {
    .ff_cache$stamps <- list(
      s1 = plan$s1_run_id,
      s2 = unique(plan$ett$s2_run_id),
      created_at = plan$created_at
    )
  }
  plan$s1_run_id <- .ff_cache$stamps$s1
  plan$created_at <- .ff_cache$stamps$created_at
  ett <- data.table::copy(plan$ett)
  data.table::set(ett, j = "s1_run_id", value = .ff_cache$stamps$s1)
  data.table::set(ett, j = "s2_run_id", value = .ff_cache$stamps$s2)
  plan$ett <- ett
  out <- utils::capture.output(suppressWarnings(plan$s3_analyze(
    output_dir = dir,
    n_workers = 3L,
    swereg_dev_path = swereg:::.swereg_dev_path()
  )))
  xlsx <- file.path(root, "results", "tables.xlsx")
  # One export time for both workbooks, so every Provenance cell compares.
  testthat::local_mocked_bindings(
    .export_timestamp = function() FF_EXPORT_TIME,
    .package = "swereg"
  )
  suppressMessages(suppressWarnings(plan$export_tables(path = xlsx)))
  spec_xlsx <- file.path(root, "results", "spec.xlsx")
  suppressMessages(suppressWarnings(plan$excel_spec_summary(path = spec_xlsx)))
  .ff_cache[[name]] <- list(
    root = root,
    dir = dir,
    plan = plan,
    xlsx = xlsx,
    spec_xlsx = spec_xlsx,
    out = out
  )
  return(.ff_cache[[name]])
}

.ff_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("survey")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
}

# Every cell of one sheet, as text.
.ff_cells <- function(path, sheet) {
  m <- openxlsx::read.xlsx(
    path,
    sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  m[] <- lapply(m, as.character)
  return(m)
}
