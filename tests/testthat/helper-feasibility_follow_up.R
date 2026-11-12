# The feasibility-horizon fixtures of `test-feasibility-follow-up.R`. They
# live here because `test-s3-feasibility.R` reads the same s3 run and export.
# The functions need the spec helpers of `helper-sparse_rule.R`.

FF_W <- 260L
FF_FOLLOW_UP <- list(
  list(label = "1 year", weeks = 52L),
  list(label = "2 years", weeks = 104L)
)
# The two sheets of the minimum-events rule in both workbooks. Minimum events
# holds the largest passing cut of each comparison, outcome and year. Longest
# reportable holds the horizon each threshold reports; its column for the
# spec's own threshold reads the stored verdict of the rule.
FF_MIN_SHEETS <- c("Minimum events", "Longest reportable")
FF_EXPORT_TIME <- as.POSIXct("2026-10-29 12:00:00", tz = "UTC")
# The administrative censoring week that `.ff_plan()` writes into every spec.
FF_ADMIN <- "2023-52"

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
  spec$admin_censoring_isoyearweek <- FF_ADMIN
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

# One grid sheet of the minimum-events rule, Minimum events or Longest
# reportable. The sheet holds two header rows: the outcome over its columns,
# then the column label (a follow-up year, or "threshold <k>"). One row per
# comparison follows, with its ID in column 1 and its name in column 2.
# Returns a list:
#   `outcome`, `label`: the outcome and the label of each sheet column;
#   `id`, `comparison`: the ID and the name of each comparison row;
#   `first_row`: the sheet row of the first comparison;
#   `m`: every cell of the sheet, as text.
.ff_grid <- function(path, sheet) {
  m <- .ff_cells(path, sheet)
  h <- which(m[[1L]] %in% "ID")[1L]
  outcome <- as.character(unlist(m[h, ], use.names = FALSE))
  label <- as.character(unlist(m[h + 1L, ], use.names = FALSE))
  # The outcome stands over the first of its columns only.
  for (j in seq_along(outcome)[-(1:3)]) {
    if (is.na(outcome[j]) || !nzchar(outcome[j])) {
      outcome[j] <- outcome[j - 1L]
    }
  }
  rows <- seq.int(h + 2L, nrow(m))
  rows <- rows[!is.na(m[[1L]][rows])]
  return(list(
    outcome = outcome,
    label = label,
    id = m[[1L]][rows],
    comparison = m[[2L]][rows],
    first_row = h + 2L,
    m = m
  ))
}

# The sheet column of `outcome` and `label` in a grid of `.ff_grid()`.
.ff_grid_col <- function(g, outcome, label) {
  j <- which(g$outcome %in% outcome & g$label %in% label)
  stopifnot(length(j) == 1L)
  return(j)
}

# The cell of comparison `id`, `outcome` and `label` in a grid of
# `.ff_grid()`. An empty cell reads as `NA` or as "".
.ff_grid_cell <- function(g, id, outcome, label) {
  r <- which(g$id %in% id)
  stopifnot(length(r) == 1L)
  return(g$m[[.ff_grid_col(g, outcome, label)]][g$first_row + r - 1L])
}

# The labels of the columns of `outcome`, in sheet order.
.ff_grid_labels <- function(g, outcome) {
  return(g$label[g$outcome %in% outcome])
}

# The fill colour of one cell, as "#RRGGBB", or NA when it has none.
.ff_fill <- function(path, sheet, row, col) {
  wb <- openxlsx::loadWorkbook(path)
  for (so in wb$styleObjects) {
    if (so$sheet == sheet && any(so$rows == row & so$cols == col)) {
      f <- so$style$fill$fillFg
      if (!is.null(f)) {
        return(toupper(sub("^FF", "#", f[[1]])))
      }
    }
  }
  return(NA_character_)
}
