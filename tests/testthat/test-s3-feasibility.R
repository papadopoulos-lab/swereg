# $s3_feasibility(): the minimum-events rule after s2, with no outcome model.
#
# Three properties carry the invariant:
#   1. The decision equals the one `$s3_analyze()` makes on the same plan,
#      for every ETT and estimand. The two methods resolve the analysis file,
#      the weight, the follow-up and the rule through one helper, and call one
#      record builder.
#   2. No artifact of the step holds an outcome after time zero: no
#      `events_weighted`, `rate_pooled` or `events_expected`, by column name
#      or by value.
#   3. `slurm_job("feasibility")` and `tte_stage("feasibility", ...)` reach
#      the method.
#
# The fixture is the s1 -> s2 -> s3 run of `spr_report_pipeline()`: two ETTs
# of one 52-week window each. Two changes follow that run.
#   * ETT00002 loses its `file_analysis_itt`, as on a grid from before that
#     column existed. Both methods then derive the ITT path from the PP path.
#   * The threshold becomes the largest, over ETT and estimand, of the
#     smallest expected events of an arm. The ETT estimand that holds that
#     value passes with one arm exactly at the threshold, because a window
#     passes at `>=`. Every estimand with a smaller minimum fails.
# The windows do not depend on the threshold, so the s3 run inside the helper
# gives the expected events that pick it.

.fz_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("qs2")
  skip_if_not_installed("survey")
}

.FZ_STRIPPED <- c("events_weighted", "rate_pooled", "events_expected")

# Every numeric leaf of a nested list, data.tables included.
.fz_numbers <- function(x) {
  if (is.list(x)) {
    return(unlist(lapply(x, .fz_numbers), use.names = FALSE))
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  return(numeric(0))
}

# Every data.frame in a nested list.
.fz_tables <- function(x) {
  if (is.data.frame(x)) {
    return(list(x))
  }
  if (is.list(x)) {
    return(unlist(lapply(x, .fz_tables), recursive = FALSE))
  }
  return(list())
}

# Every cell of every sheet, as text.
.fz_cells <- function(xlsx) {
  out <- list()
  for (s in openxlsx::getSheetNames(xlsx)) {
    m <- openxlsx::read.xlsx(
      xlsx,
      sheet = s,
      colNames = FALSE,
      skipEmptyRows = FALSE,
      skipEmptyCols = FALSE
    )
    v <- unlist(lapply(m, as.character), use.names = FALSE)
    out[[s]] <- v[!is.na(v)]
  }
  return(out)
}

# The rows of `sheet` under the header row that holds the cell `header`.
.fz_sheet <- function(xlsx, sheet, header) {
  m <- openxlsx::read.xlsx(
    xlsx,
    sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  m[] <- lapply(m, as.character)
  h <- which(apply(m, 1L, function(r) header %in% r))[1L]
  out <- m[-seq_len(h), , drop = FALSE]
  names(out) <- as.character(unlist(m[h, ]))
  return(out)
}

.fz_cache <- new.env()
.fz_fixture <- function() {
  if (!is.null(.fz_cache$fx)) {
    return(.fz_cache$fx)
  }
  run <- spr_report_pipeline()
  withr::defer(
    unlink(run$root, recursive = TRUE),
    envir = testthat::teardown_env()
  )
  plan <- run$plan
  dev <- swereg:::.swereg_dev_path()

  # The values the step MUST NOT write, from the s3 run of the helper.
  wins <- list()
  for (eid in plan$ett$ett_id) {
    for (est in names(swereg:::.TTE_ESTIMANDS)) {
      wins[[paste(eid, est)]] <-
        plan$results_ett[[eid]][[paste0("horizon_", est)]]$windows
    }
  }
  seeded <- unique(unlist(
    lapply(wins, function(w) unlist(w[, .FZ_STRIPPED, with = FALSE])),
    use.names = FALSE
  ))
  threshold <- max(vapply(
    wins,
    function(w) min(w$events_expected),
    numeric(1)
  ))

  ett <- data.table::copy(plan$ett)
  ett[ett_id == "ETT00002", file_analysis_itt := NA_character_]
  plan$ett <- ett
  plan$spec$min_expected_events_per_arm_per_year <- threshold

  # Record every dispatch, then run the real one.
  calls <- new.env()
  calls$log <- list()
  real <- swereg:::.batch_run
  testthat::local_mocked_bindings(
    .batch_run = function(target, ..., collect = TRUE) {
      calls$log[[length(calls$log) + 1L]] <- list(
        symbol = target$symbol,
        items = list(...)$items
      )
      return(real(target, ..., collect = collect))
    },
    .package = "swereg"
  )
  # The feasibility step runs FIRST, so s3 runs on a plan that holds its
  # result.
  out_fz <- utils::capture.output(
    plan$s3_feasibility(n_workers = 2L, swereg_dev_path = dev)
  )
  feas_parity <- plan$feasibility
  n_fz <- length(calls$log)
  out_s3 <- utils::capture.output(suppressWarnings(
    plan$s3_analyze(n_workers = 2L, swereg_dev_path = dev)
  ))

  # The leak run: the threshold back to an integer, through the real stage
  # entry point, from the plan on disk.
  plan$spec$min_expected_events_per_arm_per_year <- 5
  path <- plan$save()
  testthat::local_mocked_bindings(
    handlers = function(...) invisible(NULL),
    .package = "progressr"
  )
  withr::local_options(list(progressr.enable = getOption("progressr.enable")))
  n_before_stage <- length(calls$log)
  out_stage <- utils::capture.output(
    staged <- swereg::tte_stage(
      "feasibility",
      plan$dir_tteplan,
      n_workers = 1L,
      swereg_dev_path = dev
    )
  )
  .fz_cache$fx <- list(
    plan = plan,
    feas = feas_parity,
    seeded = seeded,
    threshold = threshold,
    log_fz = calls$log[seq_len(n_fz)],
    log_s3 = calls$log[(n_fz + 1L):n_before_stage],
    log_stage = calls$log[-seq_len(n_before_stage)],
    out_fz = out_fz,
    out_stage = out_stage,
    staged = staged,
    saved = swereg::tteplan_load(path),
    xlsx = file.path(staged$dir_results, "feasibility.xlsx")
  )
  return(.fz_cache$fx)
}

# The items one recorded run sent to one worker.
.fz_items <- function(log, symbol) {
  hit <- Filter(function(x) identical(x$symbol, symbol), log)
  return(unlist(lapply(hit, `[[`, "items"), recursive = FALSE))
}


# ---- 1. the decision equals s3's ---------------------------------------------

test_that("the fixture passes one estimand, fails another and holds a window at the threshold", {
  .fz_skip()
  fx <- .fz_fixture()
  withheld <- unlist(lapply(fx$plan$ett$ett_id, function(eid) {
    return(c(
      fx$plan$results_ett[[eid]]$horizon_pp$withheld,
      fx$plan$results_ett[[eid]]$horizon_itt$withheld
    ))
  }))
  expect_true(any(withheld))
  expect_true(any(!withheld))
  # One arm sits exactly at the threshold, and its window passes.
  at <- unlist(lapply(fx$plan$results_ett, function(r) {
    return(c(
      r$horizon_pp$windows[events_expected == fx$threshold]$pass,
      r$horizon_itt$windows[events_expected == fx$threshold]$pass
    ))
  }))
  expect_identical(unname(at), TRUE)
  expect_true(is.na(
    fx$plan$ett[ett_id == "ETT00002"]$file_analysis_itt
  ))
})

test_that("each estimand reads the analysis file, weight, follow-up and rule that s3 reads", {
  .fz_skip()
  fx <- .fz_fixture()
  fz <- .fz_items(fx$log_fz, ".s3_feasibility_worker")
  rd <- Filter(
    function(it) identical(it$method, "risk_difference"),
    .fz_items(fx$log_s3, ".s3_ett_worker")
  )
  expect_length(fz, 4L)
  expect_length(rd, 4L)
  for (it in rd) {
    est <- if (identical(it$weight_col, "ipw_trunc")) "itt" else "pp"
    got <- fz[[paste(it$ett_id, est, sep = "__")]]
    info <- paste(it$ett_id, est)
    expect_identical(got$analysis_path, it$analysis_path, info = info)
    expect_identical(got$weight_col, it$weight_col, info = info)
    expect_identical(got$follow_up, it$follow_up, info = info)
    expect_identical(got$horizon_rule, it$horizon_rule, info = info)
  }
  # The same four fields, read off the two stored records.
  for (eid in fx$plan$ett$ett_id) {
    for (est in c("pp", "itt")) {
      s3 <- fx$plan$results_ett[[eid]][[paste0("horizon_", est)]]
      fz_rec <- fx$feas$records[[eid]][[est]]
      for (f in c("rule", "weight_col", "follow_up")) {
        expect_identical(fz_rec[[f]], s3[[f]], info = paste(eid, est, f))
      }
    }
  }
})

test_that("the decision of every ETT and estimand equals s3's, field by field", {
  .fz_skip()
  fx <- .fz_fixture()
  for (eid in fx$plan$ett$ett_id) {
    for (est in c("pp", "itt")) {
      s3 <- fx$plan$results_ett[[eid]][[paste0("horizon_", est)]]
      fz_rec <- fx$feas$records[[eid]][[est]]
      info <- paste(eid, est)
      expect_identical(
        fz_rec$windows,
        s3$windows[, c("window_lo", "window_hi", "arm", "py_weighted", "pass")],
        info = info
      )
      for (f in c(
        "reportable_weeks",
        "withheld",
        "fail_lo",
        "fail_hi",
        "fail_arm",
        "status"
      )) {
        expect_identical(fz_rec[[f]], s3[[f]], info = paste(info, f))
      }
    }
  }
})

test_that("an old grid without file_analysis_itt derives the ITT file in both methods", {
  .fz_skip()
  fx <- .fz_fixture()
  fz <- .fz_items(fx$log_fz, ".s3_feasibility_worker")
  pp <- fz[["ETT00002__pp"]]$analysis_path
  itt <- fz[["ETT00002__itt"]]$analysis_path
  expect_identical(itt, sub("_analysis_", "_analysis_itt_", pp, fixed = TRUE))
  expect_true(file.exists(itt))
  # A grid with no file_analysis_itt column at all resolves the same paths.
  ett <- fx$plan$ett
  no_col <- data.table::copy(ett)[, file_analysis_itt := NULL]
  for (i in seq_len(nrow(ett))) {
    a <- swereg:::.tte_s3_resolve_ett(
      ett,
      i,
      "D",
      list(min_expected_events = 1)
    )
    b <- swereg:::.tte_s3_resolve_ett(
      no_col,
      i,
      "D",
      list(min_expected_events = 1)
    )
    expect_identical(b$estimands, a$estimands, info = ett$ett_id[i])
  }
})

test_that("$s3_analyze() reads no plan$feasibility", {
  # Its body and the three helpers it shares with the feasibility step.
  fns <- list(
    TTEPlan$public_methods$s3_analyze,
    swereg:::.tte_s3_output_dir,
    swereg:::.tte_s3_enrollment_item,
    swereg:::.tte_s3_resolve_ett
  )
  for (fn in fns) {
    txt <- deparse(body(utils::removeSource(fn)))
    expect_false(any(grepl("feasibility", txt, fixed = TRUE)))
  }
})


# ---- 2. no outcome after time zero -------------------------------------------

test_that("no stored window table holds an event, rate or expected-events column", {
  .fz_skip()
  fx <- .fz_fixture()
  for (feas in list(fx$feas, fx$staged$feasibility, fx$saved$feasibility)) {
    tabs <- .fz_tables(feas)
    expect_length(tabs, 4L)
    for (t in tabs) {
      expect_identical(
        names(t),
        c("window_lo", "window_hi", "arm", "py_weighted", "pass")
      )
      expect_length(intersect(names(t), .FZ_STRIPPED), 0L)
      expect_false(any(grepl("event|rate", names(t), ignore.case = TRUE)))
    }
  }
})

test_that("no sheet of feasibility.xlsx names a stripped column", {
  .fz_skip()
  fx <- .fz_fixture()
  expect_identical(
    openxlsx::getSheetNames(fx$xlsx),
    c(
      "Study Specification",
      "Rare conditions",
      "Attrition",
      "Table 1",
      "Minimum events"
    )
  )
  me <- .fz_sheet(fx$xlsx, "Minimum events", "ETT")
  expect_false(any(grepl("event|rate", names(me), ignore.case = TRUE)))
  expect_identical(nrow(me), 4L)
  cells <- .fz_cells(fx$xlsx)
  for (s in names(cells)) {
    expect_length(intersect(cells[[s]], .FZ_STRIPPED), 0L)
  }
})

test_that("no stripped value reaches the workbook or the saved plan", {
  .fz_skip()
  fx <- .fz_fixture()
  v <- fx$seeded
  # The check means something only for values that nothing else holds by
  # chance, as an integer count or a window edge would.
  expect_gt(length(v), 0L)
  expect_true(all(v != round(v)))
  hit <- function(x) {
    x <- x[is.finite(x)]
    return(v[vapply(
      v,
      function(s) any(abs(x - s) <= 1e-9 * max(1, abs(s))),
      logical(1)
    )])
  }
  cells <- .fz_cells(fx$xlsx)
  for (s in names(cells)) {
    num <- suppressWarnings(as.numeric(cells[[s]]))
    expect_length(hit(num), 0L)
    # A value written as text, rounded to four decimals.
    txt <- sprintf("%.4f", v)
    found <- txt[vapply(
      txt,
      function(t) any(grepl(t, cells[[s]], fixed = TRUE)),
      logical(1)
    )]
    expect_length(found, 0L)
  }
  # The stage run uses the threshold 5. The parity run is not checked here:
  # its threshold is itself one of the expected events, by construction.
  expect_length(hit(.fz_numbers(fx$saved$feasibility)), 0L)
  expect_length(hit(.fz_numbers(fx$staged$feasibility)), 0L)
})

test_that("Table 1 of feasibility.xlsx names no outcome", {
  .fz_skip()
  fx <- .fz_fixture()
  cells <- .fz_cells(fx$xlsx)[["Table 1"]]
  expect_gt(length(cells), 0L)
  for (o in unique(c(fx$plan$ett$outcome_var, fx$plan$ett$outcome_name))) {
    expect_false(any(grepl(o, cells, fixed = TRUE)), info = o)
  }
})


# ---- 3. the stage plumbing ----------------------------------------------------

test_that("slurm_job('feasibility') calls tte_stage('feasibility', ...)", {
  dir <- withr::local_tempdir()
  plan <- TTEPlan$new("proj009", "/tmp/skeleton.qs2", "2020-52")
  plan$dir_tteplan_cp <- CandidatePath$new(dir, "dir_tteplan")
  withr::local_envvar(c(SWEREG_N_WORKERS_FEASIBILITY = "3"))

  job <- plan$slurm_job("feasibility", time = "01:00:00")

  expect_identical(
    job[["script"]],
    sprintf(
      "Rscript -e 'swereg::tte_stage(\"feasibility\", \"%s\", n_workers = 3L)'",
      normalizePath(dir, winslash = "/", mustWork = TRUE)
    )
  )
  expect_identical(job[["name"]], "proj009_feasibility")
  expect_identical(job[["cpus"]], "3")
  expect_true(job[["requeue"]])
})

test_that("tte_stage('feasibility', ...) runs $s3_feasibility() and saves the plan", {
  .fz_skip()
  fx <- .fz_fixture()
  # The stage dispatched the feasibility worker, on every ETT and estimand.
  expect_length(.fz_items(fx$log_stage, ".s3_feasibility_worker"), 4L)
  expect_length(.fz_items(fx$log_stage, ".s3_ett_worker"), 0L)
  expect_match(fx$out_stage, "feasibility: wrote ", all = FALSE)
  # The saved plan holds the result of the stage run, at threshold 5.
  expect_identical(
    fx$saved$feasibility$records$ETT00001$pp$rule,
    list(min_expected_events = 5)
  )
  expect_identical(
    fx$saved$feasibility$computed_at,
    fx$staged$feasibility$computed_at
  )
})

test_that("tteplan_load() accepts a plan saved without the feasibility field", {
  .fz_skip()
  fx <- .fz_fixture()
  path <- file.path(fx$plan$dir_tteplan, "tteplan.qs2")
  old <- qs2::qs_read(path)
  e <- new.env(parent = parent.env(old))
  for (nm in setdiff(ls(old, all.names = TRUE), "feasibility")) {
    assign(nm, get(nm, envir = old), envir = e)
  }
  class(e) <- class(old)
  expect_false(exists("feasibility", envir = e, inherits = FALSE))
  old_path <- file.path(withr::local_tempdir(), "tteplan.qs2")
  qs2::qs_save(e, old_path)
  loaded <- swereg::tteplan_load(old_path)
  expect_null(loaded$feasibility)
})


# ---- a window that only the comparator arm fails ------------------------------

# Two 52-week windows, threshold 0.3. Window 1 passes in both arms. In window
# 2 the comparator arm holds 0.15 person-years, so its expected events are
# 0.028, and the intervention arm's are 1.97. Only the comparator fails.
.fz_cmp_panel <- function() {
  return(data.table::data.table(
    id = c("i1", "i2", "i3", "c1", "c2"),
    enrollment_person_trial_id = c("i1_t1", "i2_t1", "i3_t1", "c1_t1", "c2_t1"),
    rd_intervention = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    tstart = c(0, 0, 52, 0, 52),
    tstop = c(104, 52, 104, 52, 60),
    event = c(0L, 1L, 1L, 0L, 1L),
    person_weeks = c(104, 52, 52, 52, 8),
    rd_age_continuous = c(50, 51, 52, 53, 54),
    analysis_weight_pp_trunc = c(10, 1, 1, 10, 1)
  ))
}

# A plan that holds one feasibility record, for the sheet row builder.
.fz_cmp_plan <- function(rec) {
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = "ETT00001",
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    outcome_role = "primary",
    follow_up = 104L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "id",
    treatment_var = "rd_intervention",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = "analysis_01.qs2",
    file_analysis_itt = "analysis_itt_01.qs2",
    description = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "fz",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$feasibility <- list(records = list(ETT00001 = list(pp = rec)))
  return(plan)
}

test_that("a window that only the comparator fails reads 'no' and matches s3's record", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("survey")
  design <- swereg::TTEDesign$new(
    person_id_var = "id",
    id_var = "enrollment_person_trial_id",
    treatment_var = "rd_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "rd_age_continuous",
    follow_up_time = 104L
  )
  path <- file.path(withr::local_tempdir(), "analysis_01.qs2")
  qs2::qs_save(
    swereg::TTEEnrollment$new(.fz_cmp_panel(), design, data_level = "trial"),
    path
  )
  rule <- list(min_expected_events = 0.3)
  # The two real workers, in process: the feasibility worker and the s3
  # risk-difference item, which returns s3's record as `horizon_pp`.
  fz_rec <- swereg:::.s3_feasibility_worker(
    analysis_path = path,
    ett_id = "ETT00001",
    n_threads = 1L,
    weight_col = "analysis_weight_pp_trunc",
    follow_up = 104,
    horizon_rule = rule
  )
  s3_rec <- suppressWarnings(swereg:::.s3_ett_worker(
    analysis_path = path,
    method = "risk_difference",
    weight_col = "analysis_weight_pp_trunc",
    ett_id = "ETT00001",
    n_threads = 1L,
    horizon_rule = rule,
    follow_up = 104
  ))$horizon_pp

  # The fixture: in window 2 the intervention arm alone would pass.
  w2 <- s3_rec$windows[window_lo == 52]
  expect_gte(w2[arm == "intervention"]$events_expected, 0.3)
  expect_lt(w2[arm == "comparator"]$events_expected, 0.3)

  expect_true(fz_rec$withheld)
  expect_identical(fz_rec$fail_arm, "comparator")
  expect_identical(fz_rec$status, s3_rec$status)
  expect_identical(fz_rec$reportable_weeks, s3_rec$reportable_weeks)
  expect_identical(fz_rec$reportable_weeks, 52)

  # The sheet reads 'no' for window 2. It MUST also read 'no' when the window
  # table carries one pass value per arm, as an arm-level rule would write
  # it: the intervention row TRUE and the comparator row FALSE.
  per_arm <- data.table::copy(fz_rec)
  per_arm$windows <- data.table::copy(fz_rec$windows)
  per_arm$windows[, pass := s3_rec$windows$events_expected >= 0.3]
  expect_identical(
    per_arm$windows[window_lo == 52]$pass,
    c(TRUE, FALSE)
  )
  for (r in list(window = fz_rec, arm = per_arm)) {
    rows <- swereg:::.feasibility_sheet_rows(.fz_cmp_plan(r))
    expect_identical(rows$`Window (weeks)`, c("0-52", "52-104"))
    expect_identical(rows$`Window passes`, c("yes", "no"))
  }
})
