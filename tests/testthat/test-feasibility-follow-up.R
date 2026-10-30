# The feasibility horizon of the spec key `feasibility_follow_up_weeks`.
#
# With the key set to W, and W longer than every follow-up of the spec, the
# ETT grid gets one more follow-up of W weeks for every enrollment and
# outcome. s2 builds those ETTs as any other ETT. s3 computes only the
# per-protocol horizon record and the intention-to-treat record that follows
# it, and stores no estimate. Every output other than the two Minimum events
# sheets is the output of the same plan without the key.
#
# The blocks follow the brief, (a) to (d):
#   (a) the key: a bad value stops and names the key, an absent key is fine,
#       and $reload_spec() refuses a change of it as structural;
#   (b) the grid: one W-week ETT per enrollment and outcome, after the spec's
#       own ETTs, and no extra ETT without the key or with W <= 104;
#   (c) after s3: two horizon records and no estimate for each W-week ETT,
#       and no accessor row for it;
#   (d) parity: tables.xlsx, results.qs2, the TARGET, OSF and protocol texts,
#       the forest data and the survival figure input, with and without the
#       key.

skip_if_not_installed("data.table")
skip_if_not_installed("yaml")
skip_if_not_installed("qs2")

# The fixtures `FF_*` and `.ff_*` are in `helper-feasibility_follow_up.R`.

# ---- (a): the key ---------------------------------------------------------------

test_that("(a) an absent key is fine, and a whole number of weeks reads as an integer", {
  dir <- withr::local_tempdir()
  spec <- suppressWarnings(swereg::tteplan_read_spec(
    .ff_spec_path(file.path(dir, "none"))
  ))
  expect_false("feasibility_follow_up_weeks" %in% names(spec))
  for (raw in c("260", "260.0")) {
    spec <- suppressWarnings(swereg::tteplan_read_spec(
      .ff_spec_path(file.path(dir, raw), raw)
    ))
    expect_identical(spec$feasibility_follow_up_weeks, 260L, info = raw)
  }
  expect_identical(
    nrow(swereg::tteplan_check_spec(.ff_spec_path(
      file.path(dir, "ok"),
      "260"
    ))),
    0L
  )
})

test_that("(a) a value that is not one positive whole number stops, naming the key", {
  dir <- withr::local_tempdir()
  bad <- c(
    "true",
    "'260'",
    ".na",
    "0",
    "-52",
    "26.5",
    ".inf",
    "[52, 104]",
    ""
  )
  for (raw in bad) {
    path <- .ff_spec_path(file.path(dir, make.names(raw)), raw)
    expect_error(
      suppressWarnings(swereg::tteplan_read_spec(path)),
      "feasibility_follow_up_weeks MUST be one positive whole number of weeks",
      fixed = TRUE,
      info = raw
    )
    found <- swereg::tteplan_check_spec(path)
    expect_identical(found$kind, "feasibility_follow_up_weeks", info = raw)
    expect_identical(found$path, "$/feasibility_follow_up_weeks", info = raw)
    expect_match(
      found$message,
      "^feasibility_follow_up_weeks has the value ",
      info = raw
    )
  }
})

test_that("(a) reload_spec refuses a change of the key as structural", {
  dir <- withr::local_tempdir()
  read <- function(raw, name) {
    return(suppressWarnings(swereg::tteplan_read_spec(
      .ff_spec_path(file.path(dir, name), raw)
    )))
  }
  old <- read(FF_W, "old")
  for (new_w in list(312L, NULL)) {
    new <- read(new_w, paste0("new", length(new_w)))
    d <- swereg:::.diff_specs(old, new)
    expect_length(d$cosmetic, 0L)
    expect_identical(
      d$structural,
      paste0(
        "feasibility_follow_up_weeks: 260 -> ",
        if (is.null(new_w)) "NULL" else new_w
      )
    )
  }
  d <- swereg:::.diff_specs(read(NULL, "none"), old)
  expect_identical(d$structural, "feasibility_follow_up_weeks: NULL -> 260")

  # The real method: the plan keeps the old value and records the refusal.
  root <- withr::local_tempdir()
  plan <- .ff_plan(.ff_spec(FF_W), root)
  path <- spr_write_spec(.ff_spec(312L), withr::local_tempdir())
  expect_warning(
    plan$reload_spec(path, quiet = TRUE),
    "feasibility_follow_up_weeks: 260 -> 312",
    fixed = TRUE
  )
  expect_identical(plan$spec$feasibility_follow_up_weeks, FF_W)
  expect_identical(
    plan$spec_reload_skipped_diffs,
    "feasibility_follow_up_weeks: 260 -> 312"
  )
})


# ---- (b): the grid ----------------------------------------------------------------

test_that("(b) the key adds one 260-week ETT per enrollment and outcome, after the spec's own ETTs", {
  with_key <- .ff_plan(.ff_spec(FF_W, n_outcomes = 2L), withr::local_tempdir())
  without <- .ff_plan(.ff_spec(NULL, n_outcomes = 2L), withr::local_tempdir())
  ett <- with_key$ett
  base <- without$ett

  # 2 enrollments x 2 outcomes x 2 follow-ups, then 2 x 2 feasibility ETTs.
  expect_identical(nrow(base), 8L)
  expect_identical(nrow(ett), 12L)
  expect_type(ett$feasibility_horizon, "logical")
  expect_identical(ett$feasibility_horizon, rep(c(FALSE, TRUE), c(8L, 4L)))
  expect_identical(base$feasibility_horizon, rep(FALSE, 8L))

  extra <- ett[ett$feasibility_horizon]
  expect_identical(extra$ett_id, sprintf("ETT%05d", 9:12))
  expect_identical(as.numeric(extra$follow_up), rep(260, 4L))
  pairs <- paste(extra$enrollment_id, extra$outcome_var)
  expect_identical(
    sort(pairs),
    sort(unique(paste(base$enrollment_id, base$outcome_var)))
  )
  expect_identical(
    extra$file_raw,
    base$file_raw[match(
      extra$enrollment_id,
      base$enrollment_id
    )]
  )
  expect_identical(
    extra$treatment_impl,
    base$treatment_impl[match(extra$enrollment_id, base$enrollment_id)]
  )

  # The spec's own ETTs keep the rows, ids and files of the build without
  # the key.
  expect_identical(ett[!ett$feasibility_horizon], base)
  expect_identical(swereg:::.plan_ett_reported(with_key), base)
  expect_identical(
    swereg:::.plan_ett_feasibility_ids(with_key),
    sprintf("ETT%05d", 9:12)
  )

  # The follow-up grid carries the label of the feasibility horizon.
  grid <- swereg:::.tte_follow_up_grid(with_key$spec)
  expect_identical(
    grid[[3L]],
    list(
      label = "5 years",
      weeks = 260L,
      feasibility_horizon = TRUE
    )
  )
  odd <- swereg:::.tte_follow_up_grid(utils::modifyList(
    with_key$spec,
    list(feasibility_follow_up_weeks = 130L)
  ))
  expect_identical(odd[[3L]]$label, "130 weeks")
})

test_that("(b) an absent key or a value of at most 104 weeks adds no ETT", {
  base <- .ff_plan(.ff_spec(NULL), withr::local_tempdir())$ett
  expect_identical(base$feasibility_horizon, rep(FALSE, nrow(base)))
  for (w in c(104L, 100L, 52L)) {
    plan <- .ff_plan(.ff_spec(w), withr::local_tempdir())
    expect_identical(plan$ett, base, info = w)
  }
})

test_that("(b) a plan saved without the column reads FALSE on load", {
  plan <- .ff_plan(.ff_spec(NULL), withr::local_tempdir())
  ett <- data.table::copy(plan$ett)
  ett[, feasibility_horizon := NULL]
  plan$ett <- ett
  loaded <- swereg::tteplan_load(plan$save())
  expect_identical(loaded$ett$feasibility_horizon, rep(FALSE, nrow(ett)))
  expect_identical(
    loaded$ett[,
      setdiff(names(loaded$ett), "feasibility_horizon"),
      with = FALSE
    ],
    ett
  )
})


# The slots of a stored ETT result that hold an estimate.
FF_ESTIMATE_SLOT <- "^(summary|rates_|irr_|rd_|subgroup_|emtest_)"


# ---- (c): after s3 --------------------------------------------------------------------

test_that("(c) each 260-week ETT holds a PP and an ITT horizon record and no estimate", {
  .ff_skip()
  fx <- .ff_fixture(TRUE)
  plan <- fx$plan
  feas <- swereg:::.plan_ett_feasibility_ids(plan)
  expect_identical(feas, c("ETT00005", "ETT00006"))
  expect_true(all(feas %in% names(plan$results_ett)))
  for (id in feas) {
    res <- plan$results_ett[[id]]
    expect_identical(
      grep(FF_ESTIMATE_SLOT, names(res), value = TRUE),
      character(0),
      info = id
    )
    pp <- res$horizon_pp
    itt <- res$horizon_itt
    expect_type(pp$withheld, "logical")
    expect_identical(pp$follow_up, 260, info = id)
    expect_identical(pp$weight_col, "analysis_weight_pp_trunc", info = id)
    expect_identical(itt$weight_col, "ipw_trunc", info = id)
    expect_identical(max(pp$windows$window_hi), 260, info = id)
    expect_identical(itt$decided_by, "pp", info = id)
    for (f in c("withheld", "reportable_weeks", "status", "max_passing_cut")) {
      expect_identical(itt[[f]], pp[[f]], info = paste(id, f))
    }
  }
  # The thin intervention tail fails a year after the second.
  expect_identical(plan$results_ett$ETT00005$horizon_pp$withheld, TRUE)
  expect_gte(plan$results_ett$ETT00005$horizon_pp$reportable_weeks, 104)
  expect_lt(plan$results_ett$ETT00005$horizon_pp$reportable_weeks, 260)

  # The reported ETTs hold their estimates.
  for (id in sprintf("ETT%05d", 1:4)) {
    res <- plan$results_ett[[id]]
    expect_true(all(
      c("irr_pp_trunc", "irr_itt", "rd_pp_trunc") %in% names(res)
    ))
  }

  # Two horizon items per feasibility-horizon ETT, and no other item.
  calls <- function(out) {
    hit <- grep("^Analyzing: ", out, value = TRUE)
    return(as.integer(sub(".*\\((\\d+) analysis calls.*", "\\1", hit)))
  }
  expect_identical(
    calls(fx$out) - calls(.ff_fixture(FALSE)$out),
    2L * length(feas)
  )
})

test_that("(c) no public accessor returns a row for a 260-week ETT", {
  .ff_skip()
  plan <- .ff_fixture(TRUE)$plan
  feas <- swereg:::.plan_ett_feasibility_ids(plan)
  per_ett <- list(
    get_estimates = plan$get_estimates,
    get_curves = plan$get_curves,
    get_subgroups = plan$get_subgroups,
    get_horizon_windows = plan$get_horizon_windows
  )
  for (nm in names(per_ett)) {
    tab <- per_ett[[nm]]()
    expect_gt(nrow(tab), 0L)
    expect_false(any(tab$ett_id %in% feas), info = nm)
    expect_true(all(sprintf("ETT%05d", 1:4) %in% tab$ett_id), info = nm)
  }
  per_enrollment <- list(
    get_baselines = plan$get_baselines,
    get_attrition = plan$get_attrition,
    get_matching = plan$get_matching
  )
  for (nm in names(per_enrollment)) {
    tab <- per_enrollment[[nm]]()
    expect_false(
      "ett_id" %in% names(tab) && any(tab$ett_id %in% feas),
      info = nm
    )
  }
  expect_error(plan$get_protocol_table(feas[1L]))
  summary_out <- utils::capture.output(plan$results_summary())
  expect_false(any(grepl(paste(feas, collapse = "|"), summary_out)))
  expect_true(any(grepl("ETT00004", summary_out)))
  print_out <- utils::capture.output(print(plan))
  expect_false(any(grepl(paste(feas, collapse = "|"), print_out)))
})


# ---- (d): parity ----------------------------------------------------------------------

test_that("(d) every sheet of tables.xlsx but the two Minimum events sheets is identical cell by cell", {
  .ff_skip()
  a <- .ff_fixture(TRUE)
  b <- .ff_fixture(FALSE)
  sheets <- openxlsx::getSheetNames(a$xlsx)
  expect_identical(sheets, openxlsx::getSheetNames(b$xlsx))
  expect_true(all(FF_MIN_SHEETS %in% sheets))
  expect_true(all(
    c("PP results", "ITT results", "Summary", "ETTs") %in% sheets
  ))
  # Every cell of every other sheet, Provenance included. The fixture pins the
  # export time, so no cell is replaced before the comparison.
  for (sh in setdiff(sheets, FF_MIN_SHEETS)) {
    expect_identical(.ff_cells(a$xlsx, sh), .ff_cells(b$xlsx, sh), info = sh)
  }
  prov <- .ff_cells(a$xlsx, "Provenance")
  expect_identical(
    prov[[2L]][prov[[1L]] == "Exported at"],
    format(FF_EXPORT_TIME, "%Y-%m-%d %H:%M:%S")
  )
  for (sh in openxlsx::getSheetNames(a$spec_xlsx)) {
    expect_identical(
      .ff_cells(a$spec_xlsx, sh),
      .ff_cells(b$spec_xlsx, sh),
      info = paste("spec workbook", sh)
    )
  }
})

test_that("(d) only the Minimum events sheets show the 260-week ETT", {
  .ff_skip()
  a <- .ff_fixture(TRUE)
  b <- .ff_fixture(FALSE)
  for (sh in FF_MIN_SHEETS) {
    ca <- .ff_cells(a$xlsx, sh)
    cb <- .ff_cells(b$xlsx, sh)
    # Since 27.3.1 the 260-week ETT gives the columns of years 3 to 5.
    for (yr in paste(3:5, "years")) {
      expect_true(any(ca == yr, na.rm = TRUE), info = paste(sh, yr))
      expect_false(any(cb == yr, na.rm = TRUE), info = paste(sh, yr))
    }
    expect_false(any(ca == "260 weeks", na.rm = TRUE), info = sh)
  }
})

test_that("(d) results.qs2, the TARGET checklist and the protocol tables are identical", {
  .ff_skip()
  a <- .ff_fixture(TRUE)
  b <- .ff_fixture(FALSE)
  ra <- qs2::qs_read(file.path(dirname(a$xlsx), "results.qs2"))
  rb <- qs2::qs_read(file.path(dirname(b$xlsx), "results.qs2"))
  expect_identical(names(ra), names(rb))
  for (nm in names(ra)) {
    expect_identical(ra[[nm]], rb[[nm]], info = nm)
  }
  expect_identical(
    utils::capture.output(a$plan$print_target_checklist()),
    utils::capture.output(b$plan$print_target_checklist())
  )
  for (id in c(list(NULL), as.list(sprintf("ETT%05d", 1:4)))) {
    expect_identical(
      a$plan$get_protocol_table(id),
      b$plan$get_protocol_table(id),
      info = if (is.null(id)) "default" else id
    )
  }
  expect_identical(
    utils::capture.output(print(a$plan)),
    utils::capture.output(print(b$plan))
  )
})

test_that("(d) the OSF text of the spec with the key equals the text without it", {
  dir <- withr::local_tempdir()
  with_key <- suppressWarnings(swereg::tteplan_osf_text(
    .ff_spec_path(file.path(dir, "key"), FF_W)
  ))
  without <- suppressWarnings(swereg::tteplan_osf_text(
    .ff_spec_path(file.path(dir, "none"))
  ))
  expect_gt(length(with_key), 0L)
  expect_identical(with_key, without)
})

test_that("(d) the forest data and the survival figure input are identical", {
  .ff_skip()
  a <- .ff_fixture(TRUE)
  b <- .ff_fixture(FALSE)
  manifest <- list(
    list(
      type = "forest",
      label = "forest",
      exposures = list(Treated = sprintf("ETT%05d", 1:4)),
      estimands = c("pp", "itt"),
      risk_difference = TRUE
    ),
    list(
      type = "survival",
      label = "surv",
      enrollment = "01",
      outcome = "osd_a",
      follow_up = 104L,
      age_group = "40_80",
      estimands = c("pp", "itt")
    )
  )
  seen <- function(fx) {
    forest <- list()
    surv <- list()
    real_forest <- swereg:::.render_combined_forest_plot
    real_surv <- swereg:::.render_survival_curve
    testthat::local_mocked_bindings(
      .render_combined_forest_plot = function(...) {
        res <- real_forest(...)
        forest[[length(forest) + 1L]] <<- res$text
        return(res)
      },
      .render_survival_curve = function(...) {
        surv[[length(surv) + 1L]] <<- list(...)
        return(real_surv(...))
      },
      .package = "swereg"
    )
    paths <- suppressWarnings(fx$plan$export(
      manifest,
      dir = file.path(fx$root, "exhibits")
    ))
    return(list(forest = forest, surv = surv, n = length(paths)))
  }
  sa <- seen(a)
  sb <- seen(b)
  expect_identical(sa$n, 4L)
  expect_length(sa$forest, 2L)
  expect_length(sa$surv, 2L)
  expect_identical(sa$forest, sb$forest)
  for (k in seq_along(sa$surv)) {
    for (nm in names(sa$surv[[k]])) {
      expect_identical(
        sa$surv[[k]][[nm]],
        sb$surv[[k]][[nm]],
        info = paste(k, nm)
      )
    }
  }
  expect_identical(
    swereg:::.build_forest_df(a$plan),
    swereg:::.build_forest_df(b$plan)
  )

  # The exhibits refuse a 260-week ETT as an ETT they do not hold.
  expect_error(
    a$plan$export(
      list(list(
        type = "forest",
        label = "forest260",
        exposures = list(Treated = "ETT00005"),
        estimands = "pp"
      )),
      dir = file.path(a$root, "refused")
    ),
    "unknown ETT ids: ETT00005",
    fixed = TRUE
  )
  surv260 <- manifest[[2L]]
  surv260$follow_up <- FF_W
  expect_error(
    a$plan$export(list(surv260), dir = file.path(a$root, "refused")),
    "survival figure needs exactly 1 matching ETT, found 0",
    fixed = TRUE
  )
})


# ---- (e): parity through the real s1 -> s2 -> s3 -------------------------------

# The fixture of (c) and (d) writes the analysis files by hand, so it never
# reaches s1. Here the real stages run on one skeleton, with and without the
# key. With the key, s1 enrolls to 260 weeks. s1e MUST still truncate `ipw`
# and count the fill over the rows the reported ETTs reach. One enrollment,
# the Age confounder and follow-ups 52 and 104. Seed 5 is chosen because the
# 1st percentile of `ipw` over every row of its 260-week panel (0.6277904771)
# differs from the one over the rows up to week 104 (0.6257526794). Seeds 1
# to 4 and 6 to 10 give the same bounds both ways, so they cannot show a
# truncation over the wrong rows.
.ff_real_cache <- new.env()
.ff_real <- function(key = TRUE) {
  name <- if (key) "key" else "none"
  if (!is.null(.ff_real_cache[[name]])) {
    return(.ff_real_cache[[name]])
  }
  root <- tempfile(paste0("ff_real_", name, "_"))
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = testthat::teardown_env())
  dirs <- file.path(root, c("spec", "tteplan", "results", "meta"))
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  sk <- ttm_skeleton(
    scenario = "A",
    n_persons = 200L,
    date_min = "2017-01-05",
    date_max = "2022-06-30",
    n_init_periods = 32L,
    seed = 5L
  )
  skel_path <- file.path(dirs[2], SPR_SKELETON)
  qs2::qs_save(sk, skel_path)
  spec <- spr_spec_list()
  spec$confounders <- spec$confounders[1L]
  spec$enrollments <- spec$enrollments[1L]
  spec$follow_up <- FF_FOLLOW_UP
  if (key) {
    spec$feasibility_follow_up_weeks <- FF_W
  }
  spr_write_spec(spec, dirs[1])
  plan <- suppressMessages(swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs[4]),
    candidate_dir_spec = dirs[1],
    candidate_dir_tteplan = dirs[2],
    candidate_dir_results = dirs[3],
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  ))
  dev <- swereg:::.swereg_dev_path()
  suppressWarnings(invisible(utils::capture.output(
    {
      plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = dev,
        check_skeletons = FALSE
      )
      plan$s2_generate_analysis_files_and_ipcw_pp(
        n_workers = 2L,
        swereg_dev_path = dev
      )
      plan$s3_analyze(n_workers = 2L, swereg_dev_path = dev)
    },
    type = "output"
  )))
  imp <- swereg::qs2_read(file.path(dirs[2], plan$ett$file_imp[1L]))
  .ff_real_cache[[name]] <- list(
    plan = plan,
    imp = imp,
    ipw_trunc_range = range(imp$data$ipw_trunc)
  )
  return(.ff_real_cache[[name]])
}

test_that("(e) through the real s1, the key changes no reported estimate, weight bound or fill count", {
  .ff_skip()
  a <- .ff_real(TRUE)
  b <- .ff_real(FALSE)
  # The key reaches s1: the panel with the key runs past 104 weeks.
  expect_gt(max(a$imp$data$tstop), 104)
  expect_identical(max(b$imp$data$tstop), 104L)
  expect_gt(nrow(a$imp$data), nrow(b$imp$data))
  # Over every row of the 260-week panel, the 1st percentile of `ipw` is
  # another number. So a truncation over every row moves the bounds.
  q_all <- stats::quantile(a$imp$data$ipw, 0.01, names = FALSE)
  q_rep <- stats::quantile(
    a$imp$data$ipw[a$imp$data$tstop <= 104],
    0.01,
    names = FALSE
  )
  expect_false(isTRUE(all.equal(q_all, q_rep)))

  est_a <- a$plan$get_estimates()
  est_b <- b$plan$get_estimates()
  expect_identical(nrow(est_a), 6L)
  expect_identical(est_a, est_b)
  expect_identical(a$ipw_trunc_range, b$ipw_trunc_range)
  expect_identical(a$imp$fill_summary, b$imp$fill_summary)
  expect_identical(
    a$plan$results_enrollment[["01"]]$fill_summary,
    b$plan$results_enrollment[["01"]]$fill_summary
  )
  expect_identical(
    a$plan$get_baselines(),
    b$plan$get_baselines()
  )
})
