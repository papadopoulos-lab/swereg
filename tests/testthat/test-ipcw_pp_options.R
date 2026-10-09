# The censoring-model settings that item 6h of the TARGET checklist states.
#
# s2 records `estimate_ipcw_pp_with_gam` and
# `estimate_ipcw_pp_separately_by_treatment` on each per-protocol analysis
# enrollment, because `tte_stage()` does not save the plan after s2. s3 reads
# them off the analysis file into `results_ett[[ett_id]]$ipcw_pp_options`, and
# item 6h is generated from those stored values. A result without them came
# from s2 before 27.2.0, and item 6h says so instead of naming the default.
#
# The 6h tests read the GENERATED checklist text. The last test runs s1, s2
# and s3 through the real workers with non-default settings.

skip_if_not_installed("data.table")
skip_if_not_installed("withr")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")

.ipo_running <- function() {
  return(as.character(utils::packageVersion("swereg")))
}

.ipo_opts <- function(gam, by_arm) {
  return(list(
    estimate_ipcw_pp_with_gam = gam,
    estimate_ipcw_pp_separately_by_treatment = by_arm
  ))
}

# A plan from a written specification with two ETTs: one outcome and two
# follow-up horizons. It runs no stage.
.ipo_spec_plan <- function(dir, prefix = "ipo") {
  dirs <- list(
    spec = file.path(dir, "spec"),
    tteplan = file.path(dir, "tteplan"),
    results = file.path(dir, "results"),
    meta = file.path(dir, "meta")
  )
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  sk <- ttm_skeleton(
    "A",
    n_persons = 20L,
    date_max = "2016-12-31",
    n_init_periods = 4L
  )
  skel <- file.path(dirs$tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel)
  spec_path <- file.path(dirs$spec, "spec_v001.yaml")
  ttm_write_spec(spec_path, prefix, "ri_highrisk")
  spec <- yaml::read_yaml(spec_path)
  spec$follow_up <- list(
    list(label = "1 year", weeks = 52L),
    list(label = "2 years", weeks = 104L)
  )
  yaml::write_yaml(spec, spec_path)
  return(swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel, data_meta_dir = dirs$meta),
    candidate_dir_spec = dirs$spec,
    candidate_dir_tteplan = dirs$tteplan,
    candidate_dir_results = dirs$results,
    spec_version = "v001",
    global_max_isoyearweek = max(sk$isoyearweek, na.rm = TRUE),
    check_skeletons = FALSE
  ))
}

# The dispatcher stand-in. A worker on a per-protocol analysis file returns
# the settings of that ETT, as `.s3_ett_worker()` reads them off the file. A
# worker on an ITT file returns no settings. `opts[[ett_id]]` is `NULL` for an
# analysis file written before 27.2.0.
.ipo_fake_batch_run <- function(opts) {
  return(function(target, items, n_workers, ...) {
    out <- lapply(items, function(it) {
      if (!identical(target$symbol, ".s3_ett_worker")) {
        return(list(n_baseline = 100L, swereg_version_s2 = .ipo_running()))
      }
      res <- list(summary = "SUMMARY", swereg_version_s2 = .ipo_running())
      if (!grepl("_analysis_itt_", basename(it$analysis_path), fixed = TRUE)) {
        res$ipcw_pp_options <- opts[[it$ett_id]]
      }
      return(res)
    })
    names(out) <- names(items)
    return(out)
  })
}

.ipo_run_s3 <- function(plan, opts) {
  testthat::local_mocked_bindings(
    .batch_run = .ipo_fake_batch_run(opts),
    .package = "swereg"
  )
  utils::capture.output(
    plan$s3_analyze(output_dir = withr::local_tempdir(), n_workers = 1L)
  )
  return(invisible(plan))
}

# Item 6h of the printed checklist, from its title line to the next item.
.ipo_6h <- function(plan) {
  lines <- utils::capture.output(plan$print_target_checklist())
  i <- grep("Item 6h\\. ", lines)[1]
  j <- grep("Item 7a-h\\. ", lines)[1]
  return(paste(lines[i:(j - 1L)], collapse = "\n"))
}

# The settings sentences of item 6h: the sentence after "three factors." and
# the sentence after "a person-time offset.".
.ipo_settings <- function(it6h) {
  by_arm <- regmatches(
    it6h,
    regexpr("(?<=three factors\\. ).*?(?= The first factor)", it6h, perl = TRUE)
  )
  gam <- regmatches(
    it6h,
    regexpr("(?<=person-time offset\\. ).*?(?= Each included)", it6h, perl = TRUE)
  )
  return(c(by_arm = by_arm, gam = gam))
}

.ipo_cache <- new.env(parent = emptyenv())
.ipo_plan <- function() {
  if (is.null(.ipo_cache$plan)) {
    dir <- withr::local_tempdir(.local_envir = teardown_env())
    .ipo_cache$plan <- .ipo_spec_plan(dir)
  }
  return(.ipo_cache$plan)
}


test_that("TTEEnrollment declares ipcw_pp_options and it starts NULL", {
  expect_true("ipcw_pp_options" %in% names(TTEEnrollment$public_fields))
  expect_null(TTEEnrollment$public_fields$ipcw_pp_options)
})

test_that("the s2 worker records the settings on a per-protocol file only, and the s3 worker returns them", {
  skip_on_cran()
  skip_if_not_installed("survey")

  dt <- tte_simulate(N = 3000, persist_coef = 4, seed = 7)
  long <- tte_build_long(dt)
  trial <- TTEEnrollment$new(long, tte_make_design(long))
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  imp <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(trial, imp, nthreads = 1L)

  s2 <- function(estimand) {
    return(swereg:::.s2_worker(
      outcome = "event",
      follow_up = max(long$tstop),
      file_imp_path = imp,
      n_threads = 1L,
      sep_by_tx = FALSE,
      with_gam = FALSE,
      estimand = estimand
    )$analysis)
  }
  pp <- s2("pp")
  itt <- s2("itt")
  expect_identical(pp$ipcw_pp_options, .ipo_opts(FALSE, FALSE))
  expect_null(itt$ipcw_pp_options)

  pp_path <- withr::local_tempfile(fileext = ".qs2")
  itt_path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(pp, pp_path, nthreads = 1L)
  qs2::qs_save(itt, itt_path, nthreads = 1L)
  s3 <- function(path, weight_col) {
    return(swereg:::.s3_ett_worker(
      analysis_path = path,
      method = "irr",
      weight_col = weight_col,
      ett_id = "ETT00001",
      n_threads = 1L
    ))
  }
  res_pp <- s3(pp_path, "analysis_weight_pp_trunc")
  expect_identical(res_pp$ipcw_pp_options, .ipo_opts(FALSE, FALSE))
  # An ITT file adds no key, so it cannot overwrite the per-protocol value
  # when s3 merges both returns into one result.
  res_itt <- s3(itt_path, "ipw_trunc")
  expect_false("ipcw_pp_options" %in% names(res_itt))
})

test_that("s3 stores the settings of each ETT, and an ITT return does not overwrite them", {
  plan <- .ipo_plan()
  eids <- plan$ett$ett_id
  expect_length(eids, 2L)
  opts <- stats::setNames(
    list(.ipo_opts(FALSE, TRUE), .ipo_opts(TRUE, FALSE)),
    eids
  )
  .ipo_run_s3(plan, opts)
  for (eid in eids) {
    expect_identical(plan$results_ett[[eid]]$ipcw_pp_options, opts[[eid]])
  }
})

test_that("item 6h states the default settings that s2 recorded", {
  plan <- .ipo_plan()
  eids <- plan$ett$ett_id
  .ipo_run_s3(
    plan,
    stats::setNames(list(.ipo_opts(TRUE, TRUE), .ipo_opts(TRUE, TRUE)), eids)
  )
  s <- .ipo_settings(.ipo_6h(plan))
  expect_identical(
    s[["by_arm"]],
    "Each factor was fitted separately in each arm (estimate_ipcw_pp_separately_by_treatment = TRUE)."
  )
  expect_identical(
    s[["gam"]],
    "The loss and deviation models were generalized additive models (estimate_ipcw_pp_with_gam = TRUE)."
  )
})

test_that("item 6h states non-default settings that s2 recorded", {
  plan <- .ipo_plan()
  eids <- plan$ett$ett_id
  .ipo_run_s3(
    plan,
    stats::setNames(list(.ipo_opts(FALSE, FALSE), .ipo_opts(FALSE, FALSE)), eids)
  )
  s <- .ipo_settings(.ipo_6h(plan))
  expect_identical(
    s[["by_arm"]],
    "Each factor was fitted once on both arms pooled (estimate_ipcw_pp_separately_by_treatment = FALSE)."
  )
  expect_identical(
    s[["gam"]],
    "The loss and deviation models were generalized linear models (estimate_ipcw_pp_with_gam = FALSE)."
  )
})

test_that("item 6h names the ETTs of each setting when the ETTs differ", {
  plan <- .ipo_plan()
  eids <- plan$ett$ett_id
  .ipo_run_s3(
    plan,
    stats::setNames(list(.ipo_opts(TRUE, TRUE), .ipo_opts(FALSE, FALSE)), eids)
  )
  s <- .ipo_settings(.ipo_6h(plan))
  expect_identical(
    s[["by_arm"]],
    paste0(
      "Each factor was fitted separately in each arm ",
      "(estimate_ipcw_pp_separately_by_treatment = TRUE) for ETT ",
      eids[1],
      ", and once on both arms pooled ",
      "(estimate_ipcw_pp_separately_by_treatment = FALSE) for ETT ",
      eids[2],
      "."
    )
  )
  expect_identical(
    s[["gam"]],
    paste0(
      "The loss and deviation models were generalized additive models ",
      "(estimate_ipcw_pp_with_gam = TRUE) for ETT ",
      eids[1],
      ", and generalized linear models (estimate_ipcw_pp_with_gam = FALSE) ",
      "for ETT ",
      eids[2],
      "."
    )
  )
})

test_that("item 6h names the ETTs without a record beside the ETTs with one", {
  plan <- .ipo_plan()
  eids <- plan$ett$ett_id
  # The second ETT's analysis file came from s2 before 27.2.0.
  .ipo_run_s3(plan, stats::setNames(list(.ipo_opts(FALSE, TRUE)), eids[1]))
  s <- .ipo_settings(.ipo_6h(plan))
  expect_identical(
    s[["by_arm"]],
    paste0(
      "Each factor was fitted separately in each arm ",
      "(estimate_ipcw_pp_separately_by_treatment = TRUE) for ETT ",
      eids[1],
      ". Whether each factor was fitted separately in each arm ",
      "(estimate_ipcw_pp_separately_by_treatment) was not recorded ",
      "(computed before swereg 27.2.0) for ETT ",
      eids[2],
      "."
    )
  )
  expect_identical(
    s[["gam"]],
    paste0(
      "The loss and deviation models were generalized linear models ",
      "(estimate_ipcw_pp_with_gam = FALSE) for ETT ",
      eids[1],
      ". Whether the loss and deviation models were generalized additive ",
      "models (estimate_ipcw_pp_with_gam) was not recorded ",
      "(computed before swereg 27.2.0) for ETT ",
      eids[2],
      "."
    )
  )
})

test_that("item 6h says a plan computed before 27.2.0 recorded no settings", {
  plan <- .ipo_plan()
  .ipo_run_s3(plan, list())
  it6h <- .ipo_6h(plan)
  s <- .ipo_settings(it6h)
  expect_identical(
    s[["by_arm"]],
    paste0(
      "Whether each factor was fitted separately in each arm ",
      "(estimate_ipcw_pp_separately_by_treatment) was not recorded ",
      "(computed before swereg 27.2.0)."
    )
  )
  expect_identical(
    s[["gam"]],
    paste0(
      "Whether the loss and deviation models were generalized additive ",
      "models (estimate_ipcw_pp_with_gam) was not recorded ",
      "(computed before swereg 27.2.0)."
    )
  )
  # The default MUST NOT stand in for a value nobody recorded.
  expect_false(grepl("= TRUE", it6h, fixed = TRUE))
})

test_that("item 6h states no settings before s3 stores a result", {
  plan <- .ipo_plan()
  plan$results_ett <- list()
  it6h <- .ipo_6h(plan)
  expect_match(
    it6h,
    "No ETT holds a stored s3 result, so this item does not state the settings of the censoring models.",
    fixed = TRUE
  )
  expect_false(grepl("not recorded", it6h, fixed = TRUE))
})

test_that("real s2 and s3 workers carry non-default settings into item 6h", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(dev_tree, "R", "batch_adapter.R")),
    "package source tree not available"
  )

  sk <- ttm_skeleton("A", n_persons = 2500L, seed = 2026L)
  root <- withr::local_tempdir()
  dirs <- list(
    spec = file.path(root, "spec"),
    tteplan = file.path(root, "tteplan"),
    results = file.path(root, "results"),
    meta = file.path(root, "meta")
  )
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  skel_path <- file.path(dirs$tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dirs$spec, "spec_v001.yaml"),
    "ttmipo",
    "rd_age_continuous"
  )
  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs$meta),
    candidate_dir_spec = dirs$spec,
    candidate_dir_tteplan = dirs$tteplan,
    candidate_dir_results = dirs$results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  )
  dev_path <- ttm_dev_path()
  invisible(utils::capture.output(
    {
      plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = dev_path,
        check_skeletons = FALSE
      )
      plan$s2_generate_analysis_files_and_ipcw_pp(
        estimate_ipcw_pp_separately_by_treatment = FALSE,
        estimate_ipcw_pp_with_gam = FALSE,
        n_workers = 1L,
        swereg_dev_path = dev_path
      )
      plan$s3_analyze(n_workers = 1L, swereg_dev_path = dev_path)
    },
    type = "output"
  ))

  expect_gt(length(plan$results_ett), 0L)
  for (r in plan$results_ett) {
    expect_identical(r$ipcw_pp_options, .ipo_opts(FALSE, FALSE))
  }
  s <- .ipo_settings(.ipo_6h(plan))
  expect_identical(
    s[["by_arm"]],
    "Each factor was fitted once on both arms pooled (estimate_ipcw_pp_separately_by_treatment = FALSE)."
  )
  expect_identical(
    s[["gam"]],
    "The loss and deviation models were generalized linear models (estimate_ipcw_pp_with_gam = FALSE)."
  )
})
