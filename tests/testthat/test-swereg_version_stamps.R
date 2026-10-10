# =============================================================================
# Every stored s3 result names the swereg versions that computed it
# =============================================================================
#
# s3 stamps `swereg_version` (the version that ran s3) and `swereg_version_s2`
# (the version that ran s2 on the analysis file) inside every entry of
# `results_enrollment` and `results_ett`. s3 calls the methods of the
# serialised analysis enrollment, so an analysis file from another release
# computes with that release's estimators. s3 warns when that happens, the
# Provenance sheet prints both versions, and the export warns when either
# differs from the exporting version.
#
# The dispatcher is mocked in the first tests, so they drive the driver's own
# store path on crafted worker returns. The last test runs s1, s2 and s3 through
# the real workers, so the stamp crosses the subprocess boundary.

skip_if_not_installed("data.table")
skip_if_not_installed("withr")
skip_if_not_installed("qs2")

.sv_running <- function() {
  return(as.character(utils::packageVersion("swereg")))
}

.sv_plan <- function() {
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = c("ETT00001", "ETT00002"),
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    follow_up = 52L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = c("analysis_001.qs2", "analysis_002.qs2"),
    description = c("ETT00001: A", "ETT00002: A")
  )
  return(swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  ))
}

# The dispatcher stand-in. Every worker return carries `s2_version` as its
# `swereg_version_s2`, which is what the real workers read off the analysis
# file. `NA` is what they return for an analysis file without the field.
.sv_fake_batch_run <- function(s2_version) {
  return(function(target, items, n_workers, ...) {
    out <- lapply(items, function(it) {
      res <- if (identical(target$symbol, ".s3_ett_worker")) {
        list(summary = "SUMMARY")
      } else {
        list(n_baseline = 100L)
      }
      res$swereg_version_s2 <- s2_version
      return(res)
    })
    names(out) <- names(items)
    return(out)
  })
}

.sv_run_s3 <- function(plan, s2_version) {
  testthat::local_mocked_bindings(
    .batch_run = .sv_fake_batch_run(s2_version),
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()
  utils::capture.output(
    plan$s3_analyze(output_dir = output_dir, n_workers = 1L)
  )
  return(invisible(plan))
}

# The Provenance sheet, read back as a named vector of Value by Item.
.sv_provenance <- function(plan) {
  wb <- openxlsx::createWorkbook()
  swereg:::.write_provenance(wb, plan)
  path <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, path)
  d <- openxlsx::read.xlsx(path, sheet = "Provenance", colNames = FALSE)
  return(stats::setNames(d[[2L]], d[[1L]]))
}


test_that("TTEEnrollment declares swereg_version_s2 and it starts NULL", {
  expect_true("swereg_version_s2" %in% names(TTEEnrollment$public_fields))
  expect_null(TTEEnrollment$public_fields$swereg_version_s2)
})

test_that("s3 stamps both versions on every stored result", {
  plan <- .sv_plan()
  .sv_run_s3(plan, .sv_running())

  # Read the stamp with `[[`. `$swereg_version` partially matches
  # `swereg_version_s2` when the stamp is absent, and then reads the s2 value.
  for (eid in c("ETT00001", "ETT00002")) {
    expect_identical(plan$results_ett[[eid]][["swereg_version"]], .sv_running())
    expect_identical(plan$results_ett[[eid]]$swereg_version_s2, .sv_running())
  }
  expect_identical(
    plan$results_enrollment[["01"]][["swereg_version"]],
    .sv_running()
  )
  expect_identical(
    plan$results_enrollment[["01"]]$swereg_version_s2,
    .sv_running()
  )
})

test_that("s3 warns when the analysis file was written by another s2 version", {
  plan <- .sv_plan()
  expect_warning(
    .sv_run_s3(plan, "27.1.0"),
    paste0("s3 ran swereg ", .sv_running(), ".*swereg 27\\.1\\.0")
  )
  expect_identical(plan$results_ett[["ETT00001"]]$swereg_version_s2, "27.1.0")

  # An analysis file without the field names no version. s3 warns and calls
  # the version unknown.
  plan <- .sv_plan()
  expect_warning(.sv_run_s3(plan, NA_character_), "wrote with swereg unknown")
  expect_identical(
    plan$results_ett[["ETT00001"]]$swereg_version_s2,
    NA_character_
  )

  # A matching s2 version raises no warning.
  plan <- .sv_plan()
  expect_no_warning(.sv_run_s3(plan, .sv_running()))
})

test_that("the stamps survive a save and load round trip", {
  plan <- .sv_plan()
  suppressWarnings(.sv_run_s3(plan, "27.1.0"))
  dir <- withr::local_tempdir()
  plan$save(dir = dir)

  reloaded <- swereg::tteplan_locate_and_load(dir)
  expect_identical(
    reloaded$results_ett[["ETT00002"]][["swereg_version"]],
    .sv_running()
  )
  expect_identical(
    reloaded$results_ett[["ETT00002"]]$swereg_version_s2,
    "27.1.0"
  )
  expect_identical(
    reloaded$results_enrollment[["01"]][["swereg_version"]],
    .sv_running()
  )
  expect_identical(
    reloaded$results_enrollment[["01"]]$swereg_version_s2,
    "27.1.0"
  )
})

test_that("the Provenance sheet shows the versions that s3 stamped", {
  skip_if_not_installed("openxlsx")
  plan <- .sv_plan()
  suppressWarnings(.sv_run_s3(plan, "27.1.0"))

  prov <- .sv_provenance(plan)
  expect_identical(unname(prov["swereg version (export)"]), .sv_running())
  expect_identical(unname(prov["swereg version (s2)"]), "27.1.0")
  expect_identical(unname(prov["swereg version (s3)"]), .sv_running())
  expect_false("swereg version" %in% names(prov))

  # A plan computed before the stamps existed reads `unknown`.
  plan$results_enrollment[["01"]][["swereg_version"]] <- NULL
  plan$results_enrollment[["01"]]$swereg_version_s2 <- NULL
  for (eid in names(plan$results_ett)) {
    plan$results_ett[[eid]][["swereg_version"]] <- NULL
    plan$results_ett[[eid]]$swereg_version_s2 <- NULL
  }
  prov <- .sv_provenance(plan)
  expect_identical(unname(prov["swereg version (s2)"]), "unknown")
  expect_identical(unname(prov["swereg version (s3)"]), "unknown")
})

test_that("export warns once and the sheet shows a fixture stamped by an old version", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  plan <- .xp_plan("new", subgroups = FALSE)
  # Two s3 versions, so the sheet MUST show the sorted unique set. 27.10.0
  # sorts after 27.9.0 as a version and before it as a string.
  eids <- names(plan$results_ett)
  for (i in seq_along(eids)) {
    plan$results_ett[[eids[i]]][["swereg_version"]] <-
      if (i %% 2L == 0L) "27.10.0" else "27.9.0"
    plan$results_ett[[eids[i]]]$swereg_version_s2 <- "26.1.0"
  }
  # Every result carries a stamp, so no cell adds `unknown`.
  for (eid in names(plan$results_enrollment)) {
    plan$results_enrollment[[eid]][["swereg_version"]] <- "27.9.0"
    plan$results_enrollment[[eid]]$swereg_version_s2 <- "26.1.0"
  }

  path <- withr::local_tempfile(fileext = ".xlsx")
  ws <- testthat::capture_warnings(suppressMessages(
    plan$export_tables(path = path)
  ))
  hit <- grep("other versions computed the stored results", ws, value = TRUE)
  expect_length(hit, 1L)
  expect_match(hit, "s2: 26.1.0. s3: 27.9.0, 27.10.0. export: ", fixed = TRUE)
  expect_match(hit, paste0("export: ", .sv_running(), "."), fixed = TRUE)

  d <- openxlsx::read.xlsx(path, sheet = "Provenance", colNames = FALSE)
  prov <- stats::setNames(d[[2L]], d[[1L]])
  expect_identical(unname(prov["swereg version (s2)"]), "26.1.0")
  expect_identical(unname(prov["swereg version (s3)"]), "27.9.0, 27.10.0")

  # `$export()` warns through the same rule. The bad spec stops it after the
  # warning, which keeps the test free of figure rendering.
  ws <- testthat::capture_warnings(try(
    plan$export(list(list(type = "not_a_type")), dir = withr::local_tempdir()),
    silent = TRUE
  ))
  expect_length(
    grep("other versions computed the stored results", ws),
    1L
  )
})

test_that("export names unknown when only some results carry a version", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  plan <- .xp_plan("new", subgroups = FALSE)
  # Every ETT result carries the running version. The enrollment results
  # carry none, as a result from before 27.2.0 does.
  for (eid in names(plan$results_ett)) {
    plan$results_ett[[eid]][["swereg_version"]] <- .sv_running()
    plan$results_ett[[eid]]$swereg_version_s2 <- .sv_running()
  }
  expect_gt(length(plan$results_enrollment), 0L)
  expect_null(plan$results_enrollment[[1L]][["swereg_version"]])

  mixed <- paste0(.sv_running(), ", unknown")
  path <- withr::local_tempfile(fileext = ".xlsx")
  ws <- testthat::capture_warnings(suppressMessages(
    plan$export_tables(path = path)
  ))
  hit <- grep("Some stored results carry no recorded version", ws, value = TRUE)
  expect_length(hit, 1L)
  expect_match(
    hit,
    paste0("s2: ", mixed, ". s3: ", mixed, ". export: "),
    fixed = TRUE
  )
  expect_no_match(hit, "other versions computed", fixed = TRUE)

  d <- openxlsx::read.xlsx(path, sheet = "Provenance", colNames = FALSE)
  prov <- stats::setNames(d[[2L]], d[[1L]])
  expect_identical(unname(prov["swereg version (s2)"]), mixed)
  expect_identical(unname(prov["swereg version (s3)"]), mixed)

  ws <- testthat::capture_warnings(try(
    plan$export(list(list(type = "not_a_type")), dir = withr::local_tempdir()),
    silent = TRUE
  ))
  expect_length(grep("Some stored results carry no recorded version", ws), 1L)

  # A plan whose results all lack the stamps reads `unknown` and does not
  # warn.
  for (eid in names(plan$results_ett)) {
    plan$results_ett[[eid]][["swereg_version"]] <- NULL
    plan$results_ett[[eid]]$swereg_version_s2 <- NULL
  }
  path <- withr::local_tempfile(fileext = ".xlsx")
  ws <- testthat::capture_warnings(suppressMessages(
    plan$export_tables(path = path)
  ))
  expect_length(grep("recorded version|other versions computed", ws), 0L)
  d <- openxlsx::read.xlsx(path, sheet = "Provenance", colNames = FALSE)
  prov <- stats::setNames(d[[2L]], d[[1L]])
  expect_identical(unname(prov["swereg version (s2)"]), "unknown")
  expect_identical(unname(prov["swereg version (s3)"]), "unknown")
})

# A plan whose ETTs name a separate ITT analysis file, and a dispatcher whose
# workers read `pp` off a PP analysis file and `itt` off an ITT one.
# `plan` re-runs s3 on a plan that already holds results, and `...` passes the
# scope arguments to `$s3_analyze()`.
.sv_run_s3_pp_itt <- function(pp, itt, plan = NULL, ...) {
  if (is.null(plan)) {
    plan <- .sv_plan()
    plan$ett$file_analysis_itt <- c(
      "analysis_itt_001.qs2",
      "analysis_itt_002.qs2"
    )
  }
  fake <- .sv_fake_batch_run(NA_character_)
  testthat::local_mocked_bindings(
    .batch_run = function(target, items, n_workers, ...) {
      out <- fake(target, items, n_workers, ...)
      for (i in seq_along(items)) {
        # The enrollment items name no file here: the analysis files are
        # absent, so `which.min()` of their sizes is empty.
        is_itt <- any(grepl(
          "analysis_itt_",
          items[[i]]$analysis_path,
          fixed = TRUE
        ))
        out[[i]]$swereg_version_s2 <- if (is_itt) itt else pp
      }
      return(out)
    },
    .package = "swereg"
  )
  output_dir <- withr::local_tempdir()
  suppressWarnings(utils::capture.output(
    plan$s3_analyze(output_dir = output_dir, n_workers = 1L, ...)
  ))
  return(plan)
}

test_that("one ETT keeps the s2 version of both its PP and ITT analysis files", {
  skip_if_not_installed("openxlsx")
  plan <- .sv_run_s3_pp_itt(pp = "27.2.0", itt = "26.1.0")
  for (eid in c("ETT00001", "ETT00002")) {
    expect_identical(
      plan$results_ett[[eid]]$swereg_version_s2,
      c("26.1.0", "27.2.0")
    )
  }
  prov <- .sv_provenance(plan)
  expect_identical(unname(prov["swereg version (s2)"]), "26.1.0, 27.2.0")
  ws <- testthat::capture_warnings(try(
    plan$export(list(list(type = "not_a_type")), dir = withr::local_tempdir()),
    silent = TRUE
  ))
  hit <- grep("other versions computed the stored results", ws, value = TRUE)
  expect_length(hit, 1L)
  expect_match(hit, "s2: 26.1.0, 27.2.0. s3: ", fixed = TRUE)

  # An unstamped ITT analysis file adds NA to the stamp and `unknown` to the
  # Provenance cell and the warning.
  plan <- .sv_run_s3_pp_itt(pp = "27.2.0", itt = NA_character_)
  expect_identical(
    plan$results_ett[["ETT00001"]]$swereg_version_s2,
    c("27.2.0", NA_character_)
  )
  prov <- .sv_provenance(plan)
  expect_identical(unname(prov["swereg version (s2)"]), "27.2.0, unknown")
  ws <- testthat::capture_warnings(try(
    plan$export(list(list(type = "not_a_type")), dir = withr::local_tempdir()),
    silent = TRUE
  ))
  hit <- grep("Some stored results carry no recorded version", ws, value = TRUE)
  expect_length(hit, 1L)
  expect_match(hit, "s2: 27.2.0, unknown. s3: ", fixed = TRUE)
})

test_that("a re-run of s3 replaces a stored s2 stamp", {
  skip_if_not_installed("openxlsx")
  plan <- .sv_run_s3_pp_itt(pp = "27.2.0", itt = "27.2.0")
  # A stored result from an s2 version that the re-run no longer reads.
  for (eid in c("ETT00001", "ETT00002")) {
    plan$results_ett[[eid]]$swereg_version_s2 <- c("26.0.0")
  }
  # A scoped re-run takes the per-ETT clear, not the whole-list reset.
  plan <- .sv_run_s3_pp_itt(
    pp = "27.2.0",
    itt = "27.2.0",
    plan = plan,
    enrollment_ids = "01"
  )
  for (eid in c("ETT00001", "ETT00002")) {
    expect_identical(
      plan$results_ett[[eid]]$swereg_version_s2,
      "27.2.0",
      label = paste(eid, "stored s2 stamp")
    )
  }
  prov <- .sv_provenance(plan)
  expect_identical(unname(prov["swereg version (s2)"]), "27.2.0")
  expect_false(any(grepl("26.0.0", prov, fixed = TRUE)))
})

test_that("a recompute keeps every version of a carried s2 stamp", {
  plan <- .sv_run_s3_pp_itt(pp = "27.2.0", itt = "26.1.0")
  plan$results_enrollment[["01"]]$swereg_version_s2 <- c("26.1.0", "27.2.0")
  dir <- withr::local_tempdir()
  for (f in unique(c(plan$ett$file_analysis, plan$ett$file_raw))) {
    file.create(file.path(dir, f))
  }
  # A worker return without the field, so the stored stamp is carried.
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(...) {
      return(list(n_baseline = 4242L))
    },
    .package = "swereg"
  )
  suppressMessages(
    plan$recompute_baselines(output_dir = dir, enrollment_ids = "01", force = TRUE)
  )
  expect_identical(
    plan$results_enrollment[["01"]]$swereg_version_s2,
    c("26.1.0", "27.2.0")
  )
})

test_that("a recompute re-stamps the s3 version and keeps the worker's s2 stamp", {
  plan <- .sv_plan()
  suppressWarnings(.sv_run_s3(plan, "27.1.0"))
  # A stored result from an older s3, as a plan saved by that release holds.
  plan$results_enrollment[["01"]][["swereg_version"]] <- "27.0.0"
  dir <- withr::local_tempdir()
  for (f in unique(c(plan$ett$file_analysis, plan$ett$file_raw))) {
    file.create(file.path(dir, f))
  }

  # What the s3 worker returns: baseline counts and the s2 stamp it read.
  testthat::local_mocked_bindings(
    .s3_enrollment_worker = function(...) {
      return(list(n_baseline = 4242L, swereg_version_s2 = "27.1.5"))
    },
    .package = "swereg"
  )
  suppressMessages(
    plan$recompute_baselines(output_dir = dir, enrollment_ids = "01", force = TRUE)
  )

  after <- plan$results_enrollment[["01"]]
  expect_identical(after[["n_baseline"]], 4242L)
  expect_identical(after[["swereg_version"]], .sv_running())
  expect_identical(after[["swereg_version_s2"]], "27.1.5")
})

test_that("real s2 and s3 workers carry the s2 stamp to the stored results", {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("yaml")
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
    "ttmsv",
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
        n_workers = 1L,
        swereg_dev_path = dev_path
      )
    },
    type = "output"
  ))

  # The s2 worker stamped the analysis file in its own subprocess.
  analysis <- swereg::qs2_read(
    file.path(plan$dir_tteplan, plan$ett$file_analysis[1L])
  )
  expect_identical(analysis$swereg_version_s2, .sv_running())

  # s3 reads the stamp back, and the versions match, so it does not warn.
  expect_no_warning(invisible(utils::capture.output(
    plan$s3_analyze(n_workers = 1L, swereg_dev_path = dev_path),
    type = "output"
  )))
  expect_gt(length(plan$results_ett), 0L)
  for (r in c(plan$results_ett, plan$results_enrollment)) {
    expect_identical(r[["swereg_version"]], .sv_running())
    expect_identical(r$swereg_version_s2, .sv_running())
  }
})
