# tte_stage() across process boundaries, as Slurm runs it.
#
# Each stage runs in its own Rscript process. Each process loads the plan from
# disk, as one Slurm job does. A field that a stage sets and does not save is
# lost when its process ends. The next stage then reads the plan saved before
# it. In 27.2.1, tte_stage("s2") did not save the plan, so s3 found no s2
# stamp on any ETT and stopped (smrd 002, Slurm job 119).
#
# The fixture is helper-sparse_report.R. Its plan has two ETTs.

skip_on_cran()
skip_if_not_installed("processx")
skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")

tsp <- new.env(parent = emptyenv())

# One stage in a new Rscript process. The process loads the same swereg as
# this session: the source tree under load_all(), or the installed package
# under R CMD check.
tsp_stage <- function(stage, extra = "") {
  dev_path <- swereg:::.swereg_dev_path()
  script <- file.path(tsp$root, paste0(stage, ".R"))
  writeLines(
    c(
      sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
      sprintf("dev <- %s", paste(deparse(dev_path), collapse = "")),
      "if (is.null(dev)) {",
      "  suppressPackageStartupMessages(library(swereg))",
      "} else {",
      "  pkgload::load_all(dev, quiet = TRUE, export_all = FALSE)",
      "}",
      sprintf(
        "swereg::tte_stage(%s, %s, n_workers = 1L, swereg_dev_path = dev%s)",
        deparse(stage),
        deparse(tsp$dir_tteplan),
        extra
      )
    ),
    script
  )
  return(processx::run(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", script),
    error_on_status = FALSE,
    timeout = 900
  ))
}

tsp_run <- function() {
  if (isTRUE(tsp$done)) {
    return(tsp)
  }
  tsp$root <- withr::local_tempdir(.local_envir = testthat::teardown_env())
  dirs <- file.path(tsp$root, c("spec", "tteplan", "results", "meta"))
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  tsp$dir_tteplan <- dirs[2]
  sk <- spr_skeleton()
  skel_path <- file.path(dirs[2], SPR_SKELETON)
  qs2::qs_save(sk, skel_path)
  spr_write_spec(spr_report_spec(spr_report_rule()), dirs[1])
  plan <- suppressMessages(swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dirs[4]),
    candidate_dir_spec = dirs[1],
    candidate_dir_tteplan = dirs[2],
    candidate_dir_results = dirs[3],
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)],
    check_skeletons = FALSE
  ))
  # s0 saves the plan. Each stage below loads it from this file.
  tsp$path <- plan$save()

  tsp$s1 <- tsp_stage("s1", ", check_skeletons = FALSE")
  tsp$after_s1 <- swereg::tteplan_load(tsp$path)
  tsp$s2 <- tsp_stage("s2")
  tsp$after_s2 <- swereg::tteplan_load(tsp$path)
  tsp$s3 <- tsp_stage("s3")
  tsp$after_s3 <- swereg::tteplan_load(tsp$path)
  tsp$done <- TRUE
  return(tsp)
}

# Fails with the exit status and the last lines of the stage's stderr, so the
# message names the error that stopped the stage.
tsp_expect_exit_0 <- function(res, stage) {
  err <- utils::tail(strsplit(res$stderr, "\n", fixed = TRUE)[[1]], 8L)
  testthat::expect(
    identical(res$status, 0L),
    sprintf(
      "tte_stage(\"%s\") exited with status %s:\n%s",
      stage,
      res$status,
      paste(err, collapse = "\n")
    )
  )
  return(invisible(res))
}

test_that("s1 and s2 exit 0 in their own processes", {
  r <- tsp_run()
  tsp_expect_exit_0(r$s1, "s1")
  tsp_expect_exit_0(r$s2, "s2")
})

test_that("the plan saved after s2 carries the s2 stamp of every ETT", {
  r <- tsp_run()
  ett <- r$after_s2$ett
  expect_identical(nrow(ett), 2L)
  expect_identical(ett[["s1_run_id"]], rep(r$after_s1$s1_run_id, 2L))
  expect_true(all(startsWith(
    ett[["s2_run_id"]] %||% rep(NA_character_, 2L),
    paste0(r$after_s1$s1_run_id, ":s2:")
  )))
})

test_that("s3 accepts the files of the s2 stage before it", {
  r <- tsp_run()
  tsp_expect_exit_0(r$s3, "s3")
  plan <- r$after_s3
  expect_setequal(names(plan$results_ett) %||% character(0), plan$ett$ett_id)
  # Each stored result names the s1 run and the s2 run whose files it read.
  for (eid in plan$ett$ett_id) {
    res <- plan$results_ett[[eid]]
    expect_identical(res[["s1_run_id"]], r$after_s1$s1_run_id)
    expect_identical(
      res[["s2_run_id"]],
      r$after_s2$ett$s2_run_id[r$after_s2$ett$ett_id == eid]
    )
  }
})
