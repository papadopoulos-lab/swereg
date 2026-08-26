# TTEPlan$slurm_job() describes one Slurm job per pipeline stage.
#
# Three properties of the emitted script carry the whole invariant, and each
# one has failed elsewhere in this package before:
#
#   * the plan directory is ABSOLUTE. `plan$dir_tteplan` carries no
#     normalizePath() anywhere in its chain, and a relative path in a job
#     script resolves against the compute node's working directory.
#   * `n_workers` reaches the script BY NAME. A count that is validated and
#     then dropped runs the stage at whatever the compute node defaults to.
#   * s2 stays at ONE worker. `default_n_workers("s2")` reads
#     SWEREG_N_WORKERS_S2 and returns whatever the variable says, so the
#     mapping states 1L instead of calling it.

# A TTEPlan with nothing on disk but its own directory. `$new()` does not set
# `dir_tteplan_cp`, so the fixture sets it directly. `label` matters only for
# an error message.
.slurm_job_plan <- function(dir, prefix = "proj003") {
  plan <- TTEPlan$new(prefix, "/tmp/skeleton.qs2", "2020-52")
  plan$dir_tteplan_cp <- CandidatePath$new(dir, "dir_tteplan")
  plan
}

# A temporary directory holding one subdirectory named "project".
.slurm_job_dirs <- function(env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  project <- file.path(base, "project")
  dir.create(project)
  list(base = base, project = project)
}

test_that("slurm_job('s1') names tte_stage with stage, absolute dir and workers", {
  d <- .slurm_job_dirs()
  plan <- .slurm_job_plan(d$project)
  withr::local_envvar(c(SWEREG_N_WORKERS_S1 = "6"))

  job <- plan$slurm_job("s1", time = "12:00:00")

  expect_s3_class(job, "slurm_it")
  expect_identical(
    job[["script"]],
    sprintf(
      "Rscript -e 'swereg::tte_stage(\"s1\", \"%s\", n_workers = 6L)'",
      normalizePath(d$project, winslash = "/", mustWork = TRUE)
    )
  )
  # The resolved count reaches the job request too, not only the script.
  expect_identical(job[["cpus"]], "6")
  expect_identical(job[["mem"]], "85G")
  expect_identical(job[["time"]], "12:00:00")
  expect_true(job[["requeue"]])
  expect_true(job[["exclusive"]])
})

test_that("the plan directory reaches the script as an absolute path", {
  # The fixture points dir_tteplan_cp at a RELATIVE candidate. CandidatePath
  # resolves with first_existing_path(), which normalises nothing, so
  # `plan$dir_tteplan` comes back as "project".
  d <- .slurm_job_dirs()
  withr::local_dir(d$base)
  plan <- .slurm_job_plan("project")
  expect_identical(plan$dir_tteplan, "project")

  job <- plan$slurm_job("s1", time = "12:00:00")

  expect_match(job[["script"]], 'swereg::tte_stage\\("s1", "/')
})

test_that("s2 stays at one worker whatever SWEREG_N_WORKERS_S2 says", {
  d <- .slurm_job_dirs()
  plan <- .slurm_job_plan(d$project)
  withr::local_envvar(c(SWEREG_N_WORKERS_S2 = "4"))

  # The variable is live: default_n_workers() reads it and returns 4.
  expect_identical(default_n_workers("s2"), 4L)

  job <- plan$slurm_job("s2", time = "24:00:00")

  expect_match(job[["script"]], "n_workers = 1L", fixed = TRUE)
  expect_false(grepl("n_workers = 4L", job[["script"]], fixed = TRUE))
  expect_identical(job[["cpus"]], "1")
})

test_that("s1 and s3 take their default worker count from default_n_workers", {
  d <- .slurm_job_dirs()
  plan <- .slurm_job_plan(d$project)
  withr::local_envvar(c(SWEREG_N_WORKERS_S1 = "6", SWEREG_N_WORKERS_S3 = "2"))

  expect_match(
    plan$slurm_job("s1", time = "12:00:00")[["script"]],
    "n_workers = 6L",
    fixed = TRUE
  )
  expect_match(
    plan$slurm_job("s3", time = "12:00:00")[["script"]],
    "n_workers = 2L",
    fixed = TRUE
  )
})

test_that("an explicit n_workers overrides the per-stage default", {
  d <- .slurm_job_dirs()
  plan <- .slurm_job_plan(d$project)
  withr::local_envvar(c(SWEREG_N_WORKERS_S1 = "6"))

  job <- plan$slurm_job("s1", time = "12:00:00", n_workers = 3L)

  expect_match(job[["script"]], "n_workers = 3L", fixed = TRUE)
  expect_identical(job[["cpus"]], "3")
})

test_that("the job name is derived from project_prefix and stage", {
  d <- .slurm_job_dirs()
  plan <- .slurm_job_plan(d$project, prefix = "proj006")

  expect_identical(plan$slurm_job("s1", time = "12:00:00")[["name"]], "proj006_s1")
  expect_identical(plan$slurm_job("s2", time = "12:00:00")[["name"]], "proj006_s2")
  expect_identical(plan$slurm_job("s3", time = "12:00:00")[["name"]], "proj006_s3")

  # `name` is not a formal, so a caller cannot collide two projects' jobs.
  expect_false("name" %in% names(formals(TTEPlan$public_methods$slurm_job)))
})

test_that("the job pins the swereg version at generation time", {
  d <- .slurm_job_dirs()
  plan <- .slurm_job_plan(d$project)

  job <- plan$slurm_job("s1", time = "12:00:00")

  expect_identical(
    job[["require_r_package"]],
    c(swereg = as.character(utils::packageVersion("swereg")))
  )
})

test_that("slurm_job refuses a stage it does not know", {
  d <- .slurm_job_dirs()
  plan <- .slurm_job_plan(d$project)

  # No default is forced here: `n_workers` is supplied, so the stage check in
  # the body is the only thing that can reject "s4".
  expect_error(
    plan$slurm_job("s4", time = "12:00:00", n_workers = 1L),
    "must be one of"
  )
  expect_error(plan$slurm_job(c("s1", "s2"), time = "12:00:00", n_workers = 1L))
})

test_that("slurm_job refuses a worker count that is not a whole number", {
  d <- .slurm_job_dirs()
  plan <- .slurm_job_plan(d$project)

  expect_error(
    plan$slurm_job("s1", time = "12:00:00", n_workers = 2.5),
    "slurm_job()",
    fixed = TRUE
  )
  expect_error(plan$slurm_job("s1", time = "12:00:00", n_workers = 0L))
})

test_that("slurm_job names slurm_it and never slurm_write", {
  # P7 classified slurm_it as a batchit primitive and left slurm_write
  # unclassified, because swereg does not write the job script.
  # test-batch_lockdown.R holds that decision; this asserts the source obeys it.
  src <- testthat::test_path("..", "..", "R", "r6_tteplan_slurm.R")
  skip_if_not(file.exists(src), "R/ sources not present (installed package?)")

  txt <- readLines(src, warn = FALSE)
  expect_equal(sum(grepl("slurm_write", txt, fixed = TRUE)), 0L)
  expect_gte(sum(grepl("batchit::slurm_it", txt, fixed = TRUE)), 1L)
})
