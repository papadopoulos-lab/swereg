# Tests for the CandidatePath + stub-free filename migration on TTEPlan.

# Minimal study helper: a bare list with `$skeleton_files` passes the loose
# duck-typing check in tteplan_from_spec_and_registrystudy().
.mock_study_min <- function(files) {
  list(skeleton_files = files)
}

# Build a minimal valid spec + directory layout and return it as a list
# containing the paths. The spec is written to `<dir>/spec_v001.yaml`.
.make_spec_env <- function() {
  base <- withr::local_tempdir(.local_envir = parent.frame())

  spec_dir    <- file.path(base, "home")
  tteplan_dir <- file.path(base, "project")
  results_dir <- file.path(base, "results")
  dir.create(spec_dir)
  dir.create(tteplan_dir)
  dir.create(results_dir)

  spec <- list(
    study = list(
      title = "CandidatePath test",
      implementation = list(
        project_prefix = "cp_test",
        version = "v001"
      )
    ),
    inclusion_criteria = list(isoyears = c(2010L, 2020L)),
    enrollments = list(
      list(
        id = "01",
        name = "Test enrollment",
        additional_inclusion = list(
          list(
            name = "Age 50-60",
            type = "age_range",
            min = 50,
            max = 60,
            implementation = list(variable = "rd_age_continuous")
          )
        ),
        treatment = list(
          implementation = list(
            variable = "rd_exposure",
            intervention_value = "treated",
            comparator_value = "control",
            matching_ratio = 2L,
            seed = 42L
          )
        )
      )
    ),
    outcomes = list(
      list(name = "Event A", implementation = list(variable = "diag_a"))
    ),
    follow_up = list(
      list(label = "1 year", weeks = 52L)
    ),
    confounders = list(
      list(name = "Age", implementation = list(variable = "rd_age_continuous"))
    )
  )
  yaml::write_yaml(spec, file.path(spec_dir, "spec_v001.yaml"))

  list(
    base = base,
    spec_dir = spec_dir,
    tteplan_dir = tteplan_dir,
    results_dir = results_dir
  )
}

test_that("tteplan_from_spec_and_registrystudy wires up CandidatePath fields", {
  env <- .make_spec_env()
  study <- .mock_study_min("/tmp/skel.qs2")

  plan <- tteplan_from_spec_and_registrystudy(
    study = study,
    candidate_dir_spec    = env$spec_dir,
    candidate_dir_tteplan = env$tteplan_dir,
    candidate_dir_results = env$results_dir,
    spec_version = "v001",
    global_max_isoyearweek = "2020-52"
  )

  expect_s3_class(plan$dir_tteplan_cp, "CandidatePath")
  expect_s3_class(plan$dir_spec_cp,    "CandidatePath")
  expect_s3_class(plan$dir_results_cp, "CandidatePath")

  expect_equal(plan$spec_version, "v001")
  expect_equal(plan$project_prefix, "cp_test")

  # Active bindings resolve to the host's directories
  expect_equal(plan$dir_tteplan, env$tteplan_dir)
  expect_equal(plan$dir_spec,    env$spec_dir)
  expect_equal(plan$dir_results, file.path(env$results_dir, "v001"))
})

test_that("derived file paths use stub-free filenames", {
  env <- .make_spec_env()
  study <- .mock_study_min("/tmp/skel.qs2")

  plan <- tteplan_from_spec_and_registrystudy(
    study = study,
    candidate_dir_spec    = env$spec_dir,
    candidate_dir_tteplan = env$tteplan_dir,
    candidate_dir_results = env$results_dir,
    spec_version = "v001",
    global_max_isoyearweek = "2020-52"
  )

  expect_equal(basename(plan$tteplan),     "tteplan.qs2")
  expect_equal(basename(plan$spec_path),   "spec_v001.yaml")
  expect_equal(basename(plan$spec_xlsx),   "spec_v001.xlsx")
  expect_equal(basename(plan$tables_xlsx), "tables.xlsx")
})

test_that("plan$save writes tteplan.qs2 and clears caches", {
  env <- .make_spec_env()
  study <- .mock_study_min("/tmp/skel.qs2")

  plan <- tteplan_from_spec_and_registrystudy(
    study = study,
    candidate_dir_spec    = env$spec_dir,
    candidate_dir_tteplan = env$tteplan_dir,
    candidate_dir_results = env$results_dir,
    spec_version = "v001",
    global_max_isoyearweek = "2020-52"
  )

  # Resolving bindings populates the caches
  plan$dir_tteplan
  plan$dir_spec
  plan$dir_results_base
  expect_true(plan$dir_tteplan_cp$is_resolved())
  expect_true(plan$dir_spec_cp$is_resolved())
  expect_true(plan$dir_results_cp$is_resolved())

  saved_path <- plan$save()

  expect_true(file.exists(saved_path))
  expect_equal(basename(saved_path), "tteplan.qs2")

  # Caches must be cleared post-save so the on-disk file is host-agnostic
  expect_false(plan$dir_tteplan_cp$is_resolved())
  expect_false(plan$dir_spec_cp$is_resolved())
  expect_false(plan$dir_results_cp$is_resolved())
})

test_that("tteplan_locate_and_load round-trip preserves spec + CandidatePath fields", {
  env <- .make_spec_env()
  study <- .mock_study_min("/tmp/skel.qs2")

  plan <- tteplan_from_spec_and_registrystudy(
    study = study,
    candidate_dir_spec    = env$spec_dir,
    candidate_dir_tteplan = env$tteplan_dir,
    candidate_dir_results = env$results_dir,
    spec_version = "v001",
    global_max_isoyearweek = "2020-52"
  )
  plan$save()

  reloaded <- tteplan_locate_and_load(env$tteplan_dir)

  expect_s3_class(reloaded, "TTEPlan")
  expect_equal(reloaded$spec_version, "v001")
  expect_equal(reloaded$project_prefix, "cp_test")
  expect_equal(nrow(reloaded$ett), 1L)

  # CandidatePath fields round-tripped and are NOT pre-resolved (cache was
  # cleared at save time)
  expect_s3_class(reloaded$dir_tteplan_cp, "CandidatePath")
  expect_false(reloaded$dir_tteplan_cp$is_resolved())

  # First access after reload re-resolves
  expect_equal(reloaded$dir_tteplan, env$tteplan_dir)
  expect_true(reloaded$dir_tteplan_cp$is_resolved())
})

test_that("spec_version mismatch between arg and YAML errors", {
  env <- .make_spec_env()
  study <- .mock_study_min("/tmp/skel.qs2")

  expect_error(
    tteplan_from_spec_and_registrystudy(
      study = study,
      candidate_dir_spec    = env$spec_dir,
      candidate_dir_tteplan = env$tteplan_dir,
      candidate_dir_results = env$results_dir,
      spec_version = "v999",
      global_max_isoyearweek = "2020-52"
    ),
    "not found"
  )
})

test_that("check_version errors on old TTEPlan schema", {
  env <- .make_spec_env()
  study <- .mock_study_min("/tmp/skel.qs2")

  plan <- tteplan_from_spec_and_registrystudy(
    study = study,
    candidate_dir_spec    = env$spec_dir,
    candidate_dir_tteplan = env$tteplan_dir,
    candidate_dir_results = env$results_dir,
    spec_version = "v001",
    global_max_isoyearweek = "2020-52"
  )

  assign(".schema_version", 0L, envir = plan$.__enclos_env__$private)
  expect_error(plan$check_version(), "schema version")
})
