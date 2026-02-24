# Tests for tte_spec.R: spec-driven study configuration

# =============================================================================
# Helper: write a minimal valid spec YAML to a temp file
# =============================================================================

.write_test_spec <- function(overrides = list()) {
  spec <- list(
    study = list(
      title = "Test Study",
      implementation = list(project_prefix = "test_project")
    ),
    population = list(
      calendar_years = c(2010L, 2020L)
    ),
    enrollments = list(
      list(
        id = "01",
        name = "Test enrollment",
        additional_eligibility = list(
          list(
            name = "Age 50-60",
            type = "age_range",
            min = 50,
            max = 60,
            implementation = list(variable = "rd_age_continuous")
          )
        ),
        exposure = list(
          matching_ratio = 2L,
          implementation = list(
            variable = "rd_exposure",
            exposed_value = "treated",
            comparator_value = "control",
            seed = 42L
          )
        )
      )
    ),
    outcomes = list(
      list(
        name = "Event A",
        implementation = list(variable = "diag_event_a")
      ),
      list(
        name = "Event B",
        implementation = list(variable = "diag_event_b")
      )
    ),
    follow_up = list(
      list(label = "1 year", weeks = 52L),
      list(label = "2 years", weeks = 104L)
    ),
    exclusion_criteria = list(
      list(
        name = "Prior event A",
        window = "lifetime",
        implementation = list(variable = "diag_event_a")
      ),
      list(
        name = "Prior exposure",
        window = "lifetime",
        implementation = list(
          type = "no_prior_exposure",
          variable = "rd_exposure",
          exposure_value = "treated"
        )
      ),
      list(
        name = "Recent drug use",
        window = "3 years",
        implementation = list(variable = "rx_drug")
      )
    ),
    confounders = list(
      list(
        name = "Age",
        implementation = list(variable = "rd_age_continuous")
      ),
      list(
        name = "Drug use in past year",
        window = "1 year",
        implementation = list(
          variable = "rd_no_drug_52wk",
          source_variable = "rx_drug",
          computed = TRUE
        )
      )
    )
  )

  # Apply overrides
  for (nm in names(overrides)) {
    spec[[nm]] <- overrides[[nm]]
  }

  path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(spec, path)
  path
}


# =============================================================================
# Helper: create a minimal skeleton data.table for testing eligibility
# =============================================================================

.make_test_skeleton <- function() {
  # 2 people, 5 weeks each
  dt <- data.table::data.table(
    id = rep(c(1L, 2L), each = 5),
    isoyear = rep(c(2015L, 2015L, 2015L, 2015L, 2015L), 2),
    isoyearweek = rep(paste0("2015-0", 1:5), 2),
    rd_age_continuous = rep(c(55, 45), each = 5),
    diag_event_a = c(
      FALSE, FALSE, FALSE, FALSE, FALSE,   # person 1: no events
      FALSE, TRUE,  FALSE, FALSE, FALSE    # person 2: event at week 2
    ),
    rd_exposure = c(
      rep("control", 5),                    # person 1: always control
      "control", "treated", "treated", "treated", "treated"  # person 2: switches
    ),
    rx_drug = c(
      FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, TRUE,  FALSE, FALSE
    ),
    diag_event_b = c(
      FALSE, FALSE, FALSE, TRUE,  FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE
    )
  )
  dt
}


# =============================================================================
# tte_read_spec tests
# =============================================================================

test_that("tte_read_spec parses valid YAML", {
  path <- .write_test_spec()
  on.exit(unlink(path))

  spec <- tte_read_spec(path)

  expect_type(spec, "list")
  expect_equal(spec$study$implementation$project_prefix, "test_project")
  expect_length(spec$outcomes, 2)
  expect_length(spec$enrollments, 1)
  expect_length(spec$follow_up, 2)
})

test_that("tte_read_spec converts windows correctly", {
  path <- .write_test_spec()
  on.exit(unlink(path))

  spec <- tte_read_spec(path)

  # "lifetime" -> Inf
  expect_equal(spec$exclusion_criteria[[1]]$window_weeks, Inf)
  expect_equal(spec$exclusion_criteria[[2]]$window_weeks, Inf)

  # "3 years" -> 156
  expect_equal(spec$exclusion_criteria[[3]]$window_weeks, 156)

  # "1 year" -> 52
  expect_equal(spec$confounders[[2]]$window_weeks, 52)
})

test_that("tte_read_spec errors on missing file", {
  expect_error(
    tte_read_spec("/nonexistent/path.yaml"),
    "Spec file not found"
  )
})

test_that("tte_read_spec errors on missing required sections", {
  path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(study = list(title = "X")), path)
  on.exit(unlink(path))

  expect_error(
    tte_read_spec(path),
    "Missing required sections"
  )
})

test_that("tte_read_spec errors on missing project_prefix", {
  path <- .write_test_spec(overrides = list(
    study = list(title = "No prefix")
  ))
  on.exit(unlink(path))

  expect_error(
    tte_read_spec(path),
    "project_prefix"
  )
})

test_that("tte_read_spec errors on outcome missing implementation variable", {
  path <- .write_test_spec(overrides = list(
    outcomes = list(
      list(name = "Bad outcome", implementation = list())
    )
  ))
  on.exit(unlink(path))

  expect_error(
    tte_read_spec(path),
    "missing implementation\\$variable"
  )
})

test_that("tte_read_spec errors on exclusion missing implementation variable", {
  path <- .write_test_spec(overrides = list(
    exclusion_criteria = list(
      list(name = "Bad exclusion", window = "lifetime", implementation = list())
    )
  ))
  on.exit(unlink(path))

  expect_error(
    tte_read_spec(path),
    "missing implementation\\$variable"
  )
})

test_that("tte_read_spec errors on computed confounder missing source_variable", {
  path <- .write_test_spec(overrides = list(
    confounders = list(
      list(
        name = "Bad computed",
        window = "1 year",
        implementation = list(variable = "x", computed = TRUE)
      )
    )
  ))
  on.exit(unlink(path))

  expect_error(
    tte_read_spec(path),
    "source_variable"
  )
})

test_that("tte_read_spec warns about open questions", {
  path <- .write_test_spec(overrides = list(
    open_questions = list(
      list(question = "Should we do X?", status = "open"),
      list(question = "Resolved thing", status = "resolved")
    )
  ))
  on.exit(unlink(path))

  expect_warning(
    tte_read_spec(path),
    "1 open question"
  )
})

test_that("tte_read_spec does not warn when all questions resolved", {
  path <- .write_test_spec(overrides = list(
    open_questions = list(
      list(question = "Done", status = "resolved")
    )
  ))
  on.exit(unlink(path))

  expect_no_warning(tte_read_spec(path))
})


# =============================================================================
# tte_apply_exclusions tests
# =============================================================================

test_that("tte_apply_exclusions creates eligible column", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))
  skeleton <- .make_test_skeleton()

  enrollment_spec <- list(enrollment_id = "01")
  result <- tte_apply_exclusions(skeleton, spec, enrollment_spec)

  expect_true("eligible" %in% names(result))
  expect_true("eligible_isoyears" %in% names(result))
  expect_true("eligible_age" %in% names(result))
  expect_type(result$eligible, "logical")
})

test_that("tte_apply_exclusions applies age range correctly", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))
  skeleton <- .make_test_skeleton()

  enrollment_spec <- list(enrollment_id = "01")
  result <- tte_apply_exclusions(skeleton, spec, enrollment_spec)

  # Person 1 (age 55) should pass age check, person 2 (age 45) should fail
  expect_true(all(result[id == 1, eligible_age]))
  expect_true(all(!result[id == 2, eligible_age]))
})

test_that("tte_apply_exclusions errors on unknown enrollment_id", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))
  skeleton <- .make_test_skeleton()

  expect_error(
    tte_apply_exclusions(skeleton, spec, list(enrollment_id = "99")),
    "not found"
  )
})


# =============================================================================
# tte_apply_derived_confounders tests
# =============================================================================

test_that("tte_apply_derived_confounders creates computed columns", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))
  skeleton <- .make_test_skeleton()

  result <- tte_apply_derived_confounders(skeleton, spec)

  expect_true("rd_no_drug_52wk" %in% names(result))
  expect_type(result$rd_no_drug_52wk, "logical")
})

test_that("tte_apply_derived_confounders skips non-computed confounders", {
  path <- .write_test_spec(overrides = list(
    confounders = list(
      list(
        name = "Age",
        implementation = list(variable = "rd_age_continuous")
      )
    )
  ))
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))
  skeleton <- .make_test_skeleton()

  n_cols_before <- ncol(skeleton)
  result <- tte_apply_derived_confounders(skeleton, spec)
  expect_equal(ncol(result), n_cols_before)
})

test_that("tte_apply_derived_confounders handles NULL confounders", {
  path <- .write_test_spec(overrides = list(confounders = NULL))
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))
  skeleton <- .make_test_skeleton()

  result <- tte_apply_derived_confounders(skeleton, spec)
  expect_identical(result, skeleton)
})


# =============================================================================
# tte_plan_from_spec tests
# =============================================================================

test_that("tte_plan_from_spec creates correct ETT grid", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))

  plan <- tte_plan_from_spec(
    spec,
    skeleton_files = c("/tmp/skel_001.qs2", "/tmp/skel_002.qs2"),
    global_max_isoyearweek = "2020-52"
  )

  expect_s3_class(plan, "TTEPlan")
  # 1 enrollment x 2 outcomes x 2 follow-ups = 4 ETTs
  expect_equal(nrow(plan$ett), 4)
  expect_equal(plan$project_prefix, "test_project")
})

test_that("tte_plan_from_spec stores exposure_impl in ETT", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))

  plan <- tte_plan_from_spec(
    spec,
    skeleton_files = "/tmp/skel.qs2",
    global_max_isoyearweek = "2020-52"
  )

  expect_true("exposure_impl" %in% names(plan$ett))
  expect_true("matching_ratio" %in% names(plan$ett))
  expect_true("seed" %in% names(plan$ett))

  # Check values
  expect_equal(plan$ett$matching_ratio[1], 2L)
  expect_equal(plan$ett$seed[1], 42L)
  expect_equal(plan$ett$exposure_impl[[1]]$variable, "rd_exposure")
  expect_equal(plan$ett$exposure_impl[[1]]$exposed_value, "treated")
})

test_that("tte_plan_from_spec passes exposure_impl through enrollment_spec", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))

  plan <- tte_plan_from_spec(
    spec,
    skeleton_files = "/tmp/skel.qs2",
    global_max_isoyearweek = "2020-52"
  )

  es <- plan[[1]]
  expect_equal(es$exposure_impl$variable, "rd_exposure")
  expect_equal(es$exposure_impl$exposed_value, "treated")
  expect_equal(es$matching_ratio, 2L)
  expect_equal(es$seed, 42L)
})

test_that("tte_plan_from_spec extracts confounder_vars", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))

  plan <- tte_plan_from_spec(
    spec,
    skeleton_files = "/tmp/skel.qs2",
    global_max_isoyearweek = "2020-52"
  )

  es <- plan[[1]]
  expect_equal(
    es$design$confounder_vars,
    c("rd_age_continuous", "rd_no_drug_52wk")
  )
})


# =============================================================================
# .convert_window tests
# =============================================================================

test_that(".convert_window handles all formats", {
  expect_equal(swereg:::.convert_window("lifetime"), Inf)
  expect_equal(swereg:::.convert_window("1 year"), 52L)
  expect_equal(swereg:::.convert_window("3 years"), 156L)
  expect_equal(swereg:::.convert_window("10 years"), 520L)
  expect_error(swereg:::.convert_window("unknown"), "Cannot parse window")
})
