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
    inclusion_criteria = list(
      isoyears = c(2010L, 2020L)
    ),
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
        implementation = list(
          source_variable = "diag_event_a",
          window = "lifetime_before_baseline",
          computed = TRUE
        )
      ),
      list(
        name = "Prior exposure",
        implementation = list(
          type = "no_prior_exposure",
          source_variable = "rd_exposure",
          exposure_value = "treated",
          window = "lifetime_before_baseline",
          computed = TRUE
        )
      ),
      list(
        name = "Recent drug use",
        implementation = list(
          source_variable = "rx_drug",
          window = 156L,
          computed = TRUE
        )
      )
    ),
    confounders = list(
      list(
        name = "Age",
        implementation = list(variable = "rd_age_continuous")
      ),
      list(
        name = "Drug use in past year",
        implementation = list(
          variable = "rd_no_drug_52wk",
          source_variable = "rx_drug",
          window = 52L,
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

  # "lifetime_before_baseline" -> Inf
  expect_equal(spec$exclusion_criteria[[1]]$implementation$window_weeks, Inf)
  expect_equal(spec$exclusion_criteria[[2]]$implementation$window_weeks, Inf)

  # 156L (numeric) -> 156
  expect_equal(spec$exclusion_criteria[[3]]$implementation$window_weeks, 156L)

  # 52L (numeric) -> 52
  expect_equal(spec$confounders[[2]]$implementation$window_weeks, 52L)
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

test_that("tte_read_spec errors on exclusion missing source_variable", {
  path <- .write_test_spec(overrides = list(
    exclusion_criteria = list(
      list(
        name = "Bad exclusion",
        implementation = list(window = "lifetime_before_baseline")
      )
    )
  ))
  on.exit(unlink(path))

  expect_error(
    tte_read_spec(path),
    "missing implementation\\$source_variable"
  )
})

test_that("tte_read_spec errors on computed confounder missing source_variable", {
  path <- .write_test_spec(overrides = list(
    confounders = list(
      list(
        name = "Bad computed",
        implementation = list(variable = "x", window = "1 year", computed = TRUE)
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

test_that("tte_apply_exclusions applies additional_exclusion criteria", {
  path <- .write_test_spec(overrides = list(
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
        additional_exclusion = list(
          list(
            name = "Prior event B",
            implementation = list(
              source_variable = "diag_event_b",
              window = "lifetime_before_baseline",
              computed = TRUE
            )
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
    )
  ))
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()

  enrollment_spec <- list(enrollment_id = "01")
  result <- tte_apply_exclusions(skeleton, spec, enrollment_spec)

  expect_true("eligible_no_diag_event_b_ever" %in% names(result))
  expect_true("eligible" %in% names(result))
})


# =============================================================================
# tte_read_spec additional_exclusion validation tests
# =============================================================================

test_that("tte_read_spec validates additional_exclusion entries", {
  # Missing implementation$source_variable
  path <- .write_test_spec(overrides = list(
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
        additional_exclusion = list(
          list(
            name = "Bad exclusion",
            implementation = list(window = "lifetime_before_baseline")
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
    )
  ))
  on.exit(unlink(path))

  expect_error(
    tte_read_spec(path),
    "additional_exclusion.*missing implementation\\$source_variable"
  )
})

test_that("tte_read_spec errors on additional_exclusion missing window", {
  path <- .write_test_spec(overrides = list(
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
        additional_exclusion = list(
          list(
            name = "Bad exclusion",
            implementation = list(source_variable = "diag_event_b")
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
    )
  ))
  on.exit(unlink(path))

  expect_error(
    tte_read_spec(path),
    "additional_exclusion.*missing implementation\\$window"
  )
})

test_that("tte_read_spec converts additional_exclusion windows", {
  path <- .write_test_spec(overrides = list(
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
        additional_exclusion = list(
          list(
            name = "Prior event B",
            implementation = list(
              source_variable = "diag_event_b",
              window = "lifetime_before_baseline",
              computed = TRUE
            )
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
    )
  ))
  on.exit(unlink(path))

  spec <- tte_read_spec(path)
  expect_equal(
    spec$enrollments[[1]]$additional_exclusion[[1]]$implementation$window_weeks,
    Inf
  )
})


# =============================================================================
# tte_validate_spec additional_exclusion tests
# =============================================================================

test_that("tte_validate_spec catches missing additional_exclusion source_variable", {
  path <- .write_test_spec(overrides = list(
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
        additional_exclusion = list(
          list(
            name = "Nonexistent",
            implementation = list(
              source_variable = "nonexistent_var",
              window = "lifetime_before_baseline",
              computed = TRUE
            )
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
    )
  ))
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()

  expect_error(
    tte_validate_spec(spec, skeleton),
    "additional_exclusion source_variable.*nonexistent_var.*not found"
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
# tte_plan_from_spec_and_skeleton_meta tests
# =============================================================================

test_that("tte_plan_from_spec_and_skeleton_meta creates correct ETT grid", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))

  plan <- tte_plan_from_spec_and_skeleton_meta(
    spec,
    skeleton_files = c("/tmp/skel_001.qs2", "/tmp/skel_002.qs2"),
    global_max_isoyearweek = "2020-52"
  )

  expect_s3_class(plan, "TTEPlan")
  # 1 enrollment x 2 outcomes x 2 follow-ups = 4 ETTs
  expect_equal(nrow(plan$ett), 4)
  expect_equal(plan$project_prefix, "test_project")
})

test_that("tte_plan_from_spec_and_skeleton_meta stores exposure_impl in ETT", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))

  plan <- tte_plan_from_spec_and_skeleton_meta(
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

test_that("tte_plan_from_spec_and_skeleton_meta passes exposure_impl through enrollment_spec", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))

  plan <- tte_plan_from_spec_and_skeleton_meta(
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

test_that("tte_plan_from_spec_and_skeleton_meta extracts confounder_vars", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))

  plan <- tte_plan_from_spec_and_skeleton_meta(
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
  expect_equal(swereg:::.convert_window("lifetime_before_baseline"), Inf)
  expect_equal(swereg:::.convert_window("1 year"), 52L)
  expect_equal(swereg:::.convert_window("3 years"), 156L)
  expect_equal(swereg:::.convert_window("10 years"), 520L)
  # Numeric input (preferred format)
  expect_equal(swereg:::.convert_window(52), 52L)
  expect_equal(swereg:::.convert_window(156L), 156L)
  expect_error(swereg:::.convert_window("unknown"), "Cannot parse window")
})


# =============================================================================
# tte_plan_from_spec_and_skeleton_meta stores spec on plan
# =============================================================================

test_that("tte_plan_from_spec_and_skeleton_meta stores spec on plan", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- suppressWarnings(tte_read_spec(path))

  plan <- tte_plan_from_spec_and_skeleton_meta(
    spec,
    skeleton_files = "/tmp/skel.qs2",
    global_max_isoyearweek = "2020-52"
  )

  expect_false(is.null(plan$spec))
  expect_equal(plan$spec$study$implementation$project_prefix, "test_project")
})


# =============================================================================
# generate_enrollments_and_ipw default callback
# =============================================================================

test_that("generate_enrollments_and_ipw errors when no process_fn and no spec", {
  plan <- tte_plan(
    project_prefix = "test",
    skeleton_files = "/tmp/skel.qs2",
    global_max_isoyearweek = "2020-52"
  )
  plan$add_one_ett(
    enrollment_id = "01",
    outcome_var = "death",
    outcome_name = "Death",
    follow_up = 52,
    confounder_vars = c("age"),
    time_exposure_var = "rd_exposed",
    eligible_var = "eligible",
    argset = list(age_group = "50_60", age_min = 50, age_max = 60)
  )

  expect_error(
    plan$generate_enrollments_and_ipw(output_dir = tempdir()),
    "process_fn is NULL and plan has no spec"
  )
})


# =============================================================================
# tte_validate_spec tests
# =============================================================================

test_that("tte_validate_spec passes on valid spec + skeleton", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()

  expect_message(
    result <- tte_validate_spec(spec, skeleton),
    "Spec validation passed"
  )
  expect_true(result)
})

test_that("tte_validate_spec errors on missing exclusion source_variable", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()
  skeleton[, diag_event_a := NULL]

  expect_error(
    tte_validate_spec(spec, skeleton),
    "exclusion_criteria.*source_variable.*diag_event_a.*not found"
  )
})

test_that("tte_validate_spec errors on missing outcome variable", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()
  skeleton[, diag_event_b := NULL]

  expect_error(
    tte_validate_spec(spec, skeleton),
    "outcomes.*diag_event_b.*not found"
  )
})

test_that("tte_validate_spec checks source_variable for computed confounders", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()
  skeleton[, rx_drug := NULL]

  expect_error(
    tte_validate_spec(spec, skeleton),
    "source_variable.*rx_drug.*not found"
  )
})

test_that("tte_validate_spec skips variable check for computed confounders", {
  # rd_no_drug_52wk is a computed variable — it won't exist in skeleton
  # but that's OK, validation should only check source_variable
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()

  # rd_no_drug_52wk is NOT in skeleton — should still pass
  expect_false("rd_no_drug_52wk" %in% names(skeleton))
  expect_message(
    tte_validate_spec(spec, skeleton),
    "Spec validation passed"
  )
})

test_that("tte_validate_spec errors on missing exposure variable", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()
  skeleton[, rd_exposure := NULL]

  expect_error(
    tte_validate_spec(spec, skeleton),
    "exposure variable.*rd_exposure.*not found"
  )
})

test_that("tte_validate_spec errors on wrong exposure values", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()
  # Replace values so exposed_value "treated" is missing
  skeleton[, rd_exposure := "other"]

  expect_error(
    tte_validate_spec(spec, skeleton),
    "exposed_value.*treated.*not found"
  )
})

test_that("tte_validate_spec reports category mismatches", {
  path <- .write_test_spec(overrides = list(
    confounders = list(
      list(
        name = "Education",
        categories = list("primary", "secondary"),
        implementation = list(variable = "rd_education")
      )
    )
  ))
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()
  skeleton[, rd_education := c(
    rep("primary", 5),
    rep("university", 5)
  )]

  # "university" is in data but not spec, "secondary" is in spec but not data
  expect_error(
    tte_validate_spec(spec, skeleton),
    "values in data but not spec.*university"
  )
})

test_that("tte_validate_spec collects ALL issues before stopping", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- tte_read_spec(path)
  skeleton <- .make_test_skeleton()
  # Remove both outcome columns
  skeleton[, diag_event_a := NULL]
  skeleton[, diag_event_b := NULL]

  err <- expect_error(tte_validate_spec(spec, skeleton))
  # Both outcomes should be reported
  expect_match(err$message, "diag_event_a")
  expect_match(err$message, "diag_event_b")
  # Should report total issue count
  expect_match(err$message, "issue\\(s\\)")
})

test_that("tte_validate_spec errors on non-data.table input", {
  path <- .write_test_spec()
  on.exit(unlink(path))
  spec <- tte_read_spec(path)

  expect_error(
    tte_validate_spec(spec, data.frame(x = 1)),
    "must be a data.table"
  )
})


# =============================================================================
# lifetime_before_and_after_baseline tests
# =============================================================================

test_that("tte_read_spec accepts lifetime_before_and_after_baseline window", {
  path <- .write_test_spec(overrides = list(
    exclusion_criteria = list(
      list(
        name = "Gender dysphoria (F64)",
        implementation = list(
          source_variable = "diag_gd",
          window = "lifetime_before_and_after_baseline"
        )
      )
    )
  ))
  on.exit(unlink(path))

  spec <- tte_read_spec(path)

  # Should NOT have window_weeks (person-level, not converted)
  expect_null(spec$exclusion_criteria[[1]]$implementation$window_weeks)
  expect_equal(
    spec$exclusion_criteria[[1]]$implementation$window,
    "lifetime_before_and_after_baseline"
  )
})

test_that("tte_apply_exclusions handles lifetime_before_and_after_baseline", {
  path <- .write_test_spec(overrides = list(
    exclusion_criteria = list(
      list(
        name = "Gender dysphoria (F64)",
        implementation = list(
          source_variable = "diag_gd",
          window = "lifetime_before_and_after_baseline"
        )
      )
    )
  ))
  on.exit(unlink(path))
  spec <- tte_read_spec(path)

  # 2 people, 5 weeks each. Person 2 has diag_gd at week 4 only.
  skeleton <- data.table::data.table(
    id = rep(c(1L, 2L), each = 5),
    isoyear = rep(2015L, 10),
    isoyearweek = rep(paste0("2015-0", 1:5), 2),
    rd_age_continuous = rep(c(55, 55), each = 5),
    diag_gd = c(
      FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, TRUE,  FALSE
    ),
    rd_exposure = rep("control", 10)
  )

  enrollment_spec <- list(enrollment_id = "01")
  result <- tte_apply_exclusions(skeleton, spec, enrollment_spec)

  col <- "eligible_no_diag_gd_lifetime_before_and_after_baseline"
  expect_true(col %in% names(result))

  # Person 1: no events -> all eligible

  expect_true(all(result[id == 1, get(col)]))

  # Person 2: event at week 4 -> ALL rows ineligible (person-level)
  expect_true(all(!result[id == 2, get(col)]))
})
