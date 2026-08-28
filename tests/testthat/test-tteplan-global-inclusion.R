# A global inclusion criterion applies to every enrollment.
#
# `inclusion_criteria` is a fixed container. It holds the `isoyears` pair and a
# `criteria` list, and nothing else. The container shape is what lets a strict
# key validator name every legal path inside it.
#
# Before this file existed, swereg read `inclusion_criteria` for `isoyears` and
# for nothing else. A global cohort restriction written there was parsed, held
# in the spec list, and never reached the eligibility filter.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Write a minimal valid spec. `criteria` is the `inclusion_criteria$criteria`
# list, or NULL to leave the container holding only `isoyears`.
#
# The spec carries two enrollments. Enrollment "01" declares an age range of
# its own. Enrollment "02" declares no additional inclusion at all, so an
# eligibility column on "02" can only come from the global container.
#
# The `no_prior_intervention` exclusion on `rd_exposure` is what silences the
# prevalent-user warning.
gi_spec_path <- function(criteria = NULL, enrollment_has_event = NULL) {
  inclusion <- list(isoyears = c(2015L, 2016L))
  if (!is.null(criteria)) {
    inclusion$criteria <- criteria
  }
  treatment <- list(
    implementation = list(
      variable = "rd_exposure",
      intervention_value = "treated",
      comparator_value = "control",
      comparator_to_intervention_ratio = 2L,
      seed = 42L
    )
  )
  spec <- list(
    study = list(
      title = "Global inclusion test",
      implementation = list(project_prefix = "test_project", version = "v001")
    ),
    inclusion_criteria = inclusion,
    enrollments = list(
      list(
        id = "01",
        name = "With its own inclusion",
        observed_var = list(sentinel = "row_presence"),
        additional_inclusion = c(
          list(list(
            name = "Age 50-60",
            type = "age_range",
            min = 50,
            max = 60,
            implementation = list(variable = "rd_age_continuous")
          )),
          if (is.null(enrollment_has_event)) {
            NULL
          } else {
            list(enrollment_has_event)
          }
        ),
        treatment = treatment
      ),
      list(
        id = "02",
        name = "With no inclusion of its own",
        observed_var = list(sentinel = "row_presence"),
        treatment = treatment
      )
    ),
    outcomes = list(list(
      name = "Event A",
      implementation = list(variable = "diag_event_a")
    )),
    follow_up = list(list(label = "1 year", weeks = 52L)),
    exclusion_criteria = list(list(
      name = "Prior intervention",
      implementation = list(
        type = "no_prior_intervention",
        source_variable = "rd_exposure",
        intervention_value = "treated",
        window = "lifetime_before_baseline",
        computed = TRUE
      )
    ))
  )
  dir <- tempfile("spec_")
  dir.create(dir)
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(spec, path)
  return(path)
}

# One criterion in the shape the container accepts.
gi_criterion <- function(
  name = "Prior psychotic disorder (ICD-10 F20-F29)",
  source_variable = "osd_f20_to_f29",
  window = "lifetime_before_baseline"
) {
  impl <- list(source_variable = source_variable, computed = TRUE)
  if (!is.null(window)) {
    impl$window <- window
  }
  return(list(
    name = name,
    rationale = "Restricts the study population.",
    type = "has_event",
    implementation = impl
  ))
}

# Two people, five weeks each. Person 1 has the event in week 2. Person 2 never
# has it. Both are 55, so the age range of enrollment "01" excludes neither.
#
# Person 2 starts the treatment in week 4. That is what puts both the
# intervention value and the comparator value in `rd_exposure`, which
# `tteplan_validate_spec()` checks. It leaves the combined `eligible` column
# unchanged, because no week of person 2 is eligible either way.
gi_skeleton <- function() {
  return(data.table::data.table(
    id = rep(c(1L, 2L), each = 5),
    isoyear = rep(2015L, 10),
    isoyearweek = rep(paste0("2015-0", 1:5), 2),
    rd_age_continuous = rep(55, 10),
    rd_exposure = c(
      rep("control", 5),
      "control",
      "control",
      "control",
      "treated",
      "treated"
    ),
    diag_event_a = rep(FALSE, 10),
    osd_f20_to_f29 = c(
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    ),
    osd_f30_to_f39 = rep(FALSE, 10)
  ))
}

gi_read <- function(criteria = NULL, enrollment_has_event = NULL) {
  path <- gi_spec_path(criteria, enrollment_has_event)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)
  return(tteplan_read_spec(path))
}


# ---------------------------------------------------------------------------
# 1. Reading the container
# ---------------------------------------------------------------------------

test_that("read_spec normalizes a global criterion the way it normalizes a per-enrollment one", {
  spec <- gi_read(list(gi_criterion(
    source_variable = "osd_f20_to_f29",
    window = 104L
  )))

  impl <- spec$inclusion_criteria$criteria[[1]]$implementation
  expect_identical(impl$source_variable, "osd_f20_to_f29")
  expect_identical(impl$source_variable_combined, "osd_f20_to_f29")
  expect_identical(impl$window_weeks, 104L)
})

test_that("read_spec joins a multi-source global criterion into one combined name", {
  spec <- gi_read(list(gi_criterion(
    source_variable = list("osd_f20_to_f29", "osd_f30_to_f39"),
    window = "lifetime_before_baseline"
  )))

  impl <- spec$inclusion_criteria$criteria[[1]]$implementation
  expect_identical(
    impl$source_variable,
    c("osd_f20_to_f29", "osd_f30_to_f39")
  )
  expect_identical(
    impl$source_variable_combined,
    "osd_f20_to_f29__osd_f30_to_f39"
  )
  expect_identical(impl$window_weeks, Inf)
})

test_that("read_spec rejects a global criterion whose type is not has_event", {
  # `has_events` is the typo this container exists to catch. A criterion swereg
  # reads and ignores never restricts the study population, and it looks
  # exactly like one that does.
  bad <- gi_criterion()
  bad$type <- "has_events"
  expect_error(
    gi_read(list(gi_criterion(source_variable = "osd_f30_to_f39"), bad)),
    "inclusion_criteria\\$criteria\\[2\\].*type 'has_events'"
  )
})

test_that("read_spec rejects a global criterion with no type at all", {
  bad <- gi_criterion()
  bad$type <- NULL
  expect_error(
    gi_read(list(bad)),
    "inclusion_criteria\\$criteria\\[1\\].*type '<missing>'"
  )
})

test_that("read_spec rejects a global criterion with no source_variable", {
  bad <- gi_criterion()
  bad$implementation$source_variable <- NULL
  expect_error(
    gi_read(list(bad)),
    "inclusion_criteria\\$criteria\\[1\\].*missing implementation\\$source_variable"
  )
})

test_that("read_spec rejects two global criteria that generate the same column name", {
  # Same source variable, same window, so both generate
  # `eligible_has_osd_f20_to_f29_104wk`.
  expect_error(
    gi_read(list(
      gi_criterion(name = "First", window = 104L),
      gi_criterion(name = "Second", window = 104L)
    )),
    "generates the eligibility column 'eligible_has_osd_f20_to_f29_104wk'"
  )
})

test_that("read_spec rejects a per-enrollment criterion that collides with a global one", {
  # Both generate `eligible_has_osd_f20_to_f29_everbefore`. The global criterion
  # already applies to every enrollment, so the enrollment's copy is a second
  # write of the same column.
  expect_error(
    gi_read(
      criteria = list(gi_criterion(window = "lifetime_before_baseline")),
      enrollment_has_event = gi_criterion(
        name = "The same thing again",
        window = "lifetime_before_baseline"
      )
    ),
    "generates the eligibility column 'eligible_has_osd_f20_to_f29_everbefore', which a global inclusion criterion already generates"
  )
})

test_that("read_spec accepts a per-enrollment criterion that differs from the global one by window", {
  spec <- gi_read(
    criteria = list(gi_criterion(window = "lifetime_before_baseline")),
    enrollment_has_event = gi_criterion(name = "Two years", window = 104L)
  )
  expect_identical(
    spec$enrollments[[1]]$additional_inclusion[[2]]$implementation$window_weeks,
    104L
  )
})

test_that("a global and a per-enrollment criterion on the same variable both reach the skeleton", {
  spec <- gi_read(
    criteria = list(gi_criterion(window = "lifetime_before_baseline")),
    enrollment_has_event = gi_criterion(name = "Two years", window = 104L)
  )
  result <- tteplan_apply_exclusions(
    gi_skeleton(),
    spec,
    list(enrollment_id = "01")
  )
  cols <- attr(result, "eligible_cols")
  expect_false(any(duplicated(cols)))
  expect_identical(
    cols,
    c(
      "eligible_isoyears",
      "eligible_has_osd_f20_to_f29_everbefore",
      "eligible_age",
      "eligible_has_osd_f20_to_f29_104wk",
      "eligible_no_rd_exposure_everbefore"
    )
  )
})

test_that("read_spec accepts two global criteria that differ only by window", {
  spec <- gi_read(list(
    gi_criterion(name = "Two years", window = 104L),
    gi_criterion(name = "Ever before", window = "lifetime_before_baseline")
  ))
  windows <- vapply(
    spec$inclusion_criteria$criteria,
    function(ic) ic$implementation$window_weeks,
    numeric(1)
  )
  expect_identical(windows, c(104, Inf))
})


# ---------------------------------------------------------------------------
# 2. The invariant: the criterion reaches the eligibility filter, for EVERY
#    enrollment
# ---------------------------------------------------------------------------

test_that("a global has_event criterion adds an eligibility column for every enrollment", {
  spec <- gi_read(list(gi_criterion(window = "lifetime_before_baseline")))

  for (eid in c("01", "02")) {
    skeleton <- gi_skeleton()
    result <- tteplan_apply_exclusions(
      skeleton,
      spec,
      list(enrollment_id = eid)
    )
    expect_true(
      "eligible_has_osd_f20_to_f29_everbefore" %in% names(result),
      info = paste("enrollment", eid)
    )
    expect_true(
      "eligible_has_osd_f20_to_f29_everbefore" %in%
        attr(result, "eligible_cols"),
      info = paste("enrollment", eid)
    )
  }
})

test_that("a global has_event criterion restricts the combined eligible column", {
  spec <- gi_read(list(gi_criterion(window = "lifetime_before_baseline")))
  result <- tteplan_apply_exclusions(
    gi_skeleton(),
    spec,
    list(enrollment_id = "02")
  )

  # Person 1 has the event in week 2, so weeks 3 to 5 are eligible. Person 2
  # never has it, so no week of person 2 is eligible.
  expect_identical(
    result$eligible_has_osd_f20_to_f29_everbefore,
    c(
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    )
  )
  expect_identical(
    result$eligible,
    c(
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    )
  )
})

test_that("a global has_event criterion names its column from source and window", {
  spec <- gi_read(list(gi_criterion(window = 104L)))
  result <- tteplan_apply_exclusions(
    gi_skeleton(),
    spec,
    list(enrollment_id = "02")
  )
  expect_identical(
    attr(result, "eligible_cols"),
    c(
      "eligible_isoyears",
      "eligible_has_osd_f20_to_f29_104wk",
      "eligible_no_rd_exposure_everbefore"
    )
  )
})

test_that("a multi-source global criterion builds its combined column on the skeleton", {
  spec <- gi_read(list(gi_criterion(
    source_variable = list("osd_f20_to_f29", "osd_f30_to_f39"),
    window = "lifetime_before_baseline"
  )))
  result <- tteplan_apply_exclusions(
    gi_skeleton(),
    spec,
    list(enrollment_id = "02")
  )
  expect_true("osd_f20_to_f29__osd_f30_to_f39" %in% names(result))
  expect_identical(
    result$eligible_has_osd_f20_to_f29__osd_f30_to_f39_everbefore,
    c(
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    )
  )
})


# ---------------------------------------------------------------------------
# 3. The column survives the canonical projection
# ---------------------------------------------------------------------------

test_that("the needed-column union keeps a global source variable", {
  # `.tte_canonical_needed_cols()` ends with an intersect() against the
  # canonical's own columns, so a source variable it does not collect is
  # dropped in silence and the eligibility filter then sees no column.
  #
  # Neither enrollment in this spec declares an additional inclusion that names
  # a source variable, so `osd_f20_to_f29` can only reach the union through the
  # global container.
  spec <- gi_read(list(gi_criterion(window = 104L)))
  enrollment_specs <- list(list(
    design = list(observed_var = list(sentinel = "row_presence")),
    treatment_impl = list(variable = "rd_exposure")
  ))

  cols <- swereg:::.tte_canonical_needed_cols(
    spec,
    enrollment_specs,
    names(gi_skeleton())
  )
  expect_true("osd_f20_to_f29" %in% cols)
})

test_that("the needed-column union keeps both members of a multi-source global criterion", {
  spec <- gi_read(list(gi_criterion(
    source_variable = list("osd_f20_to_f29", "osd_f30_to_f39"),
    window = 104L
  )))
  enrollment_specs <- list(list(
    design = list(observed_var = list(sentinel = "row_presence")),
    treatment_impl = list(variable = "rd_exposure")
  ))

  cols <- swereg:::.tte_canonical_needed_cols(
    spec,
    enrollment_specs,
    names(gi_skeleton())
  )
  expect_true(all(c("osd_f20_to_f29", "osd_f30_to_f39") %in% cols))
})


# ---------------------------------------------------------------------------
# 4. Validation against the skeleton
# ---------------------------------------------------------------------------

test_that("validate_spec reports a global source variable the skeleton lacks", {
  spec <- gi_read(list(gi_criterion(source_variable = "osd_f99_absent")))
  expect_error(
    tteplan_validate_spec(spec, gi_skeleton()),
    "inclusion_criteria\\$criteria.*'osd_f99_absent' not found in skeleton"
  )
})

test_that("validate_spec passes a global source variable the skeleton holds", {
  spec <- gi_read(list(gi_criterion(source_variable = "osd_f20_to_f29")))
  expect_message(
    expect_true(tteplan_validate_spec(spec, gi_skeleton())),
    "Spec validation passed"
  )
})


# ---------------------------------------------------------------------------
# 5. The passing direction: a container holding only `isoyears`
# ---------------------------------------------------------------------------
#
# Every spec in the fleet today writes `inclusion_criteria` with an `isoyears`
# pair and no `criteria` key. A container scan that mishandles that shape
# breaks all of them, and no red proof reaches this branch. The two vectors
# below are the measured behaviour of the code before the container existed.

test_that("an isoyears-only container produces the eligibility columns it always did", {
  spec <- gi_read(NULL)

  r01 <- tteplan_apply_exclusions(
    gi_skeleton(),
    spec,
    list(enrollment_id = "01")
  )
  expect_identical(
    attr(r01, "eligible_cols"),
    c(
      "eligible_isoyears",
      "eligible_age",
      "eligible_no_rd_exposure_everbefore"
    )
  )

  r02 <- tteplan_apply_exclusions(
    gi_skeleton(),
    spec,
    list(enrollment_id = "02")
  )
  expect_identical(
    attr(r02, "eligible_cols"),
    c("eligible_isoyears", "eligible_no_rd_exposure_everbefore")
  )

  # No column the container would have added.
  expect_false(any(grepl("^eligible_has_", names(r01))))
  expect_false(any(grepl("^eligible_has_", names(r02))))
})

test_that("an isoyears-only container leaves the needed-column union unchanged", {
  spec <- gi_read(NULL)
  enrollment_specs <- list(list(
    design = list(observed_var = list(sentinel = "row_presence")),
    treatment_impl = list(variable = "rd_exposure")
  ))

  cols <- swereg:::.tte_canonical_needed_cols(
    spec,
    enrollment_specs,
    names(gi_skeleton())
  )
  expect_identical(
    cols,
    c(
      "id",
      "isoyearweek",
      "isoyear",
      "rd_exposure",
      "rd_age_continuous",
      "diag_event_a"
    )
  )
})

test_that("an isoyears-only container leaves the spec list untouched", {
  spec <- gi_read(NULL)
  expect_identical(names(spec$inclusion_criteria), "isoyears")
  expect_identical(spec$inclusion_criteria$isoyears, c(2015L, 2016L))
})
