# The landmark contract: one definition of observation and of the arm
# tolerances, carried from the spec YAML to the s1 cache without a break.
#
# swereg used to have no way to tell an unobserved person from one who was out
# of arm or ineligible. A trimmed skeleton made the gap invisible: a person is
# simply absent that week, and an absent week reads as "not treated" exactly
# like an observed comparator week does. Every landmark step needs that
# distinction, so the spec now states how observation is encoded, and this
# file pins the whole chain.
#
# The chain has five links, and a break in any one leaves the rest dark:
#
#   spec YAML
#     -> tteplan_read_spec()
#     -> the ett grid columns
#     -> enrollment_spec()
#     -> TTEDesign
#     -> .tte_s1_cache_columns()
#     -> the s1b / s1c workers
#
# `.tte_s1_cache_columns()` is the link that fails most quietly. It is a named
# allow-list, so a column missing from it is dropped before s1b and s1c ever
# read the cache. Nothing errors and nothing warns.

skip_if_not_installed("data.table")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")
skip_if_not_installed("qs2")

# ---------------------------------------------------------------------------
# A minimal spec, with per-enrollment overrides. `enrollments` replaces the
# whole block, so a test can drop `observed_var` from one enrollment.
# ---------------------------------------------------------------------------

lc_enrollment <- function(id = "01", ..., drop = character()) {
  enr <- list(
    id = id,
    name = paste0("Enrollment ", id),
    observed_var = list(sentinel = "row_presence"),
    intervention_tolerance_weeks = 0L,
    comparator_tolerance_weeks = 0L,
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
        name = "No prior intervention",
        implementation = list(
          type = "no_prior_intervention",
          source_variable = "rd_exposure",
          intervention_value = "treated",
          window = "lifetime_before_baseline",
          computed = TRUE
        )
      )
    ),
    treatment = list(
      arms = list(intervention = "Treated", comparator = "Control"),
      implementation = list(
        variable = "rd_exposure",
        intervention_value = "treated",
        comparator_value = "control",
        comparator_to_intervention_ratio = 2L,
        seed = 42L
      )
    )
  )
  overrides <- list(...)
  for (nm in names(overrides)) {
    enr[[nm]] <- overrides[[nm]]
  }
  for (nm in drop) {
    enr[[nm]] <- NULL
  }
  enr
}

lc_write_spec <- function(enrollments = list(lc_enrollment()), env = parent.frame()) {
  spec <- list(
    study = list(
      title = "Landmark contract fixture",
      implementation = list(project_prefix = "lc_test", version = "v001")
    ),
    inclusion_criteria = list(isoyears = c(2010L, 2020L)),
    enrollments = enrollments,
    outcomes = list(
      list(name = "Event A", implementation = list(variable = "osd_a"))
    ),
    follow_up = list(list(label = "1 year", weeks = 52L)),
    confounders = list(
      list(name = "Age", implementation = list(variable = "rd_age_continuous"))
    )
  )
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(spec, path)
  path
}

# A two-row skeleton carrying every column the s1 cache allow-list can keep.
# Two rows, because `tteplan_validate_spec()` checks that both arm values
# appear in the treatment column.
lc_skeleton <- function() {
  sk <- data.table::data.table(
    id = c(1L, 2L),
    isoyearweek = "2015-01",
    isoyear = 2015L,
    trial_id = 0L,
    rd_intervention = c(TRUE, FALSE),
    baseline_intervention = c(TRUE, FALSE),
    rd_age_continuous = 55,
    rd_exposure = c("treated", "control"),
    rd_observed = TRUE,
    osd_a = FALSE,
    eligible = TRUE
  )
  data.table::setattr(sk, "eligible_cols", "eligible")
  sk
}

lc_design <- function(observed_var = list(column = "rd_observed"), ...) {
  swereg::TTEDesign$new(
    person_id_var = "id",
    treatment_var = "baseline_intervention",
    outcome_vars = "osd_a",
    confounder_vars = "rd_age_continuous",
    follow_up_time = 52L,
    eligible_var = "eligible",
    observed_var = observed_var,
    ...
  )
}

# ---------------------------------------------------------------------------
# 1. Both spec forms parse
# ---------------------------------------------------------------------------

test_that("tteplan_read_spec: both observation forms parse", {
  path <- lc_write_spec(list(
    lc_enrollment("01", observed_var = list(sentinel = "row_presence")),
    lc_enrollment("02", observed_var = list(column = "rd_observed"))
  ))
  spec <- swereg::tteplan_read_spec(path)

  first <- spec$enrollments[[1]]$observed_var
  expect_s3_class(first, "tte_observed_var")
  expect_identical(first$sentinel, "row_presence")
  expect_true(is.na(first$column))

  second <- spec$enrollments[[2]]$observed_var
  expect_s3_class(second, "tte_observed_var")
  expect_identical(second$column, "rd_observed")
  expect_true(is.na(second$sentinel))
})

test_that("tteplan_read_spec: both tolerances parse and default to 0 weeks", {
  path <- lc_write_spec(list(
    lc_enrollment(
      "01",
      intervention_tolerance_weeks = 4L,
      comparator_tolerance_weeks = 8L
    ),
    lc_enrollment(
      "02",
      drop = c("intervention_tolerance_weeks", "comparator_tolerance_weeks")
    )
  ))
  spec <- swereg::tteplan_read_spec(path)

  expect_identical(spec$enrollments[[1]]$intervention_tolerance_weeks, 4L)
  expect_identical(spec$enrollments[[1]]$comparator_tolerance_weeks, 8L)
  expect_identical(spec$enrollments[[2]]$intervention_tolerance_weeks, 0L)
  expect_identical(spec$enrollments[[2]]$comparator_tolerance_weeks, 0L)
})

# ---------------------------------------------------------------------------
# 2. The parser rejects every malformed declaration
# ---------------------------------------------------------------------------

test_that("tteplan_read_spec: an enrollment without observed_var errors", {
  # One enrollment out of two omits the key. The error names that enrollment.
  path <- lc_write_spec(list(
    lc_enrollment("01"),
    lc_enrollment("02", drop = "observed_var")
  ))
  expect_error(
    swereg::tteplan_read_spec(path),
    "enrollments\\[2\\] 'Enrollment 02' is missing 'observed_var'"
  )

  # No enrollment declares the key. There is no exemption for an older spec,
  # so this errors too.
  path_none <- lc_write_spec(list(
    lc_enrollment(
      "01",
      drop = c(
        "observed_var",
        "intervention_tolerance_weeks",
        "comparator_tolerance_weeks"
      )
    ),
    lc_enrollment(
      "02",
      drop = c(
        "observed_var",
        "intervention_tolerance_weeks",
        "comparator_tolerance_weeks"
      )
    )
  ))
  expect_error(
    swereg::tteplan_read_spec(path_none),
    "enrollments\\[1\\] 'Enrollment 01' is missing 'observed_var'"
  )

  # A tolerance without an observation encoding is rejected by the same rule.
  path_tol <- lc_write_spec(list(
    lc_enrollment(
      "01",
      intervention_tolerance_weeks = 4L,
      drop = "observed_var"
    )
  ))
  expect_error(
    swereg::tteplan_read_spec(path_tol),
    "is missing 'observed_var'"
  )
})

test_that("tteplan_read_spec: observed_var with both column and sentinel errors", {
  path <- lc_write_spec(list(
    lc_enrollment(
      "01",
      observed_var = list(column = "rd_observed", sentinel = "row_presence")
    )
  ))
  expect_error(
    swereg::tteplan_read_spec(path),
    "gives both `column` and `sentinel`"
  )
})

test_that("observed_var rejects a two-key mapping even when one value is NULL", {
  # `observed_var: {column: null, sentinel: row_presence}` is valid YAML. It
  # parses to a two-key list whose `column` value is NULL. A reader of the
  # YAML sees two claims, so swereg MUST reject it. A `!is.null()` test reads
  # it as one claim and accepts it.
  raw <- yaml::yaml.load("observed_var: {column: null, sentinel: row_presence}")
  expect_named(raw$observed_var, c("column", "sentinel"))
  expect_null(raw$observed_var$column)

  expect_error(
    swereg:::.tte_observed_var(list(column = NULL, sentinel = "row_presence")),
    "gives both `column` and `sentinel`"
  )
  expect_error(
    swereg:::.tte_observed_var(list(sentinel = NULL, column = "rd_observed")),
    "gives both `column` and `sentinel`"
  )
  expect_error(
    lc_design(observed_var = list(column = NULL, sentinel = "row_presence")),
    "gives both `column` and `sentinel`"
  )
  expect_error(
    lc_design(observed_var = list(sentinel = NULL, column = "rd_observed")),
    "gives both `column` and `sentinel`"
  )

  # A single key whose value is NULL is a different fault, and it also errors.
  expect_error(
    swereg:::.tte_observed_var(list(column = NULL)),
    "must be a single non-empty column name"
  )
  expect_error(
    swereg:::.tte_observed_var(list(sentinel = NULL)),
    "must be a single sentinel name"
  )
})

test_that("tteplan_read_spec: an unknown observation sentinel errors", {
  # `row_presence` is the only sentinel this version of swereg knows. Any
  # other name is a claim swereg cannot honour, so it MUST NOT parse.
  for (name in c("any_row", "banana", "ROW_PRESENCE")) {
    path <- lc_write_spec(list(
      lc_enrollment("01", observed_var = list(sentinel = name))
    ))
    expect_error(
      swereg::tteplan_read_spec(path),
      "which swereg does not know",
      info = paste("sentinel:", name)
    )
    expect_error(
      lc_design(observed_var = list(sentinel = name)),
      "which swereg does not know",
      info = paste("sentinel:", name)
    )
  }
})

# The tolerance guard has three independent requirements: finite and
# representable, non-negative, and whole. Each one gets its own test_that()
# block, so a mutation that weakens one names a label the other two do not
# share.

test_that("a negative tolerance is rejected", {
  for (v in list(-1L, -1, -0.5, -52L)) {
    expect_error(
      swereg:::.tte_tolerance_weeks(v, "tol"),
      "must be a single whole number of weeks",
      info = paste("value:", format(v))
    )
    expect_error(
      lc_design(intervention_tolerance_weeks = v),
      "must be a single whole number of weeks",
      info = paste("value:", format(v))
    )
    expect_error(
      lc_design(comparator_tolerance_weeks = v),
      "must be a single whole number of weeks",
      info = paste("value:", format(v))
    )
  }
  path <- lc_write_spec(list(
    lc_enrollment("01", comparator_tolerance_weeks = -1L)
  ))
  expect_error(
    swereg::tteplan_read_spec(path),
    "must be a single whole number of weeks"
  )
})

test_that("a fractional tolerance is rejected", {
  for (v in list(1.5, 0.5, 2.0001, 51.9)) {
    expect_error(
      swereg:::.tte_tolerance_weeks(v, "tol"),
      "must be a single whole number of weeks",
      info = paste("value:", format(v))
    )
    expect_error(
      lc_design(intervention_tolerance_weeks = v),
      "must be a single whole number of weeks",
      info = paste("value:", format(v))
    )
    expect_error(
      lc_design(comparator_tolerance_weeks = v),
      "must be a single whole number of weeks",
      info = paste("value:", format(v))
    )
  }
  path <- lc_write_spec(list(
    lc_enrollment("01", intervention_tolerance_weeks = 1.5)
  ))
  expect_error(
    swereg::tteplan_read_spec(path),
    "must be a single whole number of weeks"
  )
})

test_that("a tolerance is never stored as NA", {
  # `as.integer(Inf)` and `as.integer(3e9)` both return NA with only a
  # warning, and both pass a whole-number test. An NA tolerance compares as
  # neither tolerated nor discordant in every later adherence rule, so it is
  # worse than a loud error.
  # A negative value and a fractional value belong to their own labels, so
  # they are NOT in this list. Keeping them here would make the mutation that
  # weakens `x >= 0` redden this label too, and the three requirements could
  # not be told apart.
  bad <- list(
    Inf,
    -Inf,
    NaN,
    NA_real_,
    NA_integer_,
    3e9,
    .Machine$integer.max + 1,
    "2",
    c(1L, 2L),
    integer(0)
  )
  for (v in bad) {
    expect_error(
      swereg:::.tte_tolerance_weeks(v, "tol"),
      "must be a single whole number of weeks",
      info = paste("value:", paste(format(v), collapse = ", "))
    )
    expect_error(
      lc_design(intervention_tolerance_weeks = v),
      "must be a single whole number of weeks",
      info = paste("value:", paste(format(v), collapse = ", "))
    )
    expect_error(
      lc_design(comparator_tolerance_weeks = v),
      "must be a single whole number of weeks",
      info = paste("value:", paste(format(v), collapse = ", "))
    )
  }

  # The largest accepted value round-trips, and it is not NA.
  ok <- lc_design(intervention_tolerance_weeks = .Machine$integer.max)
  expect_identical(ok$intervention_tolerance_weeks, .Machine$integer.max)
  expect_false(is.na(ok$intervention_tolerance_weeks))
  expect_false(is.na(ok$comparator_tolerance_weeks))
})

test_that("tteplan_validate_spec: an observation column that is missing or not logical errors", {
  # The parser reads no data, so it cannot run this check. The skeleton-aware
  # validator runs it.
  path <- lc_write_spec(list(
    lc_enrollment("01", observed_var = list(column = "rd_observed"))
  ))
  spec <- swereg::tteplan_read_spec(path)

  sk_missing <- lc_skeleton()
  sk_missing[, rd_observed := NULL]
  expect_error(
    swereg::tteplan_validate_spec(spec, sk_missing),
    "observed_var column 'rd_observed' not found in skeleton"
  )

  # Delete the column before adding the integer one. `:=` into an existing
  # logical column coerces the value back to logical, and the wrong type
  # never reaches the check.
  sk_wrong_type <- lc_skeleton()
  sk_wrong_type[, rd_observed := NULL]
  sk_wrong_type[, rd_observed := c(1L, 0L)]
  expect_false(is.logical(sk_wrong_type$rd_observed))
  expect_error(
    swereg::tteplan_validate_spec(spec, sk_wrong_type),
    "observed_var column 'rd_observed' must be logical"
  )

  expect_silent(suppressMessages(
    swereg::tteplan_validate_spec(spec, lc_skeleton())
  ))
})

# ---------------------------------------------------------------------------
# 3. The three fields travel the whole chain
# ---------------------------------------------------------------------------

# Propagation: spec YAML -> tteplan_read_spec() -> the ETT grid ->
# enrollment_spec() -> TTEDesign.
#
# THE THREE FIELDS GET THREE SEPARATE test_that() BLOCKS, one per field. A
# single block asserting all three goes red whichever field a mutation cuts,
# so it cannot tell the three links apart. Separate blocks can.
#
# Every tolerance below differs from the default of 0. A cut argument falls
# back to that default, so an assertion against 0 could not detect the cut.

lc_propagation_plan <- function(env = parent.frame()) {
  path <- lc_write_spec(
    list(
      lc_enrollment(
        "01",
        observed_var = list(column = "rd_observed"),
        intervention_tolerance_weeks = 4L,
        comparator_tolerance_weeks = 8L
      ),
      lc_enrollment(
        "02",
        observed_var = list(sentinel = "row_presence"),
        intervention_tolerance_weeks = 12L,
        comparator_tolerance_weeks = 16L
      )
    ),
    env = env
  )
  dir <- dirname(path)
  swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = file.path(dir, "skel.qs2")),
    candidate_dir_spec = dir,
    candidate_dir_tteplan = dir,
    candidate_dir_results = dir,
    spec_version = "v001",
    global_max_isoyearweek = "2020-52"
  )
}

test_that("the observation encoding reaches TTEDesign through enrollment_spec()", {
  plan <- lc_propagation_plan()
  expect_true("observed_var" %in% names(plan$ett))

  es1 <- plan$enrollment_spec(1L)
  expect_identical(es1$enrollment_id, "01")
  expect_identical(es1$design$observed_var$column, "rd_observed")
  expect_true(is.na(es1$design$observed_var$sentinel))

  es2 <- plan$enrollment_spec(2L)
  expect_identical(es2$enrollment_id, "02")
  expect_identical(es2$design$observed_var$sentinel, "row_presence")
  expect_true(is.na(es2$design$observed_var$column))
})

test_that("the intervention tolerance reaches TTEDesign through enrollment_spec()", {
  plan <- lc_propagation_plan()
  expect_true("intervention_tolerance_weeks" %in% names(plan$ett))
  expect_identical(
    plan$enrollment_spec(1L)$design$intervention_tolerance_weeks,
    4L
  )
  expect_identical(
    plan$enrollment_spec(2L)$design$intervention_tolerance_weeks,
    12L
  )
})

test_that("the comparator tolerance reaches TTEDesign through enrollment_spec()", {
  plan <- lc_propagation_plan()
  expect_true("comparator_tolerance_weeks" %in% names(plan$ett))
  expect_identical(
    plan$enrollment_spec(1L)$design$comparator_tolerance_weeks,
    8L
  )
  expect_identical(
    plan$enrollment_spec(2L)$design$comparator_tolerance_weeks,
    16L
  )
})

test_that("the observation encoding and both tolerances survive a plan save/load round trip", {
  dir <- withr::local_tempdir()
  plan <- swereg::TTEPlan$new(
    project_prefix = "lc_roundtrip",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52"
  )
  plan$add_one_ett(
    enrollment_id = "01",
    outcome_var = "osd_a",
    outcome_name = "Event A",
    follow_up = 52L,
    confounder_vars = "rd_age_continuous",
    time_treatment_var = "rd_intervention",
    eligible_var = "eligible",
    observed_var = list(column = "rd_observed"),
    intervention_tolerance_weeks = 4L,
    comparator_tolerance_weeks = 8L,
    argset = list(age_group = "50_60", age_min = 50L, age_max = 60L)
  )
  plan$save(dir = dir)

  reloaded <- swereg::tteplan_locate_and_load(dir)
  expect_identical(reloaded$ett$observed_var[[1]]$column, "rd_observed")
  expect_true(is.na(reloaded$ett$observed_var[[1]]$sentinel))
  expect_identical(reloaded$ett$intervention_tolerance_weeks, 4L)
  expect_identical(reloaded$ett$comparator_tolerance_weeks, 8L)

  design <- reloaded$enrollment_spec(1L)$design
  expect_identical(design$observed_var$column, "rd_observed")
  expect_identical(design$intervention_tolerance_weeks, 4L)
  expect_identical(design$comparator_tolerance_weeks, 8L)
})

test_that("TTEDesign rejects a malformed observation encoding or tolerance", {
  expect_error(
    lc_design(observed_var = list(sentinel = "any_row")),
    "which swereg does not know"
  )
  expect_error(
    lc_design(observed_var = list(column = "a", sentinel = "row_presence")),
    "gives both `column` and `sentinel`"
  )
  expect_error(
    lc_design(intervention_tolerance_weeks = -1L),
    "must be a single whole number of weeks"
  )
  expect_error(
    lc_design(comparator_tolerance_weeks = 2.5),
    "must be a single whole number of weeks"
  )
})

# ---------------------------------------------------------------------------
# 4. The s1 cache allow-list
# ---------------------------------------------------------------------------

test_that(".tte_s1_cache_columns() retains the observation column", {
  sk <- lc_skeleton()
  es <- list(design = lc_design(observed_var = list(column = "rd_observed")))
  spec <- list(confounders = list())

  cols <- swereg:::.tte_s1_cache_columns(sk, es, spec)
  expect_true("rd_observed" %in% cols)

  # The sentinel names no column, so nothing extra is kept.
  es_sentinel <- list(
    design = lc_design(observed_var = list(sentinel = "row_presence"))
  )
  cols_sentinel <- swereg:::.tte_s1_cache_columns(sk, es_sentinel, spec)
  expect_false("rd_observed" %in% cols_sentinel)
})

test_that(".tte_canonical_needed_cols() retains the observation column", {
  # This projection runs BEFORE the cache allow-list, so a column missing
  # here never reaches the allow-list at all.
  sk <- lc_skeleton()
  es <- list(list(
    design = lc_design(observed_var = list(column = "rd_observed")),
    treatment_impl = list(variable = "rd_exposure")
  ))
  spec <- list(enrollments = list(), confounders = list(), outcomes = list())

  cols <- swereg:::.tte_canonical_needed_cols(spec, es, names(sk))
  expect_true("rd_observed" %in% cols)
})

# ---------------------------------------------------------------------------
# 5. Schema versions
# ---------------------------------------------------------------------------

# One assertion per constant, in its own test_that(). A single joint test
# would report the same label whichever constant moved, so a mutation of one
# could not be told apart from a mutation of another.

test_that("the TTEDesign schema version is 3L", {
  expect_identical(swereg:::.TTE_DESIGN_SCHEMA_VERSION, 3L)
})

test_that("the TTEEnrollment schema version is 3L", {
  expect_identical(swereg:::.TTE_ENROLLMENT_SCHEMA_VERSION, 3L)
})

test_that("the TTEPlan schema version is 3L", {
  expect_identical(swereg:::.TTE_PLAN_SCHEMA_VERSION, 3L)
})

# ---------------------------------------------------------------------------
# 6. reload_spec classifies the three fields as structural
# ---------------------------------------------------------------------------

test_that(".diff_specs() calls the observation encoding and both tolerances structural", {
  path_old <- lc_write_spec(list(lc_enrollment("01")))
  path_new <- lc_write_spec(list(
    lc_enrollment(
      "01",
      observed_var = list(column = "rd_observed"),
      intervention_tolerance_weeks = 4L,
      comparator_tolerance_weeks = 8L
    )
  ))
  old <- swereg::tteplan_read_spec(path_old)
  new <- swereg::tteplan_read_spec(path_new)

  diffs <- swereg:::.diff_specs(old, new)
  expect_length(diffs$cosmetic, 0L)
  expect_true(any(grepl("observed_var", diffs$structural)))
  expect_true(any(grepl("intervention_tolerance_weeks", diffs$structural)))
  expect_true(any(grepl("comparator_tolerance_weeks", diffs$structural)))
})
