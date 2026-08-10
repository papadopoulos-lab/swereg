# Cumulative-incidence option on the weighted survival curve.
#
# `$survival_curve()` now delegates plot construction to the pure internal
# renderer `.render_survival_curve()`, which can draw either scale:
#   scale = "survival"           -> plots surv, starting at 100%
#   scale = "cumulative_failure" -> plots 1 - surv, starting at 0
#
# Deaths are censored rather than modelled as a competing risk, so `1 - surv`
# is CAUSE-SPECIFIC failure and not a competing-risk cumulative incidence
# function. The y label has to say so, and one test below pins that wording.
#
# Fixture: the canonical 9-row panel from test-tte_classes.R. Hand-computed
# survivals are S_TRUE(4) = 2/3, S_TRUE(8) = 1/3, S_FALSE(4) = 1,
# S_FALSE(8) = 1/2, so the cumulative failures are 1/3, 2/3, 0 and 1/2.

skip_if_not_installed("ggplot2")
skip_if_not_installed("data.table")

# --- fixture ---------------------------------------------------------------

cif_trial <- function(weight_name = "w") {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:9,
    # One person per trial, so every curve value below is unchanged.
    id = 1:9,
    exposed = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    tstop = c(4L, 4L, 4L, 4L, 4L, 8L, 8L, 8L, 8L),
    event = c(0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L),
    w = c(1, 1, 1, 2, 2, 1, 1, 2, 2),
    age = 50,
    death = 0L
  )
  data.table::setnames(dt, "w", weight_name)
  design <- swereg::TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )
  swereg::TTEEnrollment$new(dt, design)
}

# Drive the REAL path -- `$survival_curve()` with a save_path -- and hand back
# the ggplot the method actually rendered. The mock wraps the genuine renderer
# (captured before mocking, so there is no recursion) rather than replacing it,
# so what is asserted on is the production object, not a test-built lookalike.
cif_rendered_plot <- function(trial, scale = NULL) {
  real_renderer <- swereg:::.render_survival_curve
  captured <- NULL
  testthat::local_mocked_bindings(
    .render_survival_curve = function(...) {
      captured <<- real_renderer(...)
      captured
    },
    .package = "swereg"
  )
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  if (is.null(scale)) {
    trial$survival_curve(weight_col = "w", save_path = png)
  } else {
    trial$survival_curve(weight_col = "w", save_path = png, scale = scale)
  }
  captured
}

# --- assertion 1 -----------------------------------------------------------

test_that("cumulative-failure curve starts at 0", {
  p_c <- cif_rendered_plot(cif_trial(), scale = "cumulative_failure")
  ld <- ggplot2::layer_data(p_c, 1L)
  # The origin row carries surv = 1. If it is bound in without being
  # transformed, the curve starts at 1 and dives -- plausible on screen and
  # completely wrong.
  expect_equal(sort(unique(ld$y[ld$x == 0])), 0)
})

# --- assertion 2 -----------------------------------------------------------

test_that("cumulative-failure curve plots 1 - surv at the last period", {
  p_c <- cif_rendered_plot(cif_trial(), scale = "cumulative_failure")
  ld <- ggplot2::layer_data(p_c, 1L)
  # S_TRUE(8) = 1/3 and S_FALSE(8) = 1/2, so 1 - S is 2/3 and 1/2.
  expect_equal(sort(ld$y[ld$x == 8]), c(1 / 2, 2 / 3))
})

# --- assertion 3 -----------------------------------------------------------

test_that("cumulative-failure y label says cause-specific, not cumulative incidence", {
  p_c <- cif_rendered_plot(cif_trial(), scale = "cumulative_failure")
  # get_labs() is the supported ggplot2 4.0 accessor; `p$labels` is not
  # guaranteed populated.
  y_lab <- ggplot2::get_labs(p_c)$y
  expect_false(grepl("cumulative incidence", y_lab, ignore.case = TRUE))
  expect_true(grepl("cause-specific", y_lab, ignore.case = TRUE))
})

# --- assertion 6 (regression guard on the default path) --------------------

test_that("survival scale is unchanged: starts at 1 and reads as event-free survival", {
  p_s <- cif_rendered_plot(cif_trial(), scale = "survival")
  ld <- ggplot2::layer_data(p_s, 1L)
  expect_equal(sort(unique(ld$y[ld$x == 0])), 1)
  expect_equal(sort(ld$y[ld$x == 8]), c(1 / 3, 1 / 2))
  expect_true(grepl(
    "event-free survival",
    ggplot2::get_labs(p_s)$y,
    ignore.case = TRUE
  ))
})

test_that("survival is the default scale", {
  p_d <- cif_rendered_plot(cif_trial())
  ld <- ggplot2::layer_data(p_d, 1L)
  expect_equal(sort(unique(ld$y[ld$x == 0])), 1)
})

# --- assertion 4: survival_curve() delegates ------------------------------

test_that("survival_curve delegates plot construction to .render_survival_curve", {
  called <- FALSE
  got_scale <- NULL
  testthat::local_mocked_bindings(
    .render_survival_curve = function(curve, time_var, scale = NULL, ...) {
      called <<- TRUE
      got_scale <<- scale
      ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
        ggplot2::geom_point()
    },
    .package = "swereg"
  )
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  cif_trial()$survival_curve(
    weight_col = "w",
    save_path = png,
    scale = "cumulative_failure"
  )
  # A renderer called only by its own test is not wired. This fails if
  # survival_curve() goes back to building its ggplot inline.
  expect_true(called)
  expect_identical(got_scale, "cumulative_failure")
})

# --- assertion 5: the production caller requests cumulative failure --------

test_that(".export_figure requests the cumulative-failure scale", {
  skip_if_not_installed("qs2")

  dir <- tempfile("cif_export")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  trial <- cif_trial(weight_name = "analysis_weight_pp_trunc")
  swereg::qs2_write_atomic(
    list(data = trial$data, design = trial$design),
    file.path(dir, "analysis_001.qs2")
  )

  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = "ETT00001",
    outcome_var = "death",
    outcome_name = "Death",
    follow_up = 52L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "age",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = "analysis_001.qs2",
    file_analysis_itt = "analysis_itt_001.qs2",
    description = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$output_dir <- dir

  # s3 first. The figure reads the STORED curve, so the plan must hold one.
  # The production worker writes it, so the fixture is the real result and not
  # a hand-built lookalike.
  res <- swereg:::.s3_ett_worker(
    analysis_path = file.path(dir, "analysis_001.qs2"),
    method = "risk_difference",
    weight_col = "analysis_weight_pp_trunc",
    ett_id = "ETT00001",
    n_threads = 1L,
    subgroup_var = NULL,
    conf_level = 0.95
  )
  plan$results_ett <- list(ETT00001 = res)

  real_renderer <- swereg:::.render_survival_curve
  got_scale <- NULL
  testthat::local_mocked_bindings(
    .render_survival_curve = function(..., scale) {
      got_scale <<- scale
      real_renderer(..., scale = scale)
    },
    .package = "swereg"
  )

  spec <- list(
    type = "survival",
    enrollment = "01",
    outcome = "death",
    follow_up = 52L,
    age_group = "50_59",
    estimands = "pp",
    label = "surv"
  )
  out <- plan$.__enclos_env__$private$.export_figure(
    spec,
    file.path(dir, "fig")
  )

  # This is a RUNTIME proof, not a syntax-tree check: the plan's own
  # .export_figure() ran and the renderer it reached saw the scale below.
  expect_true(file.exists(out))
  expect_identical(got_scale, "cumulative_failure")
})
