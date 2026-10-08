# Export, forest and survival-curve checks added in 27.2.0.
#
# Each block pins one issue. Each failed before the fix, and the failure was
# silent or the error named the wrong thing:
#
#   #35 a CONSORT or forest exhibit that drew nothing returned a path. A PNG
#       from an earlier run then sat at that path and looked current.
#   #26 a survival exhibit without `age_group` failed inside data.table, and an
#       ETT with no age group could not be selected at all.
#   #32 an empty `estimands` wrote nothing and raised nothing. A manifest
#       element that was not a list failed with an unrelated message.
#   #29 an outcome with no role rendered as "Stroke ()".
#   #30 `group_by = "outcome"` labelled each row with the enrollment name, and
#       ordered the outcomes as the ETT grid stores them.
#   #28 an NA treatment drew a third curve, and one arm drew a curve with no
#       contrast.

skip_if_not_installed("data.table")
skip_if_not_installed("ggplot2")

# --- fixtures --------------------------------------------------------------

# Four ETTs: two enrollments by two outcomes. The ETT grid stores Outcome B
# first, and the spec lists Outcome A first, so the two orders differ.
# Outcome B has no role.
ec_plan <- function() {
  ett <- data.table::data.table(
    enrollment_id = c("01", "01", "02", "02"),
    ett_id = c("ETT00001", "ETT00002", "ETT00003", "ETT00004"),
    outcome_var = c("osd_b", "osd_a", "osd_b", "osd_a"),
    outcome_name = c("Outcome B", "Outcome A", "Outcome B", "Outcome A"),
    outcome_role = c(NA_character_, "primary", NA_character_, "primary"),
    follow_up = 52L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "age",
    person_id_var = "lopnr",
    treatment_var = "tx",
    file_imp = "imp.qs2",
    file_raw = "raw.qs2",
    file_analysis = paste0("a", 1:4, ".qs2"),
    description = c("ETT00001", "ETT00002", "ETT00003", "ETT00004")
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "ec",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$spec <- list(
    outcomes = list(
      list(
        name = "Outcome A",
        role = "primary",
        implementation = list(variable = "osd_a")
      ),
      list(name = "Outcome B", implementation = list(variable = "osd_b"))
    )
  )
  one <- function(eid, enr) {
    rt <- data.table::data.table(
      tx = c(TRUE, FALSE),
      events_weighted = c(10.4, 20.6),
      py_weighted = c(1000, 2000),
      rate_per_100000py = c(1040, 1030)
    )
    data.table::setattr(rt, "treatment_var", "tx")
    return(list(
      enrollment_id = enr,
      description = eid,
      irr_pp_trunc = list(
        IRR = 1.5,
        IRR_lower = 0.9,
        IRR_upper = 2.5,
        IRR_pvalue = 0.1,
        skipped = FALSE
      ),
      rates_pp_trunc = rt
    ))
  }
  plan$results_ett <- list(
    ETT00001 = one("ETT00001", "01"),
    ETT00002 = one("ETT00002", "01"),
    ETT00003 = one("ETT00003", "02"),
    ETT00004 = one("ETT00004", "02")
  )
  return(plan)
}

ec_exposures <- list(
  "Exposure X" = c("ETT00001", "ETT00002"),
  "Exposure Y" = c("ETT00003", "ETT00004")
)

# Run the REAL forest export and return the row text the real renderer built.
ec_forest_rows <- function(plan, spec, dir) {
  real_renderer <- swereg:::.render_combined_forest_plot
  got <- NULL
  testthat::local_mocked_bindings(
    .render_combined_forest_plot = function(...) {
      got <<- real_renderer(...)
      return(got)
    },
    .package = "swereg"
  )
  out <- swereg:::.plan_export(plan, list(spec), dir)
  rows <- got$text[!is.na(got$text$ett_id)]
  return(list(out = out, rows = rows))
}

# One PNG from an earlier run, at the path the exhibit writes.
ec_stale_png <- function(dir, name) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, name)
  writeBin(as.raw(1:8), path)
  return(path)
}

# --- #35: an exhibit that draws nothing MUST stop ---------------------------

ec_consort <- function(render) {
  testthat::local_mocked_bindings(
    .plan_counted_enrollment_ids = function(plan) "01",
    .plan_cohort_counts = function(plan, eid) list(),
    .enrollment_label = function(plan, eid) "Enrollment 01",
    .render_consort_sidecars = render,
    .package = "swereg",
    .env = parent.frame()
  )
}

test_that("#35 consort: a NULL render stops and leaves no stale PNG", {
  dir <- tempfile("ec_consort_null")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  stale <- ec_stale_png(dir, "01_consort.png")
  ec_consort(function(...) NULL)
  spec <- list(type = "consort", enrollment = "01", label = "consort")
  expect_error(
    swereg:::.plan_export(ec_plan(), list(spec), dir),
    "CONSORT figure for enrollment '01' in exhibit spec 1 was not rendered"
  )
  expect_false(file.exists(stale))
})

test_that("#35 consort: a render that writes no PNG stops", {
  dir <- tempfile("ec_consort_nopng")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  stale <- ec_stale_png(dir, "01_consort.png")
  ec_consort(function(...) list(png = stale, pdf = "x.pdf"))
  spec <- list(type = "consort", enrollment = "01", label = "consort")
  expect_error(
    swereg:::.plan_export(ec_plan(), list(spec), dir),
    "consort figure in exhibit spec 1 wrote no image"
  )
  expect_false(file.exists(stale))
})

test_that("#35 consort: a render that writes the PNG returns its path", {
  dir <- tempfile("ec_consort_ok")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  ec_consort(function(..., output_dir, img_basename) {
    writeBin(as.raw(1:8), file.path(output_dir, paste0(img_basename, ".png")))
    return(list(png = "p", pdf = "q"))
  })
  spec <- list(type = "consort", enrollment = "01", label = "consort")
  out <- swereg:::.plan_export(ec_plan(), list(spec), dir)
  expect_identical(out, file.path(dir, "01_consort.png"))
})

test_that("#35 forest: a writer that draws nothing stops and leaves no stale PNG", {
  skip_if_not_installed("openxlsx")
  dir <- tempfile("ec_forest_null")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  stale <- ec_stale_png(dir, "01_forest_pp.png")
  testthat::local_mocked_bindings(
    .write_forest_irr = function(...) invisible(NULL),
    .package = "swereg"
  )
  spec <- list(type = "forest", exposures = ec_exposures, label = "forest")
  expect_error(
    swereg:::.plan_export(ec_plan(), list(spec), dir),
    "forest figure in exhibit spec 1 wrote no image"
  )
  expect_false(file.exists(stale))
})

# --- #26: age_group -----------------------------------------------------------

ec_survival_plan <- function() {
  plan <- ec_plan()
  # One ETT with no age group beside one with an age group.
  plan$ett[1L, age_group := NA_character_]
  plan$ett[3L, `:=`(enrollment_id = "01", outcome_var = "osd_b")]
  return(plan)
}

ec_survival_spec <- function(...) {
  return(utils::modifyList(
    list(
      type = "survival",
      enrollment = "01",
      outcome = "osd_b",
      follow_up = 52L,
      label = "surv"
    ),
    list(...)
  ))
}

test_that("#26 a survival exhibit without age_group names age_group", {
  dir <- tempfile("ec_surv_noage")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  expect_error(
    swereg:::.plan_export(ec_survival_plan(), list(ec_survival_spec()), dir),
    "exhibit spec 1 needs exactly one 'age_group'"
  )
})

test_that("#26 age_group = NA selects the ETT with no age group", {
  dir <- tempfile("ec_surv_na")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # The ETT matched: the export got as far as reading its stored curve.
  expect_error(
    swereg:::.plan_export(
      ec_survival_plan(),
      list(ec_survival_spec(age_group = NA)),
      dir
    ),
    "no stored survival curve for ETT00001"
  )
  expect_error(
    swereg:::.plan_export(
      ec_survival_plan(),
      list(ec_survival_spec(age_group = "50_59")),
      dir
    ),
    "no stored survival curve for ETT00003"
  )
})

# --- #32: empty estimands and a non-list manifest element ---------------------

test_that("#32 an empty estimands stops and names the manifest index", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("patchwork")
  dir <- tempfile("ec_empty_est")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  ok <- list(type = "forest", exposures = ec_exposures, label = "forest")
  empty <- utils::modifyList(ok, list(estimands = character(0)))
  expect_error(
    swereg:::.plan_export(ec_plan(), list(ok, empty), dir),
    "exhibit spec 2 has an empty 'estimands'"
  )
  expect_error(
    swereg:::.plan_export(
      ec_survival_plan(),
      list(ec_survival_spec(age_group = NA, estimands = character(0))),
      dir
    ),
    "exhibit spec 1 has an empty 'estimands'"
  )
})

test_that("#32 a manifest element that is not a list names the manifest index", {
  dir <- tempfile("ec_nonlist")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  expect_error(
    swereg:::.plan_export(ec_plan(), list("forest"), dir),
    "exhibit spec 1 must be a list, got class 'character'"
  )
  expect_error(
    swereg:::.plan_export(ec_plan(), list(mean), dir),
    "exhibit spec 1 must be a list, got class 'function'"
  )
})

# --- #29: no "()" for an outcome with no role --------------------------------

test_that("#29 the label formatter drops a parenthesised empty field", {
  row <- data.table::data.table(
    outcome_name = "Stroke",
    outcome_role = NA_character_
  )
  expect_identical(
    swereg:::.forest_format_label("{outcome_name} ({outcome_role})", row),
    "Stroke"
  )
  row$outcome_role <- "primary"
  expect_identical(
    swereg:::.forest_format_label("{outcome_name} ({outcome_role})", row),
    "Stroke (primary)"
  )
})

test_that("#29 the forest export draws no '()' for an outcome with no role", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("patchwork")
  dir <- tempfile("ec_role")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  spec <- list(type = "forest", exposures = ec_exposures, label = "forest")
  res <- ec_forest_rows(ec_plan(), spec, dir)
  expect_identical(
    res$rows$txt_desc,
    c("Outcome B", "Outcome A (primary)", "Outcome B", "Outcome A (primary)")
  )
})

# --- #30: group_by = "outcome" ------------------------------------------------

test_that("#30 group_by outcome labels rows by exposure, in spec outcome order", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("patchwork")
  dir <- tempfile("ec_by_outcome")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  spec <- list(
    type = "forest",
    exposures = ec_exposures,
    group_by = "outcome",
    label = "forest"
  )
  res <- ec_forest_rows(ec_plan(), spec, dir)
  expect_identical(res$out, file.path(dir, "01_forest_pp.png"))
  expect_true(file.exists(res$out))
  expect_identical(
    res$rows$ett_id,
    c("ETT00002", "ETT00004", "ETT00001", "ETT00003")
  )
  expect_identical(
    res$rows$group_label,
    c("Outcome A", "Outcome A", "Outcome B", "Outcome B")
  )
  expect_identical(
    res$rows$txt_desc,
    c("Exposure X", "Exposure Y", "Exposure X", "Exposure Y")
  )
})

# --- #28: the treatment of a survival curve -----------------------------------

ec_trial <- function() {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:9,
    id = 1:9,
    exposed = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    tstop = c(4L, 4L, 4L, 4L, 4L, 8L, 8L, 8L, 8L),
    event = c(0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L),
    w = c(1, 1, 1, 2, 2, 1, 1, 2, 2),
    age = 50,
    death = 0L
  )
  design <- swereg::TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )
  return(swereg::TTEEnrollment$new(dt, design))
}

test_that("#28 an NA treatment stops the curve and the plot", {
  trial <- ec_trial()
  trial$data[1L, exposed := NA]
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  expect_error(
    trial$survival_curve(weight_col = "w"),
    "treatment 'exposed' must be non-missing"
  )
  expect_error(
    trial$survival_curve(weight_col = "w", save_path = png),
    "treatment 'exposed' must be non-missing"
  )
  expect_false(file.exists(png))
})

test_that("#28 a single-arm treatment stops the curve and the plot", {
  trial <- ec_trial()
  trial$data[, exposed := TRUE]
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  expect_error(
    trial$survival_curve(weight_col = "w"),
    "treatment 'exposed' must have both arms"
  )
  expect_error(
    trial$survival_curve(weight_col = "w", save_path = png),
    "treatment 'exposed' must have both arms"
  )
  expect_false(file.exists(png))
})

test_that("#28 two non-missing arms still give a curve", {
  curve <- ec_trial()$survival_curve(weight_col = "w")
  expect_setequal(unique(curve$exposed), c(TRUE, FALSE))
})
