# Numbers at risk, drawn under the survival panel.
#
# `.render_survival_curve()` composes two panels: the curve on top, and a
# numbers-at-risk table beneath it. Only two things about that table are worth
# a mechanical check, and both are checked here.
#
#   1. WHICH NUMBER IT PRINTS. Three quantities live in the same arm-band cell
#      and are easy to confuse: the row count (person-trials), `at_risk` (the
#      WEIGHTED risk set, sum(w), which is the hazard denominator) and
#      `n_persons_at_risk` (distinct people). A risk table reports people.
#      Reading the table off `at_risk` is the plausible wrong turn: it is the
#      column sitting next door, it is already on the curve object, and on a
#      figure it prints numbers that look like counts.
#
#   2. WHETHER THE COLUMNS LINE UP. A risk table whose columns do not sit
#      under the curve's ticks is worse than no risk table: it invites the
#      reader to attach a count to the wrong time. Misalignment is the actual
#      failure mode, and it is readable from the two built scales -- the panel
#      x range and the x breaks -- without touching a grob.
#
# Fixture: phase 3's panel from test-tte_at_risk_counts.R, reused because it
# separates the two candidate sources in every single cell. Three people hold
# five trials:
#
#   p1 -> p1_trialA, p1_trialB      (exposed)
#   p2 -> p2_trialA                 (exposed)
#   p3 -> p3_trialC, p3_trialD      (unexposed)
#
# and the weights are 0.5 for the exposed trials and 2 for the unexposed ones,
# giving four arm-band cells in which the two candidate sources never agree:
#
#   exposed  tstop   n_persons_at_risk   at_risk (weighted)
#   FALSE        4                   1                  4.0
#   FALSE        8                   1                  4.0
#   TRUE         4                   2                  1.5
#   TRUE         8                   2                  1.0
#
# So a table populated from the wrong column cannot pass by coincidence.

skip_if_not_installed("ggplot2")
skip_if_not_installed("data.table")
skip_if_not_installed("patchwork")

# --- fixture ---------------------------------------------------------------

risk_table_trial <- function() {
  dt <- data.table::data.table(
    enrollment_person_trial_id = c(
      "p1_trialA",
      "p1_trialB",
      "p2_trialA",
      "p3_trialC",
      "p3_trialD",
      "p1_trialA",
      "p2_trialA",
      "p3_trialC",
      "p3_trialD"
    ),
    id = c("p1", "p1", "p2", "p3", "p3", "p1", "p2", "p3", "p3"),
    exposed = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    tstop = c(4L, 4L, 4L, 4L, 4L, 8L, 8L, 8L, 8L),
    event = c(0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L),
    w = c(0.5, 0.5, 0.5, 2, 2, 0.5, 0.5, 2, 2),
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
  swereg::TTEEnrollment$new(dt, design)
}

# The arm names the legend uses, taken from the same lookup the production
# exporter calls, so the table cannot be labelled from a private list of names.
risk_table_arm_labels <- function() {
  spec <- list(
    enrollments = list(
      list(
        id = "01",
        treatment = list(
          arms = list(
            intervention = "Intervention",
            comparator = "Comparator"
          )
        )
      )
    )
  )
  swereg:::.lookup_arm_labels(spec, "01")
}

# Drive the REAL path -- `$survival_curve()` with a save_path -- and hand back
# the object the method actually rendered. The mock wraps the genuine renderer
# (captured before mocking, so there is no recursion) rather than replacing it,
# so what is asserted on is the production object.
risk_table_rendered <- function(scale = "survival", arm_labels = NULL) {
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
  risk_table_trial()$survival_curve(
    weight_col = "w",
    save_path = png,
    scale = scale,
    arm_labels = arm_labels
  )
  captured
}

# Split the composition into its two panels by ROLE, not by index. The table is
# the panel carrying the risk-table title; the curve is the other one. Indexing
# blind would silently swap the two if the composition order ever changed.
#
# THIS HELPER NEVER ABORTS. A renderer that returns the bare curve, with no
# table composed, gives `table = NULL` and the curve unchanged. That is the
# whole point: an abort here would end every affected block as an ERROR before
# a single expectation ran, so the tests would go red because the fixture blew
# up and not because an assertion detected the missing table. A causally-red
# proof needs the intended assertion to be the thing that fires.
risk_table_panels <- function(p) {
  if (!inherits(p, "patchwork") || length(p) != 2L) {
    return(list(table = NULL, curve = p))
  }
  panels <- list(p[[1]], p[[2]])
  titles <- vapply(
    panels,
    function(x) {
      lab <- ggplot2::get_labs(x)$title
      if (is.null(lab)) "" else as.character(lab)
    },
    character(1)
  )
  i_tbl <- which(titles == "Numbers at risk (persons)")
  if (length(i_tbl) != 1L) {
    # Two panels, but not exactly one risk table among them. No mutation run
    # against this file reaches this branch; it returns a value rather than
    # aborting so that the helper's contract stays uniform.
    return(list(table = NULL, curve = NULL))
  }
  list(table = panels[[i_tbl]], curve = panels[[3L - i_tbl]])
}

# The risk table's text cells, as plain data an expectation can judge: one row
# per printed number, with its x position, its row position and its label. A
# composition with no table panel gives a ZERO-ROW frame, so "the table is
# missing" reads as an expectation failure.
risk_table_cells <- function(p) {
  tbl <- risk_table_panels(p)$table
  if (is.null(tbl)) {
    return(data.frame(x = numeric(0), y = numeric(0), label = character(0)))
  }
  ld <- ggplot2::layer_data(tbl, 1L)
  # ggplot2 returns a discrete position as class `mapped_discrete`.
  out <- data.frame(
    x = as.numeric(ld$x),
    y = as.numeric(ld$y),
    label = as.character(ld$label)
  )
  out[order(out$y, out$x), ]
}

# The risk table's rows, one per arm, labelled as the y scale prints them. A
# composition with no table panel gives a zero-length character vector.
risk_table_arms <- function(p) {
  tbl <- risk_table_panels(p)$table
  if (is.null(tbl)) {
    return(character(0))
  }
  as.character(
    ggplot2::ggplot_build(tbl)$layout$panel_params[[1]]$y$get_labels()
  )
}

# The x range and the x breaks of each panel, as built. A panel that does not
# exist gives NULL, and `expect_equal(NULL, <numeric>)` is a failure, not an
# error.
risk_table_x_params <- function(p) {
  pan <- risk_table_panels(p)
  pp <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    ggplot2::ggplot_build(x)$layout$panel_params[[1]]
  }
  pp_table <- pp(pan$table)
  pp_curve <- pp(pan$curve)
  list(
    range_table = if (is.null(pp_table)) NULL else pp_table$x.range,
    range_curve = if (is.null(pp_curve)) NULL else pp_curve$x.range,
    breaks_table = if (is.null(pp_table)) {
      NULL
    } else {
      as.numeric(pp_table$x$get_breaks())
    },
    breaks_curve = if (is.null(pp_curve)) {
      NULL
    } else {
      as.numeric(pp_curve$x$get_breaks())
    }
  )
}

# Each panel built, or NULL where the panel does not exist.
risk_table_built <- function(p) {
  lapply(risk_table_panels(p), function(x) {
    if (is.null(x)) NULL else ggplot2::ggplot_build(x)
  })
}

# --- assertion 6: the labelled times are evenly spaced ----------------------
#
# Six-digit counts are wide. Two of them collide unless the gap between
# adjacent labelled times is wide enough to hold them. A rule that keeps every
# k-th band and then adds the last band leaves a final gap of `(n - 1) %% k`
# bands, which is shorter than every other gap by construction. On a real
# 156-week national-registry panel that final gap was 12 weeks against a
# 20-week stride. Two six-digit counts then printed on top of each other.
#
# The assertions below pin the PROPERTY, not the numbers: every gap is the same
# width, and the last band is always labelled. Neither depends on how many
# columns the rule picks or on how long the follow-up is.

test_that("no labelled time is closer to its neighbour than the stride", {
  # Three follow-up horizons a real study uses, in 4-week bands.
  for (last in c(52L, 156L, 260L)) {
    times <- seq(4L, last, by = 4L)
    brk <- swereg:::.risk_table_break_times(times)
    gaps <- diff(brk)

    # No short gap anywhere, including the final one.
    expect_equal(min(gaps), max(gaps))
    # The last band carries the number readers look up, so it is never dropped.
    expect_equal(brk[length(brk)], last)
    expect_lte(length(brk), 8L)
  }
})

test_that("the labelled bands are equally spaced by index, whatever the bands", {
  # Unconditional form of the same property: the selection is an arithmetic
  # sequence of INDICES ending at the last index. It holds even if the observed
  # band times are not equally spaced, which the loop above cannot show.
  times <- as.numeric(c(1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233))
  brk <- swereg:::.risk_table_break_times(times, max_n = 4L)
  idx <- match(brk, times)

  expect_equal(min(diff(idx)), max(diff(idx)))
  expect_equal(idx[length(idx)], length(times))
  expect_lte(length(brk), 4L)
})

test_that("a follow-up short enough to label in full keeps every band", {
  times <- seq(4L, 32L, by = 4L)
  expect_equal(swereg:::.risk_table_break_times(times), times)
})

# --- the fixture separates the two candidate sources ------------------------

test_that("the fixture's person counts and weighted risk sets share no value", {
  curve <- risk_table_trial()$survival_curve(weight_col = "w")

  expect_equal(curve$exposed, c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(curve$tstop, c(4L, 8L, 4L, 8L))
  expect_equal(curve$n_persons_at_risk, c(1L, 1L, 2L, 2L))
  expect_equal(curve$at_risk, c(4, 4, 1.5, 1))
  # Cell by cell, never equal: no arm-band can pass assertion 1 by coincidence.
  expect_false(any(curve$n_persons_at_risk == curve$at_risk))
})

# --- assertion 1: the table prints persons, not weights ---------------------

test_that("the risk table is populated from n_persons_at_risk", {
  cells <- risk_table_cells(risk_table_rendered(
    arm_labels = risk_table_arm_labels()
  ))

  # y = 1 is the bottom row (comparator), y = 2 the top row (intervention),
  # matching the legend order in which the intervention is listed first.
  expect_equal(cells$x, c(4, 8, 4, 8))
  expect_equal(cells$y, c(1, 1, 2, 2))

  # Persons: 1, 1 for the comparator and 2, 2 for the intervention. The
  # weighted risk sets in the same four cells are 4, 4, 1.5 and 1, so this
  # vector is reachable only from `n_persons_at_risk`.
  expect_equal(cells$label, c("1", "1", "2", "2"))
})

# --- assertion 2: the two panels share one x scale --------------------------

test_that("the risk table panel shares the curve panel's x limits and breaks", {
  xp <- risk_table_x_params(risk_table_rendered(
    arm_labels = risk_table_arm_labels()
  ))

  expect_equal(xp$range_table, xp$range_curve)
  expect_equal(xp$breaks_table, xp$breaks_curve)

  # Pinned, so "identical to each other" cannot be satisfied by both panels
  # drifting together. Bands are 4 and 8; the limits are 0 to 8 with the
  # default 5% continuous expansion on each side.
  expect_equal(xp$range_curve, c(-0.4, 8.4))
  expect_equal(xp$breaks_curve, c(4, 8))
})

# --- assertion 3: one row per arm, named as the legend names them -----------

test_that("the risk table has one row per arm, labelled from .lookup_arm_labels", {
  labs <- risk_table_arm_labels()
  p <- risk_table_rendered(arm_labels = labs)

  # A renderer that returns the bare curve composes no table, so this vector is
  # empty and every expectation below FAILS. It does not error: the property
  # this block claims to cover is the thing that detects the missing table.
  arms <- risk_table_arms(p)

  expect_length(arms, 2L)
  expect_equal(arms, c(labs[["comparator"]], labs[["intervention"]]))
  expect_equal(arms, c("Comparator", "Intervention"))

  # One text row per arm, and no more.
  expect_equal(sort(unique(risk_table_cells(p)$y)), c(1, 2))
})

# --- assertion 4: both panels really build ----------------------------------

test_that("both panels build and the composed figure is written to disk", {
  built <- risk_table_built(risk_table_rendered(
    arm_labels = risk_table_arm_labels()
  ))

  expect_s3_class(built$curve, "ggplot_built")
  expect_s3_class(built$table, "ggplot_built")

  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  risk_table_trial()$survival_curve(
    weight_col = "w",
    save_path = png,
    scale = "cumulative_failure",
    arm_labels = risk_table_arm_labels()
  )
  expect_true(file.exists(png))
  expect_gt(file.size(png), 0)
})

# --- assertion 5: the curve panel did not regress ---------------------------

test_that("the curve panel still starts cumulative failure at 0 and says cause-specific", {
  p <- risk_table_rendered(
    scale = "cumulative_failure",
    arm_labels = risk_table_arm_labels()
  )
  pan <- risk_table_panels(p)

  ld <- ggplot2::layer_data(pan$curve, 1L)
  expect_equal(sort(unique(ld$y[ld$x == 0])), 0)

  y_lab <- ggplot2::get_labs(pan$curve)$y
  expect_false(grepl("cumulative incidence", y_lab, ignore.case = TRUE))
  expect_true(grepl("cause-specific", y_lab, ignore.case = TRUE))

  # The composition's own ggplot is the CURVE, so callers that treat the
  # return value as the curve keep working.
  expect_equal(ggplot2::get_labs(p)$y, y_lab)
  expect_equal(sort(unique(ggplot2::layer_data(p, 1L)$y[
    ggplot2::layer_data(p, 1L)$x == 0
  ])), 0)
})
