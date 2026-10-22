# The flagged items of swereg #56. One test per item, each seen red before
# its fix.

# Item 2. The forest panel of a figure with the given rows. `breaks` and
# `labels` are the ticks the panel shows, after the scale limits clip them.
.i56_forest_axis <- function(irr, lo, hi) {
  n <- length(irr)
  df <- data.table::data.table(
    ett_id = sprintf("ETT%05d", seq_len(n)),
    enrollment_id = "01",
    enrollment_name = "Exposure A",
    outcome_name = paste("Outcome", seq_len(n)),
    outcome_description = paste("Outcome", seq_len(n)),
    outcome_role = "primary",
    follow_up = 52L,
    intervention_name = "Intervention",
    comparator_name = "Comparator",
    group_label = "Exposure A",
    events_intervention = 10,
    py_intervention = 1000,
    rate_intervention = 1000,
    events_comparator = 20,
    py_comparator = 2000,
    rate_comparator = 1000,
    irr = irr,
    lo = lo,
    hi = hi,
    pvalue = 0.1,
    irr_estimable = TRUE
  )
  r <- swereg:::.render_combined_forest_plot(
    df,
    arm_labels = c(comparator = "Comparator", intervention = "Intervention")
  )
  panel <- r$plot[[length(r$plot)]]
  pp <- ggplot2::ggplot_build(panel)$layout$panel_params[[1]]
  b <- as.numeric(pp$x$get_breaks())
  shown <- !is.na(b)
  return(list(
    breaks = 10^b[shown],
    labels = pp$x$get_labels()[shown],
    range = 10^pp$x.range
  ))
}

# A lower bound of 0.002 needs a labelled tick at or below it, inside the
# drawn range. Before the fix, the first tick was 0.1. A first fix added 0.001
# as a break, but the data range clipped it, so the first visible tick was
# 0.003.
test_that("the forest axis labels a tick at or below the smallest bound", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  ax <- .i56_forest_axis(
    irr = c(0.004, 1.5),
    lo = c(0.002, 0.9),
    hi = c(0.008, 2.5)
  )
  expect_lte(ax$breaks[[1L]], 0.002)
  expect_gte(ax$breaks[[1L]], ax$range[[1L]])
  expect_equal(
    ax$breaks,
    c(0.001, 0.003, 0.01, 0.03, 0.1, 0.25, 0.5, 1, 2)
  )
  expect_identical(
    ax$labels,
    c("0.001", "0.003", "0.01", "0.03", "0.1", "0.25", "0.5", "1", "2")
  )
})

# Nothing below 0.1, so the axis is the one it was before item 2: the same
# visible ticks and the same drawn range, measured on 27.2.1 before the change.
test_that("the forest axis is unchanged when no bound is below 0.1", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  ax <- .i56_forest_axis(
    irr = c(0.95, 1.2),
    lo = c(0.8, 0.9),
    hi = c(1.1, 1.6)
  )
  expect_equal(ax$breaks, 1)
  expect_identical(ax$labels, "1")
  expect_equal(ax$range, c(0.773, 1.656), tolerance = 1e-3)
})

# Item 3. A parenthesised group that starts with an empty field goes, for any
# field. Before the fix, only `({outcome_role})` went.
test_that(".forest_format_label drops ({key}...) when the field is empty", {
  row <- list(outcome_name = "Outcome A", follow_up = NA, ett_id = "ETT00001")
  expect_identical(
    swereg:::.forest_format_label("{outcome_name} ({follow_up}w)", row),
    "Outcome A"
  )
  row$follow_up <- ""
  expect_identical(
    swereg:::.forest_format_label("{outcome_name} ({follow_up}w)", row),
    "Outcome A"
  )
  row$follow_up <- 52L
  expect_identical(
    swereg:::.forest_format_label("{outcome_name} ({follow_up}w)", row),
    "Outcome A (52w)"
  )
  expect_identical(
    swereg:::.forest_format_label(
      "{outcome_name} ({outcome_role})",
      list(outcome_name = "Outcome A", outcome_role = NA, ett_id = "ETT00001")
    ),
    "Outcome A"
  )
})

# Item 4. Before the fix, add_annual() wrote `isoyear` into `data`.
test_that("add_annual leaves every column of the caller's data unchanged", {
  sk <- data.table::data.table(
    id = rep(1:3, each = 2L),
    isoyear = 2021L,
    isoyearweek = rep(c("2021-01", "2021-02"), times = 3L),
    is_isoyear = FALSE
  )
  annual <- data.table::data.table(lopnr = 1:3, income = c(10, 20, 30))
  before <- data.table::copy(annual)
  sk <- swereg::add_annual(sk, annual, id_name = "lopnr", isoyear = 2021L)
  expect_identical(names(annual), c("lopnr", "income"))
  expect_equal(annual, before, ignore_attr = TRUE)
  expect_identical(sk[id == 2L, unique(income)], 20)
})

# Item 7. Before the fix, "0.0040" stood beside " 1.23" and "10.50".
test_that(".irr_combine_num pads every value to one width", {
  expect_identical(
    swereg:::.irr_combine_num(c(0.004, 1.234, 10.5, NA)),
    c("0.0040", "  1.23", " 10.50", "    NA")
  )
})

# Item 8. At 1e34 events in each arm both ratios round to 1. Before the fix,
# the strict orientation assertion stopped.
test_that(".tte_mde accepts two ratios that round to 1", {
  r <- NULL
  expect_no_error(r <- swereg:::.tte_mde(1e34, 1, 1e34, 1))
  expect_identical(r$mde_protective, 1)
  expect_identical(r$mde_harmful, 1)
})
