# The flagged items of swereg #56. One test per item, each seen red before
# its fix.

# Item 2. A ratio of 0.004 gets labelled ticks down to its lower bound. Before
# the fix, the first tick was 0.1.
test_that("the forest axis labels a tick below 0.1 when a ratio needs one", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  df <- data.table::data.table(
    ett_id = c("ETT00001", "ETT00002"),
    enrollment_id = "01",
    enrollment_name = "Exposure A",
    outcome_name = c("Outcome A", "Outcome B"),
    outcome_description = c("Outcome A", "Outcome B"),
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
    irr = c(0.004, 1.5),
    lo = c(0.002, 0.9),
    hi = c(0.008, 2.5),
    pvalue = 0.1,
    irr_estimable = TRUE
  )
  r <- swereg:::.render_combined_forest_plot(
    df,
    arm_labels = c(comparator = "Comparator", intervention = "Intervention")
  )
  panel <- r$plot[[length(r$plot)]]
  pp <- ggplot2::ggplot_build(panel)$layout$panel_params[[1]]
  shown <- !is.na(pp$x$get_breaks())
  expect_equal(
    10^as.numeric(pp$x$get_breaks()[shown]),
    c(0.003, 0.01, 0.03, 0.1, 0.25, 0.5, 1, 2)
  )
  expect_identical(
    pp$x$get_labels()[shown],
    c("0.003", "0.01", "0.03", "0.1", "0.25", "0.5", "1", "2")
  )
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

# Item 8. At 1e34 comparator events both ratios round to 1. Before the fix,
# the strict orientation assertion stopped.
test_that(".tte_mde accepts two ratios that round to 1", {
  r <- NULL
  expect_no_error(r <- swereg:::.tte_mde(1e34, 1, 1))
  expect_identical(r$mde_protective, 1)
  expect_identical(r$mde_harmful, 1)
})
