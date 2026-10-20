# The median, quartiles and range rows of a continuous variable in Table 1.
#
# The weighted p-quantile is the first distinct value at which the cumulative
# normalised weight reaches p, after the values are sorted and tied values are
# pooled. Where that cumulative weight equals p, the quantile is the mean of
# that value and the next distinct value. Every expected value below is
# computed by hand from that definition, and the comment beside each one shows
# the cumulative weights.

skip_if_not_installed("data.table")

test_that("weighted quartiles: hand-computed example", {
  # Sorted: 10 (w 1), 20 (w 2), 30 (w 3), 40 (w 4). Total 10.
  # Cumulative normalised weight: 0.1, 0.3, 0.6, 1.0.
  # Q1: first >= 0.25 is 20. Median: first >= 0.5 is 30. Q3: first >= 0.75
  # is 40.
  q <- swereg:::.t1_wtd_quartiles(c(40, 10, 30, 20), c(4, 1, 3, 2))
  expect_identical(q$q1, 20)
  expect_identical(q$median, 30)
  expect_identical(q$q3, 40)
  expect_identical(q$min, 10)
  expect_identical(q$max, 40)
})

test_that("weighted quartiles: a cumulative weight that reaches p exactly", {
  # Sorted: 1 (w 1), 5 (w 1), 5 (w 2). The value 3 has weight 0 and the value
  # 100 has weight 0, so neither enters. Total 4.
  # Distinct values: 1 (cumulative 0.25), 5 (cumulative 1.00).
  # Q1: 0.25 is reached exactly at 1, so Q1 is the mean of 1 and 5, 3.
  # Median: 0.5 is first reached at 5. Q3: 5.
  q <- swereg:::.t1_wtd_quartiles(c(5, 1, 5, 3, 100), c(1, 1, 2, 0, 0))
  expect_identical(q$q1, 3)
  expect_identical(q$median, 5)
  expect_identical(q$q3, 5)
  # The range reads only the rows with a positive weight.
  expect_identical(q$min, 1)
  expect_identical(q$max, 5)
})

test_that("weighted quartiles: tied values", {
  # Sorted: 2, 2, 2, 8, equal weights. Distinct values: 2 (cumulative
  # 0.75), 8 (cumulative 1). Q1 and the median are 2, because 2 passes 0.25
  # and 0.5. Q3: 0.75 is reached exactly at 2, so Q3 is the mean of 2 and 8,
  # 5. quantile(type = 2) gives the same three values.
  q <- swereg:::.t1_wtd_quartiles(c(8, 2, 2, 2), NULL)
  expect_identical(q$q1, 2)
  expect_identical(q$median, 2)
  expect_identical(q$q3, 5)
  expect_identical(q$min, 2)
  expect_identical(q$max, 8)
})

test_that("weighted quartiles: a cumulative weight just below p by rounding", {
  # Four weights of 0.7 give a cumulative normalised weight of
  # 0.7499999999999999 at the third value. In exact arithmetic it is 0.75, so
  # Q3 is the mean of the third and fourth values, 3.5. Q1 and the median are
  # reached exactly too: 1.5 and 2.5.
  w <- rep(0.7, 4)
  expect_lt((cumsum(w) / sum(w))[3], 0.75)
  q <- swereg:::.t1_wtd_quartiles(1:4, w)
  expect_identical(q$q1, 1.5)
  expect_identical(q$median, 2.5)
  expect_identical(q$q3, 3.5)
})

test_that("equal weights give quantile type 2 and stats::median()", {
  # 40 values, so n * p is a whole number for all three quartiles, and the
  # averaging rule decides each one. Type 1 would give other values.
  set.seed(11)
  x <- sample.int(1000L, 40L) / 10
  expect_identical(anyDuplicated(x), 0L)
  q <- swereg:::.t1_wtd_quartiles(x, NULL)
  expect_equal(
    c(q$q1, q$median, q$q3),
    unname(stats::quantile(x, c(0.25, 0.5, 0.75), type = 2)),
    tolerance = 1e-12
  )
  expect_false(isTRUE(all.equal(
    unname(stats::quantile(x, c(0.25, 0.5, 0.75), type = 2)),
    unname(stats::quantile(x, c(0.25, 0.5, 0.75), type = 1))
  )))
  expect_equal(q$median, stats::median(x), tolerance = 1e-12)
  # The four values of the Table 1 example: median 25, as stats::median().
  q4 <- swereg:::.t1_wtd_quartiles(c(10, 20, 30, 40), NULL)
  expect_identical(c(q4$q1, q4$median, q4$q3), c(15, 25, 35))
  expect_identical(q4$median, stats::median(c(10, 20, 30, 40)))
})

test_that("weighted quartiles: negative weights do not enter", {
  # 100 has weight -5, so it does not enter. Sorted: 1, 2, 3, equal weights.
  # Cumulative 1/3, 2/3, 1: Q1 = 1, median = 2, Q3 = 3. The range is 1 to 3.
  q <- swereg:::.t1_wtd_quartiles(c(1, 2, 3, 100), c(1, 1, 1, -5))
  expect_identical(c(q$q1, q$median, q$q3, q$min, q$max), c(1, 2, 3, 1, 3))
})

test_that("weighted quartiles: missing weights do not enter", {
  # 100 has a missing weight, so it does not enter. The result is the same as
  # in the test of negative weights.
  q <- swereg:::.t1_wtd_quartiles(c(1, 2, 3, 100), c(1, 1, 1, NA))
  expect_identical(c(q$q1, q$median, q$q3, q$min, q$max), c(1, 2, 3, 1, 3))
})

test_that("weighted quartiles: one surviving row gives its value", {
  # The missing value and the zero weight leave the row 7 alone.
  q <- swereg:::.t1_wtd_quartiles(c(7, NA, 9), c(2, 1, 0))
  expect_identical(c(q$q1, q$median, q$q3, q$min, q$max), rep(7, 5))
})

test_that("weighted quartiles: weights near the largest double", {
  # sum(c(1e308, 1e308)) is Inf. Equal weights on 5 and 9: cumulative 0.5, 1.
  # Q1 = 5, median = mean of 5 and 9 = 7, Q3 = 9.
  q <- swereg:::.t1_wtd_quartiles(c(9, 5), c(1e308, 1e308))
  expect_identical(c(q$q1, q$median, q$q3, q$min, q$max), c(5, 7, 9, 5, 9))
})

test_that("weighted quartiles: zero total weight gives missing values", {
  na5 <- list(
    q1 = NA_real_,
    median = NA_real_,
    q3 = NA_real_,
    min = NA_real_,
    max = NA_real_
  )
  expect_identical(swereg:::.t1_wtd_quartiles(c(1, 2, 3), c(0, 0, 0)), na5)
  expect_identical(swereg:::.t1_wtd_quartiles(c(NA, NA), c(1, 1)), na5)
  expect_identical(swereg:::.t1_wtd_quartiles(numeric(0), NULL), na5)
})

test_that("Table 1: a continuous variable gets median and range rows", {
  d <- data.table::data.table(
    age = c(10, 20, 30, 40, 15, 25, 35, NA),
    w = c(1, 2, 3, 4, 1, 1, 1, 1),
    exp = c(rep(FALSE, 4), rep(TRUE, 4))
  )
  t1 <- swereg:::.swereg_table1(
    d,
    vars = "age",
    strata = "exp",
    weights = "w",
    show_missing = "always"
  )
  expect_identical(
    t1$Level,
    c("", "", "", "Median (Q1, Q3)", "Range (min, max)", "Missing")
  )
  expect_identical(
    t1$Variable,
    c("N", "Sum of weights", "age (mean (SD))", "", "", "")
  )
  # Overall, weighted. Sorted: 10 (1), 15 (1), 20 (2), 25 (1), 30 (3),
  # 35 (1), 40 (4); the NA row does not enter. Total 13. Cumulative:
  # 1, 2, 4, 5, 8, 9, 13 over 13 = 0.077, 0.154, 0.308, 0.385, 0.615,
  # 0.692, 1. Q1 = 20, median = 30, Q3 = 40.
  expect_identical(t1$Overall[4], "30.00 (20.00, 40.00)")
  expect_identical(t1$Overall[5], "10.00, 40.00")
  # Comparator, weighted: 0.1, 0.3, 0.6, 1.0 as in the first test.
  expect_identical(t1[["FALSE"]][4], "30.00 (20.00, 40.00)")
  expect_identical(t1[["FALSE"]][5], "10.00, 40.00")
  # Intervention, weighted: 15, 25, 35 with equal weights; the NA row does not
  # enter. Cumulative 1/3, 2/3, 1. Q1 = 15, median = 25, Q3 = 35.
  expect_identical(t1[["TRUE"]][4], "25.00 (15.00, 35.00)")
  expect_identical(t1[["TRUE"]][5], "15.00, 35.00")
  # The two new rows carry no SMD.
  expect_identical(t1$SMD[4:5], c("", ""))
  expect_true(all(is.na(t1$smd_numeric[4:5])))
  # The mean (SD) row keeps its meaning: the weighted mean and SD.
  expect_identical(t1[["FALSE"]][3], "30.00 (11.95)")
})

test_that("Table 1: unweighted median uses equal weights", {
  d <- data.table::data.table(
    age = c(10, 20, 30, 40, 15, 25, 35, NA),
    w = c(1, 2, 3, 4, 1, 1, 1, 1),
    exp = c(rep(FALSE, 4), rep(TRUE, 4))
  )
  t1 <- swereg:::.swereg_table1(d, vars = "age", strata = "exp")
  # Comparator, equal weights: cumulative 0.25, 0.5, 0.75, 1 at 10, 20, 30,
  # 40. Each quartile is reached exactly, so each is a mean of two values:
  # Q1 = 15, median = 25, Q3 = 35.
  expect_identical(t1[["FALSE"]][4], "25.00 (15.00, 35.00)")
  # The range is the same as in the weighted panel, whose weights are all
  # positive.
  expect_identical(t1[["FALSE"]][5], "10.00, 40.00")
})

test_that("Table 1: an arm with no positive weight shows empty cells", {
  d <- data.table::data.table(
    age = c(10, 20, 30, 40),
    w = c(1, 1, 0, 0),
    exp = c(FALSE, FALSE, TRUE, TRUE)
  )
  t1 <- swereg:::.swereg_table1(
    d,
    vars = "age",
    strata = "exp",
    weights = "w",
    show_missing = "none"
  )
  expect_identical(t1[["TRUE"]][4:5], c("", ""))
  # Comparator: 10 and 20, equal weights, cumulative 0.5 and 1. Q1 = 10,
  # median = mean of 10 and 20 = 15, Q3 = 20.
  expect_identical(t1[["FALSE"]][4:5], c("15.00 (10.00, 20.00)", "10.00, 20.00"))
})

test_that("get_baselines keys the new rows under the continuous variable", {
  d <- data.table::data.table(
    age = c(10, 20, 30, 40, 15, 25, 35, NA),
    exp = c(rep(FALSE, 4), rep(TRUE, 4))
  )
  t1 <- swereg:::.swereg_table1(
    d,
    vars = "age",
    strata = "exp",
    show_missing = "always"
  )
  rows <- swereg:::.acc_baseline_rows(
    "01",
    list(imputation = "imputed", weighting = "none", variant = "supplementary"),
    t1,
    list(
      n_baseline = 8,
      n_baseline_intervention = 4,
      n_baseline_comparator = 4
    )
  )
  # The rows that existed before keep their (variable, level) keys.
  expect_identical(
    rows$variable[rows$level %in% c("", "Missing")][3:4],
    c("age (mean (SD))", "age (mean (SD))")
  )
  expect_identical(
    rows[level == "Median (Q1, Q3)"]$variable,
    "age (mean (SD))"
  )
  expect_identical(
    rows[level == "Range (min, max)"]$variable,
    "age (mean (SD))"
  )
})

test_that("weighted quartiles: the midpoint of two values near the largest double", {
  # (1e308 + 1.2e308) / 2 overflows to Inf. Half of each gives 1.1e308.
  q <- swereg:::.t1_wtd_quartiles(c(1e308, 1.2e308), NULL)
  expect_true(is.finite(q$median))
  expect_equal(q$median, 1.1e308, tolerance = 1e-12)
})
