test_that(".swereg_table1 returns a long-format data.table with the expected columns", {
  set.seed(1)
  d <- data.table::data.table(
    age = c(rnorm(50, 55, 5), rnorm(50, 60, 5)),
    edu = factor(c(rep("primary", 40), rep("secondary", 40), rep("uni", 20))),
    exp = c(rep(FALSE, 50), rep(TRUE, 50))
  )
  t1 <- swereg:::.swereg_table1(
    d,
    vars = c("age", "edu"),
    strata = "exp",
    include_smd = TRUE,
    show_missing = "when_present"
  )
  expect_s3_class(t1, "swereg_table1")
  expect_s3_class(t1, "data.table")
  expect_true(all(c("Variable", "Level", "Overall", "FALSE", "TRUE", "SMD") %in%
                    names(t1)))
  expect_true(t1$Variable[1] == "n")
  expect_true(any(grepl("^age", t1$Variable)))
  expect_true(any(grepl("^edu", t1$Variable)))
})

test_that("show_missing = 'when_present' emits a Missing row only for vars with missing", {
  d <- data.table::data.table(
    edu = factor(c(rep("primary", 40), rep("secondary", 40),
                   rep("uni", 18), NA, NA)),
    age = rnorm(100, 55, 5),
    exp = c(rep(FALSE, 50), rep(TRUE, 50))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = c("edu", "age"), strata = "exp",
    show_missing = "when_present"
  )
  # edu has 2 missing -> expect a Missing row
  edu_miss <- t1[Level == "Missing"]
  expect_true(nrow(edu_miss) == 1L)
  # age has no missing -> expect no Missing row for age
  age_rows <- t1[grepl("^age", Variable)]
  expect_equal(nrow(age_rows), 1L)
})

test_that("show_missing = 'always' emits a Missing row for every variable", {
  d <- data.table::data.table(
    age = rnorm(100, 55, 5),
    edu = factor(rep(c("a", "b"), times = 50)),
    exp = c(rep(FALSE, 50), rep(TRUE, 50))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = c("age", "edu"), strata = "exp",
    show_missing = "always"
  )
  # Expect one Missing row per variable (age + edu = 2)
  expect_equal(sum(t1$Level == "Missing"), 2L)
})

test_that("show_missing = 'none' suppresses all Missing rows", {
  d <- data.table::data.table(
    edu = factor(c(rep("primary", 40), rep("secondary", 40),
                   rep("uni", 18), NA, NA)),
    exp = c(rep(FALSE, 50), rep(TRUE, 50))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = "edu", strata = "exp",
    show_missing = "none"
  )
  expect_false(any(t1$Level == "Missing"))
})

test_that("percentages sum to 100 when Missing row is emitted (total-N denominator)", {
  d <- data.table::data.table(
    edu = factor(c(rep("primary", 40), rep("secondary", 40),
                   rep("uni", 18), NA, NA)),
    exp = c(rep(FALSE, 50), rep(TRUE, 50))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = "edu", strata = "exp",
    show_missing = "when_present"
  )
  edu_rows <- t1[Variable == "edu" | (Variable == "" & Level != "")]
  pcts <- as.numeric(sub(".* \\(([0-9.]+)%\\)", "\\1", edu_rows$Overall))
  expect_equal(sum(pcts), 100, tolerance = 0.1)
})

test_that("percentages sum to 100 when Missing is suppressed (non-missing denominator)", {
  d <- data.table::data.table(
    edu = factor(c(rep("primary", 40), rep("secondary", 40),
                   rep("uni", 18), NA, NA)),
    exp = c(rep(FALSE, 50), rep(TRUE, 50))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = "edu", strata = "exp",
    show_missing = "none"
  )
  edu_rows <- t1[Variable == "edu" | (Variable == "" & Level != "")]
  pcts <- as.numeric(sub(".* \\(([0-9.]+)%\\)", "\\1", edu_rows$Overall))
  expect_equal(sum(pcts), 100, tolerance = 0.1)
})

test_that(".swereg_table1 respects include_smd = FALSE", {
  d <- data.table::data.table(
    age = rnorm(50),
    exp = c(rep(FALSE, 25), rep(TRUE, 25))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = "age", strata = "exp",
    include_smd = FALSE
  )
  expect_false("SMD" %in% names(t1))
})

test_that(".swereg_table1 weighted mean matches closed-form expectation", {
  set.seed(2)
  x <- rnorm(200, 50, 10)
  w <- runif(200, 0.5, 1.5)
  d <- data.table::data.table(
    age = x,
    w = w,
    exp = c(rep(FALSE, 100), rep(TRUE, 100))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = "age", strata = "exp", weights = "w"
  )
  age_row <- t1[grepl("^age", Variable)][1]
  ow <- as.numeric(sub("^([0-9.-]+) .*", "\\1", age_row$Overall))
  expected <- sum(w * x) / sum(w)
  expect_equal(ow, expected, tolerance = 0.01)
})

test_that(".swereg_table1 SMD is zero for identical groups", {
  d <- data.table::data.table(
    age = rep(c(50, 60), each = 50),
    grp = factor(rep(c("a", "b"), times = 50))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = "age", strata = "grp", include_smd = TRUE
  )
  smd <- as.numeric(t1[grepl("^age", Variable)]$SMD[1])
  expect_equal(smd, 0, tolerance = 0.01)
})

test_that(".swereg_table1 SMD is non-zero for different groups", {
  set.seed(3)
  d <- data.table::data.table(
    age = c(rnorm(100, 50, 5), rnorm(100, 60, 5)),
    grp = c(rep(FALSE, 100), rep(TRUE, 100))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = "age", strata = "grp", include_smd = TRUE
  )
  smd <- as.numeric(t1[grepl("^age", Variable)]$SMD[1])
  expect_gt(smd, 1.5)
})

test_that(".swereg_table1 uses arm_labels when supplied", {
  d <- data.table::data.table(
    age = rnorm(50),
    exp = c(rep(FALSE, 25), rep(TRUE, 25))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = "age", strata = "exp",
    arm_labels = c(comparator = "Local MHT", intervention = "Systemic MHT")
  )
  expect_true("Local MHT" %in% names(t1))
  expect_true("Systemic MHT" %in% names(t1))
  expect_false("FALSE" %in% names(t1))
  expect_false("TRUE" %in% names(t1))
})

test_that(".swereg_table1 stops on unknown variables", {
  d <- data.table::data.table(age = rnorm(10), exp = c(rep(FALSE, 5), rep(TRUE, 5)))
  expect_error(
    swereg:::.swereg_table1(d, vars = "nonexistent", strata = "exp"),
    "not found in data"
  )
})

test_that(".swereg_table1 stops when strata has != 2 levels", {
  d <- data.table::data.table(
    age = rnorm(10),
    grp = c("a", "b", "c", rep("a", 7))
  )
  expect_error(
    swereg:::.swereg_table1(d, vars = "age", strata = "grp"),
    "exactly two non-missing levels"
  )
})

test_that("continuous variable emits a Missing row under 'always'", {
  set.seed(4)
  d <- data.table::data.table(
    age = c(rnorm(98, 55, 5), NA, NA),
    exp = c(rep(FALSE, 50), rep(TRUE, 50))
  )
  t1 <- swereg:::.swereg_table1(
    d, vars = "age", strata = "exp", show_missing = "always"
  )
  # Expect two rows: the mean(SD) row and the Missing row
  age_rows <- t1[grepl("^age", Variable) | Level == "Missing"]
  expect_equal(nrow(age_rows), 2L)
  expect_true("Missing" %in% age_rows$Level)
})
