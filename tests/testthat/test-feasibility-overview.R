# The overview sheets at the front of feasibility.xlsx.

test_that("an attrition step reads as the spec criterion it applies", {
  spec <- list(
    exclusion_criteria = list(list(
      name = "Absent genital organs",
      implementation = list(source_variable = "osd_z907")
    )),
    enrollments = list(list(
      id = "M01",
      additional_inclusion = list(list(
        name = "No earlier systemic MHT",
        implementation = list(source_variable = "rd_approach1_single")
      ))
    ))
  )
  f <- swereg:::.overview_step_label
  expect_identical(f("before_exclusions", spec, "M01"), "Start: all person-trials")
  expect_identical(
    f("eligible_no_osd_z907_lifetime_before_baseline", spec, "M01"),
    "Excl. Absent genital organs"
  )
  expect_identical(
    f("eligible_has_rd_approach1_single_lifetime", spec, "M01"),
    "Incl. No earlier systemic MHT"
  )
  expect_identical(f("Landmark: observed", spec, "M01"), "Landmark: observed")
})

test_that("a Table 1 cell splits into its count and share", {
  x <- swereg:::.overview_split_cell(c("14,964 (7.5%)", "51.52 (3.38)", ""))
  expect_identical(x$value, c("14,964", "51.52 (3.38)", ""))
  expect_identical(x$pct, c("7.5", NA, NA))
})

test_that("a cut takes the fill of its band", {
  expect_identical(
    swereg:::.overview_cut_fill(c("-", "1", "4", "5", "10", "20", NA)),
    c("red", "orange", "orange", "yellow", "lightgreen", "green", NA)
  )
})

.ov_plan <- function() {
  att <- data.table::data.table(
    enrollment_id = rep(c("M01", "M02"), each = 3L),
    enrollment_period_id = NA_integer_,
    step_order = rep(1:3, 2L),
    step_name = rep(c("before_exclusions", "eligible_no_osd_z907_lifetime", "eligible_no_osd_i26_lifetime"), 2L),
    n_persons = c(100, 100, 90, 50, 50, 45),
    n_person_trials = c(100, 100, 90, 50, 50, 45),
    n_arm_intervention = c(20, 20, 18, 10, 10, 9),
    n_arm_comparator = c(80, 80, 72, 40, 40, 36)
  )
  counts <- data.table::CJ(
    enrollment_id = c("M01", "M02"),
    variable = c("rd_a", "rd_b"),
    arm = c("intervention", "comparator"),
    sorted = FALSE
  )
  counts[, n_persons := c(300L, 500L, 3L, 90L, 150L, 120L, 40L, 2L)]
  return(list(
    spec = list(
      exclusion_criteria = list(
        list(name = "Absent genital organs", implementation = list(source_variable = "osd_z907")),
        list(name = "VTE", implementation = list(source_variable = "osd_i26"))
      ),
      confounders = list(
        list(name = "Condition A", implementation = list(variable = "rd_a")),
        list(name = "Condition B", implementation = list(variable = "rd_b"))
      ),
      rare_condition_rule = list(min_n = 100, decided_by = list("M01"), candidates = list("rd_a", "rd_b")),
      enrollments = list(list(id = "M01", name = "One"), list(id = "M02", name = "Two"))
    ),
    sparse_counts = counts,
    get_attrition = function() {
      return(att)
    }
  ))
}

test_that("Attrition by arm writes each step once, with intervention and comparator columns", {
  skip_if_not_installed("openxlsx")
  wb <- openxlsx::createWorkbook()
  swereg:::.write_overview_attrition(wb, .ov_plan())
  path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, path)
  d <- openxlsx::read.xlsx(path, sheet = "Attrition by arm", colNames = FALSE, skipEmptyRows = FALSE)
  expect_true("Excl. Absent genital organs" %in% d[[2]])
  expect_true(any(d == "M01 intervention", na.rm = TRUE))
  expect_true(any(d == "M02 comparator", na.rm = TRUE))
})

test_that("Rare conditions by comparison shows the smaller arm, marks c and counts below min_n", {
  skip_if_not_installed("openxlsx")
  wb <- openxlsx::createWorkbook()
  expect_true(swereg:::.write_overview_rare(wb, .ov_plan()))
  path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, path)
  d <- openxlsx::read.xlsx(path, sheet = "Rare conditions by comparison", startRow = 7L, check.names = FALSE)
  expect_identical(names(d)[2:3], c("M01 (decides)", "M02"))
  # M01, rd_a: intervention 300, comparator 500 -> 300.
  # M01, rd_b: intervention 3, comparator 90 -> <5.
  # M02, rd_a: intervention 150, comparator 120 -> 120 c.
  # M02, rd_b: intervention 40, comparator 2 -> <5 c.
  expect_identical(d[[2]][1:2], c("300", "<5"))
  expect_identical(d[[3]][1:2], c("120 c", "<5 c"))
  expect_identical(as.numeric(d[["Comparisons < 100"]][1:2]), c(0, 2))
})
