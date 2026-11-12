# The Arm sizes sheet of feasibility.xlsx flags an enrollment whose comparator
# pool is smaller than its intervention arm, so the arms can be reversed.

.as_plan <- function(matching, ratio = 2) {
  enr <- function(id, int, cmp) {
    return(list(
      id = id,
      name = paste(int, "vs", cmp),
      treatment = list(
        arms = list(intervention = int, comparator = cmp),
        implementation = list(comparator_to_intervention_ratio = ratio)
      )
    ))
  }
  return(list(
    spec = list(
      enrollments = list(
        enr("A", "Oral", "Transdermal"),
        enr("B", "Systemic", "None"),
        enr("C", "Tibolone", "None")
      )
    ),
    get_matching = function() {
      return(matching)
    }
  ))
}

test_that("a comparator rarer than the intervention is flagged, summed over trials", {
  m <- data.table::data.table(
    enrollment_id = c("A", "A", "B", "C"),
    enrollment_period_id = c(1L, 2L, 1L, 1L),
    # A: 30 intervention, 20 comparator over two trials: rarer.
    # B: 10 intervention, 50 comparator: fills 2:1.
    # C: 10 intervention, 15 comparator: larger pool, short of 2:1.
    n_intervention_total = c(10, 20, 10, 10),
    n_comparator_total = c(15, 5, 50, 15),
    n_intervention_enrolled = c(10, 20, 10, 10),
    n_comparator_enrolled = c(15, 5, 20, 15)
  )
  tab <- swereg:::.tte_arm_sizes(.as_plan(m))
  expect_identical(tab$enrollment_id, c("A", "B", "C"))
  expect_identical(tab$n_intervention_eligible, c(30, 10, 10))
  expect_identical(tab$n_comparator_eligible, c(20, 50, 15))
  expect_identical(tab$comparator_rarer, c(TRUE, FALSE, FALSE))
  expect_identical(tab$comparator_short, c(FALSE, FALSE, TRUE))
  expect_identical(tab$ratio_eligible, c(0.67, 5, 1.5))
  expect_identical(tab$intervention[1], "Oral")
  expect_identical(tab$comparator[1], "Transdermal")
  expect_identical(
    tab$flag,
    c(
      swereg:::.ARM_SIZE_FLAGS[["rarer"]],
      "",
      swereg:::.ARM_SIZE_FLAGS[["short"]]
    )
  )
  expect_output(swereg:::.arm_sizes_report(tab), "reversing the arms: A")
})

test_that("a plan with no comparator-draw counts gives an empty table and an empty sheet", {
  skip_if_not_installed("openxlsx")
  empty <- swereg:::.acc_empty(swereg:::.ACC_SCHEMA$matching)
  plan <- .as_plan(empty)
  tab <- swereg:::.tte_arm_sizes(plan)
  expect_identical(nrow(tab), 0L)
  expect_silent(swereg:::.arm_sizes_report(tab))
  wb <- openxlsx::createWorkbook()
  swereg:::.write_arm_sizes_sheet(wb, plan, openxlsx::createStyle())
  expect_identical(names(wb), "Arm sizes")
})

test_that("the Arm sizes sheet writes one row per enrollment with the flag", {
  skip_if_not_installed("openxlsx")
  m <- data.table::data.table(
    enrollment_id = c("A", "B"),
    enrollment_period_id = c(1L, 1L),
    n_intervention_total = c(30, 10),
    n_comparator_total = c(20, 50),
    n_intervention_enrolled = c(30, 10),
    n_comparator_enrolled = c(20, 20)
  )
  wb <- openxlsx::createWorkbook()
  swereg:::.write_arm_sizes_sheet(wb, .as_plan(m), openxlsx::createStyle())
  path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, path)
  d <- openxlsx::read.xlsx(path, sheet = "Arm sizes", startRow = 4L)
  expect_identical(d$Enrollment, c("A", "B"))
  expect_identical(d$Flag[1], swereg:::.ARM_SIZE_FLAGS[["rarer"]])
  expect_true(is.na(d$Flag[2]) || identical(d$Flag[2], ""))
})
