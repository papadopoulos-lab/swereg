# The Coverage (ETT) baseline counts read the entry-window snapshot
# `.tte_entry__<v>` of each confounder, the value IPW and Table 1 use. In
# the baseline rows of this panel the current value differs from the entry
# value of every confounder that has a snapshot. `reg` has no snapshot, so
# it pins the per-confounder fallback to its own column.
.cvb_panel <- function() {
  return(data.table::data.table(
    tstart = c(0, 0, 0, 0, 1),
    tx = c(TRUE, TRUE, FALSE, FALSE, TRUE),
    dm = c(FALSE, TRUE, TRUE, FALSE, TRUE),
    .tte_entry__dm = c(TRUE, TRUE, FALSE, FALSE, TRUE),
    civ = c("a", "c", "c", "b", "c"),
    .tte_entry__civ = c("a", "b", "a", "b", "a"),
    age = c(NA, 40, 30, NA, 1),
    .tte_entry__age = c(50, 40, NA, 60, 1),
    reg = c("x", "y", "x", NA, "x")
  ))
}

.cvb_coverage <- function() {
  enr <- list(
    data = .cvb_panel(),
    design = list(
      tstart_var = "tstart",
      treatment_var = "tx",
      confounder_vars = c("dm", "civ", "age", "reg")
    )
  )
  return(swereg:::.tte_confounder_coverage(enr))
}

test_that("A: a logical confounder counts TRUE on its entry snapshot", {
  cov <- .cvb_coverage()
  dm <- cov[variable == "dm"]
  expect_identical(dm$level, "TRUE")
  expect_identical(dm$n_intervention, 2L)
  expect_identical(dm$n_comparator, 0L)
})

test_that("B: a character confounder counts the levels of its entry snapshot", {
  cov <- .cvb_coverage()
  civ <- cov[variable == "civ"]
  expect_false("c" %in% civ$level)
  expect_identical(civ$level, c("a", "b"))
  expect_identical(civ$n_intervention, c(1L, 1L))
  expect_identical(civ$n_comparator, c(1L, 1L))
})

test_that("C: a numeric confounder counts non-missing entry values", {
  cov <- .cvb_coverage()
  age <- cov[variable == "age"]
  expect_identical(age$level, "(non-missing)")
  expect_identical(age$n_intervention, 2L)
  expect_identical(age$n_comparator, 1L)
})

test_that("D: a confounder with no snapshot counts its own column", {
  cov <- .cvb_coverage()
  reg <- cov[variable == "reg"]
  expect_identical(reg$level, c("x", "y"))
  expect_identical(reg$n_intervention, c(1L, 1L))
  expect_identical(reg$n_comparator, c(1L, 0L))
})
