# add_annual(): a person with more than one row in the annual data gets NA
# for that isoyear, with one warning that names the number of such persons.

make_dup_skeleton <- function(ids = c(1L, 2L)) {
  data.table::data.table(
    id = rep(ids, each = 2L),
    isoyear = 2017L,
    isoyearweek = rep(c("2017-01", "2017-02"), times = length(ids)),
    is_isoyear = FALSE
  )
}

collect_warnings <- function(expr) {
  msgs <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = msgs)
}

test_that("add_annual gives NA to a duplicated id and keeps the other id", {
  sk <- make_dup_skeleton()
  d <- data.table::data.table(lopnr = c(1L, 2L, 2L), x = c(10, 20, 30))
  res <- collect_warnings(
    add_annual(sk, d, id_name = "lopnr", isoyear = 2017L)
  )
  expect_identical(sk$id, c(1L, 1L, 2L, 2L))
  expect_identical(sk$x, c(10, 10, NA, NA))
  expect_length(res$warnings, 1L)
})

test_that("add_annual warns once with the count of duplicated ids", {
  sk <- make_dup_skeleton(ids = c(1L, 2L, 3L))
  d <- data.table::data.table(
    lopnr = c(1L, 2L, 2L, 3L, 3L, 3L),
    x = c(10, 20, 30, 40, 50, 60),
    y = c("a", "b", "c", "d", "e", "f")
  )
  res <- collect_warnings(
    add_annual(sk, d, id_name = "lopnr", isoyear = 2017L)
  )
  expect_length(res$warnings, 1L)
  expect_match(res$warnings, "^2 ID\\(s\\) have more than one row in data")
  expect_identical(sk$x, c(10, 10, NA, NA, NA, NA))
  expect_identical(sk$y, c("a", "a", NA, NA, NA, NA))

  sk2 <- make_dup_skeleton()
  d2 <- data.table::data.table(lopnr = c(1L, 2L, 2L), x = c(10, 20, 30))
  expect_warning(
    add_annual(sk2, d2, id_name = "lopnr", isoyear = 2017L),
    "1 ID(s) have more than one row in data for isoyear = 2017",
    fixed = TRUE
  )
})

test_that("add_annual sets NA on an existing column for a duplicated id", {
  sk <- make_dup_skeleton()
  sk[, x := c(1, 1, 2, 2)]
  d <- data.table::data.table(lopnr = c(1L, 2L, 2L), x = c(10, 20, 30))
  suppressWarnings(add_annual(sk, d, id_name = "lopnr", isoyear = 2017L))
  expect_identical(sk$x, c(10, 10, NA, NA))
})

test_that("add_annual leaves other isoyears of a duplicated id alone", {
  sk <- data.table::data.table(
    id = c(1L, 1L, 2L, 2L),
    isoyear = c(2017L, 2018L, 2017L, 2018L),
    isoyearweek = c("2017-01", "2018-01", "2017-01", "2018-01"),
    is_isoyear = FALSE
  )
  add_annual(
    sk,
    data.table::data.table(lopnr = c(1L, 2L), x = c(1, 2)),
    id_name = "lopnr",
    isoyear = 2018L
  )
  suppressWarnings(add_annual(
    sk,
    data.table::data.table(lopnr = c(1L, 2L, 2L), x = c(10, 20, 30)),
    id_name = "lopnr",
    isoyear = 2017L
  ))
  expect_identical(sk$x, c(10, 1, NA, 2))
})

test_that("add_annual gives no warning when no id is duplicated", {
  sk <- make_dup_skeleton()
  d <- data.table::data.table(lopnr = c(1L, 2L), x = c(10, 20))
  expect_no_warning(add_annual(sk, d, id_name = "lopnr", isoyear = 2017L))
  expect_identical(sk$x, c(10, 10, 20, 20))
})
