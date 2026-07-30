test_that("qs2_read round-trips a standard qs2 file", {
  path <- withr::local_tempfile(fileext = ".qs2")
  obj <- list(a = 1:3, b = "x")
  qs2::qs_save(obj, path)
  expect_identical(qs2_read(path), obj)
})

test_that("qs2_read errors on a qdata-format file", {
  path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qd_save(list(a = 1:3), path)
  expect_error(qs2_read(path), "qdata format detected")
})

test_that("qs2_read errors on a corrupt file", {
  path <- withr::local_tempfile(fileext = ".qs2")
  writeBin(as.raw(rep(c(1L, 2L, 3L, 250L), 200L)), path)
  expect_error(qs2_read(path), "Unknown file format detected")
})

test_that("qs2_read errors on a missing file", {
  path <- withr::local_tempfile(fileext = ".qs2")
  expect_false(file.exists(path))
  expect_error(qs2_read(path), "Failed to open for reading")
})

test_that("qs2_read invokes check_version() on an R6-like object", {
  path <- withr::local_tempfile(fileext = ".qs2")
  obj <- new.env(parent = emptyenv())
  obj$check_version <- function() stop("check_version was invoked")
  qs2::qs_save(obj, path)
  expect_error(qs2_read(path), "check_version was invoked")
})
