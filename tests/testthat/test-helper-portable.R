# The helpers of helper-portable.R, run for both platforms on any platform.
# The Windows paths are the values that R-hub run 39403023594 printed on
# Windows R-devel.

test_that("md5_of_lines() ends each line in a line feed only", {
  f <- withr::local_tempfile()
  writeBin(charToRaw("a|1\nb|2\n"), f)
  expect_identical(md5_of_lines(c("a|1", "b|2")), unname(tools::md5sum(f)))
})

test_that("abs_path_start() accepts a drive letter on Windows only", {
  win <- paste0("^", abs_path_start("windows"))
  unix <- paste0("^", abs_path_start("unix"))
  backslashed <- paste0(
    "C:\\Users\\runneradmin\\AppData\\Local\\Temp\\RtmpMH79Kk\\working_dir",
    "\\RtmpKE6cx8\\file20b07c1a3e1\\tteplan/s1eatomic_raw_01.qs2"
  )
  slashed <- "C:/Users/runneradmin/AppData/Local/Temp/file21f053dd7aa1/project"

  expect_match(backslashed, win)
  expect_match(slashed, win)
  expect_no_match("/tmp/RtmpX/project", win)
  expect_no_match("project", win)
  expect_no_match("tteplan/s1eatomic_raw_01.qs2", win)

  expect_match("/tmp/RtmpX/project", unix)
  expect_no_match(backslashed, unix)
  expect_no_match(slashed, unix)
  expect_no_match("project", unix)
})

test_that("dirname_form() gives the separators of dirname() on Windows only", {
  root <- paste0(
    "C:\\Users\\RUNNER~1\\AppData\\Local\\Temp\\RtmpMH79Kk/working_dir",
    "\\RtmpOANGap\\file21f0c8326fb/swereg"
  )
  expect_identical(
    dirname_form(root, "windows"),
    paste0(
      "C:/Users/RUNNER~1/AppData/Local/Temp/RtmpMH79Kk/working_dir",
      "/RtmpOANGap/file21f0c8326fb/swereg"
    )
  )
  expect_identical(dirname_form(root, "unix"), root)
  expect_identical(dirname_form("/tmp/a/b", "unix"), "/tmp/a/b")
})
