# .clear_s1_work_dir() removes the s1 work directory before a run starts.
#
# The count and the duration are the point. A run that inherits thousands of
# leftover chunk files spends minutes in unlink() before the first sub-step,
# and the log said nothing about it.

test_that(".clear_s1_work_dir() reports the file count and removes the directory", {
  work_dir <- file.path(withr::local_tempdir(), "proj003")
  dir.create(file.path(work_dir, "sub"), recursive = TRUE)
  writeLines("a", file.path(work_dir, "s1a_pre_enr1.qs2"))
  writeLines("b", file.path(work_dir, "s1b_enrolled_ids_enr1.qs2"))
  writeLines("c", file.path(work_dir, "sub", "s1c_panel_enr1.qs2"))

  out <- utils::capture.output(n <- .clear_s1_work_dir(work_dir))

  # Three files across two levels. The subdirectory itself is not counted.
  expect_identical(n, 3L)
  expect_length(out, 1L)
  expect_match(out, "^Cleared s1 work directory: 3 files in [0-9]+\\.[0-9] s$")
  expect_false(dir.exists(work_dir))
})

test_that(".clear_s1_work_dir() returns 0 and prints nothing when the directory is absent", {
  work_dir <- file.path(withr::local_tempdir(), "proj003")
  expect_false(dir.exists(work_dir))

  out <- utils::capture.output(n <- .clear_s1_work_dir(work_dir))

  expect_identical(n, 0L)
  expect_identical(out, character(0))
})
