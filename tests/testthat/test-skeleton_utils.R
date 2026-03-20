# =============================================================================
# Tests for skeleton utility functions
# =============================================================================

test_that("skeleton_save stamps created_at attribute on saved files", {
  dir <- withr::local_tempdir()
  dt <- data.table::data.table(
    id = rep(1:3, each = 2),
    week = rep(1:2, 3),
    val = letters[1:6]
  )

  before <- Sys.time()
  path <- skeleton_save(dt, batch_number = 1, output_dir = dir)
  after <- Sys.time()

  expect_true(file.exists(path))
  loaded <- qs2_read(path)
  ts <- attr(loaded, "created_at")

  expect_s3_class(ts, "POSIXct")
  expect_true(ts >= before)
  expect_true(ts <= after)
})

test_that("skeleton_save produces correct filename", {
  dir <- withr::local_tempdir()
  dt <- data.table::data.table(
    id = rep(1:6, each = 2),
    week = rep(1:2, 6),
    val = letters[1:12]
  )

  path <- skeleton_save(dt, batch_number = 2, output_dir = dir)
  expect_equal(length(path), 1)
  expect_true(file.exists(path))
  expect_true(grepl("skeleton_002\\.qs2$", path))
})

test_that("skeleton_checkpoint records time and memory", {
  cp <- skeleton_checkpoint()
  cp("start")
  Sys.sleep(0.01)
  cp("end")
  prof <- cp(done = TRUE)

  expect_s3_class(prof, "data.table")
  expect_equal(nrow(prof), 2)
  expect_equal(prof$label, c("start", "end"))
  expect_true(all(c("time", "mem_mb") %in% names(prof)))
  expect_true(prof$time[2] > prof$time[1])
})
