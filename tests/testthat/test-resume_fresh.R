# .resume_fresh(): which existing outputs may resume = TRUE skip?
#
# The bug this exists for: the s2 resume check took max(mtime) across every
# existing analysis file and, if that ONE newest file was under 24h, skipped
# ALL existing files. A single fresh output therefore validated an arbitrarily
# old one -- a 100-hour-old file was silently reused because some unrelated
# sibling had been written an hour ago -- while the log printed "analysis files
# <24h old" about files that were nothing of the sort.
#
# Each file must be aged on its own. That is the whole contract here.

.touch <- function(path, hours_ago) {
  writeLines("x", path)
  t <- Sys.time() - as.difftime(hours_ago, units = "hours")
  Sys.setFileTime(path, t)
  path
}

test_that("each file is aged on its own -- one fresh file does not validate a stale one", {
  # THE regression. Under the old max(mtime) logic, `old` was skipped because
  # `new` happened to be recent.
  dir <- tempfile("resume_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  new <- .touch(file.path(dir, "new.qs2"), hours_ago = 1)
  old <- .touch(file.path(dir, "old.qs2"), hours_ago = 100)

  fresh <- swereg:::.resume_fresh(c(new, old))

  expect_equal(fresh, c(TRUE, FALSE))
})

test_that("a missing file is never fresh", {
  dir <- tempfile("resume_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  present <- .touch(file.path(dir, "here.qs2"), hours_ago = 1)
  absent <- file.path(dir, "gone.qs2")

  expect_equal(swereg:::.resume_fresh(c(present, absent)), c(TRUE, FALSE))
})

test_that("the cutoff is applied per file, at the boundary", {
  dir <- tempfile("resume_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  just_under <- .touch(file.path(dir, "a.qs2"), hours_ago = 23.5)
  just_over <- .touch(file.path(dir, "b.qs2"), hours_ago = 24.5)

  expect_equal(swereg:::.resume_fresh(c(just_under, just_over)), c(TRUE, FALSE))
})

test_that("nothing existing means nothing fresh", {
  dir <- tempfile("resume_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_equal(
    swereg:::.resume_fresh(file.path(dir, c("a.qs2", "b.qs2"))),
    c(FALSE, FALSE)
  )
  expect_equal(swereg:::.resume_fresh(character(0)), logical(0))
})

test_that("the cutoff is honoured, and `now` is injectable", {
  dir <- tempfile("resume_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  f <- .touch(file.path(dir, "a.qs2"), hours_ago = 10)

  expect_true(swereg:::.resume_fresh(f, max_age_hours = 24))
  expect_false(swereg:::.resume_fresh(f, max_age_hours = 5))
  # ... and looking from 100h in the future, nothing is fresh
  expect_false(swereg:::.resume_fresh(f, now = Sys.time() + 100 * 3600))
})
