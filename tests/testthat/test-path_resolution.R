test_that("first_existing_path returns the first existing candidate", {
  d1 <- tempfile("ctx1_")
  d2 <- tempfile("ctx2_")
  dir.create(d1)
  dir.create(d2)
  on.exit(unlink(c(d1, d2), recursive = TRUE), add = TRUE)

  expect_equal(first_existing_path(c(d1, d2)), d1)
  expect_equal(first_existing_path(c(d2, d1)), d2)
})

test_that("first_existing_path skips earlier non-existent candidates", {
  nonexistent <- file.path(tempdir(), "definitely-absent-xyz-123")
  existing <- tempfile("ctx_existing_")
  dir.create(existing)
  on.exit(unlink(existing, recursive = TRUE), add = TRUE)

  expect_equal(
    first_existing_path(c(nonexistent, existing)),
    existing
  )
})

test_that("first_existing_path errors on an empty candidate list", {
  expect_error(
    first_existing_path(character(0)),
    "no paths provided"
  )
  expect_error(
    first_existing_path(character(0), "my_label"),
    "my_label"
  )
})

test_that("first_existing_path error uses the supplied label", {
  nonexistent1 <- file.path(tempdir(), "absent-grandparent-a", "child")
  nonexistent2 <- file.path(tempdir(), "absent-grandparent-b", "child")
  expect_error(
    first_existing_path(c(nonexistent1, nonexistent2), "dir_widget"),
    "dir_widget"
  )
})

test_that("first_existing_path auto-creates first candidate whose parent exists", {
  parent <- tempfile("ctx_parent_")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)

  absent_parent <- file.path(tempdir(), "absent-yyy", "child-a")
  creatable <- file.path(parent, "child-b")

  result <- first_existing_path(c(absent_parent, creatable))

  expect_equal(result, creatable)
  expect_true(dir.exists(creatable))
})

test_that("first_existing_path prefers existing candidate over creatable one", {
  existing <- tempfile("ctx_existing_")
  dir.create(existing)
  on.exit(unlink(existing, recursive = TRUE), add = TRUE)

  # `creatable` is listed first but `existing` already exists -- the existing
  # candidate must win, even though it comes later.
  creatable <- file.path(tempdir(), "ctx_never_created_zzz")
  on.exit(unlink(creatable, recursive = TRUE), add = TRUE)

  expect_equal(
    first_existing_path(c(creatable, existing)),
    existing
  )
  expect_false(dir.exists(creatable))
})
