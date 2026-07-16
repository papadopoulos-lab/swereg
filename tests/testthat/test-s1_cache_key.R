# Tests for the s1 resume-cache identity: the manifest commit protocol, the
# cache key, and the selection check.
#
# The bug these exist for: the cache key used to hash the spec ALONE, so
# regenerating skeletons under an unchanged spec produced the same key and
# resume=TRUE reused sentinels computed from the previous skeletons. Every test
# below is a property that, if it breaks, lets that class of silent reuse back in.

fake_manifest <- function(
  batches = 1:5,
  pipeline_hash = "aaaaaaaaaaaaaaaa",
  identity = "1111111111111111"
) {
  list(
    manifest_version = 1L,
    committed_at = as.POSIXct("2026-01-01", tz = "UTC"),
    swereg_version = "26.7.19",
    n_batches = length(batches),
    batches = as.integer(batches),
    pipeline_hash = pipeline_hash,
    identity = identity
  )
}

skel_paths <- function(ids) sprintf("/tmp/skeleton_%05d.qs2", ids)

key <- function(spec = list(a = 1), state = NULL, impute_fn = NULL,
                stabilize = TRUE, output_dir = "/out") {
  .s1_cache_key(spec, state, impute_fn, stabilize, output_dir)
}

# --- the original bug -------------------------------------------------------

test_that("regenerating skeletons under an UNCHANGED spec moves the cache key", {
  spec <- list(enrollments = "same")
  before <- key(spec, fake_manifest(identity = "1111111111111111"))
  after <- key(spec, fake_manifest(identity = "2222222222222222"))
  expect_false(identical(before, after))
})

test_that("an unchanged spec and unchanged skeletons give a STABLE key", {
  spec <- list(enrollments = "same")
  expect_identical(key(spec, fake_manifest()), key(spec, fake_manifest()))
})

test_that("a different spec moves the key even with identical skeletons", {
  m <- fake_manifest()
  expect_false(identical(key(list(v = "009"), m), key(list(v = "010"), m)))
})

# --- codex: the selected-inventory collision --------------------------------

test_that("a capped run and a full run do NOT share a cache key", {
  m <- fake_manifest(batches = 1:5)
  full <- .assert_skeleton_selection(m, skel_paths(1:5))
  capped <- .assert_skeleton_selection(m, skel_paths(1:2))
  expect_false(identical(key(state = full), key(state = capped)))
})

test_that("selection order is part of the key (matching consumes in plan order)", {
  m <- fake_manifest(batches = 1:5)
  a <- .assert_skeleton_selection(m, skel_paths(c(1, 2, 3)))
  b <- .assert_skeleton_selection(m, skel_paths(c(3, 2, 1)))
  expect_identical(a$selected, c(1L, 2L, 3L))
  expect_identical(b$selected, c(3L, 2L, 1L))
  expect_false(identical(key(state = a), key(state = b)))
})

# --- codex: other omitted s1 inputs -----------------------------------------

test_that("impute_fn, stabilize and output_dir all move the key", {
  m <- fake_manifest()
  f1 <- function(x) x
  f2 <- function(x) x + 1
  expect_false(identical(key(state = m, impute_fn = f1),
                         key(state = m, impute_fn = f2)))
  expect_false(identical(key(state = m, stabilize = TRUE),
                         key(state = m, stabilize = FALSE)))
  expect_false(identical(key(state = m, output_dir = "/a"),
                         key(state = m, output_dir = "/b")))
})

test_that("the key is a full 64-bit digest, not a 48-bit prefix", {
  expect_equal(nchar(key(state = fake_manifest())), 16L)
})

# --- selection validation ---------------------------------------------------

test_that("selecting a batch the manifest does not describe is an error", {
  m <- fake_manifest(batches = 1:5)
  expect_error(
    .assert_skeleton_selection(m, skel_paths(c(1, 99))),
    "does not describe"
  )
})

test_that("duplicate and empty selections are rejected", {
  m <- fake_manifest(batches = 1:5)
  expect_error(.assert_skeleton_selection(m, skel_paths(c(1, 1))), "more than once")
  expect_error(.assert_skeleton_selection(m, character(0)), "no skeleton files")
})

test_that("an unparseable skeleton filename is an error, not a silent NA", {
  m <- fake_manifest(batches = 1:5)
  expect_error(
    .assert_skeleton_selection(m, c(skel_paths(1), "/tmp/not_a_skeleton.qs2")),
    "batch number"
  )
})

# --- manifest reading -------------------------------------------------------

test_that("a manifest of the wrong version or shape is treated as absent", {
  root <- file.path(tempdir(), "mft"); dir.create(root, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  study <- list(data_meta_dir = root)

  expect_null(.skeleton_manifest_on_disk(study))          # no file at all
  expect_null(.skeleton_manifest_on_disk(NULL))
  expect_null(.skeleton_manifest_on_disk(list(data_meta_dir = "/nope/nope")))

  write_study <- function(m) {
    obj <- list2env(list(skeleton_manifest = m))
    qs2::qs_save(obj, file.path(root, "registrystudy.qs2"))
  }
  write_study(NULL)
  expect_null(.skeleton_manifest_on_disk(study))           # field is NULL
  write_study(fake_manifest()[c("manifest_version", "identity")])
  expect_null(.skeleton_manifest_on_disk(study))           # missing fields
  m2 <- fake_manifest(); m2$manifest_version <- 99L
  write_study(m2)
  expect_null(.skeleton_manifest_on_disk(study))           # future version
  write_study(fake_manifest())
  expect_false(is.null(.skeleton_manifest_on_disk(study))) # valid
})

# --- cache deletion ---------------------------------------------------------

test_that("tteplan_s1_cache_delete respects dry_run and deletes only on request", {
  root <- file.path(tempdir(), "cd"); unlink(root, recursive = TRUE)
  plan <- list(
    registrystudy = list(data_meta_dir = root),
    project_prefix = "002-x"
  )
  wd <- file.path(root, "s1_work", "002-x")
  dir.create(file.path(wd, "key_a"), recursive = TRUE)
  dir.create(file.path(wd, "key_b"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  invisible(utils::capture.output(tteplan_s1_cache_delete(plan, dry_run = TRUE)))
  expect_length(list.dirs(wd, recursive = FALSE), 2L)      # dry run deletes nothing

  invisible(utils::capture.output(tteplan_s1_cache_delete(plan, dry_run = FALSE)))
  expect_length(list.dirs(wd, recursive = FALSE), 0L)

  # absent parent is not an error
  expect_silent(invisible(utils::capture.output(
    tteplan_s1_cache_delete(
      list(registrystudy = list(data_meta_dir = file.path(tempdir(), "gone")),
           project_prefix = "z"),
      dry_run = FALSE
    )
  )))
})
