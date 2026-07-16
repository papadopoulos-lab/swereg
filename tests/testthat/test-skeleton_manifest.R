# Integration tests for the skeleton manifest commit protocol.
#
# These exist because 25 unit tests of the cache key passed while TWO deterministic
# control-flow bugs sat in the protocol, both found by review rather than by tests:
#
#   1. `full_run` was read as is.null(batches) AFTER process_skeletons() overwrites
#      that parameter with seq_len(n_batches) -- so it was always FALSE and a failed
#      full run exited 0.
#   2. .invalidate_skeleton_manifest() early-returned when the in-memory field was
#      NULL, which it always is in the real caller (a fresh study +
#      adopt_runtime_state_from(), which does not copy the manifest) -- so the stale
#      manifest survived the whole rebuild on disk.
#
# Both are about the protocol's control flow against a REAL study, which is exactly
# what unit tests of the key could not see. Test the protocol, not just the digest.

make_study <- function(root, n_ids = 6L, batch_size = 3L) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  study <- RegistryStudy$new(
    data_rawbatch_dir = root,
    data_skeleton_dir = root,
    data_meta_dir = root,
    group_names = "other",
    batch_size = batch_size
  )
  study$register_framework(function(batch_data, config) {
    data.table::data.table(id = integer(0))
  })
  study$set_ids(seq_len(n_ids))
  study
}

# Write a skeleton + its meta sidecar for `batch`, as $save_skeleton() would.
# framework_hash defaults to the study's CURRENT hash, so a batch written this way
# is "current" and each test can isolate the one failure it is about. The commit
# checks run in order (unreadable -> mixed -> obsolete -> inventory), so a wrong
# default here makes every test trip the obsolete check instead of its own.
write_batch <- function(study, batch, framework_hash = NULL, codes = character(0)) {
  if (is.null(framework_hash)) {
    framework_hash <- swereg:::.hash_function(study$framework_fn)
  }
  sk <- Skeleton$new(data.table::data.table(id = 1L), batch_number = batch)
  sk$framework_fn_hash <- framework_hash
  sk$randvars_state <- list()
  sk$applied_registry <- stats::setNames(vector("list", length(codes)), codes)
  study$save_skeleton(sk)
  invisible(sk)
}

test_that("a stale manifest on disk is cleared even when the in-memory field is NULL", {
  root <- file.path(tempdir(), "mfclear"); unlink(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  study <- make_study(root)

  # Simulate a previous run's committed manifest sitting on disk.
  study$skeleton_manifest <- list(manifest_version = 1L, identity = "STALE")
  study$save_meta()
  expect_false(is.null(qs2_read(study$meta_file)$skeleton_manifest))

  # The real caller builds a FRESH study (manifest NULL in memory) and adopts only
  # runtime state -- adopt_runtime_state_from() does NOT copy the manifest.
  fresh <- make_study(root)
  fresh$adopt_runtime_state_from(qs2_read(study$meta_file))
  expect_null(fresh$skeleton_manifest)

  invisible(utils::capture.output(fresh$.__enclos_env__$private$.invalidate_skeleton_manifest()))

  # The stale manifest must be gone from DISK, not merely from memory.
  expect_null(qs2_read(fresh$meta_file)$skeleton_manifest)
})

test_that("clearing the manifest does not clobber the rest of the on-disk study", {
  # The hazard of clearing via save_meta(): it serialises the WHOLE in-memory
  # object, which at that moment may be less complete than the file. A caller that
  # had not adopted runtime state would overwrite a good registrystudy.qs2 with an
  # empty one -- a no-op run turning into data loss. Invalidation must remove one
  # field and touch nothing else.
  root <- file.path(tempdir(), "mfnoclob"); unlink(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  good <- make_study(root, n_ids = 6L)          # has ids + batch list
  good$skeleton_manifest <- list(manifest_version = 1L, identity = "STALE")
  good$save_meta()
  expect_equal(qs2_read(good$meta_file)$n_ids, 6L)

  # A study that never adopted runtime state: n_ids is 0, batch_id_list empty.
  empty <- make_study(root, n_ids = 0L)
  expect_true(is.null(empty$n_ids) || empty$n_ids == 0L)
  invisible(utils::capture.output(
    empty$.__enclos_env__$private$.invalidate_skeleton_manifest()
  ))

  after <- qs2_read(empty$meta_file)
  expect_null(after$skeleton_manifest)   # the manifest IS cleared
  expect_equal(after$n_ids, 6L)          # ...and the good state SURVIVES
})

test_that("clearing is a no-op when there is no meta file or no manifest", {
  root <- file.path(tempdir(), "mfnoop"); unlink(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  study <- make_study(root)

  # No meta file yet (a genuine first run): must not create one.
  expect_false(file.exists(study$meta_file))
  invisible(utils::capture.output(
    study$.__enclos_env__$private$.invalidate_skeleton_manifest()
  ))
  expect_false(file.exists(study$meta_file))

  # Meta file with no manifest: must not rewrite it.
  study$save_meta()
  before <- file.mtime(study$meta_file)
  invisible(utils::capture.output(
    study$.__enclos_env__$private$.invalidate_skeleton_manifest()
  ))
  expect_equal(file.mtime(study$meta_file), before)
})

test_that("adopt_runtime_state_from does not carry a manifest across studies", {
  root <- file.path(tempdir(), "mfadopt"); unlink(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  a <- make_study(root)
  a$skeleton_manifest <- list(manifest_version = 1L, identity = "A")
  b <- make_study(root)
  b$adopt_runtime_state_from(a)
  expect_null(b$skeleton_manifest)
})

test_that("a full run that fails to validate RAISES and leaves no manifest", {
  root <- file.path(tempdir(), "mffull"); unlink(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  study <- make_study(root)          # expects 2 batches
  write_batch(study, 1L)             # only 1 -> incomplete

  expect_error(
    invisible(utils::capture.output(
      study$.__enclos_env__$private$.commit_skeleton_manifest(full_run = TRUE)
    )),
    "Refusing to commit"
  )
  expect_null(study$skeleton_manifest)
  expect_null(qs2_read(study$meta_file)$skeleton_manifest)
})

test_that("a deliberate SUBSET run leaves no manifest but does not raise", {
  root <- file.path(tempdir(), "mfsub"); unlink(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  study <- make_study(root)
  write_batch(study, 1L)

  expect_no_error(
    invisible(utils::capture.output(
      study$.__enclos_env__$private$.commit_skeleton_manifest(full_run = FALSE)
    ))
  )
  expect_null(study$skeleton_manifest)
})

test_that("an incomplete inventory is rejected even when internally consistent", {
  # The nastiest case: a FIRST build interrupted leaves N mutually-consistent
  # skeletons that a hash-only check waves through.
  root <- file.path(tempdir(), "mfinv"); unlink(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  study <- make_study(root, n_ids = 9L, batch_size = 3L)     # expects 3
  write_batch(study, 1L)
  write_batch(study, 2L)                                     # uniform, but 2 of 3
  expect_error(
    invisible(utils::capture.output(
      study$.__enclos_env__$private$.commit_skeleton_manifest(full_run = TRUE)
    )),
    "inventory is wrong"
  )
})

test_that("a uniform but OBSOLETE dataset is rejected (hash != study's current)", {
  root <- file.path(tempdir(), "mfobs"); unlink(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  study <- make_study(root)
  # Every batch agrees with every other, but none agrees with the study's code.
  write_batch(study, 1L, framework_hash = "OLD")
  write_batch(study, 2L, framework_hash = "OLD")
  expect_error(
    invisible(utils::capture.output(
      study$.__enclos_env__$private$.commit_skeleton_manifest(full_run = TRUE)
    )),
    "obsolete"
  )
})

test_that("process_skeletons() computes full_run correctly (end-to-end wiring)", {
  # THE test for bug 1. Calling .commit_skeleton_manifest(full_run = TRUE) directly
  # cannot catch it: the bug was that process_skeletons() reads is.null(batches)
  # AFTER overwriting that parameter with seq_len(n_batches), so full_run was
  # always FALSE. Only driving the real entry point exercises that.
  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    data_skeleton_dir = dir,
    data_meta_dir = dir,
    group_names = "grp1",
    batch_size = 2L
  )
  study$set_ids(seq_len(4L))              # 2 batches
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = seq_len(4L), val = "x")
  )
  study$register_framework(function(batch_data, config) {
    data.table::data.table(id = batch_data[["grp1"]]$lopnr)
  })

  # A full run (batches = NULL) over a study that CAN complete must commit.
  invisible(utils::capture.output(suppressMessages(suppressWarnings(
    study$process_skeletons(n_workers = 1L)
  ))))
  m <- qs2_read(study$meta_file)$skeleton_manifest
  expect_false(is.null(m))
  expect_identical(m$manifest_version, 1L)
  expect_identical(m$batches, 1:2)
  expect_identical(m$pipeline_hash, study$pipeline_hash())

  # Re-running with no code change must be a NO-OP for identity. process_skeletons
  # replays only the phases whose hash moved, so nothing is rewritten, built_at
  # does not move, and the identity is stable -- which is what makes resume = TRUE
  # usable at all. An identity that churned on every invocation would invalidate
  # the cache constantly and quietly turn every resume into a full rebuild.
  invisible(utils::capture.output(suppressMessages(suppressWarnings(
    study$process_skeletons(batches = 1L, n_workers = 1L)
  ))))
  m2 <- qs2_read(study$meta_file)$skeleton_manifest
  expect_false(is.null(m2))
  expect_identical(m2$pipeline_hash, m$pipeline_hash)
  expect_identical(m2$identity, m$identity)

  # A full run that RUNS FINE but whose finished dataset fails validation must
  # raise. This is the assertion that actually pins bug 1: full_run only matters
  # on the failure path, so a passing end-to-end run cannot distinguish
  # full_run = TRUE from FALSE. A stray skeleton outside seq_len(n_batches) makes
  # the inventory wrong without making any batch fail.
  stray <- Skeleton$new(data.table::data.table(id = 1L), batch_number = 99L)
  stray$framework_fn_hash <- swereg:::.hash_function(study$framework_fn)
  study$save_skeleton(stray)
  expect_error(
    invisible(utils::capture.output(suppressMessages(suppressWarnings(
      study$process_skeletons(n_workers = 1L)
    )))),
    "Refusing to commit"
  )
  expect_null(qs2_read(study$meta_file)$skeleton_manifest)
})

test_that("a MIXED dataset is rejected", {
  root <- file.path(tempdir(), "mfmix"); unlink(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  study <- make_study(root)
  write_batch(study, 1L, framework_hash = "A")
  write_batch(study, 2L, framework_hash = "B")
  expect_error(
    invisible(utils::capture.output(
      study$.__enclos_env__$private$.commit_skeleton_manifest(full_run = TRUE)
    )),
    "distinct pipeline hashes"
  )
})
