# Production-boundary + snapshot-economics proof for the process_skeletons()
# migration (Phase 3): the parallel branch now dispatches
# .process_one_batch_snapshot through .batch_run -> the generic worker, with
# the study written to ONE snapshot file and each item carrying only its path
# plus small scalars.
#
# The economics are the load-bearing part. The study object is ~5.7 MB in
# production and there are 2,194 batches: the old callr engine serialized it
# per LAUNCHED batch (~n_workers in flight), while the shape-A runner
# materialises every item envelope up front -- so a naive migration putting the
# study in every item would have written ~12.5 GB of envelopes before the
# first worker launched. These tests pin the one-snapshot form structurally so
# a refactor cannot silently reintroduce the study into the items.
#
# (The REAL parallel failure path is pinned by
# test-process_skeletons_loud_errors.R, which runs n_workers = 2 against a
# framework that stops.)

skip_if_not_installed("data.table")
skip_if_not_installed("withr")

.skel_prod_study <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("grp1"),
    batch_size = 3L
  )
  study$set_ids(1:6) # -> 2 batches
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:6, val = letters[1:6])
  )
  study$register_framework(function(batch_data, config) {
    data.table::data.table(
      id = batch_data[["grp1"]]$lopnr,
      isoyear = 2020L,
      isoyearweek = "2020-01",
      is_isoyear = FALSE
    )
  })
  study
}

test_that("process_skeletons(n_workers = 2) builds REAL skeletons through .batch_run", {
  skip_on_cran() # spawns subprocesses

  study <- .skel_prod_study()
  invisible(utils::capture.output(
    suppressMessages(study$process_skeletons(n_workers = 2L)),
    type = "output"
  ))

  # Both batches were processed in real subprocesses, from the snapshot, and
  # landed as loadable skeletons with the study's own pipeline identity.
  expect_equal(length(study$skeleton_files), 2L)
  sk1 <- study$load_skeleton(1L)
  expect_s3_class(sk1, "Skeleton")
  expect_identical(sk1$pipeline_hash(), study$pipeline_hash())
  expect_true(all(c("id", "isoyear") %in% names(sk1$data)))
})

test_that("the study is snapshotted ONCE; items stay small and are cleaned up", {
  study <- .skel_prod_study()

  captured <- NULL
  testthat::local_mocked_bindings(
    .batch_run = function(target, items, n_workers, ...) {
      captured <<- list(
        target = target,
        items = items,
        snapshot_paths = unique(vapply(
          items, function(it) it[["snapshot_path"]], character(1)
        )),
        snapshot_exists_at_dispatch = file.exists(
          items[[1L]][["snapshot_path"]]
        )
      )
      stop("__SENTINEL_SKEL__")
    },
    .package = "swereg"
  )

  err <- tryCatch(
    suppressMessages(utils::capture.output(
      study$process_skeletons(n_workers = 2L),
      type = "output"
    )),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "__SENTINEL_SKEL__")
  # The wrapper keeps the halted-on-batch guidance contract. (The sentinel
  # mock throws before any per-item handling, so no batch id can appear HERE;
  # the id-carrying real failure is pinned by the n_workers = 2 test in
  # test-process_skeletons_loud_errors.R, which asserts batch_0000[12] in the
  # wrapped message.)
  expect_match(err, "halted on batch")
  expect_match(err, "batches =", fixed = TRUE)

  expect_identical(captured$target$symbol, ".process_one_batch_snapshot")
  expect_equal(length(captured$items), 2L)
  expect_identical(names(captured$items), c("batch_00001", "batch_00002"))

  # ONE snapshot, shared by every item, existing at dispatch time...
  expect_length(captured$snapshot_paths, 1L)
  expect_true(captured$snapshot_exists_at_dispatch)
  # ...and removed once process_skeletons() unwinds (on.exit cleanup), so a
  # failed run does not strand a multi-MB study copy in tempdir.
  expect_false(file.exists(captured$snapshot_paths))

  # Every item names the target's complete formal set, and none of them
  # carries the study itself: the serialized item must be tiny. 50 KB is two
  # orders of magnitude under the study, so this fails loudly if the study (or
  # any other bulk payload) creeps back into the items.
  fml <- names(formals(swereg:::.process_one_batch_snapshot))
  for (it in captured$items) {
    expect_true(setequal(names(it), fml))
    expect_lt(length(serialize(it, NULL)), 50000L)
  }
})
