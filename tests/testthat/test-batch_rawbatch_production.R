# Production-boundary proof for shape B (Phase 3): drive save_rawbatch()
# through the REAL path -- .batch_stream -> batchit's mirai daemons ->
# .rawbatch_write_worker -> qs2_write_atomic -> rawbatch files on disk -- and
# assert a real success AND a real failure. This is the production proof Phase 2
# lacked: shape B was demonstrated only on generic fixtures while the one
# shape-B production caller kept its hand-rolled mirai block.

skip_if_not_installed("data.table")
skip_if_not_installed("withr")
skip_if_not_installed("mirai")

.rawbatch_prod_study <- function(dir) {
  study <- swereg::RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "lmed",
    batch_size = 3L
  )
  study$set_ids(1:8) # -> 3 batches: 1-3, 4-6, 7-8
  study
}

test_that("save_rawbatch(n_workers = 2) streams REAL slices through .batch_stream to disk", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  study <- .rawbatch_prod_study(dir)
  lmed <- data.table::data.table(lopnr = 1:8, atc = rep("N05A", 8L))

  invisible(utils::capture.output(
    study$save_rawbatch("lmed", lmed, n_workers = 2L),
    type = "output"
  ))

  # The daemons wrote the real files, correctly named.
  files <- list.files(dir, pattern = "^\\d{5}_rawbatch_lmed\\.qs2$")
  expect_setequal(files, c(
    "00001_rawbatch_lmed.qs2",
    "00002_rawbatch_lmed.qs2",
    "00003_rawbatch_lmed.qs2"
  ))

  # Round-trip: readable slices, person-disjoint, covering every ID -- the
  # same partitioning contract the serial path is pinned to in
  # test-rawbatch_file_layout.R, proven here through the daemons.
  flat <- unlist(lapply(
    seq_len(study$n_batches),
    function(b) unique(study$load_rawbatch(b)$lmed$lopnr)
  ))
  expect_setequal(flat, 1:8)
  expect_equal(length(flat), length(unique(flat)))
  expect_true("lmed" %in% study$groups_saved)
})

test_that("a REAL slice-write failure in a daemon propagates loudly, naming the batch id", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  study <- .rawbatch_prod_study(dir)
  lmed <- data.table::data.table(lopnr = 1:8, atc = rep("N05A", 8L))

  # Block batch 2's final path with a DIRECTORY: qs2_write_atomic()'s rename
  # onto it fails inside the daemon, the target errors, and that error must
  # travel back through the result envelope and .batch_stream and make
  # save_rawbatch RAISE, naming the batch -- not report a completed group.
  dir.create(file.path(dir, "00002_rawbatch_lmed.qs2"))

  expect_error(
    suppressWarnings(utils::capture.output(
      study$save_rawbatch("lmed", lmed, n_workers = 2L),
      type = "output"
    )),
    "00002_lmed"
  )
  # The failed group must NOT be recorded as saved.
  expect_false("lmed" %in% study$groups_saved)
})
