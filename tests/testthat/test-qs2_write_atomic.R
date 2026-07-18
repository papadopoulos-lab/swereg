# qs2_write_atomic(): the write must be all-or-nothing at the destination.
#
# This matters because resume logic trusts a file's EXISTENCE. A worker killed
# mid-write used to be able to leave a truncated .qs2 at the final path, which a
# later run would then skip as "already done" -- silently substituting a torn
# file for a real result. Adding timeouts (planned) makes killed workers MORE
# likely, so this is a prerequisite, not a nicety.

test_that("no persistent-path write in R/ bypasses qs2_write_atomic", {
  # THE guard for this defect. The tests below prove qs2_write_atomic() is
  # atomic -- but the actual bug was that ten call sites never called it: they
  # used qs2::qs_save() straight to the final path (panels, counts, file_raw,
  # file_imp, file_analysis, the plan itself), while resume trusted those files'
  # existence. An atomic writer nothing routes through protects nothing.
  #
  # Originally scoped to r6_tteplan.R because the two other engines carried
  # deliberate direct writes (parallel_pool's per-item tempfiles; the mirai
  # daemon's hand-inlined temp+rename). Phase 3 deleted both -- every rawbatch/
  # skeleton write now routes through qs2_write_atomic() too -- so the guard
  # covers ALL of R/ except the one file that legitimately owns a raw write:
  #   * qs2.R -- qs2_write_atomic() itself (writes a tempfile, then renames).
  # (The dispatcher's own IPC codec, once excluded here as batch.R, left swereg
  # for batchit in Phase 4; R/batch_adapter.R writes nothing.)
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")

  files <- setdiff(list.files(r_dir, pattern = "\\.R$"), "qs2.R")
  offenders <- character(0)
  for (f in files) {
    src <- readLines(file.path(r_dir, f), warn = FALSE)
    code <- grep("^\\s*#", src, invert = TRUE, value = TRUE)
    hits <- grep("qs2::q[sd]_save\\(", code, value = TRUE)
    if (length(hits) > 0L) offenders <- c(offenders, paste0(f, ": ", trimws(hits)))
  }

  expect_equal(offenders, character(0),
    info = paste("write straight to a final path instead of qs2_write_atomic():",
                 paste(offenders, collapse = " | ")))
})

test_that("a normal write round-trips", {
  dir <- tempfile("atomic_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dest <- file.path(dir, "x.qs2")

  swereg::qs2_write_atomic(list(a = 1L, b = "two"), dest)

  expect_true(file.exists(dest))
  expect_equal(swereg::qs2_read(dest), list(a = 1L, b = "two"))
  # no temp litter next to the real data
  expect_equal(setdiff(list.files(dir), "x.qs2"), character(0))
})

test_that("a failed write leaves the previous value intact and no litter", {
  dir <- tempfile("atomic_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dest <- file.path(dir, "x.qs2")

  swereg::qs2_write_atomic("original", dest)

  # Force the rename to fail by making the destination an unremovable target:
  # a non-empty directory cannot be replaced by file.rename().
  blocked <- file.path(dir, "blocked.qs2")
  dir.create(blocked)
  writeLines("occupied", file.path(blocked, "inner"))

  # file.rename() warns before returning FALSE; the stop() is what matters.
  expect_error(suppressWarnings(swereg::qs2_write_atomic("new", blocked)),
               "could not rename")

  # the earlier good file is untouched ...
  expect_equal(swereg::qs2_read(dest), "original")
  # ... and the failed attempt left no .tmp behind
  leftovers <- grep("\\.tmp", list.files(dir), value = TRUE)
  expect_equal(leftovers, character(0))
})

test_that("the temp file is created in the destination directory", {
  # file.rename() is only atomic WITHIN a filesystem. If the temp file were
  # made in tempdir() instead of alongside the destination, the rename could
  # cross a mount boundary (this package writes to a CIFS share) and either
  # fail or silently degrade to a non-atomic copy.
  dir <- tempfile("atomic_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dest <- file.path(dir, "probe.qs2")

  seen <- character()
  # qs_save is called with the temp path; capture it before the rename.
  testthat::local_mocked_bindings(
    qs_save = function(object, file, ...) {
      seen <<- c(seen, file)
      saveRDS(object, file)
    },
    .package = "qs2"
  )
  try(swereg::qs2_write_atomic("v", dest), silent = TRUE)

  expect_length(seen, 1L)
  expect_equal(normalizePath(dirname(seen[1]), mustWork = FALSE),
               normalizePath(dir, mustWork = FALSE))
  expect_false(identical(seen[1], dest))
})

test_that("a worker killed mid-write leaves no torn file at the destination", {
  # The real scenario, not a simulation of it: SIGKILL a process while it is
  # serializing, then assert the destination is either absent or complete --
  # never a partial file that qs2_read() would choke on or resume would trust.
  skip_on_cran()
  skip_if_not_installed("processx")

  dir <- tempfile("atomic_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dest <- file.path(dir, "big.qs2")

  script <- file.path(dir, "writer.R")
  writeLines(c(
    "suppressPackageStartupMessages(requireNamespace('swereg', quietly = TRUE))",
    "big <- replicate(60L, data.frame(x = runif(2e5), y = runif(2e5)), simplify = FALSE)",
    "cat('READY\\n'); flush(stdout())",
    sprintf("swereg::qs2_write_atomic(big, %s, nthreads = 1L)", deparse(dest))
  ), script)

  p <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", script),
    stdout = "|", stderr = "|"
  )
  on.exit(tryCatch(p$kill_tree(), error = function(e) NULL), add = TRUE)

  # Wait for the object to be built, then kill DURING serialization.
  p$poll_io(30000)
  p$read_output_lines()
  Sys.sleep(0.25)
  p$kill_tree()
  p$wait(5000)

  expect_false(p$is_alive())
  # Either the rename never happened (absent) or it completed (readable).
  # What must never happen is a file that exists but cannot be read back.
  if (file.exists(dest)) {
    expect_no_error(swereg::qs2_read(dest))
  } else {
    succeed("destination absent -- rename never happened, as intended")
  }
})
