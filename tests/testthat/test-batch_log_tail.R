# .pp_log_tail() -- the bounded log tail -- plus the two pool-level I/O
# guarantees that survived parallel_pool()'s deletion (Phase 3): a chatty
# worker must not deadlock the dispatcher, and successful logs are reclaimed
# per item, not at pool exit. Everything else test-parallel_pool_io.R guarded
# is covered for .batch_run by test-batch_run.R (order/NULL preservation,
# failure surfacing, validation, temp hygiene).

test_that(".pp_log_tail() is bounded and survives non-text worker output", {
  # Unit tests for the tail itself. This code runs at exactly the worst moment
  # -- while reporting a worker's failure -- so a bug here converts "one item
  # failed, here's why" into "the whole run died" or "(no output captured)".
  dir <- tempfile("logtail_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  p <- function(nm) file.path(dir, nm)

  # small file: returned whole, nothing dropped
  writeLines(c("a", "b", "real-error-here"), p("small.log"))
  expect_equal(swereg:::.pp_log_tail(p("small.log")), "a\nb\nreal-error-here")

  # missing / empty: nothing to say, and no error
  expect_equal(swereg:::.pp_log_tail(p("nope.log")), "")
  file.create(p("empty.log"))
  expect_equal(swereg:::.pp_log_tail(p("empty.log")), "")

  # no trailing newline: the last line is still reported
  cat("x\nlast-no-newline", file = p("nonl.log"))
  expect_match(swereg:::.pp_log_tail(p("nonl.log")), "last-no-newline")

  # embedded NUL: rawToChar() errors on it. Unscrubbed, the tryCatch swallows
  # that and the caller reports "(no output captured)" for a worker that said
  # exactly what was wrong.
  writeBin(as.raw(c(0x45, 0x52, 0x52, 0x00, 0x4f, 0x52, 0x0a)), p("nul.log"))
  expect_match(swereg:::.pp_log_tail(p("nul.log")), "ERROR")

  # invalid UTF-8: strsplit() errors on an invalid multibyte string. The bad
  # byte is substituted; the rest of the diagnostic survives.
  writeBin(as.raw(c(0xc3, 0x28, 0x0a, 0x6b, 0x65, 0x70, 0x74, 0x0a)), p("bad.log"))
  expect_match(swereg:::.pp_log_tail(p("bad.log")), "kept")

  # bounded: a 4 MB file yields ~64 KB, not 4 MB
  big <- paste(rep(paste(rep("N", 999L), collapse = ""), 4000L), collapse = "\n")
  cat(big, "\nFINAL-LINE\n", sep = "", file = p("big.log"))
  out <- swereg:::.pp_log_tail(p("big.log"))
  expect_lt(nchar(out), 100000L)
  expect_match(out, "FINAL-LINE")   # the tail is the USEFUL end
  expect_match(out, "tail of")      # and it says it was clipped

  # one enormous line with no newline at all: still bounded
  cat(paste(rep("Z", 200000L), collapse = ""), file = p("oneline.log"))
  expect_lt(nchar(swereg:::.pp_log_tail(p("oneline.log"))), 100000L)
})

test_that(".pp_log_tail() reads by seek, never by readLines()", {
  # The DETERMINISTIC half of the boundedness guarantee. The timing test below
  # is real evidence but portability-fragile: a cached 120 MB readLines() can
  # finish under a second on a fast host, and a loaded slow host can push the
  # bounded implementation over it. This one cannot be fooled by hardware --
  # readLines()/readRDS-style whole-file reads simply must not appear in the
  # function that runs while a worker's failure is being reported.
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")

  src <- readLines(file.path(r_dir, "batch.R"), warn = FALSE)
  start <- grep("^\\.pp_log_tail <- function", src)
  expect_length(start, 1L)          # not vacuous: the function must exist
  # to the end of the function (its closing brace at column 0)
  rest <- src[start:length(src)]
  end <- which(rest == "}")[1L]
  body_src <- rest[seq_len(end)]
  code <- grep("^\\s*#", body_src, invert = TRUE, value = TRUE)

  expect_equal(grep("readLines\\(", code, value = TRUE), character(0),
    info = "readLines() reads the WHOLE file; .pp_log_tail must seek to the end")
  expect_gt(length(grep("seek\\(", code)), 0L)
  expect_gt(length(grep("readBin\\(", code)), 0L)
})

test_that(".pp_log_tail() bounds the READ, not merely the output", {
  # Replaces an assertion that only LOOKED load-bearing. Capping the emitted
  # message cannot distinguish the fix from the bug: readLines(whole_file) then
  # tail(100) ALSO emits a small message. The defect was never output size, it
  # was that the entire file entered memory. Time discriminates: reading 120 MB
  # takes seconds; seeking to the last 64 KB takes milliseconds.
  skip_on_cran()
  dir <- tempfile("bigread_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- file.path(dir, "huge.log")

  chunk <- paste(rep(paste(rep("N", 1023L), collapse = ""), 1000L), collapse = "\n")
  con <- file(path, "wb")
  for (i in 1:120) writeLines(chunk, con)          # ~120 MB
  writeLines("FINAL-LINE-OF-HUGE-LOG", con)
  close(con)
  expect_gt(file.size(path), 100e6)

  elapsed <- system.time(out <- swereg:::.pp_log_tail(path))[["elapsed"]]

  expect_match(out, "FINAL-LINE-OF-HUGE-LOG")   # still useful
  expect_lt(nchar(out), 100000L)                # still bounded
  expect_lt(elapsed, 1.0)                       # ... and never read the 120 MB
})

# --- pool-level I/O guarantees, ported from parallel_pool to .batch_run -----

.log_tail_dev_tree <- normalizePath(testthat::test_path("..", ".."),
  mustWork = FALSE)
.log_tail_have_tree <- file.exists(file.path(.log_tail_dev_tree, "DESCRIPTION")) &&
  file.exists(file.path(.log_tail_dev_tree, "inst", "batch_worker.R"))

test_that("a chatty worker does not deadlock .batch_run", {
  # Regression class: parallel_pool's stdout/stderr used to be pipes that were
  # only read AFTER the child exited. A child out-writing the OS pipe buffer
  # (64 KB on Linux) blocks forever in write(), never exits, and stays
  # is_alive() == TRUE -- so the dispatch loop spins until killed. Measured on
  # the old code: 1 KB finished in 0.7s, 100 KB never returned. .batch_run
  # writes worker output to per-item FILES, so 512 KB per stream (8x the pipe
  # buffer) must complete rather than hang.
  skip_on_cran()
  skip_if_not(.log_tail_have_tree, "package source tree not available")

  expect_no_error(
    swereg:::.batch_run(
      target = swereg:::.batch_target("swereg", ".batch_fixture_chatty"),
      items = list(list(n_kb = 512L)),
      n_workers = 1L,
      dev_path = .log_tail_dev_tree,
      collect = FALSE
    )
  )
})

test_that("a successful worker's log is reclaimed as it completes, not at pool exit", {
  # Observe DURING the run (the progressor fires once per completed item), not
  # by comparing before/after counts -- the pool's own on.exit cleanup makes
  # those equal whether or not the per-item unlink exists. s1c dispatches
  # 39,492 items over ~10h; deferring reclaim to pool exit would sit on ~39k
  # log files for the whole stage.
  skip_on_cran()
  skip_if_not(.log_tail_have_tree, "package source tree not available")

  seen <- integer()
  probe <- function(...) {
    seen <<- c(seen, length(list.files(tempdir(), pattern = "^batch_log_")))
  }

  swereg:::.batch_run(
    target = swereg:::.batch_target("swereg", ".batch_fixture_echo"),
    items = lapply(1:8, function(i) list(x = i)),
    n_workers = 1L,
    dev_path = .log_tail_dev_tree,
    p = probe,
    collect = FALSE
  )

  expect_length(seen, 8L)
  # With per-item cleanup at most a couple coexist; without it they accumulate
  # to 8, so the final observation alone would be >= 8.
  expect_lt(max(seen), 4L)
})
