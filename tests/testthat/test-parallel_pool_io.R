# parallel_pool() child I/O and input validation.
#
# These are the failure modes that MASQUERADE as a hung or mysterious worker,
# which is the worst way to lose a 14-hour stage: there is no error, no
# progress, and nothing in the log to point at.
#
# Deliberately small and single-worker so they are safe under R CMD check's
# two-core limit. They are fast (~1s each) and must NOT be skipped on CRAN --
# they cover a deadlock, and a deadlock in a test is a hang in the wild.

# Builds a throwaway "dev tree" that parallel_pool() will resolve worker
# scripts from, with a bootstrap that does NOT load swereg. That isolates the
# pool's own process/IO behaviour from anything in the package.
.fake_dev <- function(scripts, package = "swereg") {
  root <- tempfile("ppdev_")
  dir <- file.path(root, "inst")
  dir.create(dir, recursive = TRUE)
  # parallel_pool() checks the tree really is the package it was asked for, so
  # the harness has to be an honest source tree rather than a bag of scripts.
  writeLines(
    c(paste0("Package: ", package), "Version: 0.0.0.9000", "Title: Test fixture"),
    file.path(root, "DESCRIPTION")
  )
  writeLines("params <- qs2::qs_read(args[2L])",
             file.path(dir, "worker_bootstrap.R"))
  for (nm in names(scripts)) writeLines(scripts[[nm]], file.path(dir, nm))
  root
}

test_that(".pp_log_tail() is bounded and survives non-text worker output", {
  # Unit tests for the tail itself. Worth having separately from the pool tests
  # because this code runs at exactly the worst moment -- while reporting a
  # worker's failure -- so a bug here converts "one item failed, here's why"
  # into "the whole run died" or "(no output captured)".
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

test_that("a chatty worker does not deadlock the pool", {
  # Regression: stdout/stderr used to be pipes (`stdout = "|"`) that were only
  # read AFTER the child exited. A child out-writing the OS pipe buffer (64 KB
  # on Linux) blocks forever in write(), never exits, and stays
  # is_alive() == TRUE -- so the dispatch loop spins on it until killed.
  # Measured before the fix: 1 KB finished in 0.7s, 100 KB never returned.
  dev <- .fake_dev(list("worker_chatty.R" = c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "source(args[1L])",
    "line <- paste(rep('x', 1023L), collapse = '')",
    "for (i in seq_len(params$n_kb)) {",
    "  cat(line, '\\n', sep = '', file = stdout())",
    "  cat(line, '\\n', sep = '', file = stderr())",
    "}"
  )))
  on.exit(unlink(dev, recursive = TRUE), add = TRUE)

  # 512 KB per stream: 8x the Linux pipe buffer, so it deadlocks reliably on
  # the old code while staying trivially fast on the new.
  expect_no_error(
    swereg:::parallel_pool(
      items = list(list(n_kb = 512L)),
      worker_script = "worker_chatty.R",
      n_workers = 1L,
      swereg_dev_path = dev,
      collect = FALSE
    )
  )
})

test_that("a failing worker raises, and its output survives in the message", {
  # The pool no longer has pipes to read, so the diagnostic path had to move to
  # the per-item log file. If that regressed, failures would go silent -- which
  # is worse than the deadlock it replaced.
  dev <- .fake_dev(list("worker_boom.R" = c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "source(args[1L])",
    "cat('SENTINEL-marker-on-stdout\\n')",
    "stop('deliberate worker failure')"
  )))
  on.exit(unlink(dev, recursive = TRUE), add = TRUE)

  msgs <- character()
  err <- tryCatch(
    withCallingHandlers(
      swereg:::parallel_pool(
        items = list(list(x = 1L)),
        worker_script = "worker_boom.R",
        n_workers = 1L,
        swereg_dev_path = dev,
        collect = FALSE
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, "Worker 1 failed")
  emitted <- paste(msgs, collapse = "\n")
  expect_match(emitted, "deliberate worker failure")
  expect_match(emitted, "SENTINEL-marker-on-stdout")
})

test_that("a failing worker with a huge log reports a BOUNDED tail", {
  # The first version of the deadlock fix redirected output to a file and then
  # called readLines() on the WHOLE file before taking the tail -- so a worker
  # that died after emitting a multi-GB log would OOM the PARENT while trying to
  # report the error, turning one worker's failure into the whole run's. The
  # comment above it even claimed it was "bounded on purpose". It was not.
  # Only the last max_bytes are read now.
  dev <- .fake_dev(list("worker_loud_boom.R" = c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "source(args[1L])",
    "line <- paste(rep('N', 999L), collapse = '')",
    "for (i in seq_len(4000L)) cat(line, '\\n', sep = '')",   # ~4 MB of noise
    "cat('LAST-LINE-BEFORE-FAILURE\\n')",
    "stop('deliberate failure after a huge log')"
  )))
  on.exit(unlink(dev, recursive = TRUE), add = TRUE)

  msgs <- character()
  tryCatch(
    withCallingHandlers(
      swereg:::parallel_pool(
        items = list(list(x = 1L)),
        worker_script = "worker_loud_boom.R",
        n_workers = 1L,
        swereg_dev_path = dev,
        collect = FALSE
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) NULL
  )

  emitted <- paste(msgs, collapse = "\n")
  # The tail must still be USEFUL: the end of the log is where the failure is.
  expect_match(emitted, "LAST-LINE-BEFORE-FAILURE")
  expect_match(emitted, "deliberate failure after a huge log")
  # NOTE: this asserts the message is useful, NOT that the read was bounded --
  # the old readLines(whole_file) implementation produced a small message too.
  # The boundedness of the READ is proved by the timing test below.
  expect_lt(nchar(emitted), 200000L)
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

  src <- readLines(file.path(r_dir, "parallel_pool.R"), warn = FALSE)
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

test_that("a successful worker's log is reclaimed as it completes, not at pool exit", {
  # Also replaces a test that only appeared to guard: comparing the log count
  # before and after the run, the pre-existing on.exit(unlink(log_paths)) made
  # those equal whether or not the per-item unlink existed. Observe DURING the
  # run instead -- the progressor fires once per completed item, so it can count
  # the logs still on disk while the pool is still working.
  dev <- .fake_dev(list("worker_quiet.R" = c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "source(args[1L])",
    "cat('some output\\n')"
  )))
  on.exit(unlink(dev, recursive = TRUE), add = TRUE)

  seen <- integer()
  probe <- function(...) {
    seen <<- c(seen, length(list.files(tempdir(), pattern = "^pp_log_")))
  }

  swereg:::parallel_pool(
    items = lapply(1:8, function(i) list(x = i)),
    worker_script = "worker_quiet.R",
    n_workers = 1L,
    swereg_dev_path = dev,
    p = probe,
    collect = FALSE
  )

  # With per-item cleanup at most a couple coexist; without it they accumulate
  # to 8, so the final observation alone would be >= 8.
  expect_lt(max(seen), 4L)
})

test_that("a worker that exits 0 but violates the output contract still reports its log", {
  # The per-item unlink() had to move BELOW output validation. The output
  # contract used to be checked in a second pass after the dispatch loop, by
  # which point every successful log was gone -- so a worker that exited 0 and
  # wrote nothing was reported as "produced no output file" having just
  # destroyed the only evidence of why.
  dev <- .fake_dev(list("worker_liar.R" = c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "source(args[1L])",
    "cat('DIAGNOSTIC-BREADCRUMB: exited 0 without writing output\\n')",
    "invisible(NULL)"   # exits 0, writes NO output file, despite collect = TRUE
  )))
  on.exit(unlink(dev, recursive = TRUE), add = TRUE)

  msgs <- character()
  err <- tryCatch(
    withCallingHandlers(
      swereg:::parallel_pool(
        items = list(list(x = 1L)),
        worker_script = "worker_liar.R",
        n_workers = 1L,
        swereg_dev_path = dev,
        collect = TRUE
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, "produced no output file")
  # The point of the change: the log survived long enough to say why.
  expect_match(paste(msgs, collapse = "\n"), "DIAGNOSTIC-BREADCRUMB")
})

test_that("collect = TRUE returns results in ITEM order, not completion order", {
  # Guards the .collect_output() restructure. Results used to be gathered in a
  # second pass over seq_len(n_items) after the dispatch loop, which was
  # trivially in item order. They are now written from INSIDE the loop, as each
  # worker finishes -- i.e. in completion order. `results[[entry$idx]]` is what
  # keeps that honest, and it is one character away from being wrong.
  dev <- .fake_dev(list("worker_ord.R" = c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "source(args[1L])",
    "Sys.sleep(params$delay)",
    "qs2::qs_save(params$tag, args[3L])"
  )))
  on.exit(unlink(dev, recursive = TRUE), add = TRUE)

  # Item 1 is slow, item 2 is fast: with 2 workers, item 2 finishes FIRST.
  items <- list(
    list(delay = 1.5, tag = "FIRST-item-slow"),
    list(delay = 0.1, tag = "SECOND-item-fast")
  )
  res <- swereg:::parallel_pool(
    items = items,
    worker_script = "worker_ord.R",
    n_workers = 2L,
    swereg_dev_path = dev,
    collect = TRUE
  )

  expect_length(res, 2L)
  expect_equal(res[[1]], "FIRST-item-slow")
  expect_equal(res[[2]], "SECOND-item-fast")
})

test_that("collect = TRUE preserves NULL results instead of deleting them", {
  # `results[[idx]] <- NULL` DELETES the element in R. Since collection now
  # happens in COMPLETION order, a worker legitimately returning NULL can
  # shorten the list and shift every result gathered before it -- and WHICH ones
  # move depends on worker timing. `results[idx] <- list(x)` assigns the NULL
  # rather than removing it.
  #
  # ORDER IS THE WHOLE TEST. The first version of this had the NULL item finish
  # FIRST, which is the BENIGN order: deleting position 2 of a freshly
  # pre-allocated all-NULL list is invisible, and later higher-index assignments
  # re-extend it, so the buggy code passed. The corruption needs a HIGHER index
  # to be filled BEFORE the NULL lands:
  #   r[[3]] <- "three"; r[[1]] <- "one"; r[[2]] <- NULL  ->  length 2, shifted.
  # So the NULL item is in the MIDDLE and finishes LAST.
  dev <- .fake_dev(list("worker_maybe_null.R" = c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "source(args[1L])",
    "Sys.sleep(params$delay)",
    "qs2::qs_save(params$val, args[3L])"
  )))
  on.exit(unlink(dev, recursive = TRUE), add = TRUE)

  items <- list(
    list(delay = 1.0, val = "one"),
    list(delay = 2.0, val = NULL),    # middle, and finishes LAST
    list(delay = 0.1, val = "three")  # finishes FIRST, so index 3 is filled early
  )
  res <- swereg:::parallel_pool(
    items = items,
    worker_script = "worker_maybe_null.R",
    n_workers = 3L,
    swereg_dev_path = dev,
    collect = TRUE
  )

  expect_length(res, 3L)            # nothing deleted
  expect_equal(res[[1]], "one")     # nothing shifted
  expect_null(res[[2]])             # the NULL survived, in place
  expect_equal(res[[3]], "three")
})

test_that("a dev_path that exists but is the WRONG package is rejected", {
  # Existing is not correct. The bootstrap load_all()s this tree into every
  # worker, so a mistargeted directory loads the wrong package and mixes it with
  # the installed swereg -- the same silently-running-different-code failure as
  # a missing path, only harder to notice.
  dev <- .fake_dev(
    list("worker_noop.R" = c(
      "args <- commandArgs(trailingOnly = TRUE)",
      "source(args[1L])"
    )),
    package = "someoneelsespkg"
  )
  on.exit(unlink(dev, recursive = TRUE), add = TRUE)

  expect_error(
    swereg:::parallel_pool(
      items = list(list(x = 1L)),
      worker_script = "worker_noop.R",
      n_workers = 1L,
      swereg_dev_path = dev,
      collect = FALSE
    ),
    regexp = "someoneelsespkg"
  )
})

test_that("n_workers must be a positive whole scalar", {
  # n_workers = 0 previously gave floor(n_cores / 0) -> n_threads = Inf, and
  # then the dispatch loop spun at 100% CPU forever: the inner while never
  # launches anything, `active` stays empty, and the Sys.sleep(0.1) is inside
  # `if (length(active) > 0L)`. An infinite BUSY loop, not merely a stall.
  dev <- .fake_dev(list("worker_noop.R" = c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "source(args[1L])"
  )))
  on.exit(unlink(dev, recursive = TRUE), add = TRUE)

  for (bad in list(0L, -1L, NA_integer_, c(1L, 2L), "2", NULL)) {
    expect_error(
      swereg:::parallel_pool(
        items = list(list(x = 1L)),
        worker_script = "worker_noop.R",
        n_workers = bad,
        swereg_dev_path = dev,
        collect = FALSE
      ),
      regexp = "n_workers",
      info = paste("n_workers =", paste(format(bad), collapse = ","))
    )
  }
})

test_that("a non-NULL but nonexistent dev_path errors instead of silently using installed code", {
  # Previously `is_dev` just went FALSE and the pool fell back to the INSTALLED
  # package -- so a typo'd dev path ran stale code and reported success. Asking
  # for a dev tree that is not there is a mistake, not a preference.
  expect_error(
    swereg:::parallel_pool(
      items = list(list(x = 1L)),
      worker_script = "worker_noop.R",
      n_workers = 1L,
      swereg_dev_path = "/nonexistent/dev/tree/xyzzy",
      collect = FALSE
    ),
    regexp = "swereg_dev_path"
  )
})
