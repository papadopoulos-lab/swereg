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
.fake_dev <- function(scripts) {
  dir <- file.path(tempfile("ppdev_"), "inst")
  dir.create(dir, recursive = TRUE)
  writeLines("params <- qs2::qs_read(args[2L])",
             file.path(dir, "worker_bootstrap.R"))
  for (nm in names(scripts)) writeLines(scripts[[nm]], file.path(dir, nm))
  dirname(dir)
}

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
