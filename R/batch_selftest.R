# Internal fixtures for exercising the batch dispatcher through a REAL
# subprocess and a REAL mirai daemon. A dispatch target is a descriptor
# (package + symbol), never a closure -- deliberately, so it can be hash-verified
# across sessions -- which means a fixture defined inside a test is unreachable
# in the worker's freshly-loaded namespace. These live here, @noRd, and are used
# only by tests/testthat/test-batch_*.R.

#' @noRd
.batch_fixture_echo <- function(x) x

#' @noRd
.batch_fixture_boom <- function(message) stop(message, call. = FALSE)

#' @noRd
.batch_fixture_sleep <- function(seconds) {
  Sys.sleep(seconds)
  TRUE
}

# Sleeps, then echoes x. Lets a test force a specific COMPLETION order across
# workers -- e.g. make the NULL-returning item finish last, after a
# higher-indexed item has already filled its slot, which is the only order that
# exposes the results[[idx]] <- NULL deletion bug.
#' @noRd
.batch_fixture_slow_echo <- function(x, seconds) {
  Sys.sleep(seconds)
  x
}

#' @noRd
.batch_fixture_pid <- function() Sys.getpid()

# Terminates the worker HARD (no error condition, no result envelope), so the
# parent must fall back to its exit-code channel -- distinct from
# .batch_fixture_boom(), which raises a catchable error and returns a structured
# error envelope with exit status 0.
#' @noRd
.batch_fixture_crash <- function() quit(save = "no", status = 3L)

# Emits a warning and still returns a value (status "ok"), like
# .s3_enrollment_worker() does when a Table 1 sub-computation fails: the runner
# must carry that warning back to the parent, not lose it.
#' @noRd
.batch_fixture_warn <- function(x) {
  warning("fixture warning about ", x)
  x
}

# Returns a large object. Used to check that collect = FALSE drops the value
# before it is ever put into the result envelope (the shape-A memory guarantee).
#' @noRd
.batch_fixture_big <- function(n) {
  rep_len(42.0, n)
}

# Fails with a FIXED message that does not echo its argument. Lets the retention
# test prove that the argument VALUE is not persisted, without the value leaking
# via the error message (which .batch_fixture_boom would do by design).
#' @noRd
.batch_fixture_secret_fail <- function(payload) {
  stop("fixed failure with no argument echoed", call. = FALSE)
}

# Writes ~n_kb KB to EACH of stdout and stderr. Exercises the deadlock class
# that killed parallel_pool's pipe transport: a child out-writing the OS pipe
# buffer (64 KB on Linux) blocks forever in write() if the parent only reads
# after exit. .batch_run's file-backed logs must swallow this without blocking.
#' @noRd
.batch_fixture_chatty <- function(n_kb) {
  line <- paste(rep("x", 1023L), collapse = "")
  for (i in seq_len(n_kb)) {
    cat(line, "\n", sep = "", file = stdout())
    cat(line, "\n", sep = "", file = stderr())
  }
  invisible(NULL)
}
