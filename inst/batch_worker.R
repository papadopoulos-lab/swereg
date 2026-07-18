# Generic batch worker (shape A) -- the ONE worker script, the process boundary
# only (all dispatch logic lives in package code so it is unit-testable):
#
#   Rscript --vanilla batch_worker.R <input_envelope.qs2> <output_envelope.qs2>
#
# Read the envelope ONCE with bare `qs2::` (the consumer package is not loaded
# yet -- the envelope names where to load it from), structurally check the fields
# that decide WHAT CODE loads, load the consumer (and, once extracted, the
# runner) package, then hand the whole envelope to <runner>:::.batch_execute()
# (resolve + hash-verify the target, re-validate args, run, write result).
#
# Failure contract: any failure at or before .batch_execute() (unreadable or
# malformed envelope, a consumer/dev tree that will not load) writes NOTHING and
# exits non-zero -- the parent's exit-code channel plus the per-item log tail is
# the diagnostic path. A TARGET-level failure is different: .batch_execute() is
# total and returns a structured error envelope (exit 0).

argv <- commandArgs(trailingOnly = TRUE)
input_path <- argv[1L]
output_path <- argv[2L]

# Package-independent STRUCTURAL check, BEFORE any package/dev tree loads (base R
# + bare qs2 only): meta$dev_path / meta$package / meta$runner_package feed
# load_all()/requireNamespace() before the in-package .batch_check_envelope()
# could reject them. Exact `[[`, never `$`: `$` PARTIAL-matches, so `meta$dev_path`
# would resolve a field `dev_path_payload` -- letting a noncanonical field steer
# which code loads.
.batch_worker_check <- function(env) {
  if (!is.list(env) || anyDuplicated(names(env))) {
    stop("batch_worker: envelope is not a list, or has duplicate field names")
  }
  meta <- env[["meta"]]
  if (!is.list(meta) || anyDuplicated(names(meta))) {
    stop("batch_worker: envelope meta is not a list, or has duplicate field names")
  }
  is_str1 <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  for (f in c("package", "symbol", "hash", "id")) {
    if (!is_str1(meta[[f]])) {
      stop(sprintf("batch_worker: meta$%s missing or not a non-empty string", f))
    }
  }
  if (!is.null(meta[["runner_package"]]) && !is_str1(meta[["runner_package"]])) {
    stop("batch_worker: meta$runner_package is not a non-empty string")
  }
  if (!is.null(meta[["dev_path"]]) && !is_str1(meta[["dev_path"]])) {
    stop("batch_worker: meta$dev_path is not a valid path string")
  }
  invisible(TRUE)
}

env <- qs2::qs_read(input_path)
.batch_worker_check(env)  # validate structure BEFORE loading any code
meta <- env[["meta"]]
dev_path <- meta[["dev_path"]]
package <- meta[["package"]]
runner <- if (!is.null(meta[["runner_package"]])) meta[["runner_package"]] else package

suppressPackageStartupMessages({
  if (!is.null(dev_path)) {
    devtools::load_all(dev_path, quiet = TRUE)
  } else if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("could not load consumer package '%s'", package))
  }
  if (!identical(runner, package) &&
      !requireNamespace(runner, quietly = TRUE)) {
    stop(sprintf("could not load runner package '%s'", runner))
  }
})

ns <- asNamespace(runner)
result <- get(".batch_execute", envir = ns)(env)
get(".batch_write_envelope", envir = ns)(result, output_path)
