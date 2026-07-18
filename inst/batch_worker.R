# Generic batch worker (shape A). The ONE worker script: it replaces the eight
# hand-written inst/worker_*.R dispatchers (plus their shared bootstrap) and
# the regex-verified do.call they amounted to. Invoked as:
#
#   Rscript --vanilla batch_worker.R <input_envelope.qs2> <output_envelope.qs2>
#
# It reads the input envelope with bare `qs2::` (the consumer package is not
# loaded yet -- the envelope itself names where to load it from), loads the
# consumer (and, once extracted, the runner) package, then hands the whole
# envelope to <runner>:::.batch_execute(), which resolves + verifies the target,
# re-validates the args against the child's own formals, runs it, and returns a
# result envelope. All of that logic lives in package code so it is unit-testable
# without a subprocess; this script is only the process boundary.

argv <- commandArgs(trailingOnly = TRUE)
input_path <- argv[1L]
output_path <- argv[2L]

# Read the dispatched id up front (best-effort) so that even the failure-fallback
# envelope names the item. The parent validates the id BEFORE the status, so a
# fallback carrying NA would surface as an "id mismatch" and mask the real load
# error; carrying the real id lets the load error surface instead.
# Exact `[[` extraction throughout, never `$`: `$` does PARTIAL name matching, so
# `meta$dev_path` would match a field named `dev_path_payload` when no exact
# `dev_path` exists -- letting a noncanonical field steer which code is loaded.
.worker_id <- tryCatch(qs2::qs_read(input_path)[["meta"]][["id"]],
  error = function(e) NA_character_)
if (!is.character(.worker_id) || length(.worker_id) != 1L) .worker_id <- NA_character_

# Package-independent STRUCTURAL check, run BEFORE any package or dev tree is
# loaded (base R + bare qs2 only). Without it, a malformed or duplicate-field
# envelope could steer which code the worker loads -- meta$dev_path,
# meta$package, meta$runner_package are consumed by load_all()/requireNamespace()
# before .batch_check_envelope() (which lives inside the not-yet-loaded package)
# could reject them. So the fields that decide WHAT to load are validated here
# first; .batch_execute() re-validates the whole envelope once loaded (defence in
# depth). Deliberately mirrors .batch_check_envelope(); it cannot call it, since
# that would require loading the package this check exists to gate.
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

.batch_worker_main <- function() {
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
}

tryCatch(
  .batch_worker_main(),
  error = function(e) {
    # Best-effort structured error envelope: the failure may be that the consumer
    # package could not be loaded, so this path must NOT depend on it -- bare
    # qs2 + rename only. If even this fails, exit non-zero so the parent's
    # exit-code channel reports the worker's death.
    tmp <- paste0(output_path, ".worker_err_tmp")
    # conditionMessage() dispatches, so a hostile condition could itself throw --
    # extract it safely (swereg may have failed to load, so this stays inline).
    emsg <- tryCatch(conditionMessage(e), error = function(e2) "<unprintable condition>")
    ok <- tryCatch(
      {
        out <- list(
          protocol = 1L,  # must match swereg:::.BATCH_PROTOCOL
          id = .worker_id,
          status = "error",
          value = NULL,
          error = list(message = emsg, call = "batch_worker"),
          target = NULL
        )
        qs2::qs_save(out, tmp)
        file.rename(tmp, output_path)
      },
      error = function(e2) FALSE
    )
    # Never leave the partial/renamed-from temp behind on failure -- the parent
    # only cleans the requested output path, not this suffix (blocker fixed in the
    # main codec, mirrored here).
    if (file.exists(tmp)) unlink(tmp, force = TRUE)
    if (!isTRUE(ok)) quit(save = "no", status = 1L)
  }
)
