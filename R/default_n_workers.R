#' Resolve the default number of parallel workers for a pipeline stage
#'
#' Used as the default for `n_workers` across the heavy pipeline steps. Each
#' step passes its own `stage` tag so worker counts can be tuned independently
#' --- a memory-heavy stage never inherits a box-wide setting meant for a light
#' one. Resolution, in order:
#' \enumerate{
#'   \item `getOption("swereg.n_workers.<stage>")`
#'   \item the `SWEREG_N_WORKERS_<STAGE>` environment variable (stage upper-cased)
#'   \item fallback `1L` (serial)
#' }
#' The default is **1 worker everywhere**; parallelism is opt-in, per stage. For
#' example, to run the enrollment/IPW loop with 3 workers but keep the analysis
#' loop serial, set `SWEREG_N_WORKERS_S1=3` and leave `SWEREG_N_WORKERS_S3`
#' unset. Pass `n_workers` explicitly at the call site to override for a single
#' run.
#'
#' Stage tags currently used by the pipeline:
#' \describe{
#'   \item{`"rawbatch"`}{[RegistryStudy]'s `save_rawbatch()`}
#'   \item{`"skeleton"`}{[RegistryStudy]'s `process_skeletons()`}
#'   \item{`"s1"`}{[TTEPlan]'s `s1_generate_enrollments_and_ipw()` (~6 GB/worker)}
#'   \item{`"s3"`}{[TTEPlan]'s `s3_analyze()` --- peaks ~20 GB/worker on large
#'     "vs none" panels, so keep at 1 unless you have the RAM headroom}
#' }
#'
#' Note: `s2_generate_analysis_files_and_ipcw_pp()` deliberately does NOT use
#' this --- it stays single-worker (`n_workers = 1L`) for per-ETT memory
#' isolation.
#'
#' The former box-wide `SWEREG_N_WORKERS` (and `getOption("swereg.n_workers")`)
#' are retired: a single global knob could silently leak a high worker count
#' into a heavy stage (3 x ~20 GB s3 workers -> OOM). If the deprecated
#' `SWEREG_N_WORKERS` is still set, a one-time warning is emitted.
#'
#' @param stage Optional character stage tag (e.g. `"s1"`, `"s3"`,
#'   `"skeleton"`, `"rawbatch"`). When `NULL`, no per-stage override is
#'   consulted and the function returns `1L`.
#' @return Integer worker count (>= 1).
#' @export
default_n_workers <- function(stage = NULL) {
  .default_n_workers_impl(stage)
}

#' Usable core count, never `NA`
#'
#' [parallel::detectCores()] is documented to return `NA` when it cannot
#' determine the core count. Every unguarded use of it fed that `NA` either into
#' a division -- `floor(NA / n_workers)` gives `NA` threads, which only surfaces
#' much later inside a worker's `setDTthreads()`, a long way from the cause --
#' or straight into qs2's `nthreads`. One helper, so there is one place to be
#' wrong.
#'
#' @param fallback Value to use when the core count cannot be determined.
#' @return A positive integer.
#' @noRd
.safe_n_cores <- function(fallback = 1L) {
  n <- suppressWarnings(parallel::detectCores())
  if (length(n) != 1L || is.na(n) || !is.finite(n) || n < 1L) {
    return(as.integer(fallback))
  }
  as.integer(n)
}

#' Threads per worker: never `NA`, never zero
#'
#' @param n_workers Positive worker count.
#' @return A positive integer.
#' @noRd
.threads_per_worker <- function(n_workers) {
  max(1L, .safe_n_cores() %/% max(1L, as.integer(n_workers)))
}

#' Validate a worker count, loudly
#'
#' Callers used to do `as.integer(n_workers)` *before* any check, which silently
#' turned `2.5` into `2` -- so `parallel_pool()`'s own validation never saw the
#' bad value. Validate first, convert second.
#'
#' @param n_workers Candidate worker count.
#' @param what Caller name, for the error message.
#' @return `n_workers` as a positive integer.
#' @noRd
.validate_n_workers <- function(n_workers, what = "n_workers") {
  if (
    !is.numeric(n_workers) || length(n_workers) != 1L || is.na(n_workers) ||
      !is.finite(n_workers) || n_workers < 1L ||
      !isTRUE(n_workers == floor(n_workers))
  ) {
    stop(
      what, ": n_workers must be a single finite whole number >= 1, got: ",
      paste(utils::capture.output(utils::str(n_workers)), collapse = " "),
      call. = FALSE
    )
  }
  as.integer(n_workers)
}

.default_n_workers_impl <- function(stage = NULL) {
  # Deprecated box-wide global: warn once per session if it is still set so
  # stale env files (which now do nothing) get noticed.
  if (nzchar(Sys.getenv("SWEREG_N_WORKERS", unset = "")) &&
      !isTRUE(getOption("swereg.n_workers_global_warned"))) {
    warning(
      "SWEREG_N_WORKERS is deprecated and ignored. Worker counts default to 1 ",
      "and are opt-in per stage via SWEREG_N_WORKERS_<STAGE> (e.g. ",
      "SWEREG_N_WORKERS_S1, SWEREG_N_WORKERS_SKELETON) or the equivalent ",
      "options(swereg.n_workers.<stage> = ).",
      call. = FALSE
    )
    options(swereg.n_workers_global_warned = TRUE)
  }

  if (!is.null(stage)) {
    opt <- getOption(paste0("swereg.n_workers.", stage), default = NA)
    if (!is.na(opt)) return(max(1L, as.integer(opt)))
    env <- Sys.getenv(paste0("SWEREG_N_WORKERS_", toupper(stage)), unset = "")
    if (nzchar(env)) return(max(1L, as.integer(env)))
  }

  1L
}
