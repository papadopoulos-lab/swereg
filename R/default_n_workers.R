#' Resolve the default number of parallel workers
#'
#' Used as the default for `n_workers` across the heavy pipeline steps
#' ([RegistryStudy]'s `save_rawbatch()` / `process_skeletons()`, and
#' [TTEPlan]'s `s1_generate_enrollments_and_ipw()` / `s3_analyze()`). Resolves,
#' in order:
#' \enumerate{
#'   \item `getOption("swereg.n_workers")`
#'   \item the `SWEREG_N_WORKERS` environment variable
#'   \item fallback `max(1L, parallel::detectCores() - 2L)`
#' }
#' Set the per-host value once (e.g. `SWEREG_N_WORKERS` in the host's
#' `Renviron`) and every step picks it up; pass `n_workers` explicitly at the
#' call site to override for a single run.
#'
#' Note: `s2_generate_analysis_files_and_ipcw_pp()` deliberately does NOT use
#' this — it stays single-worker (`n_workers = 1L`) for per-ETT memory
#' isolation, and should keep that default.
#'
#' @return Integer worker count (>= 1).
#' @export
default_n_workers <- function() {
  opt <- getOption("swereg.n_workers", default = NA)
  if (!is.na(opt)) return(max(1L, as.integer(opt)))
  env <- Sys.getenv("SWEREG_N_WORKERS", unset = "")
  if (nzchar(env)) return(max(1L, as.integer(env)))
  max(1L, parallel::detectCores() - 2L)
}
