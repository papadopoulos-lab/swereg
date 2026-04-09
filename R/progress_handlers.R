#' Choose a progressr handler that works in the current launch context
#'
#' Sets `progressr::handlers(global = TRUE)` and selects a sensible handler
#' based on whether the current R session can drive the RStudio Jobs pane:
#'
#' * If `rstudioapi` is available and exposes `jobAdd()`, `jobSetProgress()`,
#'   and `jobRemove()`, the "rstudio" handler is installed
#'   ([progressr::handler_rstudio()]). Progress is drawn as a virtual RStudio
#'   Jobs-pane bar via `rstudioapi::jobAdd()` / `rstudioapi::jobSetProgress()`.
#'   This works both in the interactive RStudio console and inside a background
#'   job launched via `rstudioapi::jobRunScript()` / RStudio's "Source as
#'   Background Job" menu, where the default text bar renders poorly in the
#'   job log.
#' * Otherwise (plain R, `Rscript`, non-RStudio front-ends, RStudio Server
#'   without the jobs API), falls back to `progressr::handler_progress()`
#'   with the given `format`.
#'
#' Intended to be called once at the top of a run script, replacing the
#' hand-rolled
#'
#' ```r
#' progressr::handlers(global = TRUE)
#' progressr::handlers(progressr::handler_progress(format = "..."))
#' ```
#'
#' boilerplate. Safe to call multiple times. Because the choice is made via
#' feature detection on `rstudioapi`, no changes are needed to swereg's
#' long-running methods (`RegistryStudy$process_skeletons`,
#' `TTEPlan$s1_generate_enrollments_and_ipw`,
#' `TTEPlan$s2_generate_analysis_files_and_ipcw_pp`, `TTEPlan$s3_analyze`) —
#' they all already emit `progressr::progressor()` signals and automatically
#' render through whichever handler is active.
#'
#' @param format Format string passed to [progressr::handler_progress()] in
#'   the text-fallback branch. Ignored when the RStudio handler is chosen.
#' @return Invisibly returns `NULL`.
#' @export
#' @examples
#' \dontrun{
#' swereg::setup_progress_handlers()
#' study$process_skeletons(skeleton_create, n_workers = 4L)
#' }
setup_progress_handlers <- function(
  format = ":bar :percent :elapsed / :eta (last: :message)"
) {
  progressr::handlers(global = TRUE)
  # Detect whether we can drive the RStudio Jobs pane. Two valid contexts:
  #   (1) running inside the RStudio IDE itself -> isAvailable() == TRUE.
  #   (2) running inside an RStudio background job subprocess spawned via
  #       rstudioapi::jobRunScript() -> isAvailable() == FALSE (because the
  #       subprocess's .Platform$GUI is not "RStudio"), but isJob() == TRUE
  #       (because RStudio sets the RSTUDIOAPI_IPC_REQUESTS_FILE env var in
  #       the subprocess). In that case, rstudioapi::callFun() auto-delegates
  #       jobAdd/jobSetProgress/jobRemove to the parent RStudio session via
  #       IPC, so handler_rstudio works correctly.
  in_rstudio <- requireNamespace("rstudioapi", quietly = TRUE) &&
    (rstudioapi::isAvailable() ||
       (rstudioapi::hasFun("isJob") &&
          tryCatch(rstudioapi::isJob(), error = function(e) FALSE)))
  use_rstudio <- in_rstudio &&
    rstudioapi::hasFun("jobAdd") &&
    rstudioapi::hasFun("jobSetProgress") &&
    rstudioapi::hasFun("jobRemove")
  if (use_rstudio) {
    progressr::handlers("rstudio")
  } else {
    progressr::handlers(progressr::handler_progress(format = format))
  }
  invisible(NULL)
}
