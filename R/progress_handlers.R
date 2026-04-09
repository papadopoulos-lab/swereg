#' Install a progressr handler that works in interactive R and RStudio jobs
#'
#' Sets `progressr::handlers(global = TRUE)` and installs
#' [progressr::handler_progress()] with a format chosen based on
#' `interactive()`:
#'
#' * **Interactive sessions** (normal R console, RStudio foreground console):
#'   use a `\r`-based single-line repaint with `clear = TRUE`. Same behavior
#'   you get from a terminal progress bar -- updates in place, disappears
#'   when the run finishes.
#' * **Non-interactive sessions** (RStudio background jobs spawned via
#'   *Source as Background Job* / `rstudioapi::jobRunScript()`, Rscript, CI):
#'   use a trailing `\n` with `clear = FALSE`. Each step is a new line in
#'   the log, finished bars stay in the scrollback, and `\r` (which job logs
#'   do not honor) is never emitted.
#'
#' Also forces `options("progressr.enable" = TRUE)` so progressr emits
#' signals in non-interactive sessions -- without this, every
#' `progressor()` emission is silently dropped in a jobRunScript subprocess
#' and no bar ever appears.
#'
#' @return Invisibly returns `NULL`.
#' @export
#' @examples
#' \dontrun{
#' swereg::setup_progress_handlers()
#' study$process_skeletons(skeleton_create, n_workers = 4L)
#' }
setup_progress_handlers <- function() {
  # Force progressr to report in non-interactive sessions (e.g. RStudio
  # background jobs where interactive() is FALSE). Without this the global
  # handler is installed but every progressor() emission is silently dropped.
  options("progressr.enable" = TRUE)
  progressr::handlers(global = TRUE)
  base_format <- "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta (last: :message)"
  if (interactive()) {
    progressr::handlers(progressr::handler_progress(
      format = base_format,
      clear  = TRUE
    ))
  } else {
    progressr::handlers(progressr::handler_progress(
      format = paste0(base_format, "\n"),
      clear  = FALSE
    ))
  }
  invisible(NULL)
}
