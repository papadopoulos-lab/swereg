#' Install a progressr handler for interactive R and for job logs
#'
#' Sets `progressr::handlers(global = TRUE)` and installs a handler chosen by
#' `interactive()`. An interactive session gets a repainting progress bar; a
#' job log gets one plain line per interval.
#'
#' @details
#' * **Interactive sessions** (normal R console, RStudio foreground console):
#'   [progressr::handler_progress()] with `clear = TRUE`. The bar repaints in
#'   place and disappears when the run finishes.
#' * **Non-interactive sessions** (Rscript, Slurm, CI, RStudio background jobs
#'   spawned via *Source as Background Job* / `rstudioapi::jobRunScript()`):
#'   one plain line on stderr, at most one per interval, plus one line when
#'   the progression finishes. The handler writes no carriage return, so
#'   `grep` and `tail` stay usable on the log file.
#'
#' A non-interactive line looks like this:
#'
#' ```
#' 2026-09-09T14:03:12 123/2194 (5.6%) elapsed 00:12:34 last: batch_00123
#' ```
#'
#' Set the interval in seconds with `options(swereg.progress_interval_s = ...)`.
#' The default is 600 seconds.
#'
#' Also forces `options("progressr.enable" = TRUE)` so progressr emits signals
#' in non-interactive sessions. Without this, every `progressor()` emission is
#' silently dropped in a background job and no progress ever appears.
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
  if (interactive()) {
    base_format <- paste0(
      "[:bar] :current/:total (:percent) in :elapsedfull, ",
      "eta: :eta (last: :message)"
    )
    progressr::handlers(progressr::handler_progress(
      format = base_format,
      clear  = TRUE
    ))
  } else {
    progressr::handlers(progress_line_handler())
  }
  return(invisible(NULL))
}

#' One plain progress line per interval, on stderr
#'
#' A progressr handler for a log file. It writes no carriage return, and it
#' writes at most one line per `interval` seconds plus one line at the finish.
#'
#' @param interval Seconds between printed lines.
#' @return A progressr progression handler.
#' @noRd
progress_line_handler <- function(
  interval = getOption("swereg.progress_interval_s", 600)
) {
  reporter <- local({
    start_time <- NULL
    last_time <- NULL
    last_step <- NULL
    last_signalled <- NULL

    emit <- function(step, max_steps, message) {
      now <- Sys.time()
      if (is.null(start_time)) start_time <<- now
      elapsed <- max(0, as.numeric(difftime(now, start_time, units = "secs")))
      percent <- if (max_steps > 0) 100 * step / max_steps else 100
      cat(
        sprintf(
          "%s %.0f/%.0f (%.1f%%) elapsed %02d:%02d:%02d last: %s\n",
          format(now, "%Y-%m-%dT%H:%M:%S"),
          step,
          max_steps,
          percent,
          as.integer(elapsed %/% 3600),
          as.integer((elapsed %% 3600) %/% 60),
          as.integer(elapsed %% 60),
          paste(c(message, ""), collapse = "")
        ),
        file = stderr()
      )
      last_time <<- now
      last_step <<- step
      return(invisible(NULL))
    }

    # `last_signalled` starts a RUN at 0, not at NULL. A run that signals
    # nothing has reached step 0, and the finish must report `0/<max_steps>`.
    # NULL means no run started, which happens only when a caller drives the
    # reporter directly. The finish then falls back to the step it is handed.
    reset <- function(...) {
      start_time <<- NULL
      last_time <<- NULL
      last_step <<- NULL
      last_signalled <<- 0L
      return(invisible(NULL))
    }

    # TRUE when the progression carries real progress.
    #
    # `with_progress()` sends synthetic progressions of type "shutdown" when
    # the expression ends, and each one adds a step the caller never signalled.
    # A 200-step progressor signalled 100 times receives shutdown steps 101 and
    # 102, so an unfiltered reporter ends a run at `101/200`. One signalled
    # zero times receives shutdown steps 1 and 2, and ends at `1/200`. Count
    # only what the caller signalled.
    is_real <- function(progression) {
      return(!identical(progression$type, "shutdown"))
    }

    list(
      reset = reset,
      hide = function(...) return(NULL),
      unhide = function(...) return(NULL),
      interrupt = function(...) return(NULL),
      initiate = function(...) {
        reset()
        start_time <<- Sys.time()
        last_time <<- start_time
        return(invisible(NULL))
      },
      update = function(config, state, progression = NULL, ...) {
        if (!is_real(progression)) {
          return(invisible(NULL))
        }
        last_signalled <<- state$step
        waited <- if (is.null(last_time)) {
          Inf
        } else {
          as.numeric(difftime(Sys.time(), last_time, units = "secs"))
        }
        if (waited >= interval || state$step >= config$max_steps) {
          emit(state$step, config$max_steps, state$message)
        }
        return(invisible(NULL))
      },
      finish = function(config, state, progression = NULL, ...) {
        # Report the last step the caller signalled, and never a count above
        # the total. `state$step` at the finish holds a shutdown increment, and
        # a step above the total prints `201/200 (100.5%)`. Both read as a
        # defect in the run.
        step <- if (is.null(last_signalled)) state$step else last_signalled
        step <- min(step, config$max_steps)
        if (is.null(last_step) || !identical(last_step, step)) {
          emit(step, config$max_steps, state$message)
        }
        return(invisible(NULL))
      }
    )
  })

  return(progressr::make_progression_handler(
    "swereg_line",
    reporter,
    interval = 0
  ))
}
