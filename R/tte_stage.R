# =============================================================================
# tte_stage(): one call per pipeline stage
# =============================================================================
# The three stage methods take their arguments in DIFFERENT orders and none of
# them takes `...`:
#
#   s1  output_dir, impute_fn, stabilize, n_workers, swereg_dev_path
#   s2  output_dir, estimate_ipcw_pp_separately_by_treatment,
#       estimate_ipcw_pp_with_gam, n_workers, swereg_dev_path
#   s3  enrollment_ids, ett_ids, output_dir, swereg_dev_path, n_workers
#
# s3 reverses the last two. A positional forward therefore binds n_workers to
# enrollment_ids and reports no error, so every forward here is by name.

#' Run one target trial emulation pipeline stage
#'
#' Loads the plan in `dir_tteplan`, runs the stage method, then runs the steps
#' that follow it. This is the body of the per-project `s1.R`, `s2.R` and
#' `s3.R` stage scripts as one call.
#'
#' @details
#' The stage id selects the method and the steps after it:
#'
#' | `stage` | method | steps after the method |
#' |---|---|---|
#' | `"s1"` | `$s1_generate_enrollments_and_ipw()` | `$save()`, then `$print_target_checklist()` |
#' | `"s2"` | `$s2_generate_analysis_files_and_ipcw_pp()` | n/a |
#' | `"s3"` | `$s3_analyze()` | `$results_summary()`, then `$save()` |
#'
#' Every element of `...` MUST carry a name. `tte_stage()` matches each name
#' against the formals of the stage method, and forwards by name.
#'
#' The three methods take their arguments in different orders, and none of them
#' takes `...`. A positional forward binds the wrong formal and reports no
#' error. Naming every argument is what makes the forward safe.
#'
#' @section Why an unknown name fails early:
#'
#' `tte_stage()` rejects a name that the stage method does not declare BEFORE
#' it calls [tteplan_locate_and_load()]. That load reads the plan from a
#' network share and is slow. A mistyped argument name therefore costs no
#' load. `do.call()` would also reject the name, but only after the load.
#'
#' The formals come from the [TTEPlan] generator, so the check needs no plan.
#'
#' @param stage One of `"s1"`, `"s2"` or `"s3"`.
#' @param dir_tteplan Character vector of candidate directories, in priority
#'   order, where `tteplan.qs2` lives. Passed to
#'   [tteplan_locate_and_load()], which takes the first one that exists.
#' @param ... Arguments for the stage method. Each one MUST be named.
#' @return The [TTEPlan], invisibly.
#' @seealso [tteplan_locate_and_load()], [TTEPlan].
#'   `vignette("tte-workflow")` describes the stage scripts.
#' @family tte_plan
#' @export
#' @examples
#' \dontrun{
#' swereg::tte_stage(
#'   "s1",
#'   "~/plans/003-iliadis-stroke",
#'   n_workers = 6L,
#'   swereg_dev_path = NULL
#' )
#' }
tte_stage <- function(stage, dir_tteplan, ...) {
  if (!is.character(stage) || length(stage) != 1L || is.na(stage)) {
    stop(
      "tte_stage(): `stage` must be a single string, one of \"s1\", ",
      "\"s2\" or \"s3\".",
      call. = FALSE
    )
  }
  # switch(), never string concatenation: a typo must fail here, and must not
  # build a method name that does not exist.
  method <- switch(
    stage,
    s1 = "s1_generate_enrollments_and_ipw",
    s2 = "s2_generate_analysis_files_and_ipcw_pp",
    s3 = "s3_analyze",
    stop(
      sprintf(
        "tte_stage(): `stage` must be \"s1\", \"s2\" or \"s3\". Got \"%s\".",
        stage
      ),
      call. = FALSE
    )
  )

  # Validate BEFORE the load. See "Why an unknown name fails early".
  args <- .tte_stage_args(list(...), method)

  setup_progress_handlers()
  plan <- tteplan_locate_and_load(dir_tteplan)
  do.call(plan[[method]], args)

  switch(
    stage,
    s1 = {
      plan$save()
      plan$print_target_checklist()
    },
    s2 = NULL,
    s3 = {
      plan$results_summary()
      plan$save()
    }
  )
  invisible(plan)
}

#' Check the forwarded arguments of one stage
#'
#' Returns `args` unchanged when every element carries a name and every name is
#' a formal of `method`. Errors otherwise.
#'
#' @param args `list(...)` from [tte_stage()].
#' @param method Name of the [TTEPlan] method that receives the arguments.
#' @return `args`, unchanged.
#' @noRd
.tte_stage_args <- function(args, method) {
  nms <- names(args)
  if (length(args) > 0L && (is.null(nms) || any(!nzchar(nms)))) {
    stop(
      "tte_stage(): every argument in `...` must be named. ",
      method,
      "() takes no `...`, so an unnamed argument binds a formal by position ",
      "and the wrong formal receives it.",
      call. = FALSE
    )
  }
  allowed <- names(formals(TTEPlan$public_methods[[method]]))
  unknown <- setdiff(nms, allowed)
  if (length(unknown) > 0L) {
    stop(
      "tte_stage(): ",
      method,
      "() has no argument ",
      toString(unknown),
      ". It takes: ",
      toString(allowed),
      ".",
      call. = FALSE
    )
  }
  args
}
