# TTEPlan$slurm_job(): one Slurm job description per pipeline stage.
#
# The job body is one `Rscript -e` call to swereg::tte_stage(). Nothing in this
# file writes a file. The analysis repository takes the returned object and
# writes the job script itself.
#
# Two values resolve HERE, at generation time, and reach the script as
# literals. The plan directory normalises to an absolute path, because a
# relative one resolves against the compute node's working directory. The
# swereg version is captured now, so the job refuses to start under a
# different one.
#
# `name` is derived from `self$project_prefix` and is never passed. Four
# projects share one queue, and a caller-supplied name lets two of them
# collide.

#' @include r6_tteplan.R
#' @description Describe one Slurm job that runs one pipeline stage. The job
#' body is a single `Rscript` call to [tte_stage()], and this method writes
#' nothing.
#'
#' The job name is derived, not passed: it is
#' `paste0(self$project_prefix, "_", stage)`. That is what keeps job names
#' unique across the projects that share one queue.
#'
#' @param stage One of `"s1"`, `"s2"` or `"s3"`.
#' @param n_workers Worker count for the stage, forwarded to [tte_stage()] as
#'   a named argument. The default is per stage: `default_n_workers("s1")` for
#'   `"s1"`, `1L` for `"s2"`, and `default_n_workers("s3")` for `"s3"`.
#' @param cpus Cores the job asks Slurm for. Defaults to `n_workers`.
#' @param mem Memory request in Slurm's own notation. Defaults to `"85G"`.
#' @param time Wall-clock limit, as `HH:MM:SS` or `D-HH:MM:SS`. There is no
#'   default, because a job that outlives its stage costs a queue slot.
#' @param requeue Logical(1). `TRUE` asks Slurm to requeue the job after a
#'   node failure.
#' @param exclusive Logical(1). `TRUE` asks for the whole node.
#' @return An object of class `slurm_it`, from `batchit::slurm_it()`.
TTEPlan$set(
  "public",
  "slurm_job",
  function(
    stage,
    n_workers = .slurm_job_workers(stage),
    cpus = n_workers,
    mem = "85G",
    time,
    requeue = TRUE,
    exclusive = TRUE
  ) {
    n_workers <- .validate_n_workers(n_workers, "slurm_job()")
    .slurm_job_assert_stage(stage)
    .plan_slurm_job(
      self,
      stage = stage,
      n_workers = n_workers,
      cpus = cpus,
      mem = mem,
      time = time,
      requeue = requeue,
      exclusive = exclusive
    )
  }
)

#' The three stage identities `slurm_job()` accepts.
#' @noRd
.SLURM_JOB_STAGES <- c("s1", "s2", "s3")

#' Stop unless `stage` names one pipeline stage.
#'
#' `switch()` on a value that is not one string reports `EXPR must be a length
#' 1 vector`, which names neither the argument nor the caller. This check runs
#' first so the message names both.
#'
#' @param stage The candidate stage identity.
#' @return `invisible(stage)`.
#' @noRd
.slurm_job_assert_stage <- function(stage) {
  ok <- is.character(stage) &&
    length(stage) == 1L &&
    !is.na(stage) &&
    stage %in% .SLURM_JOB_STAGES
  if (!ok) {
    stop(
      "slurm_job(): `stage` must be one of \"s1\", \"s2\" or \"s3\". Got: ",
      paste(utils::capture.output(utils::str(stage)), collapse = " "),
      call. = FALSE
    )
  }
  invisible(stage)
}

#' Default worker count for one stage.
#'
#' The mapping is explicit, and `default_n_workers("s2")` is deliberately
#' absent from it. That call reads `SWEREG_N_WORKERS_S2` and returns whatever
#' the variable says. s2 stays at one worker for per-ETT memory isolation, so
#' the mapping states `1L` and no environment variable can move it.
#'
#' @param stage One of `"s1"`, `"s2"` or `"s3"`.
#' @return Integer worker count.
#' @noRd
.slurm_job_workers <- function(stage) {
  .slurm_job_assert_stage(stage)
  switch(
    stage,
    s1 = default_n_workers("s1"),
    s2 = 1L,
    s3 = default_n_workers("s3")
  )
}

#' Build the `slurm_it` description of one stage job.
#'
#' @param plan The [TTEPlan].
#' @param stage One of `"s1"`, `"s2"` or `"s3"`.
#' @param n_workers Validated integer worker count.
#' @param cpus,mem,time,requeue,exclusive Passed to `batchit::slurm_it()`.
#' @return An object of class `slurm_it`.
#' @noRd
.plan_slurm_job <- function(
  plan,
  stage,
  n_workers,
  cpus,
  mem,
  time,
  requeue,
  exclusive
) {
  # Resolved on the submitting host, embedded as a literal. The active binding
  # `plan$dir_tteplan` carries no normalizePath() anywhere in its chain, so a
  # relative candidate reaches the script as a relative path.
  dir_tteplan <- normalizePath(
    plan$dir_tteplan,
    winslash = "/",
    mustWork = TRUE
  )
  .slurm_assert_no_whitespace(dir_tteplan, "dir_tteplan")

  name <- paste0(plan$project_prefix, "_", stage)

  # `n_workers` reaches tte_stage() BY NAME. The stage methods take their
  # arguments in different orders, and a positional forward binds the wrong
  # formal without reporting an error.
  expr <- sprintf(
    'swereg::tte_stage("%s", "%s", n_workers = %dL)',
    stage,
    dir_tteplan,
    n_workers
  )

  batchit::slurm_it(
    script = paste0("Rscript -e ", .slurm_shquote(expr)),
    name = name,
    cpus = cpus,
    mem = mem,
    time = time,
    requeue = requeue,
    exclusive = exclusive,
    require_r_package = c(
      swereg = as.character(utils::packageVersion("swereg"))
    )
  )
}

# `.slurm_assert_no_whitespace()` and `.slurm_shquote()` serve
# `.plan_slurm_job()` above. Nothing else in the package calls either one.

#' Stop when a path would break a `#SBATCH` directive.
#'
#' Slurm splits a directive on whitespace, and directive quoting has rules of
#' its own. A path field rejects every whitespace character instead, which
#' covers the line break too.
#'
#' @param x Character vector of path values headed for generated text.
#' @param field Character(1), the field name the error message reports.
#' @return `invisible(x)`.
#' @noRd
.slurm_assert_no_whitespace <- function(x, field) {
  x <- as.character(x)
  bad <- x[grepl("[[:space:]]", x)]
  if (length(bad) > 0L) {
    stop(
      "`",
      field,
      "` holds a whitespace character, and every path this function embeds ",
      "MUST hold none: ",
      paste(encodeString(bad, quote = "\""), collapse = ", "),
      ". A `#SBATCH` directive ends at the first whitespace character, so ",
      "Slurm would read the rest as a separate option.",
      call. = FALSE
    )
  }
  invisible(x)
}

#' Quote one literal for POSIX sh.
#'
#' Single quotes stop every expansion. An embedded single quote closes the
#' string, escapes itself, and reopens it.
#' @param x Character vector.
#' @return Character vector of quoted literals.
#' @noRd
.slurm_shquote <- function(x) {
  paste0("'", gsub("'", "'\\\\''", x), "'")
}
