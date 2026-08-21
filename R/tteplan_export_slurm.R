# =============================================================================
# tteplan_export_slurm(): a TTEPlan as a Slurm job chain
# =============================================================================
# The generator writes text and returns paths. Nothing in this file submits
# anything, and only the generated driver script names sbatch. That is what
# makes the whole chain testable on a machine with no scheduler.
#
# Every literal a generated script needs is resolved here, at generation time:
# the data directory, the filesystem type under it, the repository root and the
# per-stage resources. A generated script reads no configuration file, so the
# text on disk is the whole record of what will run.
# =============================================================================

# A Slurm job name, and every file name derived from it, hold these characters
# and no others, and start with a letter or a digit.
#
# The leading character is the load-bearing half. A name that starts with `-`
# is a legal file name, and a command that receives it positionally reads it
# as an option instead. Measured on R 4.6.0 and bash 5.2: `Rscript
# --version.R` prints `file name is missing` and exits 1, and `cd -d` reports
# an invalid option. `sbatch` parses with `getopt_long`, so `-proj_s1.sh`
# enters the option stream rather than naming the script.
.SLURM_NAME_PATTERN <- "^[A-Za-z0-9][A-Za-z0-9_.-]*$"

# The driver's own identity, and the one stage identity no chain may hold. One
# constant, so the reserved word and the file name cannot drift apart.
.SLURM_DRIVER_STAGE_ID <- "submit"

# Every value that reaches generated text, named as the caller names it.
#
# A generated script is executable shell, and a `#SBATCH` directive and a
# comment both end at the first newline. A value carrying a newline therefore
# escapes its line and the rest becomes a command. A stage path of
# `x\nsbatch --wrap=true #/s1.R` has a valid basename and a valid identity, and
# it puts a live `sbatch` line into a job script.
#
# A `#SBATCH` directive also ends at the first whitespace character, and Slurm
# reads what follows as a separate token. `#SBATCH --output=/tmp/a b/x.out`
# therefore sets the output to `/tmp/a` and leaves `b/x.out` in the option
# stream. Directive quoting has rules of its own, so a path field rejects every
# whitespace character instead.
#
# `tteplan_export_slurm()` checks every field here before it writes anything,
# and `tests/testthat/test-tteplan_export_slurm.R` sweeps this same constant.
# The value is the rule tag, and the sweep reads it: add a field, or change a
# tag, without a matching injector and the sweep fails.
#
#   line   one line, and nothing else
#   path   no whitespace character at all, which covers a line break
#   count  NULL, NA, or one non-negative whole number
#   cpus   .SLURM_CPUS_PATTERN
#   mem    .SLURM_MEM_PATTERN
.SLURM_EMBEDDED_FIELDS <- c(
  project_prefix = "line",
  spec_version = "line",
  expected_n_ids = "count",
  expected_skeleton_file_count = "count",
  dir_tteplan = "path",
  repo = "path",
  dir = "path",
  stages = "path",
  cpus = "cpus",
  mem = "mem",
  fstype = "line"
)

# A memory request in Slurm's own notation.
.SLURM_MEM_PATTERN <- "^[0-9]+[KMGT]?$"

# A core count. One non-negative integer, and nothing else.
.SLURM_CPUS_PATTERN <- "^[0-9]+$"

#' Stop when a value would escape its line in generated text.
#'
#' @param x Character vector of values headed for generated text.
#' @param field Character(1), the field name the error message reports.
#' @return `invisible(x)`.
#' @noRd
.slurm_assert_single_line <- function(x, field) {
  x <- as.character(x)
  bad <- x[grepl("[\r\n]", x)]
  if (length(bad) > 0L) {
    stop(
      "`",
      field,
      "` holds a line break, and every value this function embeds MUST be one ",
      "line: ",
      paste(encodeString(bad, quote = "\""), collapse = ", "),
      ". A comment and a `#SBATCH` directive both end at the first newline, ",
      "so the rest of the value would run as a command.",
      call. = FALSE
    )
  }
  invisible(x)
}

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

#' Stage identity from a stage script path.
#'
#' `"s4_export.R"` and `"scripts/s4_export.R"` both give `"s4_export"`.
#' @param stages Character vector of stage script paths.
#' @return Character vector of stage identities.
#' @noRd
.slurm_stage_ids <- function(stages) {
  tools::file_path_sans_ext(basename(stages))
}

#' Replace every character Slurm and the filesystem cannot carry.
#'
#' @param x Character(1).
#' @return Character(1) holding only `[A-Za-z0-9_.-]`.
#' @noRd
.slurm_sanitize <- function(x) {
  gsub("[^A-Za-z0-9_.-]+", "_", x)
}

#' Format a progress denominator for a comment line.
#'
#' `NULL` and `NA` give `"unknown"`, because a plan built before a count was
#' measured carries neither. Anything else MUST be one non-negative whole
#' number, so no string reaches the comment line unchecked.
#'
#' @param x Integer, numeric, NULL or NA.
#' @param field Character(1), the field name the error message reports.
#' @return Character(1) holding digits, or `"unknown"`.
#' @noRd
.slurm_count_text <- function(x, field) {
  if (is.null(x)) {
    return("unknown")
  }
  if (length(x) == 1L && is.na(x)) {
    return("unknown")
  }
  ok <- length(x) == 1L &&
    is.numeric(x) &&
    is.finite(x) &&
    x >= 0 &&
    x == trunc(x)
  if (!ok) {
    stop(
      "`",
      field,
      "` MUST be NULL, NA, or one non-negative whole number. Got: ",
      paste(encodeString(as.character(x), quote = "\""), collapse = ", "),
      call. = FALSE
    )
  }
  format(x, scientific = FALSE, trim = TRUE)
}

#' Resolve one resource argument to a per-stage character vector.
#'
#' A length-1 unnamed value applies to every stage. A named vector MUST name
#' every stage identity exactly once. An unknown name is an error, so a
#' mistyped stage never falls back to a default no one asked for.
#'
#' @param value The `cpus` or `mem` argument.
#' @param stage_ids Character vector of stage identities.
#' @param arg_name Character(1), the argument name used in error messages.
#' @param pattern Character(1) regular expression every value MUST match. The
#'   value reaches a `#SBATCH` directive, so this is what keeps a line break
#'   and a shell metacharacter out of it.
#' @return Character vector, one element per stage, named by stage identity.
#' @noRd
.slurm_resource <- function(value, stage_ids, arg_name, pattern) {
  if (length(value) == 0L) {
    stop("`", arg_name, "` must not be empty.", call. = FALSE)
  }
  .slurm_assert_single_line(value, arg_name)
  bad <- as.character(value)[!grepl(pattern, as.character(value))]
  if (length(bad) > 0L) {
    stop(
      "`",
      arg_name,
      "` MUST match ",
      pattern,
      ": ",
      paste(encodeString(bad, quote = "\""), collapse = ", "),
      call. = FALSE
    )
  }
  nm <- names(value)
  if (is.null(nm)) {
    if (length(value) != 1L) {
      stop(
        "`",
        arg_name,
        "` is length ",
        length(value),
        " and unnamed. Give one value for every stage, or name every element ",
        "after a stage.",
        call. = FALSE
      )
    }
    return(stats::setNames(
      rep(as.character(value), length(stage_ids)),
      stage_ids
    ))
  }
  if (any(!nzchar(nm))) {
    stop(
      "`",
      arg_name,
      "` mixes named and unnamed elements. Name every element after a stage.",
      call. = FALSE
    )
  }
  if (anyDuplicated(nm) > 0L) {
    stop(
      "`",
      arg_name,
      "` names a stage twice: ",
      paste(unique(nm[duplicated(nm)]), collapse = ", "),
      call. = FALSE
    )
  }
  unknown <- setdiff(nm, stage_ids)
  if (length(unknown) > 0L) {
    stop(
      "`",
      arg_name,
      "` names no stage of this chain: ",
      paste(unknown, collapse = ", "),
      ". The stages are: ",
      paste(stage_ids, collapse = ", "),
      call. = FALSE
    )
  }
  absent <- setdiff(stage_ids, nm)
  if (length(absent) > 0L) {
    stop(
      "`",
      arg_name,
      "` names no value for: ",
      paste(absent, collapse = ", "),
      ". A named `",
      arg_name,
      "` MUST cover every stage.",
      call. = FALSE
    )
  }
  out <- as.character(value)[match(stage_ids, nm)]
  stats::setNames(out, stage_ids)
}

#' Read the filesystem type of one directory.
#'
#' Calls GNU `stat -f -c %T`. The generated job script runs the same command
#' and compares its answer against the one this function observed.
#'
#' @param path Character(1), an existing directory.
#' @return Character(1), the filesystem type.
#' @noRd
.slurm_fstype <- function(path) {
  out <- suppressWarnings(system2(
    "stat",
    c("-f", "-c", "%T", "--", shQuote(path)),
    stdout = TRUE,
    stderr = FALSE
  ))
  failed <- !is.null(attr(out, "status")) ||
    length(out) != 1L ||
    !nzchar(out[[1L]])
  if (failed) {
    stop(
      "tteplan_export_slurm() needs GNU `stat -f -c %T` to read the ",
      "filesystem type of ",
      path,
      ". The command failed or is absent on this host.",
      call. = FALSE
    )
  }
  # `system2()` splits on newlines, so a two-line answer already failed the
  # length check above. This asserts the field anyway, because machine output
  # reaches generated text like every other value.
  .slurm_assert_single_line(out[[1L]], "fstype")
  out[[1L]]
}

#' Build the text of one job script.
#'
#' @param job_name Character(1), the Slurm job name.
#' @param stage Character(1), the stage script path relative to `repo`.
#' @param stage_id Character(1), the stage identity.
#' @param index,n_stages Integer(1), the stage position and the chain length.
#' @param cpus,mem Character(1), the resources for this stage.
#' @param repo_abs,slurm_abs,dir_tteplan Character(1) absolute directories.
#' @param touch_path Character(1), the path the job reads before the type check.
#' @param fstype Character(1), the filesystem type observed at generation time.
#' @param spec_version,n_ids,n_skeletons Character(1) header values.
#' @return Character vector of lines.
#' @noRd
.slurm_job_script <- function(
  job_name,
  stage,
  stage_id,
  index,
  n_stages,
  cpus,
  mem,
  repo_abs,
  slurm_abs,
  dir_tteplan,
  touch_path,
  fstype,
  spec_version,
  n_ids,
  n_skeletons
) {
  c(
    "#!/bin/bash",
    paste0("#SBATCH --job-name=", job_name),
    paste0("#SBATCH --cpus-per-task=", cpus),
    paste0("#SBATCH --mem=", mem),
    paste0("#SBATCH --output=", slurm_abs, "/", job_name, "_%j.out"),
    paste0("#SBATCH --error=", slurm_abs, "/", job_name, "_%j.err"),
    "#",
    "# Generated by swereg::tteplan_export_slurm(). Do not edit.",
    paste0("# spec version: ", spec_version),
    paste0(
      "# stage ",
      index,
      " of ",
      n_stages,
      ": ",
      stage_id,
      " (script ",
      stage,
      ")"
    ),
    paste0(
      "# progress denominators: ",
      n_ids,
      " ids across ",
      n_skeletons,
      " skeleton files"
    ),
    "#",
    "# The driver script submits this file. This file submits nothing.",
    "",
    "set -euo pipefail",
    "",
    paste0("swereg_dir_tteplan=", .slurm_shquote(dir_tteplan)),
    paste0("swereg_touch_path=", .slurm_shquote(touch_path)),
    paste0("swereg_expect_fstype=", .slurm_shquote(fstype)),
    paste0("swereg_repo=", .slurm_shquote(repo_abs)),
    # Absolute, so the path always starts with `/` and no command can read it
    # as an option, whatever the caller named the stage or its directory.
    paste0(
      "swereg_stage_script=",
      .slurm_shquote(paste0(repo_abs, "/", stage))
    ),
    paste0(
      "swereg_mem_log=",
      .slurm_shquote(paste0(slurm_abs, "/", job_name, "_mem.log"))
    ),
    "",
    "# Mount check. Read a path under the data directory before you read the",
    "# filesystem type. An automounted share stays unmounted until something",
    "# reads a path under it, and a type read on an unmounted automount",
    "# reports the local disk. That order fails a healthy job.",
    "head -c 1 -- \"$swereg_touch_path\" > /dev/null",
    "swereg_fstype=\"$(stat -f -c %T -- \"$swereg_dir_tteplan\")\"",
    "if [ \"$swereg_fstype\" != \"$swereg_expect_fstype\" ]; then",
    "  printf 'swereg: %s is on filesystem %s, expected %s\\n' \\",
    "    \"$swereg_dir_tteplan\" \"$swereg_fstype\" \"$swereg_expect_fstype\" >&2",
    "  exit 1",
    "fi",
    "",
    "# Peak memory, written on every exit. The scheduler's own accounting is",
    "# empty on some builds, so this job reads the kernel counter itself.",
    "swereg_record_peak_memory() {",
    "  if [ -r /sys/fs/cgroup/memory.peak ]; then",
    "    printf 'memory_peak_bytes %s\\n' \\",
    "      \"$(cat -- /sys/fs/cgroup/memory.peak)\" > \"$swereg_mem_log\"",
    "  elif [ -r /proc/self/status ]; then",
    "    printf 'vmhwm_kb %s\\n' \\",
    "      \"$(awk -- '/^VmHWM:/ { print $2 }' /proc/self/status)\" \\",
    "      > \"$swereg_mem_log\"",
    "  else",
    "    printf 'peak_memory_unavailable\\n' > \"$swereg_mem_log\"",
    "  fi",
    "}",
    "trap swereg_record_peak_memory EXIT",
    "",
    "# The plan on disk MUST carry the schema version this swereg understands.",
    "# The path after the expression takes no `--` terminator: Rscript passes",
    "# `--` through as the argument itself. The path is absolute instead.",
    paste0(
      "Rscript -e ",
      "'swereg::tteplan_locate_and_load(commandArgs(TRUE)[1])$check_version()'",
      " \"$swereg_dir_tteplan\""
    ),
    "",
    "cd -- \"$swereg_repo\"",
    "Rscript \"$swereg_stage_script\""
  )
}

#' Build the text of the driver script.
#'
#' @param job_names Character vector of job names, in submission order.
#' @param stage_ids Character vector of stage identities, in the same order.
#' @param no_requeue Character vector of stage identities to submit
#'   `--no-requeue`.
#' @param prefix Character(1), the sanitized project prefix.
#' @param spec_version Character(1).
#' @param slurm_abs Character(1), the absolute directory holding the scripts.
#' @return Character vector of lines.
#' @noRd
.slurm_driver_script <- function(
  job_names,
  stage_ids,
  no_requeue,
  prefix,
  spec_version,
  slurm_abs
) {
  head_lines <- c(
    "#!/bin/bash",
    "#",
    "# Generated by swereg::tteplan_export_slurm(). Do not edit.",
    paste0("# spec version: ", spec_version),
    paste0("# project: ", prefix),
    paste0(
      "# ",
      length(stage_ids),
      " stages: ",
      paste(stage_ids, collapse = ", ")
    ),
    "#",
    "# Submits one job per stage. Each dependant starts only after the stage",
    "# before it finishes with exit code 0.",
    "",
    "set -euo pipefail",
    "",
    "# Slurm records the submitting directory as each job's working directory.",
    "cd -- \"$(dirname -- \"$0\")\"",
    ""
  )
  var_names <- sprintf("swereg_jid_%02d", seq_along(job_names))
  body <- character(0)
  for (i in seq_along(job_names)) {
    flags <- "--parsable"
    if (i > 1L) {
      flags <- c(
        flags,
        paste0("--dependency=afterok:\"$", var_names[[i - 1L]], "\""),
        "--kill-on-invalid-dep=yes"
      )
    }
    if (stage_ids[[i]] %in% no_requeue) {
      flags <- c(flags, "--no-requeue")
    }
    body <- c(
      body,
      # `--` stops sbatch's option scanning, and the job script path is
      # absolute. Either one alone keeps a job name out of the option stream.
      paste0(
        var_names[[i]],
        "=\"$(sbatch ",
        paste(flags, collapse = " "),
        " -- ",
        .slurm_shquote(paste0(slurm_abs, "/", job_names[[i]], ".sh")),
        ")\""
      ),
      paste0(
        "printf 'submitted %s as %s\\n' ",
        .slurm_shquote(stage_ids[[i]]),
        " \"$",
        var_names[[i]],
        "\""
      ),
      ""
    )
  }
  c(head_lines, body)
}

#' Export a TTEPlan as a Slurm job chain
#'
#' @description
#' Writes one Slurm job script per stage, plus a driver script that chains
#' them with `--dependency=afterok`. This function never calls `sbatch`: it
#' writes files and returns their paths.
#'
#' @details
#' A [TTEPlan] already holds what a job chain needs. `project_prefix` names
#' the jobs, `spec_version` labels them, and `expected_n_ids` with
#' `expected_skeleton_file_count` give the progress denominators. The caller
#' supplies the two facts the plan does not hold: the repository root, and the
#' stage script names inside it.
#'
#' @section Generated files:
#' The function writes into `file.path(dir, "slurm")` and creates that
#' directory when it is absent:
#' \describe{
#'   \item{`<prefix>_<stage>.sh`}{One job script per stage. It runs the mount
#'     check, calls `check_version()` on the plan, runs the stage script, and
#'     writes `<prefix>_<stage>_mem.log`.}
#'   \item{`<prefix>_submit.sh`}{The driver. It is the only generated file
#'     that names `sbatch`.}
#' }
#' `<prefix>` is `plan$project_prefix`, with each run of characters outside
#' `[A-Za-z0-9_.-]` replaced by one `_`. `<stage>` is the stage identity, which
#' is `tools::file_path_sans_ext(basename(stage))`.
#'
#' Every derived name MUST match `^[A-Za-z0-9][A-Za-z0-9_.-]*$`. The leading
#' character carries the rule. A name that starts with `-` is a legal file
#' name, and a command can read it as an option.
#'
#' @section Injection:
#' A generated script is executable shell. A comment and a `#SBATCH` directive
#' both end at the first line break. A value that carries one therefore escapes
#' its line, and the rest of it becomes a command. The function checks every
#' value it embeds before it writes anything, and it names the field it
#' rejects.
#'
#' Eleven fields reach generated text, under five rules:
#' \describe{
#'   \item{one line}{`project_prefix`, `spec_version`, and the filesystem type
#'     `stat` reports.}
#'   \item{no whitespace at all}{`dir_tteplan`, `repo`, `dir` and `stages`.
#'     These four are paths. A `#SBATCH` directive ends at the first whitespace
#'     character, and Slurm reads the rest as a separate option, so
#'     `#SBATCH --output=/tmp/a b/x.out` writes to `/tmp/a`.}
#'   \item{a whole number}{`expected_n_ids` and
#'     `expected_skeleton_file_count`, or `NULL`, or `NA`.}
#'   \item{`^[0-9]+$`}{`cpus`.}
#'   \item{`^[0-9]+[KMGT]?$`}{`mem`.}
#' }
#'
#' The check covers a whole stage path, and not its file name alone. Take a
#' stage of `x<newline>sbatch --wrap=true #/s1.R`. Its file name and its
#' identity are both valid, and it puts a live `sbatch` line into a job script.
#'
#' @section Reuse:
#' The exporter overwrites the files it generates. It removes no other file, so
#' `file.path(dir, "slurm")` MAY hold more files than this call returns. Run it
#' again with a stage removed or renamed, and that stage's job script stays on
#' disk. The driver names only the current stage set, so the chain does not
#' submit a stale script. Delete `file.path(dir, "slurm")` first when you want
#' the directory to hold this chain and nothing else.
#'
#' @section What the generated chain guarantees:
#' \describe{
#'   \item{One job per stage}{`sacct` then reports each stage's own state,
#'     elapsed time and exit code.}
#'   \item{`--no-requeue` on a destructive stage}{`$s1_generate_enrollments_and_ipw()`
#'     deletes its work directory at startup. A requeue after a node failure
#'     restarts the script, which deletes the partial output and the evidence.
#'     Name that stage in `no_requeue`.}
#'   \item{`--kill-on-invalid-dep=yes` on every dependant}{A job whose
#'     `afterok` dependency can never be satisfied otherwise sits in the queue
#'     forever as `DependencyNeverSatisfied`.}
#'   \item{A mount check inside the job}{A queued stage can start days later,
#'     and an unmounted mount point is still a directory. The job reads a path
#'     under `plan$dir_tteplan` first, then compares `stat -f -c %T` against
#'     the filesystem type this function observed at generation time.}
#'   \item{Its own peak-memory reading}{The job samples
#'     `/sys/fs/cgroup/memory.peak`, and falls back to `VmHWM` in
#'     `/proc/self/status`. It never reads the scheduler's accounting, which
#'     is empty on some builds.}
#'   \item{No path in the option stream}{Every path a generated command takes
#'     positionally is absolute, apart from `$0` in the driver. `sbatch`, `cd`,
#'     `dirname`, `head`, `stat`, `cat` and `awk` each take `--` as well.}
#' }
#'
#' @section Errors:
#' The function stops, and writes nothing, on any of these:
#' \itemize{
#'   \item `stages` is empty.
#'   \item Two stages share one identity.
#'   \item A stage script is absent under `repo`.
#'   \item A stage script file name does not match
#'     `^[A-Za-z0-9][A-Za-z0-9_.-]*$`.
#'   \item A derived job or file name does not match that same pattern.
#'   \item A stage identity is `submit`, which the driver reserves.
#'   \item Any embedded value holds a line break. See the Injection section.
#'   \item `dir`, `repo`, `dir_tteplan` or a stage path holds any whitespace
#'     character.
#'   \item `cpus` does not match `^[0-9]+$`, or `mem` does not match
#'     `^[0-9]+[KMGT]?$`.
#'   \item `expected_n_ids` or `expected_skeleton_file_count` is neither
#'     `NULL`, nor `NA`, nor one non-negative whole number.
#'   \item `cpus`, `mem` or `no_requeue` names a stage the chain does not hold.
#'   \item A named `cpus` or `mem` leaves a stage unnamed.
#' }
#'
#' @param plan A [TTEPlan] with `dir_tteplan_cp` set.
#' @param repo Character(1). The analysis repository root that holds the stage
#'   scripts.
#' @param dir Character(1). The directory to write the `slurm/` sub-directory
#'   into. Defaults to the working directory.
#' @param stages Character vector of stage script paths, relative to `repo`,
#'   in submission order.
#' @param cpus Cores per stage. One unnamed value for every stage, or a vector
#'   naming every stage identity.
#' @param mem Memory per stage, in Slurm's own notation. One unnamed value for
#'   every stage, or a vector naming every stage identity.
#' @param no_requeue Character vector of stage identities to submit with
#'   `--no-requeue`. Use `character(0)` for none.
#' @return The written paths, invisibly, in submission order: the job scripts,
#'   then the driver.
#' @seealso [tteplan_locate_and_load()], which every generated job script
#'   calls. `vignette("tte-workflow")` describes the stage scripts this
#'   function chains.
#' @family tte_plan
#' @examples
#' # The function reads the filesystem type with GNU `stat -f -c %T`, so this
#' # example runs on Linux only.
#' if (identical(Sys.info()[["sysname"]], "Linux")) {
#'   data_dir <- file.path(tempdir(), "slurm-example-data")
#'   repo <- file.path(tempdir(), "slurm-example-repo")
#'   out <- file.path(tempdir(), "slurm-example-out")
#'   dir.create(data_dir, showWarnings = FALSE)
#'   dir.create(repo, showWarnings = FALSE)
#'   file.create(file.path(data_dir, "tteplan.qs2"))
#'   file.create(file.path(repo, c("s0_init.R", "s1.R")))
#'
#'   plan <- TTEPlan$new(
#'     project_prefix = "example",
#'     skeleton_files = "skeleton_001.qs2",
#'     global_max_isoyearweek = "2023-52"
#'   )
#'   plan$spec_version <- "v001"
#'   plan$dir_tteplan_cp <- CandidatePath$new(data_dir, "dir_tteplan")
#'
#'   paths <- tteplan_export_slurm(
#'     plan,
#'     repo = repo,
#'     dir = out,
#'     stages = c("s0_init.R", "s1.R"),
#'     no_requeue = "s1"
#'   )
#'   basename(paths)
#' }
#' @export
tteplan_export_slurm <- function(
  plan,
  repo,
  dir = ".",
  stages = c("s0_init.R", "s1.R", "s2.R", "s3.R", "s4_export.R"),
  cpus = 6,
  mem = "85G",
  no_requeue = "s1"
) {
  if (!inherits(plan, "TTEPlan")) {
    stop("`plan` must be a TTEPlan.", call. = FALSE)
  }
  if (length(repo) != 1L || is.na(repo) || !dir.exists(repo)) {
    stop(
      "`repo` must be one existing directory. Got: ",
      paste(repo, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(dir) != 1L || is.na(dir)) {
    stop("`dir` must be one path.", call. = FALSE)
  }
  # Both reach generated text as absolute paths, and `dir` also names a
  # directory this function creates. Check them before anything is created.
  .slurm_assert_no_whitespace(repo, "repo")
  .slurm_assert_no_whitespace(dir, "dir")

  stages <- as.character(stages)
  if (length(stages) == 0L) {
    stop("`stages` must name at least one stage script.", call. = FALSE)
  }
  # The whole stage path reaches a comment line, so the check covers the path
  # and not the file name alone. `x\nsbatch --wrap=true #/s1.R` has a valid
  # basename and a valid identity, and it escapes that comment.
  .slurm_assert_no_whitespace(stages, "stages")
  stage_ids <- .slurm_stage_ids(stages)
  if (any(!nzchar(stage_ids))) {
    stop("Every stage script MUST have a file name.", call. = FALSE)
  }
  if (anyDuplicated(stage_ids) > 0L) {
    stop(
      "Two stages share one identity: ",
      paste(unique(stage_ids[duplicated(stage_ids)]), collapse = ", "),
      ". A stage identity is the file name without its extension, so ",
      "`a/s1.R` and `b/s1.R` collide.",
      call. = FALSE
    )
  }

  # A stage script file name reaches `Rscript` as an operand, so it obeys the
  # same rule as a job name. This checks the file name only. A leading `-` in
  # a DIRECTORY component of the path is handled by the absolute stage path the
  # job script embeds.
  bad_stage_files <- basename(stages)[
    !grepl(.SLURM_NAME_PATTERN, basename(stages))
  ]
  if (length(bad_stage_files) > 0L) {
    stop(
      "A stage script file name MUST match ",
      .SLURM_NAME_PATTERN,
      ": ",
      paste(bad_stage_files, collapse = ", "),
      ". A name that starts with `-` reads as a command-line option.",
      call. = FALSE
    )
  }

  # The check runs before the sanitizer. A newline is outside
  # `[A-Za-z0-9_.-]`, so sanitizing would turn it into `_` and hide the fault
  # from the caller. A loud error names the field instead.
  .slurm_assert_single_line(plan$project_prefix, "project_prefix")
  prefix <- .slurm_sanitize(as.character(plan$project_prefix)[[1L]])
  job_names <- paste0(prefix, "_", stage_ids)
  # Every generated file name derives from one of these two, so checking both
  # covers `<job>.sh`, `<job>_mem.log`, `<job>_%j.out`, `<job>_%j.err` and
  # `<prefix>_submit.sh`.
  driver_name <- paste0(prefix, "_", .SLURM_DRIVER_STAGE_ID)
  derived_names <- c(job_names, driver_name)
  bad_names <- derived_names[!grepl(.SLURM_NAME_PATTERN, derived_names)]
  if (length(bad_names) > 0L) {
    stop(
      "A derived name MUST match ",
      .SLURM_NAME_PATTERN,
      ", so it starts with a letter or a digit: ",
      paste(bad_names, collapse = ", "),
      ". A name that starts with `-` reads as a command-line option.",
      call. = FALSE
    )
  }

  # The driver is written last, so a stage of this identity would derive the
  # driver's own file name and lose its job script to it.
  if (.SLURM_DRIVER_STAGE_ID %in% stage_ids) {
    stop(
      "The stage identity `",
      .SLURM_DRIVER_STAGE_ID,
      "` is reserved for the driver script. It derives ",
      prefix,
      "_",
      .SLURM_DRIVER_STAGE_ID,
      ".sh, which is the driver's own file name, so one file would overwrite ",
      "the other. Rename that stage script.",
      call. = FALSE
    )
  }

  # The stage scripts come before the resource arguments, so a caller who
  # renames a stage reads about the missing script and not about a stale
  # `no_requeue` entry that names the old identity.
  repo_abs <- normalizePath(repo, winslash = "/", mustWork = TRUE)
  absent <- stages[!file.exists(file.path(repo_abs, stages))]
  if (length(absent) > 0L) {
    stop(
      "No stage script at: ",
      paste(file.path(repo_abs, absent), collapse = ", "),
      call. = FALSE
    )
  }

  no_requeue <- if (is.null(no_requeue)) {
    character(0)
  } else {
    as.character(
      no_requeue
    )
  }
  unknown_nq <- setdiff(no_requeue, stage_ids)
  if (length(unknown_nq) > 0L) {
    stop(
      "`no_requeue` names no stage of this chain: ",
      paste(unknown_nq, collapse = ", "),
      ". The stages are: ",
      paste(stage_ids, collapse = ", "),
      call. = FALSE
    )
  }

  cpus_by_stage <- .slurm_resource(
    cpus,
    stage_ids,
    "cpus",
    .SLURM_CPUS_PATTERN
  )
  mem_by_stage <- .slurm_resource(mem, stage_ids, "mem", .SLURM_MEM_PATTERN)

  # Resolved once, here, and embedded as a literal in every job script. A job
  # that resolved it again would resolve it on the compute node, which is the
  # host whose mount this check exists to distrust.
  dir_tteplan <- normalizePath(
    plan$dir_tteplan,
    winslash = "/",
    mustWork = TRUE
  )
  .slurm_assert_no_whitespace(dir_tteplan, "dir_tteplan")
  touch_path <- paste0(dir_tteplan, "/", FILENAME_TTEPLAN)
  fstype <- .slurm_fstype(dir_tteplan)

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir_abs <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  .slurm_assert_no_whitespace(dir_abs, "dir")
  .slurm_assert_no_whitespace(repo_abs, "repo")
  slurm_abs <- paste0(dir_abs, "/slurm")
  dir.create(slurm_abs, showWarnings = FALSE, recursive = TRUE)

  spec_version <- if (length(plan$spec_version) == 1L) {
    as.character(plan$spec_version)
  } else {
    "unset"
  }
  .slurm_assert_single_line(spec_version, "spec_version")
  n_ids <- .slurm_count_text(plan$expected_n_ids, "expected_n_ids")
  n_skeletons <- .slurm_count_text(
    plan$expected_skeleton_file_count,
    "expected_skeleton_file_count"
  )

  job_paths <- paste0(slurm_abs, "/", job_names, ".sh")
  for (i in seq_along(stages)) {
    writeLines(
      .slurm_job_script(
        job_name = job_names[[i]],
        stage = stages[[i]],
        stage_id = stage_ids[[i]],
        index = i,
        n_stages = length(stages),
        cpus = cpus_by_stage[[i]],
        mem = mem_by_stage[[i]],
        repo_abs = repo_abs,
        slurm_abs = slurm_abs,
        dir_tteplan = dir_tteplan,
        touch_path = touch_path,
        fstype = fstype,
        spec_version = spec_version,
        n_ids = n_ids,
        n_skeletons = n_skeletons
      ),
      job_paths[[i]]
    )
  }

  driver_path <- paste0(slurm_abs, "/", driver_name, ".sh")
  writeLines(
    .slurm_driver_script(
      job_names = job_names,
      stage_ids = stage_ids,
      no_requeue = no_requeue,
      prefix = prefix,
      spec_version = spec_version,
      slurm_abs = slurm_abs
    ),
    driver_path
  )

  out <- c(job_paths, driver_path)
  Sys.chmod(out, "0755")
  invisible(out)
}
