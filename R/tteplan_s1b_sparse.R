# =============================================================================
# The rare_condition_rule and the s1b step (#52)
# =============================================================================
# A study MAY declare a `rare_condition_rule` block:
#
#   rare_condition_rule:
#     min_n: 100
#     decided_by: ["01"]
#     candidates: [rd_no_osd_i63_everbefore, rd_no_osd_i21_everbefore]
#
# Each candidate is a computed confounder. s1b counts, in each driving
# enrollment and each arm, the persons who hold the condition at baseline. A
# condition whose smallest count is below `min_n` becomes an exclusion
# criterion of every enrollment. Every other candidate stays a confounder. The
# decision is made from baseline counts in the eligible population, before the
# comparator draw and without follow-up outcomes.
#
# The flow through s1:
#
#   s1a  derives the candidate columns once per skeleton, and writes the value
#        at the recruiting week onto every eligible, landmark-qualified tuple.
#   s1b  counts the holders per arm in the driving enrollments, and the master
#        freezes the decision on the plan with a hash of the rule.
#   s1c  drops the person-trials that hold an excluded condition, in the order
#        of `candidates`, before the comparator draw. It appends one attrition
#        step per excluded condition.
#
# Without the block, s1a adds no column, s1b does not run and s1c drops
# nothing.
# =============================================================================

# The two arms, in the order every count table lists them.
.TTE_SPARSE_ARMS <- c("intervention", "comparator")

#' Stop with the message for an enrollment id that YAML read as a number
#' @noRd
.tte_sparse_stop_unquoted <- function(value) {
  stop(
    "rare_condition_rule$decided_by holds the unquoted value ",
    paste(format(value), collapse = ", "),
    ". YAML reads an unquoted 01 as the integer 1, so the id is lost. Quote ",
    "every enrollment id, as in decided_by: [\"01\"].",
    call. = FALSE
  )
}

#' Read a YAML sequence of strings as a character vector
#'
#' @param x The parsed value.
#' @param on_bad A function of the offending value. It MUST stop.
#' @return A character vector.
#' @noRd
.tte_sparse_strings <- function(x, on_bad) {
  if (is.list(x)) {
    ok <- vapply(
      x,
      function(e) {
        return(is.character(e) && length(e) == 1L)
      },
      logical(1)
    )
    if (!all(ok)) {
      on_bad(unlist(x[!ok]))
    }
    x <- unlist(x)
  }
  if (!is.character(x)) {
    on_bad(x)
  }
  return(x)
}

#' The computed confounder variables of a parsed specification
#' @noRd
.tte_computed_confounder_vars <- function(spec) {
  out <- character(0)
  for (conf in spec[["confounders"]] %||% list()) {
    impl <- conf[["implementation"]]
    if (isTRUE(impl[["computed"]]) && !is.null(impl[["variable"]])) {
      out <- c(out, impl[["variable"]])
    }
  }
  return(out)
}

#' Validate and normalise the rare_condition_rule block of a parsed
#' specification
#'
#' `tteplan_read_spec()` calls it after it derives the variable name of each
#' computed confounder. It stops on a block that cannot run, and returns the
#' specification with `min_n` as an integer and the two sequences as character
#' vectors.
#'
#' @param spec The parsed specification.
#' @return `spec`.
#' @noRd
.tte_check_sparse_rule <- function(spec) {
  rule <- spec[["rare_condition_rule"]]
  # A key written with no value, or with an empty mapping, is a rule the
  # author started and did not finish. Only an absent key means no rule.
  if ("rare_condition_rule" %in% names(spec) && length(rule) == 0L) {
    stop(
      "rare_condition_rule is present but empty. Give it min_n, decided_by and ",
      "candidates, or delete the key.",
      call. = FALSE
    )
  }
  if (is.null(rule)) {
    return(spec)
  }
  if (!is.list(rule) || is.null(names(rule))) {
    stop(
      "rare_condition_rule MUST be a mapping with the keys min_n, decided_by and ",
      "candidates.",
      call. = FALSE
    )
  }

  decided_by <- rule[["decided_by"]]
  if (length(decided_by) == 0L) {
    stop(
      "rare_condition_rule has no decided_by. Name the enrollments whose counts ",
      "decide the rule, as in decided_by: [\"01\"]. Without it a small test ",
      "enrollment could decide the population of the whole study.",
      call. = FALSE
    )
  }
  decided_by <- .tte_sparse_strings(decided_by, .tte_sparse_stop_unquoted)
  if (anyNA(decided_by) || !all(nzchar(decided_by))) {
    stop("rare_condition_rule$decided_by holds an empty id.", call. = FALSE)
  }
  if (anyDuplicated(decided_by) > 0L) {
    stop(
      "rare_condition_rule$decided_by names an enrollment twice: ",
      paste(unique(decided_by[duplicated(decided_by)]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  ids <- vapply(
    spec[["enrollments"]] %||% list(),
    function(e) {
      return(as.character(e[["id"]]))
    },
    character(1)
  )
  unknown <- setdiff(decided_by, ids)
  if (length(unknown) > 0L) {
    stop(
      "rare_condition_rule$decided_by names an enrollment the spec does not declare: ",
      paste(unknown, collapse = ", "),
      ". The declared enrollment ids are: ",
      paste(ids, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  min_n <- rule[["min_n"]]
  .tte_check_sparse_min_n(min_n)

  candidates <- rule[["candidates"]]
  if (length(candidates) == 0L) {
    stop(
      "rare_condition_rule$candidates MUST name at least one computed confounder.",
      call. = FALSE
    )
  }
  candidates <- .tte_sparse_strings(candidates, function(value) {
    stop(
      "rare_condition_rule$candidates MUST hold variable names. It holds: ",
      paste(format(value), collapse = ", "),
      ".",
      call. = FALSE
    )
  })
  if (anyDuplicated(candidates) > 0L) {
    stop(
      "rare_condition_rule$candidates names a variable twice: ",
      paste(unique(candidates[duplicated(candidates)]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  computed <- .tte_computed_confounder_vars(spec)
  bad <- setdiff(candidates, computed)
  if (length(bad) > 0L) {
    stop(
      "rare_condition_rule$candidates names a variable that is not a computed ",
      "confounder of this spec: ",
      paste(bad, collapse = ", "),
      ". Each candidate MUST be the implementation$variable of a confounder ",
      "with computed: true. The computed confounders are: ",
      if (length(computed) == 0L) "none" else paste(computed, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  spec[["rare_condition_rule"]] <- list(
    min_n = as.integer(min_n),
    decided_by = decided_by,
    candidates = candidates
  )
  return(spec)
}

#' Stop unless min_n is one whole number from 1 to the largest R integer
#'
#' Each class of bad value gets its own message, and every message names
#' `min_n`. `as.integer()` would turn an infinite value or one above
#' `.Machine$integer.max` into `NA`, so both are refused here. The three range
#' clauses use `isTRUE()`, so each clause refuses only its own class and an NA
#' value reaches none of them.
#'
#' @param min_n The parsed `rare_condition_rule$min_n`.
#' @return `invisible(TRUE)`.
#' @noRd
.tte_check_sparse_min_n <- function(min_n) {
  refuse <- function(why) {
    stop(
      "rare_condition_rule$min_n MUST be one whole number from 1 to ",
      .Machine$integer.max,
      ". It ",
      why,
      ".",
      call. = FALSE
    )
  }
  if (length(min_n) != 1L || !is.numeric(min_n)) {
    refuse("is not one number")
  }
  if (is.na(min_n)) {
    refuse(paste0(
      "is NA. YAML reads an integer above ",
      .Machine$integer.max,
      " as NA"
    ))
  }
  if (is.infinite(min_n)) {
    refuse("is infinite")
  }
  if (isTRUE(min_n != round(min_n))) {
    refuse(paste0("is ", min_n, ", which is not a whole number"))
  }
  if (isTRUE(min_n < 1)) {
    refuse(paste0("is ", min_n, ", which is below 1"))
  }
  if (isTRUE(min_n > .Machine$integer.max)) {
    refuse(paste0(
      "is ",
      format(min_n, scientific = FALSE),
      ", which is above the largest R integer"
    ))
  }
  return(invisible(TRUE))
}

#' Hash a rare_condition_rule over its three fields
#'
#' @param rule The normalised `spec$rare_condition_rule`, or `NULL`.
#' @return A string, or `NULL` when `rule` is `NULL`.
#' @noRd
.tte_sparse_rule_hash <- function(rule) {
  if (is.null(rule)) {
    return(NULL)
  }
  return(digest::digest(
    list(
      min_n = as.integer(rule[["min_n"]]),
      decided_by = as.character(rule[["decided_by"]]),
      candidates = as.character(rule[["candidates"]])
    ),
    algo = "sha256"
  ))
}

#' Stop when the spec's rare_condition_rule differs from the rule of the
#' decision
#'
#' The plan stores the hash of the rule that s1b decided under. s1c, s2 and s3
#' call this before any work, so a decision never applies to a rule it was
#' not made for.
#'
#' @param plan A TTEPlan.
#' @param where The stage, for the message.
#' @return `invisible(TRUE)`.
#' @noRd
.tte_assert_sparse_decision_current <- function(plan, where) {
  now <- .tte_sparse_rule_hash(plan$spec[["rare_condition_rule"]])
  if (identical(now, plan$sparse_rule_hash)) {
    return(invisible(TRUE))
  }
  stop(
    where,
    ": the rare_condition_rule in plan$spec does not match the rule of the frozen ",
    "decision. The hash covers min_n, decided_by and candidates. Re-run s1: ",
    "its step s1b decides again from the current rule.",
    call. = FALSE
  )
}

#' The part of an s1 run id that names the decision of the run
#'
#' A digest of the frozen decision table and of the rule hash. Two decisions
#' that differ give two different digests.
#'
#' @param plan A TTEPlan.
#' @return A sha256 string.
#' @noRd
.tte_s1_run_basis <- function(plan) {
  return(digest::digest(
    list(
      decision = as.list(plan$sparse_decision),
      rule = plan$sparse_rule_hash %||% "no sparse_rule"
    ),
    algo = "sha256"
  ))
}

# The count of s1 run ids that this R process made.
.tte_s1_runs <- new.env(parent = emptyenv())

#' A new s1 run id for the current decision of the plan
#'
#' The id is `<basis>:<token>`, where `<basis>` is `.tte_s1_run_basis()`. The
#' token holds the UTC time to the microsecond, the process id and a count of
#' the ids this process made. So no two s1 runs share an id. The token reads
#' no random number, so the s1 run id does not change the random seed.
#'
#' @param plan A TTEPlan.
#' @return A string.
#' @noRd
.tte_s1_run_new_id <- function(plan) {
  .tte_s1_runs$n <- (.tte_s1_runs$n %||% 0L) + 1L
  token <- paste(
    format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"),
    Sys.getpid(),
    .tte_s1_runs$n,
    sep = "."
  )
  return(paste0(.tte_s1_run_basis(plan), ":", token))
}

#' `TRUE` when s1 completed for the current decision of the plan
#'
#' `plan$s1_run_id` MUST be one string, and its basis MUST be that of the
#' current decision and rule hash.
#'
#' @param plan A TTEPlan.
#' @return `TRUE` or `FALSE`.
#' @noRd
.tte_s1_run_current <- function(plan) {
  id <- plan$s1_run_id
  return(
    is.character(id) &&
      length(id) == 1L &&
      !is.na(id) &&
      startsWith(id, paste0(.tte_s1_run_basis(plan), ":"))
  )
}

#' Stop unless s1 completed for the plan's current sparse decision
#'
#' s1 sets the decision in s1b, and s1c, s1d and s1e can still fail after it.
#' s1 clears `plan$s1_run_id` first and sets it last. s2 and s3 call this, so
#' they never read enrollment files of an earlier decision.
#'
#' @param plan A TTEPlan.
#' @param where The stage, for the message.
#' @return `invisible(TRUE)`.
#' @noRd
.tte_assert_s1_completed <- function(plan, where) {
  if (.tte_s1_run_current(plan)) {
    return(invisible(TRUE))
  }
  stop(
    where,
    ": s1 did not complete for the current sparse decision of the plan. ",
    "Run $s1_generate_enrollments_and_ipw() to its end, then run this stage.",
    call. = FALSE
  )
}

#' Set the s2 stamp of every ETT of the plan
#'
#' The stamp is two columns of `plan$ett`. `s1_run_id` names the s1 run whose
#' enrollment files s2 read for the analysis files of that ETT. `s2_run_id`
#' names the s2 run that wrote them, so it changes on every s2 run, also under
#' the same s1 run with other IPCW options. s2 sets both to `NA` before it
#' writes, and to the two run ids after every file is written.
#'
#' @param plan A TTEPlan.
#' @param id The s1 run id, or `NA_character_`.
#' @param s2_id The s2 run id, or `NA_character_`.
#' @return `invisible(NULL)`.
#' @noRd
.tte_s2_stamp <- function(plan, id, s2_id) {
  ett <- data.table::copy(plan$ett)
  data.table::set(
    ett,
    j = "s1_run_id",
    value = rep(as.character(id), nrow(ett))
  )
  data.table::set(
    ett,
    j = "s2_run_id",
    value = rep(as.character(s2_id), nrow(ett))
  )
  plan$ett <- ett
  return(invisible(NULL))
}

#' A new s2 run id
#'
#' The id is the s1 run id, then `:s2:`, then a token unique to this R
#' session, as `.tte_s1_run_new_id()` makes it.
#'
#' @param plan A TTEPlan whose s1 completed.
#' @return A string.
#' @noRd
.tte_s2_run_new_id <- function(plan) {
  .tte_s1_runs$n <- (.tte_s1_runs$n %||% 0L) + 1L
  token <- paste(
    format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"),
    Sys.getpid(),
    .tte_s1_runs$n,
    sep = "."
  )
  return(paste0(plan$s1_run_id, ":s2:", token))
}

#' The s2 run id of each row of an ETT table
#'
#' @param ett Rows of `plan$ett`, or `NULL` for a plan without ETTs.
#' @return A character vector with one element per row. It is `NA` for a row
#'   without the stamp, and for every row of a table without the column.
#' @noRd
.tte_s2_run_of <- function(ett) {
  if (is.null(ett)) {
    return(character(0))
  }
  id <- ett[["s2_run_id"]]
  if (is.null(id)) {
    return(rep(NA_character_, nrow(ett)))
  }
  return(as.character(id))
}

#' Stop when a stored s3 result comes from an earlier s2 run
#'
#' s3 copies the s2 run id of each ETT into the results it stores. s2 can run
#' again under the same s1 run, with other IPCW options. A result whose
#' `s2_run_id` differs from the current stamp of its ETT was computed on
#' analysis files that a later s2 run replaced. An enrollment result MUST carry the
#' current stamp of one of its ETTs, because s3 reads one of their analysis
#' files for it.
#'
#' @param plan A TTEPlan.
#' @param where The writer, for the message.
#' @return `invisible(TRUE)`.
#' @noRd
.tte_assert_one_s2_run <- function(plan, where) {
  ett <- plan$ett
  current <- .tte_s2_run_of(ett)
  ok_of <- function(results, of_key) {
    keys <- names(results) %||% as.character(seq_along(results))
    ok <- vapply(
      seq_along(results),
      function(i) {
        s <- results[[i]][["s2_run_id"]]
        if (!is.character(s) || length(s) != 1L || is.na(s)) {
          return(FALSE)
        }
        return(s %in% current[of_key(keys[i])])
      },
      logical(1)
    )
    return(keys[!ok])
  }
  stale <- c(
    ok_of(plan$results_enrollment, function(k) ett$enrollment_id == k),
    ok_of(plan$results_ett, function(k) ett$ett_id == k)
  )
  if (length(stale) == 0L) {
    return(invisible(TRUE))
  }
  stop(
    where,
    ": ",
    length(stale),
    " stored s3 result(s) do not come from the current s2 run of their ETT: ",
    paste(utils::head(stale, 5L), collapse = ", "),
    if (length(stale) > 5L) ", ...",
    ". s2 ran after s3, and its options can differ. Run $s3_analyze() ",
    "after the last $s2_generate_analysis_files_and_ipcw_pp().",
    call. = FALSE
  )
}

#' Stop unless s2 wrote the analysis files of each ETT after the current s1
#'
#' s3 calls this after `.tte_assert_s1_completed()`. An ETT whose s2 stamp is
#' absent, or names an earlier s1 run, has analysis files that the current s1
#' run did not feed.
#'
#' @param plan A TTEPlan.
#' @param where The stage, for the message.
#' @param ett_ids The ETTs whose analysis files the stage reads.
#' @return `invisible(TRUE)`.
#' @noRd
.tte_assert_s2_current <- function(plan, where, ett_ids) {
  ett <- plan$ett
  stamp <- ett[["s1_run_id"]] %||% rep(NA_character_, nrow(ett))
  stale <- ett$ett_id[ett$ett_id %in% ett_ids & !stamp %in% plan$s1_run_id]
  if (length(stale) == 0L) {
    return(invisible(TRUE))
  }
  stop(
    where,
    ": s2 did not write the analysis files of ",
    length(stale),
    " ETT(s) after the current s1 run: ",
    paste(utils::head(stale, 5L), collapse = ", "),
    if (length(stale) > 5L) ", ...",
    ". Run $s2_generate_analysis_files_and_ipcw_pp(), then run this stage.",
    call. = FALSE
  )
}

#' Stop when a writer would mix the outputs of two s1 runs
#'
#' The writers read the sparse decision and the stored s3 results. When the
#' plan holds a decision or a stored result, the rule in `plan$spec` MUST hash
#' to `plan$sparse_rule_hash`. The plan MUST NOT hold a decision while s1 is
#' not complete. Each stored s3 result
#' MUST carry the s1 run id of the plan, and that run MUST be complete. A plan
#' with no decision and no stored result passes, as after s0.
#'
#' @param plan A TTEPlan.
#' @param where The writer, for the message.
#' @return `invisible(TRUE)`.
#' @noRd
.tte_assert_one_s1_run <- function(plan, where) {
  # A decision or a stored result belongs to the rule it was made under. The
  # rule in plan$spec MAY change after s1, and the writers would then state
  # the new rule beside the old counts, exclusions and adjustment set.
  has_results <- length(plan$results_enrollment) > 0L ||
    length(plan$results_ett) > 0L
  if (!is.null(plan$sparse_decision) || has_results) {
    .tte_assert_sparse_decision_current(plan, where)
  }
  current <- .tte_s1_run_current(plan)
  if (!is.null(plan$sparse_decision) && !current) {
    stop(
      where,
      ": the plan holds a sparse decision, but s1 did not complete for it. ",
      "Run $s1_generate_enrollments_and_ipw() to its end, then s2 and s3.",
      call. = FALSE
    )
  }
  stale_of <- function(results) {
    ok <- vapply(
      results,
      function(r) {
        return(current && identical(r[["s1_run_id"]], plan$s1_run_id))
      },
      logical(1)
    )
    return((names(results) %||% as.character(seq_along(results)))[!ok])
  }
  stale <- c(stale_of(plan$results_enrollment), stale_of(plan$results_ett))
  if (length(stale) == 0L) {
    return(invisible(TRUE))
  }
  stop(
    where,
    ": ",
    length(stale),
    " stored s3 result(s) do not come from the current s1 run: ",
    paste(utils::head(stale, 5L), collapse = ", "),
    if (length(stale) > 5L) ", ...",
    ". Run s2 and s3 after a completed $s1_generate_enrollments_and_ipw().",
    call. = FALSE
  )
}

#' The confounder entry of each candidate, in the order of `candidates`
#' @noRd
.tte_sparse_condition_entries <- function(spec) {
  vars <- spec[["rare_condition_rule"]][["candidates"]]
  if (length(vars) == 0L) {
    return(list())
  }
  out <- lapply(vars, function(v) {
    for (conf in spec[["confounders"]] %||% list()) {
      impl <- conf[["implementation"]]
      if (isTRUE(impl[["computed"]]) && identical(impl[["variable"]], v)) {
        return(conf)
      }
    }
    stop(
      "rare_condition_rule candidate '",
      v,
      "' is not a computed confounder of the spec.",
      call. = FALSE
    )
  })
  names(out) <- vars
  return(out)
}

#' Derive the candidate columns on a skeleton, once per skeleton
#'
#' s1a calls it on the projected canonical skeleton, before its loop over the
#' enrollments. A computed confounder is a rolling window over one person's
#' weeks, so its value does not depend on the enrollment.
#'
#' @param skeleton The canonical skeleton, keyed by `id` and `isoyearweek`.
#' @param spec The parsed specification.
#' @return The skeleton. The caller MUST assign it.
#' @noRd
.s1a_derive_sparse_conditions <- function(skeleton, spec) {
  entries <- .tte_sparse_condition_entries(spec)
  if (length(entries) == 0L) {
    return(skeleton)
  }
  skeleton <- .grow_dt_alloc(skeleton, 2L * length(entries))
  built <- .tte_build_confounder_specs(
    skeleton,
    list(confounders = unname(entries))
  )
  return(.tte_apply_eligibility_batch(
    built$skeleton,
    built$grouped_specs,
    id_col = "id"
  ))
}

#' Write each candidate's value at the recruiting week onto the tuples
#'
#' The recruiting week is the instant the entry-window snapshot reads every
#' confounder at (`.tte_entry_snapshot()`). The lookup matches the week string,
#' as that function does. A tuple with no row at its recruiting week reads
#' `NA`, and an `NA` never counts as holding the condition.
#'
#' @param tuples The eligible, landmark-qualified tuples of one enrollment.
#' @param skeleton The skeleton the tuples came from.
#' @param pid The person id column.
#' @param vars The candidate variables. Empty without a rare_condition_rule.
#' @return The tuples. The caller MUST assign them.
#' @noRd
.s1a_attach_sparse_baseline <- function(tuples, skeleton, pid, vars) {
  if (length(vars) == 0L) {
    return(tuples)
  }
  want <- data.table::data.table(
    .tte_pid = tuples[[pid]],
    .tte_week = cstime::dates_by_isoyearweek$isoyearweek[
      as.integer(tuples[["recruit_week_index"]]) + 1L
    ]
  )
  row_of <- skeleton[
    want,
    on = stats::setNames(c(".tte_pid", ".tte_week"), c(pid, "isoyearweek")),
    which = TRUE,
    mult = "first"
  ]
  tuples <- .grow_dt_alloc(tuples, length(vars))
  for (v in vars) {
    data.table::set(tuples, j = v, value = skeleton[[v]][row_of])
  }
  return(tuples)
}

#' s1b worker: count the holders of each candidate per arm
#'
#' Reads the s1a pre files of the driving enrollments. A person holds a
#' condition in an arm when its column is `FALSE` on any of her tuples in that
#' arm. The same person can count in both arms. The worker reads no outcome.
#'
#' Dispatched by `$s1_generate_enrollments_and_ipw()` through
#' `.batch_run_and_write(style = "return")`, one item, one subworker.
#'
#' @param enrollment_specs The enrollment specs of the driving enrollments.
#' @param work_dir The s1 work directory, an input.
#' @param skel_basenames The skeleton basenames, which name the pre files.
#' @param condition_vars The candidate variables, in the order of the spec.
#' @return `list(counts = )`: one row per driving enrollment, candidate and
#'   arm, with `n_persons`. A cell with no holder reads 0.
#' @noRd
.s1b_worker <- function(
  enrollment_specs,
  work_dir,
  skel_basenames,
  condition_vars
) {
  person <- n_persons <- NULL # nolint
  eids <- vapply(
    enrollment_specs,
    function(es) {
      return(es$enrollment_id)
    },
    character(1)
  )
  chunks <- list()
  for (es in enrollment_specs) {
    eid <- es$enrollment_id
    pid <- es$design$person_id_var
    data.table::setDTthreads(es$n_threads %||% 1L)
    pre_paths <- vapply(
      skel_basenames,
      function(bn) {
        return(.s1a_pre_path(work_dir, eid, bn))
      },
      character(1)
    )
    missing_pre <- !file.exists(pre_paths)
    if (any(missing_pre)) {
      stop(
        sprintf(
          "s1b: %d/%d pre files missing for driving enrollment '%s'. First missing: %s",
          sum(missing_pre),
          length(pre_paths),
          eid,
          pre_paths[which(missing_pre)[1L]]
        ),
        call. = FALSE
      )
    }
    for (path in pre_paths) {
      tp <- qs2_read(path, nthreads = 1L)$tuples
      absent <- setdiff(condition_vars, names(tp))
      if (length(absent) > 0L) {
        stop(
          "s1b: the s1a tuples of enrollment '",
          eid,
          "' carry no baseline column for: ",
          paste(absent, collapse = ", "),
          ". s1a writes them only when the spec has a rare_condition_rule.",
          call. = FALSE
        )
      }
      arm <- ifelse(tp[["intervention"]], "intervention", "comparator")
      for (v in condition_vars) {
        x <- tp[[v]]
        held <- !is.na(x) & !x
        if (!any(held)) {
          next
        }
        chunks[[length(chunks) + 1L]] <- unique(data.table::data.table(
          enrollment_id = eid,
          variable = v,
          arm = arm[held],
          person = tp[[pid]][held]
        ))
      }
      rm(tp)
    }
  }
  grid <- data.table::CJ(
    enrollment_id = eids,
    variable = condition_vars,
    arm = .TTE_SPARSE_ARMS,
    sorted = FALSE
  )
  if (length(chunks) == 0L) {
    grid[, n_persons := 0L]
    return(list(counts = grid[]))
  }
  holders <- data.table::rbindlist(chunks, use.names = TRUE)
  counts <- holders[,
    list(n_persons = data.table::uniqueN(person)),
    by = c("enrollment_id", "variable", "arm")
  ]
  out <- merge(
    grid,
    counts,
    by = c("enrollment_id", "variable", "arm"),
    all.x = TRUE,
    sort = FALSE
  )
  out[is.na(n_persons), n_persons := 0L]
  return(list(counts = out[]))
}

#' Turn the s1b counts into the frozen decision table
#'
#' @param counts The `counts` table of `.s1b_worker()`.
#' @param rule The normalised `spec$rare_condition_rule`.
#' @param labels The confounder name of each candidate, named by variable.
#' @param decided_at When the decision step ran.
#' @return One row per candidate, in the order of `candidates`. The columns are
#'   `variable`, `label`, one `n_<enrollment>_<arm>` count per driving
#'   enrollment and arm, `smallest`, `smallest_enrollment`, `smallest_arm`,
#'   `threshold`, `decided_by`, `decision` and `decided_at`. `decision` is
#'   `"excluded"` when `smallest` is below `threshold`, and `"adjusted"`
#'   otherwise.
#' @noRd
.s1b_decide <- function(counts, rule, labels, decided_at = Sys.time()) {
  min_n <- as.integer(rule[["min_n"]])
  rows <- lapply(rule[["candidates"]], function(v) {
    row <- list(variable = v, label = unname(labels[[v]]))
    best <- NA_integer_
    best_enrollment <- NA_character_
    best_arm <- NA_character_
    for (eid in rule[["decided_by"]]) {
      for (arm in .TTE_SPARSE_ARMS) {
        n <- counts$n_persons[
          counts$enrollment_id == eid &
            counts$variable == v &
            counts$arm == arm
        ]
        if (length(n) != 1L) {
          stop(
            "s1b: no count for candidate '",
            v,
            "' in enrollment '",
            eid,
            "', arm ",
            arm,
            ".",
            call. = FALSE
          )
        }
        row[[sprintf("n_%s_%s", eid, arm)]] <- as.integer(n)
        if (is.na(best) || n < best) {
          best <- as.integer(n)
          best_enrollment <- eid
          best_arm <- arm
        }
      }
    }
    return(c(
      row,
      list(
        smallest = best,
        smallest_enrollment = best_enrollment,
        smallest_arm = best_arm,
        threshold = min_n,
        decided_by = paste(rule[["decided_by"]], collapse = ", "),
        decision = if (best < min_n) "excluded" else "adjusted"
      )
    ))
  })
  out <- data.table::rbindlist(rows, use.names = TRUE)
  data.table::set(out, j = "decided_at", value = rep(decided_at, nrow(out)))
  return(out[])
}

#' Run s1b: dispatch the counting worker, then build the decision
#'
#' @param spec The plan's parsed specification.
#' @param all_es The enrollment specs of every enrollment.
#' @param enrollment_ids The enrollment ids, in the order of `all_es`.
#' @param work_dir The s1 work directory.
#' @param skel_basenames The skeleton basenames.
#' @param dev_path The swereg development path for the subworker, or `NULL`.
#' @param p The progressor.
#' @return The decision table of `.s1b_decide()`.
#' @noRd
.s1b_run <- function(
  spec,
  all_es,
  enrollment_ids,
  work_dir,
  skel_basenames,
  dev_path,
  p
) {
  rule <- .tte_check_sparse_rule(spec)[["rare_condition_rule"]]
  absent <- setdiff(rule[["decided_by"]], enrollment_ids)
  if (length(absent) > 0L) {
    stop(
      "s1b: rare_condition_rule$decided_by names an enrollment the plan does not ",
      "run: ",
      paste(absent, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  items <- list(
    s1b_decide = list(
      enrollment_specs = all_es[match(rule[["decided_by"]], enrollment_ids)],
      work_dir = work_dir,
      skel_basenames = skel_basenames,
      condition_vars = rule[["candidates"]]
    )
  )
  outputs <- list(s1b_decide = c(counts = .s1b_counts_path(work_dir)))
  .batch_run_and_write(
    target = .batch_target("swereg", ".s1b_worker"),
    items = items,
    outputs = outputs,
    style = "return",
    n_workers = 1L,
    dev_path = dev_path,
    p = p,
    label = "s1b"
  )
  counts <- qs2_read(.s1b_counts_path(work_dir), nthreads = 1L)
  labels <- vapply(
    .tte_sparse_condition_entries(spec),
    function(conf) {
      return(as.character(conf[["name"]] %||% conf$implementation$variable))
    },
    character(1)
  )
  return(.s1b_decide(counts, rule, labels))
}

#' Remove the excluded conditions from every ETT's confounder set
#'
#' A condition the previous decision excluded comes back first, in the order
#' of `spec$confounders`. A condition the new decision excludes then leaves.
#' The function changes nothing when neither decision excludes a condition.
#'
#' @param plan A TTEPlan.
#' @param previous The decision table of the previous s1 run, or `NULL`.
#' @param current The decision table of this run, or `NULL`.
#' @return `TRUE` when it changed `plan$ett`, invisibly.
#' @noRd
.s1b_apply_to_confounders <- function(plan, previous, current) {
  excluded_of <- function(d) {
    if (is.null(d)) {
      return(character(0))
    }
    return(d$variable[d$decision == "excluded"])
  }
  back <- excluded_of(previous)
  out <- excluded_of(current)
  if (length(back) == 0L && length(out) == 0L) {
    return(invisible(FALSE))
  }
  spec_vars <- vapply(
    plan$spec[["confounders"]] %||% list(),
    function(conf) {
      return(conf$implementation$variable %||% NA_character_)
    },
    character(1)
  )
  ett <- data.table::copy(plan$ett)
  cv <- lapply(ett$confounder_vars, function(x) {
    restore <- setdiff(back, x)
    if (length(restore) > 0L) {
      x <- c(x, restore)
      x <- x[order(match(x, spec_vars))]
    }
    return(x[!x %in% out])
  })
  data.table::set(ett, j = "confounder_vars", value = list(cv))
  plan$ett <- ett
  return(invisible(TRUE))
}

#' The step label of one excluded condition
#' @noRd
.tte_sparse_criterion <- function(min_n, label) {
  return(sprintf("Excluded (fewer than %s at baseline): %s", min_n, label))
}

#' The excluded conditions of the frozen decision, in the order of the spec
#'
#' @param plan A TTEPlan.
#' @return A data.table with `variable` and `criterion`, or `NULL` when the
#'   decision excludes nothing or the plan holds none.
#' @noRd
.tte_sparse_exclusions <- function(plan) {
  d <- plan$sparse_decision
  if (is.null(d)) {
    return(NULL)
  }
  ex <- d[d$decision == "excluded"]
  if (nrow(ex) == 0L) {
    return(NULL)
  }
  return(data.table::data.table(
    variable = ex$variable,
    criterion = .tte_sparse_criterion(ex$threshold, ex$label)
  ))
}

#' The attrition rows of one step-2 exclusion
#'
#' Same columns and units as the s1a cascade: one row per enrollment period,
#' and one global row with `enrollment_period_id = NA` that carries the true
#' count of persons.
#' @noRd
.s1c_sparse_step_rows <- function(tuples, pid, criterion) {
  intervention <- NULL # nolint
  one <- function(tp) {
    return(list(
      n_persons = data.table::uniqueN(tp[[pid]]),
      n_person_trials = nrow(tp),
      n_intervention = sum(tp$intervention == TRUE),
      n_comparator = sum(tp$intervention == FALSE)
    ))
  }
  per_trial <- tuples[, one(.SD), by = "enrollment_period_id"]
  global <- data.table::as.data.table(c(
    list(enrollment_period_id = NA_integer_),
    one(tuples)
  ))
  out <- data.table::rbindlist(list(per_trial, global), use.names = TRUE)
  data.table::set(out, j = "criterion", value = criterion)
  data.table::setcolorder(out, c("enrollment_period_id", "criterion"))
  return(out[])
}

#' Drop the person-trials that hold an excluded condition, in spec order
#'
#' A person-trial holds a condition when its column reads `FALSE` at the
#' recruiting week. The counts after each step are cumulative, as the s1a
#' cascade's are.
#'
#' @param tuples All tuples of one enrollment, before the comparator draw.
#' @param exclusions The `.tte_sparse_exclusions()` table.
#' @param pid The person id column.
#' @return `list(tuples = , attrition = )`.
#' @noRd
.s1c_drop_sparse_conditions <- function(tuples, exclusions, pid) {
  keep <- rep(TRUE, nrow(tuples))
  rows <- vector("list", nrow(exclusions))
  for (k in seq_len(nrow(exclusions))) {
    v <- exclusions$variable[k]
    if (!v %in% names(tuples)) {
      stop(
        "s1c: the s1a tuples carry no baseline column '",
        v,
        "'. s1a writes it only when the spec has a rare_condition_rule.",
        call. = FALSE
      )
    }
    x <- tuples[[v]]
    keep <- keep & !(!is.na(x) & !x)
    rows[[k]] <- .s1c_sparse_step_rows(
      tuples[keep],
      pid,
      exclusions$criterion[k]
    )
  }
  return(list(
    tuples = tuples[keep],
    attrition = data.table::rbindlist(rows, use.names = TRUE)
  ))
}
