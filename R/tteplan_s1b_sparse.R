# =============================================================================
# The sparse_rule and the s1b step (#52)
# =============================================================================
# A study MAY declare a `sparse_rule` block:
#
#   sparse_rule:
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
    "sparse_rule$decided_by holds the unquoted value ",
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

#' Validate and normalise the sparse_rule block of a parsed specification
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
  rule <- spec[["sparse_rule"]]
  # A key written with no value, or with an empty mapping, is a rule the
  # author started and did not finish. Only an absent key means no rule.
  if ("sparse_rule" %in% names(spec) && length(rule) == 0L) {
    stop(
      "sparse_rule is present but empty. Give it min_n, decided_by and ",
      "candidates, or delete the key.",
      call. = FALSE
    )
  }
  if (is.null(rule)) {
    return(spec)
  }
  if (!is.list(rule) || is.null(names(rule))) {
    stop(
      "sparse_rule MUST be a mapping with the keys min_n, decided_by and ",
      "candidates.",
      call. = FALSE
    )
  }

  decided_by <- rule[["decided_by"]]
  if (length(decided_by) == 0L) {
    stop(
      "sparse_rule has no decided_by. Name the enrollments whose counts ",
      "decide the rule, as in decided_by: [\"01\"]. Without it a small test ",
      "enrollment could decide the population of the whole study.",
      call. = FALSE
    )
  }
  decided_by <- .tte_sparse_strings(decided_by, .tte_sparse_stop_unquoted)
  if (anyNA(decided_by) || !all(nzchar(decided_by))) {
    stop("sparse_rule$decided_by holds an empty id.", call. = FALSE)
  }
  if (anyDuplicated(decided_by) > 0L) {
    stop(
      "sparse_rule$decided_by names an enrollment twice: ",
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
      "sparse_rule$decided_by names an enrollment the spec does not declare: ",
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
      "sparse_rule$candidates MUST name at least one computed confounder.",
      call. = FALSE
    )
  }
  candidates <- .tte_sparse_strings(candidates, function(value) {
    stop(
      "sparse_rule$candidates MUST hold variable names. It holds: ",
      paste(format(value), collapse = ", "),
      ".",
      call. = FALSE
    )
  })
  if (anyDuplicated(candidates) > 0L) {
    stop(
      "sparse_rule$candidates names a variable twice: ",
      paste(unique(candidates[duplicated(candidates)]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  computed <- .tte_computed_confounder_vars(spec)
  bad <- setdiff(candidates, computed)
  if (length(bad) > 0L) {
    stop(
      "sparse_rule$candidates names a variable that is not a computed ",
      "confounder of this spec: ",
      paste(bad, collapse = ", "),
      ". Each candidate MUST be the implementation$variable of a confounder ",
      "with computed: true. The computed confounders are: ",
      if (length(computed) == 0L) "none" else paste(computed, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  spec[["sparse_rule"]] <- list(
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
#' @param min_n The parsed `sparse_rule$min_n`.
#' @return `invisible(TRUE)`.
#' @noRd
.tte_check_sparse_min_n <- function(min_n) {
  refuse <- function(why) {
    stop(
      "sparse_rule$min_n MUST be one whole number from 1 to ",
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

#' Hash a sparse_rule over its three fields
#'
#' @param rule The normalised `spec$sparse_rule`, or `NULL`.
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

#' Stop when the spec's sparse_rule differs from the rule of the decision
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
  now <- .tte_sparse_rule_hash(plan$spec[["sparse_rule"]])
  if (identical(now, plan$sparse_rule_hash)) {
    return(invisible(TRUE))
  }
  stop(
    where,
    ": the sparse_rule in plan$spec does not match the rule of the frozen ",
    "decision. The hash covers min_n, decided_by and candidates. Re-run s1: ",
    "its step s1b decides again from the current rule.",
    call. = FALSE
  )
}

#' The confounder entry of each candidate, in the order of `candidates`
#' @noRd
.tte_sparse_condition_entries <- function(spec) {
  vars <- spec[["sparse_rule"]][["candidates"]]
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
      "sparse_rule candidate '",
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
#' @param vars The candidate variables. Empty without a sparse_rule.
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
          ". s1a writes them only when the spec has a sparse_rule.",
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
#' @param rule The normalised `spec$sparse_rule`.
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
  rule <- .tte_check_sparse_rule(spec)[["sparse_rule"]]
  absent <- setdiff(rule[["decided_by"]], enrollment_ids)
  if (length(absent) > 0L) {
    stop(
      "s1b: sparse_rule$decided_by names an enrollment the plan does not ",
      "run: ",
      paste(absent, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  items <- list(s1b_decide = list(
    enrollment_specs = all_es[match(rule[["decided_by"]], enrollment_ids)],
    work_dir = work_dir,
    skel_basenames = skel_basenames,
    condition_vars = rule[["candidates"]]
  ))
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
        "'. s1a writes it only when the spec has a sparse_rule.",
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
