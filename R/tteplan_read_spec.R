# =============================================================================
# Spec functions (called by TTEPlan methods and Loop 1 workers)
# =============================================================================

# =============================================================================
# tteplan_read_spec
# =============================================================================

#' Read and validate a YAML study specification
#'
#' Parses a YAML study specification file, converts human-readable windows
#' (e.g., "lifetime_before_baseline", "3 years") to weeks, and validates that all required
#' fields are present.
#'
#' @param spec_path Path to the YAML specification file.
#' @return A nested list representing the parsed specification, with
#'   `window_weeks` fields added to exclusion criteria and confounders.
#'
#' @details
#' Window conversion rules:
#' \itemize{
#'   \item `"lifetime_before_baseline"` -> `Inf`
#'   \item `"N year"` or `"N years"` -> `N * 52`
#' }
#'
#' Validation checks:
#' \itemize{
#'   \item Required sections: study, enrollments, outcomes, follow_up
#'   \item `study$implementation$project_prefix` must exist
#'   \item Each entry in `inclusion_criteria$criteria` must declare
#'     `type: "has_event"` and `implementation$source_variable`, and must
#'     generate an eligibility column name no earlier entry generates
#'   \item Each exclusion criterion must have `implementation$source_variable`
#'   \item Each outcome must have `implementation$variable`
#'   \item Each enrollment must have `id` and `treatment$implementation$variable`
#'   \item Computed confounders must have `implementation$source_variable`
#' }
#'
#' Warns about open questions with `status: "open"`.
#'
#' @section The observation contract:
#'
#' An enrollment states how the data records that a person was under
#' observation in a week. It is a flat key on the enrollment, `observed_var`,
#' and it takes exactly one of two forms.
#'
#' \preformatted{
#' observed_var: {column: rd_observed}      # a real logical person-week column
#' observed_var: {sentinel: row_presence}   # the skeleton is trimmed
#' }
#'
#' The `row_presence` sentinel asserts that the caller already deleted every
#' unobserved person-week. A row then exists if and only if the person was
#' observed that week. Use it when the skeleton already deletes every
#' person-week the person was not under observation. The production skeleton
#' is one example. It deletes every person-week up to and including first
#' immigration, every person-week on or after emigration, and every
#' person-week after death. It keeps the death week itself. A real `observed`
#' column there would hold `TRUE` on every retained row. It could not
#' represent an absent week. Row presence as a silent proxy stays forbidden.
#' The sentinel is what makes the assumption explicit and testable.
#'
#' Two flat sibling keys carry the arm tolerances:
#' `intervention_tolerance_weeks` and `comparator_tolerance_weeks`. Each MUST
#' be a whole number of at least 0. Each defaults to 0.
#'
#' Every enrollment MUST declare `observed_var`. There is no exemption for an
#' older spec. A spec that cannot say who was under observation carries the
#' immortal-time defect silently. It looks exactly like a spec that can.
#' To migrate a spec, copy it to a new version and add the key to every
#' enrollment. Never edit a released spec version: that version is the record
#' of what produced a run.
#'
#' The function rejects a declaration that gives both `column` and `sentinel`,
#' a declaration that gives neither, and a sentinel name swereg does not know.
#' It cannot check that a named column exists and is logical, because it reads
#' no data. [tteplan_validate_spec()] runs that check against the skeleton.
#'
#' @family tte_spec
#' @export
tteplan_read_spec <- function(spec_path) {
  if (!file.exists(spec_path)) {
    stop("Spec file not found: ", spec_path, call. = FALSE)
  }

  # Read the spec as raw bytes and decode as UTF-8 explicitly, independent of
  # the session locale. yaml::read_yaml() -> readLines() under a non-UTF-8
  # locale (e.g. LC_CTYPE=C in headless/cron runs) silently truncates the YAML
  # at the first non-ASCII byte: the v008 spec lost statin arms 16-18 (a
  # 15/18-enrollment grid) at an em-dash in a comment, with only a readLines
  # warning. readBin bypasses readLines; validUTF8() then fails loudly on a
  # genuinely non-UTF-8 file instead of silently mis-decoding it.
  fsize <- file.info(spec_path)$size
  if (is.na(fsize)) {
    stop(
      "Cannot determine the size of the spec file: ",
      spec_path,
      call. = FALSE
    )
  }
  spec_bytes <- readBin(spec_path, "raw", n = fsize)
  if (
    length(spec_bytes) >= 3L &&
      identical(spec_bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
  ) {
    spec_bytes <- spec_bytes[-(1:3)] # strip a UTF-8 BOM (some Windows editors add one)
  }
  spec_txt <- rawToChar(spec_bytes)
  if (!validUTF8(spec_txt)) {
    stop(
      "Spec file is not valid UTF-8 (re-save it as UTF-8): ",
      spec_path,
      call. = FALSE
    )
  }
  Encoding(spec_txt) <- "UTF-8"
  spec <- yaml::yaml.load(spec_txt)

  # The key gate. It reads the specification as written. swereg writes derived
  # keys back into the specification below (`window_weeks`,
  # `source_variable_combined`, `variable_combined`, and the two-field
  # `observed_var`), and the schema names none of them. The schema describes
  # the input, so the gate MUST run before any normalisation.
  .tte_spec_check_keys(spec, spec_path)

  # Validate required sections
  required_sections <- c(
    "study",
    "enrollments",
    "outcomes",
    "follow_up"
  )
  missing <- setdiff(required_sections, names(spec))
  if (length(missing) > 0) {
    stop(
      "Missing required sections in spec: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate study has implementation$project_prefix
  if (is.null(spec$study$implementation$project_prefix)) {
    stop("study$implementation$project_prefix is required", call. = FALSE)
  }

  # Normalize the global inclusion criteria. `inclusion_criteria` is a fixed
  # container, so a strict-key validator can name every legal path inside it.
  spec <- .tte_normalize_global_inclusion(spec)

  # Validate and convert exclusion_criteria
  if (!is.null(spec$exclusion_criteria)) {
    for (i in seq_along(spec$exclusion_criteria)) {
      ec <- spec$exclusion_criteria[[i]]

      if (is.null(ec$implementation$source_variable)) {
        stop(
          "exclusion_criteria[",
          i,
          "] '",
          ec$name,
          "' is missing implementation$source_variable",
          call. = FALSE
        )
      }

      # Normalize source_variable (may be a YAML list for multi-source)
      spec$exclusion_criteria[[i]]$implementation <-
        .normalize_source_variable(spec$exclusion_criteria[[i]]$implementation)

      if (
        identical(
          ec$implementation$window,
          "lifetime_before_and_after_baseline"
        )
      ) {
        # Person-level: no window_weeks conversion needed
      } else {
        if (is.null(ec$implementation$window)) {
          stop(
            "exclusion_criteria[",
            i,
            "] '",
            ec$name,
            "' is missing implementation$window",
            call. = FALSE
          )
        }
        spec$exclusion_criteria[[i]]$implementation$window_weeks <-
          .convert_window(ec$implementation$window)
      }
    }
  }

  # Validate and normalize outcomes
  for (i in seq_along(spec$outcomes)) {
    if (is.null(spec$outcomes[[i]]$implementation$variable)) {
      stop(
        "outcomes[",
        i,
        "] '",
        spec$outcomes[[i]]$name,
        "' is missing implementation$variable",
        call. = FALSE
      )
    }
    # Normalize variable (may be a YAML list for multi-source outcomes)
    v <- spec$outcomes[[i]]$implementation$variable
    if (is.list(v)) {
      v <- unlist(v)
    }
    spec$outcomes[[i]]$implementation$variable <- as.character(v)
    spec$outcomes[[i]]$implementation$variable_combined <-
      paste(spec$outcomes[[i]]$implementation$variable, collapse = "__")
  }

  # The eligibility column names the global inclusion criteria generate. A
  # per-enrollment `has_event` entry that generates one of these collides with
  # it: both write the same column, and the enrollment's eligibility cascade
  # then lists that column twice.
  global_inclusion_cols <- .tte_global_inclusion_col_names(spec)

  # Validate enrollments
  for (i in seq_along(spec$enrollments)) {
    enr <- spec$enrollments[[i]]
    if (is.null(enr$id)) {
      stop("enrollments[", i, "] is missing 'id'", call. = FALSE)
    }

    # The observation contract. Every enrollment MUST state how observation is
    # encoded. There is no exemption for an older spec. A spec that cannot say
    # who was under observation carries the immortal-time defect silently. It
    # looks exactly like one that can.
    if (is.null(enr$observed_var)) {
      stop(
        "enrollments[",
        i,
        "] '",
        enr$name %||% enr$id,
        "' is missing 'observed_var'. Every enrollment MUST state how ",
        "observation is encoded: `observed_var: {column: <name>}` for a real ",
        "logical column, or `observed_var: {sentinel: row_presence}` for a ",
        "trimmed skeleton. Copy the spec to a new version and add the key ",
        "to every enrollment. Never edit a released spec version.",
        call. = FALSE
      )
    }
    spec$enrollments[[i]]$observed_var <- .tte_observed_var(
      enr$observed_var,
      paste0("enrollments[", i, "]$observed_var")
    )
    spec$enrollments[[i]]$intervention_tolerance_weeks <- .tte_tolerance_weeks(
      enr$intervention_tolerance_weeks,
      paste0("enrollments[", i, "]$intervention_tolerance_weeks")
    )
    spec$enrollments[[i]]$comparator_tolerance_weeks <- .tte_tolerance_weeks(
      enr$comparator_tolerance_weeks,
      paste0("enrollments[", i, "]$comparator_tolerance_weeks")
    )

    if (is.null(enr$treatment$implementation$variable)) {
      stop(
        "enrollments[",
        i,
        "] '",
        enr$name %||% enr$id,
        "' is missing treatment$implementation$variable",
        call. = FALSE
      )
    }
    # The key gate above refuses the retired `matching_ratio` key. The schema
    # carries that rule at
    # `$/enrollments[]/treatment/implementation/matching_ratio`.
    # `[[` is exact; `$` would partial-match a longer key.
    tx_impl <- enr$treatment$implementation
    if (is.null(tx_impl[["comparator_to_intervention_ratio"]])) {
      stop(
        "enrollments[",
        i,
        "] '",
        enr$name %||% enr$id,
        "' is missing treatment$implementation$comparator_to_intervention_ratio",
        call. = FALSE
      )
    }

    # Validate and convert additional_exclusion entries
    if (!is.null(enr$additional_exclusion)) {
      for (j in seq_along(enr$additional_exclusion)) {
        ae <- enr$additional_exclusion[[j]]
        if (is.null(ae$implementation$source_variable)) {
          stop(
            "enrollments[",
            i,
            "] '",
            enr$name %||% enr$id,
            "' additional_exclusion[",
            j,
            "] '",
            ae$name,
            "' is missing implementation$source_variable",
            call. = FALSE
          )
        }

        # Normalize source_variable (may be a YAML list for multi-source)
        spec$enrollments[[i]]$additional_exclusion[[
          j
        ]]$implementation <-
          .normalize_source_variable(ae$implementation)

        if (
          identical(
            ae$implementation$window,
            "lifetime_before_and_after_baseline"
          )
        ) {
          # Person-level: no window_weeks conversion needed
        } else {
          if (is.null(ae$implementation$window)) {
            stop(
              "enrollments[",
              i,
              "] '",
              enr$name %||% enr$id,
              "' additional_exclusion[",
              j,
              "] '",
              ae$name,
              "' is missing implementation$window",
              call. = FALSE
            )
          }
          spec$enrollments[[i]]$additional_exclusion[[
            j
          ]]$implementation$window_weeks <-
            .convert_window(ae$implementation$window)
        }
      }
    }

    # Normalize has_event additional_inclusion entries
    if (!is.null(enr$additional_inclusion)) {
      for (j in seq_along(enr$additional_inclusion)) {
        ai <- enr$additional_inclusion[[j]]
        if (identical(ai$type, "has_event")) {
          if (is.null(ai$implementation$source_variable)) {
            stop(
              "enrollments[",
              i,
              "] '",
              enr$name %||% enr$id,
              "' additional_inclusion[",
              j,
              "] '",
              ai$name,
              "' is missing implementation$source_variable",
              call. = FALSE
            )
          }
          spec$enrollments[[i]]$additional_inclusion[[
            j
          ]]$implementation <-
            .normalize_source_variable(ai$implementation)
          spec$enrollments[[i]]$additional_inclusion[[
            j
          ]]$implementation$window_weeks <-
            .convert_window(
              ai$implementation$window %||% "lifetime_before_baseline"
            )

          ai_col <- .tte_has_event_col_name(
            spec$enrollments[[i]]$additional_inclusion[[j]]$implementation
          )
          if (ai_col %in% global_inclusion_cols) {
            stop(
              "enrollments[",
              i,
              "] '",
              enr$name %||% enr$id,
              "' additional_inclusion[",
              j,
              "] '",
              ai$name %||% "unnamed",
              "' generates the eligibility column '",
              ai_col,
              "', which a global inclusion criterion already generates. A ",
              "global criterion already applies to every enrollment. Delete ",
              "the enrollment's copy, or give it a different window.",
              call. = FALSE
            )
          }
        }
      }
    }
  }

  # New-user / washout guard: enrollment classifies a person-band as
  # "intervention" via any(rd_intervention) with no built-in initiation rule,
  # so without an exclusion tied to the treatment variable, prevalent users
  # enrol as intervention at every eligible band and discontinuers flip to
  # comparator -- a prevalent-user design, almost never the intended estimand.
  # Warn rather than stop: discontinuation/switching studies legitimately
  # enrol prevalent users.
  for (enr in spec$enrollments) {
    tx_var <- enr$treatment$implementation$variable
    if (is.null(tx_var)) {
      next
    }
    excls <- c(
      spec$exclusion_criteria %||% list(),
      enr$additional_exclusion %||% list()
    )
    # Only an exclusion that TARGETS the treatment variable counts, whatever
    # its type. An exclusion names its target columns in `source_variable`.
    # `.normalize_source_variable()` derives `source_variable_combined` as the
    # `__`-joined name of those columns. Either key can name the treatment
    # variable: a multi-source exclusion whose OR column IS the treatment
    # variable carries it in `source_variable_combined` only.
    # Testing the type counted one `no_prior_intervention` exclusion on any
    # unrelated variable, and silenced a warning whose own text names the
    # treatment variable.
    # `[[` is exact; `$source_variable` would partial-match the `_combined`
    # key when a spec entry carries only that one.
    has_newuser <- any(vapply(
      excls,
      function(ec) {
        impl <- ec$implementation %||% list()
        targets <- c(
          impl[["source_variable"]] %||% character(),
          impl[["source_variable_combined"]] %||% character()
        )
        return(any(tx_var %in% targets))
      },
      logical(1)
    ))
    if (
      !has_newuser &&
        isTRUE(getOption("swereg.warn_prevalent_user", TRUE))
    ) {
      warning(
        "enrollment '",
        enr$id %||% enr$name,
        "' has no new-user/washout exclusion on its treatment variable ('",
        tx_var,
        "'): prevalent users will enrol as intervention at every eligible ",
        "trial period (prevalent-user design). If an incident-user design ",
        "is intended, add an exclusion on the treatment variable -- either ",
        "a finite washout window (e.g. window: 104 weeks, as in Danaei ",
        "2013) or window: 'lifetime_before_baseline' for a never-user ",
        "design (implementation type 'no_prior_intervention').",
        call. = FALSE
      )
    }
  }

  # Convert confounder windows and validate computed confounders
  if (!is.null(spec$confounders)) {
    for (i in seq_along(spec$confounders)) {
      conf <- spec$confounders[[i]]
      if (!is.null(conf$implementation$window)) {
        spec$confounders[[i]]$implementation$window_weeks <-
          .convert_window(conf$implementation$window)
      }
      if (isTRUE(conf$implementation$computed)) {
        if (is.null(conf$implementation$source_variable)) {
          stop(
            "confounders[",
            i,
            "] '",
            conf$name,
            "' is computed but missing implementation$source_variable",
            call. = FALSE
          )
        }
        if (is.null(conf$implementation$window)) {
          stop(
            "confounders[",
            i,
            "] '",
            conf$name,
            "' is computed but missing implementation$window",
            call. = FALSE
          )
        }

        # Normalize source_variable (may be a YAML list for multi-source)
        spec$confounders[[i]]$implementation <-
          .normalize_source_variable(spec$confounders[[i]]$implementation)

        # Auto-derive variable name from source_variable_combined + window
        spec$confounders[[i]]$implementation$variable <- paste0(
          "rd_no_",
          spec$confounders[[i]]$implementation$source_variable_combined,
          "_",
          .window_label(spec$confounders[[i]]$implementation$window_weeks)
        )
      }
    }
  }

  # Normalize subgroups (optional): categorical effect modifiers, each with an
  # implementation$variable that must also be a confounder (checked in
  # tteplan_validate_spec against the skeleton + confounder list).
  if (!is.null(spec$subgroups)) {
    for (i in seq_along(spec$subgroups)) {
      if (is.null(spec$subgroups[[i]]$implementation$variable)) {
        stop(
          "subgroups[",
          i,
          "] (",
          spec$subgroups[[i]]$name %||% "unnamed",
          ") is missing implementation$variable",
          call. = FALSE
        )
      }
      spec$subgroups[[i]]$implementation$variable <-
        as.character(spec$subgroups[[i]]$implementation$variable)
    }
  }

  # Warn about open questions
  if (!is.null(spec$open_questions)) {
    open <- Filter(
      function(q) is.null(q$status) || q$status == "open",
      spec$open_questions
    )
    if (length(open) > 0) {
      warning(
        length(open),
        " open question(s) in spec:\n",
        paste0(
          "  - ",
          vapply(open, function(q) q$question, character(1)),
          collapse = "\n"
        ),
        call. = FALSE
      )
    }
  }

  return(spec)
}


#' Derive the eligibility column name of a `has_event` criterion
#'
#' The one implementation of that name. A global inclusion criterion and a
#' per-enrollment `additional_inclusion` entry both generate it, and
#' [tteplan_apply_exclusions()] reads it back to build the column. A second
#' spelling of the rule would let the read-time collision check guard a name
#' the skeleton never carries.
#'
#' @param impl An implementation list, after `.normalize_source_variable()` and
#'   the `window_weeks` conversion.
#' @return A single string.
#' @noRd
.tte_has_event_col_name <- function(impl) {
  return(paste0(
    "eligible_has_",
    impl$source_variable_combined,
    "_",
    .window_label(impl$window_weeks)
  ))
}


#' The eligibility column names the global inclusion criteria generate
#'
#' Call it after `.tte_normalize_global_inclusion()`. It reads `window_weeks`
#' and `source_variable_combined`, and both are absent before that.
#'
#' @param spec The parsed specification list.
#' @return A character vector, empty when the container holds no `criteria`.
#' @noRd
.tte_global_inclusion_col_names <- function(spec) {
  criteria <- spec[["inclusion_criteria"]][["criteria"]] %||% list()
  return(vapply(
    criteria,
    function(ic) .tte_has_event_col_name(ic[["implementation"]]),
    character(1)
  ))
}


#' Normalize the global inclusion criteria container
#'
#' `inclusion_criteria` holds an `isoyears` pair and a `criteria` list. Each
#' entry in `criteria` MUST declare `type: has_event`. The container accepts no
#' other type today. A criterion that names a type swereg does not know is an
#' error. A criterion that swereg reads and ignores never restricts the study
#' population, and it looks exactly like one that does.
#'
#' Each entry is normalized the same way a per-enrollment `has_event` entry is:
#' `source_variable` gains `source_variable_combined`, and `window` gains
#' `window_weeks`. A missing `window` means `lifetime_before_baseline`.
#'
#' Two criteria that share a source variable and a window generate the same
#' eligibility column name. That is one criterion written twice, so it is an
#' error here rather than a silent overwrite in the skeleton.
#'
#' @param spec The parsed specification list.
#' @return The specification list, with each criterion's `implementation`
#'   normalized.
#' @noRd
.tte_normalize_global_inclusion <- function(spec) {
  # `[[` is exact. `$` partial-matches, and this key comes from a user's YAML.
  criteria <- spec[["inclusion_criteria"]][["criteria"]]
  if (is.null(criteria)) {
    return(spec)
  }

  col_names <- character(0)
  for (j in seq_along(criteria)) {
    ic <- criteria[[j]]
    label <- paste0("inclusion_criteria$criteria[", j, "]")
    if (!is.list(ic)) {
      stop(
        label,
        " is not a mapping. Write `criteria` as a list of criterion objects, ",
        "each with `name`, `type` and `implementation`.",
        call. = FALSE
      )
    }
    label <- paste0(label, " '", ic[["name"]] %||% "unnamed", "'")

    if (!identical(ic[["type"]], "has_event")) {
      stop(
        label,
        " has type '",
        ic[["type"]] %||% "<missing>",
        "'. The only type this container accepts is 'has_event'.",
        call. = FALSE
      )
    }
    if (is.null(ic[["implementation"]][["source_variable"]])) {
      stop(label, " is missing implementation$source_variable", call. = FALSE)
    }

    impl <- .normalize_source_variable(ic[["implementation"]])
    impl$window_weeks <- .convert_window(
      impl[["window"]] %||% "lifetime_before_baseline"
    )
    col_name <- .tte_has_event_col_name(impl)
    if (col_name %in% col_names) {
      stop(
        label,
        " generates the eligibility column '",
        col_name,
        "', which an earlier criterion already generates. Two criteria that ",
        "share a source variable and a window are one criterion. Give them ",
        "different windows, or delete one.",
        call. = FALSE
      )
    }
    col_names <- c(col_names, col_name)
    spec[["inclusion_criteria"]][["criteria"]][[j]][["implementation"]] <- impl
  }
  return(spec)
}


#' Convert a window value to weeks
#'
#' @param window Numeric weeks, or character: "lifetime_before_baseline",
#'   "N year", "N years".
#' @return Numeric: `Inf` for lifetime_before_baseline, integer weeks otherwise.
#' @noRd
.convert_window <- function(window) {
  if (identical(window, "lifetime_before_baseline")) {
    return(Inf)
  }
  if (is.numeric(window)) {
    return(as.integer(window))
  }

  # Legacy string support
  m <- regmatches(window, regexec("^(\\d+)\\s+years?$", window))[[1]]
  if (length(m) == 2) {
    return(as.integer(m[2]) * 52L)
  }

  stop(
    "Cannot parse window: '",
    window,
    "'. Expected 'lifetime_before_baseline', numeric weeks, or 'N year(s)'.",
    call. = FALSE
  )
}
