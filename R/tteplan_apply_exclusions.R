# =============================================================================
# tteplan_apply_exclusions
# =============================================================================

# --- internal: batched per-group eligibility evaluator -----------------------
#
# Collects an arbitrary set of eligibility specs and emits them all in ONE
# `dt[, c(...) := list(...), by = id]` call. Each spec is one of:
#
#   list(col_name, type = "lifetime",         source_var)
#   list(col_name, type = "windowed",         source_var, window_weeks,
#        negate_final = FALSE)
#   list(col_name, type = "windowed_no_obs",  source_var, value, window_weeks)
#
# The per-criterion helpers (skeleton_eligible_*) each do their own by=id
# pass; with 12 exclusions in 003 that's 12 separate radix-walks + 12 column
# allocations. Batching shares the group identification across all specs.
# Each spec contributes one element to the `list(...)` j-expression below,
# so data.table compiles the j once and evaluates it per group.
.tte_apply_eligibility_batch <- function(skeleton, specs, id_col = "id") {
  if (length(specs) == 0L) {
    return(skeleton)
  }

  col_names <- vapply(specs, `[[`, character(1), "col_name")

  # Build the j-expression as a list() call of N typed sub-expressions.
  # Using bquote() so the function is named at compile time (no .C-style
  # dispatch through `get()` per group, and no R-level for-loop inside j).
  per_spec_call <- lapply(specs, function(sp) {
    src_sym <- as.name(sp$source_var)
    switch(
      sp$type,
      "lifetime" = bquote(!any(.(src_sym), na.rm = TRUE)),
      "windowed" = {
        inner <- bquote(swereg::any_events_prior_to(
          .(src_sym),
          window_excluding_wk0 = .(as.integer(sp$window_weeks))
        ))
        if (isTRUE(sp$negate_final)) inner else bquote(!.(inner))
      },
      "windowed_no_obs" = bquote(
        !swereg::any_events_prior_to(
          .(src_sym) == .(sp$value),
          window_excluding_wk0 = .(as.integer(sp$window_weeks))
        )
      ),
      stop("Unknown eligibility spec type: ", sp$type)
    )
  })
  j_expr <- as.call(c(quote(list), per_spec_call))

  skeleton[,
    (col_names) := eval(j_expr),
    by = id_col
  ]
  skeleton
}

# --- internal: collect exclusion grouped specs ------------------------------
#
# Mirrors tteplan_apply_exclusions but returns
#   list(eligible_cols = ..., grouped_specs = ...)
# instead of running the by=id batch. The skeleton is still mutated in place
# (combined outcome columns, vectorized isoyears + age_range eligibles,
# .ensure_combined_column for list-valued source_variable). Callers run the
# batch themselves -- standalone via tteplan_apply_exclusions(), or fused
# with .tte_build_confounder_specs() in .s1_prepare_skeleton() for a single
# combined batch across exclusions + computed confounders.
.tte_build_exclusion_specs <- function(skeleton, spec, enrollment_spec) {
  enrollment_id <- enrollment_spec$enrollment_id

  enrollment_def <- NULL
  for (enr in spec$enrollments) {
    if (enr$id == enrollment_id) {
      enrollment_def <- enr
      break
    }
  }
  if (is.null(enrollment_def)) {
    stop("Enrollment ID '", enrollment_id, "' not found in spec$enrollments")
  }

  # 0. Create combined outcome columns (multi-source outcomes)
  for (outcome in spec$outcomes) {
    v <- outcome$implementation$variable
    if (length(v) > 1L) {
      combined <- outcome$implementation$variable_combined
      if (!combined %in% names(skeleton)) {
        skeleton[, (combined) := Reduce(`|`, .SD), .SDcols = v]
      }
    }
  }

  # 1. Calendar years (vectorized, no by=id grouping)
  years <- seq(
    spec$inclusion_criteria$isoyears[1],
    spec$inclusion_criteria$isoyears[2]
  )
  skeleton_eligible_isoyears(skeleton, years)
  eligible_cols <- "eligible_isoyears"

  grouped_specs <- list()

  # 2. Enrollment-specific additional inclusion (age_range is vectorized;
  #    has_event goes into the grouped batch)
  if (!is.null(enrollment_def$additional_inclusion)) {
    for (ae in enrollment_def$additional_inclusion) {
      if (identical(ae$type, "age_range")) {
        skeleton_eligible_age_range(
          skeleton,
          age_var = ae$implementation$variable,
          min_age = ae$min,
          max_age = ae$max
        )
        eligible_cols <- c(eligible_cols, "eligible_age")
      } else if (identical(ae$type, "has_event")) {
        impl <- ae$implementation
        sv <- impl$source_variable_combined
        .ensure_combined_column(skeleton, impl)
        window <- impl$window_weeks
        col_name <- paste0(
          "eligible_has_",
          sv,
          "_",
          .window_label(window)
        )
        # negate_final = TRUE: emit `any_events_prior_to(...)` directly
        # (i.e. has-event semantics) without the temp-col round-trip.
        grouped_specs[[length(grouped_specs) + 1L]] <- list(
          col_name = col_name,
          type = "windowed",
          source_var = sv,
          window_weeks = if (is.infinite(window)) {
            99999L
          } else {
            as.integer(window)
          },
          negate_final = TRUE
        )
        eligible_cols <- c(eligible_cols, col_name)
      }
    }
  }

  # 3. Global exclusion criteria (all grouped)
  for (ec in spec$exclusion_criteria) {
    impl <- ec$implementation
    sv <- impl$source_variable_combined
    .ensure_combined_column(skeleton, impl)

    if (identical(impl$window, "lifetime_before_and_after_baseline")) {
      col_name <- paste0(
        "eligible_no_",
        sv,
        "_lifetime_before_and_after_baseline"
      )
      grouped_specs[[length(grouped_specs) + 1L]] <- list(
        col_name = col_name,
        type = "lifetime",
        source_var = sv
      )
    } else if (identical(impl$type, "no_prior_intervention")) {
      window <- impl$window_weeks
      col_name <- paste0("eligible_no_", sv, "_", .window_label(window))
      grouped_specs[[length(grouped_specs) + 1L]] <- list(
        col_name = col_name,
        type = "windowed_no_obs",
        source_var = sv,
        value = impl$intervention_value,
        window_weeks = if (is.infinite(window)) 99999L else as.integer(window)
      )
    } else {
      window <- impl$window_weeks
      col_name <- paste0("eligible_no_", sv, "_", .window_label(window))
      grouped_specs[[length(grouped_specs) + 1L]] <- list(
        col_name = col_name,
        type = "windowed",
        source_var = sv,
        window_weeks = if (is.infinite(window)) 99999L else as.integer(window),
        negate_final = FALSE
      )
    }
    eligible_cols <- c(eligible_cols, col_name)
  }

  # 4. Enrollment-specific additional exclusion criteria (all grouped)
  if (!is.null(enrollment_def$additional_exclusion)) {
    for (ec in enrollment_def$additional_exclusion) {
      impl <- ec$implementation
      sv <- impl$source_variable_combined
      .ensure_combined_column(skeleton, impl)

      if (identical(impl$window, "lifetime_before_and_after_baseline")) {
        col_name <- paste0(
          "eligible_no_",
          sv,
          "_lifetime_before_and_after_baseline"
        )
        grouped_specs[[length(grouped_specs) + 1L]] <- list(
          col_name = col_name,
          type = "lifetime",
          source_var = sv
        )
      } else if (identical(impl$type, "no_prior_intervention")) {
        window <- impl$window_weeks
        col_name <- paste0("eligible_no_", sv, "_", .window_label(window))
        grouped_specs[[length(grouped_specs) + 1L]] <- list(
          col_name = col_name,
          type = "windowed_no_obs",
          source_var = sv,
          value = impl$intervention_value,
          window_weeks = if (is.infinite(window)) 99999L else as.integer(window)
        )
      } else {
        window <- impl$window_weeks
        col_name <- paste0("eligible_no_", sv, "_", .window_label(window))
        grouped_specs[[length(grouped_specs) + 1L]] <- list(
          col_name = col_name,
          type = "windowed",
          source_var = sv,
          window_weeks = if (is.infinite(window)) {
            99999L
          } else {
            as.integer(window)
          },
          negate_final = FALSE
        )
      }
      eligible_cols <- c(eligible_cols, col_name)
    }
  }

  list(eligible_cols = eligible_cols, grouped_specs = grouped_specs)
}

#' Apply exclusion criteria from a study spec to a skeleton
#'
#' Applies calendar year eligibility, enrollment-specific additional inclusion
#' (e.g., age range), global exclusion criteria, and enrollment-specific
#' additional exclusion criteria from the parsed study specification. Calls
#' [skeleton_eligible_combine()] at the end to AND all criteria into a single
#' `eligible` column.
#'
#' @param skeleton A data.table skeleton (person-week panel).
#' @param spec Parsed study specification from [tteplan_read_spec()].
#' @param enrollment_spec Enrollment spec from the plan (must contain
#'   `enrollment_id`), as returned by `plan[[i]]`.
#' @return The skeleton (modified by reference), with eligibility columns
#'   added and a combined `eligible` column.
#'
#' @family tte_spec
#' @export
tteplan_apply_exclusions <- function(skeleton, spec, enrollment_spec) {
  built <- .tte_build_exclusion_specs(skeleton, spec, enrollment_spec)
  skeleton <- .tte_apply_eligibility_batch(
    skeleton,
    built$grouped_specs,
    id_col = "id"
  )
  skeleton <- skeleton_eligible_combine(skeleton, built$eligible_cols)
  data.table::setattr(skeleton, "eligible_cols", built$eligible_cols)
  skeleton
}


#' Format a window value as a label for column names
#'
#' @param window_weeks Numeric: weeks or Inf.
#' @return Character: "everbefore" for Inf, "{weeks}wk" otherwise.
#' @noRd
.window_label <- function(window_weeks) {
  if (is.infinite(window_weeks)) "everbefore" else paste0(window_weeks, "wk")
}


#' Normalize source_variable to a character vector and derive a combined name
#'
#' YAML lists become R lists; this ensures we always work with character vectors.
#' If multiple variables, `source_variable_combined` is the `__`-joined name.
#' If single, `source_variable_combined` equals `source_variable`.
#'
#' @param impl The implementation list from a spec entry.
#' @return The implementation list with `source_variable` as character vector
#'   and `source_variable_combined` as a single string.
#' @noRd
.normalize_source_variable <- function(impl) {
  sv <- impl$source_variable
  if (is.list(sv)) {
    sv <- unlist(sv)
  }
  impl$source_variable <- as.character(sv)
  impl$source_variable_combined <- paste(impl$source_variable, collapse = "__")
  impl
}


#' Ensure a combined source variable column exists on the skeleton
#'
#' If `source_variable` has multiple elements, creates (or overwrites) the
#' combined column as the row-wise OR. If single, does nothing.
#'
#' @param skeleton A data.table.
#' @param impl Implementation list (after `.normalize_source_variable()`).
#' @return The skeleton (modified by reference).
#' @noRd
.ensure_combined_column <- function(skeleton, impl) {
  sv <- impl$source_variable
  if (length(sv) > 1L) {
    combined <- impl$source_variable_combined
    skeleton[, (combined) := Reduce(`|`, .SD), .SDcols = sv]
  }
  invisible(skeleton)
}
