# =============================================================================
# Cosmetic spec reload
# =============================================================================
# Lets the user refresh enrollment names, treatment-arm labels, outcome names,
# and ETT descriptions on a cached plan WITHOUT re-running the upstream
# pipeline. Structural changes (confounders, exclusions, follow-up windows,
# matching parameters, etc.) are detected and reported via a loud warning but
# NOT applied — the cached results are still bound to the old definitions.
# =============================================================================

#' Build the canonical ETT description string used by `add_one_ett()`.
#'
#' Single source of truth for the ETT description format. Imports from
#' stringr for str_replace, matching the original construction site.
#'
#' @noRd
.format_ett_description <- function(ett_id, outcome_name, follow_up,
                                    age_group) {
  paste0(
    ett_id,
    ": ",
    outcome_name,
    " (",
    follow_up,
    "w, age ",
    stringr::str_replace(age_group, "_", "-"),
    ")"
  )
}


#' Walk the spec's `outcomes` block and build lookup tables keyed by the
#' implementation variable used in the analysis.
#'
#' Returns a list with two named character vectors: `name` and
#' `description`. Both are keyed by `outcome_var`. `description` is NA
#' when the spec omits the field.
#'
#' @noRd
.spec_outcome_name_lookup <- function(spec) {
  if (is.null(spec) || is.null(spec$outcomes)) return(NULL)
  rows <- lapply(spec$outcomes, function(o) {
    var <- o$implementation$variable_combined %||% o$implementation$variable
    if (is.null(var)) return(NULL)
    list(
      outcome_var = var,
      outcome_name = o$name %||% var,
      outcome_description = o$description %||% NA_character_
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(NULL)
  vars <- vapply(rows, `[[`, character(1), "outcome_var")
  list(
    name = setNames(
      vapply(rows, `[[`, character(1), "outcome_name"),
      vars
    ),
    description = setNames(
      vapply(rows, `[[`, character(1), "outcome_description"),
      vars
    )
  )
}


#' Compare the cosmetic and structural fields of two specs and return a
#' two-element list of human-readable difference messages.
#'
#' Cosmetic fields can be hot-reloaded. Structural fields warrant a warning
#' because cached results were computed against the old values.
#'
#' @noRd
.diff_specs <- function(old, new) {
  cosmetic <- character()
  structural <- character()

  push_c <- function(path, before, after) {
    cosmetic <<- c(
      cosmetic,
      sprintf("%s: %s -> %s",
              path,
              .ds_show(before), .ds_show(after))
    )
  }
  push_s <- function(path, before, after) {
    structural <<- c(
      structural,
      sprintf("%s: %s -> %s",
              path,
              .ds_show(before), .ds_show(after))
    )
  }

  # Study block (all cosmetic)
  for (k in c("title", "description", "principal_investigator")) {
    a <- old$study[[k]]
    b <- new$study[[k]]
    if (!identical(a, b)) push_c(paste0("study$", k), a, b)
  }

  # Inclusion criteria (structural)
  if (!identical(old$inclusion_criteria, new$inclusion_criteria)) {
    push_s("inclusion_criteria", old$inclusion_criteria, new$inclusion_criteria)
  }

  # Exclusion criteria (structural — name/window/implementation)
  if (!identical(old$exclusion_criteria, new$exclusion_criteria)) {
    push_s("exclusion_criteria", "<changed>", "<changed>")
  }

  # Confounders (structural)
  if (!identical(old$confounders, new$confounders)) {
    # Compare by implementation variable + computed flag
    old_vars <- vapply(
      old$confounders %||% list(), function(c) c$implementation$variable %||% "",
      character(1)
    )
    new_vars <- vapply(
      new$confounders %||% list(), function(c) c$implementation$variable %||% "",
      character(1)
    )
    if (!identical(sort(old_vars), sort(new_vars))) {
      push_s("confounders$variables",
             paste(sort(old_vars), collapse = ", "),
             paste(sort(new_vars), collapse = ", "))
    }
    # Cosmetic name updates handled separately on apply
  }

  # Follow-up (structural)
  if (!identical(old$follow_up, new$follow_up)) {
    push_s("follow_up", old$follow_up, new$follow_up)
  }

  # Outcomes — name is cosmetic, implementation is structural
  old_out <- old$outcomes %||% list()
  new_out <- new$outcomes %||% list()
  for (i in seq_along(new_out)) {
    o_new <- new_out[[i]]
    o_old <- if (i <= length(old_out)) old_out[[i]] else NULL
    if (is.null(o_old)) {
      push_s(sprintf("outcomes[[%d]]", i), "<missing>", "<added>")
      next
    }
    if (!identical(o_old$implementation, o_new$implementation)) {
      push_s(sprintf("outcomes[[%d]]$implementation", i),
             "<changed>", "<changed>")
    }
    if (!identical(o_old$name, o_new$name)) {
      push_c(sprintf("outcomes[[%d]]$name", i), o_old$name, o_new$name)
    }
  }
  if (length(old_out) > length(new_out)) {
    push_s("outcomes", sprintf("%d outcomes", length(old_out)),
           sprintf("%d outcomes", length(new_out)))
  }

  # Enrollments — names + arm labels are cosmetic; implementation is structural
  old_enr <- old$enrollments %||% list()
  new_enr <- new$enrollments %||% list()
  enr_by_id <- function(lst) {
    setNames(lst, vapply(lst, function(e) as.character(e$id %||% NA),
                          character(1)))
  }
  old_by <- enr_by_id(old_enr)
  new_by <- enr_by_id(new_enr)
  all_ids <- unique(c(names(old_by), names(new_by)))
  for (id in all_ids) {
    o_old <- old_by[[id]]
    o_new <- new_by[[id]]
    if (is.null(o_old)) {
      push_s(sprintf("enrollments[%s]", id), "<missing>", "<added>")
      next
    }
    if (is.null(o_new)) {
      push_s(sprintf("enrollments[%s]", id), "<present>", "<removed>")
      next
    }
    if (!identical(o_old$additional_inclusion, o_new$additional_inclusion)) {
      push_s(sprintf("enrollments[%s]$additional_inclusion", id),
             "<changed>", "<changed>")
    }
    if (!identical(o_old$additional_exclusion, o_new$additional_exclusion)) {
      push_s(sprintf("enrollments[%s]$additional_exclusion", id),
             "<changed>", "<changed>")
    }
    if (!identical(o_old$treatment$implementation,
                   o_new$treatment$implementation)) {
      push_s(sprintf("enrollments[%s]$treatment$implementation", id),
             "<changed>", "<changed>")
    }
    if (!identical(o_old$name, o_new$name)) {
      push_c(sprintf("enrollments[%s]$name", id), o_old$name, o_new$name)
    }
    if (!identical(o_old$treatment$description, o_new$treatment$description)) {
      push_c(sprintf("enrollments[%s]$treatment$description", id),
             o_old$treatment$description, o_new$treatment$description)
    }
    arm_keys <- c("intervention", "comparator")
    for (k in arm_keys) {
      a <- o_old$treatment$arms[[k]]
      b <- o_new$treatment$arms[[k]]
      if (!identical(a, b)) {
        push_c(sprintf("enrollments[%s]$treatment$arms$%s", id, k), a, b)
      }
    }
  }

  list(cosmetic = cosmetic, structural = structural)
}


#' Pretty-print a value for diff messages. Truncates long strings.
#' @noRd
.ds_show <- function(x) {
  if (is.null(x)) return("NULL")
  if (length(x) == 0L) return("()")
  s <- if (is.atomic(x)) {
    paste(x, collapse = ", ")
  } else {
    paste0("<", class(x)[1], ">")
  }
  if (nchar(s) > 80L) s <- paste0(substr(s, 1L, 77L), "...")
  s
}


#' Apply cosmetic spec updates in place on a TTEPlan object.
#'
#' Updates `self$spec`, refreshes `self$ett$outcome_name`, recomputes
#' `self$ett$description`, and overwrites cached
#' `self$results_ett[[ett_id]]$description` to match.
#'
#' @noRd
.apply_cosmetic_spec_updates <- function(plan, new_spec) {
  # Replace the spec entirely (it's safe — structural diffs already warned)
  # but only the cosmetic fields will actually be CONSULTED downstream.
  plan$spec <- new_spec

  # Refresh outcome_name and outcome_description on plan$ett by re-joining
  # from the new outcomes block.
  outcome_lookup <- .spec_outcome_name_lookup(new_spec)
  if (!is.null(outcome_lookup) && !is.null(plan$ett) && nrow(plan$ett) > 0L) {
    ov <- as.character(plan$ett$outcome_var)
    new_names <- outcome_lookup$name[ov]
    new_desc  <- outcome_lookup$description[ov]
    ok <- !is.na(new_names)
    if (any(ok)) {
      data.table::set(
        plan$ett, which(ok), "outcome_name", new_names[ok]
      )
    }
    if (!"outcome_description" %in% names(plan$ett)) {
      plan$ett[, outcome_description := NA_character_]
    }
    ok_desc <- !is.na(new_desc)
    if (any(ok_desc)) {
      data.table::set(
        plan$ett, which(ok_desc), "outcome_description", new_desc[ok_desc]
      )
    }
  }

  # Recompute ett$description from the refreshed outcome names.
  if (!is.null(plan$ett) && nrow(plan$ett) > 0L) {
    new_desc <- vapply(seq_len(nrow(plan$ett)), function(i) {
      r <- plan$ett[i]
      .format_ett_description(
        ett_id = r$ett_id,
        outcome_name = r$outcome_name,
        follow_up = r$follow_up,
        age_group = r$age_group
      )
    }, character(1))
    data.table::set(plan$ett, j = "description", value = new_desc)
  }

  # Mirror description into cached per-ETT results.
  if (!is.null(plan$results_ett) && length(plan$results_ett) > 0L &&
      !is.null(plan$ett) && nrow(plan$ett) > 0L) {
    desc_lookup <- setNames(plan$ett$description, plan$ett$ett_id)
    for (eid in names(plan$results_ett)) {
      if (eid %in% names(desc_lookup)) {
        plan$results_ett[[eid]]$description <- desc_lookup[[eid]]
      }
    }
  }

  invisible(plan)
}
