# =============================================================================
# Cosmetic spec reload
# =============================================================================
# Lets the user refresh enrollment names, treatment-arm labels, outcome names,
# and ETT descriptions on a cached plan WITHOUT re-running the upstream
# pipeline. Structural changes (confounders, exclusions, follow-up windows,
# comparator-draw parameters, etc.) are detected and reported via a loud
# warning. They are NOT applied: the cached results are still bound to the
# old definitions.
# =============================================================================

#' Build the canonical ETT description string used by `add_one_ett()`.
#'
#' Single source of truth for the ETT description format. Imports from
#' stringr for str_replace, matching the original construction site.
#'
#' @noRd
.format_ett_description <- function(
  ett_id,
  outcome_name,
  follow_up,
  age_group
) {
  return(paste0(
    ett_id,
    ": ",
    outcome_name,
    " (",
    follow_up,
    "w, age ",
    stringr::str_replace(age_group, "_", "-"),
    ")"
  ))
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
  if (is.null(spec) || is.null(spec$outcomes)) {
    return(NULL)
  }
  rows <- lapply(spec$outcomes, function(o) {
    var <- o$implementation$variable_combined %||% o$implementation$variable
    if (is.null(var)) {
      return(NULL)
    }
    return(list(
      outcome_var = var,
      outcome_name = o$name %||% var,
      outcome_description = o$description %||% NA_character_,
      outcome_role = o$role %||% NA_character_
    ))
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(NULL)
  }
  vars <- vapply(rows, `[[`, character(1), "outcome_var")
  return(list(
    name = setNames(
      vapply(rows, `[[`, character(1), "outcome_name"),
      vars
    ),
    description = setNames(
      vapply(rows, `[[`, character(1), "outcome_description"),
      vars
    ),
    role = setNames(
      vapply(rows, `[[`, character(1), "outcome_role"),
      vars
    )
  ))
}


#' Name a spec block by each entry's implementation variable.
#'
#' `confounders` and `subgroups` carry no `id`. The implementation variable
#' identifies one entry, and it is what the analysis reads. Matching on it
#' means a reorder of the block is not a change.
#'
#' @return A character vector, one element per entry. An entry that declares
#'   no variable gets `NA_character_`.
#' @noRd
.ds_var_ids <- function(lst) {
  return(vapply(
    lst %||% list(),
    function(x) {
      v <- x$implementation$variable
      if (is.null(v)) {
        return(NA_character_)
      }
      return(paste(as.character(v), collapse = "__"))
    },
    character(1)
  ))
}


#' Report any key that no branch of `.diff_specs()` classifies.
#'
#' Every container that `.diff_specs()` opens names the keys it handles, then
#' calls this function on the rest. A changed key outside that list is an
#' unclassified change, and this function calls it structural. The message
#' carries the marker `(unclassified)`.
#'
#' Structural is the safe direction. swereg cannot know whether a key it never
#' saw is a label or a definition. A warning that refuses the change costs a
#' reader one message. A silent copy costs a cached result its meaning.
#'
#' @noRd
.ds_residual <- function(old_node, new_node, handled, prefix, ps) {
  keys <- setdiff(union(names(old_node), names(new_node)), handled)
  for (k in keys) {
    if (!identical(old_node[[k]], new_node[[k]])) {
      ps(paste0(prefix, k, " (unclassified)"), old_node[[k]], new_node[[k]])
    }
  }
  return(invisible(NULL))
}


#' Classify the `study` block.
#'
#' `title`, `description`, `principal_investigator` and `design` are labels.
#' The spec summary sheet and the printed report are the only readers.
#'
#' Three keys under `study$implementation` are definitions:
#'
#' 1. `conf_level`. s3 computed every interval at this level, and the export
#'    header states the level s3 used.
#' 2. `project_prefix`. `plan$project_prefix` holds a copy, and every
#'    `file_raw`, `file_imp` and `file_analysis` value in `plan$ett` is built
#'    from it.
#' 3. `version`. `plan$spec_version` holds a copy, and it selects
#'    `plan$dir_results` and `plan$spec_path`.
#'
#' `date` and `status` are metadata. No code in `R/` reads either one.
#'
#' @noRd
.ds_diff_study <- function(old, new, pc, ps) {
  for (k in c("title", "description", "principal_investigator", "design")) {
    if (!identical(old$study[[k]], new$study[[k]])) {
      pc(paste0("study$", k), old$study[[k]], new$study[[k]])
    }
  }
  for (k in c("conf_level", "project_prefix", "version")) {
    a <- old$study$implementation[[k]]
    b <- new$study$implementation[[k]]
    if (!identical(a, b)) {
      ps(paste0("study$implementation$", k), a, b)
    }
  }
  for (k in c("date", "status")) {
    a <- old$study$implementation[[k]]
    b <- new$study$implementation[[k]]
    if (!identical(a, b)) {
      pc(paste0("study$implementation$", k), a, b)
    }
  }
  .ds_residual(
    old$study$implementation,
    new$study$implementation,
    c("conf_level", "project_prefix", "version", "date", "status"),
    "study$implementation$",
    ps
  )
  .ds_residual(
    old$study,
    new$study,
    c(
      "title",
      "description",
      "principal_investigator",
      "design",
      "implementation"
    ),
    "study$",
    ps
  )
  return(invisible(NULL))
}


#' Classify the two criteria containers and the follow-up grid.
#'
#' Each one decides who enters the study, or for how long swereg follows them.
#' A cached result cannot be refreshed with a new value, so every change inside
#' the three blocks is structural.
#'
#' @noRd
.ds_diff_definitions <- function(old, new, ps) {
  if (!identical(old$inclusion_criteria, new$inclusion_criteria)) {
    ps("inclusion_criteria", old$inclusion_criteria, new$inclusion_criteria)
  }
  if (!identical(old$exclusion_criteria, new$exclusion_criteria)) {
    ps("exclusion_criteria", "<changed>", "<changed>")
  }
  if (!identical(old$follow_up, new$follow_up)) {
    ps("follow_up", old$follow_up, new$follow_up)
  }
  return(invisible(NULL))
}


#' Classify the `outcomes` block.
#'
#' `name`, `description` and `role` are labels. `R/tteplan_export.R:817` reads
#' `role` for the forest row label, and `plan$ett` carries a copy of all three.
#' `implementation` names the analysis column, so it is a definition.
#'
#' The block matches by position. An outcome that only one spec holds is
#' structural, and so is a change in the number of outcomes.
#'
#' @noRd
.ds_diff_outcomes <- function(old, new, pc, ps) {
  old_out <- old$outcomes %||% list()
  new_out <- new$outcomes %||% list()
  for (i in seq_along(new_out)) {
    o_new <- new_out[[i]]
    o_old <- if (i <= length(old_out)) old_out[[i]] else NULL
    if (is.null(o_old)) {
      ps(sprintf("outcomes[[%d]]", i), "<missing>", "<added>")
      next
    }
    if (!identical(o_old$implementation, o_new$implementation)) {
      ps(sprintf("outcomes[[%d]]$implementation", i), "<changed>", "<changed>")
    }
    for (k in c("name", "description", "role")) {
      if (!identical(o_old[[k]], o_new[[k]])) {
        pc(sprintf("outcomes[[%d]]$%s", i, k), o_old[[k]], o_new[[k]])
      }
    }
    .ds_residual(
      o_old,
      o_new,
      c("name", "description", "role", "implementation"),
      sprintf("outcomes[[%d]]$", i),
      ps
    )
  }
  if (length(old_out) > length(new_out)) {
    ps(
      "outcomes",
      sprintf("%d outcomes", length(old_out)),
      sprintf("%d outcomes", length(new_out))
    )
  }
  return(invisible(NULL))
}


#' Classify `confounders` and `subgroups`.
#'
#' Both blocks match by implementation variable, so a reorder is not a change.
#' The variable set enters the model and each `implementation` states how the
#' column is built, so both are definitions. Every other key is a label that
#' reaches the spec summary sheet.
#'
#' @noRd
.ds_diff_var_blocks <- function(old, new, pc, ps) {
  blocks <- list(
    confounders = c("name", "categories", "codes", "rationale"),
    subgroups = "name"
  )
  for (blk in names(blocks)) {
    old_lst <- old[[blk]] %||% list()
    new_lst <- new[[blk]] %||% list()
    old_v <- .ds_var_ids(old_lst)
    new_v <- .ds_var_ids(new_lst)
    if (!identical(sort(old_v, na.last = TRUE), sort(new_v, na.last = TRUE))) {
      ps(
        paste0(blk, "$variables"),
        paste(sort(old_v, na.last = TRUE), collapse = ", "),
        paste(sort(new_v, na.last = TRUE), collapse = ", ")
      )
    }
    labels <- blocks[[blk]]
    for (i in seq_along(old_v)) {
      j <- match(old_v[i], new_v)
      if (is.na(j)) {
        next
      }
      e_old <- old_lst[[i]]
      e_new <- new_lst[[j]]
      if (!identical(e_old$implementation, e_new$implementation)) {
        ps(
          sprintf("%s[%s]$implementation", blk, old_v[i]),
          "<changed>",
          "<changed>"
        )
      }
      for (k in labels) {
        if (!identical(e_old[[k]], e_new[[k]])) {
          pc(sprintf("%s[%s]$%s", blk, old_v[i], k), e_old[[k]], e_new[[k]])
        }
      }
      .ds_residual(
        e_old,
        e_new,
        c(labels, "implementation"),
        sprintf("%s[%s]$", blk, old_v[i]),
        ps
      )
    }
  }
  return(invisible(NULL))
}


#' Classify the three display-only blocks.
#'
#' `R/tteplan_spec_summary_sheet.R:590` reads `standing_methods`.
#' `R/protocol_table.R` renders `target_trial`, which is the study team's own
#' prose. `tteplan_read_spec()` warns about `open_questions` and nothing else
#' reads it.
#'
#' None of the three enters an analysis, so every change inside is cosmetic.
#'
#' @noRd
.ds_diff_display <- function(old, new, pc) {
  for (k in c("standing_methods", "target_trial", "open_questions")) {
    if (!identical(old[[k]], new[[k]])) {
      pc(k, "<changed>", "<changed>")
    }
  }
  return(invisible(NULL))
}
#' Classify the `enrollments` block.
#'
#' Enrollments match by `id`. An enrollment that only one spec holds is
#' structural, because a cached run covers the enrollments the old spec named.
#'
#' The name, the treatment description and both arm labels are labels.
#'
#' Four keys are definitions: the two additional criteria containers, the
#' treatment implementation, the observation encoding and both arm tolerances.
#' Each one decides who is enrolled, or when swereg censors them. A cached run
#' cannot be refreshed with a new value.
#'
#' @noRd
.ds_diff_enrollments <- function(old, new, pc, ps) {
  old_enr <- old$enrollments %||% list()
  new_enr <- new$enrollments %||% list()
  enr_by_id <- function(lst) {
    return(setNames(
      lst,
      vapply(lst, function(e) as.character(e$id %||% NA), character(1))
    ))
  }
  old_by <- enr_by_id(old_enr)
  new_by <- enr_by_id(new_enr)
  all_ids <- unique(c(names(old_by), names(new_by)))
  for (id in all_ids) {
    o_old <- old_by[[id]]
    o_new <- new_by[[id]]
    if (is.null(o_old)) {
      ps(sprintf("enrollments[%s]", id), "<missing>", "<added>")
      next
    }
    if (is.null(o_new)) {
      ps(sprintf("enrollments[%s]", id), "<present>", "<removed>")
      next
    }
    if (!identical(o_old$additional_inclusion, o_new$additional_inclusion)) {
      ps(
        sprintf("enrollments[%s]$additional_inclusion", id),
        "<changed>",
        "<changed>"
      )
    }
    if (!identical(o_old$additional_exclusion, o_new$additional_exclusion)) {
      ps(
        sprintf("enrollments[%s]$additional_exclusion", id),
        "<changed>",
        "<changed>"
      )
    }
    if (
      !identical(o_old$treatment$implementation, o_new$treatment$implementation)
    ) {
      ps(
        sprintf("enrollments[%s]$treatment$implementation", id),
        "<changed>",
        "<changed>"
      )
    }
    # The observation encoding and both arm tolerances are structural. Each
    # one changes who is enrolled and when they are censored, so a cached run
    # cannot be refreshed with the new value.
    if (!identical(o_old$observed_var, o_new$observed_var)) {
      ps(
        sprintf("enrollments[%s]$observed_var", id),
        .ds_observed_var(o_old$observed_var),
        .ds_observed_var(o_new$observed_var)
      )
    }
    for (k in c(
      "intervention_tolerance_weeks",
      "comparator_tolerance_weeks"
    )) {
      # A plan saved before the observation contract carries no tolerance at
      # all. Read that as the default of 0 weeks, which is what it meant, so
      # an upgrade alone does not report a structural change.
      a <- o_old[[k]] %||% 0L
      b <- o_new[[k]] %||% 0L
      if (!identical(a, b)) {
        ps(sprintf("enrollments[%s]$%s", id, k), a, b)
      }
    }
    if (!identical(o_old$name, o_new$name)) {
      pc(sprintf("enrollments[%s]$name", id), o_old$name, o_new$name)
    }
    if (!identical(o_old$treatment$description, o_new$treatment$description)) {
      pc(
        sprintf("enrollments[%s]$treatment$description", id),
        o_old$treatment$description,
        o_new$treatment$description
      )
    }
    arm_keys <- c("intervention", "comparator")
    for (k in arm_keys) {
      a <- o_old$treatment$arms[[k]]
      b <- o_new$treatment$arms[[k]]
      if (!identical(a, b)) {
        pc(sprintf("enrollments[%s]$treatment$arms$%s", id, k), a, b)
      }
    }
    .ds_residual(
      o_old$treatment$arms,
      o_new$treatment$arms,
      arm_keys,
      sprintf("enrollments[%s]$treatment$arms$", id),
      ps
    )
    .ds_residual(
      o_old$treatment,
      o_new$treatment,
      c("arms", "description", "implementation"),
      sprintf("enrollments[%s]$treatment$", id),
      ps
    )
    .ds_residual(
      o_old,
      o_new,
      c(
        "id",
        "name",
        "observed_var",
        "intervention_tolerance_weeks",
        "comparator_tolerance_weeks",
        "additional_inclusion",
        "additional_exclusion",
        "treatment"
      ),
      sprintf("enrollments[%s]$", id),
      ps
    )
  }
  return(invisible(NULL))
}


# The top-level blocks that `.diff_specs()` classifies. A key outside this
# vector is unclassified, and `.ds_residual()` calls it structural.
.DS_TOP_LEVEL_BLOCKS <- c(
  "confounders",
  "enrollments",
  "exclusion_criteria",
  "follow_up",
  "inclusion_criteria",
  "open_questions",
  "outcomes",
  "standing_methods",
  "study",
  "subgroups",
  "target_trial"
)


#' Compare two specs and classify every changed path.
#'
#' Every field is deliberately cosmetic or structural. A cosmetic field is a
#' label, and `$reload_spec()` copies it. A structural field is a definition
#' that the cached results were computed against, so `$reload_spec()` warns and
#' refuses it.
#'
#' Nothing is left unclassified. Every container this function opens names the
#' keys it handles and then calls `.ds_residual()` on the rest. An unhandled
#' change is reported as structural, with the marker `(unclassified)`.
#'
#' @return A list of two character vectors, `cosmetic` and `structural`. Each
#'   element is one human-readable difference message.
#' @noRd
.diff_specs <- function(old, new) {
  cosmetic <- character()
  structural <- character()

  push_c <- function(path, before, after) {
    return(
      cosmetic <<- c(
        cosmetic,
        sprintf("%s: %s -> %s", path, .ds_show(before), .ds_show(after))
      )
    )
  }
  push_s <- function(path, before, after) {
    return(
      structural <<- c(
        structural,
        sprintf("%s: %s -> %s", path, .ds_show(before), .ds_show(after))
      )
    )
  }

  .ds_diff_study(old, new, push_c, push_s)
  .ds_diff_definitions(old, new, push_s)
  .ds_diff_outcomes(old, new, push_c, push_s)
  .ds_diff_var_blocks(old, new, push_c, push_s)
  .ds_diff_display(old, new, push_c)
  .ds_diff_enrollments(old, new, push_c, push_s)
  .ds_residual(old, new, .DS_TOP_LEVEL_BLOCKS, "", push_s)

  return(list(cosmetic = cosmetic, structural = structural))
}


#' Pretty-print an observation encoding for diff messages.
#'
#' `.ds_show()` alone would print the class name and hide the value, which is
#' the one thing a reader of the diff needs.
#'
#' @noRd
.ds_observed_var <- function(x) {
  if (is.null(x)) {
    return("<not declared>")
  }
  col <- .tte_observed_column(x)
  if (!is.null(col)) {
    return(paste0("column ", col))
  }
  return(paste0("sentinel ", x$sentinel))
}


#' Pretty-print a value for diff messages. Truncates long strings.
#' @noRd
.ds_show <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (length(x) == 0L) {
    return("()")
  }
  s <- if (is.atomic(x)) {
    paste(x, collapse = ", ")
  } else {
    paste0("<", class(x)[1], ">")
  }
  if (nchar(s) > 80L) {
    s <- paste0(substr(s, 1L, 77L), "...")
  }
  return(s)
}


#' Copy one block's label keys from `new_lst` onto `old_lst`, matched by the
#' implementation variable.
#'
#' `.ds_diff_var_blocks()` matches `confounders` and `subgroups` the same way,
#' so the two agree on which pair to compare.
#'
#' @return `old_lst`, with the label keys replaced.
#' @noRd
.copy_by_var <- function(old_lst, new_lst, keys) {
  if (length(old_lst) == 0L || length(new_lst) == 0L) {
    return(old_lst)
  }
  old_v <- .ds_var_ids(old_lst)
  new_v <- .ds_var_ids(new_lst)
  for (i in seq_along(old_v)) {
    j <- match(old_v[i], new_v)
    if (is.na(j)) {
      next
    }
    for (k in keys) {
      if (!identical(old_lst[[i]][[k]], new_lst[[j]][[k]])) {
        old_lst[[i]][[k]] <- new_lst[[j]][[k]]
      }
    }
  }
  return(old_lst)
}


#' Copy one enrollment's label keys from `new_enr` onto `old_enr`, matched by
#' `id`.
#'
#' `.ds_diff_enrollments()` matches enrollments the same way, so the two agree
#' on which pair to compare. An enrollment that only one spec holds is a
#' structural change, so this function skips it.
#'
#' @return `old_enr`, with the label keys replaced.
#' @noRd
.copy_enrollment_labels <- function(old_enr, new_enr) {
  if (length(old_enr) == 0L || length(new_enr) == 0L) {
    return(old_enr)
  }
  enr_ids <- function(lst) {
    return(vapply(lst, function(e) as.character(e$id %||% NA), character(1)))
  }
  old_ids <- enr_ids(old_enr)
  new_ids <- enr_ids(new_enr)
  for (id in unique(old_ids)) {
    oi <- match(id, old_ids)
    ni <- match(id, new_ids)
    if (is.na(ni)) {
      next
    }
    e_old <- old_enr[[oi]]
    e_new <- new_enr[[ni]]
    if (!identical(e_old$name, e_new$name)) {
      e_old$name <- e_new$name
    }
    if (!identical(e_old$treatment$description, e_new$treatment$description)) {
      e_old$treatment$description <- e_new$treatment$description
    }
    for (k in c("intervention", "comparator")) {
      if (!identical(e_old$treatment$arms[[k]], e_new$treatment$arms[[k]])) {
        e_old$treatment$arms[[k]] <- e_new$treatment$arms[[k]]
      }
    }
    old_enr[[oi]] <- e_old
  }
  return(old_enr)
}


#' Copy the cosmetic fields of `new` onto `old` and return the result.
#'
#' `.diff_specs()` classifies every changed path as cosmetic or structural.
#' This function copies each path that classification calls cosmetic. It
#' copies no other path. A structural field keeps its old value, because the
#' cached results were computed against that value.
#'
#' The cosmetic paths are the ones `.diff_specs()` reports through `push_c()`:
#'
#' 1. `study$title`, `study$description`, `study$principal_investigator` and
#'    `study$design`
#' 2. `study$implementation$date` and `study$implementation$status`
#' 3. `outcomes[[i]]$name`, `$description` and `$role`
#' 4. `confounders[<var>]$name`, `$categories`, `$codes` and `$rationale`
#' 5. `subgroups[<var>]$name`
#' 6. `standing_methods`, `target_trial` and `open_questions`, each whole
#' 7. `enrollments[<id>]$name`
#' 8. `enrollments[<id>]$treatment$description`
#' 9. `enrollments[<id>]$treatment$arms$intervention` and `$comparator`
#'
#' It matches an outcome by position, a confounder and a subgroup by
#' implementation variable, and an enrollment by `id`. That is how
#' `.diff_specs()` matches them, so the two agree on which pair to compare. An
#' entry that only one spec holds is a structural change, so this function
#' skips it.
#'
#' @return `old`, with the cosmetic fields replaced.
#' @noRd
.copy_cosmetic_spec_fields <- function(old, new) {
  for (k in c("title", "description", "principal_investigator", "design")) {
    if (!identical(old$study[[k]], new$study[[k]])) {
      old$study[[k]] <- new$study[[k]]
    }
  }
  for (k in c("date", "status")) {
    a <- old$study$implementation[[k]]
    b <- new$study$implementation[[k]]
    if (!identical(a, b)) {
      old$study$implementation[[k]] <- b
    }
  }

  old_out <- old$outcomes %||% list()
  new_out <- new$outcomes %||% list()
  for (i in seq_len(min(length(old_out), length(new_out)))) {
    for (k in c("name", "description", "role")) {
      if (!identical(old_out[[i]][[k]], new_out[[i]][[k]])) {
        old$outcomes[[i]][[k]] <- new_out[[i]][[k]]
      }
    }
  }

  old$confounders <- .copy_by_var(
    old$confounders,
    new$confounders,
    c("name", "categories", "codes", "rationale")
  )
  old$subgroups <- .copy_by_var(old$subgroups, new$subgroups, "name")

  for (k in c("standing_methods", "target_trial", "open_questions")) {
    if (!identical(old[[k]], new[[k]])) {
      old[[k]] <- new[[k]]
    }
  }

  old$enrollments <- .copy_enrollment_labels(old$enrollments, new$enrollments)

  return(old)
}


#' Apply cosmetic spec updates in place on a TTEPlan object.
#'
#' Copies the cosmetic fields of `new_spec` into `plan$spec`, refreshes
#' `plan$ett$outcome_name` and recomputes `plan$ett$description`.
#'
#' It writes `plan$spec` only after it checks the copy. A copy that reaches a
#' structural field raises an error, and `plan$spec` keeps its old value.
#'
#' It writes nothing into `self$results_ett`. A description is input-derived,
#' `plan$ett` is where it lives, and the accessors join it from there. This
#' function used to mirror the new description onto each cached result as well.
#' A stale result then carried a current label, which is the exact staleness a
#' reader needs to see.
#'
#' A study that needs the label a result was computed under MUST store it under
#' its own immutable name. It MUST NOT overwrite one during a spec reload.
#'
#' @noRd
.apply_cosmetic_spec_updates <- function(plan, new_spec) {
  outcome_description <- outcome_role <- NULL # nolint
  spec_before <- plan$spec
  candidate <- .copy_cosmetic_spec_fields(spec_before, new_spec)

  # The check runs BEFORE anything reaches `plan`. `plan` is an R6 object and
  # carries reference semantics, so `stop()` unwinds the call stack and rolls
  # back no assignment. A check placed after the write would raise the right
  # error and leave the structural field changed. That is the state this
  # function exists to prevent.
  #
  # The check also covers a field that someone adds to `.diff_specs()` later.
  leaked <- .diff_specs(spec_before, candidate)$structural
  if (length(leaked) > 0L) {
    stop(
      "Spec reload changed structural field(s). The cached results were ",
      "computed against the old value, so this is a bug in swereg:\n  ",
      paste(leaked, collapse = "\n  "),
      call. = FALSE
    )
  }
  plan$spec <- candidate

  # Refresh outcome_name and outcome_description on plan$ett by re-joining
  # from the new outcomes block.
  outcome_lookup <- .spec_outcome_name_lookup(new_spec)
  if (!is.null(outcome_lookup) && !is.null(plan$ett) && nrow(plan$ett) > 0L) {
    ov <- as.character(plan$ett$outcome_var)
    new_names <- outcome_lookup$name[ov]
    new_desc <- outcome_lookup$description[ov]
    ok <- !is.na(new_names)
    if (any(ok)) {
      data.table::set(
        plan$ett,
        which(ok),
        "outcome_name",
        new_names[ok]
      )
    }
    if (!"outcome_description" %in% names(plan$ett)) {
      plan$ett[, outcome_description := NA_character_]
    }
    ok_desc <- !is.na(new_desc)
    if (any(ok_desc)) {
      data.table::set(
        plan$ett,
        which(ok_desc),
        "outcome_description",
        new_desc[ok_desc]
      )
    }
    if (!"outcome_role" %in% names(plan$ett)) {
      plan$ett[, outcome_role := NA_character_]
    }
    new_role <- outcome_lookup$role[ov]
    ok_role <- !is.na(new_role)
    if (any(ok_role)) {
      data.table::set(
        plan$ett,
        which(ok_role),
        "outcome_role",
        new_role[ok_role]
      )
    }
  }

  # Recompute ett$description from the refreshed outcome names.
  if (!is.null(plan$ett) && nrow(plan$ett) > 0L) {
    new_desc <- vapply(
      seq_len(nrow(plan$ett)),
      function(i) {
        r <- plan$ett[i]
        return(.format_ett_description(
          ett_id = r$ett_id,
          outcome_name = r$outcome_name,
          follow_up = r$follow_up,
          age_group = r$age_group
        ))
      },
      character(1)
    )
    data.table::set(plan$ett, j = "description", value = new_desc)
  }

  # The cached per-ETT description is NOT touched. `plan$ett` owns the label,
  # and a reader reaches it through `$get_estimates()`, which joins the current
  # one. Overwriting the cached copy hid the age of the result behind a fresh
  # label.

  return(invisible(plan))
}
