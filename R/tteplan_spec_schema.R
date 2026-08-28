# =============================================================================
# The specification schema
# =============================================================================
# One table names every legal path in a study specification. Nothing else in
# swereg holds that list.
#
# A path is written the way a walker over the parsed YAML produces it. The
# root is `$`. A mapping key appends `/<key>`. A sequence index becomes `[]`,
# so the third enrollment's treatment block is `$/enrollments[]/treatment`.
#
# The table is keyed by the mapping context, which is the path of the mapping
# that holds the key. Path keys are what separate the two `matching_ratio`
# keys. `$/standing_methods/matching_ratio_default` is accepted, and
# `$/enrollments[]/treatment/implementation/matching_ratio` is refused. A
# table keyed by key name alone cannot state that.
#
# Every context declares its own children. There is no generic rule for an
# `implementation` block. A treatment implementation, an outcome
# implementation and a confounder implementation accept different keys.
#
# Each key carries one of three classes.
#
#   consumed  swereg reads it.
#   metadata  swereg accepts it and never reads it. A spec MAY carry it.
#   legacy    swereg refuses it. A message names the replacement.
#
# The `metadata` class is what makes the whole table usable. Without it, a
# rule that refuses every key swereg does not read would refuse specifications
# that are correct today.

# -----------------------------------------------------------------------------
# Migration messages for the legacy keys
# -----------------------------------------------------------------------------
# The caller states the path. Each message states the repair for that path and
# nothing else.

# `matching_ratio` was the old name for the same number.
.TTE_SPEC_MSG_MATCHING_RATIO <- paste0(
  "That key is gone. Rename it to comparator_to_intervention_ratio. The ",
  "number is unchanged: the draw takes that many times a trial's count of ",
  "intervention individuals. swereg draws comparators by incidence density ",
  "sampling within each sequential trial, and builds no matched set. The old ",
  "name named a scheme swereg does not use."
)

# `inclusion_criteria$additional_inclusion` copied the per-enrollment key name
# to the global container. swereg never read it there.
.TTE_SPEC_MSG_ADDITIONAL_INCLUSION <- paste0(
  "That key is gone. swereg never read it, so it never restricted the study ",
  "population. Move each entry to inclusion_criteria$criteria. Each ",
  "criterion there declares name, type: has_event, and ",
  "implementation$source_variable."
)

# A criterion written as direct children of `inclusion_criteria`, with no
# `criteria` list around it.
.TTE_SPEC_MSG_FLAT_CRITERION <- paste0(
  "A criterion's fields do not sit directly under inclusion_criteria. That ",
  "container holds isoyears and criteria, and nothing else. Move name, ",
  "rationale and implementation into one entry of inclusion_criteria",
  "$criteria. Each criterion there declares type: has_event."
)


#' Declare a set of legacy keys that share one migration message
#'
#' @param keys Character vector of key names.
#' @param msg Character scalar, the migration message.
#' @return A named character vector: names are the keys, values are `msg`.
#' @noRd
.tte_spec_legacy <- function(keys, msg) {
  out <- rep(msg, length(keys))
  names(out) <- keys
  return(out)
}


# -----------------------------------------------------------------------------
# The table
# -----------------------------------------------------------------------------
# Each element is one mapping context. Each context declares `consumed`,
# `metadata` and `legacy` children. `legacy` is a named character vector, so a
# context can hold two groups of legacy keys with different messages.

.TTE_SPEC_SCHEMA <- list(
  "$" = list(
    consumed = c(
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
  ),

  # --- confounders -----------------------------------------------------------
  "$/confounders[]" = list(
    consumed = c(
      "categories",
      "codes",
      "implementation",
      "name",
      "rationale"
    )
  ),
  "$/confounders[]/implementation" = list(
    consumed = c("computed", "source_variable", "variable", "window")
  ),

  # --- enrollments -----------------------------------------------------------
  "$/enrollments[]" = list(
    consumed = c(
      "additional_exclusion",
      "additional_inclusion",
      "comparator_tolerance_weeks",
      "id",
      "intervention_tolerance_weeks",
      "name",
      "observed_var",
      "treatment"
    )
  ),
  "$/enrollments[]/additional_exclusion[]" = list(
    consumed = c("implementation", "name", "rationale")
  ),
  "$/enrollments[]/additional_exclusion[]/implementation" = list(
    consumed = c(
      "computed",
      "intervention_value",
      "source_variable",
      "type",
      "window"
    )
  ),
  "$/enrollments[]/additional_inclusion[]" = list(
    consumed = c("implementation", "max", "min", "name", "type")
  ),
  "$/enrollments[]/additional_inclusion[]/implementation" = list(
    consumed = c("source_variable", "variable", "window")
  ),
  "$/enrollments[]/observed_var" = list(
    consumed = c("column", "sentinel")
  ),
  "$/enrollments[]/treatment" = list(
    consumed = c("arms", "description", "implementation")
  ),
  "$/enrollments[]/treatment/arms" = list(
    consumed = c("comparator", "intervention")
  ),
  "$/enrollments[]/treatment/implementation" = list(
    consumed = c(
      "comparator_to_intervention_ratio",
      "comparator_value",
      "intervention_value",
      "seed",
      "variable"
    ),
    legacy = .tte_spec_legacy(
      "matching_ratio",
      .TTE_SPEC_MSG_MATCHING_RATIO
    )
  ),

  # --- exclusion criteria ----------------------------------------------------
  "$/exclusion_criteria[]" = list(
    consumed = c("implementation", "name", "rationale")
  ),
  "$/exclusion_criteria[]/implementation" = list(
    consumed = c(
      "computed",
      "intervention_value",
      "source_variable",
      "type",
      "window"
    )
  ),

  # --- follow-up -------------------------------------------------------------
  "$/follow_up[]" = list(
    consumed = c("label", "weeks")
  ),

  # --- inclusion criteria ----------------------------------------------------
  # `inclusion_criteria` is a fixed container. It holds `isoyears` and
  # `criteria`, and nothing else. The two legacy groups are the two shapes a
  # collaborator wrote instead, and swereg read neither.
  "$/inclusion_criteria" = list(
    consumed = c("criteria", "isoyears"),
    legacy = c(
      .tte_spec_legacy(
        "additional_inclusion",
        .TTE_SPEC_MSG_ADDITIONAL_INCLUSION
      ),
      .tte_spec_legacy(
        c("implementation", "name", "rationale"),
        .TTE_SPEC_MSG_FLAT_CRITERION
      )
    )
  ),
  "$/inclusion_criteria/criteria[]" = list(
    consumed = c("implementation", "name", "rationale", "type")
  ),
  "$/inclusion_criteria/criteria[]/implementation" = list(
    consumed = c("computed", "source_variable", "window")
  ),
  "$/inclusion_criteria/additional_inclusion[]" = list(
    legacy = .tte_spec_legacy(
      c("implementation", "name", "rationale", "type"),
      .TTE_SPEC_MSG_ADDITIONAL_INCLUSION
    )
  ),
  "$/inclusion_criteria/additional_inclusion[]/implementation" = list(
    legacy = .tte_spec_legacy(
      c("computed", "source_variable", "window"),
      .TTE_SPEC_MSG_ADDITIONAL_INCLUSION
    )
  ),
  "$/inclusion_criteria/implementation" = list(
    legacy = .tte_spec_legacy(
      c("computed", "source_variable", "window"),
      .TTE_SPEC_MSG_FLAT_CRITERION
    )
  ),

  # --- open questions --------------------------------------------------------
  "$/open_questions[]" = list(
    consumed = c("question", "raised_by", "status"),
    metadata = "resolution"
  ),

  # --- outcomes --------------------------------------------------------------
  "$/outcomes[]" = list(
    consumed = c("description", "implementation", "name", "role")
  ),
  "$/outcomes[]/implementation" = list(
    consumed = "variable"
  ),

  # --- standing methods ------------------------------------------------------
  # `matching_ratio_default` is accepted here and refused under a treatment
  # implementation. The path is what tells them apart.
  #
  # `calendar_time` is the one block swereg reads.
  # `R/tteplan_spec_summary_sheet.R:590` reads it, then reads its `handling`
  # and `note`. Nothing in `R/` reads `admin_censoring` or
  # `comparator_to_intervention_ratio_default`, so those two blocks are
  # metadata. A specification MAY carry them, and swereg never acts on them.
  "$/standing_methods" = list(
    consumed = "calendar_time",
    metadata = c(
      "admin_censoring",
      "comparator_to_intervention_ratio_default",
      "matching_ratio_default"
    )
  ),
  "$/standing_methods/admin_censoring" = list(
    metadata = c("handling", "note")
  ),
  "$/standing_methods/calendar_time" = list(
    consumed = c("handling", "note")
  ),
  "$/standing_methods/comparator_to_intervention_ratio_default" = list(
    metadata = c("handling", "note")
  ),
  "$/standing_methods/matching_ratio_default" = list(
    metadata = c("handling", "note")
  ),

  # --- study -----------------------------------------------------------------
  "$/study" = list(
    consumed = c(
      "description",
      "design",
      "implementation",
      "principal_investigator",
      "title"
    )
  ),
  "$/study/implementation" = list(
    consumed = c(
      "conf_level",
      "date",
      "project_prefix",
      "status",
      "version"
    )
  ),

  # --- subgroups -------------------------------------------------------------
  "$/subgroups[]" = list(
    consumed = c("implementation", "name")
  ),
  "$/subgroups[]/implementation" = list(
    consumed = "variable"
  ),

  # --- target trial ----------------------------------------------------------
  "$/target_trial" = list(
    consumed = c(
      "analysis_plan",
      "assignment_procedure",
      "causal_contrast",
      "eligibility_criteria",
      "follow_up_period",
      "outcome",
      "treatment_strategies"
    )
  ),
  "$/target_trial/analysis_plan" = list(consumed = "specification"),
  "$/target_trial/assignment_procedure" = list(consumed = "specification"),
  "$/target_trial/causal_contrast" = list(consumed = "specification"),
  "$/target_trial/eligibility_criteria" = list(consumed = "specification"),
  "$/target_trial/follow_up_period" = list(consumed = "specification"),
  "$/target_trial/outcome" = list(consumed = "specification"),
  "$/target_trial/treatment_strategies" = list(consumed = "specification")
)


# -----------------------------------------------------------------------------
# Accessors
# -----------------------------------------------------------------------------

#' Split a key path into its mapping context and its key
#'
#' @param path Character scalar, a normalised key path.
#' @return A character vector of length 2: the context, then the key.
#' @noRd
.tte_spec_split_path <- function(path) {
  return(c(sub("/[^/]*$", "", path), sub("^.*/", "", path)))
}


#' Classify one key path
#'
#' @param path Character scalar, a normalised key path.
#' @return One of "consumed", "metadata", "legacy", or `NA_character_` when
#'   the schema does not declare the path.
#' @noRd
.tte_spec_key_class_one <- function(path) {
  parts <- .tte_spec_split_path(path)
  node <- .TTE_SPEC_SCHEMA[[parts[1]]]
  if (is.null(node)) {
    return(NA_character_)
  }
  key <- parts[2]
  if (key %in% node[["consumed"]]) {
    return("consumed")
  }
  if (key %in% node[["metadata"]]) {
    return("metadata")
  }
  if (key %in% names(node[["legacy"]])) {
    return("legacy")
  }
  return(NA_character_)
}


#' Classify key paths against the schema
#'
#' @param path Character vector of normalised key paths.
#' @return A character vector the same length as `path`. Each element is
#'   "consumed", "metadata", "legacy", or `NA_character_` for a path the
#'   schema does not declare.
#' @noRd
.tte_spec_key_class <- function(path) {
  return(vapply(
    path,
    .tte_spec_key_class_one,
    character(1),
    USE.NAMES = FALSE
  ))
}


#' Read the migration message for a legacy key path
#'
#' @param path Character scalar, a normalised key path.
#' @return The migration message, or `NA_character_` when the path is not
#'   legacy.
#' @noRd
.tte_spec_legacy_message <- function(path) {
  parts <- .tte_spec_split_path(path)
  legacy <- .TTE_SPEC_SCHEMA[[parts[1]]][["legacy"]]
  if (!parts[2] %in% names(legacy)) {
    return(NA_character_)
  }
  return(unname(legacy[[parts[2]]]))
}


#' List every key path the schema declares
#'
#' @param class Optional character scalar: "consumed", "metadata" or
#'   "legacy". `NULL` returns every declared path.
#' @return A sorted character vector of normalised key paths.
#' @noRd
.tte_spec_paths <- function(class = NULL) {
  out <- character(0)
  for (context in names(.TTE_SPEC_SCHEMA)) {
    node <- .TTE_SPEC_SCHEMA[[context]]
    keys <- list(
      consumed = node[["consumed"]],
      metadata = node[["metadata"]],
      legacy = names(node[["legacy"]])
    )
    if (!is.null(class)) {
      keys <- keys[class]
    }
    # `recycle0 = TRUE` so a context with no key of this class contributes
    # nothing. Without it `paste0()` drops the empty vector and returns the
    # context with a trailing slash.
    out <- c(out, paste0(context, "/", unlist(keys), recycle0 = TRUE))
  }
  return(sort(unique(out)))
}
