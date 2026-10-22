# =============================================================================
# Target trial protocol sheet (Dickerman et al., Table S1 layout)
# =============================================================================
#
# Three columns: "Protocol component", "Target trial specification",
# "Target trial emulation", and seven rows in the Dickerman order.
#
# The two columns have deliberately different provenance.
#
#   Target trial specification -- the CLINICAL fields the spec already carries
#     (criterion names, arm labels, outcome names, follow-up labels), plus the
#     study team's own prose from the spec's `target_trial:` key. It never
#     reads an `implementation:` block.
#
#   Target trial emulation -- RENDERED from the nested `implementation:`
#     blocks that the rest of the spec already carries. It is never read from
#     YAML prose, and there is deliberately no `emulation:` key under
#     `target_trial:` for it to read. A hand-written emulation column can drift
#     away from the pipeline it claims to describe; a rendered one cannot.
#     The rules come from those blocks, and the words come from the spec's
#     `name` fields and arm labels. A journal table then prints no column
#     name that the spec names. A variable without a `name` keeps its column
#     name.
#
# A spec materialises many ETTs (enrollments x outcomes x follow-up horizons),
# and one protocol table cannot stand for all of them. The sheet therefore
# names the single ETT it documents in its title row.

#' Protocol components, in Dickerman Table S1 order
#'
#' @return A list of seven `list(key = , label = )` pairs. `key` indexes the
#'   spec's `target_trial:` section; `label` is the rendered row label.
#' @noRd
.tte_protocol_components <- function() {
  return(list(
    list(key = "eligibility_criteria", label = "Eligibility criteria"),
    list(key = "treatment_strategies", label = "Treatment strategies"),
    list(key = "assignment_procedure", label = "Assignment procedure"),
    list(key = "outcome", label = "Outcome"),
    list(key = "follow_up_period", label = "Follow-up period"),
    list(key = "causal_contrast", label = "Causal contrast"),
    list(key = "analysis_plan", label = "Analysis plan")
  ))
}


#' Join rendered fragments into one worksheet cell
#'
#' Drops `NULL`, `NA` and empty fragments, then joins with newlines.
#'
#' @param x Character vector or list of fragments.
#' @return A length-1 character string, possibly `""`.
#' @noRd
.protocol_lines <- function(x) {
  x <- unlist(x, use.names = FALSE)
  if (length(x) == 0L) {
    return("")
  }
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[nzchar(x)]
  if (length(x) == 0L) {
    return("")
  }
  return(paste(x, collapse = "\n"))
}


#' Render one implementation block's variable name
#'
#' Confounders and outcomes carry `variable`; criteria carry
#' `source_variable`. Both gain a `*_combined` sibling when the spec lists
#' several sources for one concept, and the combined name is the column the
#' pipeline actually builds, so it wins.
#'
#' @param impl An `implementation:` list, or `NULL`.
#' @return A length-1 character string, or `NA_character_`.
#' @noRd
.protocol_impl_variable <- function(impl) {
  if (!is.list(impl)) {
    return(NA_character_)
  }
  v <- impl[["variable_combined"]] %||%
    impl[["variable"]] %||%
    impl[["source_variable_combined"]] %||%
    impl[["source_variable"]]
  if (is.null(v)) {
    return(NA_character_)
  }
  return(paste(as.character(v), collapse = "__"))
}


#' The spec's display name of each confounder and outcome variable
#'
#' The emulation column names a variable by the `name` of its spec entry. A
#' reader of the table then meets no column name that the spec names.
#' Confounders come first, and the first entry that names a variable wins.
#'
#' @param spec Parsed spec list.
#' @return A named character vector. The names are the column names that
#'   `.protocol_impl_variable()` renders, and the values are the spec names.
#' @noRd
.protocol_variable_names <- function(spec) {
  out <- character()
  for (entry in c(spec[["confounders"]], spec[["outcomes"]])) {
    v <- .protocol_impl_variable(entry[["implementation"]])
    nm <- .protocol_entry_name(entry)
    if (!is.na(v) && !is.null(nm) && !(v %in% names(out))) {
      out[[v]] <- nm
    }
  }
  return(out)
}


#' The `name` of one spec entry, or `NULL` when it has none
#'
#' @param entry A spec list entry.
#' @return A length-1 character string, or `NULL`.
#' @noRd
.protocol_entry_name <- function(entry) {
  nm <- entry[["name"]]
  if (!is.character(nm) || length(nm) != 1L || is.na(nm) || !nzchar(nm)) {
    return(NULL)
  }
  return(nm)
}


#' Display text for one variable: its spec name, or its column name
#'
#' @param code A column name, from `.protocol_impl_variable()`.
#' @param var_names A lookup, from `.protocol_variable_names()`.
#' @return A length-1 character string. A variable that the spec does not
#'   name keeps its column name.
#' @noRd
.protocol_variable_label <- function(code, var_names) {
  if (length(code) == 1L && !is.na(code) && code %in% names(var_names)) {
    return(unname(var_names[[code]]))
  }
  return(code)
}


#' Display text for one value of a variable
#'
#' A value of the treatment variable that equals the intervention or the
#' comparator value is an exposure level, and the spec labels it under
#' `treatment$arms`. Every other value keeps its spec text.
#'
#' @param tx The enrollment's `treatment` list, or `NULL`.
#' @param variable The column name the value belongs to.
#' @param value The value, as the spec writes it.
#' @return A length-1 character string.
#' @noRd
.protocol_level_label <- function(tx, variable, value) {
  impl <- tx[["implementation"]]
  if (
    length(value) == 1L &&
      identical(variable, .protocol_impl_variable(impl))
  ) {
    for (arm in c("intervention", "comparator")) {
      level <- impl[[paste0(arm, "_value")]]
      label <- .protocol_entry_name(list(name = tx[["arms"]][[arm]]))
      if (
        !is.null(level) &&
          !is.null(label) &&
          identical(as.character(level), as.character(value))
      ) {
        return(label)
      }
    }
  }
  return(.protocol_value(value))
}


#' Display text for one arm's exposure level
#'
#' @param tx The enrollment's `treatment` list, or `NULL`.
#' @param arm `"intervention"` or `"comparator"`.
#' @return The arm label under `treatment$arms`, or the arm's value when the
#'   spec gives no label.
#' @noRd
.protocol_arm_label <- function(tx, arm) {
  impl <- tx[["implementation"]]
  return(.protocol_level_label(
    tx,
    .protocol_impl_variable(impl),
    impl[[paste0(arm, "_value")]]
  ))
}


#' The rule of one washout entry, in display names
#'
#' The wording is that of `.tte_washout_prose()`, with the variable and the
#' value in display names. A named entry leads with its name.
#'
#' @param entry An inclusion or exclusion entry of a washout type.
#' @param var_names A lookup, from `.protocol_variable_names()`.
#' @param tx The enrollment's `treatment` list, or `NULL`.
#' @return A length-1 character string, or `NULL` when the entry is not a
#'   washout.
#' @noRd
.protocol_washout_rule <- function(entry, var_names, tx) {
  impl <- entry[["implementation"]]
  if (!isTRUE(impl[["type"]] %in% .TTE_WASHOUT_TYPES)) {
    return(NULL)
  }
  sv <- impl[["source_variable_combined"]] %||%
    paste(as.character(unlist(impl[["source_variable"]])), collapse = "__")
  relation <- if (identical(impl[["type"]], "only_prior_value")) {
    " other than "
  } else {
    " equal to "
  }
  nm <- .protocol_entry_name(entry)
  return(paste0(
    if (is.null(nm)) "No prior " else paste0(nm, ": no prior "),
    .protocol_variable_label(sv, var_names),
    relation,
    .protocol_level_label(tx, sv, impl[["value"]])
  ))
}


#' Display text for the criterion of one inclusion or exclusion entry
#'
#' @param entry An inclusion or exclusion entry.
#' @param var_names A lookup, from `.protocol_variable_names()`.
#' @return The entry's `name`. An entry with no name gives its variable, in
#'   display text.
#' @noRd
.protocol_criterion_label <- function(entry, var_names) {
  return(
    .protocol_entry_name(entry) %||%
      .protocol_variable_label(
        .protocol_impl_variable(entry[["implementation"]]),
        var_names
      )
  )
}


#' Render a scalar spec value for display
#'
#' @param x Any length-1 atomic, or `NULL`.
#' @return A length-1 character string.
#' @noRd
.protocol_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("(not specified)")
  }
  return(paste(as.character(x), collapse = ", "))
}


#' Render the stratum of the comparator draw
#'
#' The draw runs inside one `enrollment_period_id` group, and
#' `enrollment_period_id` is the week index divided by `period_width`. The
#' stratum is therefore the enrollment period,
#' and the enrollment period is the only stratum. A width of 1 makes the
#' enrollment period one week.
#'
#' @param period_width Integer width of the enrollment period in weeks, or
#'   `NULL`.
#' @return A length-1 character string.
#' @noRd
.protocol_draw_stratum <- function(period_width) {
  if (is.null(period_width) || length(period_width) == 0L) {
    return("Comparator draw stratum: the enrollment period, and nothing else")
  }
  pw <- as.integer(period_width)[1]
  if (is.na(pw) || pw <= 1L) {
    return("Comparator draw stratum: the entry week, and nothing else")
  }
  return(paste0(
    "Comparator draw stratum: the ",
    pw,
    "-week enrollment period, and nothing else"
  ))
}


#' Resolve the one ETT a protocol sheet documents
#'
#' @param plan A [TTEPlan] (or any object exposing `$spec`, `$ett` and
#'   `$period_width`).
#' @param ett_id Character ETT id. `NULL` takes the first ETT in the grid.
#' @return A list carrying the spec's enrollment and outcome entries plus the
#'   labels the sheet title needs.
#' @noRd
.protocol_context <- function(plan, ett_id = NULL) {
  spec <- plan$spec
  if (is.null(spec)) {
    stop(
      "plan has no spec; cannot build the target trial protocol table",
      call. = FALSE
    )
  }
  ett <- plan$ett
  if (is.null(ett) || nrow(ett) == 0L) {
    stop(
      "plan has no ETTs; cannot build the target trial protocol table",
      call. = FALSE
    )
  }
  # Resolve the row outside `[.data.table` on purpose: inside `i` the name
  # `ett_id` would bind to the COLUMN, not to this argument, and every row
  # would match.
  idx <- if (is.null(ett_id)) {
    1L
  } else {
    which(as.character(ett$ett_id) == as.character(ett_id))[1]
  }
  if (is.na(idx)) {
    stop("unknown ett_id: ", ett_id, call. = FALSE)
  }
  row <- ett[idx]

  eid <- as.character(row$enrollment_id)
  enrollment <- NULL
  for (enr in spec[["enrollments"]]) {
    if (identical(as.character(enr[["id"]]), eid)) {
      enrollment <- enr
      break
    }
  }

  outcome_var <- as.character(row$outcome_var)
  outcome <- NULL
  for (out in spec[["outcomes"]]) {
    if (
      identical(.protocol_impl_variable(out[["implementation"]]), outcome_var)
    ) {
      outcome <- out
      break
    }
  }

  weeks <- row$follow_up
  fu_label <- ""
  for (fu in spec[["follow_up"]]) {
    if (isTRUE(all.equal(as.numeric(fu[["weeks"]]), as.numeric(weeks)))) {
      fu_label <- as.character(fu[["label"]] %||% "")
      break
    }
  }

  return(list(
    ett_id = as.character(row$ett_id),
    enrollment_id = eid,
    enrollment = enrollment,
    enrollment_label = .enrollment_label(plan, eid),
    outcome = outcome,
    outcome_var = outcome_var,
    outcome_name = as.character(row$outcome_name),
    follow_up_weeks = weeks,
    follow_up_label = fu_label,
    # The assignment row names the stratum of the comparator draw, and the
    # stratum is the enrollment period. Its width comes from the plan.
    period_width = plan$period_width,
    # The rare-condition rule: its lines in the three cells, and the
    # conditions the frozen decision excluded. All are empty without a rule.
    sparse_specification = .tte_sparse_protocol_lines(plan, "specification"),
    sparse_emulation = .tte_sparse_protocol_lines(plan, "emulation"),
    sparse_analysis = .tte_sparse_protocol_lines(plan, "analysis"),
    sparse_excluded_vars = .tte_sparse_report(plan)$excluded_vars
  ))
}


#' Render the "Target trial specification" cell for one component
#'
#' Clinical fields only, plus the study team's `target_trial:` prose. Two
#' components (causal contrast, analysis plan) have no clinical fields in the
#' spec at all, so their cell is the `target_trial:` entry alone.
#'
#' @param spec Parsed spec list.
#' @param key Component key, from `.tte_protocol_components()`.
#' @param ctx Context list, from `.protocol_context()`.
#' @return A length-1 character string.
#' @noRd
.protocol_specification <- function(spec, key, ctx) {
  enrollment <- ctx$enrollment
  outcome <- ctx$outcome
  derived <- switch(
    key,
    eligibility_criteria = {
      out <- character()
      iso <- spec[["inclusion_criteria"]][["isoyears"]]
      if (!is.null(iso)) {
        out <- c(
          out,
          paste0("Study period: ISO years ", paste(iso, collapse = " to "))
        )
      }
      # `inclusion_criteria` is a fixed container: the `isoyears` pair,
      # handled above, and a `criteria` list. Every entry in `criteria`
      # restricts the cohort of every enrollment, so it belongs in the
      # eligibility cell of every protocol sheet.
      for (ic in spec[["inclusion_criteria"]][["criteria"]] %||% list()) {
        out <- c(out, paste0("Include: ", .protocol_value(ic[["name"]])))
      }
      for (ec in spec[["exclusion_criteria"]]) {
        out <- c(out, paste0("Exclude: ", .protocol_value(ec[["name"]])))
      }
      for (ai in enrollment[["additional_inclusion"]]) {
        if (identical(ai[["type"]], "age_range")) {
          out <- c(
            out,
            paste0(
              "Include (enrollment ",
              ctx$enrollment_id,
              "): age ",
              .protocol_value(ai[["min"]]),
              " to ",
              .protocol_value(ai[["max"]])
            )
          )
        } else if (identical(ai[["type"]], "isoyear_range")) {
          out <- c(
            out,
            paste0(
              "Include (enrollment ",
              ctx$enrollment_id,
              "): ISO years ",
              .protocol_value(ai[["min"]]),
              " to ",
              .protocol_value(ai[["max"]])
            )
          )
        } else {
          # A washout with no name of its own still states its rule.
          out <- c(
            out,
            paste0(
              "Include (enrollment ",
              ctx$enrollment_id,
              "): ",
              .protocol_value(
                ai[["name"]] %||% .tte_washout_prose(ai[["implementation"]])
              )
            )
          )
        }
      }
      for (ae in enrollment[["additional_exclusion"]]) {
        out <- c(
          out,
          paste0(
            "Exclude (enrollment ",
            ctx$enrollment_id,
            "): ",
            .protocol_value(ae[["name"]])
          )
        )
      }
      c(out, ctx$sparse_specification)
    },
    treatment_strategies = {
      tx <- enrollment[["treatment"]]
      arms <- tx[["arms"]]
      c(
        tx[["description"]],
        if (!is.null(arms)) {
          paste0("Intervention arm: ", .protocol_value(arms[["intervention"]]))
        },
        if (!is.null(arms)) {
          paste0("Comparator arm: ", .protocol_value(arms[["comparator"]]))
        }
      )
    },
    assignment_procedure = {
      arms <- enrollment[["treatment"]][["arms"]]
      if (is.null(arms)) {
        character()
      } else {
        paste0(
          "Arms compared: ",
          .protocol_value(arms[["intervention"]]),
          " versus ",
          .protocol_value(arms[["comparator"]])
        )
      }
    },
    outcome = c(
      paste0("Outcome: ", .protocol_value(outcome[["name"]])),
      if (!is.null(outcome[["role"]])) {
        paste0("Role: ", .protocol_value(outcome[["role"]]))
      },
      outcome[["description"]]
    ),
    follow_up_period = {
      labels <- vapply(
        spec[["follow_up"]],
        function(f) {
          return(as.character(f[["label"]] %||% paste0(f[["weeks"]], " weeks")))
        },
        character(1)
      )
      c(
        "Start: time zero, which the emulation column defines",
        paste0("Horizons in the spec: ", paste(labels, collapse = ", ")),
        paste0(
          "This sheet documents the ",
          if (nzchar(ctx$follow_up_label)) {
            ctx$follow_up_label
          } else {
            paste0(ctx$follow_up_weeks, " week")
          },
          " horizon"
        )
      )
    },
    character()
  )
  authored <- spec[["target_trial"]][[key]][["specification"]]
  return(.protocol_lines(c(derived, authored)))
}


#' Render the eligibility criteria lines of the "Target trial emulation" cell
#'
#' @param spec Parsed spec list.
#' @param ctx Context list, from `.protocol_context()`.
#' @param var_names A lookup, from `.protocol_variable_names()`.
#' @return A character vector, one element per line.
#' @noRd
.protocol_emulation_eligibility <- function(spec, ctx, var_names) {
  enrollment <- ctx$enrollment
  tx <- enrollment[["treatment"]]
  out <- character()
  iso <- spec[["inclusion_criteria"]][["isoyears"]]
  if (!is.null(iso)) {
    out <- c(
      out,
      paste0("Require isoyear in ", paste(iso, collapse = " to "))
    )
  }
  # Symmetric partner to the exclusion loop below. It reads the
  # `criteria` list of the `inclusion_criteria` container, and each entry
  # there generates one eligibility column on every enrollment.
  for (ic in spec[["inclusion_criteria"]][["criteria"]] %||% list()) {
    out <- c(
      out,
      paste0(
        "Require ",
        .protocol_criterion_label(ic, var_names),
        " (",
        .tte_inclusion_window_human(ic[["implementation"]]),
        ")"
      )
    )
  }
  # A washout of type `no_prior_value` or `only_prior_value` states its
  # own rule. Every other exclusion drops rows where its column is TRUE: a
  # global exclusion and an enrollment exclusion alike, as
  # `tteplan_apply_exclusions()` treats the two.
  exclude <- function(ec) {
    return(paste0(
      .protocol_washout_rule(ec, var_names, tx) %||%
        paste0(
          "Drop rows where ",
          .protocol_criterion_label(ec, var_names),
          " is TRUE"
        ),
      " (",
      .format_window_human(ec[["implementation"]]),
      ")"
    ))
  }
  for (ec in spec[["exclusion_criteria"]]) {
    out <- c(out, exclude(ec))
  }
  for (ai in enrollment[["additional_inclusion"]]) {
    impl <- ai[["implementation"]]
    if (identical(ai[["type"]], "age_range")) {
      # The entry's own name is the band, so the line names the variable.
      out <- c(
        out,
        paste0(
          "Require ",
          .protocol_variable_label(.protocol_impl_variable(impl), var_names),
          " between ",
          .protocol_value(ai[["min"]]),
          " and ",
          .protocol_value(ai[["max"]])
        )
      )
    } else if (identical(ai[["type"]], "isoyear_range")) {
      # The entry names no variable. It reads `isoyear`, as the global
      # range does.
      out <- c(
        out,
        paste0(
          "Require isoyear in ",
          .protocol_value(ai[["min"]]),
          " to ",
          .protocol_value(ai[["max"]])
        )
      )
    } else {
      out <- c(
        out,
        paste0(
          .protocol_washout_rule(ai, var_names, tx) %||%
            paste0("Require ", .protocol_criterion_label(ai, var_names)),
          " (",
          .format_window_human(impl),
          ")"
        )
      )
    }
  }
  for (ae in enrollment[["additional_exclusion"]]) {
    out <- c(out, exclude(ae))
  }
  return(c(out, ctx$sparse_emulation))
}


#' Render the "Target trial emulation" cell for one component
#'
#' The rules come from the nested `implementation:` blocks, or from the ETT
#' grid that those blocks generated. Nothing is read from `target_trial:`.
#' Each variable, criterion and exposure level shows as the spec's `name` or
#' arm label. A variable that the spec does not name shows as its column
#' name.
#'
#' @param spec Parsed spec list.
#' @param key Component key, from `.tte_protocol_components()`.
#' @param ctx Context list, from `.protocol_context()`.
#' @return A length-1 character string.
#' @noRd
.protocol_emulation <- function(spec, key, ctx) {
  enrollment <- ctx$enrollment
  var_names <- .protocol_variable_names(spec)
  tx <- enrollment[["treatment"]]
  tx_impl <- tx[["implementation"]]
  tx_label <- .protocol_variable_label(
    .protocol_impl_variable(tx_impl),
    var_names
  )
  intervention <- .protocol_arm_label(tx, "intervention")
  comparator <- .protocol_arm_label(tx, "comparator")
  fragments <- switch(
    key,
    eligibility_criteria = .protocol_emulation_eligibility(
      spec,
      ctx,
      var_names
    ),
    treatment_strategies = c(
      paste0("Treatment variable: ", tx_label),
      paste0("Intervention value: ", intervention),
      paste0("Comparator value: ", comparator)
    ),
    assignment_procedure = c(
      "Comparator draw: incidence density sampling within each sequential trial",
      paste0(
        "Comparator-to-intervention ratio: ",
        .protocol_value(tx_impl[["comparator_to_intervention_ratio"]]),
        ":1"
      ),
      paste0(
        "Comparator draw size: ",
        .protocol_value(tx_impl[["comparator_to_intervention_ratio"]]),
        " times that trial's count of intervention individuals, capped at the comparators that trial holds"
      ),
      paste0(
        "Comparator draw seed: ",
        .protocol_value(tx_impl[["seed"]])
      ),
      .protocol_draw_stratum(ctx[["period_width"]]),
      "Comparator pairing: none, so no matched set exists and no later step conditions on one",
      "Confounding adjustment: inverse probability weighting on the covariates taken at the recruiting week",
      paste0("Treatment variable: ", tx_label),
      paste0("Arm values: ", intervention, " versus ", comparator)
    ),
    # `outcome_var` is the ETT's outcome column, and `.protocol_context()`
    # matched the outcome entry on it. One line names both, and it keeps the
    # column when no spec entry matched.
    outcome = paste0(
      "Outcome variable: ",
      .protocol_variable_label(ctx$outcome_var, var_names)
    ),
    follow_up_period = {
      weeks <- vapply(
        spec[["follow_up"]],
        function(f) as.numeric(f[["weeks"]]),
        numeric(1)
      )
      c(
        .TTE_TIME_ZERO_DEFINITION,
        paste0(
          "Follow-up stops at the earliest of the outcome event, loss to ",
          "follow-up, the administrative end of the data and the horizon. ",
          "Per-protocol follow-up also stops at protocol deviation"
        ),
        paste0("Horizon on this sheet: ", ctx$follow_up_weeks, " weeks"),
        paste0(
          "Horizons in the ETT grid: ",
          paste(weeks, collapse = ", "),
          " weeks"
        )
      )
    },
    causal_contrast = c(
      # `.TTE_ESTIMANDS` is the set s2 and s3 build, so the cell cannot name
      # an estimand the pipeline does not estimate.
      paste0("Estimands: ", paste(.TTE_ESTIMANDS, collapse = ", ")),
      paste0("ETT: ", ctx$ett_id),
      paste0("Treatment variable: ", tx_label),
      paste0("Intervention value: ", intervention),
      paste0("Comparator value: ", comparator),
      paste0(
        "Outcome variable: ",
        .protocol_variable_label(ctx$outcome_var, var_names)
      ),
      paste0("Horizon: ", ctx$follow_up_weeks, " weeks")
    ),
    analysis_plan = {
      # The fixed lines state what s1 to s3 fit. Item 6h of the TARGET
      # checklist states the same model in full. The confounder lines sit
      # between the weight lines and the outcome model line, and each names
      # the models it enters: the outcome model holds no confounder.
      # A condition the rare-condition rule excluded is not adjusted for.
      adjust <- character()
      for (conf in spec[["confounders"]]) {
        impl <- conf[["implementation"]]
        v <- .protocol_impl_variable(impl)
        if (v %in% ctx$sparse_excluded_vars) {
          next
        }
        adjust <- c(
          adjust,
          paste0(
            "Treatment and censoring models adjust for ",
            .protocol_variable_label(v, var_names),
            if (isTRUE(impl[["computed"]])) {
              paste0(" (", .format_window_human(impl), ")")
            }
          )
        )
      }
      c(
        paste0(
          "Treatment weights: stabilized inverse probability weights from a ",
          "logistic model fitted on the baseline rows"
        ),
        paste0(
          "Censoring weights (per-protocol): inverse probability of ",
          "censoring weights from three models in each arm, one each for ",
          "loss to follow-up, protocol deviation and protocol deviation at ",
          "time zero"
        ),
        adjust,
        ctx$sparse_analysis,
        paste0(
          "Truncation: the intention-to-treat weight, and the per-protocol ",
          "product of the treatment and censoring weights, at the 1st and ",
          "99th percentiles"
        ),
        paste0(
          "Outcome model: survey-weighted quasi-Poisson regression on the ",
          "treatment, the time since time zero and the trial index, with a ",
          "person-time offset and person-level clustered standard errors. ",
          "It holds no confounder"
        ),
        paste0(
          "Absolute scale: cause-specific risk difference, with a percentile ",
          "interval from ",
          .S3_RD_N_BOOT,
          " bootstrap replicates that resample persons"
        ),
        .protocol_horizon_rule_line(spec)
      )
    },
    character()
  )
  return(.protocol_lines(fragments))
}


#' The horizon rule, as one line of the Analysis plan cell
#'
#' @param spec Parsed spec list. It MUST carry `horizon_rule`.
#' @return A character(1). With `min_expected_events` 0, the line says that
#'   the rule withholds no estimate.
#' @noRd
.protocol_horizon_rule_line <- function(spec) {
  rule <- .tte_assert_horizon_rule_present(spec, "the target trial protocol")
  k <- format(rule$min_expected_events, trim = TRUE, scientific = FALSE)
  first <- paste0(
    "Horizon rule: at least ",
    k,
    " expected events per arm in each year of follow-up"
  )
  if (rule$min_expected_events == 0) {
    return(paste0(first, ", so no estimate is withheld"))
  }
  return(paste0(
    first,
    ", at the pooled weighted event rate of both arms. Follow-up years are ",
    .HORIZON_WINDOW_WEEKS,
    " weeks from time zero. When any year up to the horizon fails, the rate ",
    "ratio, risk difference and number needed to treat of that estimand ",
    "read NE"
  ))
}


#' Build the three-column target trial protocol table
#'
#' @param spec Parsed spec list.
#' @param ctx Context list, from `.protocol_context()`.
#' @return A `data.table` with seven rows and exactly three columns.
#' @noRd
.build_protocol_table <- function(spec, ctx) {
  comps <- .tte_protocol_components()
  return(data.table::data.table(
    `Protocol component` = vapply(comps, function(x) x$label, character(1)),
    `Target trial specification` = vapply(
      comps,
      function(x) .protocol_specification(spec, x$key, ctx),
      character(1)
    ),
    `Target trial emulation` = vapply(
      comps,
      function(x) .protocol_emulation(spec, x$key, ctx),
      character(1)
    )
  ))
}


#' Title naming the single ETT the protocol sheet documents
#'
#' @param ctx Context list, from `.protocol_context()`.
#' @return A length-1 character string.
#' @noRd
.protocol_sheet_title <- function(ctx) {
  return(paste0(
    "Target trial protocol -- ",
    ctx$ett_id,
    " | enrollment ",
    ctx$enrollment_id,
    " (",
    ctx$enrollment_label,
    ") | outcome ",
    ctx$outcome_name,
    " (",
    ctx$outcome_var,
    ") | follow-up horizon ",
    ctx$follow_up_weeks,
    " weeks",
    if (nzchar(ctx$follow_up_label)) {
      paste0(" (", ctx$follow_up_label, ")")
    } else {
      ""
    }
  ))
}


#' Write the target trial protocol sheet
#'
#' Title in row 1, provenance note in row 2, header row 3, then the seven
#' component rows. The table starts at row 3, as every other titled sheet in
#' `$export_tables()` does; the note sits ABOVE it so nothing follows the last
#' component row and the table reads back as exactly seven rows.
#'
#' @param wb An `openxlsx` workbook.
#' @param sheet_name Worksheet name.
#' @param plan A [TTEPlan] (or any object exposing `$spec`, `$ett` and
#'   `$period_width`).
#' @param ett_id Character ETT id the sheet documents. `NULL` takes the first
#'   ETT in the grid.
#' @return `invisible(NULL)`.
#' @noRd
.write_protocol_table <- function(wb, sheet_name, plan, ett_id = NULL) {
  ctx <- .protocol_context(plan, ett_id)
  dt <- .build_protocol_table(plan$spec, ctx)

  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, .protocol_sheet_title(ctx), startRow = 1L)
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    paste0(
      "The specification column is authored in the spec under `target_trial:`. ",
      "The emulation column is rendered from the spec's `implementation:` ",
      "blocks and is never authored by hand."
    ),
    startRow = 2L
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    dt,
    startRow = 3L,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  openxlsx::addStyle(
    wb,
    sheet_name,
    style = openxlsx::createStyle(wrapText = TRUE, valign = "top"),
    rows = seq_len(nrow(dt)) + 3L,
    cols = 1:3,
    gridExpand = TRUE
  )
  openxlsx::setColWidths(wb, sheet_name, cols = 1:3, widths = c(26, 70, 70))
  return(invisible(NULL))
}
