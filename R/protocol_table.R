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
  list(
    list(key = "eligibility_criteria", label = "Eligibility criteria"),
    list(key = "treatment_strategies", label = "Treatment strategies"),
    list(key = "assignment_procedure", label = "Assignment procedure"),
    list(key = "outcome", label = "Outcome"),
    list(key = "follow_up_period", label = "Follow-up period"),
    list(key = "causal_contrast", label = "Causal contrast"),
    list(key = "analysis_plan", label = "Analysis plan")
  )
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
  paste(x, collapse = "\n")
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
  paste(as.character(v), collapse = "__")
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
  paste(as.character(x), collapse = ", ")
}


#' Render the stratum of the comparator draw
#'
#' The draw runs inside one `trial_id` group, and `trial_id` is the week index
#' divided by `period_width`. The stratum is therefore the entry band, and the
#' band is the only stratum. A width of 1 makes the band one week.
#'
#' @param period_width Integer band width in weeks, or `NULL`.
#' @return A length-1 character string.
#' @noRd
.protocol_draw_stratum <- function(period_width) {
  if (is.null(period_width) || length(period_width) == 0L) {
    return("Comparator draw stratum: the entry band, and nothing else")
  }
  pw <- as.integer(period_width)[1]
  if (is.na(pw) || pw <= 1L) {
    return("Comparator draw stratum: the entry week, and nothing else")
  }
  paste0(
    "Comparator draw stratum: the ",
    pw,
    "-week entry band, and nothing else"
  )
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
    stop("plan has no spec; cannot build the target trial protocol table")
  }
  ett <- plan$ett
  if (is.null(ett) || nrow(ett) == 0L) {
    stop("plan has no ETTs; cannot build the target trial protocol table")
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
    stop("unknown ett_id: ", ett_id)
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
    if (identical(.protocol_impl_variable(out[["implementation"]]), outcome_var)) {
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

  list(
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
    # stratum is the entry band. Its width comes from the plan.
    period_width = plan$period_width
  )
}


#' Render the "Target trial specification" cell for one component
#'
#' Clinical fields only, plus the study team's `target_trial:` prose. Two
#' components (causal contrast, analysis plan) have no clinical fields in the
#' spec at all, so their cell is the `target_trial:` entry alone.
#'
#' @param spec Parsed spec list.
#' @param key Component key, from [.tte_protocol_components()].
#' @param ctx Context list, from [.protocol_context()].
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
      for (ic in spec[["inclusion_criteria"]]) {
        if (is.list(ic) && !is.null(ic[["name"]])) {
          out <- c(out, paste0("Include: ", ic[["name"]]))
        }
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
        } else {
          out <- c(
            out,
            paste0(
              "Include (enrollment ",
              ctx$enrollment_id,
              "): ",
              .protocol_value(ai[["name"]])
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
      out
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
          as.character(f[["label"]] %||% paste0(f[["weeks"]], " weeks"))
        },
        character(1)
      )
      c(
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
  .protocol_lines(c(derived, authored))
}


#' Render the "Target trial emulation" cell for one component
#'
#' Every fragment here comes from a nested `implementation:` block, or from
#' the ETT grid that those blocks generated. Nothing is read from
#' `target_trial:`.
#'
#' @param spec Parsed spec list.
#' @param key Component key, from [.tte_protocol_components()].
#' @param ctx Context list, from [.protocol_context()].
#' @return A length-1 character string.
#' @noRd
.protocol_emulation <- function(spec, key, ctx) {
  enrollment <- ctx$enrollment
  outcome <- ctx$outcome
  tx_impl <- enrollment[["treatment"]][["implementation"]]
  fragments <- switch(
    key,
    eligibility_criteria = {
      out <- character()
      iso <- spec[["inclusion_criteria"]][["isoyears"]]
      if (!is.null(iso)) {
        out <- c(
          out,
          paste0("Require isoyear in ", paste(iso, collapse = " to "))
        )
      }
      # Symmetric partner to the exclusion loop below. `inclusion_criteria`
      # is a NAMED list whose only entry in every spec written so far is the
      # `isoyears` scalar pair, handled above -- so this loop renders an empty
      # set today. It exists so that a spec which later adds a global
      # inclusion criterion OBJECT with its own implementation block is not
      # silently dropped from the protocol table. The `is.list()` guard is
      # what keeps the `isoyears` vector out of it, and it matches the guard
      # `.protocol_specification()` already uses.
      for (ic in spec[["inclusion_criteria"]]) {
        if (!is.list(ic) || is.null(ic[["name"]])) {
          next
        }
        impl <- ic[["implementation"]]
        out <- c(
          out,
          paste0(
            "Require ",
            .protocol_impl_variable(impl),
            " (",
            .format_window_human(impl),
            ")"
          )
        )
      }
      for (ec in spec[["exclusion_criteria"]]) {
        impl <- ec[["implementation"]]
        out <- c(
          out,
          paste0(
            "Drop rows where ",
            .protocol_impl_variable(impl),
            " is TRUE (",
            .format_window_human(impl),
            ")"
          )
        )
      }
      for (ai in enrollment[["additional_inclusion"]]) {
        impl <- ai[["implementation"]]
        if (identical(ai[["type"]], "age_range")) {
          out <- c(
            out,
            paste0(
              "Require ",
              .protocol_impl_variable(impl),
              " between ",
              .protocol_value(ai[["min"]]),
              " and ",
              .protocol_value(ai[["max"]])
            )
          )
        } else {
          out <- c(
            out,
            paste0(
              "Require ",
              .protocol_impl_variable(impl),
              " (",
              .format_window_human(impl),
              ")"
            )
          )
        }
      }
      for (ae in enrollment[["additional_exclusion"]]) {
        impl <- ae[["implementation"]]
        out <- c(
          out,
          paste0(
            "Drop rows where ",
            .protocol_impl_variable(impl),
            " is ",
            .protocol_value(impl[["intervention_value"]]),
            " (",
            .format_window_human(impl),
            ")"
          )
        )
      }
      out
    },
    treatment_strategies = c(
      paste0("Treatment variable: ", .protocol_impl_variable(tx_impl)),
      paste0(
        "Intervention value: ",
        .protocol_value(tx_impl[["intervention_value"]])
      ),
      paste0(
        "Comparator value: ",
        .protocol_value(tx_impl[["comparator_value"]])
      )
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
      paste0("Treatment variable: ", .protocol_impl_variable(tx_impl)),
      paste0(
        "Arm values: ",
        .protocol_value(tx_impl[["intervention_value"]]),
        " versus ",
        .protocol_value(tx_impl[["comparator_value"]])
      )
    ),
    outcome = c(
      paste0(
        "Outcome variable: ",
        .protocol_impl_variable(outcome[["implementation"]])
      ),
      paste0("Outcome column in the analysis file: ", ctx$outcome_var)
    ),
    follow_up_period = {
      weeks <- vapply(
        spec[["follow_up"]],
        function(f) as.numeric(f[["weeks"]]),
        numeric(1)
      )
      c(
        paste0("Horizon on this sheet: ", ctx$follow_up_weeks, " weeks"),
        paste0(
          "Horizons in the ETT grid: ",
          paste(weeks, collapse = ", "),
          " weeks"
        )
      )
    },
    causal_contrast = c(
      paste0("ETT: ", ctx$ett_id),
      paste0("Treatment variable: ", .protocol_impl_variable(tx_impl)),
      paste0(
        "Intervention value: ",
        .protocol_value(tx_impl[["intervention_value"]])
      ),
      paste0(
        "Comparator value: ",
        .protocol_value(tx_impl[["comparator_value"]])
      ),
      paste0("Outcome variable: ", ctx$outcome_var),
      paste0("Horizon: ", ctx$follow_up_weeks, " weeks")
    ),
    analysis_plan = {
      out <- character()
      for (conf in spec[["confounders"]]) {
        impl <- conf[["implementation"]]
        v <- .protocol_impl_variable(impl)
        if (isTRUE(impl[["computed"]])) {
          out <- c(
            out,
            paste0("Adjust for ", v, " (", .format_window_human(impl), ")")
          )
        } else {
          out <- c(out, paste0("Adjust for ", v))
        }
      }
      out
    },
    character()
  )
  .protocol_lines(fragments)
}


#' Build the three-column target trial protocol table
#'
#' @param spec Parsed spec list.
#' @param ctx Context list, from [.protocol_context()].
#' @return A `data.table` with seven rows and exactly three columns.
#' @noRd
.build_protocol_table <- function(spec, ctx) {
  comps <- .tte_protocol_components()
  data.table::data.table(
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
  )
}


#' Title naming the single ETT the protocol sheet documents
#'
#' @param ctx Context list, from [.protocol_context()].
#' @return A length-1 character string.
#' @noRd
.protocol_sheet_title <- function(ctx) {
  paste0(
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
  )
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
  invisible(NULL)
}
