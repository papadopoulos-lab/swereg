# =============================================================================
# Reporting the frozen sparse_rule decision (#52)
# =============================================================================
# Every output that names the population or the adjustment set reads the
# decision through `.tte_sparse_report()`, which reads
# `plan$get_sparse_decision()`. No output recomputes the decision. The outputs
# are the Sparse conditions sheet, the Study Specification sheet, the
# Enrollments and ETTs sheets, Table 1, the CONSORT labels, the TARGET
# checklist (items 6a, 6g and 7) and the target trial protocol table.
# =============================================================================

#' The basis of the rule, as every output states it
#'
#' The decision reads no follow-up outcome. Eligibility already removes a
#' person-trial with an outcome before time zero, so the text MUST NOT claim
#' that no outcome was examined.
#' @noRd
.TTE_SPARSE_BASIS <- paste(
  "decided from baseline counts in the eligible population, before the",
  "comparator draw and without follow-up outcomes"
)

#' The reporting view of the frozen sparse_rule decision
#'
#' Reads `plan$get_sparse_decision()`, and the spec only for the names of the
#' confounders. A plan without the accessor, as a test's list plan, has no
#' decision.
#'
#' @param plan A TTEPlan, or a list that carries `spec`.
#' @return `NULL` when the plan holds no decision. Otherwise a list with these
#'   elements:
#'   * `min_n`, and `decided_by`, the driving enrollment ids.
#'   * `decision`, the decision table.
#'   * `excluded` and `excluded_vars`, the names and variables of the excluded
#'     conditions.
#'   * `adjusted`, the names of the candidates that stay confounders.
#'   * `adjustment_set` and `adjustment_vars`, the names and variables of each
#'     spec confounder that the rule did not exclude, in spec order.
#' @noRd
.tte_sparse_report <- function(plan) {
  # `$`, never `[[`: `[[.TTEPlan` returns an enrollment spec, not a field.
  getter <- plan$get_sparse_decision
  if (!is.function(getter)) {
    return(NULL)
  }
  d <- getter()
  if (is.null(d) || nrow(d) == 0L) {
    return(NULL)
  }
  excluded <- d$decision == "excluded"
  ex_vars <- as.character(d$variable[excluded])
  confs <- plan$spec[["confounders"]] %||% list()
  conf_var <- vapply(
    confs,
    function(conf) {
      return(as.character(conf$implementation$variable %||% NA_character_))
    },
    character(1)
  )
  conf_name <- vapply(
    confs,
    function(conf) {
      return(as.character(conf$name %||% conf$implementation$variable))
    },
    character(1)
  )
  keep <- !conf_var %in% ex_vars
  return(list(
    min_n = as.integer(d$threshold[1L]),
    decided_by = strsplit(as.character(d$decided_by[1L]), ", ", fixed = TRUE)[[
      1L
    ]],
    decision = d,
    excluded = as.character(d$label[excluded]),
    excluded_vars = ex_vars,
    adjusted = as.character(d$label[!excluded]),
    adjustment_set = conf_name[keep],
    adjustment_vars = conf_var[keep]
  ))
}

#' Join names for a report line, or say "none"
#' @noRd
.tte_sparse_join <- function(x) {
  if (length(x) == 0L) {
    return("none")
  }
  return(paste(x, collapse = ", "))
}

#' The driving enrollments, each with its name
#' @noRd
.tte_sparse_drivers <- function(plan, ids) {
  return(paste0(
    "enrollment ",
    ids,
    " (",
    vapply(
      ids,
      function(eid) {
        return(as.character(.enrollment_label(plan, eid)))
      },
      character(1)
    ),
    ")",
    collapse = ", "
  ))
}

#' The rule in two present-tense sentences, without a final full stop
#' @noRd
.tte_sparse_rule_sentence <- function(min_n, drivers) {
  return(paste0(
    "A candidate condition held by fewer than ",
    min_n,
    " persons in the smallest arm of the driving comparisons becomes an ",
    "exclusion criterion of every comparison. The driving comparisons are ",
    drivers
  ))
}

#' The names of the candidates, from the spec, in the order of the rule
#' @noRd
.tte_sparse_candidate_names <- function(plan) {
  return(vapply(
    .tte_sparse_condition_entries(plan$spec),
    function(conf) {
      return(as.character(conf$name %||% conf$implementation$variable))
    },
    character(1)
  ))
}

#' How a person counts, from R15 of the plan, in the present tense
#' @noRd
.TTE_SPARSE_COUNT_DEFINITION <- paste(
  "A person counts once in an arm when any of her eligible person-trials in",
  "that arm holds the condition at its recruiting-week covariate snapshot.",
  "The same person can count in both arms."
)

#' The count shown for a smallest-arm count
#'
#' A count below the threshold shows as `<min_n`, so the sheet never prints a
#' small count.
#' @noRd
.tte_sparse_display_count <- function(n, min_n) {
  return(ifelse(n < min_n, paste0("<", min_n), as.character(n)))
}

#' Write the "Sparse conditions" sheet
#'
#' One row per candidate. The smallest arm count shows as `<min_n` when it is
#' below the threshold.
#'
#' @param wb An `openxlsx` workbook.
#' @param plan A TTEPlan.
#' @return `TRUE` when the sheet was added, and `FALSE` when the plan holds no
#'   decision. The caller MUST read it before it names the sheet in the table
#'   of contents.
#' @noRd
.write_sparse_conditions_sheet <- function(wb, plan) {
  rpt <- .tte_sparse_report(plan)
  if (is.null(rpt)) {
    return(invisible(FALSE))
  }
  d <- rpt$decision
  sht <- "Sparse conditions"
  arm_label <- function(eid, arm) {
    labels <- .lookup_arm_labels(plan$spec, eid)
    if (is.null(labels)) {
      return(arm)
    }
    return(paste0(arm, " (", labels[[arm]], ")"))
  }
  dt <- data.table::data.table(
    Condition = as.character(d$label),
    Variable = as.character(d$variable),
    Threshold = as.integer(d$threshold),
    `Smallest arm count` = .tte_sparse_display_count(d$smallest, d$threshold),
    Comparison = vapply(
      d$smallest_enrollment,
      function(eid) {
        return(.tte_sparse_drivers(plan, eid))
      },
      character(1)
    ),
    Arm = mapply(arm_label, d$smallest_enrollment, d$smallest_arm),
    Decision = as.character(d$decision)
  )
  openxlsx::addWorksheet(wb, sht)
  openxlsx::writeData(
    wb,
    sht,
    paste0(
      "Sparse conditions: the rare-condition rule of ",
      .tte_sparse_drivers(plan, rpt$decided_by),
      ", ",
      .TTE_SPARSE_BASIS
    ),
    startRow = 1L
  )
  openxlsx::addStyle(
    wb,
    sht,
    style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )
  openxlsx::writeData(
    wb,
    sht,
    paste(
      "Baseline counts of eligible persons per arm before the comparator",
      "draw, without follow-up outcomes.",
      .TTE_SPARSE_COUNT_DEFINITION,
      paste0(
        "A count below the threshold shows as <",
        rpt$min_n,
        "."
      )
    ),
    startRow = 2L
  )
  openxlsx::writeData(
    wb,
    sht,
    dt,
    startRow = 4L,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  openxlsx::setColWidths(
    wb,
    sht,
    cols = seq_len(ncol(dt)),
    widths = c(35, 35, 12, 20, 40, 30, 12)
  )
  return(invisible(TRUE))
}

#' The Study Specification rows of the rule
#'
#' @param plan A TTEPlan.
#' @param st_header,st_label The styles of a header and of a label.
#' @return A list of rows, each `list(a, b, sa, sb)`. Empty when the spec has
#'   no `sparse_rule`.
#' @noRd
.tte_sparse_spec_rows <- function(plan, st_header, st_label) {
  rule <- plan$spec[["sparse_rule"]]
  if (is.null(rule)) {
    return(list())
  }
  row <- function(a, b, sa = st_label) {
    return(list(a = a, b = b, sa = sa, sb = NULL))
  }
  out <- list(
    row("Rare-condition rule (sparse_rule)", NA_character_, st_header),
    row("Threshold (min_n):", as.character(rule$min_n)),
    row("Decided by:", .tte_sparse_drivers(plan, rule$decided_by)),
    row(
      "Rule:",
      paste0(
        .tte_sparse_rule_sentence(
          rule$min_n,
          .tte_sparse_drivers(plan, rule$decided_by)
        ),
        ". Every other candidate stays a confounder. The rule is ",
        .TTE_SPARSE_BASIS,
        ". The other comparisons inherit the decision."
      )
    ),
    row("Candidates:", .tte_sparse_join(.tte_sparse_candidate_names(plan)))
  )
  rpt <- .tte_sparse_report(plan)
  if (is.null(rpt)) {
    out <- c(out, list(row("Decision:", "not made yet: s1 has not run")))
  } else {
    out <- c(
      out,
      list(
        row("Excluded conditions:", .tte_sparse_join(rpt$excluded)),
        row("Final adjustment set:", .tte_sparse_join(rpt$adjustment_set))
      )
    )
  }
  return(c(out, list(row("", NA_character_, NULL))))
}

#' The role of each enrollment under the rule
#'
#' Adds the column `rare_condition_rule` to an overview table: "drove the
#' decision" for a driving enrollment, and "inherited the decision" for every
#' other. The table is unchanged when the plan holds no decision.
#'
#' @param dt A table with an `enrollment_id` column.
#' @param plan A TTEPlan.
#' @return `dt`.
#' @noRd
.tte_sparse_role_column <- function(dt, plan) {
  rpt <- .tte_sparse_report(plan)
  if (is.null(rpt) || nrow(dt) == 0L) {
    return(dt)
  }
  data.table::set(
    dt,
    j = "rare_condition_rule",
    value = ifelse(
      as.character(dt$enrollment_id) %in% rpt$decided_by,
      "drove the decision",
      "inherited the decision"
    )
  )
  return(dt)
}

#' Drop the rows of the excluded conditions from a baseline table
#'
#' A Table 1 variable cell is the variable name, or the name followed by a
#' space and a parenthesised suffix.
#'
#' @param baselines A `$get_baselines()` table.
#' @param plan A TTEPlan.
#' @return `baselines`, without the rows of an excluded condition.
#' @noRd
.tte_sparse_drop_baseline_rows <- function(baselines, plan) {
  rpt <- .tte_sparse_report(plan)
  if (is.null(rpt) || is.null(baselines) || nrow(baselines) == 0L) {
    return(baselines)
  }
  stem <- sub(" \\(.*$", "", as.character(baselines$variable))
  return(baselines[!stem %in% rpt$excluded_vars])
}

#' The Table 1 footnote, or NULL when the rule excludes nothing
#' @noRd
.tte_sparse_table1_footnote <- function(plan) {
  rpt <- .tte_sparse_report(plan)
  if (is.null(rpt) || length(rpt$excluded) == 0L) {
    return(NULL)
  }
  return(paste0(
    "Not shown: ",
    .tte_sparse_join(rpt$excluded),
    ". The rare-condition rule made each of these conditions an exclusion ",
    "criterion. See the Sparse conditions sheet."
  ))
}

#' The CONSORT label of each step-2 attrition row
#'
#' The key is the raw criterion that s1c writes, from
#' `.tte_sparse_criterion()`. The label names the condition, and its second
#' line names the rule.
#'
#' @param plan A TTEPlan.
#' @return A named character vector, empty when the decision excludes nothing.
#' @noRd
.tte_sparse_consort_labels <- function(plan) {
  rpt <- .tte_sparse_report(plan)
  if (is.null(rpt) || length(rpt$excluded) == 0L) {
    return(character())
  }
  ex <- rpt$decision[rpt$decision$decision == "excluded"]
  return(stats::setNames(
    paste0(
      ex$label,
      "\\n(rare-condition rule: fewer than ",
      ex$threshold,
      " persons at baseline in the smallest arm of the driving comparisons)"
    ),
    .tte_sparse_criterion(ex$threshold, ex$label)
  ))
}

#' The spec's confounders that the rule did not exclude
#' @noRd
.tte_sparse_keep_confounders <- function(plan, confounders) {
  rpt <- .tte_sparse_report(plan)
  if (is.null(rpt)) {
    return(confounders)
  }
  keep <- vapply(
    confounders,
    function(conf) {
      return(!isTRUE(conf$implementation$variable %in% rpt$excluded_vars))
    },
    logical(1)
  )
  return(confounders[keep])
}

#' The TARGET item 6a lines of the rule
#' @noRd
.tte_sparse_6a_lines <- function(plan) {
  rule <- plan$spec[["sparse_rule"]]
  if (is.null(rule)) {
    return(character())
  }
  drivers <- .tte_sparse_drivers(plan, rule$decided_by)
  rpt <- .tte_sparse_report(plan)
  return(c(
    paste0(
      "- Rare-condition rule: ",
      .tte_sparse_rule_sentence(rule$min_n, drivers),
      ". The rule is ",
      .TTE_SPARSE_BASIS,
      ". Candidates: ",
      .tte_sparse_join(.tte_sparse_candidate_names(plan)),
      "."
    ),
    if (is.null(rpt)) {
      "- Excluded under the rare-condition rule: not decided yet (s1 has not run)"
    } else {
      paste0(
        "- Excluded under the rare-condition rule: ",
        .tte_sparse_join(rpt$excluded)
      )
    }
  ))
}

#' The TARGET item 6g lines of the rule
#' @noRd
.tte_sparse_6g_lines <- function(plan) {
  rpt <- .tte_sparse_report(plan)
  if (is.null(rpt)) {
    return(character())
  }
  return(c(
    paste0("Final adjustment set: ", .tte_sparse_join(rpt$adjustment_set)),
    paste0(
      "Not adjusted for, because the rare-condition rule made each an ",
      "exclusion criterion (item 6a): ",
      .tte_sparse_join(rpt$excluded)
    )
  ))
}

#' The methods sentences of the rule, for TARGET item 7
#'
#' @return One string that ends in a space, or `""` when the spec has no
#'   `sparse_rule`.
#' @noRd
.tte_sparse_methods_text <- function(plan) {
  rule <- plan$spec[["sparse_rule"]]
  if (is.null(rule)) {
    return("")
  }
  rpt <- .tte_sparse_report(plan)
  outcome <- ""
  if (!is.null(rpt)) {
    outcome <- paste0(
      if (length(rpt$excluded) == 0L) {
        "The rule excluded no condition. "
      } else {
        paste0("The rule excluded ", .tte_sparse_join(rpt$excluded), ". ")
      },
      "The final adjustment set was ",
      .tte_sparse_join(rpt$adjustment_set),
      ". "
    )
  }
  return(paste0(
    "A pre-specified rare-condition rule was applied, with a threshold of ",
    rule$min_n,
    " persons. ",
    "The rule was ",
    .TTE_SPARSE_BASIS,
    ". ",
    "The counts were of eligible persons per arm in the driving comparisons: ",
    .tte_sparse_drivers(plan, rule$decided_by),
    ". ",
    "A person counted once in an arm when any of her eligible person-trials ",
    "in that arm held the condition at its recruiting-week covariate snapshot. ",
    "The same person could count in both arms. ",
    "A candidate condition held by fewer than ",
    rule$min_n,
    " persons in the smallest arm became an exclusion criterion of every ",
    "comparison. Every other candidate stayed a confounder. ",
    "The comparisons that did not drive the rule inherited its decision and ",
    "the adjustment set of the driving comparisons. ",
    outcome
  ))
}

#' The protocol table lines of the rule
#'
#' @param plan A TTEPlan.
#' @param where One of `"specification"`, `"emulation"` and `"analysis"`.
#' @return A character vector, empty when the spec has no `sparse_rule`.
#' @noRd
.tte_sparse_protocol_lines <- function(plan, where) {
  rule <- plan$spec[["sparse_rule"]]
  if (is.null(rule)) {
    return(character())
  }
  rpt <- .tte_sparse_report(plan)
  drivers <- .tte_sparse_drivers(plan, rule$decided_by)
  return(switch(
    where,
    specification = c(
      paste0(
        "Rare-condition rule, a pre-specified feasibility rule. ",
        .tte_sparse_rule_sentence(rule$min_n, drivers),
        ". Every other candidate stays a confounder"
      ),
      paste0(
        "Candidates: ",
        .tte_sparse_join(.tte_sparse_candidate_names(plan))
      ),
      paste0("The rule is ", .TTE_SPARSE_BASIS),
      paste0(
        "A randomised trial cannot make eligibility depend on enrolled ",
        "counts. The emulation applies this rule for feasibility"
      )
    ),
    emulation = if (is.null(rpt)) {
      "Rare-condition rule: not decided yet (s1 has not run)"
    } else {
      paste0(
        "Not eligible under the rare-condition rule: ",
        .tte_sparse_join(rpt$excluded),
        " (see the Sparse conditions sheet)"
      )
    },
    analysis = if (is.null(rpt)) {
      character()
    } else {
      paste0(
        "Final adjustment set, after the rare-condition rule: ",
        .tte_sparse_join(rpt$adjustment_set)
      )
    }
  ))
}
