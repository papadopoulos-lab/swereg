# Helpers for TTEPlan reporting. They state the censoring-model settings of
# item 6h of the TARGET checklist and the rule clause of a checklist criterion
# line. The file also holds the stored-results diagnostic.
#
# `.plan_results_summary()` holds the whole body of `TTEPlan$results_summary()`.
# The method in `R/r6_tteplan_results.R` is a one-call delegate to it.

#' The censoring-model settings that s3 stored beside each ETT result
#'
#' s2 records `estimate_ipcw_pp_with_gam` and
#' `estimate_ipcw_pp_separately_by_treatment` on each per-protocol analysis
#' file. s3 carries them into `results_ett[[ett_id]]$ipcw_pp_options`. The
#' settings describe how a result was computed, and no accessor returns them.
#'
#' @param plan A `TTEPlan`.
#' @return A list named by ETT, one element per stored result. An element is
#'   `NULL` for a result whose analysis file carries no record of the
#'   settings.
#' @noRd
.plan_ipcw_pp_options <- function(plan) {
  return(lapply(plan$results_ett, function(r) r[["ipcw_pp_options"]]))
}

#' The censoring-model settings that item 6h of the TARGET checklist states
#'
#' Each setting gets one sentence, and the sentence names the ETTs of each
#' value when the ETTs differ.
#'
#' A result has no record when its analysis file carries none. Its setting
#' reads `not recorded (s1 or s2 ran before swereg 27.2.0)`, never the
#' default, because the default does not show what s2 used.
#'
#' @param opts The list `.plan_ipcw_pp_options()` returns.
#' @return A named character vector with the elements `by_arm` and `gam`.
#'   Each element is empty or holds sentences that end in a space.
#' @noRd
.target_6h_ipcw_settings <- function(opts) {
  if (length(opts) == 0L) {
    return(c(
      by_arm = paste0(
        "No ETT holds a stored s3 result, so this item does not state the ",
        "settings of the censoring models. "
      ),
      gam = ""
    ))
  }
  return(c(
    by_arm = .target_6h_setting_sentence(
      .target_6h_read_setting(opts, "estimate_ipcw_pp_separately_by_treatment"),
      "estimate_ipcw_pp_separately_by_treatment",
      "Each factor was fitted",
      c("separately in each arm", "once on both arms pooled"),
      "Whether each factor was fitted separately in each arm"
    ),
    gam = .target_6h_setting_sentence(
      .target_6h_read_setting(opts, "estimate_ipcw_pp_with_gam"),
      "estimate_ipcw_pp_with_gam",
      "The loss and deviation models were",
      c("generalized additive models", "generalized linear models"),
      "Whether the loss and deviation models were generalized additive models"
    )
  ))
}

#' Read one stored s2 setting of each ETT
#'
#' @param opts The list `.plan_ipcw_pp_options()` returns.
#' @param key The name of the setting.
#' @return A logical vector named by ETT. `NA` marks an ETT whose result holds
#'   no record of the setting.
#' @noRd
.target_6h_read_setting <- function(opts, key) {
  return(vapply(
    names(opts),
    function(id) {
      v <- opts[[id]][[key]]
      if (is.logical(v) && length(v) == 1L) {
        return(v)
      }
      return(NA)
    },
    logical(1)
  ))
}

#' The clause that names the ETTs of one setting value
#'
#' @param ids Character vector of ETT ids.
#' @return `" for ETT a"`, `" for ETTs a and b"` or `" for ETTs a, b and c"`.
#' @noRd
.target_6h_for_etts <- function(ids) {
  n <- length(ids)
  if (n == 1L) {
    return(paste0(" for ETT ", ids))
  }
  return(paste0(
    " for ETTs ",
    paste(ids[-n], collapse = ", "),
    " and ",
    ids[n]
  ))
}

#' The item 6h sentences for one s2 setting
#'
#' The sentence names the ETTs of each value only when the ETTs differ.
#'
#' @param vals A logical vector named by ETT, from
#'   `.target_6h_read_setting()`.
#' @param arg The name of the s2 argument.
#' @param lead The words before the value.
#' @param phrases Character(2), the words for `TRUE` and for `FALSE`.
#' @param unknown The subject of the sentence for an ETT without a record.
#' @return A character scalar of sentences that end in a space.
#' @noRd
.target_6h_setting_sentence <- function(vals, arg, lead, phrases, unknown) {
  mixed <- length(unique(vals)) > 1L
  known <- !is.na(vals)
  parts <- character(0)
  for (i in 1:2) {
    v <- c(TRUE, FALSE)[i]
    hit <- names(vals)[known & vals == v]
    if (length(hit) > 0L) {
      parts <- c(
        parts,
        paste0(
          phrases[i],
          " (",
          arg,
          " = ",
          v,
          ")",
          if (mixed) .target_6h_for_etts(hit)
        )
      )
    }
  }
  out <- ""
  if (length(parts) > 0L) {
    out <- paste0(lead, " ", paste(parts, collapse = ", and "), ". ")
  }
  if (any(!known)) {
    out <- paste0(
      out,
      unknown,
      " (",
      arg,
      ") was not recorded (s1 or s2 ran before swereg 27.2.0)",
      if (mixed) .target_6h_for_etts(names(vals)[!known]),
      ". "
    )
  }
  return(out)
}


#' Print the stored-results diagnostic
#'
#' The body of `TTEPlan$results_summary()`. It reports on the CACHE and
#' never on a number.
#'
#' @param plan A `TTEPlan`.
#' @return The plan, invisibly.
#' @noRd
.plan_results_summary <- function(plan) {
  if (is.null(plan$results_ett) || length(plan$results_ett) == 0L) {
    cat("No ETT results stored. Run $s3_analyze() first.\n")
    return(invisible(plan))
  }

  # A feasibility-horizon ETT gets no row, as in `$get_estimates()`.
  feasibility_ids <- .plan_ett_feasibility_ids(plan)
  rows <- lapply(names(plan$results_ett), function(ett_id) {
    if (ett_id %in% feasibility_ids) {
      return(NULL)
    }
    r <- plan$results_ett[[ett_id]]
    n_events <- if (!is.null(r$summary)) r$summary$n_events else NA
    irr_status <- if (is.null(r$irr_pp_trunc)) {
      "NULL"
    } else if (isTRUE(r$irr_pp_trunc$skipped)) {
      paste0("SKIP: ", r$irr_pp_trunc$reason)
    } else {
      "OK"
    }
    rates_status <- if (is.null(r$rates_pp_trunc)) {
      "NULL"
    } else if (isTRUE(r$rates_pp_trunc$skipped)) {
      "SKIP"
    } else {
      "OK"
    }
    return(data.table::data.table(
      enrollment = r$enrollment_id,
      ett_id = ett_id,
      description = r$description,
      n_events = n_events,
      irr = irr_status,
      rates = rates_status
    ))
  })
  dt <- data.table::rbindlist(rows)
  print(dt, nrows = Inf)

  # Enrollment summary
  if (!is.null(plan$results_enrollment)) {
    cat(sprintf(
      "\nEnrollment results: %d/%d computed\n",
      length(plan$results_enrollment),
      length(unique(plan$ett$enrollment_id))
    ))
  }
  return(invisible(plan))
}


#' The rule clause the TARGET checklist appends to one criterion line
#'
#' The checklist is manuscript prose, so a washout of type `no_prior_value` or
#' `only_prior_value` states what it does. A criterion that is not a washout
#' adds nothing.
#'
#' @param impl An implementation list.
#' @return A single string, empty when the implementation is not a washout.
#' @noRd
.tte_checklist_rule <- function(impl) {
  rule <- .tte_washout_prose(impl)
  if (is.null(rule)) {
    return("")
  }
  return(paste0(", rule: ", rule))
}
