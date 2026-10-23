# This writer produces the effect-modification sheet, which compares stratum
# estimates side by side.

#' Write an "Effect modification" sheet: per ETT x subgroup, the stratum IRRs
#' (per-protocol and intention-to-treat side by side) and the interaction-test
#' p-value / ratio of stratum IRRs.
#'
#' Reads `$get_subgroups()`, which returns the union of the two stored slot
#' families and reports a skipped result as absent.
#'
#' The sheet iterates the SPECIFICATION, `plan$ett$subgroup_vars`, and
#' `$get_subgroups()` iterates what was stored. A variable the specification
#' names and no worker stored therefore gets no accessor row, and this function
#' emits the one all-`NA` row it always did. That row is the consumer's, and
#' the accessor invents nothing.
#'
#' The two interval headers state the study level `.s3_conf_level()`, the
#' level s3 computed the stratum intervals at.
#'
#' An estimand that the minimum-events rule withheld reads `NE` in its stratum
#' ratio and interval cells, and in its interaction-test cells on the `"all"`
#' row. The writer reads the decision from `horizon_status` in
#' `$get_estimates()`.
#' @noRd
.write_effect_modification <- function(wb, sheet_name, plan, title = NULL) {
  openxlsx::addWorksheet(wb, sheet_name)
  row_ptr <- 1L
  if (!is.null(title)) {
    openxlsx::writeData(wb, sheet_name, title, startRow = row_ptr)
    openxlsx::addStyle(
      wb,
      sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
      rows = row_ptr,
      cols = 1L
    )
    row_ptr <- row_ptr + 2L
  }

  ett <- plan$ett
  if (is.null(ett) || nrow(ett) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No ETTs to report.",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  sg <- plan$get_subgroups()
  analysed <- .plan_analysed_ett_ids(plan)
  est <- plan$get_estimates()
  withheld_of <- function(eid, want_estimand) {
    hit <- which(est$ett_id == eid & est$estimand == want_estimand)
    return(any(.tte_horizon_withheld(est$horizon_status[hit])))
  }
  ne_rows <- list()
  ci_hdr <- paste0(.ff_conf_pct(.s3_conf_level(plan$spec)), "% CI")

  # `which()` runs OUTSIDE the data.table subset, so `want_estimand` is the
  # argument. Inside `sg[...]` it would resolve to the COLUMN of that name and
  # the filter would keep every estimand.
  slot_rows <- function(eid, sv, want_estimand) {
    if (nrow(sg) == 0L) {
      return(sg)
    }
    hit <- which(
      sg$ett_id == eid &
        sg$subgroup_var == sv &
        sg$estimand == want_estimand
    )
    return(sg[hit])
  }
  # `strata_stored` is the stored SHAPE: the plan holds a stratified table for
  # this subgroup variable and estimand. A row without it is the accessor's
  # INTERACTION-ONLY row, which stands for a stored interaction test with no
  # stored stratified table, and it names no stratum.
  #
  # The test is on the shape and never on the numbers. A stored stratum whose
  # rate ratio is inestimable keeps its level, so a per-protocol level that
  # could not be computed never removes the intention-to-treat result beside
  # it.
  strata_levels <- function(rows) {
    if (nrow(rows) == 0L) {
      return(character(0))
    }
    return(as.character(rows$subgroup_level)[which(rows$strata_stored)])
  }
  irr_cell <- function(rows, lvl) {
    hit <- which(as.character(rows$subgroup_level) == lvl)
    if (nrow(rows) == 0L || length(hit) == 0L) {
      return(list(irr = NA_real_, ci = NA_character_))
    }
    rr <- rows[hit[1L]]
    return(list(
      irr = rr$irr,
      ci = if (is.na(rr$irr)) {
        NA_character_
      } else {
        sprintf("(%.2f, %.2f)", rr$irr_lo, rr$irr_hi)
      }
    ))
  }
  em_val <- function(rows, field) {
    if (nrow(rows) == 0L) {
      return(NA_real_)
    }
    return(as.numeric(rows[[field]][1L]))
  }

  rows <- list()
  for (i in seq_len(nrow(ett))) {
    eid <- ett$ett_id[i]
    if (!eid %in% analysed) {
      next
    }
    sg_vars <- if (
      "subgroup_vars" %in% names(ett) && !is.null(ett$subgroup_vars[[i]])
    ) {
      ett$subgroup_vars[[i]]
    } else {
      character(0)
    }
    ne_pp <- withheld_of(eid, "pp")
    ne_itt <- withheld_of(eid, "itt")
    for (sv in sg_vars) {
      pp <- slot_rows(eid, sv, "pp")
      itt <- slot_rows(eid, sv, "itt")
      pp_levels <- strata_levels(pp)
      itt_levels <- strata_levels(itt)
      levels <- if (length(pp_levels) > 0L) {
        pp_levels
      } else if (length(itt_levels) > 0L) {
        itt_levels
      } else {
        "all"
      }
      for (lvl in levels) {
        pc <- irr_cell(pp, lvl)
        ic <- irr_cell(itt, lvl)
        is_all <- identical(lvl, "all")
        # `[<-` with `list()`, so a row with nothing withheld keeps its slot.
        ne_rows[length(ne_rows) + 1L] <- list(c(
          if (ne_pp) c(5L, 6L, if (is_all) c(9L, 10L)),
          if (ne_itt) c(7L, 8L, if (is_all) c(11L, 12L))
        ))
        rows[[length(rows) + 1L]] <- data.frame(
          Enrollment = eid,
          Outcome = ett$outcome_name[i],
          Subgroup = sv,
          Level = as.character(lvl),
          `PP IRR` = pc$irr,
          pp_ci = pc$ci,
          `ITT IRR` = ic$irr,
          itt_ci = ic$ci,
          `EM p (PP)` = if (is_all) em_val(pp, "em_pvalue") else NA_real_,
          `EM ratio (PP)` = if (is_all) {
            em_val(pp, "ratio_of_irrs")
          } else {
            NA_real_
          },
          `EM p (ITT)` = if (is_all) em_val(itt, "em_pvalue") else NA_real_,
          `EM ratio (ITT)` = if (is_all) {
            em_val(itt, "ratio_of_irrs")
          } else {
            NA_real_
          },
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      "No subgroups configured (add a top-level `subgroups:` block to the spec).",
      startRow = row_ptr
    )
    return(invisible(NULL))
  }

  df <- do.call(rbind, rows)
  names(df)[match(c("pp_ci", "itt_ci"), names(df))] <-
    paste(c("PP", "ITT"), ci_hdr)
  openxlsx::writeData(
    wb,
    sheet_name,
    df,
    startRow = row_ptr,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold",
      fgFill = "#EFEFEF",
      border = "bottom"
    )
  )
  # A withheld estimand reads `NE`. The number columns keep their numbers on
  # every other row, so the text goes in cell by cell, after the table.
  for (k in seq_along(ne_rows)) {
    for (col in ne_rows[[k]]) {
      .sheet_write_ne(wb, sheet_name, row_ptr + k, col)
    }
  }
  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_len(ncol(df)),
    widths = "auto"
  )
  return(invisible(NULL))
}
