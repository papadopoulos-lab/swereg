# The arm-size and variable-coverage sheets of feasibility.xlsx and
# tables.xlsx. `.write_feasibility_sheets()` in r6_tteplan_feasibility.R
# writes them into both workbooks.

#' The name of the arm-size sheet of the feasibility workbook.
#' @noRd
.SHEET_ARM_SIZES <- "Arm sizes"

#' The flag texts of the arm-size table.
#' @noRd
.ARM_SIZE_FLAGS <- c(
  rarer = "Comparator rarer than intervention: consider reversing the arms",
  short = "Comparator pool short of the target ratio"
)


#' The eligible and enrolled person-trials of each arm, per enrollment
#'
#' Sums the comparator-draw counts of `$get_matching()` over the trials of
#' each enrollment. The intervention arm enters in full. The draw takes
#' `round(ratio * n)` comparators per trial, or every comparator when the
#' pool is smaller, so a comparator pool smaller than the intervention arm
#' can never fill a ratio of 1 or more.
#'
#' @param plan A TTEPlan with s1 counts.
#' @return A data.table with one row per enrollment that stored counts:
#'   `enrollment_id`, `comparison`, `intervention`, `comparator`,
#'   `n_intervention_eligible`, `n_comparator_eligible`,
#'   `ratio_eligible` (comparator per intervention, `NA` when no intervention
#'   person-trial is eligible), `ratio_target`, `n_intervention_enrolled`,
#'   `n_comparator_enrolled`, `comparator_rarer`, `comparator_short` and
#'   `flag`. Zero rows when no enrollment stored counts.
#' @noRd
.tte_arm_sizes <- function(plan) {
  . <- n_intervention_total <- n_comparator_total <- NULL
  n_intervention_enrolled <- n_comparator_enrolled <- enrollment_id <- NULL
  mat <- plan$get_matching()
  empty <- data.table::data.table(
    enrollment_id = character(0),
    comparison = character(0),
    intervention = character(0),
    comparator = character(0),
    n_intervention_eligible = numeric(0),
    n_comparator_eligible = numeric(0),
    ratio_eligible = numeric(0),
    ratio_target = numeric(0),
    n_intervention_enrolled = numeric(0),
    n_comparator_enrolled = numeric(0),
    comparator_rarer = logical(0),
    comparator_short = logical(0),
    flag = character(0)
  )
  if (nrow(mat) == 0L) {
    return(empty)
  }
  agg <- mat[,
    .(
      n_intervention_eligible = sum(n_intervention_total, na.rm = TRUE),
      n_comparator_eligible = sum(n_comparator_total, na.rm = TRUE),
      n_intervention_enrolled = sum(n_intervention_enrolled, na.rm = TRUE),
      n_comparator_enrolled = sum(n_comparator_enrolled, na.rm = TRUE)
    ),
    keyby = enrollment_id
  ]
  enr_spec <- function(eid) {
    for (enr in plan$spec$enrollments) {
      if (isTRUE(enr$id == eid)) {
        return(enr)
      }
    }
    return(NULL)
  }
  rows <- lapply(seq_len(nrow(agg)), function(i) {
    eid <- agg$enrollment_id[i]
    enr <- enr_spec(eid)
    arms <- .lookup_arm_labels(plan$spec, eid)
    ratio <- enr$treatment$implementation$comparator_to_intervention_ratio
    ratio <- if (is.null(ratio)) NA_real_ else as.numeric(ratio)
    n_int <- agg$n_intervention_eligible[i]
    n_cmp <- agg$n_comparator_eligible[i]
    rarer <- n_int > 0 && n_cmp < n_int
    short <- !rarer && n_int > 0 && is.finite(ratio) && n_cmp < ratio * n_int
    flag <- if (rarer) {
      .ARM_SIZE_FLAGS[["rarer"]]
    } else if (short) {
      .ARM_SIZE_FLAGS[["short"]]
    } else {
      ""
    }
    return(data.table::data.table(
      enrollment_id = eid,
      comparison = .enrollment_label(plan, eid),
      intervention = if (is.null(arms)) NA_character_ else arms[["intervention"]],
      comparator = if (is.null(arms)) NA_character_ else arms[["comparator"]],
      n_intervention_eligible = n_int,
      n_comparator_eligible = n_cmp,
      ratio_eligible = if (n_int > 0) round(n_cmp / n_int, 2) else NA_real_,
      ratio_target = ratio,
      n_intervention_enrolled = agg$n_intervention_enrolled[i],
      n_comparator_enrolled = agg$n_comparator_enrolled[i],
      comparator_rarer = rarer,
      comparator_short = short,
      flag = flag
    ))
  })
  return(data.table::rbindlist(c(list(empty), rows), use.names = TRUE))
}


#' Write the Arm sizes sheet of the feasibility workbook
#'
#' A flagged row is filled red when the comparator is rarer than the
#' intervention, and yellow when the comparator pool is short of the target
#' ratio.
#'
#' @param wb An openxlsx workbook.
#' @param plan A TTEPlan.
#' @param header The header style of the workbook.
#' @return `invisible(NULL)`.
#' @noRd
.write_arm_sizes_sheet <- function(wb, plan, header) {
  sht <- .SHEET_ARM_SIZES
  openxlsx::addWorksheet(wb, sht)
  openxlsx::writeData(
    wb,
    sht,
    "Arm sizes: person-trials eligible for each arm, before the comparator draw"
  )
  openxlsx::addStyle(
    wb,
    sht,
    openxlsx::createStyle(textDecoration = "bold", fontSize = 12),
    rows = 1L,
    cols = 1L
  )
  openxlsx::writeData(
    wb,
    sht,
    paste0(
      "Summed over trials. The draw takes every eligible intervention ",
      "person-trial and up to the target ratio of comparators per trial. ",
      "When the comparator is rarer than the intervention, reversing the arms ",
      "lets the draw fill the ratio."
    ),
    startRow = 2L
  )
  tab <- .tte_arm_sizes(plan)
  if (nrow(tab) == 0L) {
    openxlsx::writeData(wb, sht, "(no comparator-draw counts)", startRow = 4L)
    return(invisible(NULL))
  }
  out <- data.table::data.table(
    Enrollment = tab$enrollment_id,
    Comparison = tab$comparison,
    Intervention = tab$intervention,
    Comparator = tab$comparator,
    `Eligible intervention person-trials` = tab$n_intervention_eligible,
    `Eligible comparator person-trials` = tab$n_comparator_eligible,
    `Comparators per intervention (eligible)` = tab$ratio_eligible,
    `Comparators per intervention (target)` = tab$ratio_target,
    `Enrolled intervention person-trials` = tab$n_intervention_enrolled,
    `Enrolled comparator person-trials` = tab$n_comparator_enrolled,
    Flag = tab$flag
  )
  openxlsx::writeData(wb, sht, out, startRow = 4L, headerStyle = header)
  fills <- list(
    rarer = openxlsx::createStyle(fgFill = "#F4B6B6"),
    short = openxlsx::createStyle(fgFill = "#FFE699")
  )
  for (k in names(fills)) {
    hit <- which(tab[[paste0("comparator_", k)]])
    if (length(hit) > 0L) {
      openxlsx::addStyle(
        wb,
        sht,
        fills[[k]],
        rows = 4L + hit,
        cols = seq_len(ncol(out)),
        gridExpand = TRUE
      )
    }
  }
  openxlsx::setColWidths(
    wb,
    sht,
    cols = seq_len(ncol(out)),
    widths = c(12, 50, 30, 30, rep(16, 6), 40)
  )
  return(invisible(NULL))
}


#' Name every enrollment whose comparator is rarer than its intervention
#'
#' @param tab The table of `.tte_arm_sizes()`.
#' @return `invisible(NULL)`.
#' @noRd
.arm_sizes_report <- function(tab) {
  rarer <- tab$enrollment_id[tab$comparator_rarer]
  short <- tab$enrollment_id[tab$comparator_short]
  if (length(rarer) > 0L) {
    cat(sprintf(
      "feasibility: the comparator is rarer than the intervention in %d of %d enrollments; consider reversing the arms: %s\n",
      length(rarer),
      nrow(tab),
      paste(rarer, collapse = ", ")
    ))
  }
  if (length(short) > 0L) {
    cat(sprintf(
      "feasibility: the comparator pool is short of the target ratio in %d of %d enrollments: %s\n",
      length(short),
      nrow(tab),
      paste(short, collapse = ", ")
    ))
  }
  return(invisible(NULL))
}


# ---- Variable coverage --------------------------------------------------------
#
# Two sheets answer "does the data hold what the spec asks for?". The first
# reads the skeleton summary that `$process_skeletons()` writes, so it covers
# the whole raw dataset and runs no new pass over the skeletons. The second
# reads what s1 to s3 stored for each enrollment and ETT: the persons each
# eligibility criterion removed, the baseline counts of each confounder per
# arm, and the minimum-events verdict of each outcome.

#' The names of the two coverage sheets.
#' @noRd
.SHEET_COVERAGE_RAW <- "Coverage (whole dataset)"

#' @noRd
.SHEET_COVERAGE_ETT <- "Coverage (ETT)"

#' The flag texts of the coverage sheets.
#' @noRd
.COVERAGE_FLAGS <- c(
  absent = "Not in the data: 0 persons",
  rare = "Fewer than 5 persons",
  arm_absent = "0 in an arm",
  arm_rare = "Fewer than 5 in an arm",
  removes_nobody = "Removes nobody",
  no_cut = "Passes no cut, not even 1 expected event per arm per year"
)

#' Show a count, with 1 to 4 written as `<5`.
#' @noRd
.coverage_count <- function(n) {
  out <- format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  out[is.na(n)] <- ""
  out[!is.na(n) & n > 0 & n < 5] <- "<5"
  return(out)
}


#' Every skeleton column the spec reads, with its role
#'
#' @param spec A spec from `tteplan_read_spec()`.
#' @return A data.table with one row per item and column: `scope` (`"All"` or
#'   an enrollment id), `role`, `name`, `variable` (the column the pipeline
#'   reads, or the derived confounder), `column` (a skeleton column that the
#'   summary counts) and `level` (a level of a character column, or `NA`).
#' @noRd
.tte_spec_columns <- function(spec) {
  rows <- list()
  add <- function(scope, role, name, impl, levels = NULL, variable = NULL) {
    cols <- unlist(impl[["source_variable"]] %||% impl[["variable"]])
    cols <- as.character(cols)
    if (length(cols) == 0L) {
      return(invisible(NULL))
    }
    variable <- variable %||%
      as.character(impl[["variable_combined"]] %||% impl[["variable"]] %||%
        impl[["source_variable_combined"]] %||% cols[1L])[1L]
    if (!is.null(impl[["value"]]) && is.null(levels)) {
      levels <- as.character(impl[["value"]])
    }
    levels <- if (length(levels) == 0L) NA_character_ else as.character(levels)
    for (col in cols) {
      for (lv in levels) {
        rows[[length(rows) + 1L]] <<- data.table::data.table(
          scope = scope,
          role = role,
          name = as.character(name %||% variable),
          variable = variable,
          column = col,
          level = lv
        )
      }
    }
    return(invisible(NULL))
  }
  for (o in spec$outcomes) {
    add("All", "Outcome", o$name, o$implementation)
  }
  for (cr in spec$inclusion_criteria$criteria) {
    add("All", "Inclusion", cr$name, cr$implementation)
  }
  for (cr in spec$exclusion_criteria) {
    add("All", "Exclusion", cr$name, cr$implementation)
  }
  cand <- as.character(unlist(spec$rare_condition_rule$candidates))
  for (cf in spec$confounders) {
    v <- as.character(cf$implementation[["variable"]] %||% NA_character_)[1L]
    role <- if (!is.na(v) && v %in% cand) {
      "Confounder (rare-condition candidate)"
    } else {
      "Confounder"
    }
    add("All", role, cf$name, cf$implementation, cf$categories, variable = v)
  }
  for (enr in spec$enrollments) {
    for (cr in enr$additional_inclusion) {
      add(enr$id, "Inclusion", cr$name, cr$implementation)
    }
    for (cr in enr$additional_exclusion) {
      add(enr$id, "Exclusion", cr$name, cr$implementation)
    }
    tx <- enr$treatment$implementation
    if (!is.null(tx[["variable"]])) {
      vals <- c(tx[["intervention_value"]], tx[["comparator_value"]])
      if (is.logical(vals)) {
        vals <- NULL
      }
      add(
        enr$id,
        "Treatment",
        paste(enr$treatment$arms$intervention, "vs", enr$treatment$arms$comparator),
        list(variable = tx[["variable"]]),
        vals
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.table::data.table(
      scope = character(0),
      role = character(0),
      name = character(0),
      variable = character(0),
      column = character(0),
      level = character(0)
    ))
  }
  return(unique(data.table::rbindlist(rows)))
}


#' Read the live skeleton summary of the plan's study
#'
#' A study held as a plain list is read by exact name: `$` on a list matches
#' a partial name, so `$summary` would return a `summary_table` element.
#'
#' @param plan A TTEPlan.
#' @return The summary list, or `NULL` when the study, its `summary.qs2` or a
#'   list summary is absent.
#' @noRd
.tte_live_skeleton_summary <- function(plan) {
  study <- plan$registrystudy
  s <- tryCatch(
    suppressMessages(
      if (is.environment(study)) study$summary else study[["summary", exact = TRUE]]
    ),
    error = function(e) NULL
  )
  if (!is.list(s)) {
    return(NULL)
  }
  return(s)
}


#' Refuse a skeleton summary that a reclean left incomplete
#'
#' s0 calls this from `tteplan_from_spec_and_registrystudy()`. An incomplete
#' summary means a reclean has not finished, so the skeletons are a mix of two
#' builds. s1 to s3 run as queued Slurm jobs and never stop on it.
#'
#' @param study A RegistryStudy, or `NULL`.
#' @return `NULL`, invisibly. Stops when the summary exists and is incomplete.
#' @noRd
.tte_check_skeleton_summary <- function(study) {
  s <- .tte_live_skeleton_summary(list(registrystudy = study))
  if (!is.null(s) && isFALSE(s$meta$is_complete)) {
    stop(
      "tteplan_from_spec_and_registrystudy(): the skeleton summary is incomplete (",
      s$meta$n_batches_present %||% "?",
      " of ",
      s$meta$n_batches_expected %||% "?",
      " batches). A reclean has not finished. Let it finish, then rerun s0.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}


#' Freeze the skeleton summary at the start of s1
#'
#' s1 never stops here: it runs as a queued Slurm job, and s0 refuses an
#' incomplete summary. The frozen copy records `is_complete`, so the Coverage
#' sheet can say when a reclean was running as s1 froze it.
#'
#' @param plan A TTEPlan.
#' @return A list of `columns`, `built_at`, `swereg_version` and
#'   `is_complete`, or `NULL` when the study has no summary.
#' @noRd
.tte_freeze_skeleton_summary <- function(plan) {
  s <- .tte_live_skeleton_summary(plan)
  if (is.null(s) || is.null(s$columns)) {
    return(NULL)
  }
  return(list(
    columns = data.table::copy(data.table::as.data.table(s$columns)),
    built_at = s$meta$built_at %||% NA,
    swereg_version = s$meta$swereg_version %||% NA_character_,
    is_complete = !isFALSE(s$meta$is_complete)
  ))
}


#' What the spec reads that the whole dataset does not hold
#'
#' Reads the whole-dataset coverage of `.tte_coverage_raw()`. A column is
#' absent when it holds 0 persons; a count of 1 to 4 counts as present.
#'
#' @param plan A TTEPlan.
#' @return A list: `outcomes`, the names of the outcomes whose columns hold
#'   0 persons, and `enrollments`, the ids of the enrollments with an
#'   inclusion criterion, exclusion criterion or confounder whose column holds
#'   0 persons.
#' @noRd
.tte_not_in_data <- function(plan) {
  n_persons <- NULL
  out <- list(outcomes = character(0), enrollments = character(0))
  tab <- .tte_coverage_raw(plan)$table
  if (is.null(tab) || nrow(tab) == 0L) {
    return(out)
  }
  tab <- tab[!is.na(tab$n_persons)]
  by_col <- tab[,
    list(n = max(n_persons)),
    by = c("scope", "role", "name", "column")
  ]
  absent <- by_col[by_col$n == 0]
  out$outcomes <- unique(absent$name[absent$role == "Outcome"])
  crit <- absent[absent$role %in% c("Inclusion", "Exclusion") | startsWith(absent$role, "Confounder")]
  ids <- vapply(plan$spec$enrollments, function(e) as.character(e$id), character(1))
  out$enrollments <- if (any(crit$scope == "All")) ids else intersect(ids, unique(crit$scope))
  return(out)
}


#' The whole-dataset coverage of every column the spec reads
#'
#' @param plan A TTEPlan.
#' @param summary_columns The `columns` table of a skeleton summary, or
#'   `NULL`. The default reads the summary that s1 froze in
#'   `plan$skeleton_summary`, and otherwise the live summary of the study.
#' @return A list: `table`, one row per row of `.tte_spec_columns()` with
#'   `n_persons` (`NA` when the summary does not count the column), `flag`
#'   and `rare_rule`; `built_at`, the time the summary was built, or `NA`;
#'   and `source`, a sentence that says which summary the counts come from.
#' @noRd
.tte_coverage_raw <- function(plan, summary_columns = NULL) {
  n_persons <- flag <- rare_rule <- NULL
  built_at <- NA
  source <- ""
  if (is.null(summary_columns)) {
    frozen <- plan$skeleton_summary
    live <- .tte_live_skeleton_summary(plan)
    live_at <- live$meta$built_at %||% NA
    if (!is.null(frozen$columns)) {
      summary_columns <- frozen$columns
      built_at <- frozen$built_at %||% NA
      source <- "From the skeleton summary that s1 froze"
      if (isFALSE(frozen$is_complete)) {
        source <- paste0(
          source,
          ". That summary was incomplete when s1 froze it: a reclean was ",
          "running, so some counts may be missing"
        )
      }
      if (!is.na(live_at) && !is.na(built_at) && live_at > built_at) {
        source <- paste0(
          source,
          ". A newer summary is on disk, built ",
          format(live_at, "%Y-%m-%d %H:%M"),
          ": a reclean ran after s1, so these counts may not match the ",
          "current skeletons"
        )
      }
    } else if (!is.null(live$columns)) {
      summary_columns <- live$columns
      built_at <- live_at
      source <- "From the live skeleton summary: s1 froze none"
    }
  }
  tab <- .tte_spec_columns(plan$spec)
  if (is.null(summary_columns) || nrow(tab) == 0L) {
    return(list(table = NULL, built_at = built_at, source = source))
  }
  counts <- stats::setNames(
    as.numeric(summary_columns$n_persons_with),
    as.character(summary_columns$column_name)
  )
  key <- ifelse(is.na(tab$level), tab$column, paste0(tab$column, "=", tab$level))
  n <- unname(counts[key])
  # A level the summary does not hold, of a column it counts by level, is a
  # level with no person.
  by_level <- unique(sub("=.*$", "", grep("=", names(counts), value = TRUE)))
  n[is.na(n) & !is.na(tab$level) & tab$column %in% by_level] <- 0
  tab[, n_persons := n]
  tab[,
    flag := data.table::fifelse(
      is.na(n_persons),
      "",
      data.table::fifelse(
        n_persons == 0,
        .COVERAGE_FLAGS[["absent"]],
        data.table::fifelse(n_persons < 5, .COVERAGE_FLAGS[["rare"]], "")
      )
    )
  ]
  # The rare-condition rule turns a candidate into an exclusion when its
  # smallest arm holds fewer than min_n persons. A candidate held by fewer
  # than min_n persons in the whole dataset always becomes one.
  min_n <- plan$spec$rare_condition_rule$min_n
  dec <- plan$sparse_decision
  tab[, rare_rule := ""]
  if (!is.null(min_n)) {
    for (v in unique(tab$variable[grepl("rare-condition", tab$role)])) {
      i <- which(tab$variable == v & grepl("rare-condition", tab$role))
      tot <- sum(tab$n_persons[i], na.rm = TRUE)
      txt <- if (all(!is.na(tab$n_persons[i])) && tot == 0) {
        "Becomes an exclusion that removes nobody"
      } else if (all(!is.na(tab$n_persons[i])) && tot < min_n) {
        paste0("Certain exclusion: fewer than ", min_n, " persons in the whole dataset")
      } else {
        "Decided at s1b from the eligible population"
      }
      if (data.table::is.data.table(dec) && v %in% dec$variable) {
        txt <- paste0(txt, "; s1b decision: ", dec$decision[match(v, dec$variable)])
      }
      tab[i, rare_rule := txt]
    }
  }
  return(list(table = tab, built_at = built_at, source = source))
}


#' Baseline counts of each confounder per arm, from one enrollment
#'
#' Reads the baseline rows of an enrollment object. A logical confounder
#' counts its `TRUE` rows, a character or factor confounder each level, and a
#' numeric one its non-missing rows. Every count is of person-trials.
#'
#' @param enrollment A TTEEnrollment, from the raw s1 file.
#' @return A data.table with `variable`, `level`, `n_intervention` and
#'   `n_comparator`, or `NULL` when the object holds no design.
#' @noRd
.tte_confounder_coverage <- function(enrollment) {
  des <- enrollment$design
  d <- enrollment$data
  if (is.null(des) || is.null(d)) {
    return(NULL)
  }
  base <- d[d[[des$tstart_var]] == 0]
  tx <- base[[des$treatment_var]] == TRUE
  tx[is.na(tx)] <- FALSE
  rows <- list()
  for (v in intersect(des$confounder_vars, names(base))) {
    x <- base[[v]]
    if (is.logical(x)) {
      hit <- x %in% TRUE
      rows[[length(rows) + 1L]] <- data.table::data.table(
        variable = v,
        level = "TRUE",
        n_intervention = sum(hit & tx),
        n_comparator = sum(hit & !tx)
      )
    } else if (is.character(x) || is.factor(x)) {
      x <- as.character(x)
      for (lv in sort(unique(stats::na.omit(x)))) {
        hit <- x %in% lv
        rows[[length(rows) + 1L]] <- data.table::data.table(
          variable = v,
          level = lv,
          n_intervention = sum(hit & tx),
          n_comparator = sum(hit & !tx)
        )
      }
    } else {
      hit <- !is.na(x)
      rows[[length(rows) + 1L]] <- data.table::data.table(
        variable = v,
        level = "(non-missing)",
        n_intervention = sum(hit & tx),
        n_comparator = sum(hit & !tx)
      )
    }
  }
  if (length(rows) == 0L) {
    return(NULL)
  }
  return(data.table::rbindlist(rows))
}


#' The coverage of each enrollment and ETT
#'
#' @param plan A TTEPlan.
#' @param coverage A named list by enrollment id of the tables of
#'   `.tte_confounder_coverage()`.
#' @param records A list keyed by ETT id: the per-protocol horizon records.
#' @return A data.table with `enrollment_id`, `ett_id`, `section`, `name`,
#'   `variable`, `level`, `intervention`, `comparator`, `persons`, `flag`
#'   and `note`, all character.
#' @noRd
.tte_coverage_ett <- function(plan, coverage, records) {
  enrollment_period_id <- enrollment_id <- step_order <- section <- NULL
  rows <- list()
  row <- function(...) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(...)
    return(invisible(NULL))
  }
  # Eligibility: the persons each criterion removed, after the criteria
  # above it. The global rows count persons across trials.
  att <- plan$get_attrition()
  if (nrow(att) > 0L) {
    glob <- att[is.na(enrollment_period_id)]
    for (eid in unique(glob$enrollment_id)) {
      g <- glob[enrollment_id == eid][order(step_order)]
      if (nrow(g) < 2L) {
        next
      }
      for (k in 2:nrow(g)) {
        removed <- g$n_persons[k - 1L] - g$n_persons[k]
        row(
          enrollment_id = eid,
          ett_id = "",
          section = "Eligibility",
          name = g$step_name[k],
          variable = "",
          level = "",
          intervention = .coverage_count(g$n_arm_intervention[k - 1L] - g$n_arm_intervention[k]),
          comparator = .coverage_count(g$n_arm_comparator[k - 1L] - g$n_arm_comparator[k]),
          persons = .coverage_count(removed),
          flag = if (isTRUE(removed == 0)) .COVERAGE_FLAGS[["removes_nobody"]] else "",
          note = "Removed by this criterion, after the criteria above it"
        )
      }
    }
  }
  # Confounders at baseline, per arm. A declared category that no baseline
  # row holds counts 0.
  conf_name <- list()
  conf_cats <- list()
  for (cf in plan$spec$confounders) {
    v <- as.character(cf$implementation[["variable"]] %||% NA_character_)[1L]
    if (!is.na(v)) {
      conf_name[[v]] <- cf$name
      conf_cats[[v]] <- as.character(unlist(cf$categories))
    }
  }
  dec <- plan$sparse_decision
  for (eid in names(coverage)) {
    cov <- coverage[[eid]]
    if (!data.table::is.data.table(cov) || nrow(cov) == 0L) {
      next
    }
    for (v in unique(cov$variable)) {
      cats <- conf_cats[[v]]
      if (length(cats) > 0L) {
        miss <- setdiff(cats, cov$level[cov$variable == v])
        if (length(miss) > 0L) {
          cov <- data.table::rbindlist(list(
            cov,
            data.table::data.table(
              variable = v,
              level = miss,
              n_intervention = 0,
              n_comparator = 0
            )
          ))
        }
      }
    }
    for (i in seq_len(nrow(cov))) {
      v <- cov$variable[i]
      ni <- cov$n_intervention[i]
      nc <- cov$n_comparator[i]
      excluded <- data.table::is.data.table(dec) &&
        v %in% dec$variable &&
        identical(dec$decision[match(v, dec$variable)], "excluded")
      flag <- if (excluded || cov$level[i] == "(non-missing)") {
        ""
      } else if (min(ni, nc) == 0) {
        .COVERAGE_FLAGS[["arm_absent"]]
      } else if (min(ni, nc) < 5) {
        .COVERAGE_FLAGS[["arm_rare"]]
      } else {
        ""
      }
      row(
        enrollment_id = eid,
        ett_id = "",
        section = "Confounder at baseline",
        name = as.character(conf_name[[v]] %||% v),
        variable = v,
        level = cov$level[i],
        intervention = .coverage_count(ni),
        comparator = .coverage_count(nc),
        persons = "",
        flag = flag,
        note = if (excluded) {
          "The rare-condition rule made it an exclusion, so every baseline row is FALSE"
        } else {
          "Person-trials at baseline, before imputation"
        }
      )
    }
  }
  # Rare-condition candidates: the s1b baseline counts of every enrollment.
  # A candidate the rule excluded has left the confounder set, so the
  # baseline rows above cannot show it.
  sc <- plan$sparse_counts
  if (data.table::is.data.table(sc) && nrow(sc) > 0L) {
    for (eid in unique(sc$enrollment_id)) {
      for (v in unique(sc$variable[sc$enrollment_id == eid])) {
        ni <- sc$n_persons[sc$enrollment_id == eid & sc$variable == v & sc$arm == "intervention"]
        nc <- sc$n_persons[sc$enrollment_id == eid & sc$variable == v & sc$arm == "comparator"]
        if (length(ni) != 1L || length(nc) != 1L) {
          next
        }
        excluded <- data.table::is.data.table(dec) &&
          v %in% dec$variable &&
          identical(dec$decision[match(v, dec$variable)], "excluded")
        row(
          enrollment_id = eid,
          ett_id = "",
          section = "Rare-condition candidate",
          name = as.character(conf_name[[v]] %||% v),
          variable = v,
          level = "TRUE",
          intervention = .coverage_count(ni),
          comparator = .coverage_count(nc),
          persons = "",
          flag = if (min(ni, nc) == 0) {
            .COVERAGE_FLAGS[["arm_absent"]]
          } else if (min(ni, nc) < 5) {
            .COVERAGE_FLAGS[["arm_rare"]]
          } else {
            ""
          },
          note = paste0(
            "Persons at baseline before the comparator draw (s1b). Rule: ",
            if (excluded) "excluded in every comparison" else "adjusted for"
          )
        )
      }
    }
  }
  # Outcomes: the minimum-events verdict at the horizon of each ETT.
  ett <- plan$ett
  for (eid_ett in names(records)) {
    rec <- records[[eid_ett]]
    j <- match(eid_ett, ett$ett_id)
    if (is.na(j) || !is.list(rec)) {
      next
    }
    cut <- rec$max_passing_cut
    row(
      enrollment_id = as.character(ett$enrollment_id[j]),
      ett_id = eid_ett,
      section = "Outcome",
      name = as.character(ett$outcome_name[j] %||% ett$outcome_var[j]),
      variable = as.character(ett$outcome_var[j]),
      level = paste0(ett$follow_up[j], " weeks"),
      intervention = "",
      comparator = "",
      persons = "",
      flag = if (length(cut) == 1L && is.na(cut)) .COVERAGE_FLAGS[["no_cut"]] else "",
      note = paste0(
        "Largest passing cut: ",
        if (length(cut) == 1L && !is.na(cut)) format(cut) else "none",
        "; ",
        rec$status %||% ""
      )
    )
  }
  if (length(rows) == 0L) {
    return(NULL)
  }
  out <- data.table::rbindlist(rows, use.names = TRUE)
  section_order <- c(
    "Eligibility",
    "Confounder at baseline",
    "Rare-condition candidate",
    "Outcome"
  )
  out <- out[order(enrollment_id, match(section, section_order))]
  return(out)
}


#' Fill the rows of a sheet that carry a red or a yellow flag
#' @noRd
.coverage_fill <- function(wb, sht, flags, start_row, n_col) {
  red <- c(
    .COVERAGE_FLAGS[["absent"]],
    .COVERAGE_FLAGS[["arm_absent"]],
    .COVERAGE_FLAGS[["removes_nobody"]],
    .COVERAGE_FLAGS[["no_cut"]]
  )
  fills <- list(
    red = list(hit = which(flags %in% red), fill = "#F4B6B6"),
    yellow = list(hit = which(nzchar(flags) & !flags %in% red), fill = "#FFE699")
  )
  for (f in fills) {
    if (length(f$hit) > 0L) {
      openxlsx::addStyle(
        wb,
        sht,
        openxlsx::createStyle(fgFill = f$fill),
        rows = start_row + f$hit,
        cols = seq_len(n_col),
        gridExpand = TRUE
      )
    }
  }
  return(invisible(NULL))
}


#' Write the two coverage sheets of the feasibility workbook
#'
#' @param wb An openxlsx workbook.
#' @param plan A TTEPlan.
#' @param coverage A named list by enrollment id of the tables of
#'   `.tte_confounder_coverage()`.
#' @param records A list keyed by ETT id: the per-protocol horizon records.
#' @param header The header style of the workbook.
#' @return `invisible(NULL)`.
#' @noRd
.write_coverage_sheets <- function(wb, plan, coverage, records, header) {
  bold <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12)

  sht <- .SHEET_COVERAGE_RAW
  openxlsx::addWorksheet(wb, sht)
  openxlsx::writeData(
    wb,
    sht,
    "Coverage in the whole dataset: persons with each skeleton column the specification reads"
  )
  openxlsx::addStyle(wb, sht, bold, rows = 1L, cols = 1L)
  raw <- .tte_coverage_raw(plan)
  openxlsx::writeData(
    wb,
    sht,
    paste0(
      if (nzchar(raw$source)) raw$source else "From the skeleton summary",
      if (!is.na(raw$built_at)) paste0(", built ", format(raw$built_at, "%Y-%m-%d %H:%M")) else "",
      ". Every person in the skeleton, over every week, before any ",
      "eligibility, and blind to arm. A column with 0 persons is not in the ",
      "data delivery or is coded wrongly. An empty count is a column the ",
      "summary does not count, such as a numeric column. 1 to 4 show as <5."
    ),
    startRow = 2L
  )
  tab <- raw$table
  if (is.null(tab)) {
    openxlsx::writeData(wb, sht, "(no skeleton summary found)", startRow = 4L)
  } else {
    out <- data.table::data.table(
      Scope = tab$scope,
      Role = tab$role,
      Name = tab$name,
      `Skeleton column` = tab$column,
      Level = data.table::fifelse(is.na(tab$level), "", tab$level),
      Persons = .coverage_count(tab$n_persons),
      Flag = tab$flag,
      `Rare-condition rule` = tab$rare_rule
    )
    openxlsx::writeData(wb, sht, out, startRow = 4L, headerStyle = header)
    .coverage_fill(wb, sht, tab$flag, 4L, ncol(out))
    openxlsx::setColWidths(
      wb,
      sht,
      cols = seq_len(ncol(out)),
      widths = c(10, 24, 60, 40, 24, 12, 30, 50)
    )
  }

  sht <- .SHEET_COVERAGE_ETT
  openxlsx::addWorksheet(wb, sht)
  openxlsx::writeData(
    wb,
    sht,
    "Coverage in each enrollment and ETT"
  )
  openxlsx::addStyle(wb, sht, bold, rows = 1L, cols = 1L)
  openxlsx::writeData(
    wb,
    sht,
    paste0(
      "Eligibility: persons each criterion removed, after the criteria above ",
      "it, with the person-trials per arm. Confounder at baseline: ",
      "person-trials per arm at time zero, from the s1 raw file. Outcome: ",
      "the largest cut of the minimum-events rule that the per-protocol ",
      "estimate passes at the horizon of the ETT. 1 to 4 show as <5."
    ),
    startRow = 2L
  )
  ett_tab <- .tte_coverage_ett(plan, coverage, records)
  if (is.null(ett_tab)) {
    openxlsx::writeData(wb, sht, "(no stored counts)", startRow = 4L)
  } else {
    out <- data.table::data.table(
      Enrollment = ett_tab$enrollment_id,
      ETT = ett_tab$ett_id,
      Section = ett_tab$section,
      Name = ett_tab$name,
      Variable = ett_tab$variable,
      `Level or follow-up` = ett_tab$level,
      Intervention = ett_tab$intervention,
      Comparator = ett_tab$comparator,
      `Persons removed` = ett_tab$persons,
      Flag = ett_tab$flag,
      Note = ett_tab$note
    )
    openxlsx::writeData(wb, sht, out, startRow = 4L, headerStyle = header)
    .coverage_fill(wb, sht, ett_tab$flag, 4L, ncol(out))
    openxlsx::setColWidths(
      wb,
      sht,
      cols = seq_len(ncol(out)),
      widths = c(12, 12, 22, 60, 36, 18, 14, 14, 14, 30, 60)
    )
  }
  return(invisible(NULL))
}
