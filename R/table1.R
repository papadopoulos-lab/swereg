# =============================================================================
# Custom Table 1 engine
# =============================================================================
# Replaces the previous tableone-based implementation. Produces a long-format
# data.table laid out for direct writing to Excel.
#
# Layout (one row per variable level + one row per continuous variable):
#   Variable | Level | Overall | <comparator> | <intervention> | [SMD]
#
# Missingness handling:
#   * Percentages for non-missing levels are computed against the non-missing
#     denominator within the column, so they sum to 100 across levels.
#   * When show_missing = TRUE, the variable name in the Variable column is
#     suffixed with " (missing X.X%)" using the Overall-column missing share.
#   * Continuous variables emit one stat row per variable; categorical
#     variables emit one row per level. There is no separate Missing row.
# =============================================================================


# --- type detection ----------------------------------------------------------

#' @noRd
.t1_var_type <- function(x) {
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    return("categorical")
  }
  if (is.numeric(x) || inherits(x, c("Date", "POSIXct"))) {
    return("continuous")
  }
  "categorical"
}


# --- weighted summary statistics ---------------------------------------------

#' Weighted mean and SD over the non-NA support of x.
#'
#' Reliability-weighted SD: matches the survey package convention so that
#' equal weights collapse to the standard sample SD.
#' @noRd
.t1_wtd_mean_sd <- function(x, w) {
  if (inherits(x, "Date")) x <- as.numeric(x)
  if (inherits(x, "POSIXct")) x <- as.numeric(x)
  if (is.null(w)) w <- rep(1, length(x))
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) {
    return(list(mean = NA_real_, sd = NA_real_, n = 0, sum_w = 0))
  }
  xv <- x[ok]
  wv <- w[ok]
  sw <- sum(wv)
  m <- sum(wv * xv) / sw
  v_num <- sum(wv * (xv - m)^2)
  v_den <- sw - sum(wv^2) / sw
  sd <- if (v_den > 0) sqrt(v_num / v_den) else NA_real_
  list(mean = m, sd = sd, n = length(xv), sum_w = sw)
}


#' Weighted counts and proportions for each level of a categorical variable.
#'
#' Proportions are computed against the non-missing weighted denominator so
#' they sum to 1 across the supplied levels.
#' @noRd
.t1_wtd_props <- function(x, w, levels) {
  if (is.null(w)) w <- rep(1, length(x))
  ok <- !is.na(x) & !is.na(w) & w > 0
  xv <- x[ok]
  wv <- w[ok]
  sw <- sum(wv)
  counts <- vapply(levels, function(lv) sum(wv[xv == lv]), numeric(1))
  props <- if (sw > 0) counts / sw else rep(NA_real_, length(levels))
  list(counts = counts, props = props, sum_w_nonmissing = sw)
}


#' Total weighted denominator (including missing) and weighted missing share.
#' @noRd
.t1_wtd_missing <- function(x, w) {
  if (is.null(w)) w <- rep(1, length(x))
  ok_w <- !is.na(w) & w > 0
  total_w <- sum(w[ok_w])
  miss_w <- sum(w[ok_w & is.na(x)])
  list(
    total_w = total_w,
    miss_w = miss_w,
    miss_pct = if (total_w > 0) 100 * miss_w / total_w else NA_real_
  )
}


# --- standardised mean differences -------------------------------------------

#' Continuous SMD: |m1 - m0| / sqrt((s1^2 + s0^2) / 2).
#' @noRd
.t1_smd_continuous <- function(s0, s1) {
  if (anyNA(c(s0$mean, s0$sd, s1$mean, s1$sd))) return(NA_real_)
  denom <- sqrt((s1$sd^2 + s0$sd^2) / 2)
  if (!is.finite(denom) || denom == 0) return(NA_real_)
  abs(s1$mean - s0$mean) / denom
}


#' Categorical SMD using the Yang & Dalton (2012) generalisation.
#'
#' Binary variables use the standard two-proportion form. Multi-level
#' categoricals use the multivariate form with the average of the two per-arm
#' covariance matrices. Falls back to the Moore-Penrose pseudoinverse when the
#' covariance matrix is singular (e.g. one level has zero prevalence in both
#' arms).
#' @noRd
.t1_smd_categorical <- function(p0, p1) {
  k <- length(p0)
  if (k != length(p1) || k < 2L) return(NA_real_)
  if (anyNA(p0) || anyNA(p1)) return(NA_real_)
  if (k == 2L) {
    pe <- p1[1]
    pc <- p0[1]
    denom <- sqrt((pe * (1 - pe) + pc * (1 - pc)) / 2)
    if (!is.finite(denom) || denom == 0) return(NA_real_)
    return(abs(pe - pc) / denom)
  }
  T <- (p1 - p0)[-k]
  S1 <- diag(p1[-k], nrow = k - 1L) - tcrossprod(p1[-k])
  S0 <- diag(p0[-k], nrow = k - 1L) - tcrossprod(p0[-k])
  S <- (S1 + S0) / 2
  S_inv <- tryCatch(
    solve(S),
    error = function(e) {
      if (requireNamespace("MASS", quietly = TRUE)) {
        MASS::ginv(S)
      } else {
        NULL
      }
    }
  )
  if (is.null(S_inv)) return(NA_real_)
  val <- as.numeric(t(T) %*% S_inv %*% T)
  if (!is.finite(val) || val < 0) return(NA_real_)
  sqrt(val)
}


# --- formatting helpers ------------------------------------------------------

#' Format a count + percentage for categorical rows. Counts are rounded to
#' 0 decimals and formatted with thousand separators; percentages respect
#' `digits_pct`. Works identically in both the weighted and unweighted
#' case (the unweighted path just happens to receive integer-valued
#' counts already).
#' @noRd
.t1_fmt_count_pct <- function(count, pct, weighted, digits_num, digits_pct) {
  if (is.na(count) || is.na(pct)) return("")
  count_str <- formatC(
    round(count),
    format = "d",
    big.mark = ","
  )
  sprintf(paste0("%s (%.", digits_pct, "f%%)"), count_str, pct)
}


#' Format mean (SD).
#' @noRd
.t1_fmt_mean_sd <- function(m, s, digits_num) {
  if (is.na(m)) return("")
  if (is.na(s)) {
    return(sprintf(paste0("%.", digits_num, "f"), m))
  }
  sprintf(paste0("%.", digits_num, "f (%.", digits_num, "f)"), m, s)
}


#' Format the n / sum-of-weights row at the top of the table. Rounded to
#' the nearest integer and printed with thousand separators.
#' @noRd
.t1_fmt_n <- function(sum_w, weighted, digits_num) {
  if (is.na(sum_w) || sum_w == 0) return("")
  formatC(round(sum_w), format = "d", big.mark = ",")
}


#' Format SMD to three decimals, blank for NA.
#' @noRd
.t1_fmt_smd <- function(x) {
  if (is.na(x)) return("")
  sprintf("%.3f", x)
}


# --- main entry point --------------------------------------------------------

#' Build a Table 1 data.table from a baseline slice.
#'
#' Internal helper used by `TTEEnrollment$table1()` and the workbook writers.
#' Returns a long-format `data.table` with one row per categorical level plus
#' one row per continuous variable, plus a leading `n` row. Optionally
#' includes an `SMD` column comparing the two strata levels.
#'
#' @param data A `data.table`-like object holding the baseline rows.
#' @param vars Character vector of variable names to summarise.
#' @param strata Character(1), name of a logical / 0-1 / two-level categorical
#'   column splitting rows into the two arms.
#' @param weights Character(1) or NULL, name of a weight column.
#' @param include_smd Logical, whether to emit an SMD column.
#' @param show_missing Logical, whether to append `(missing X.X%)` to variable
#'   names that have any missingness in the baseline rows.
#' @param digits_num Integer, decimal places for numeric statistics
#'   (default 2).
#' @param digits_pct Integer, decimal places for percentages (default 1).
#' @param arm_labels Optional named character vector
#'   `c(comparator = "...", intervention = "...")` providing display labels for
#'   the two arm columns. When NULL, the raw strata values are used.
#' @return A `data.table` ready to write to Excel.
#' @noRd
.swereg_table1 <- function(
  data,
  vars,
  strata,
  weights = NULL,
  include_smd = TRUE,
  show_missing = c("when_present", "always", "none"),
  digits_num = 2L,
  digits_pct = 1L,
  arm_labels = NULL
) {
  # Back-compat: accept TRUE/FALSE for older callers.
  if (isTRUE(show_missing)) show_missing <- "when_present"
  if (identical(show_missing, FALSE)) show_missing <- "none"
  show_missing <- match.arg(show_missing)
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  if (!strata %in% names(data)) {
    stop("strata column '", strata, "' not found in data")
  }
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0L) {
    stop(
      "Variables not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  weighted <- !is.null(weights)
  if (weighted) {
    if (!weights %in% names(data)) {
      stop("weight column '", weights, "' not found in data")
    }
    w_full <- data[[weights]]
  } else {
    w_full <- NULL
  }

  strata_full <- data[[strata]]
  if (weighted) {
    has_strata <- !is.na(strata_full) & !is.na(w_full) & w_full > 0
  } else {
    has_strata <- !is.na(strata_full)
  }

  strata_levels <- if (is.factor(strata_full)) {
    levels(strata_full)
  } else if (is.logical(strata_full)) {
    c("FALSE", "TRUE")
  } else {
    sort(unique(stats::na.omit(as.character(strata_full))))
  }
  if (length(strata_levels) != 2L) {
    stop(
      "strata must have exactly two non-missing levels; found: ",
      paste(strata_levels, collapse = ", ")
    )
  }

  comparator_value <- strata_levels[1]
  intervention_value <- strata_levels[2]

  if (!is.null(arm_labels) && all(c("intervention", "comparator") %in%
                                   names(arm_labels))) {
    comparator_label <- as.character(arm_labels[["comparator"]])
    intervention_label <- as.character(arm_labels[["intervention"]])
  } else {
    comparator_label <- as.character(comparator_value)
    intervention_label <- as.character(intervention_value)
  }

  strata_chr <- as.character(strata_full)
  mask_overall <- has_strata
  mask_comp <- has_strata & strata_chr == as.character(comparator_value)
  mask_int <- has_strata & strata_chr == as.character(intervention_value)

  out_cols <- c("Variable", "Level", "Overall", comparator_label, intervention_label)
  if (include_smd) out_cols <- c(out_cols, "SMD")

  rows <- list()
  add_row <- function(variable, level, overall, comp, int, smd = "") {
    r <- list(
      Variable = variable,
      Level = level,
      Overall = overall
    )
    r[[comparator_label]] <- comp
    r[[intervention_label]] <- int
    if (include_smd) r$SMD <- smd
    rows[[length(rows) + 1L]] <<- r
  }

  total_w_overall <- if (weighted) sum(w_full[mask_overall], na.rm = TRUE) else sum(mask_overall)
  total_w_comp <- if (weighted) sum(w_full[mask_comp], na.rm = TRUE) else sum(mask_comp)
  total_w_int <- if (weighted) sum(w_full[mask_int], na.rm = TRUE) else sum(mask_int)

  add_row(
    variable = "n",
    level = "",
    overall = .t1_fmt_n(total_w_overall, weighted, digits_num),
    comp = .t1_fmt_n(total_w_comp, weighted, digits_num),
    int = .t1_fmt_n(total_w_int, weighted, digits_num)
  )

  # Helper: weighted denominator for a mask (total or non-missing).
  mask_weight <- function(mask) {
    if (weighted) sum(w_full[mask], na.rm = TRUE) else sum(mask)
  }

  # When a Missing row is emitted, percentages are over the TOTAL denominator
  # (including missing), so every level plus the Missing row sums to 100.
  # When the Missing row is suppressed, percentages are over the NON-MISSING
  # denominator within each group, so level rows alone sum to 100.
  total_w_by_group <- list(
    overall = total_w_overall,
    comp    = total_w_comp,
    int     = total_w_int
  )
  emit_missing <- show_missing != "none"

  for (var in vars) {
    x_full <- data[[var]]
    type <- .t1_var_type(x_full)

    # Overall / per-group missingness counts (used for the Missing row and
    # for the non-missing denominators when show_missing is FALSE).
    miss_counts <- list(
      overall = mask_weight(mask_overall & is.na(x_full)),
      comp    = mask_weight(mask_comp    & is.na(x_full)),
      int     = mask_weight(mask_int     & is.na(x_full))
    )
    nonmiss_w_by_group <- list(
      overall = total_w_by_group$overall - miss_counts$overall,
      comp    = total_w_by_group$comp    - miss_counts$comp,
      int     = total_w_by_group$int     - miss_counts$int
    )

    denom_by_group <- if (emit_missing) total_w_by_group else nonmiss_w_by_group

    has_missing <- (miss_counts$overall > 0) ||
                   (miss_counts$comp    > 0) ||
                   (miss_counts$int     > 0)
    emit_missing_row <- switch(
      show_missing,
      none = FALSE,
      when_present = has_missing,
      always = TRUE
    )

    if (type == "continuous") {
      var_label <- paste0(var, " (mean (SD))")
      s_overall <- .t1_wtd_mean_sd(
        x_full[mask_overall],
        if (weighted) w_full[mask_overall] else NULL
      )
      s_comp <- .t1_wtd_mean_sd(
        x_full[mask_comp],
        if (weighted) w_full[mask_comp] else NULL
      )
      s_int <- .t1_wtd_mean_sd(
        x_full[mask_int],
        if (weighted) w_full[mask_int] else NULL
      )
      smd_val <- if (include_smd) {
        .t1_fmt_smd(.t1_smd_continuous(s_comp, s_int))
      } else {
        ""
      }
      add_row(
        variable = var_label,
        level = "",
        overall = .t1_fmt_mean_sd(s_overall$mean, s_overall$sd, digits_num),
        comp = .t1_fmt_mean_sd(s_comp$mean, s_comp$sd, digits_num),
        int = .t1_fmt_mean_sd(s_int$mean, s_int$sd, digits_num),
        smd = smd_val
      )
      if (emit_missing_row) {
        add_row(
          variable = "",
          level = "Missing",
          overall = .t1_fmt_count_pct(
            miss_counts$overall,
            100 * miss_counts$overall / denom_by_group$overall,
            weighted, digits_num, digits_pct
          ),
          comp = .t1_fmt_count_pct(
            miss_counts$comp,
            100 * miss_counts$comp / denom_by_group$comp,
            weighted, digits_num, digits_pct
          ),
          int = .t1_fmt_count_pct(
            miss_counts$int,
            100 * miss_counts$int / denom_by_group$int,
            weighted, digits_num, digits_pct
          )
        )
      }
      next
    }

    levels_x <- if (is.factor(x_full)) {
      levels(x_full)
    } else if (is.logical(x_full)) {
      c("FALSE", "TRUE")
    } else {
      sort(unique(as.character(stats::na.omit(x_full))))
    }
    if (length(levels_x) == 0L) {
      add_row(var, "(no observed levels)", "", "", "")
      next
    }
    x_chr <- if (is.logical(x_full)) {
      ifelse(is.na(x_full), NA_character_, as.character(x_full))
    } else {
      as.character(x_full)
    }

    # Raw weighted counts per level per group.
    counts_by_level <- function(mask) {
      xv <- x_chr[mask]
      wv <- if (weighted) w_full[mask] else rep(1, length(xv))
      ok <- !is.na(xv) & !is.na(wv) & wv > 0
      vapply(levels_x, function(lv) sum(wv[ok & xv == lv]), numeric(1))
    }
    c_overall <- counts_by_level(mask_overall)
    c_comp    <- counts_by_level(mask_comp)
    c_int     <- counts_by_level(mask_int)

    # SMD is always computed against non-missing props so it matches the
    # standardised definition.
    nm_props <- function(cts, nonmiss_w) {
      if (is.na(nonmiss_w) || nonmiss_w == 0) return(rep(NA_real_, length(cts)))
      cts / nonmiss_w
    }
    smd_val <- if (include_smd) {
      .t1_fmt_smd(.t1_smd_categorical(
        nm_props(c_comp, nonmiss_w_by_group$comp),
        nm_props(c_int, nonmiss_w_by_group$int)
      ))
    } else {
      ""
    }

    # Percentages as displayed in the table use the chosen denominator.
    for (j in seq_along(levels_x)) {
      add_row(
        variable = if (j == 1L) var else "",
        level = levels_x[j],
        overall = .t1_fmt_count_pct(
          c_overall[j],
          100 * c_overall[j] / denom_by_group$overall,
          weighted, digits_num, digits_pct
        ),
        comp = .t1_fmt_count_pct(
          c_comp[j],
          100 * c_comp[j] / denom_by_group$comp,
          weighted, digits_num, digits_pct
        ),
        int = .t1_fmt_count_pct(
          c_int[j],
          100 * c_int[j] / denom_by_group$int,
          weighted, digits_num, digits_pct
        ),
        smd = if (j == 1L) smd_val else ""
      )
    }
    if (emit_missing_row) {
      add_row(
        variable = "",
        level = "Missing",
        overall = .t1_fmt_count_pct(
          miss_counts$overall,
          100 * miss_counts$overall / denom_by_group$overall,
          weighted, digits_num, digits_pct
        ),
        comp = .t1_fmt_count_pct(
          miss_counts$comp,
          100 * miss_counts$comp / denom_by_group$comp,
          weighted, digits_num, digits_pct
        ),
        int = .t1_fmt_count_pct(
          miss_counts$int,
          100 * miss_counts$int / denom_by_group$int,
          weighted, digits_num, digits_pct
        )
      )
    }
  }

  out <- data.table::rbindlist(rows)
  data.table::setcolorder(out, out_cols)
  data.table::setattr(out, "swereg_type", "table1")
  data.table::setattr(out, "swereg_arm_labels", c(
    comparator = comparator_label,
    intervention = intervention_label
  ))
  data.table::setattr(out, "class", c("swereg_table1", class(out)))
  out
}
