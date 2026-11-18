# Hospital days for the PRE2DUP periods port in R/pre2dup_periods.R.
#
# Ported from PRE2DUP-R (https://github.com/piavat/PRE2DUP-R, commit 43b6d89,
# MIT licence, Copyright (c) 2025 PRE2DUPR authors): the stay union of
# R/check_hospitalizations.R, R/calc_hosp_days.R and R/calc_dup_hosp_days.R.
# See inst/COPYRIGHTS. data.table::foverlaps() finds the stays that touch a
# window, in place of the cartesian merge by person of R/calc_hosp_days.R.
# The intervals package is not used.

# The union of the stays of each person, as intervals::interval_union() forms
# it in PRE2DUP-R's check_hospitalizations(). The stays are closed intervals
# over the real line, so two stays that share a date join and two stays one
# day apart do not. A running maximum of the discharge date over stays sorted
# by admission finds the joins without a loop over persons.
.pre2dup_hosp_union <- function(hosp) {
  id <- admission <- discharge <- run <- NULL
  .pre2dup_check_columns(hosp, c("id", "admission", "discharge"), "hosp")
  h <- data.table(
    id = as.integer(hosp[["id"]]),
    admission = as.integer(hosp[["admission"]]),
    discharge = as.integer(hosp[["discharge"]])
  )
  if (anyNA(h)) {
    stop("hosp holds a missing id, admission or discharge.", call. = FALSE)
  }
  if (any(h$discharge <= h$admission)) {
    stop(
      "Every stay in hosp needs a discharge date after its admission date.",
      call. = FALSE
    )
  }
  if (nrow(h) == 0L) {
    return(h)
  }
  setorder(h, id, admission, discharge)
  person <- rleid(h$id)
  span <- max(h$discharge) - min(h$admission) + 1
  # The offset per person keeps the running maximum inside one person.
  key <- (person - 1) * span + (h$discharge - min(h$admission))
  run_max <- cummax(key)
  prev_max <- shift(run_max)
  first <- c(TRUE, person[-1L] != person[-length(person)])
  key_admission <- (person - 1) * span + (h$admission - min(h$admission))
  h[, run := cumsum(first | key_admission > prev_max)]
  out <- h[,
    list(admission = min(admission), discharge = max(discharge)),
    by = list(id, run)
  ]
  out[, run := NULL]
  return(out[])
}

# Hospital days that start during the exposure of each purchase, as
# calc_hosp_days()$hosp_days_exp returns them. The discharge day is not a
# hospital day. `next_start` is NA where no next purchase exists.
.pre2dup_hosp_days <- function(id, start, exp_end, next_start, stays) {
  row <- c_start <- e_end <- n_start <- w_end <- a <- d1 <- NULL
  admission <- discharge <- NULL
  s_all <- e_all <- days_all <- days_exp <- NULL
  out <- numeric(length(id))
  if (length(id) == 0L || nrow(stays) == 0L) {
    return(out)
  }
  q <- data.table(
    row = seq_along(id),
    id = as.integer(id),
    c_start = as.integer(start),
    e_end = as.integer(exp_end),
    n_start = as.integer(next_start)
  )
  # A stay outside [start, next purchase], or outside [start, exposure end]
  # for the last purchase, gets no days from the rules below.
  q[, w_end := pmax(fifelse(is.na(n_start), e_end, n_start), c_start)]
  h <- stays[, list(id, a = admission, d1 = discharge - 1L)]
  setkeyv(h, c("id", "a", "d1"))
  ov <- foverlaps(
    q,
    h,
    by.x = c("id", "c_start", "w_end"),
    by.y = c("id", "a", "d1"),
    type = "any",
    nomatch = NULL
  )
  if (nrow(ov) == 0L) {
    return(out)
  }
  # The two fcase() calls are those of calc_hosp_days(), renamed.
  ov[,
    s_all := fcase(
      a >= c_start & (!is.na(n_start) & a < n_start),
      pmax(c_start, a),
      a >= c_start & (is.na(n_start)) & a < e_end,
      pmax(c_start, a),
      a < c_start & d1 > c_start,
      c_start,
      a < c_start & d1 <= c_start,
      NA_integer_
    )
  ]
  ov[,
    e_all := fcase(
      is.na(n_start) & d1 > c_start & a < e_end,
      d1,
      d1 >= n_start,
      n_start,
      d1 < c_start,
      NA_integer_,
      d1 < n_start,
      d1
    )
  ]
  ov[, days_all := e_all - s_all]
  ov[, days_all := fifelse(is.na(days_all), 0, days_all)]
  ov[, days_exp := fifelse(a < e_end, days_all, 0)]
  agg <- ov[, list(days_exp = sum(days_exp)), by = row]
  out[agg$row] <- agg$days_exp
  return(out)
}

# Hospital days inside each period, as calc_dup_hosp_days() counts them: the
# period runs from `start` to the whole day of `end`, the discharge day is
# not a hospital day, and an overlap counts `end - start` days.
.pre2dup_period_hosp_days <- function(id, start, end, stays) {
  row <- s <- e <- a <- d1 <- days <- admission <- discharge <- NULL
  out <- numeric(length(id))
  if (length(id) == 0L || nrow(stays) == 0L) {
    return(out)
  }
  q <- data.table(
    row = seq_along(id),
    id = as.integer(id),
    s = as.integer(start),
    e = as.integer(end)
  )
  setkeyv(q, c("id", "s", "e"))
  h <- stays[, list(id, a = admission, d1 = discharge - 1L)]
  ov <- foverlaps(
    h,
    q,
    by.x = c("id", "a", "d1"),
    by.y = c("id", "s", "e"),
    type = "any",
    nomatch = NULL
  )
  if (nrow(ov) == 0L) {
    return(out)
  }
  ov[, days := pmin(e, d1) - pmax(s, a)]
  agg <- ov[, list(days = sum(days)), by = row]
  out[agg$row] <- agg$days
  return(out)
}
