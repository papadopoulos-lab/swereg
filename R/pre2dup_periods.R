# The PRE2DUP drug use periods algorithm, ported from PRE2DUP-R.
#
# Source: PRE2DUP-R (https://github.com/piavat/PRE2DUP-R), commit 43b6d89,
# PRE2DUPR 0.2.2, MIT licence, Copyright (c) 2025 PRE2DUPR authors. See
# inst/COPYRIGHTS. This file ports steps 2, 3, 5 and 6 of R/pre2dup.R in the
# same order and with the same rules. Step 1 (input checks) and step 4 (the
# common package durations) are not ported. The hospital days are in
# R/pre2dup_hospital.R.
#
# Every per-group loop of the source is a whole-column operation here: the
# lag and the lead inside a group, the sliding average, the ERFL rules and
# the ATC lookup. Where the source sums inside a closure, which base::sum()
# and base::mean() accumulate in long double, the port uses frollsum() and
# frollmean() with algo = "exact", which accumulate the same way. Where the
# source sums with data.table GForce, the port uses the same GForce call. The
# post-processing merge stays a loop over passes, as in the source.

#' ATC-level parameters of PRE2DUP
#'
#' The ATC-level parameters that PRE2DUP-R ships as `ATC_parameters`, with
#' unchanged values. PRE2DUP uses the row of the longest `partial_atc` that
#' starts the ATC code of a purchase.
#'
#' @format A data frame with 175 rows and 5 columns:
#' \describe{
#'   \item{partial_atc}{A full or a partial ATC code.}
#'   \item{lower_ddd_atc}{The lowest usual daily dose, in DDDs.}
#'   \item{usual_ddd_atc}{The usual daily dose, in DDDs.}
#'   \item{minimum_dur_atc}{The minimum duration of one purchase, in days.}
#'   \item{maximum_dur_atc}{The maximum duration of one purchase, in days.}
#' }
#' @source PRE2DUP-R 0.2.2, <https://github.com/piavat/PRE2DUP-R>, commit
#'   43b6d89. MIT licence, copyright 2025 PRE2DUPR authors. The file
#'   `COPYRIGHTS` of the installed package names the ported code.
#' @keywords datasets
"pre2dup_atc_parameters"

# Drug use periods from purchases, as PRE2DUPR::pre2dup(data_to_return =
# "periods") returns them, without date_range and drop_atcs. Purchases whose
# package_id has no row in `packages` use the ATC-level rules. The port never
# stops on PRE2DUP-R's 90% coverage rule: it returns the share of purchases
# per ATC without a package row as the attribute "package_coverage".
#
# Output: one row per period, with id, atc, block, period, start, end,
# n_purchases, hospital_days and total_ddd. `start` and `end` are IDate and
# inclusive. `end` is the day that holds PRE2DUP-R's fractional dup_end.
# `block` numbers the blocks in (id, atc, date) order and is the block of the
# first purchase of the period. `period` numbers the periods 1 to N in (id,
# atc, start) order, as PRE2DUP-R numbers them.
#
# With trace = TRUE the output carries the attribute "purchases": one row per
# purchase after the same-day aggregation, with id, atc, date, block, period,
# duration (the final ERFL, in days) and joined_by. joined_by is "first" for
# the first purchase of a period. Otherwise it names the step that joined the
# purchase to the previous purchase: "duration", "extended", "hospital",
# "stockpiling" or "merge". Each step writes its label where it sets the
# period-start flag to FALSE. With trace = FALSE no step writes a label.
.pre2dup_periods <- function(
  purchases,
  packages,
  atc_parameters,
  hosp = NULL,
  global_gap_max = 300,
  global_min = 5,
  global_max = 300,
  global_max_single = 150,
  global_ddd_high = 10,
  global_hosp_max = 30,
  days_covered = 5,
  weight_past = 1,
  weight_current = 4,
  weight_next = 1,
  weight_first_last = 5,
  post_process_perc = 1,
  trace = FALSE
) {
  prepared <- .pre2dup_prepare(purchases, packages, atc_parameters)
  p <- prepared$purchases
  stays <- if (is.null(hosp)) NULL else .pre2dup_hosp_union(hosp)

  # Step 2: purchase durations.
  p <- .pre2dup_day_level(p, global_min, global_max)
  .pre2dup_blocks(p, global_gap_max)
  .pre2dup_sliding_average(
    p,
    w_past = weight_past,
    w_present = weight_current,
    w_future = weight_next,
    w_limit = weight_first_last,
    max_refill = global_max
  )
  .pre2dup_erfl(p, global_ddd_high, trace)
  .pre2dup_extend(
    p,
    stays,
    global_gap_max,
    global_hosp_max,
    days_covered,
    trace
  )

  # Step 3: stockpiling, then the periods of fewer than three purchases.
  .pre2dup_stockpiling(p, global_hosp_max, days_covered, trace)
  .pre2dup_short_periods(p, stays, global_hosp_max, days_covered, trace)

  # Step 5: period ends. Step 6: post-processing.
  per <- .pre2dup_period_ends(p, stays, global_max_single, global_hosp_max)
  per <- .pre2dup_post_process(per, post_process_perc, trace)
  out <- .pre2dup_collect(p, per, stays)
  setattr(out, "package_coverage", prepared$coverage)
  if (trace) {
    setattr(out, "purchases", .pre2dup_trace(p, attr(per, "merged")))
  }
  return(out)
}

.pre2dup_check_columns <- function(x, cols, what) {
  miss <- setdiff(cols, names(x))
  if (length(miss) > 0L) {
    stop(
      what,
      " is missing these columns: ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The lag (or lead) of x inside groups g. The rows of each group MUST be
# contiguous. NA where the previous (or next) row is in another group.
.pre2dup_shift <- function(x, g, n = 1L, type = "lag") {
  out <- shift(x, n, type = type)
  same <- g == shift(g, n, type = type)
  out[is.na(same) | !same] <- NA
  return(out)
}

# The group sum of x on every row of the group, accumulated in long double as
# base::sum() does it. `pos` is the position of the row inside its group and
# `len` is the group size; the rows of each group MUST be contiguous.
.pre2dup_group_sum <- function(x, pos, len) {
  last <- pos == len
  s <- frollsum(x, fifelse(last, len, 1L), adaptive = TRUE, algo = "exact")
  return(s[seq_along(x) - pos + len])
}

# The group mean, computed as base::mean() computes it.
.pre2dup_group_mean <- function(x, pos, len) {
  last <- pos == len
  m <- frollmean(x, fifelse(last, len, 1L), adaptive = TRUE, algo = "exact")
  return(m[seq_along(x) - pos + len])
}

# The size of the group of each row. The rows of each group MUST be
# contiguous.
.pre2dup_group_size <- function(g) {
  len <- rle(g)$lengths
  return(rep.int(len, len))
}

# 0 where x is NA or NaN, as sum(..., na.rm = TRUE) drops such a term.
.pre2dup_nz <- function(x) {
  return(fifelse(is.na(x), 0, x))
}

# The longest entry of opt_atcs that starts each ATC code, or NA. The source
# walks one row at a time (.find_closest_atc). This joins every prefix of
# every distinct code in one step.
.pre2dup_closest_atc <- function(atc, opt_atcs) {
  code <- k <- prefix <- NULL
  u <- unique(atc)
  len <- nchar(u)
  cand <- data.table(code = rep(u, len), k = sequence(len))
  cand[, prefix := substr(code, 1L, k)]
  cand <- cand[prefix %chin% opt_atcs]
  setorder(cand, code, -k)
  cand <- cand[!duplicated(code)]
  return(cand$prefix[match(atc, cand$code)])
}

# Reads the inputs, adds the same-day purchases of one package together, and
# joins the package and ATC parameters (the preparation before step 2).
.pre2dup_prepare <- function(purchases, packages, atc_parameters) {
  id <- atc <- package_id <- ddd <- ratio <- atc_short <- no_package <- NULL
  dur_usual_pack <- NULL
  .pre2dup_check_columns(
    purchases,
    c("id", "atc", "package_id", "date", "ratio", "ddd"),
    "purchases"
  )
  .pre2dup_check_columns(
    packages,
    c(
      "package_id",
      "atc",
      "dur_min",
      "dur_usual",
      "dur_max",
      "ddd_low",
      "ddd_usual"
    ),
    "packages"
  )
  .pre2dup_check_columns(
    atc_parameters,
    c(
      "partial_atc",
      "lower_ddd_atc",
      "usual_ddd_atc",
      "minimum_dur_atc",
      "maximum_dur_atc"
    ),
    "atc_parameters"
  )
  p <- data.table(
    id = as.integer(purchases[["id"]]),
    atc = as.character(purchases[["atc"]]),
    package_id = as.integer(purchases[["package_id"]]),
    date = as.integer(purchases[["date"]]),
    ratio = as.numeric(purchases[["ratio"]]),
    ddd = as.numeric(purchases[["ddd"]])
  )
  p[is.na(ddd), ddd := 0]
  key_cols <- c("id", "atc", "date", "package_id")
  p[, let(ddd = sum(ddd), ratio = sum(ratio)), by = key_cols]
  p <- unique(p, by = key_cols)

  p[,
    atc_short := .pre2dup_closest_atc(
      atc,
      as.character(atc_parameters[["partial_atc"]])
    )
  ]
  if (anyNA(p$atc_short)) {
    stop(
      "Every ATC code in purchases needs a partial_atc in atc_parameters that ",
      "starts it. These have none: ",
      paste(unique(p$atc[is.na(p$atc_short)]), collapse = ", "),
      call. = FALSE
    )
  }
  k <- data.table(
    package_id = as.integer(packages[["package_id"]]),
    ddd_low_pack = as.numeric(packages[["ddd_low"]]),
    ddd_usual_pack = as.numeric(packages[["ddd_usual"]]),
    dur_min_pack = as.numeric(packages[["dur_min"]]),
    dur_usual_pack = as.numeric(packages[["dur_usual"]]),
    dur_max_pack = as.numeric(packages[["dur_max"]])
  )
  if (anyDuplicated(k$package_id) > 0L) {
    stop("packages holds a package_id more than once.", call. = FALSE)
  }
  a <- data.table(
    atc_short = as.character(atc_parameters[["partial_atc"]]),
    ddd_low_atc = as.numeric(atc_parameters[["lower_ddd_atc"]]),
    ddd_usual_atc = as.numeric(atc_parameters[["usual_ddd_atc"]]),
    dur_min_atc = as.numeric(atc_parameters[["minimum_dur_atc"]]),
    dur_max_atc = as.numeric(atc_parameters[["maximum_dur_atc"]])
  )
  p <- k[p, on = "package_id"]
  p <- a[p, on = "atc_short"]

  # PRE2DUP-R measures its coverage rule on these rows, after the same-day
  # aggregation. The port reports the share and does not stop.
  p[, no_package := as.numeric(is.na(dur_usual_pack))]
  coverage <- p[,
    list(n_purchases = .N, share_without_package = mean(no_package)),
    by = atc
  ]
  setorder(coverage, atc)
  p[, no_package := NULL]
  # Inside one person, ATC and day, the source holds the rows in package_id
  # order (its merge sorts them), and that order is the order of its sums.
  setorder(p, id, atc, date, package_id)
  return(list(purchases = p, coverage = coverage[]))
}

# Step 2, per person, ATC and day: the DDD and duration limits, the
# package-based ERFL (.count_erfl), and the daily DDD total.
.pre2dup_day_level <- function(p, global_min, global_max) {
  id <- atc <- day <- day_i <- day_n <- ddd <- ratio <- NULL
  ddd_low_pack <- ddd_usual_pack <- dur_min_pack <- dur_usual_pack <- NULL
  dur_max_pack <- ddd_low_atc <- ddd_usual_atc <- dur_min_atc <- NULL
  dur_max_atc <- has_low <- has_usual <- has_dur <- has_usual_atc <- NULL
  ddd_pos <- min_dur_one <- max_dur_one <- all_low <- low_min <- NULL
  all_usual <- all_dur <- all_usual_atc <- all_pos <- min_dur <- NULL
  max_dur <- max_ratio <- min_ddd <- sum_ratio <- sum_ddd_pack <- NULL
  sum_dur <- sum_ddd <- c_ddd_pack <- c_dur_pack <- c_ddd_atc <- NULL
  erfl_met <- erfl_pack_based <- date <- NULL

  p[, day := rleid(id, atc, date)]
  p[, day_i := rowid(day)]
  p[, day_n := .pre2dup_group_size(day)]
  p[, let(
    has_low = as.integer(!is.na(ddd_low_pack)),
    has_usual = as.integer(!is.na(ddd_usual_pack)),
    has_dur = as.integer(!is.na(dur_usual_pack)),
    has_usual_atc = as.integer(!is.na(ddd_usual_atc)),
    ddd_pos = as.integer(ddd > 0),
    min_dur_one = fifelse(
      !is.na(dur_min_pack),
      ratio * dur_min_pack,
      ratio * dur_min_atc
    ),
    max_dur_one = fifelse(
      !is.na(dur_max_pack),
      ratio * dur_max_pack,
      ratio * dur_max_atc
    )
  )]
  p[,
    let(
      all_low = min(has_low),
      low_min = min(ddd_low_pack),
      all_usual = min(has_usual),
      all_dur = min(has_dur),
      all_usual_atc = min(has_usual_atc),
      all_pos = min(ddd_pos),
      min_dur = max(min_dur_one),
      max_dur = max(max_dur_one),
      max_ratio = max(ratio)
    ),
    by = day
  ]
  # A missing package DDD lower limit on the day means the ATC limit.
  p[, min_ddd := fifelse(all_low == 1L, low_min, ddd_low_atc)]
  p[, min_dur := pmax(pmin(min_dur, global_max), global_min)]
  p[, max_dur := pmin(pmax(max_dur, global_min), global_max)]

  # .count_erfl(), with its four rules in the source's order.
  p[, let(
    sum_ratio = .pre2dup_group_sum(ratio, day_i, day_n),
    sum_ddd_pack = .pre2dup_group_sum(ddd / ddd_usual_pack, day_i, day_n),
    sum_dur = .pre2dup_group_sum(ratio * dur_usual_pack, day_i, day_n),
    sum_ddd = .pre2dup_group_sum(ddd, day_i, day_n)
  )]
  p[, let(
    c_ddd_pack = all_usual == 1L & all_pos == 1L,
    c_dur_pack = all_dur == 1L,
    c_ddd_atc = all_usual_atc == 1L & all_pos == 1L
  )]
  p[,
    erfl_met := fcase(
      c_ddd_pack      ,
      "erfl_ddd_pack" ,
      c_dur_pack      ,
      "erfl_dur_pack" ,
      c_ddd_atc       ,
      "erfl_ddd_atc"  ,
      default = "erfl_min_atc"
    )
  ]
  p[,
    erfl_pack_based := fcase(
      c_ddd_pack                           ,
      max_ratio / sum_ratio * sum_ddd_pack ,
      c_dur_pack                           ,
      max_ratio / sum_ratio * sum_dur      ,
      c_ddd_atc                            ,
      sum_ddd / ddd_usual_atc              ,
      default = dur_min_atc
    )
  ]

  # The daily DDD total: the source's GForce sum, so the same arithmetic.
  p[, ddd := sum(ddd), by = day]
  p <- unique(p, by = "day")
  keep <- c(
    "id",
    "atc",
    "date",
    "ddd",
    "min_ddd",
    "min_dur",
    "max_dur",
    "erfl_met",
    "erfl_pack_based"
  )
  p <- p[, keep, with = FALSE]
  setorder(p, id, atc, date)
  return(p)
}

# Step 2: time between purchases and the blocks, which a gap over
# global_gap_max starts.
.pre2dup_blocks <- function(p, global_gap_max) {
  id <- atc <- ia <- btw <- startp <- block <- block_i <- NULL
  block_length <- date <- NULL
  p[, ia := rleid(id, atc)]
  p[, btw := date - .pre2dup_shift(date, ia)]
  p[is.na(btw), btw := 0L]
  p[, startp := rowid(ia) == 1L | btw > global_gap_max]
  p[, block := cumsum(startp)]
  p[, block_i := rowid(block)]
  p[, block_length := .pre2dup_group_size(block)]
  return(invisible(p))
}

# Step 2: the sliding average of daily DDDs (.count_sliding_average) and its
# relative standard deviation (.f_rsd), for blocks of more than two
# purchases.
.pre2dup_sliding_average <- function(
  p,
  w_past,
  w_present,
  w_future,
  w_limit,
  max_refill
) {
  sliding_average <- rsd <- block_length <- NULL
  i <- p$block_i
  n <- p$block_length
  d <- p$ddd
  t <- p$btw
  b <- p$block
  d_prev <- .pre2dup_shift(d, b)
  d_next <- .pre2dup_shift(d, b, type = "lead")
  t_next <- .pre2dup_shift(t, b, type = "lead")
  t_next2 <- .pre2dup_shift(t, b, n = 2L, type = "lead")

  # The estimated time after the last purchase, read on the last row of the
  # block and copied to every row of it.
  te_last <- pmin(d / (d_prev / t), max_refill)
  te <- te_last[seq_along(i) - i + n]

  first <- (.pre2dup_nz(w_limit * d) + .pre2dup_nz(w_future * d_next)) /
    (.pre2dup_nz(w_limit * t_next) + .pre2dup_nz(w_future * t_next2))
  use_time <- fifelse(i + 2L > n, te, as.numeric(t_next2))
  mid <- (.pre2dup_nz(w_past * d_prev) +
    .pre2dup_nz(w_present * d) +
    .pre2dup_nz(w_future * d_next)) /
    (.pre2dup_nz(w_past * t) +
      .pre2dup_nz(w_present * t_next) +
      .pre2dup_nz(w_future * use_time))
  last <- (.pre2dup_nz(w_limit * d) + .pre2dup_nz(w_limit * d_prev)) /
    (.pre2dup_nz(w_limit * te) + .pre2dup_nz(w_limit * t))
  sa <- fcase(
    i == 1L , first , i < n , mid ,
    default = last
  )
  sa[n <= 2L] <- NA_real_
  p[, sliding_average := pmin(sa, 99)]

  # .f_rsd() per block, with mean() and sum() as the source computes them.
  p[, rsd := NA_real_]
  idx <- which(n > 2L)
  if (length(idx) > 0L) {
    x <- p$sliding_average[idx]
    pos <- i[idx]
    len <- n[idx]
    m <- .pre2dup_group_mean(x, pos, len)
    s <- .pre2dup_group_sum(((x - m)^2) / len, pos, len)
    set(p, idx, "rsd", pmin(0.5, pmax(0.2, sqrt(s) / m)))
  }
  return(invisible(p))
}

# Step 2: the ERFL of each purchase, and a new period where it does not reach
# the next purchase.
.pre2dup_erfl <- function(p, global_ddd_high, trace = FALSE) {
  sliding_average <- min_ddd <- erfl_ddd <- ddd <- rsd <- erfl <- NULL
  erfl_pack_based <- erfl_method <- erfl_met <- min_dur <- max_dur <- NULL
  startp <- btw <- joined_by <- NULL
  p[, sliding_average := pmin(pmax(sliding_average, min_ddd), global_ddd_high)]
  p[, erfl_ddd := ddd * (1 + rsd) / sliding_average]
  # The source writes `pre_ddd > 0 & ...` here. In its scope pre_ddd is the
  # column NAME, a string, so the term compares a string with "0" and is TRUE
  # for a name such as "ddd". erfl_ddd > 0 already holds exactly when the
  # daily DDDs are above 0, so the port leaves the term out.
  p[,
    erfl := fifelse(!is.na(erfl_ddd) & erfl_ddd > 0, erfl_ddd, erfl_pack_based)
  ]
  p[,
    erfl_method := fifelse(
      !is.na(erfl_ddd) & erfl_ddd > 0,
      "erfl_ddd",
      erfl_met
    )
  ]
  p[,
    erfl := fifelse(
      erfl_method == "erfl_min_atc",
      erfl,
      pmin(pmax(min_dur, erfl), max_dur)
    )
  ]
  p[shift(erfl) < btw & !startp, startp := TRUE]
  if (trace) {
    # A purchase that is not a period start here is reached by the ERFL of
    # the previous purchase.
    p[, joined_by := fifelse(startp, "first", "duration")]
  }
  return(invisible(p))
}

# The trace label of a join that an extended duration makes: "duration" when
# the previous ERFL reaches the purchase, "extended" when the days_covered
# extension also needs to, and "hospital" when only the hospital days make
# the extended duration reach it.
.pre2dup_reach_label <- function(prev_erfl, btw, days_covered) {
  return(fcase(
    prev_erfl >= btw                ,
    "duration"                      ,
    prev_erfl + days_covered >= btw ,
    "extended"                      ,
    default = "hospital"
  ))
}

# Step 2: hospital days during each exposure, and the extension of the ERFL
# by them and by days_covered across the gap to the next purchase.
.pre2dup_extend <- function(
  p,
  stays,
  global_gap_max,
  global_hosp_max,
  days_covered,
  trace = FALSE
) {
  purc_end <- date <- erfl <- next_purchase <- ia <- hosp_days <- id <- NULL
  erfl_extended <- prev_extended <- block <- startp <- btw <- NULL
  joined_by <- NULL
  p[, purc_end := date + erfl]
  p[, next_purchase := .pre2dup_shift(date, ia, type = "lead")]
  if (is.null(stays)) {
    p[, hosp_days := 0]
  } else {
    p[,
      hosp_days := .pre2dup_hosp_days(id, date, purc_end, next_purchase, stays)
    ]
  }
  p[, erfl_extended := erfl + pmin(hosp_days, global_hosp_max) + days_covered]
  p[, prev_extended := .pre2dup_shift(erfl_extended, block)]
  join <- p[,
    startp == TRUE &
      btw > 0 &
      btw <= global_gap_max &
      !is.na(prev_extended) &
      prev_extended >= btw
  ]
  if (trace) {
    prev_erfl <- .pre2dup_shift(p$erfl, p$block)
    p[
      join,
      joined_by := .pre2dup_reach_label(prev_erfl[join], btw, days_covered)
    ]
  }
  p[join, startp := FALSE]
  return(invisible(p))
}

# Step 3: stockpiling. Inside a block of more than two purchases, a purchase
# whose sliding average drops and whose ERFL does not reach the next purchase
# joins the next one when its DDDs and the previous purchase's cover both
# gaps.
#
# The source selects these rows with a logical vector it computed before it
# re-sorted the rows (pre2dup.R:535 and :584). When the string order of
# paste(id, atc) differs from the sorted order of the ids, as for the ids 9
# and 10, that vector selects other rows. The port selects the rows of blocks
# of more than two purchases, which is the rule the source states.
.pre2dup_stockpiling <- function(
  p,
  global_hosp_max,
  days_covered,
  trace = FALSE
) {
  block_length <- drop <- pos_stock <- stock <- startp <- joined_by <- NULL
  b <- p$block
  sa <- p$sliding_average
  pre_ta <- .pre2dup_shift(sa, b)
  pas_ta <- .pre2dup_shift(sa, b, type = "lead")
  in_long <- p$block_length > 2L

  drop <- fifelse(
    !is.na(pre_ta) &
      !is.na(pas_ta) &
      pre_ta > sa &
      (pas_ta > sa | (sa > pas_ta & p$block_i == (p$block_length - 1L))),
    TRUE,
    FALSE
  )
  next_startp <- .pre2dup_shift(p$startp, b, type = "lead")
  pos_stock <- fifelse(drop == TRUE & next_startp == TRUE, TRUE, FALSE)

  sp_ddd <- .pre2dup_shift(p$ddd, b) + p$ddd
  sp_btw <- p$btw + .pre2dup_shift(p$btw, b, type = "lead")
  sp_erfl <- sp_ddd * (1 + p$rsd) / sa
  sp_max <- .pre2dup_shift(p$max_dur, b) + p$max_dur
  sp_erfl <- pmin(sp_erfl, sp_max)
  sp_hosp_days <- .pre2dup_shift(p$hosp_days, b) + p$hosp_days
  sp_erfl_extended <- sp_erfl +
    pmin(sp_hosp_days, global_hosp_max) +
    days_covered
  stock <- fifelse(pos_stock == TRUE & sp_erfl_extended >= sp_btw, TRUE, FALSE)
  stock[!in_long] <- NA

  joins <- shift(stock)
  joins[is.na(joins)] <- FALSE
  if (trace) {
    p[joins & startp, joined_by := "stockpiling"]
  }
  p[joins, startp := FALSE]
  return(invisible(p))
}

# Step 3: periods of fewer than three purchases use the package-based ERFL,
# and join the previous purchase when its extended ERFL reaches them.
.pre2dup_short_periods <- function(
  p,
  stays,
  global_hosp_max,
  days_covered,
  trace = FALSE
) {
  period <- startp <- period_length <- erfl <- erfl_pack_based <- NULL
  erfl_method <- erfl_met <- purc_end <- date <- hosp_days <- id <- NULL
  next_purchase <- erfl_extended <- prev_dur <- block <- btw <- NULL
  joined_by <- NULL
  p[, period := cumsum(startp)]
  p[, period_length := .pre2dup_group_size(period)]
  short <- p$period_length < 3L
  p[short, let(erfl = erfl_pack_based, erfl_method = erfl_met)]
  p[short, purc_end := date + erfl]
  if (!is.null(stays) && any(short)) {
    p[
      short,
      hosp_days := .pre2dup_hosp_days(id, date, purc_end, next_purchase, stays)
    ]
  }
  p[
    short,
    erfl_extended := erfl + pmin(hosp_days, global_hosp_max) + days_covered
  ]
  p[, prev_dur := .pre2dup_shift(erfl_extended, block)]
  join <- short & p[, !is.na(prev_dur) & prev_dur >= btw]
  if (trace) {
    prev_erfl <- .pre2dup_shift(p$erfl, p$block)
    p[
      join & startp,
      joined_by := .pre2dup_reach_label(
        prev_erfl[join & p$startp],
        btw,
        days_covered
      )
    ]
  }
  p[join, startp := FALSE]
  p[, period := cumsum(startp)]
  return(invisible(p))
}

# Step 5: the start and the end of each period. The end is the last purchase
# plus its duration plus the hospital days that start during it.
.pre2dup_period_ends <- function(p, stays, global_max_single, global_hosp_max) {
  period <- dup_start <- dup_last <- n_period <- date <- duration <- NULL
  rsd <- ddd <- sliding_average <- erfl <- min_dur <- max_dur <- NULL
  last_ends <- hosp_days_end <- dup_end <- id <- atc <- block <- NULL
  p[,
    let(dup_start = min(date), dup_last = max(date), n_period = .N),
    by = period
  ]
  n <- nrow(p)
  is_last <- c(p$period[-1L] != p$period[-n], TRUE)
  is_first <- c(TRUE, p$period[-1L] != p$period[-n])
  e <- p[is_last]
  e[,
    duration := fifelse(
      n_period > 2 & !is.na(rsd) & ddd > 0,
      (ddd * (1 + rsd) / (sliding_average * (1 + exp(-n_period)))),
      erfl
    )
  ]
  e[, duration := pmin(pmax(duration, min_dur), max_dur)]
  e[,
    duration := fifelse(
      n_period == 1,
      pmin(duration, global_max_single),
      duration
    )
  ]
  e[, last_ends := dup_last + duration]
  if (is.null(stays)) {
    e[, hosp_days_end := 0]
  } else {
    e[,
      hosp_days_end := .pre2dup_hosp_days(
        id,
        dup_last,
        last_ends,
        NA_integer_,
        stays
      )
    ]
  }
  e[, dup_end := dup_last + (duration + pmin(hosp_days_end, global_hosp_max))]
  per <- e[, list(id, atc, period, dup_start, dup_end)]
  per[, block := p$block[is_first]]
  return(per)
}

# Step 6: join a period to the previous one of the same person and ATC when
# the gap is at most post_process_perc percent of the previous period's
# length. The percentage falls by 0.1 on every pass, as in the source.
.pre2dup_post_process <- function(per, post_process_perc, trace = FALSE) {
  id <- atc <- ia <- prev_start <- prev_end <- dup_start <- dup_end <- NULL
  combine <- grp <- first_period <- period <- block <- NULL
  per[, first_period := period]
  merged <- integer()
  combinable <- TRUE
  while (combinable) {
    per[, ia := rleid(id, atc)]
    per[, let(
      prev_start = .pre2dup_shift(dup_start, ia),
      prev_end = .pre2dup_shift(dup_end, ia)
    )]
    per[,
      combine := (dup_start - prev_end) <=
        post_process_perc / 100 * (prev_end - prev_start)
    ]
    per[is.na(combine), combine := FALSE]
    post_process_perc <- post_process_perc - 0.1
    if (!any(per$combine)) {
      combinable <- FALSE
    } else {
      if (trace) {
        # The periods this pass joins to the previous period.
        merged <- c(merged, per$first_period[per$combine])
      }
      per[, grp := cumsum(!combine)]
      per <- per[,
        list(
          id = id[1L],
          atc = atc[1L],
          block = block[1L],
          first_period = first_period[1L],
          dup_start = dup_start[1L],
          dup_end = dup_end[.N]
        ),
        by = grp
      ]
    }
  }
  out <- per[, list(id, atc, block, first_period, dup_start, dup_end)]
  if (trace) {
    setattr(out, "merged", merged)
  }
  return(out)
}

# The output table: counts and DDD totals per final period, the period
# numbers, and the hospital days inside each period.
.pre2dup_collect <- function(p, per, stays) {
  final <- ddd <- NULL
  p[, final := findInterval(p$period, per$first_period)]
  # The source's GForce sum of the daily DDDs, so the same arithmetic.
  agg <- p[, list(n_purchases = .N, total_ddd = sum(ddd)), by = final]
  hospital_days <- if (is.null(stays)) {
    numeric(nrow(per))
  } else {
    .pre2dup_period_hosp_days(per$id, per$dup_start, per$dup_end, stays)
  }
  out <- data.table(
    id = per$id,
    atc = per$atc,
    block = per$block,
    period = seq_len(nrow(per)),
    start = as.IDate(per$dup_start),
    end = as.IDate(floor(per$dup_end)),
    n_purchases = agg$n_purchases,
    hospital_days = hospital_days,
    total_ddd = agg$total_ddd
  )
  return(out)
}

# The purchase trace: one row per purchase after the same-day aggregation. The
# first purchase of each period that post-processing joined to the previous
# period gets "merge", from the period numbers .pre2dup_post_process()
# recorded.
.pre2dup_trace <- function(p, merged) {
  period <- final <- id <- atc <- date <- block <- erfl <- joined_by <- NULL
  n <- nrow(p)
  first <- c(TRUE, p$period[-1L] != p$period[-n])
  p[first & p$period %in% merged, joined_by := "merge"]
  out <- p[, list(
    id,
    atc,
    date = as.IDate(date),
    block,
    period = final,
    duration = erfl,
    joined_by
  )]
  return(out[])
}
