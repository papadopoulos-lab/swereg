# =============================================================================
# Fill the follow-up confounders of a trial panel
# =============================================================================
# The censoring model needs a value on every follow-up row. This file carries
# the last observed value forward inside each person-trial, seeded by the
# entry-window snapshot, and counts what the carry filled.
# =============================================================================

#' Name the confounders this file can fill.
#'
#' A confounder qualifies when the panel holds both its plain column and its
#' `.tte_entry__` snapshot column.
#'
#' @param trial A [TTEEnrollment] object.
#' @return A character vector of confounder names, possibly empty.
#' @noRd
.tte_fill_vars <- function(trial) {
  vars <- trial$design$confounder_vars
  if (length(vars) == 0L) {
    return(character(0))
  }
  cols <- names(trial$data)
  keep <- (vars %in% cols) & (.tte_entry_col(vars) %in% cols)
  return(vars[keep])
}

#' Measure the NA run at the start of one person-trial.
#'
#' The run counts only the rows an imputed entry value reaches. A person-trial
#' whose snapshot is observed contributes zero.
#'
#' @param x The plain confounder values of one person-trial, ordered by
#'   `tstart_var` and then `tstop_var`.
#' @param s The entry-window snapshot values of the same person-trial.
#' @return One integer.
#' @noRd
.tte_fill_leading_na_run <- function(x, s) {
  if (!is.na(s[1L])) {
    return(0L)
  }
  observed <- which(!is.na(x))
  if (length(observed) == 0L) {
    return(length(x))
  }
  return(observed[1L] - 1L)
}

#' Read an aggregates table from either accepted argument type.
#'
#' @param x A [TTEEnrollment] object or a `tteenrollment_fill_aggregates` table.
#' @param what Character, the argument name to name in the error.
#' @return A `tteenrollment_fill_aggregates` table.
#' @noRd
.tte_fill_aggregates_of <- function(x, what) {
  if (inherits(x, "tteenrollment_fill_aggregates")) {
    return(x)
  }
  if (inherits(x, "TTEEnrollment")) {
    return(tteenrollment_fill_aggregates(x))
  }
  stop(
    "`",
    what,
    "` must be a TTEEnrollment object or a table from ",
    "tteenrollment_fill_aggregates().",
    call. = FALSE
  )
}

#' Carry the last observed confounder value forward through follow-up
#'
#' The censoring model needs a value on every follow-up row. This function
#' fills each plain confounder column of a trial panel. It carries the last
#' observed value forward within each person-trial.
#'
#' The carry starts from the entry-window snapshot, so the first row of a
#' person-trial is never left `NA`. `$s1_impute_confounders()` supplies that
#' snapshot value for a person-trial with no observed value before follow-up.
#'
#' The function reads `trial$design` for every column name. It fills only the
#' confounders that carry a `.tte_entry__` snapshot column. A panel with no
#' snapshot column at all comes back untouched.
#'
#' The function sorts `trial$data` by `id_var`, then `tstart_var`, then
#' `tstop_var`. It modifies `trial$data` by reference. It appends
#' `"fill_followup"` to `trial$steps_completed` once. A second call appends no
#' second token and changes no value.
#'
#' The function stops when a filled row would still hold `NA`. That happens
#' when the snapshot is `NA` and no earlier row of the same person-trial holds
#' a value.
#'
#' @param trial A [TTEEnrollment] object at trial level.
#' @return The [TTEEnrollment] object, invisibly.
#'
#' @examples
#' d <- data.table::data.table(
#'   enrollment_person_trial_id = rep(c("a", "b"), each = 3L),
#'   tstart = rep(c(0L, 4L, 8L), 2L),
#'   tstop = rep(c(4L, 8L, 12L), 2L),
#'   exposed = rep(c(TRUE, FALSE), each = 3L),
#'   event = 0L,
#'   income = c(NA, "mid", NA, NA, NA, "high"),
#'   .tte_entry__income = rep(c("low", "mid"), each = 3L)
#' )
#' design <- TTEDesign$new(
#'   treatment_var = "exposed",
#'   outcome_vars = "event",
#'   confounder_vars = "income",
#'   follow_up_time = 12L
#' )
#' trial <- TTEEnrollment$new(d, design)
#' tteenrollment_fill_followup_confounders(trial)
#' trial$data$income
#'
#' @family tte_methods
#' @seealso [tteenrollment_fill_aggregates()] for the counts the fill changes.
#' @export
tteenrollment_fill_followup_confounders <- function(trial) {
  pos <- pos_locf <- NULL

  vars <- .tte_fill_vars(trial)
  if (length(vars) == 0L) {
    return(invisible(trial))
  }

  design <- trial$design
  id_var <- design$id_var
  d <- trial$data
  data.table::setorderv(d, c(id_var, design$tstart_var, design$tstop_var))

  for (v in vars) {
    x <- d[[v]]
    s <- d[[.tte_entry_col(v)]]

    # Carry a row index, not a value, so the fill is type agnostic. Index 0
    # means "take the entry-window snapshot", and it seeds every person-trial.
    idx <- seq_along(x)
    idx[is.na(x)] <- NA_integer_
    carry <- data.table::data.table(pos = idx)
    carry[, (id_var) := d[[id_var]]]
    carry[,
      pos_locf := data.table::nafill(c(0L, pos), type = "locf")[-1L],
      by = c(id_var)
    ]
    src <- carry$pos_locf

    gaps <- which(is.na(x))
    from_entry <- gaps[src[gaps] == 0L]
    from_earlier <- gaps[src[gaps] > 0L]

    unfilled <- from_entry[is.na(s[from_entry])]
    if (length(unfilled) > 0L) {
      stop(
        "Confounder '",
        v,
        "' has no value to carry forward in ",
        data.table::uniqueN(d[[id_var]][unfilled]),
        " person-trial(s). The entry-window snapshot is NA there, and no ",
        "earlier follow-up row holds a value. Run $s1_impute_confounders() ",
        "first: it supplies the seed value.",
        call. = FALSE
      )
    }

    filled <- x
    filled[from_entry] <- s[from_entry]
    filled[from_earlier] <- x[src[from_earlier]]
    data.table::set(d, j = v, value = filled)
  }

  if (!("fill_followup" %in% trial$steps_completed)) {
    trial$steps_completed <- c(trial$steps_completed, "fill_followup")
  }
  return(invisible(trial))
}

#' @include r6_tteenrollment.R
#' @description Step 1b: Fill the follow-up confounders by carrying the last
#'   observed value forward from the entry-window snapshot.
TTEEnrollment$set(
  "public",
  "s1b_fill_followup_confounders",
  function() {
    return(tteenrollment_fill_followup_confounders(self))
  }
)

#' Count the missing follow-up confounder values of a trial panel
#'
#' The function returns one row per confounder that carries a `.tte_entry__`
#' snapshot column. Run it once before the fill and once after it, then pass
#' both tables to [tteenrollment_fill_summary()].
#'
#' The function orders a local copy of the key columns. It never reorders
#' `trial$data`.
#'
#' @param trial A [TTEEnrollment] object at trial level.
#' @return A data.table of class `tteenrollment_fill_aggregates`, with columns:
#' \describe{
#'   \item{`confounder`}{The confounder name.}
#'   \item{`rows_n`}{Rows in the panel.}
#'   \item{`trials_n`}{Person-trials in the panel.}
#'   \item{`rows_na_n`}{Rows whose plain value is `NA`.}
#'   \item{`trials_na_n`}{Person-trials with at least one `NA` plain value.}
#'   \item{`trials_entry_na_n`}{Person-trials whose snapshot value is `NA`.}
#'   \item{`rows_leading_na_in_entry_na_trials_n`}{Rows the imputed snapshot
#'     value reaches. Within each person-trial whose snapshot is `NA`, it is
#'     the length of the `NA` run at the start of that person-trial.}
#'   \item{`key_digest`}{A digest of the ordered `id_var`, `tstart_var` and
#'     `tstop_var` columns. It is the same string on every row.}
#' }
#'
#' @examples
#' d <- data.table::data.table(
#'   enrollment_person_trial_id = rep(c("a", "b"), each = 3L),
#'   tstart = rep(c(0L, 4L, 8L), 2L),
#'   tstop = rep(c(4L, 8L, 12L), 2L),
#'   exposed = rep(c(TRUE, FALSE), each = 3L),
#'   event = 0L,
#'   income = c(NA, "mid", NA, NA, NA, "high"),
#'   .tte_entry__income = rep(c("low", "mid"), each = 3L)
#' )
#' design <- TTEDesign$new(
#'   treatment_var = "exposed",
#'   outcome_vars = "event",
#'   confounder_vars = "income",
#'   follow_up_time = 12L
#' )
#' tteenrollment_fill_aggregates(TTEEnrollment$new(d, design))
#'
#' @family tte_methods
#' @seealso [tteenrollment_fill_followup_confounders()] for the fill itself.
#' @export
tteenrollment_fill_aggregates <- function(trial) {
  vars <- .tte_fill_vars(trial)
  design <- trial$design
  d <- trial$data

  ord <- order(d[[design$id_var]], d[[design$tstart_var]], d[[design$tstop_var]])
  id <- d[[design$id_var]][ord]
  key_digest <- digest::digest(list(
    id,
    d[[design$tstart_var]][ord],
    d[[design$tstop_var]][ord]
  ))

  rows_n <- nrow(d)
  trials_n <- data.table::uniqueN(id)

  one <- function(v) {
    x <- d[[v]][ord]
    s <- d[[.tte_entry_col(v)]][ord]
    runs <- data.table::data.table(id = id, x = x, s = s)[,
      list(n = .tte_fill_leading_na_run(x, s)),
      by = "id"
    ]
    return(data.table::data.table(
      confounder = v,
      rows_n = rows_n,
      trials_n = trials_n,
      rows_na_n = sum(is.na(x)),
      trials_na_n = data.table::uniqueN(id[is.na(x)]),
      trials_entry_na_n = data.table::uniqueN(id[is.na(s)]),
      rows_leading_na_in_entry_na_trials_n = sum(runs$n),
      key_digest = key_digest
    ))
  }

  out <- if (length(vars) == 0L) {
    data.table::data.table(
      confounder = character(0),
      rows_n = integer(0),
      trials_n = integer(0),
      rows_na_n = integer(0),
      trials_na_n = integer(0),
      trials_entry_na_n = integer(0),
      rows_leading_na_in_entry_na_trials_n = integer(0),
      key_digest = character(0)
    )
  } else {
    data.table::rbindlist(lapply(vars, one))
  }

  data.table::setattr(
    out,
    "class",
    c("tteenrollment_fill_aggregates", "data.table", "data.frame")
  )
  return(out[])
}

#' Report what the follow-up confounder fill changed
#'
#' The function subtracts the aggregates of the filled panel from the
#' aggregates of the raw panel. The paper reports the result.
#'
#' Both arguments MUST describe the same panel. The function compares the
#' `key_digest` of the two tables and stops when they differ. It also stops
#' when the two confounder sets differ. It matches the rows of `imp` to the
#' rows of `raw` by confounder name. The two tables MAY therefore list the
#' same confounders in a different order.
#'
#' @param raw A [TTEEnrollment] object before the fill, or its aggregates
#'   table from [tteenrollment_fill_aggregates()].
#' @param imp A [TTEEnrollment] object after the fill, or its aggregates table.
#' @return A data.table, one row per confounder, with columns:
#' \describe{
#'   \item{`confounder`}{The confounder name.}
#'   \item{`rows_n`}{Rows in the panel.}
#'   \item{`rows_filled_n`}{Rows the fill gave a value.}
#'   \item{`trials_n`}{Person-trials in the panel.}
#'   \item{`trials_filled_n`}{Person-trials the fill left with no `NA`.}
#'   \item{`trials_entry_imputed_n`}{Person-trials whose snapshot value was
#'     `NA` in the raw panel.}
#'   \item{`rows_from_imputed_entry_n`}{Rows the imputed snapshot value
#'     reaches.}
#' }
#'
#' @examples
#' d <- data.table::data.table(
#'   enrollment_person_trial_id = rep(c("a", "b"), each = 3L),
#'   tstart = rep(c(0L, 4L, 8L), 2L),
#'   tstop = rep(c(4L, 8L, 12L), 2L),
#'   exposed = rep(c(TRUE, FALSE), each = 3L),
#'   event = 0L,
#'   income = c(NA, "mid", NA, NA, NA, "high"),
#'   .tte_entry__income = rep(c("low", "mid"), each = 3L)
#' )
#' design <- TTEDesign$new(
#'   treatment_var = "exposed",
#'   outcome_vars = "event",
#'   confounder_vars = "income",
#'   follow_up_time = 12L
#' )
#' trial <- TTEEnrollment$new(d, design)
#' raw <- tteenrollment_fill_aggregates(trial)
#' tteenrollment_fill_followup_confounders(trial)
#' tteenrollment_fill_summary(raw, trial)
#'
#' @family tte_methods
#' @seealso [tteenrollment_fill_aggregates()] for the input tables.
#' @export
tteenrollment_fill_summary <- function(raw, imp) {
  raw <- .tte_fill_aggregates_of(raw, "raw")
  imp <- .tte_fill_aggregates_of(imp, "imp")

  if (!identical(unique(raw$key_digest), unique(imp$key_digest))) {
    stop(
      "`raw` and `imp` are not the same panel. Their (",
      "id, tstart, tstop) key digests differ, so the two aggregate tables ",
      "MUST NOT be subtracted.",
      call. = FALSE
    )
  }
  if (!setequal(raw$confounder, imp$confounder)) {
    stop(
      "`raw` and `imp` cover different confounder sets: ",
      paste(sort(union(
        setdiff(raw$confounder, imp$confounder),
        setdiff(imp$confounder, raw$confounder)
      )), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  imp <- imp[match(raw$confounder, imp$confounder)]

  return(data.table::data.table(
    confounder = raw$confounder,
    rows_n = raw$rows_n,
    rows_filled_n = raw$rows_na_n - imp$rows_na_n,
    trials_n = raw$trials_n,
    trials_filled_n = raw$trials_na_n - imp$trials_na_n,
    trials_entry_imputed_n = raw$trials_entry_na_n,
    rows_from_imputed_entry_n = raw$rows_leading_na_in_entry_na_trials_n
  ))
}
