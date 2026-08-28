#' Add prescription drug data to skeleton
#'
#' Searches for specific drug codes (ATC or product names) in Swedish prescription
#' registry data and adds corresponding boolean variables to the skeleton based on
#' prescription periods and duration of treatment.
#'
#' @section Coverage interval:
#' Each endpoint of the coverage interval is resolved once, independently:
#' \itemize{
#'   \item start: supplied \code{start_isoyearweek}, else the ISO week of the
#'     supplied \code{start_date}, else the ISO week of \code{edatum}.
#'   \item stop: supplied \code{stop_isoyearweek}, else the ISO week of the
#'     supplied \code{stop_date}, else the ISO week of
#'     \code{edatum + round(fddd) - 1}.
#' }
#' The \code{- 1} is because \code{foverlaps(type = "any")} matches inclusively
#' at both endpoints: without it a duration of N days would cover N + 1 days.
#'
#' Rows whose \code{round(fddd)} is missing, non-finite or not positive are
#' dropped, with one warning naming the count. This applies if and only if the
#' stop endpoint is actually resolved from \code{fddd}, that is when the caller
#' supplied neither \code{stop_isoyearweek} nor \code{stop_date}.
#'
#' @section Interval validation:
#' The resolved pair is then validated, on one rule that every combination of
#' supplied columns reaches. A row is dropped, with one warning naming the
#' count, when any of the following holds:
#' \itemize{
#'   \item either endpoint is missing;
#'   \item either endpoint is not a well-formed ISO week
#'     (\code{"YYYY-WW"} with week 01 to 53, or the annual \code{"YYYY-**"});
#'   \item the start week is later than the stop week;
#'   \item both endpoints came from dates and the start date is later than the
#'     stop date. This catches an interval inverted by days but contained in one
#'     ISO week, which compares equal as week strings.
#' }
#' An endpoint that is well formed but outside the skeleton's weeks is kept, not
#' dropped: \code{"2020-53"} is a real week that a skeleton ending in
#' \code{"2020-52"} does not carry, and the interval still covers every week
#' before it.
#'
#' @section Rows before the weekly spine:
#' \code{\link{create_skeleton}} builds an annual spine (\code{"<year>-**"},
#' \code{is_isoyear == TRUE}) for every ISO year before the weekly period. After
#' validation, any endpoint that falls before the first weekly row is remapped
#' onto the annual row of its ISO year, the same rule \code{\link{add_diagnoses}}
#' uses. A prescription that starts before the weekly period and ends inside it
#' therefore sets both the annual rows of the pre-weekly portion and the weekly
#' rows it covers.
#'
#' The remap applies to every endpoint, including one the caller supplied. A
#' supplied \code{start_isoyearweek} of \code{"2019-51"}, on a skeleton whose
#' weekly period starts in 2020, marks the 2019 annual row.
#'
#' @section The prescription table is not modified:
#' \code{add_rx()} computes \code{start_date}, \code{stop_date},
#' \code{start_isoyearweek} and \code{stop_isoyearweek} on a local working copy.
#' \code{lmed} is read, never written. Earlier versions wrote these helper
#' columns back into \code{lmed} by reference; because the ISO week columns
#' depend on the skeleton, reusing one \code{lmed} across two skeletons then
#' silently reused the first skeleton's values.
#'
#' @param skeleton A data.table containing the main skeleton structure created by \code{\link{create_skeleton}}
#' @param lmed A data.table containing prescription registry data (LMED).
#'   Must have columns for person ID, prescription date (edatum), treatment duration (fddd),
#'   and drug codes (atc) or product names (produkt)
#' @param id_name Character string specifying the name of the ID variable (default: "lopnr")
#' @param codes Named list of drug code patterns. Names become column
#'   names in the skeleton; values are character vectors. Matching
#'   semantics depend on \code{source} (see below).
#'
#'   Prefixing a pattern with \code{"!"} turns it into a *row-level
#'   veto*: any prescription whose code matches the (un-prefixed)
#'   pattern is masked and does not contribute to the named output
#'   column. Final rule: a prescription row contributes to the named
#'   column iff at least one un-prefixed pattern matches AND no
#'   \code{"!"} pattern matches.
#'
#'   Behaviour notes worth knowing:
#'   \itemize{
#'     \item \strong{Vetoes are independent per named code.} A
#'       \code{"!"} entry inside one list element does not leak into
#'       any other element of the same \code{codes} list. Two named
#'       codes can produce two completely different views of the same
#'       prescription rows.
#'     \item \strong{Veto match style follows \code{source}.} For
#'       \code{source = "atc"} the veto is prefix-based via
#'       \code{startsWith()}: \code{"!C10AA"} masks \code{C10AA01},
#'       \code{C10AA02}, ... For \code{source = "produkt"} the veto
#'       is exact-match via \code{\%chin\%}: \code{"!Sertralin"}
#'       does NOT mask \code{"Sertralin Sandoz"} because product
#'       names are exact, not prefixes.
#'     \item \strong{All-negative pattern set produces an empty
#'       column.} \code{c("!C10AA")} on its own gives an all-FALSE
#'       result -- without any positive pattern there is no set to
#'       carve from. Use a wider include + the negative, e.g.
#'       \code{c("C10A", "!C10AA")}.
#'     \item \strong{Per-(id, isoyearweek) aggregation respects the
#'       veto on a per-source-row basis.} The veto removes specific
#'       prescription rows from the matched set before the per-week
#'       aggregation runs. If a person has both a vetoed Rx and a
#'       non-vetoed Rx whose coverage windows overlap in the same
#'       skeleton week, the non-vetoed Rx still drives that week to
#'       TRUE -- the veto only kills its own row's contribution, not
#'       the whole week.
#'   }
#'
#'   Examples:
#'   \itemize{
#'     \item \code{c("N06A")} -- any antidepressant.
#'     \item \code{c("C10A", "!C10AA", "!C10AB")} -- any lipid-modifying
#'       agent except statins and fibrates.
#'   }
#'
#'   Default includes hormone therapy codes for puberty blockers
#'   (L02AE, H01CA). Common patterns include:
#'   \itemize{
#'     \item Antidepressants: \code{"N06A"}
#'     \item Hormone therapy: \code{"G03"}, \code{"L02AE"}, \code{"H01CA"}
#'     \item Cardiovascular drugs: \code{"C07"}, \code{"C08"}, \code{"C09"}
#'   }
#' @param rxs Deprecated. Use \code{codes} instead.
#' @param source Character string specifying search field and matching strategy:
#'   \itemize{
#'     \item "atc" (default) - Prefix matching in ATC codes (e.g., "N06A" matches "N06AB06").
#'       Uses \code{startsWith()} for fast C-level matching.
#'     \item "produkt" - Exact matching on product names (e.g., "Delestrogen" matches
#'       only "Delestrogen", not "Delestrogen Extra"). Uses \code{\%chin\%} for fast lookup.
#'   }
#' @return The skeleton data.table is modified by reference with prescription variables added.
#'   Variables are TRUE during periods when the prescription is active based on start/stop dates
#'   calculated from prescription date + treatment duration
#' @examples
#' # Load fake data
#' data("fake_person_ids", package = "swereg")
#' data("fake_prescriptions", package = "swereg")
#' swereg::make_lowercase_names(fake_prescriptions, date_columns = "edatum")
#'
#' # Create skeleton
#' skeleton <- create_skeleton(fake_person_ids[1:10], "2020-01-01", "2020-12-31")
#'
#' # Add prescription data
#' rx_patterns <- list(
#'   "antidepressants" = c("N06A"),
#'   "hormones" = c("G03", "L02AE")
#' )
#' add_rx(skeleton, fake_prescriptions, "p444_lopnr_personnr", rx_patterns, "atc")
#' @seealso \code{\link{create_skeleton}} for creating the skeleton structure,
#'   \code{\link{add_diagnoses}} for diagnosis codes,
#'   \code{\link{add_operations}} for surgical procedures,
#'   \code{\link{make_lowercase_names}} for data preprocessing
#' @family data_integration
#' @importFrom utils head
#' @export
add_rx <- function(
    skeleton,
    lmed,
    id_name = "lopnr",
    codes = list(
      "rx_hormones_pubblock"= c(
        "L02AE",
        "H01CA"
      )
    ),
    source = "atc",
    rxs = NULL
){
  # Backwards compatibility: accept old parameter name
  if (!is.null(rxs)) {
    warning("'rxs' is deprecated, use 'codes' instead.", call. = FALSE)
    codes <- rxs
  }

  # The caller's own expression and frame. `.ensure_dt_alloc()` writes the
  # grown skeleton back to that binding. See R/dt_alloc.R.
  skeleton_expr <- substitute(skeleton)
  caller_env <- parent.frame()

  # Declare variables for data.table non-standard evaluation
  . <- NULL
  start_isoyearweek <- stop_isoyearweek <- temp <- d <- NULL
  start_date <- edatum <- stop_date <- fddd <- atc <- id <- isoyearweek <- produkt <- NULL
  is_isoyear <- NULL
  rx_name <- iyw_int <- iyw_int_end <- start_int <- stop_int <- NULL
  iyw_start <- iyw_stop <- rx_row_id <- NULL

  # Validate inputs
  validate_skeleton_structure(skeleton)
  validate_id_column(lmed, id_name)
  validate_prescription_data(lmed)
  validate_pattern_list(codes, "prescription patterns")
  validate_date_columns(lmed, c("edatum"), "prescription data")

  if (!source %in% c("atc", "produkt")) {
    stop(
      "source must be 'atc' or 'produkt', got: '",
      source,
      "'",
      call. = FALSE
    )
  }
  # Check that the source column exists
  if (!source %in% names(lmed)) {
    stop("Source column '", source, "' not found in prescription data.\n",
         "Available columns: ", paste(names(lmed), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(prescription_data)?", call. = FALSE)
  }

  codes <- expand_code_list(codes)

  # Check for ID matches
  skeleton_ids <- unique(skeleton$id)
  lmed_ids <- unique(lmed[[id_name]])
  matching_ids <- intersect(skeleton_ids, lmed_ids)

  if (length(matching_ids) == 0) {
    warning("No matching IDs found between skeleton and prescription data.\n",
            "Skeleton IDs (first 5): ", paste(head(skeleton_ids, 5), collapse = ", "), "\n",
            "Prescription IDs (first 5): ", paste(head(lmed_ids, 5), collapse = ", "), "\n",
            "Check that ID columns contain the same values.", call. = FALSE)
  }

  if (length(matching_ids) < length(skeleton_ids)) {
    warning("Only ", length(matching_ids), " out of ", length(skeleton_ids),
            " skeleton IDs found in prescription data. Some individuals will have no prescription data.", call. = FALSE)
  }

  # All derived state lives on a local working copy. add_rx() never writes a
  # derived column back into the caller's `lmed`. Only the columns used
  # downstream are copied, so the cost is a few columns rather than the table.
  #
  # WHICH COLUMNS THE CALLER SUPPLIED IS READ EXACTLY ONCE, HERE. Everything
  # after this block reads the RESOLVED endpoints and their provenance flags,
  # never the caller's column set. That is what stops the sixteen supplied-column
  # combinations from each needing their own reasoning. It is grep-checkable:
  #
  #   grep -n 'names(lmed)\|supplied_cols' R/add_rx.R
  #
  # Every hit must be inside input validation or inside this block. A hit below
  # the endpoint resolution is the defect this design exists to prevent.
  supplied_cols <- intersect(
    c("start_date", "stop_date", "start_isoyearweek", "stop_isoyearweek"),
    names(lmed)
  )
  work_cols <- intersect(
    unique(c(id_name, source, "edatum", "fddd", supplied_cols)),
    names(lmed)
  )
  work <- lmed[, work_cols, with = FALSE]

  has_start_isoyearweek <- "start_isoyearweek" %in% supplied_cols
  has_stop_isoyearweek <- "stop_isoyearweek" %in% supplied_cols
  has_start_date <- "start_date" %in% supplied_cols
  has_stop_date <- "stop_date" %in% supplied_cols

  # The stop endpoint comes from fddd only when the caller named neither a stop
  # week nor a stop date. This is provenance, not column presence: it is why a
  # stop_date column that plays no part in a fully-supplied ISO interval can no
  # longer change the result.
  stop_from_fddd <- !has_stop_isoyearweek && !has_stop_date

  # Duration filter. Applies if and only if the stop endpoint is resolved from
  # fddd. A prescription of zero or negative days is not coverage, and dropping
  # it after the ISO conversion cannot express that: once collapsed to weeks,
  # fddd = 0 and fddd = -1 both look like a valid single-week interval. Rows go
  # before the endpoint is materialised, so the arithmetic below never sees a
  # non-finite duration.
  #
  # `fddd` is touched ONLY inside this branch. When the caller supplied a stop
  # endpoint, fddd defines nothing, and a function that reads it anyway imposes
  # a contract the caller never agreed to: a non-numeric fddd would then error
  # even though it is unused.
  duration_days <- NULL
  if (stop_from_fddd) {
    duration_days <- round(work$fddd)
    drop_duration <- !is.finite(duration_days) | duration_days <= 0
    n_dropped_duration <- sum(drop_duration)
    if (n_dropped_duration > 0) {
      warning(n_dropped_duration, " prescription rows dropped before ISO week ",
              "conversion because fddd is missing, non-finite, or not positive", call. = FALSE)
      work <- work[!drop_duration]
      duration_days <- duration_days[!drop_duration]
    }
  }

  # STEP 1 -- resolve each endpoint to a (date, week) pair, independently.
  # The date is NA whenever the endpoint came from a supplied ISO week string:
  # that string, not any date column, is what defines the endpoint. The
  # interval end is duration_days - 1 days after edatum, because
  # foverlaps(type = "any") matches inclusively at both endpoints; without the
  # -1 a duration of N days would cover N + 1 days.
  na_date <- as.Date(rep(NA_real_, nrow(work)), origin = "1970-01-01")

  start_date_resolved <- if (has_start_isoyearweek) {
    na_date
  } else if (has_start_date) {
    work$start_date
  } else {
    work$edatum
  }
  stop_date_resolved <- if (has_stop_isoyearweek) {
    na_date
  } else if (has_stop_date) {
    work$stop_date
  } else {
    work$edatum + duration_days - 1
  }

  start_week <- if (has_start_isoyearweek) {
    as.character(work$start_isoyearweek)
  } else {
    cstime::date_to_isoyearweek_c(start_date_resolved)
  }
  stop_week <- if (has_stop_isoyearweek) {
    as.character(work$stop_isoyearweek)
  } else {
    cstime::date_to_isoyearweek_c(stop_date_resolved)
  }

  # STEP 2 -- validate the resolved pair. One expression, no branches, so every
  # supplied-column combination reaches it and none can pass it by. Order is
  # load-bearing: this runs BEFORE the remap, which collapses every pre-weekly
  # date of one ISO year onto a single string and would otherwise turn an
  # inverted interval into a valid-looking equal pair.
  #
  # The last term compares dates where both endpoints have one. It is vacuous
  # where either does not, and it catches what the week comparison cannot: an
  # interval inverted by days but contained in one ISO week compares equal as
  # week strings.
  #
  # Well-formedness rejects a week that does not exist, such as "2019-99", which
  # would otherwise be injected into the interval ranking as a synthetic
  # boundary. Two checks, because the second cannot be expressed as a pattern:
  #
  #   1. Shape: "YYYY-WW" with WW in 01..53, or the annual "YYYY-**" form.
  #   2. Calendar: whether a year HAS a week 53 depends on the year. 2020 does,
  #      2019 does not, so "2019-53" is as invalid as "2019-99". The last week of
  #      an ISO year is read off the package's own converter rather than a
  #      hand-rolled Thursday or leap-year rule, so this check cannot disagree
  #      with the conversion it guards: 28 December always falls in the final ISO
  #      week of its own ISO year.
  #
  # A year the converter cannot handle -- it supports roughly 1900 to 2200 --
  # yields NA rather than a week number. That is an unparseable year, which is
  # malformed input like "2019-99" and is DROPPED WITH THE WARNING. It must never
  # reach `if (n_dropped_interval > 0)` as NA, which would abort the whole call.
  # Hence `!is.na(last)` below, and the NA sweep after the expression: anything
  # this validation cannot establish as valid is invalid.
  #
  # It deliberately does NOT require the week to exist in the skeleton.
  # "2020-53" is a real week that a skeleton ending in "2020-52" does not carry,
  # and rejecting it would throw away every week of coverage before it.
  last_isoyearweek_of <- function(isoyear) {
    years <- unique(isoyear)
    december_28 <- suppressWarnings(as.Date(paste0(years, "-12-28")))
    lookup <- stats::setNames(
      as.integer(substr(
        cstime::date_to_isoyearweek_c(december_28), 6, 7
      )),
      years
    )
    return(unname(lookup[isoyear]))
  }
  well_formed <- function(week) {
    ok <- !is.na(week) &
      stringr::str_detect(week, "^[0-9]{4}-(0[1-9]|[1-4][0-9]|5[0-3]|\\*\\*)$")
    weekly <- which(ok & !stringr::str_detect(week, "\\*\\*$"))
    if (length(weekly) > 0) {
      last <- last_isoyearweek_of(substr(week[weekly], 1, 4))
      ok[weekly] <- !is.na(last) &
        as.integer(substr(week[weekly], 6, 7)) <= last
    }
    return(ok)
  }
  drop_interval <-
    is.na(start_week) | is.na(stop_week) |
      !well_formed(start_week) | !well_formed(stop_week) |
      start_week > stop_week |
      (!is.na(start_date_resolved) & !is.na(stop_date_resolved) &
        start_date_resolved > stop_date_resolved)
  # No NA may survive into the count. A row whose validity could not be
  # established is not valid.
  drop_interval[is.na(drop_interval)] <- TRUE

  n_dropped_interval <- sum(drop_interval)
  if (n_dropped_interval > 0) {
    warning(n_dropped_interval, " prescription rows dropped before matching ",
            "because the coverage interval is invalid: start_isoyearweek or ",
            "stop_isoyearweek is missing or malformed, or the interval ends ",
            "before it starts", call. = FALSE)
    work <- work[!drop_interval]
    start_week <- start_week[!drop_interval]
    stop_week <- stop_week[!drop_interval]
  }

  # STEP 3 -- remap both endpoints onto the annual rows, whatever their
  # provenance. create_skeleton() gives every ISO year before the weekly period
  # an annual row, and a prescription covering a week of such a year IS covered
  # by that annual row. A supplied endpoint is remapped on the same rule as a
  # derived one; preserving it unremapped would only make it match nothing.
  # This relies on every annual string sorting below every weekly string of the
  # same year, which holds under the locale collation R uses for `<`, `min()`
  # and `sort()` -- verified empirically, not derived from byte values.
  weekly_isoyearweek <- skeleton[is_isoyear == FALSE]$isoyearweek
  min_isoyearweek <- if (length(weekly_isoyearweek) > 0) min(weekly_isoyearweek) else NULL

  if (!is.null(min_isoyearweek)) {
    remap_to_isoyear <- function(week) {
      before <- which(week < min_isoyearweek)
      if (length(before) > 0) {
        week[before] <- paste0(substr(week[before], 1, 4), "-**")
      }
      return(week)
    }
    start_week <- remap_to_isoyear(start_week)
    stop_week <- remap_to_isoyear(stop_week)
  }

  # Attach under internal names. These cannot collide with a caller column,
  # because `work` carries only the projected columns above.
  work[, `:=`(
    iyw_start = start_week,
    iyw_stop = stop_week,
    rx_row_id = seq_len(.N)
  )]

  # Build tagged LMED: for each rx, filter matching records, tag with rx name.
  # Pattern syntax matches the add_diagnoses family:
  #   - bare patterns include via prefix (atc) or exact match (produkt)
  #   - "!"-prefixed patterns exclude (row-level veto)
  # Final rule: row matches the named rx iff (any include hits) AND
  #   (no exclude hits). When only excludes are given, no rows match.
  # ATC codes are always prefixes  -> startsWith (C-level, ~5x faster than regex)
  # Product names are exact matches -> %chin% (data.table fast character %in%)
  tagged <- data.table::rbindlist(lapply(names(codes), function(rx) {
    patterns <- codes[[rx]]
    is_neg <- startsWith(patterns, "!")
    pos_patterns <- patterns[!is_neg]
    neg_patterns <- sub("^!", "", patterns[is_neg])

    if (source == "atc") {
      atc_vals <- work[["atc"]]
      if (!is.character(atc_vals)) atc_vals <- as.character(atc_vals)

      hits_pos <- if (length(pos_patterns)) {
        Reduce(`|`, lapply(pos_patterns, function(p) startsWith(atc_vals, p)))
      } else {
        rep(FALSE, length(atc_vals))
      }
      hits_neg <- if (length(neg_patterns)) {
        Reduce(`|`, lapply(neg_patterns, function(p) startsWith(atc_vals, p)))
      } else {
        rep(FALSE, length(atc_vals))
      }
      hits <- hits_pos & !hits_neg
      subset <- work[which(hits)]
    } else {
      hits_pos <- if (length(pos_patterns)) {
        work[["produkt"]] %chin% pos_patterns
      } else {
        rep(FALSE, nrow(work))
      }
      hits_neg <- if (length(neg_patterns)) {
        work[["produkt"]] %chin% neg_patterns
      } else {
        rep(FALSE, nrow(work))
      }
      subset <- work[which(hits_pos & !hits_neg)]
    }
    return(subset[, .(
      id = get(id_name),
      start_isoyearweek = iyw_start,
      stop_isoyearweek = iyw_stop,
      rx_name = rx,
      rx_row_id = rx_row_id
    )])
  }))

  # Guarantee the column-slot headroom the writes below need, before they run.
  # See R/dt_alloc.R for the defect this prevents.
  skeleton <- .ensure_dt_alloc(
    skeleton,
    n_new = sum(!names(codes) %in% names(skeleton)),
    x_expr = skeleton_expr,
    env = caller_env,
    fn_name = "add_rx()"
  )

  # Initialize all rx columns to FALSE
  for (rx in names(codes)) skeleton[, (rx) := FALSE]

  if (nrow(tagged) > 0) {
    # Prepare skeleton point-intervals for foverlaps
    # foverlaps requires numeric interval columns, so map isoyearweek to integer rank
    all_weeks <- sort(unique(c(
      skeleton$isoyearweek,
      tagged$start_isoyearweek,
      tagged$stop_isoyearweek
    )))
    week_to_int <- stats::setNames(seq_along(all_weeks), all_weeks)

    skel_pts <- unique(skeleton[, .(id, isoyearweek)])
    skel_pts[, iyw_int := week_to_int[isoyearweek]]
    skel_pts[, iyw_int_end := iyw_int]
    data.table::setkey(skel_pts, id, iyw_int, iyw_int_end)

    # foverlaps: find all skeleton points within LMED intervals
    tagged[, start_int := week_to_int[start_isoyearweek]]
    tagged[, stop_int := week_to_int[stop_isoyearweek]]
    # Backstop for intervals that are missing or inverted after the ISO week
    # conversion. It is UNREACHABLE BY CONSTRUCTION and kept only as a guard
    # against a future change to the validation above.
    #
    # Every endpoint that reaches here passed that validation, and every
    # endpoint is injected into `all_weeks`, so the integer lookup never yields
    # NA. Ordering survives because the remap is MONOTONIC: it maps each week
    # string to one that is less than or equal to it, and it preserves order
    # between any two strings, so a pair that was not inverted before the remap
    # cannot be inverted after it. Lowering alone would not be enough; the
    # order-preserving property is what carries the argument.
    keep <- !is.na(tagged$start_int) & !is.na(tagged$stop_int) &
      tagged$start_int <= tagged$stop_int
    # Count SOURCE ROWS, not tagged matches: one prescription matching two
    # requested codes is one dropped prescription, not two.
    n_dropped <- data.table::uniqueN(tagged$rx_row_id[!keep])
    tagged <- tagged[keep]
    if (n_dropped > 0) {
      warning(n_dropped, " prescription rows dropped after ISO week conversion ",
              "because start_isoyearweek or stop_isoyearweek is missing, or ",
              "start_isoyearweek is later than stop_isoyearweek", call. = FALSE)
    }
    data.table::setkey(tagged, id, start_int, stop_int)
    matches <- data.table::foverlaps(tagged, skel_pts, type = "any", nomatch = NULL)
    matches <- unique(matches[, .(id, isoyearweek, rx_name)])

    # Bulk update per rx
    for (rx in names(codes)) {
      skeleton[matches[rx_name == rx], on = .(id, isoyearweek), (rx) := TRUE]
    }
    return(invisible(NULL))
  }

}
