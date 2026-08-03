#' Add prescription drug data to skeleton
#'
#' Searches for specific drug codes (ATC or product names) in Swedish prescription
#' registry data and adds corresponding boolean variables to the skeleton based on
#' prescription periods and duration of treatment.
#'
#' @section Coverage interval:
#' When \code{stop_date} is not supplied, each prescription covers
#' \code{round(fddd)} days starting at \code{edatum}, i.e. the interval
#' \code{[edatum, edatum + round(fddd) - 1]}. Rows whose \code{round(fddd)} is
#' missing, non-finite or not positive are dropped before the ISO week
#' conversion, with one warning naming the number of rows dropped.
#'
#' Whenever both ISO week columns are derived, the interval is then validated as
#' dates, still before the ISO week conversion: a row with a missing
#' \code{start_date} or \code{stop_date}, or with \code{stop_date} before
#' \code{start_date}, is dropped with a second warning. This validation runs
#' before the annual remap described below, because the remap collapses every
#' pre-weekly date of one ISO year onto a single string and would otherwise turn
#' an inverted interval into a valid-looking one.
#'
#' @section Rows before the weekly spine:
#' \code{\link{create_skeleton}} builds an annual spine (\code{"<year>-**"},
#' \code{is_isoyear == TRUE}) for every ISO year before the weekly period. Any
#' end of the coverage interval that falls before the first weekly row is
#' remapped onto the annual row of its ISO year, the same rule
#' \code{\link{add_diagnoses}} uses. A prescription that starts before the
#' weekly period and ends inside it therefore sets both the annual rows of the
#' pre-weekly portion and the weekly rows it covers.
#'
#' Both behaviours apply only to values \code{add_rx()} derives itself. A caller
#' who supplies \code{start_date}, \code{stop_date}, \code{start_isoyearweek} or
#' \code{stop_isoyearweek} keeps those columns exactly as given.
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
#'       \code{startsWith()}: \code{"!N05AA"} masks \code{N05AA01},
#'       \code{N05AA02}, ... For \code{source = "produkt"} the veto
#'       is exact-match via \code{\%chin\%}: \code{"!Sertralin"}
#'       does NOT mask \code{"Sertralin Sandoz"} because product
#'       names are exact, not prefixes.
#'     \item \strong{All-negative pattern set produces an empty
#'       column.} \code{c("!N05AA")} on its own gives an all-FALSE
#'       result -- without any positive pattern there is no set to
#'       carve from. Use a wider include + the negative, e.g.
#'       \code{c("N05A", "!N05AA")}.
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
#'     \item \code{c("N05A", "!N05AA", "!N05AB")} -- any antipsychotic
#'       except first-generation typical agents.
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

  # Declare variables for data.table non-standard evaluation
  . <- NULL
  start_isoyearweek <- stop_isoyearweek <- temp <- d <- NULL
  start_date <- edatum <- stop_date <- fddd <- atc <- id <- isoyearweek <- produkt <- NULL
  is_isoyear <- NULL
  rx_name <- iyw_int <- iyw_int_end <- start_int <- stop_int <- NULL

  # Validate inputs
  validate_skeleton_structure(skeleton)
  validate_id_column(lmed, id_name)
  validate_prescription_data(lmed)
  validate_pattern_list(codes, "prescription patterns")
  validate_date_columns(lmed, c("edatum"), "prescription data")

  if (!source %in% c("atc", "produkt")) {
    stop("source must be 'atc' or 'produkt', got: '", source, "'")
  }
  # Check that the source column exists
  if (!source %in% names(lmed)) {
    stop("Source column '", source, "' not found in prescription data.\n",
         "Available columns: ", paste(names(lmed), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(prescription_data)?")
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
            "Check that ID columns contain the same values.")
  }

  if (length(matching_ids) < length(skeleton_ids)) {
    warning("Only ", length(matching_ids), " out of ", length(skeleton_ids),
            " skeleton IDs found in prescription data. Some individuals will have no prescription data.")
  }

  # All derived state lives on a local working copy. add_rx() never writes a
  # derived column back into the caller's `lmed`: doing so made the cached
  # values skeleton-dependent (the annual remap below depends on the skeleton),
  # and the "already present" guards would then reuse one skeleton's remap on a
  # later call with a different skeleton. Only the columns used downstream are
  # copied, so the cost is a few columns rather than the whole table.
  supplied_cols <- intersect(
    c("start_date", "stop_date", "start_isoyearweek", "stop_isoyearweek"),
    names(lmed)
  )
  work_cols <- intersect(
    unique(c(id_name, source, "edatum", "fddd", supplied_cols)),
    names(lmed)
  )
  work <- lmed[, work_cols, with = FALSE]

  derive_start_date <- !"start_date" %in% supplied_cols
  derive_stop_date <- !"stop_date" %in% supplied_cols
  derive_start_isoyearweek <- !"start_isoyearweek" %in% supplied_cols
  derive_stop_isoyearweek <- !"stop_isoyearweek" %in% supplied_cols

  if (derive_start_date) work[, start_date := edatum]

  # Derive stop_date from fddd only when the caller has not supplied one.
  # Two things happen here, and only on this derived path:
  #   1. Rows whose duration is missing, non-finite or not positive are dropped
  #      BEFORE the ISO week conversion. A prescription of zero or negative days
  #      is not coverage, and dropping it after conversion cannot express that:
  #      once collapsed to weeks, fddd = 0 and fddd = -1 both look like a
  #      single-week interval and survive the post-conversion inverted-interval
  #      filter below.
  #   2. The interval end is `duration_days - 1` days after edatum, because
  #      foverlaps(type = "any") matches inclusively at both endpoints. Without
  #      the -1, a duration of N days covers N + 1 days.
  if (derive_stop_date) {
    duration_days <- round(work$fddd)
    drop_duration <- !is.finite(duration_days) | duration_days <= 0
    n_dropped_duration <- sum(drop_duration)
    if (n_dropped_duration > 0) {
      warning(n_dropped_duration, " prescription rows dropped before ISO week ",
              "conversion because fddd is missing, non-finite, or not positive")
      work <- work[!drop_duration]
      duration_days <- duration_days[!drop_duration]
    }
    work[, stop_date := edatum + duration_days - 1]
  }

  # Validate the interval as DATES, before any ISO conversion or remap. Order is
  # load-bearing: the remap below collapses every pre-weekly date of one ISO year
  # onto a single annual string, so an inverted interval whose endpoints share a
  # year would come out of the remap as an equal pair and read as valid coverage.
  # Only meaningful when both ISO week columns are derived from these dates; if
  # the caller supplied an ISO week column, the dates are not the interval and
  # the post-conversion filter is the only available backstop.
  if (derive_start_isoyearweek && derive_stop_isoyearweek) {
    drop_interval <- is.na(work$start_date) | is.na(work$stop_date) |
      work$start_date > work$stop_date
    n_dropped_interval <- sum(drop_interval)
    if (n_dropped_interval > 0) {
      warning(n_dropped_interval, " prescription rows dropped before ISO week ",
              "conversion because the coverage interval is invalid as dates ",
              "(missing start_date or stop_date, or stop_date before start_date)")
      work <- work[!drop_interval]
    }
  }

  # Events before the weekly spine are remapped onto the annual ("YYYY-**") rows,
  # the same rule the add_diagnoses / add_quality_registry family uses. Both
  # endpoints are remapped independently, so a prescription that starts before
  # the weekly period and ends inside it marks the annual row of its start year
  # AND the weekly rows it actually covers. This relies on every annual string
  # sorting below every weekly string of the same year, which holds under the
  # locale collation R uses for `<`, `min()` and `sort()` -- verified empirically
  # rather than derived from byte values. The integer ranking used by foverlaps()
  # below therefore keeps that interval contiguous and free of artefacts.
  # Only the derived path is remapped: a caller-supplied ISO week column is kept
  # exactly as given.
  weekly_isoyearweek <- skeleton[is_isoyear == FALSE]$isoyearweek
  min_isoyearweek <- if (length(weekly_isoyearweek) > 0) min(weekly_isoyearweek) else NULL

  if (derive_start_isoyearweek) {
    work[, start_isoyearweek := cstime::date_to_isoyearweek_c(start_date)]
    if (!is.null(min_isoyearweek)) {
      work[
        start_isoyearweek < min_isoyearweek,
        start_isoyearweek := paste0(cstime::date_to_isoyear_c(start_date), "-**")
      ]
    }
  }
  if (derive_stop_isoyearweek) {
    work[, stop_isoyearweek :=  cstime::date_to_isoyearweek_c(stop_date)]
    if (!is.null(min_isoyearweek)) {
      work[
        stop_isoyearweek < min_isoyearweek,
        stop_isoyearweek := paste0(cstime::date_to_isoyear_c(stop_date), "-**")
      ]
    }
  }

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
    subset[, .(id = get(id_name), start_isoyearweek, stop_isoyearweek, rx_name = rx)]
  }))

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
    # Backstop for intervals that are still missing or inverted after the ISO
    # week conversion. On the derived path the date-level validation above has
    # already removed these; this remains reachable when the caller supplies
    # start_isoyearweek or stop_isoyearweek, which are never validated as dates.
    n_before <- nrow(tagged)
    tagged <- tagged[!is.na(start_int) & !is.na(stop_int) & start_int <= stop_int]
    n_dropped <- n_before - nrow(tagged)
    if (n_dropped > 0) {
      warning(n_dropped, " prescription rows dropped after ISO week conversion ",
              "because start_isoyearweek is missing, unknown to the skeleton, ",
              "or later than stop_isoyearweek")
    }
    data.table::setkey(tagged, id, start_int, stop_int)
    matches <- data.table::foverlaps(tagged, skel_pts, type = "any", nomatch = NULL)
    matches <- unique(matches[, .(id, isoyearweek, rx_name)])

    # Bulk update per rx
    for (rx in names(codes)) {
      skeleton[matches[rx_name == rx], on = .(id, isoyearweek), (rx) := TRUE]
    }
  }

}
