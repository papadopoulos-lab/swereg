#' Add prescription drug data to skeleton
#'
#' Searches for specific drug codes (ATC or product names) in Swedish prescription
#' registry data and adds corresponding boolean variables to the skeleton based on
#' prescription periods and duration of treatment.
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
#'   column. The veto applies independently per named code. Final
#'   rule: a prescription row contributes to the named column iff at
#'   least one un-prefixed pattern matches AND no \code{"!"} pattern
#'   matches. A list value containing only \code{"!"} patterns
#'   produces an all-FALSE column (no positive matcher to seed the
#'   set).
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

  if(!"start_date" %in% names(lmed)) lmed[, start_date := edatum]
  if(!"stop_date" %in% names(lmed)) lmed[, stop_date := edatum + round(fddd)]
  if(!"start_isoyearweek" %in% names(lmed)) lmed[, start_isoyearweek := cstime::date_to_isoyearweek_c(start_date)]
  if(!"stop_isoyearweek" %in% names(lmed)) lmed[, stop_isoyearweek :=  cstime::date_to_isoyearweek_c(stop_date)]

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
      atc_vals <- lmed[["atc"]]
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
      subset <- lmed[which(hits)]
    } else {
      hits_pos <- if (length(pos_patterns)) {
        lmed[["produkt"]] %chin% pos_patterns
      } else {
        rep(FALSE, nrow(lmed))
      }
      hits_neg <- if (length(neg_patterns)) {
        lmed[["produkt"]] %chin% neg_patterns
      } else {
        rep(FALSE, nrow(lmed))
      }
      subset <- lmed[which(hits_pos & !hits_neg)]
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
    # Remove rows with NA or inverted intervals (NA fddd, negative fddd)
    n_before <- nrow(tagged)
    tagged <- tagged[!is.na(start_int) & !is.na(stop_int) & start_int <= stop_int]
    n_dropped <- n_before - nrow(tagged)
    if (n_dropped > 0) {
      warning(n_dropped, " prescription rows dropped due to NA or negative fddd ",
              "(start_isoyearweek > stop_isoyearweek or missing dates)")
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
