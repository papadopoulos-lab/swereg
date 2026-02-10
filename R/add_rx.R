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
#' @param rxs Named list of drug code patterns to search for. Names become variable names in skeleton.
#'   Patterns should NOT include "^" prefix (automatically added). Default includes hormone therapy
#'   codes for puberty blockers (L02AE, H01CA). Common patterns include:
#'   \itemize{
#'     \item Antidepressants: "N06A"
#'     \item Hormone therapy: "G03", "L02AE", "H01CA"
#'     \item Cardiovascular drugs: "C07", "C08", "C09"
#'   }
#' @param source Character string specifying search field:
#'   \itemize{
#'     \item "atc" (default) - Search in ATC codes
#'     \item "produkt" - Search in product names
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
    rxs = list(
      "rx_hormones_pubblock"= c(
        "L02AE",
        "H01CA"
      )
    ),
    source = "atc"
){
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  start_isoyearweek <- stop_isoyearweek <- temp <- d <- NULL
  start_date <- edatum <- stop_date <- fddd <- atc <- id <- isoyearweek <- produkt <- NULL

  # Validate inputs
  validate_skeleton_structure(skeleton)
  validate_id_column(lmed, id_name)
  validate_prescription_data(lmed)
  validate_pattern_list(rxs, "prescription patterns")
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

  setkey(lmed, start_isoyearweek, stop_isoyearweek, atc)
  setkey(skeleton, id, isoyearweek)
  for(rx in names(rxs)){
    skeleton[,(rx) := FALSE]

    atcs_for_rx <- rxs[[rx]]
    for(atc_for_rx in atcs_for_rx){
      if(source=="atc"){
        lmed_atc <- lmed[stringr::str_detect(atc, paste0("^",atc_for_rx))]
      } else if(source == "produkt"){
        lmed_atc <- lmed[stringr::str_detect(produkt, paste0("^",atc_for_rx))]
      }
      for(x_isoyearweek in sort(unique(skeleton$isoyearweek))){
        # identify all the individuals who received prescription in this isoyearweek
        individuals_in_category_and_isoyearweek <- lmed_atc[
          (start_isoyearweek <= x_isoyearweek & x_isoyearweek <= stop_isoyearweek)
        ]

        # assign rx:=TRUE for all the individuals we found above, in this isoyearweek
        if(nrow(individuals_in_category_and_isoyearweek) > 0){
          individuals_in_category_and_isoyearweek <- individuals_in_category_and_isoyearweek[[id_name]] |> unique()
          skeleton[
            .(individuals_in_category_and_isoyearweek, x_isoyearweek),
            (rx) := TRUE
          ]
        }
      }
    }
  }
}
