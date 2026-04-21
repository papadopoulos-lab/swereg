
#' Create longitudinal data skeleton
#'
#' Creates a longitudinal data skeleton with individual IDs and time periods
#' (both ISO years and ISO year-weeks) for Swedish registry data analysis.
#' The skeleton provides the framework for merging various registry datasets
#' with consistent time structure.
#'
#' @param ids Vector of individual IDs to include in the skeleton
#' @param date_min Date object specifying the start date for the analysis period
#' @param date_max Date object specifying the end date for the analysis period
#' @return A data.table skeleton with columns:
#'   \itemize{
#'     \item id: Individual identifier
#'     \item isoyear: ISO year (integer)
#'     \item isoyearweek: ISO year-week (character, format "YYYY-WW" or "YYYY-**" for annual rows)
#'     \item is_isoyear: Logical indicating if row represents annual (TRUE) or weekly (FALSE) data
#'     \item isoyearweeksun: Date representing the Sunday (last day) of the ISO week/year
#'     \item personyears: Person-time contribution (1 for annual rows, 1/52.25 for weekly rows)
#'   }
#' @examples
#' # Load fake data
#' data("fake_person_ids", package = "swereg")
#' 
#' # Create skeleton for 2020-2022 period
#' skeleton <- create_skeleton(
#'   ids = fake_person_ids[1:10],
#'   date_min = as.Date("2020-01-01"),
#'   date_max = as.Date("2022-12-31")
#' )
#' utils::head(skeleton)
#' 
#' # Check structure
#' utils::str(skeleton)
#' @seealso \code{\link{add_onetime}} for demographic data,
#'   \code{\link{add_diagnoses}} for diagnosis codes,
#'   \code{\link{add_rx}} for prescription data,
#'   \code{\link{add_operations}} for surgical procedures
#' @family skeleton_creation
#' @export
create_skeleton <- function(ids, date_min, date_max) {
  # Declare variables for data.table non-standard evaluation
  personyears <- isoyear <- isoyearweek <- is_isoyear <- isoyearweeksun <- id <- NULL

  max_isoyear <- cstime::date_to_isoyear_n(as.Date(date_min) - 1)

  # isoyears
  years <- 1900:max_isoyear
  year_spine <- data.table(
    isoyear     = years,
    isoyearweek = paste0(years, "-**"),
    is_isoyear  = TRUE,
    personyears = 1
  )
  # Add Sunday dates for each ISO year
  year_spine[, isoyearweeksun := cstime::isoyearweek_to_last_date(paste0(isoyear, "-26"))]
  year_spine[is.na(isoyearweeksun), isoyearweeksun := as.Date(paste0(isoyear, "-06-28"))]

  # isoyearweeks
  isoyearweeks <- unique(cstime::date_to_isoyearweek_c(
    seq.Date(as.Date(date_min), as.Date(date_max), 1)
  ))
  week_spine <- data.table(isoyearweek = isoyearweeks, is_isoyear = FALSE, personyears = 1/52.25)
  # Add Sunday dates and isoyear for each ISO week
  week_spine[, `:=`(
    isoyear        = cstime::isoyearweek_to_isoyear_n(isoyearweek),
    isoyearweeksun = cstime::isoyearweek_to_last_date(isoyearweek)
  )]

  # Sort the spine once — replication preserves order per id,
  # avoiding setorder() on the full expanded table
  time_spine <- rbindlist(list(year_spine, week_spine), use.names = TRUE, fill = TRUE)
  setcolorder(time_spine, c("isoyear", "isoyearweek", "is_isoyear", "isoyearweeksun", "personyears"))
  setorder(time_spine, isoyearweek)

  n_t <- nrow(time_spine)
  skeleton <- time_spine[rep.int(seq_len(n_t), length(ids))]
  skeleton[, id := rep(ids, each = n_t)]

  setcolorder(skeleton, c("id", "isoyear", "isoyearweek", "is_isoyear", "isoyearweeksun", "personyears"))
  setorder(skeleton, id, isoyearweek)

  return(skeleton)
}
