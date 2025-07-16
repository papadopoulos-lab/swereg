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
#'     \item isoyearweek_sunday: Date representing the Sunday (last day) of the ISO week/year
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
#' head(skeleton)
#' 
#' # Check structure
#' str(skeleton)
#' @seealso \code{\link{add_onetime}} for demographic data,
#'   \code{\link{add_diagnoses}} for diagnosis codes,
#'   \code{\link{add_rx}} for prescription data,
#'   \code{\link{add_operations}} for surgical procedures
#' @family skeleton_creation
#' @export
create_skeleton <- function(
  ids,
  date_min,
  date_max
  ){

  # isoyears
  skeleton_isoyear <- expand.grid(
    id = ids,
    isoyear = 1900:cstime::date_to_isoyear_n(as.Date(date_min)-1),
    stringsAsFactors = FALSE
  ) |> setDT()
  skeleton_isoyear[, isoyearweek := paste0(isoyear,"-**")]
  skeleton_isoyear[, is_isoyear := TRUE]

  # isoyearweeks
  isoyearweeks <- seq.Date(
    as.Date(date_min),
    as.Date(date_max),
    1
  ) |>
    cstime::date_to_isoyearweek_c() |>
    unique()

  skeleton_isoyearweek <- expand.grid(
    id = ids,
    isoyearweek = isoyearweeks,
    stringsAsFactors = FALSE
  ) |> setDT()
  skeleton_isoyearweek[, isoyear := cstime::isoyearweek_to_isoyear_n(isoyearweek)]
  skeleton_isoyearweek[, is_isoyear := FALSE]

  skeleton <- rbindlist(list(skeleton_isoyear, skeleton_isoyearweek), use.names=T)
  
  # Add Sunday dates for each ISO week/year
  skeleton[is_isoyear==FALSE, isoyearweek_sunday := cstime::isoyearweek_to_last_date(isoyearweek)]
  skeleton[is_isoyear==TRUE, isoyearweek_sunday := cstime::isoyearweek_to_last_date(paste0(isoyear,"-26"))]
  skeleton[is.na(isoyearweek_sunday), isoyearweek_sunday := as.Date(paste0(isoyear,"-06-28"))]
  
  # Add personyears column
  skeleton[is_isoyear==TRUE, personyears := 1]
  skeleton[is_isoyear==FALSE, personyears := 1/52.25]

  setcolorder(skeleton, c("id", "isoyear", "isoyearweek", "is_isoyear", "isoyearweek_sunday", "personyears"))
  setorder(skeleton, id, isoyearweek)

  return(skeleton)
}
