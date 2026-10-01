
#' Create longitudinal data skeleton
#'
#' Creates a longitudinal data skeleton with individual IDs and time periods
#' (both ISO years and ISO year-weeks) for Swedish registry data analysis.
#' The skeleton provides the framework for merging various registry datasets
#' with consistent time structure.
#'
#' @param ids Vector of individual IDs to include in the skeleton
#' @param isoyear_min ISO year, one whole number, whose week 01 starts the
#'   weekly rows. Every ISO year from 1900 to `isoyear_min - 1` gets one annual
#'   row.
#' @param isoyearweek_max ISO week (`"YYYY-WW"`) of the last weekly row. It
#'   MUST NOT be before `"<isoyear_min>-01"`.
#' @param ... Retired arguments. `date_min` and `date_max` stop with
#'   directions: pass `isoyear_min` and
#'   `isoyearweek_max = cstime::date_to_isoyearweek_c(date_max)` instead.
#' @details
#' No ISO year holds both an annual row and a weekly row. An annual row
#' therefore always stands for a whole ISO year, and the person-years of a year
#' sum to 1.
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
#' # Annual rows up to 2019, weekly rows from 2020-01 to 2022-52
#' skeleton <- create_skeleton(
#'   ids = fake_person_ids[1:10],
#'   isoyear_min = 2020,
#'   isoyearweek_max = "2022-52"
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
create_skeleton <- function(ids, isoyear_min, isoyearweek_max, ...) {
  # Declare variables for data.table non-standard evaluation
  personyears <- isoyear <- isoyearweek <- is_isoyear <- isoyearweeksun <- id <- NULL

  .create_skeleton_refuse_dates(isoyear_min, ...names())
  isoyear_min <- .create_skeleton_isoyear_min(isoyear_min)
  weeks <- .create_skeleton_weeks(isoyear_min, isoyearweek_max)

  # isoyears
  years <- seq_len(isoyear_min - 1900L) + 1899L
  year_spine <- data.table(
    isoyear     = years,
    isoyearweek = paste0(years, "-**"),
    is_isoyear  = rep(TRUE, length(years)),
    personyears = rep(1, length(years))
  )
  # Add Sunday dates for each ISO year
  year_spine[, isoyearweeksun := cstime::isoyearweek_to_last_date(paste0(isoyear, "-26"))]
  year_spine[is.na(isoyearweeksun), isoyearweeksun := as.Date(paste0(isoyear, "-06-28"))]

  # isoyearweeks
  week_spine <- data.table(isoyearweek = weeks, is_isoyear = FALSE, personyears = 1/52.25)
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


#' Refuse the retired date_min and date_max arguments
#'
#' @param isoyear_min The second argument as the caller passed it. A character
#'   or Date value there is a `date_min` passed by position.
#' @param dot_names Names of the arguments that reached `...`.
#' @noRd
.create_skeleton_refuse_dates <- function(isoyear_min, dot_names) {
  retired <- intersect(dot_names, c("date_min", "date_max"))
  by_position <- !missing(isoyear_min) &&
    (is.character(isoyear_min) || inherits(isoyear_min, c("Date", "POSIXt")))
  if (length(retired) > 0L || by_position) {
    stop(
      "create_skeleton() no longer takes date_min and date_max. Pass ",
      "isoyear_min, the ISO year whose week 01 starts the weekly rows, and ",
      "isoyearweek_max, the ISO week of the old date_max ",
      "(cstime::date_to_isoyearweek_c(date_max)).",
      call. = FALSE
    )
  }
  unknown <- setdiff(dot_names, c("date_min", "date_max"))
  if (length(dot_names) > 0L) {
    stop(
      "create_skeleton() got unused arguments: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  return(invisible(NULL))
}


#' Check isoyear_min and return it as an integer
#'
#' @param isoyear_min One whole number, an ISO year in
#'   cstime::dates_by_isoyearweek.
#' @noRd
.create_skeleton_isoyear_min <- function(isoyear_min) {
  ok <- is.numeric(isoyear_min) &&
    length(isoyear_min) == 1L &&
    !is.na(isoyear_min) &&
    isoyear_min == round(isoyear_min)
  years <- range(cstime::dates_by_isoyearweek$isoyear)
  if (!ok || isoyear_min < years[1L] || isoyear_min > years[2L]) {
    stop(
      "isoyear_min MUST be one whole number from ",
      years[1L],
      " to ",
      years[2L],
      ", the ISO year whose week 01 starts the weekly rows",
      call. = FALSE
    )
  }
  return(as.integer(isoyear_min))
}


#' The ISO weeks from week 01 of isoyear_min to isoyearweek_max
#'
#' @param isoyear_min Integer ISO year.
#' @param isoyearweek_max One ISO week, `"YYYY-WW"`.
#' @return Character vector of ISO weeks, in calendar order.
#' @noRd
.create_skeleton_weeks <- function(isoyear_min, isoyearweek_max) {
  all_weeks <- cstime::dates_by_isoyearweek$isoyearweek
  if (!is.character(isoyearweek_max) || length(isoyearweek_max) != 1L) {
    stop("isoyearweek_max MUST be one ISO week, \"YYYY-WW\"", call. = FALSE)
  }
  if (!isoyearweek_max %in% all_weeks) {
    stop(
      "isoyearweek_max '",
      isoyearweek_max,
      "' is not an ISO week in cstime::dates_by_isoyearweek",
      call. = FALSE
    )
  }
  first <- match(paste0(isoyear_min, "-01"), all_weeks)
  last <- match(isoyearweek_max, all_weeks)
  if (last < first) {
    stop(
      "isoyearweek_max '",
      isoyearweek_max,
      "' is before ",
      isoyear_min,
      "-01, the first weekly row",
      call. = FALSE
    )
  }
  return(all_weeks[first:last])
}
