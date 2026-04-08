#' Add quality registry data to skeleton
#'
#' Creates boolean skeleton columns from filter expressions evaluated against
#' quality registry data (e.g., Riksstroke, NKBC). Unlike [add_diagnoses()]
#' which matches ICD code prefixes, this function evaluates arbitrary R
#' expressions against the dataset columns.
#'
#' Each entry in \code{codes} produces one boolean column on the skeleton:
#' \itemize{
#'   \item \code{TRUE}: event indicator — TRUE on any week with a registry event
#'   \item \code{quote(expr)}: filter expression evaluated against dataset columns.
#'     TRUE when the expression evaluates to TRUE, FALSE otherwise (including NA).
#' }
#'
#' Multiple events in the same person-week are aggregated with \code{any()}.
#'
#' @param skeleton A data.table containing the main skeleton structure created
#'   by \code{\link{create_skeleton}}.
#' @param dataset A data.table containing quality registry data with person ID
#'   and a date column.
#' @param id_name Character string specifying the name of the ID variable in
#'   the dataset.
#' @param date_col Character string specifying the name of the date column in
#'   the dataset (must be Date or IDate class).
#' @param codes Named list of filter definitions. Names become variable names
#'   in skeleton. Values are either \code{TRUE} (event flag) or \code{quote()}
#'   expressions evaluated against dataset columns.
#'   Example: \code{list("stroke_event" = TRUE, "stroke_tia" = quote(tia == 1))}
#'
#' @return The skeleton data.table is modified by reference with boolean
#'   variables added. New columns are FALSE on non-event weeks, and
#'   FALSE when filter expressions evaluate to NA (missing data).
#'
#' @examples
#' # Create fake data
#' data("fake_person_ids", package = "swereg")
#' skeleton <- create_skeleton(fake_person_ids[1:5], "2020-01-01", "2020-12-31")
#'
#' fake_registry <- data.table::data.table(
#'   lopnr = c(fake_person_ids[1], fake_person_ids[2]),
#'   event_date = as.Date(c("2020-03-15", "2020-06-20")),
#'   severity = c(5, 18),
#'   treated = c(1, 2)
#' )
#'
#' add_quality_registry(skeleton, fake_registry, "lopnr",
#'   date_col = "event_date",
#'   codes = list(
#'     "reg_event" = TRUE,
#'     "reg_severe" = quote(severity >= 15),
#'     "reg_treated" = quote(treated == 1)
#'   )
#' )
#'
#' @seealso \code{\link{add_diagnoses}} for ICD code matching,
#'   \code{\link{add_rx}} for prescription data
#' @family data_integration
#' @export
add_quality_registry <- function(
    skeleton,
    dataset,
    id_name,
    date_col,
    codes = list()
) {
  # Declare variables for data.table non-standard evaluation
  isoyearweek <- is_isoyear <- NULL

  # Validate inputs
  validate_skeleton_structure(skeleton)
  validate_id_column(dataset, id_name)

  if (!date_col %in% names(dataset)) {
    stop(
      "date_col '", date_col, "' not found in dataset.\n",
      "Available columns: ", paste(names(dataset), collapse = ", ")
    )
  }

  if (length(codes) == 0) {
    return(invisible(skeleton))
  }

  # Convert date to isoyearweek
  dataset[, isoyearweek := cstime::date_to_isoyearweek_c(get(date_col))]
  min_isoyearweek <- min(skeleton[is_isoyear == FALSE]$isoyearweek)
  dataset[
    isoyearweek < min_isoyearweek,
    isoyearweek := paste0(cstime::date_to_isoyear_c(get(date_col)), "-**")
  ]

  # Evaluate each filter expression
  for (i in seq_along(codes)) {
    nam <- names(codes)[i]
    expr <- codes[[i]]

    if (isTRUE(expr)) {
      # Event flag: all rows are TRUE
      dataset[, (nam) := TRUE]
    } else if (is.call(expr) || is.name(expr)) {
      # Quoted expression: evaluate and coerce to logical
      result <- tryCatch(
        eval(expr, envir = dataset, enclos = parent.frame()),
        error = function(e) {
          stop(
            "Error evaluating expression for '", nam, "': ", e$message,
            "\nExpression: ", deparse(expr),
            "\nAvailable columns: ", paste(names(dataset), collapse = ", ")
          )
        }
      )
      # NA → FALSE (missing = no evidence)
      result[is.na(result)] <- FALSE
      dataset[, (nam) := result]
    } else {
      stop(
        "codes[['", nam, "']] must be TRUE or a quote() expression, got: ",
        class(expr)[1]
      )
    }
  }

  # Aggregate by (id, isoyearweek): any() per variable
  nam <- names(codes)
  txt <- paste0(
    "reduced <- dataset[, .(",
    paste0(nam, "=as.logical(max(", nam, "))", collapse = ", "),
    "), keyby=.(", id_name, ", isoyearweek)]"
  )
  eval(parse(text = txt))

  # Left-join to skeleton
  nam_left <- paste0('"', paste0(nam, collapse = '","'), '"')
  nam_right <- paste0(nam, collapse = ",")
  txt <- paste0(
    'skeleton[reduced, on = c("id==', id_name,
    '","isoyearweek"), c(', nam_left, '):=.(', nam_right, ')]'
  )
  eval(parse(text = txt))

  # Fill non-matched weeks with FALSE
  for (i in nam) {
    skeleton[is.na(get(i)), (i) := FALSE]
  }

  # Clean up temporary columns from dataset
  dataset[, isoyearweek := NULL]
  for (i in nam) {
    dataset[, (i) := NULL]
  }

  invisible(skeleton)
}
