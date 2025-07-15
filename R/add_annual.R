#' Add annual data to skeleton
#'
#' Merges annual data into the main skeleton data structure for a specific ISO year.
#' This function is used for adding data that is measured or recorded annually,
#' such as yearly income, employment status, or annual health assessments.
#'
#' @param skeleton A data.table containing the main skeleton structure with id and time variables
#' @param data A data.table containing the annual data to be merged
#' @param id_name Character string specifying the name of the ID variable in the data
#' @param isoyear Integer specifying the ISO year for which the data applies
#' @return The skeleton data.table is modified by reference with annual data merged in.
#'   Columns from data that already exist in skeleton will be prefixed with "i."
#' @examples
#' # Load fake data
#' data("fake_person_ids", package = "swereg")
#' data("fake_annual_family", package = "swereg")
#' swereg::make_lowercase_names(fake_annual_family)
#' 
#' # Create skeleton
#' skeleton <- create_skeleton(fake_person_ids[1:5], "2020-01-01", "2022-12-31")
#' 
#' # Add annual family data for 2021
#' add_annual(skeleton, fake_annual_family, "lopnr", 2021)
#' 
#' # Check data was added only for 2021
#' skeleton[isoyear == 2021 & is_isoyear == TRUE, .(id, isoyear, famtyp)]
#' @seealso \code{\link{create_skeleton}} for creating the skeleton structure,
#'   \code{\link{add_onetime}} for one-time data,
#'   \code{\link{make_lowercase_names}} for data preprocessing
#' @family data_integration
#' @export
add_annual <- function(
  skeleton,
  data,
  id_name,
  isoyear
  ){

  data[, isoyear := isoyear]
  nam_left <- names(data)[!names(data) %in% c(id_name, "isoyear")]
  nam_right <- nam_left

  for(i in seq_along(nam_left)){
    if(nam_left[i] %in% names(skeleton)) nam_right[i] <- paste0("i.",nam_left[i])
  }

  nam_left <- paste0(nam_left,collapse='","')
  nam_left <- paste0('"',nam_left, '"')
  nam_right <- paste0(nam_right,collapse=',')
  txt <- paste0('skeleton[data,on = c("id==',id_name,'","isoyear"),c(',nam_left,'):=.(',nam_right,')]')
  eval(parse(text = txt))
}
