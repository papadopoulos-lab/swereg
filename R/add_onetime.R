#' Add one-time data to skeleton
#'
#' Merges one-time data (non-longitudinal) into the main skeleton data structure.
#' This function is used for adding data that doesn't change over time, such as
#' demographic information or baseline characteristics.
#'
#' @param skeleton A data.table containing the main skeleton structure with id and time variables
#' @param data A data.table containing the one-time data to be merged
#' @param id_name Character string specifying the name of the ID variable in the data
#' @return The skeleton data.table is modified by reference with one-time data merged in.
#'   Columns from data that already exist in skeleton will be prefixed with "i."
#' @examples
#' # Load fake data
#' data("fake_person_ids", package = "swereg")
#' data("fake_demographics", package = "swereg")
#' swereg::make_lowercase_names(fake_demographics)
#' 
#' # Create skeleton
#' skeleton <- create_skeleton(fake_person_ids[1:5], "2020-01-01", "2020-12-31")
#' 
#' # Add demographic data
#' add_onetime(skeleton, fake_demographics, "lopnr")
#' 
#' # Check added variables
#' names(skeleton)
#' @seealso \code{\link{create_skeleton}} for creating the skeleton structure,
#'   \code{\link{add_annual}} for annual data,
#'   \code{\link{make_lowercase_names}} for data preprocessing
#' @family data_integration
#' @importFrom utils head
#' @export
add_onetime <- function(
  skeleton,
  data,
  id_name
  ){

  # Validate inputs
  validate_skeleton_structure(skeleton)
  validate_id_column(data, id_name)
  validate_data_structure(data, data_type = "one-time data")
  
  # Check if data has any non-ID columns to add
  potential_cols <- names(data)[!names(data) %in% c(id_name)]
  if (length(potential_cols) == 0) {
    stop("Data only contains the ID column '", id_name, "'. No variables to add to skeleton.")
  }
  
  # Check for ID matches
  skeleton_ids <- unique(skeleton$id)
  data_ids <- unique(data[[id_name]])
  matching_ids <- intersect(skeleton_ids, data_ids)
  
  if (length(matching_ids) == 0) {
    stop("No matching IDs found between skeleton and data.\n",
         "Skeleton IDs (first 5): ", paste(head(skeleton_ids, 5), collapse = ", "), "\n",
         "Data IDs (first 5): ", paste(head(data_ids, 5), collapse = ", "), "\n",
         "Check that ID columns contain the same values.")
  }
  
  if (length(matching_ids) < length(skeleton_ids)) {
    warning("Only ", length(matching_ids), " out of ", length(skeleton_ids), 
            " skeleton IDs found in data. Some individuals will have missing values.")
  }

  nam_left <- names(data)[!names(data) %in% c(id_name)]
  nam_right <- nam_left

  for(i in seq_along(nam_left)){
    if(nam_left[i] %in% names(skeleton)) nam_right[i] <- paste0("i.",nam_left[i])
  }

  nam_left <- paste0(nam_left,collapse='","')
  nam_left <- paste0('"',nam_left, '"')
  nam_right <- paste0(nam_right,collapse=',')
  txt <- paste0('skeleton[data,on = c("id==',id_name,'"),c(',nam_left,'):=.(',nam_right,')]')
  eval(parse(text = txt))

}
