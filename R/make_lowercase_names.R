#' Convert column names to lowercase
#'
#' Generic function to convert all column names in an object to lowercase.
#' Methods are provided for data.table and default objects (data.frame, etc.).
#'
#' @param x An object with named columns (data.frame, data.table, etc.)
#' @return The object with all column names converted to lowercase
#' @examples
#' # Load fake data
#' data("fake_demographics", package = "swereg")
#' 
#' # Check original column names
#' names(fake_demographics)
#' 
#' # Convert to lowercase (required for swereg functions)
#' swereg::make_lowercase_names(fake_demographics)
#' 
#' # Check converted names
#' names(fake_demographics)
#' @seealso \code{\link{create_skeleton}} for creating the skeleton structure,
#'   \code{\link{add_onetime}} for merging data,
#'   \code{\link{add_diagnoses}} for diagnosis data
#' @family data_preprocessing
#' @export
make_lowercase_names <- function(x) {
  UseMethod("make_lowercase_names", x)
}

#' @rdname make_lowercase_names
#' @export
make_lowercase_names.default <- function(x){
  names(x) <- tolower(names(x))
  x
}

#' @rdname make_lowercase_names
#' @export
make_lowercase_names.data.table <- function(x){
  setnames(x, tolower(names(x)))
  x
}
