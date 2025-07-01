#' Convert column names to lowercase
#'
#' Generic function to convert all column names in an object to lowercase.
#' Methods are provided for data.table and default objects (data.frame, etc.).
#'
#' @param x An object with named columns (data.frame, data.table, etc.)
#' @return The object with all column names converted to lowercase
#' @examples
#' \dontrun{
#' df <- data.frame(Name = 1, AGE = 2, Gender = 3)
#' make_lowercase_names(df)
#' 
#' dt <- data.table(ID = 1:3, Score = c(10, 20, 30))
#' make_lowercase_names(dt)
#' }
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
