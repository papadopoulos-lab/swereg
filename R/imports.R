#' @importFrom stats ave na.omit setNames
#' @importFrom utils getFromNamespace
#' @importFrom progressr progressor with_progress
#' @import data.table
NULL

# Every other undefined global is declared `<sym> <- NULL` at the top of the
# one function that uses it. These two symbols cannot be, so they are declared
# here for the whole package.
#
# `.` is data.table's list alias in `j`. `R CMD check` reports it as an
# undefined FUNCTION. A local `. <- NULL` binds a value, not a function, so
# the report stays.
#
# `..cache_cols` is data.table's "read this name one frame out" prefix, used
# in `.s1a_finalize_on_skeleton()` in `R/r6_tteplan.R`. `cache_cols` is
# assigned in that same frame. A local `..cache_cols <- NULL` puts both names
# in the calling scope, and data.table then warns on every call: "Both
# 'cache_cols' and '..cache_cols' exist in calling scope."
utils::globalVariables(c(".", "..cache_cols"))
