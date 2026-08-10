#' @importFrom stats ave na.omit setNames
#' @importFrom utils getFromNamespace
#' @importFrom progressr progressor with_progress
#' @import data.table
NULL

# `i.irr_estimable_itt` is a data.table update-join symbol. It names the source
# column of the join at `R/forest_plot.R:1434`, so it never appears as a
# binding. `R CMD check` reports it as an undefined global without this
# declaration.
#
# This list names ONE symbol on purpose. The four sibling symbols in the same
# join, `i.irr_itt`, `i.lo_itt`, `i.hi_itt` and `i.pvalue_itt`, stay under
# report. A declaration that covers all five would remove four diagnostics that
# the package already carries, and the goal is no NEW diagnostic.
utils::globalVariables("i.irr_estimable_itt")