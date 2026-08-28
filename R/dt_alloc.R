# Column-slot headroom for a caller's data.table
#
# data.table holds a table's columns in a list, and over-allocates that list.
# `:=` then adds a column into a spare slot, in place. When the spare slots
# run out, data.table allocates a longer list. A longer list is a new R
# object, because R cannot grow a list in place. `:=` writes the new columns
# to that new object and rebinds the variable in the frame the `[` ran in.
# Inside a package function that frame belongs to the function, so the caller
# keeps the old table and never sees the new columns. data.table 1.18.4
# reports nothing.
#
# `.ensure_dt_alloc()` grows the list before the assignment, and writes the
# grown table back to the caller's own binding. `[.data.table` does the same
# thing one frame lower. The caller's binding therefore holds a new object
# afterwards, and `address()` on it changes. The column vectors are shared,
# so the growth copies no column data.
#
# Slack is `getOption("datatable.alloccol", 4096L)` spare slots on top of the
# new columns. `.restore_dt_alloc()` in `R/qs2.R` reads the same expression,
# so swereg has one over-allocation policy. data.table sets that option to
# 1024 when it loads. `[.data.table` adds the same 1024 on its own growth
# path, so the effective slack matches data.table's. One spare slot costs 16
# bytes, measured on R 4.5.2 and data.table 1.18.4: one pointer in the column
# list and one in the names vector. 1024 slots therefore cost 16 KB per
# table, at 3, 10,000 and 200,000 rows alike.

#' @noRd
.ensure_dt_alloc <- function(x, n_new, x_expr, env, fn_name) {
  if (n_new <= 0L) {
    return(x)
  }
  free <- data.table::truelength(x) - ncol(x)
  if (free >= n_new) {
    return(x)
  }
  grown <- data.table::setalloccol(
    x,
    n = n_new + getOption("datatable.alloccol", 4096L)
  )
  if (!.rebind_dt(x_expr, grown, env)) {
    warning(
      fn_name,
      " cannot write its new columns back to the table it was given. ",
      "New columns: ",
      n_new,
      ". Free column slots: ",
      free,
      ". data.table cannot grow a column-pointer vector in place, so the ",
      "table you passed does not get the new columns. Assign that table to a ",
      "variable, then pass the variable.",
      call. = FALSE
    )
  }
  return(grown)
}

# Write `value` back to the binding the caller passed in.
#
# A name takes `assign(inherits = TRUE)`, which is what `[.data.table` uses.
# It updates the binding wherever it lives, so a table held in an enclosing
# environment is reached too.
#
# A `$`, `[[` or `@` chain that ends at a name takes an assignment evaluated
# in the caller's frame. The caller gets what it would get from writing
# `<expr> <- add_annual(<expr>, ...)` itself. An R6 object is an environment,
# so that assignment reaches it by reference.
#
# Anything else returns FALSE, and `.ensure_dt_alloc()` warns.
#' @noRd
.rebind_dt <- function(expr, value, env) {
  if (is.name(expr)) {
    assign(as.character(expr), value, envir = env, inherits = TRUE)
    return(TRUE)
  }
  if (!.is_rebindable_dt(expr)) {
    return(FALSE)
  }
  eval(call("<-", expr, value), env)
  return(TRUE)
}

#' @noRd
.is_rebindable_dt <- function(expr) {
  if (is.name(expr)) {
    return(TRUE)
  }
  if (
    is.call(expr) &&
      length(expr) == 3L &&
      is.name(expr[[1L]]) &&
      as.character(expr[[1L]]) %in% c("$", "[[", "@")
  ) {
    return(.is_rebindable_dt(expr[[2L]]))
  }
  return(FALSE)
}
