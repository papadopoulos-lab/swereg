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
# Two functions here, and they answer different questions.
#
# `.grow_dt_alloc()` grows the list and returns the grown table. It writes
# nothing back. Use it where the caller assigns the return value.
#
# `.ensure_dt_alloc()` grows the list and ALSO tries to write the grown table
# back to the caller's own binding. That write reaches a name, or a `$`, `[[`
# or `@` chain that ends at a name. It cannot reach anything else, because
# there is no binding to write to: an expression such as `identity(sk)`, and
# a table passed by value through `do.call()`, both fail it. The function
# warns there, and its new columns then live on the returned table alone.
# Every caller MUST use the return value. The rebind is a convenience for
# a user who wrote `add_x(sk, ...)` and reads `sk` afterwards. It is not the
# mechanism the package relies on.
#
# The rebind reaches the IMMEDIATE caller only. A helper that wraps such a
# call keeps the grown table in its own local, so the helper MUST reserve the
# slots itself or return the table.
#
# Slack is `.DT_ALLOC_SPARE_SLOTS` free slots on top of the new columns.
# `.restore_dt_alloc()` in `R/qs2.R` uses the same constant, so swereg has one
# over-allocation policy and one number. One spare slot costs 16 bytes,
# measured on R 4.5.2 and data.table 1.18.4: one pointer in the column list
# and one in the names vector. 4096 slots therefore cost 64 KB per table, at
# 3, 10,000 and 200,000 rows alike.
#
# Why 4096 and not data.table's own 1024: a code registry writes one column
# per code name per group, so a registry of a few hundred entries writes more
# than a thousand columns onto a skeleton that starts with ten. 1024 free
# slots run out part way through such a registry. 4096 carries it with one
# growth.
#
# 4096 is for a SKELETON. Every other data.table keeps data.table's own 1024,
# because a plan or a result list can hold thousands of small tables and each
# free slot costs 16 bytes whether it is used or not.

# Free column slots on top of the columns a skeleton already holds.
.DT_ALLOC_SPARE_SLOTS <- 4096L

# Free column slots for any other data.table. This is data.table's own
# default, and `?qs2_read` states where each of the two numbers applies.
.DT_ALLOC_NESTED_SLOTS <- 1024L

# Extra free slots the code registry reserves on top of the columns it
# predicts an entry writes. It absorbs a scratch column a custom `fn` builds
# that `.entry_columns()` cannot predict.
#
# This is a PERFORMANCE choice, not a correctness threshold. A `fn` that
# needs more slots than the margin makes data.table reallocate, and the
# applier accepts that: `.validate_columns_kept()` decides growth against
# replacement by COLUMN address, and a growth keeps every column vector.
# The margin only decides how often that reallocation happens.
.DT_ALLOC_ENTRY_MARGIN <- 64L

#' @noRd
.dt_alloc_free <- function(x) {
  return(data.table::truelength(x) - ncol(x))
}

# Grow `x` so it holds at least `n_new` free column slots, and return the
# result. The return is the same object when no growth was needed, and a new
# object otherwise. The column vectors are shared either way, so the growth
# copies no column data.
#' @noRd
.grow_dt_alloc <- function(x, n_new) {
  if (n_new <= 0L || .dt_alloc_free(x) >= n_new) {
    return(x)
  }
  return(data.table::setalloccol(x, n = n_new + .DT_ALLOC_SPARE_SLOTS))
}

#' @noRd
.ensure_dt_alloc <- function(x, n_new, x_expr, env, fn_name) {
  if (n_new <= 0L) {
    return(x)
  }
  free <- .dt_alloc_free(x)
  if (free >= n_new) {
    return(x)
  }
  grown <- .grow_dt_alloc(x, n_new)
  if (!.rebind_dt(x_expr, grown, env)) {
    warning(
      fn_name,
      " cannot write its new columns back to the table it was given. ",
      "New columns: ",
      n_new,
      ". Free column slots: ",
      free,
      ". data.table cannot grow a column-pointer vector in place, so the ",
      "table you passed does not get the new columns. Use the table this ",
      "call returns. Or assign that table to a variable, then pass the ",
      "variable.",
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
