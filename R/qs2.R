#' Read a standard-format qs2 file
#'
#' Reads a file written in standard qs2 format, that is, one saved with
#' `qs2::qs_save` or with `qs2_write_atomic`. The call goes straight to
#' `qs2::qs_read`.
#'
#' Files in the qdata format (`qs2::qd_save`) are no longer readable through
#' this function. An earlier version tried `qs2::qd_read` first and fell back to
#' the standard reader; that attempt is gone, so a qdata file now raises the
#' underlying qs2 error `qdata format detected, use qs2::qd_read`. swereg has
#' never written qdata files itself.
#'
#' @section data.table over-allocation:
#' The reader restores data.table over-allocation before it returns. qs2 does
#' not keep the over-allocated column slots, so a table read from disk has a
#' `truelength()` of 0. The first `:=` on that table inside a function writes
#' to a shallow copy. The caller then never sees the new column, and
#' data.table reports nothing.
#'
#' `qs2_read()` calls [data.table::setalloccol()] on every data.table it
#' reaches. It reaches the top-level object, every element of a plain list at
#' any depth, and every field of an R6 object, public or private. There is no
#' depth limit.
#'
#' It enters nothing else. It returns everything it does not enter
#' unchanged, so a data.table held inside one of these keeps a
#' `truelength()` of 0:
#'
#' * an active binding, because reading one runs user code
#' * a function, because its enclosure is an environment
#' * a classed list, a `data.frame` included, because `[[<-` dispatches there
#' * an environment that is not an R6 object, because a binding in one can
#'   hold a package namespace
#' * any other object, an S4 object included
#'
#' The walker visits a self-referential R6 object once. A plain list cannot
#' refer to itself, because R copies a list on assignment.
#'
#' Two figures, and which table gets which:
#'
#' * A SKELETON gets 4096 free column slots, held in `R/dt_alloc.R` as
#'   `.DT_ALLOC_SPARE_SLOTS`. A skeleton is the top-level object when that is
#'   a data.table, and the `$data` field of a [Skeleton]. The `add_*`
#'   functions reserve the same number.
#' * Every OTHER data.table gets 1024, which is data.table's own default. A
#'   plan can hold thousands of small result tables, and one free slot costs
#'   16 bytes whether it is used or not.
#'
#' The reader does not read `options(datatable.alloccol)`. Set that option and
#' it changes what `[.data.table` reserves, not what this reader restores.
#'
#' The repair is cheap. `setalloccol()` allocates a new column-pointer header
#' and shares the column data by reference. It costs a few bytes per free
#' slot, not a copy of the table.
#'
#' @param file Path to the .qs2 file.
#' @param nthreads Number of threads for decompression.
#' @return The deserialized R object.
#' @export
qs2_read <- function(file, nthreads = 1L) {
  obj <- qs2::qs_read(file, nthreads = nthreads)

  # Auto-check schema version for R6 objects
  if (is.environment(obj) && !is.null(obj$check_version)) {
    obj$check_version()
  }

  return(.restore_dt_alloc(obj))
}

# --- data.table over-allocation repair --------------------------------------
#
# One implementation, called from `qs2_read()`. Every swereg loader that reads
# a data.table it will later modify by reference goes through that one
# function. The repair therefore happens once and covers all of them. See the
# "data.table over-allocation" section of `?qs2_read` for what the defect is.
#
# Four shapes reach this walker, and swereg writes all four. They are a bare
# data.table, a plain list of them, an R6 object with a data.table field, and
# any nesting of those.
#
# What the walker refuses to enter, and why:
#   * a non-R6 environment: a binding in one can hold a package namespace,
#     which is large and locked, so `assign()` into it fails
#   * a classed list, a data.frame included: `[[<-` dispatches there
#   * an active binding: reading one runs user code, and several swereg R6
#     classes define active bindings that stop()
#   * a function: its enclosure is an environment
#   * anything else, an S4 object included: the walker enters a data.table, a
#     plain list and an R6 object, and returns the rest unchanged
#
# The walker carries no depth limit, and needs none. A plain list cannot hold
# a cycle, because R copies a list on assignment. An environment can, and the
# visited set in `.restore_dt_alloc_r6()` is what ends that one. Nesting too
# deep for the C stack raises an R error, which is loud. A silent cap is the
# one option that is wrong here: it would return an unrepaired data.table and
# say nothing.

# R6 machinery rather than user data. `.__enclos_env__` reaches `self`,
# `private` and `super`, so walking it would loop.
.RESTORE_ALLOC_SKIP <- c(".__enclos_env__", "self", "private", "super")

#' @noRd
.restore_dt_alloc <- function(obj) {
  state <- new.env(parent = emptyenv())
  state$seen <- list()
  # The top-level object is a skeleton when it is a bare data.table, so it
  # gets the skeleton figure. Everything the walker reaches from there gets
  # the nested figure, except a `Skeleton$data` field.
  return(.restore_dt_alloc_walk(obj, state, n = .DT_ALLOC_SPARE_SLOTS))
}

#' @noRd
.restore_dt_alloc_walk <- function(x, state, n = .DT_ALLOC_NESTED_SLOTS) {
  if (data.table::is.data.table(x)) {
    return(data.table::setalloccol(x, n = n))
  }
  if (is.environment(x)) {
    return(.restore_dt_alloc_r6(x, state))
  }
  if (is.list(x) && !is.object(x)) {
    for (i in seq_along(x)) {
      # `x[[i]] <- NULL` DELETES element i, so a NULL element must be
      # left alone rather than walked and written back.
      if (is.null(x[[i]])) {
        next
      }
      x[[i]] <- .restore_dt_alloc_walk(x[[i]], state)
    }
    return(x)
  }
  return(x)
}

#' @noRd
.restore_dt_alloc_r6 <- function(env, state) {
  if (!inherits(env, "R6")) {
    return(env)
  }
  # Environments are reference objects, so an object graph can hold a cycle.
  # Lists cannot, which is why only this branch keeps a visited set.
  for (seen in state$seen) {
    if (identical(seen, env)) {
      return(env)
    }
  }
  state$seen[[length(state$seen) + 1L]] <- env
  .restore_dt_alloc_bindings(env, state)
  enclos <- env$.__enclos_env__
  if (is.environment(enclos) && is.environment(enclos$private)) {
    .restore_dt_alloc_bindings(enclos$private, state)
  }
  return(env)
}

#' @noRd
.restore_dt_alloc_bindings <- function(env, state) {
  # A `Skeleton` holds the one table a code registry writes to, in `$data`.
  # That field gets the skeleton figure. Every other field gets the nested
  # figure.
  is_skeleton <- inherits(env, "Skeleton")
  # R6 locks the object environment but not its bindings, so `assign()` on a
  # name that already exists is allowed.
  for (nm in ls(env, all.names = TRUE, sorted = FALSE)) {
    if (nm %in% .RESTORE_ALLOC_SKIP) {
      next
    }
    if (bindingIsActive(nm, env)) {
      next
    }
    value <- get(nm, envir = env, inherits = FALSE)
    if (is.null(value) || is.function(value)) {
      next
    }
    n <- if (is_skeleton && identical(nm, "data")) {
      .DT_ALLOC_SPARE_SLOTS
    } else {
      .DT_ALLOC_NESTED_SLOTS
    }
    assign(nm, .restore_dt_alloc_walk(value, state, n = n), envir = env)
  }
  return(invisible(NULL))
}

#' Atomically write an object to a qs2 file
#'
#' Writes to a uniquely-named temporary file in the same directory, then renames
#' it into place. Rename-into-place is atomic on POSIX filesystems (and
#' server-side atomic on SMB/CIFS), so an interrupted write -- SIGKILL, crash,
#' dropped mount -- leaves the destination either absent (a later resume
#' rebuilds that batch) or complete, never a truncated file that `qs2_read()`
#' would halt on. `...` is forwarded to [qs2::qs_save()].
#'
#' What this does **not** promise, stated because the tempting reading is wrong:
#'
#' * **It is not durability.** `file.rename()` is atomic with respect to other
#'   *readers*; it is not an `fsync`. A power loss can still lose a renamed file
#'   whose data has not reached the disk. This protects against a killed
#'   process, not a killed machine.
#' * **It is not a lock.** Two concurrent writers of the same `path` each
#'   produce a complete file and the last rename wins. No reader sees a torn
#'   file, but nothing here decides *which* writer should have won.
#' * **It does not always clean up after itself.** The partial temp file is
#'   removed on an R-level error, but `on.exit()` cannot run after a `SIGKILL`
#'   -- so a hard-killed worker leaves its randomly-named `.tmp` behind. The
#'   *destination* is still absent-or-complete, which is the guarantee that
#'   matters; the litter is not.
#'
#' The temporary file is created with [tempfile()] in the destination directory
#' rather than `paste0(path, ".tmp", Sys.getpid())`. The PID suffix was not
#' collision-proof: PIDs are unique only among *live processes on one host*, and
#' this package's data lives on a share that two hosts mount at once -- so the
#' same PID on two machines could pick the same temp path for the same target.
#' Same directory is required: `file.rename()` is not atomic across filesystems.
#'
#' The implementation now lives in [batchit::write_qs2_atomically()]; this is a
#' thin delegation, and the contract above is what swereg promises its own
#' users. One visible consequence: the rename-failure error is raised by
#' batchit, so its prefix reads `write_qs2_atomically()` rather than
#' `qs2_write_atomic()`.
#'
#' @param object Object to serialize.
#' @param path Destination path.
#' @param ... Passed to `qs2::qs_save()` (e.g. `nthreads`).
#' @return `path`, invisibly.
#' @export
qs2_write_atomic <- function(object, path, ...) {
  return(batchit::write_qs2_atomically(object, path, ...))
}
