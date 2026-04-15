#' First candidate path that exists
#'
#' Returns the first element of `candidates` for which [dir.exists()] is
#' `TRUE`. If none exist but one has a parent directory that exists, creates
#' that candidate and returns it. Errors if none of the candidates or their
#' parents exist.
#'
#' This is the stateless primitive used by [CandidatePath] for resolution.
#' Scripts that just need "give me the first of these paths that exists" can
#' call it directly.
#'
#' @param candidates Character vector of candidate paths, in priority order.
#' @param label Optional label used in error messages to describe what kind
#'   of path is being resolved (e.g. `"data_rawbatch_dir"`).
#' @return A single character path: the first candidate that exists, or a
#'   newly-created one.
#' @seealso [CandidatePath] for the stateful, caching wrapper used by the R6
#'   classes in this package.
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' first_existing_path(c("/definitely/not/there", d))
#' @export
first_existing_path <- function(candidates, label = NULL) {
  lbl <- if (is.null(label) || !nzchar(label)) "path" else label

  if (length(candidates) == 0) {
    stop(lbl, ": no paths provided", call. = FALSE)
  }

  for (p in candidates) {
    if (dir.exists(p)) return(p)
  }

  # No candidate exists yet -- create the first one whose parent is available
  for (p in candidates) {
    if (dir.exists(dirname(p))) {
      dir.create(p, showWarnings = FALSE, recursive = TRUE)
      return(p)
    }
  }

  stop(
    lbl,
    ": none of the candidate paths (or their parents) exist:\n",
    paste("  -", candidates, collapse = "\n"),
    call. = FALSE
  )
}

#' Invalidate every CandidatePath cache inside an R6 object
#'
#' Walks the public fields of `obj` depth-first. Every field that is a
#' [CandidatePath] has its cache cleared via `$invalidate()`. Every field that
#' is another R6 object is recursed into, so embedded objects (e.g. a
#' [RegistryStudy] held inside a [TTEPlan]) are also invalidated.
#'
#' Active bindings are deliberately skipped during the walk: accessing a
#' `$dir_foo` active binding would call `$resolve()` on the underlying
#' [CandidatePath] and immediately re-populate the cache we are trying to
#' clear. We reach the [CandidatePath] instances via their backing public
#' fields instead.
#'
#' This is called from `$save()` on both [RegistryStudy] and [TTEPlan] before
#' serialization, so the on-disk qs2 file never carries host-specific resolved
#' paths. After loading on a different host, first access re-walks the
#' candidate list and caches the path valid on that host.
#'
#' @param obj An R6 object to walk.
#' @return `invisible(obj)`.
#' @export
invalidate_candidate_paths <- function(obj) {
  visited <- list()

  walk <- function(x) {
    if (!inherits(x, "R6")) return(invisible())

    for (seen in visited) {
      if (identical(seen, x)) return(invisible())
    }
    visited[[length(visited) + 1L]] <<- x

    if (inherits(x, "CandidatePath")) {
      x$invalidate()
      return(invisible())
    }

    env <- as.environment(x)
    for (nm in ls(env, all.names = FALSE)) {
      if (bindingIsActive(nm, env)) next
      child <- tryCatch(
        get(nm, envir = env, inherits = FALSE),
        error = function(e) NULL
      )
      if (is.null(child)) next
      if (inherits(child, "R6")) walk(child)
    }
  }

  walk(obj)
  invisible(obj)
}
