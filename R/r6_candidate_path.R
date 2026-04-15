#' CandidatePath: a directory with multiple candidate locations
#'
#' @description
#' Holds a priority-ordered list of candidate paths for a directory that may
#' live at different filesystem locations on different hosts (e.g. a shared
#' drive mounted at different points under Linux vs Windows). `$resolve()`
#' returns the first candidate that exists, caching the result so subsequent
#' calls are free. If no candidate exists but a candidate's parent directory
#' does, `$resolve()` creates the candidate and returns it -- this is how
#' "first run on a fresh host" gets a directory automatically.
#'
#' `CandidatePath` is the single type used by swereg R6 classes
#' ([RegistryStudy], [TTEPlan]) to own their multi-host directory knowledge.
#' Both classes hold `CandidatePath` instances as public fields, so their
#' resolution behavior is structurally identical -- it cannot drift.
#'
#' The cache is host-specific and must not persist across a save/load cycle.
#' Containing classes call [invalidate_candidate_paths()] from their `$save()`
#' methods before serialization to clear it.
#'
#' @section Methods:
#' \describe{
#'   \item{`$new(candidates, label = NULL)`}{Construct from a character vector
#'     of candidate paths.}
#'   \item{`$resolve()`}{Return the cached resolved path; otherwise walk the
#'     candidate list and pick the first that exists (or create the first
#'     whose parent exists), cache, and return.}
#'   \item{`$invalidate()`}{Clear the cache. The next `$resolve()` call
#'     re-walks the candidate list.}
#'   \item{`$is_resolved()`}{`TRUE` if the cache is populated and the cached
#'     path still exists; `FALSE` otherwise.}
#'   \item{`$print()`}{Show the candidate list, marking the cached-resolved
#'     entry with `>`.}
#' }
#'
#' @seealso [first_existing_path()] for the stateless primitive used by
#'   `$resolve()`; [invalidate_candidate_paths()] for the save-time cache
#'   clearer.
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' cp <- CandidatePath$new(c("/definitely/not/there", d), "my_dir")
#' cp$resolve()
#' cp$is_resolved()
#' print(cp)
#' cp$invalidate()
#' cp$is_resolved()
#' @export
CandidatePath <- R6::R6Class(
  "CandidatePath",
  public = list(
    #' @field candidates Character vector of candidate paths, in priority
    #'   order. Read-only after construction (modify via a new instance).
    candidates = NULL,

    #' @field label Short human-readable label used in error messages and
    #'   `$print()` output.
    label = NULL,

    #' @description Construct a CandidatePath.
    #' @param candidates Character vector of candidate paths. Must be
    #'   non-empty.
    #' @param label Optional label for error messages and printing.
    initialize = function(candidates, label = NULL) {
      if (length(candidates) == 0) {
        stop("CandidatePath: candidates must be a non-empty character vector", call. = FALSE)
      }
      self$candidates <- as.character(candidates)
      self$label <- if (is.null(label) || !nzchar(label)) "path" else label
    },

    #' @description Resolve the candidate list to a concrete path on the
    #'   current host. Returns the cached value if valid; otherwise walks the
    #'   candidates and caches the first that exists (or is creatable).
    #' @return A single character path.
    resolve = function() {
      cached <- private$.cache
      if (!is.null(cached) && dir.exists(cached)) {
        return(cached)
      }
      resolved <- first_existing_path(self$candidates, self$label)
      private$.cache <- resolved
      resolved
    },

    #' @description Clear the cached resolved path. The next `$resolve()`
    #'   call will re-walk the candidate list.
    #' @return `invisible(self)`.
    invalidate = function() {
      private$.cache <- NULL
      invisible(self)
    },

    #' @description Check whether a cached path exists and is still valid.
    #' @return `TRUE` if cached and the cached directory still exists,
    #'   `FALSE` otherwise.
    is_resolved = function() {
      !is.null(private$.cache) && dir.exists(private$.cache)
    },

    #' @description Print the candidate list, marking the cached-resolved
    #'   entry with `>`.
    #' @param ... Ignored.
    print = function(...) {
      cat("<CandidatePath: ", self$label, ">\n", sep = "")
      cached <- private$.cache
      for (p in self$candidates) {
        marker <- if (!is.null(cached) && identical(p, cached)) "  > " else "    "
        cat(marker, p, "\n", sep = "")
      }
      invisible(self)
    }
  ),
  private = list(
    .cache = NULL
  )
)
