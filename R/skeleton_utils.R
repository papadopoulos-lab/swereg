# =============================================================================
# Skeleton Pipeline Utility Functions
# =============================================================================
# skeleton_save: Split and save skeleton sub-files
# skeleton_checkpoint: Profiling closure factory
# =============================================================================


#' Save skeleton output as sub-files split by ID count
#'
#' Splits a skeleton data.table into sub-files of `ids_per_file` unique IDs
#' each and saves them as `skeleton_{BBB}_{SS}.qs2` files. This keeps
#' individual files small enough for fast loading downstream.
#'
#' @param dt A data.table of skeleton data to save.
#' @param batch_number Integer batch number (used in file naming).
#' @param output_dir Character, directory for output files.
#' @param ids_per_file Integer, number of unique IDs per sub-file.
#'   Default: `1000L`.
#' @param id_col Character, name of the ID column in `dt`.
#'   Default: `"id"`.
#'
#' @return Character vector of file paths created.
#'
#' @examples
#' \dontrun{
#' files <- skeleton_save(
#'   skeleton,
#'   batch_number = 1,
#'   output_dir = config@skeleton_dir,
#'   ids_per_file = config@ids_per_skeleton_file,
#'   id_col = "id"
#' )
#' # → skeleton_001_01.qs2, skeleton_001_02.qs2, ...
#' }
#'
#' @family skeleton_utils
#' @export
skeleton_save <- function(
    dt,
    batch_number,
    output_dir,
    ids_per_file = 1000L,
    id_col = "id"
) {
  unique_ids <- unique(dt[[id_col]])
  id_groups <- split(
    unique_ids,
    ceiling(seq_along(unique_ids) / ids_per_file)
  )

  n_threads <- max(1L, floor(parallel::detectCores() / 2))
  paths <- character(length(id_groups))

  for (s in seq_along(id_groups)) {
    fname <- sprintf("skeleton_%03d_%02d.qs2", batch_number, s)
    fpath <- file.path(output_dir, fname)
    .qs_save(
      dt[get(id_col) %in% id_groups[[s]]],
      fpath,
      nthreads = n_threads
    )
    paths[s] <- fpath
  }

  paths
}


#' Create a profiling checkpoint closure
#'
#' Factory function that returns a closure for recording time and memory
#' usage at each step of skeleton processing. Call the closure with a label
#' at each step boundary. Call with `done = TRUE` to finalize and return
#' a data.table of all checkpoints.
#'
#' @return A function with signature `function(label = NULL, done = FALSE)`:
#'   - Call with a label string to record a checkpoint
#'   - Call with `done = TRUE` to return a data.table with columns
#'     `label`, `time`, and `mem_mb`
#'
#' @examples
#' \dontrun{
#' cp <- skeleton_checkpoint()
#' cp("start")
#' # ... do work ...
#' cp("STEP 1: Select IDs")
#' # ... more work ...
#' cp("STEP 2: Create skeleton")
#' prof_dt <- cp(done = TRUE)
#' # prof_dt is a data.table with columns: label, time, mem_mb
#' }
#'
#' @family skeleton_utils
#' @export
skeleton_checkpoint <- function() {
  prof <- list()

  function(label = NULL, done = FALSE) {
    if (done) {
      return(data.table::rbindlist(prof))
    }
    mem_mb <- sum(gc()[, 2])
    prof[[length(prof) + 1L]] <<- list(
      label = label,
      time = Sys.time(),
      mem_mb = mem_mb
    )
    invisible(NULL)
  }
}
