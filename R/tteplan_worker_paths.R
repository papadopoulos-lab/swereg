# =============================================================================
# Package-level workers for Loop 1 and Loop 2 (not exported)
# =============================================================================

# --- Enrollment counts persistence helpers -----------------------------------

#' Build path for a per-enrollment counts file.
#' @noRd
.enrollment_counts_path <- function(output_dir, prefix, eid) {
  return(file.path(output_dir, paste0(prefix, "_enrollment_counts_", eid, ".qs2")))
}

# --- s1 work directory + path constructors -----------------------------------
#
# Loop 1 splits into four sub-steps (s1a..s1d). Each sub-step runs in a
# subprocess (parallel for skeleton-level work, single for enrollment-level
# work) and communicates with the next sub-step via files in a per-project
# work directory:
#
#   {data_meta_dir}/s1_work/{project_prefix}/
#
# This directory is transient dataflow, not a cache: it is cleared at the
# start of every $s1_generate_enrollments_and_ipw() call and removed again on
# success (Phase 5': s1 has no resume, so nothing here is ever read across
# runs).
#
# File-name conventions:
#
#   s1a_cache_enr{eid}_{skel_basename}            ← projected skeleton cache
#   s1a_pre_enr{eid}_{skel_basename}              ← (tuples, attrition) chunk
#   s1b_enrolled_ids_enr{eid}.qs2                 ← post-draw enrolled IDs
#   s1c_panel_enr{eid}_{skel_basename}            ← per-(enr, skel) panel chunk
#
# The work_dir is removed on successful completion of $s1_generate_*().

#' Resolve and (optionally) create the s1 work directory for a plan.
#'
#' `{data_meta_dir}/s1_work/{project_prefix}/` -- transient dataflow between
#' the s1 sub-steps, cleared at the start of each run and removed on success.
#' @param plan A TTEPlan.
#' @param ensure_exists Create the directory if missing (default TRUE).
#' @noRd
.s1_work_dir <- function(plan, ensure_exists = TRUE) {
  if (is.null(plan$registrystudy)) {
    stop(
      "TTEPlan has no embedded RegistryStudy. ",
      "The s1 work directory is derived from study$data_meta_dir.",
      call. = FALSE
    )
  }
  meta_dir <- plan$registrystudy$data_meta_dir
  if (is.null(meta_dir) || !nzchar(meta_dir)) {
    stop(
      "Could not resolve study$data_meta_dir for the s1 work directory.",
      call. = FALSE
    )
  }
  # ABSOLUTE, always: files under this work dir become batchit declared
  # `outputs`, and batchit rejects a relative declared-output path. Safe to
  # normalize `meta_dir` itself because it is guaranteed to EXIST --
  # first_existing_path() (R/path_resolution.R) returns an existing candidate,
  # creates the first one whose parent exists, or errors. That matters:
  # normalizePath(mustWork = FALSE) returns an absolute path only for a path
  # that exists, and silently returns a non-existent relative path UNCHANGED.
  dir <- file.path(
    normalizePath(meta_dir, mustWork = FALSE),
    "s1_work",
    plan$project_prefix
  )
  if (ensure_exists && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  return(dir)
}

#' @noRd
.s1a_cache_path <- function(work_dir, eid, skel_basename) {
  return(file.path(work_dir, sprintf("s1a_cache_enr%s_%s", eid, skel_basename)))
}
#' @noRd
.s1a_pre_path <- function(work_dir, eid, skel_basename) {
  return(file.path(work_dir, sprintf("s1a_pre_enr%s_%s", eid, skel_basename)))
}

# --- s1a declared-output NAMES (the batchit `outputs` keys) -----------------
#
# s1a runs `style = "staged_writer"`: the PARENT declares every file an item
# will write, and the WORKER asks for each destination by NAME via
# .batch_where_to_write_output(). The two ends must agree on the name set
# exactly -- an unknown name is a hard batchit error inside the child, not a
# silent fallback. These three helpers are the single source of that name set,
# so parent and worker cannot drift apart: the parent calls
# .s1a_outputs_for_skeleton() and the worker calls .s1a_cache_name() /
# .s1a_pre_name() with the same enrollment id.
#
# The names are batchit keys; the VALUES are the on-disk paths that s1b, s1c
# and s1d later read back through .s1a_cache_path() / .s1a_pre_path(). Keeping
# both in one function is what makes "declared here" and "read there" provably
# the same string.

#' @noRd
.s1a_cache_name <- function(eid) paste0("cache_", eid)

#' @noRd
.s1a_pre_name <- function(eid) paste0("pre_", eid)

#' Every declared output of one s1a item: `2 x length(eids)` named paths.
#'
#' Grouped by enrollment (`cache_<eid>`, `pre_<eid>`, next enrollment, ...).
#' @noRd
.s1a_outputs_for_skeleton <- function(work_dir, eids, skel_basename) {
  out <- unlist(lapply(eids, function(eid) {
    x <- c(
      .s1a_cache_path(work_dir, eid, skel_basename),
      .s1a_pre_path(work_dir, eid, skel_basename)
    )
    names(x) <- c(.s1a_cache_name(eid), .s1a_pre_name(eid))
    return(x)
  }))
  return(if (is.null(out)) character(0) else out)
}

#' @noRd
.s1b_enrolled_ids_path <- function(work_dir, eid) {
  return(file.path(work_dir, sprintf("s1b_enrolled_ids_enr%s.qs2", eid)))
}
#' @noRd
.s1c_panel_path <- function(work_dir, eid, skel_basename) {
  return(file.path(work_dir, sprintf("s1c_panel_enr%s_%s", eid, skel_basename)))
}

#' Restore enrollment counts from per-enrollment sidecar files on disk.
#' Only fills entries not already present on the plan.
#' @noRd
.restore_enrollment_counts <- function(plan, output_dir, enrollment_ids) {
  for (eid in enrollment_ids) {
    if (!is.null(plan$enrollment_counts[[eid]])) {
      next
    }
    counts_path <- .enrollment_counts_path(output_dir, plan$project_prefix, eid)
    if (file.exists(counts_path)) {
      plan$enrollment_counts[[eid]] <- qs2_read(counts_path)
    }
  }
  return(invisible(NULL))
}
