# =============================================================================
# Refuse a stored object from an older schema
# =============================================================================
# This release renamed the column that swereg used for two meanings:
#
#   * the calendar period of a skeleton week or a panel row is `period_id`
#   * the trial of a person-trial is `enrollment_period_id`
#
# TTEEnrollment schema 5 becomes 6, and TTEPlan schema 4 becomes 5.
#
# swereg does not migrate an older object. A deserialised R6 object keeps the
# method bodies it was saved with, and its panel keeps the old column names.
# An old body on a migrated panel, and a current helper on an old panel, both
# find no calendar column. The fitted model then loses its calendar term
# without an error. So `qs2_read()` refuses the object BEFORE it calls
# `check_version()`, which is the first method of the object that would run.
# The refusal reads the stored schema from the private field, and it calls no
# method of the object.
# =============================================================================

# The column name that swereg used for both meanings before schema 6. It is
# built from two parts, so that a search of the source for the retired name
# finds no live use of it.
.TTE_RETIRED_PERIOD_COLUMN <- paste0("trial", "_id")

# The remedy that every refusal states.
.TTE_REBUILD_REMEDY <- paste(
  "Rebuild the plan with s0 and re-run s1, then s2 and s3."
)

#' Refuse a stored enrollment or plan from an older schema
#'
#' `qs2_read()` calls this function on every object it deserialises, before it
#' calls `check_version()`. It stops on a [TTEEnrollment] below
#' `.TTE_ENROLLMENT_SCHEMA_VERSION` and on a [TTEPlan] below
#' `.TTE_PLAN_SCHEMA_VERSION`. It returns every other object unchanged.
#'
#' @param obj The deserialised object.
#' @param path Character(1), the file `obj` was read from. The error names it.
#' @return `obj`, invisibly, when the function does not stop.
#' @noRd
.tte_refuse_stale_on_read <- function(obj, path) {
  if (!inherits(obj, "R6")) {
    return(invisible(obj))
  }
  current <- if (inherits(obj, "TTEEnrollment")) {
    .TTE_ENROLLMENT_SCHEMA_VERSION
  } else if (inherits(obj, "TTEPlan")) {
    .TTE_PLAN_SCHEMA_VERSION
  } else {
    return(invisible(obj))
  }
  saved <- .tte_stored_schema(obj)
  if (saved >= current) {
    return(invisible(obj))
  }
  stop(
    "swereg refuses to read '",
    path,
    "'. It holds a ",
    class(obj)[1],
    " at schema version ",
    saved,
    ", and this swereg requires version ",
    current,
    ". A saved object runs the method bodies it was saved with, so swereg ",
    "does not migrate it. ",
    .TTE_REBUILD_REMEDY,
    call. = FALSE
  )
}

#' Read the stored schema version of an R6 object
#'
#' The function reads the private field `.schema_version`. It calls no method
#' of the object.
#' @param obj An R6 object with a private `.schema_version`.
#' @return The stored integer, or `0L` when the object holds none.
#' @noRd
.tte_stored_schema <- function(obj) {
  private <- obj$.__enclos_env__$private
  if (!is.environment(private)) {
    return(0L)
  }
  saved <- get0(".schema_version", envir = private, inherits = FALSE)
  if (is.null(saved)) {
    return(0L)
  }
  return(as.integer(saved))
}

#' Refuse pre-drawn enrollment ids that do not name the trial
#'
#' `enroll()` reads the trial of each pre-drawn person-trial from
#' `enrolled_ids$enrollment_period_id`. An s1 file from an earlier release
#' carries the trial under `.TTE_RETIRED_PERIOD_COLUMN` instead. The function
#' stops on that column, also when `enrollment_period_id` is present, because
#' the file then comes from a mixed run.
#'
#' @param enrolled_ids The `enrolled_ids` argument of `TTEEnrollment$new()`.
#' @return `enrolled_ids`, invisibly, when the function does not stop.
#' @noRd
.tte_check_enrolled_ids <- function(enrolled_ids) {
  nms <- names(enrolled_ids)
  if (.TTE_RETIRED_PERIOD_COLUMN %in% nms) {
    stop(
      "`enrolled_ids` carries a `",
      .TTE_RETIRED_PERIOD_COLUMN,
      "` column. This release reads the trial of each person-trial from ",
      "`enrollment_period_id`. An earlier swereg wrote these ids. ",
      .TTE_REBUILD_REMEDY,
      call. = FALSE
    )
  }
  if (!"enrollment_period_id" %in% nms) {
    stop(
      "`enrolled_ids` MUST carry an `enrollment_period_id` column, the ",
      "trial of each person-trial.",
      call. = FALSE
    )
  }
  return(invisible(enrolled_ids))
}
