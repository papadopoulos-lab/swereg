# =============================================================================
# Schema migration on read
# =============================================================================
# swereg 26.15.0 renamed two stored columns:
#
#   * `entry_band_id` becomes `enrollment_period_id`, in the panel of a
#     [TTEEnrollment] (its `$data`). TTEEnrollment schema 4 becomes 5.
#   * `band` becomes `follow_up_interval`, in the stored risk-difference rows
#     of a [TTEPlan] (`results_ett[[ett_id]]$rd_*`). TTEPlan schema 3
#     becomes 4.
#
# The rename happens in `qs2_read()`, BEFORE that function calls
# `check_version()`. A deserialised R6 object keeps the method bodies it was
# saved with. So `check_version()` on an object read from disk is the body
# the OLD release wrote. That body refuses an object whose stored schema is
# lower than the package constant. A migration placed inside the new
# `check_version()` is never reached.
#
# The migration renames columns and records the new schema version. It
# changes no value. An object older than the previous schema is left alone,
# and `check_version()` then refuses it as before.
# =============================================================================

# The schema each migration reads, and the schema it writes.
.TTE_ENROLLMENT_SCHEMA_MIGRATE_FROM <- 4L
.TTE_PLAN_SCHEMA_MIGRATE_FROM <- 3L

#' Migrate an object that `qs2_read()` deserialised
#'
#' Applies the 26.15.0 column renames to a [TTEEnrollment] at schema 4 or a
#' [TTEPlan] at schema 3. It returns every other object unchanged.
#'
#' @param obj The deserialised object.
#' @param path Character or `NULL`, the file `obj` was read from. It keys the
#'   once-per-file legacy-gap warning.
#' @return `obj`. A migrated R6 object is changed in place.
#' @noRd
.tte_migrate_on_read <- function(obj, path = NULL) {
  if (!inherits(obj, "R6")) {
    return(obj)
  }
  if (inherits(obj, "TTEEnrollment")) {
    .tte_migrate_enrollment(obj)
    .tte_warn_legacy_gap_on_read(obj, path)
  } else if (inherits(obj, "TTEPlan")) {
    .tte_migrate_plan(obj)
  }
  return(obj)
}

# The normalised paths that already gave the legacy-gap warning in this R
# process. See `.tte_warn_legacy_gap_on_read()`.
.tte_legacy_gap_seen <- new.env(parent = emptyenv())

#' Forget every path that gave the legacy-gap warning
#'
#' For tests only. It makes the next read of each file warn again.
#' @return `NULL`, invisibly.
#' @noRd
.tte_reset_legacy_gap_seen <- function() {
  rm(list = ls(.tte_legacy_gap_seen, all.names = TRUE), envir = .tte_legacy_gap_seen)
  return(invisible(NULL))
}

#' Warn when a read enrollment predates `weeks_to_observation_gap`
#'
#' Runs on every read of an enrollment at the current schema, after the
#' migration. An object below that schema is left alone, because
#' `check_version()` refuses it next. An enrollment saved before
#' swereg 26.15.0 runs its saved method bodies, so this read is the one place
#' current code can warn about it. The test is `.tte_is_legacy_gap_panel()`,
#' and the text is `.TTE_LEGACY_GAP_WARNING`.
#'
#' The warning fires once per file per R process. `.tte_legacy_gap_seen`
#' records the normalised path of every file that already warned. A later
#' read of the same file in the same process does not warn. A new process
#' warns again. A read without a path warns every time.
#'
#' The function sets the private flag `.legacy_gap_warned` where the object
#' has that binding, also on a read that does not warn. So
#' `s5_prepare_outcome()` does not warn a second time. An object saved before
#' 26.15.0 has no such binding, and its private environment is locked. Its
#' saved `s5_prepare_outcome()` never warns, so it needs no flag.
#' @param obj A [TTEEnrollment].
#' @param path Character or `NULL`, the file `obj` was read from.
#' @return `obj`, invisibly.
#' @noRd
.tte_warn_legacy_gap_on_read <- function(obj, path = NULL) {
  if (.tte_stored_schema(obj) != .TTE_ENROLLMENT_SCHEMA_VERSION) {
    return(invisible(obj))
  }
  if (!.tte_is_legacy_gap_panel(obj$data, obj$design, obj$steps_completed)) {
    return(invisible(obj))
  }
  key <- if (is.null(path)) NULL else normalizePath(path, mustWork = FALSE)
  if (is.null(key) || !exists(key, envir = .tte_legacy_gap_seen)) {
    warning(.TTE_LEGACY_GAP_WARNING, call. = FALSE)
  }
  if (!is.null(key)) {
    assign(key, TRUE, envir = .tte_legacy_gap_seen)
  }
  private <- obj$.__enclos_env__$private
  if (
    is.environment(private) &&
      exists(".legacy_gap_warned", envir = private, inherits = FALSE)
  ) {
    assign(".legacy_gap_warned", TRUE, envir = private)
  }
  return(invisible(obj))
}

#' Read the stored schema version of an R6 object
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

#' Record a schema version on an R6 object
#' @noRd
.tte_set_schema <- function(obj, version) {
  assign(
    ".schema_version",
    as.integer(version),
    envir = obj$.__enclos_env__$private
  )
  return(invisible(obj))
}

#' Rename `entry_band_id` to `enrollment_period_id` in an enrollment panel
#'
#' Acts only on an object at schema 4, the schema of swereg 26.10.19 to
#' 26.14.0.
#' @param obj A [TTEEnrollment].
#' @noRd
.tte_migrate_enrollment <- function(obj) {
  if (.tte_stored_schema(obj) != .TTE_ENROLLMENT_SCHEMA_MIGRATE_FROM) {
    return(invisible(obj))
  }
  data <- obj$data
  if (
    data.table::is.data.table(data) &&
      "entry_band_id" %in% names(data) &&
      !"enrollment_period_id" %in% names(data)
  ) {
    data.table::setnames(data, "entry_band_id", "enrollment_period_id")
  }
  .tte_set_schema(obj, .TTE_ENROLLMENT_SCHEMA_VERSION)
  return(invisible(obj))
}

#' Rename `band` to `follow_up_interval` in the stored risk-difference rows
#'
#' Acts only on a plan at schema 3, the schema of swereg 26.10.x to 26.14.0.
#' A risk-difference row sits in `results_ett[[ett_id]]` under a slot name
#' that starts with `rd_`. The full curves sit under `rd_curve_` slots and
#' carry the design's `tstop` column, so the migration leaves them alone.
#' @param obj A [TTEPlan].
#' @noRd
.tte_migrate_plan <- function(obj) {
  if (.tte_stored_schema(obj) != .TTE_PLAN_SCHEMA_MIGRATE_FROM) {
    return(invisible(obj))
  }
  results <- obj$results_ett
  if (is.list(results)) {
    for (ett_id in names(results)) {
      res <- results[[ett_id]]
      if (!is.list(res)) {
        next
      }
      slots <- grep("^rd_", names(res), value = TRUE)
      slots <- slots[!startsWith(slots, "rd_curve_")]
      for (slot in slots) {
        row <- res[[slot]]
        if (
          data.table::is.data.table(row) &&
            "band" %in% names(row) &&
            !"follow_up_interval" %in% names(row)
        ) {
          data.table::setnames(row, "band", "follow_up_interval")
        }
      }
    }
  }
  .tte_set_schema(obj, .TTE_PLAN_SCHEMA_VERSION)
  return(invisible(obj))
}
