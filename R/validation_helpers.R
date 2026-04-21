# Internal validation helper functions for swereg package
# Most are not exported; a small subset below is exported for use by
# user-authored add_* functions (see vignette("custom-add-functions")).

#' Snapshot a skeleton's structural state for post-hoc validation
#'
#' Internal helper. Captures cheap structural metadata (row count,
#' column set) before an \code{add_*} function runs, so that
#' \code{validate_skeleton_after_add()} can check afterwards that the
#' function honoured the \code{add_*} contract (modify by reference,
#' no row changes, no structural column drops).
#'
#' Called automatically by \code{.apply_code_entry_impl()} around every
#' user-registered \code{fn} in \code{RegistryStudy$register_codes()},
#' so any custom \code{add_*} plugged into the pipeline is contract-
#' validated for free. Not exported -- users should not call this
#' directly; instead, register custom \code{add_*} functions via
#' \code{RegistryStudy$register_codes()} and let the pipeline do the
#' bookkeeping.
#'
#' @param skeleton A skeleton \code{data.table} (must have the standard
#'   structural columns \code{id}, \code{isoyear}, \code{isoyearweek},
#'   \code{is_isoyear}).
#' @param input_data Optional \code{data.table} whose row count and
#'   column set to fingerprint as well. The pipeline always passes
#'   \code{NULL} (input mutation is harmless when rawbatch data is
#'   discarded after each call), but the parameter exists so
#'   interactive / test callers can opt in.
#' @return A list tagged with class \code{swereg_skeleton_snapshot},
#'   consumed by \code{validate_skeleton_after_add()}.
#' @keywords internal
skeleton_snapshot <- function(skeleton, input_data = NULL) {
  if (!is.data.table(skeleton)) {
    stop(
      "skeleton_snapshot(): `skeleton` must be a data.table, got: ",
      paste(class(skeleton), collapse = "/"),
      call. = FALSE
    )
  }

  structural_cols <- c("id", "isoyear", "isoyearweek", "is_isoyear")
  missing_cols <- setdiff(structural_cols, names(skeleton))
  if (length(missing_cols) > 0L) {
    stop(
      "skeleton_snapshot(): `skeleton` is missing structural columns: ",
      paste(missing_cols, collapse = ", "),
      ". Did you create it with create_skeleton()?",
      call. = FALSE
    )
  }

  out <- list(
    nrow       = nrow(skeleton),
    cols       = copy(names(skeleton)),
    input_nrow = NA_integer_,
    input_cols = NULL
  )

  if (!is.null(input_data)) {
    if (!is.data.table(input_data)) {
      stop(
        "skeleton_snapshot(): `input_data` must be a data.table or NULL, ",
        "got: ", paste(class(input_data), collapse = "/"),
        call. = FALSE
      )
    }
    out$input_nrow <- nrow(input_data)
    out$input_cols <- copy(names(input_data))
  }

  class(out) <- c("swereg_skeleton_snapshot", "list")
  out
}

#' Validate a skeleton after an add_* function has mutated it
#'
#' Internal helper. Given a pre-state captured by
#' \code{skeleton_snapshot()}, check that an \code{add_*} function
#' honoured the swereg \code{add_*} contract:
#'
#' \itemize{
#'   \item Reference semantics: \code{skeleton} is still a
#'     \code{data.table}.
#'   \item Row preservation: \code{nrow(skeleton)} unchanged.
#'   \item Structural columns preserved: \code{id}, \code{isoyear},
#'     \code{isoyearweek}, \code{is_isoyear} all still present.
#'   \item Expected new columns added: if \code{expected_new_cols} is
#'     supplied, every name in it is present on the skeleton.
#'   \item (Opt-in) Input-data not mutated: only checked when
#'     \code{input_data} is passed on both sides.
#' }
#'
#' Called automatically by \code{.apply_code_entry_impl()} around every
#' user-registered \code{fn} in \code{RegistryStudy$register_codes()}.
#' Not exported.
#'
#' @param skeleton The skeleton \code{data.table} after the
#'   \code{add_*} function ran.
#' @param snapshot Pre-state from \code{skeleton_snapshot()}.
#' @param expected_new_cols Optional character vector of column names
#'   that the function should have added.
#' @param input_data Optional \code{data.table} to check for mutation.
#' @param context Character label for error messages.
#' @return Invisibly, \code{skeleton}. Called for side effects.
#' @keywords internal
validate_skeleton_after_add <- function(
  skeleton,
  snapshot,
  expected_new_cols = NULL,
  input_data = NULL,
  context = "add_* function"
) {
  if (!inherits(snapshot, "swereg_skeleton_snapshot")) {
    stop(
      context, ": `snapshot` must be the value returned by ",
      "skeleton_snapshot(); got class ",
      paste(class(snapshot), collapse = "/"),
      call. = FALSE
    )
  }

  if (!is.data.table(skeleton)) {
    stop(
      context, " broke reference semantics: the object bound to ",
      "`skeleton` is no longer a data.table (got ",
      paste(class(skeleton), collapse = "/"), "). ",
      "add_* functions must modify the skeleton in place via ",
      "`skeleton[data, on = ..., := ...]`; they must not return a new ",
      "object via `merge()`, `dplyr::left_join()`, etc.",
      call. = FALSE
    )
  }

  if (nrow(skeleton) != snapshot$nrow) {
    stop(
      context, " changed skeleton row count: before = ",
      snapshot$nrow, ", after = ", nrow(skeleton), ". ",
      "add_* functions must preserve row count. Common causes: ",
      "(1) using `merge()` instead of an update-by-reference join, ",
      "(2) a non-equi join that multiplies matching rows, ",
      "(3) an inner join that drops skeleton rows without matches.",
      call. = FALSE
    )
  }

  structural_cols <- c("id", "isoyear", "isoyearweek", "is_isoyear")
  dropped <- setdiff(structural_cols, names(skeleton))
  if (length(dropped) > 0) {
    stop(
      context, " dropped structural skeleton columns: ",
      paste(dropped, collapse = ", "),
      ". These columns are read-only and must be preserved.",
      call. = FALSE
    )
  }

  if (!is.null(expected_new_cols)) {
    new_cols <- setdiff(names(skeleton), snapshot$cols)
    missing_expected <- setdiff(expected_new_cols, names(skeleton))
    if (length(missing_expected) > 0) {
      stop(
        context, " did not add the expected columns: ",
        paste(missing_expected, collapse = ", "),
        ". Check that your loop over `names(codes)` actually writes ",
        "to the skeleton (e.g. `skeleton[..., (nm) := TRUE]`).",
        call. = FALSE
      )
    }
    if (length(new_cols) == 0) {
      warning(
        context, " added no new columns to the skeleton. ",
        "Did the pattern list match any rows?",
        call. = FALSE
      )
    }
  }

  if (!is.null(input_data) && !is.null(snapshot$input_cols)) {
    if (!is.data.table(input_data)) {
      warning(
        context, ": `input_data` passed to validate_skeleton_after_add() ",
        "is no longer a data.table (got ",
        paste(class(input_data), collapse = "/"), ")",
        call. = FALSE
      )
    } else {
      if (nrow(input_data) != snapshot$input_nrow) {
        warning(
          context, " changed input_data row count: before = ",
          snapshot$input_nrow, ", after = ", nrow(input_data),
          ". add_* functions should not mutate their inputs.",
          call. = FALSE
        )
      }
      added_input_cols <- setdiff(names(input_data), snapshot$input_cols)
      if (length(added_input_cols) > 0) {
        warning(
          context, " added columns to input_data: ",
          paste(added_input_cols, collapse = ", "),
          ". add_* functions should not mutate their inputs -- if you ",
          "need scratch columns (e.g. a computed `isoyearweek`), copy ",
          "the input first or drop the scratch columns before returning.",
          call. = FALSE
        )
      }
    }
  }

  invisible(skeleton)
}

#' Validate skeleton structure
#' @param skeleton A data.table that should be a valid skeleton
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_skeleton_structure <- function(skeleton) {
  if (!is.data.table(skeleton)) {
    stop("skeleton must be a data.table. Did you forget to run setDT(skeleton)?")
  }
  
  required_cols <- c("id", "isoyear", "isoyearweek", "is_isoyear")
  missing_cols <- setdiff(required_cols, names(skeleton))
  
  if (length(missing_cols) > 0) {
    stop("skeleton is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
         "Did you create it with create_skeleton()?")
  }
  
  # Check for reasonable data types
  if (!is.logical(skeleton$is_isoyear)) {
    stop("skeleton$is_isoyear must be logical (TRUE/FALSE)")
  }
  
  if (!is.integer(skeleton$isoyear)) {
    stop("skeleton$isoyear must be integer")
  }
}

#' Validate ID column exists in dataset
#' @param data A data.table containing the data
#' @param id_name Character string with the ID column name
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_id_column <- function(data, id_name) {
  if (!is.data.table(data)) {
    stop("data must be a data.table. Did you forget to run setDT(data)?")
  }
  
  if (!id_name %in% names(data)) {
    stop("Column '", id_name, "' not found in data.\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(data)?")
  }
}

#' Validate pattern list for diagnoses/operations/prescriptions/cods
#' @param pattern_list Named list of patterns
#' @param pattern_type Character string describing the pattern type (for error messages)
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_pattern_list <- function(pattern_list, pattern_type = "patterns") {
  if (!is.list(pattern_list)) {
    stop(pattern_type, " must be a list")
  }
  
  if (length(pattern_list) == 0) {
    stop(pattern_type, " list is empty. Please provide patterns like:\n",
         "list('depression' = c('F32', 'F33'), 'anxiety' = c('F40', 'F41'))")
  }
  
  if (is.null(names(pattern_list)) || any(names(pattern_list) == "")) {
    stop("All items in ", pattern_type, " list must be named. Example:\n",
         "list('depression' = c('F32', 'F33'), 'anxiety' = c('F40', 'F41'))")
  }
  
  # Check each pattern is a character vector
  for (i in seq_along(pattern_list)) {
    if (!is.character(pattern_list[[i]])) {
      stop("Pattern '", names(pattern_list)[i], "' must be a character vector")
    }
    if (length(pattern_list[[i]]) == 0) {
      stop("Pattern '", names(pattern_list)[i], "' is empty")
    }
  }
}

#' Validate data structure for add_* functions
#' @param data A data.table containing the data
#' @param required_cols Character vector of required column names
#' @param data_type Character string describing the data type (for error messages)
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_data_structure <- function(data, required_cols = NULL, data_type = "data") {
  if (!is.data.table(data)) {
    stop(data_type, " must be a data.table. Did you forget to run setDT(", data_type, ")?")
  }
  
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(data_type, " is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
           "Available columns: ", paste(names(data), collapse = ", "), "\n",
           "Did you forget to run make_lowercase_names(", data_type, ")?")
    }
  }
  
  if (nrow(data) == 0) {
    warning(data_type, " has 0 rows. No data will be added to skeleton.")
  }
}

#' Validate isoyear parameter
#' @param isoyear Integer year value
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_isoyear <- function(isoyear) {
  if (!is.numeric(isoyear) || length(isoyear) != 1) {
    stop("isoyear must be a single numeric value")
  }
  
  if (isoyear != round(isoyear)) {
    stop("isoyear must be an integer")
  }
  
  if (isoyear < 1900 || isoyear > 2050) {
    stop("isoyear must be between 1900 and 2050, got: ", isoyear)
  }
}

#' Validate date column exists and is proper format
#' @param data A data.table containing the data
#' @param date_col Character string with the date column name
#' @param data_type Character string describing the data type (for error messages)
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_date_column <- function(data, date_col, data_type = "data") {
  if (!date_col %in% names(data)) {
    stop(data_type, " is missing required date column: ", date_col, "\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(", data_type, ")?")
  }
  
  # Check if date column has reasonable values
  if (all(is.na(data[[date_col]]))) {
    warning("Date column '", date_col, "' in ", data_type, " contains only NA values")
  }
}

#' Validate prescription-specific columns
#' @param data A data.table containing prescription data
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_prescription_data <- function(data) {
  validate_data_structure(data, data_type = "prescription data")
  
  # Check for required prescription columns
  required_cols <- c("edatum", "fddd")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop("Prescription data is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
         "Required columns: edatum (prescription date), fddd (treatment duration)\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(prescription_data)?")
  }
  
  # Check for either atc or produkt column
  if (!any(c("atc", "produkt") %in% names(data))) {
    stop("Prescription data must have either 'atc' or 'produkt' column for drug codes.\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(prescription_data)?")
  }
}

#' Validate death registry data columns
#' @param data A data.table containing death registry data
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_death_data <- function(data) {
  validate_data_structure(data, data_type = "death registry data")
  
  # Check for required death columns
  required_cols <- c("dodsdat")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop("Death registry data is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
         "Required columns: dodsdat (death date)\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(death_data)?")
  }
  
  # Check for cause of death columns
  cause_cols <- c(
    stringr::str_subset(names(data), "^ulorsak"),
    stringr::str_subset(names(data), "^morsak")
  )
  
  if (length(cause_cols) == 0) {
    stop("Death registry data must have cause of death columns (ulorsak or morsak).\n",
         "Available columns: ", paste(names(data), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(death_data)?")
  }
}


#' Validate date columns exist and are proper format
#' @param data A data.table containing the data
#' @param expected_date_cols Character vector of expected date column names
#' @param data_type Character string describing the data type (for error messages)
#' @return Nothing if valid, stops with error if invalid
#' @keywords internal
validate_date_columns <- function(data, expected_date_cols, data_type = "data") {
  missing_cols <- setdiff(expected_date_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Date columns not found in ", data_type, ": ", paste(missing_cols, collapse = ", "), "\n",
         "Please clean the data first using:\n",
         "swereg::make_lowercase_names(", data_type, ", date_columns = c('", paste(expected_date_cols, collapse = "', '"), "'))")
  }
  
  # Check if date columns have reasonable values and are Date objects
  for (date_col in expected_date_cols) {
    if (all(is.na(data[[date_col]]))) {
      warning("Date column '", date_col, "' in ", data_type, " contains only NA values")
    }
    
    if (!inherits(data[[date_col]], "Date")) {
      stop("Column '", date_col, "' in ", data_type, " is not a Date object. Please clean the data first using:\n",
           "swereg::make_lowercase_names(", data_type, ", date_columns = c('", paste(expected_date_cols, collapse = "', '"), "'))")
    }
  }
}
