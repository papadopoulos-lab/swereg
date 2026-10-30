# =============================================================================
# tteplan_check_spec
# =============================================================================

#' Check a YAML study specification and list every problem
#'
#' Reads one specification file and returns a table with one row per problem.
#' It never stops on a bad specification, so a caller can check many files in
#' one pass.
#'
#' @param spec_path Character scalar, the path of the YAML specification file.
#' @return A [data.table::data.table()] with one row per problem, and three
#'   character columns:
#'   \describe{
#'     \item{`path`}{The normalised key path of the problem, for example
#'       `$/study/implementation/version`. A problem with the whole file has
#'       the path `$`.}
#'     \item{`kind`}{The problem kind. The Details section lists the eleven
#'       kinds.}
#'     \item{`message`}{What is wrong, and the repair where swereg knows it.}
#'   }
#'   The table has zero rows when the checker finds no problem.
#'
#' @details
#' The checker reports eleven kinds of problem:
#' \describe{
#'   \item{`unreadable`}{The file is missing, is not valid UTF-8, or is not
#'     valid YAML. This row is the only row, because the checker has no
#'     parsed specification to check.}
#'   \item{`undeclared_key`}{The schema does not declare the key path. The
#'     message lists the keys that the context accepts.}
#'   \item{`retired_key`}{The schema refuses the key path. The message names
#'     the replacement.}
#'   \item{`invalid_enrollment_id`}{An enrollment has no `id`, or its `id` is
#'     not one non-empty character string or one number. The checker refuses
#'     a sequence, a mapping, and an empty or whitespace-only string. It
#'     writes one row for each such enrollment.}
#'   \item{`duplicate_enrollment_id`}{Two or more enrollments carry the same
#'     `id`. The checker writes one row for each repeated id. It compares
#'     only the ids that give no `invalid_enrollment_id` row.}
#'   \item{`version_mismatch`}{The file name has the form `spec_vNNN.yaml`,
#'     and `study$implementation$version` is missing or is not `vNNN`. The
#'     checker skips this check for every other file name.}
#'   \item{`outcome_selecting_exclusion`}{An exclusion criterion has the
#'     window `lifetime_before_and_after_baseline` on a variable of an
#'     outcome. That window reads the weeks after time zero, so the criterion
#'     selects on the follow-up outcome. The checker writes one row for each
#'     criterion and outcome. The checker reads the global
#'     `exclusion_criteria` and each enrollment's `additional_exclusion`.}
#'   \item{`enrollment_family`}{An enrollment's `family` is not one non-empty
#'     character string. Or some enrollments set `family` and others do not.
#'     When one enrollment sets `family`, every enrollment MUST set it. The
#'     checker writes one row for each bad value, and one row that names the
#'     enrollments without a `family`.}
#'   \item{`enrollment_rule_exempt`}{An enrollment sets
#'     `minimum_events_rule_exempt` to a value that is not one logical, `true`
#'     or `false`. The checker writes one row for each such enrollment, and
#'     the message names the enrollment id.}
#'   \item{`feasibility_follow_up_weeks`}{The top-level key
#'     `feasibility_follow_up_weeks` is not one positive whole number of
#'     weeks. The checker writes one row, and the message names the key.}
#'   \item{`read_error`}{[tteplan_read_spec()] stops after its key gate. The
#'     message is the message of that error.}
#' }
#'
#' The checker reports every undeclared key path and every retired key path.
#' [tteplan_read_spec()] reports them too, but stops after them. The checker
#' calls [tteplan_read_spec()] only when no key path is undeclared or retired,
#' because the key gate stops it first. [tteplan_read_spec()] stops at its
#' first problem after the key gate, so a table holds at most one
#' `read_error` row.
#'
#' A warning from [tteplan_read_spec()], for example on an open question,
#' reaches the caller unchanged.
#'
#' @seealso [tteplan_read_spec()], which stops at the first problem, and
#'   `vignette("tte-spec-schema", package = "swereg")` for the key paths the
#'   schema declares.
#' @export
#' @examples
#' spec_dir <- tempfile()
#' dir.create(spec_dir)
#' spec_path <- file.path(spec_dir, "spec_v002.yaml")
#' writeLines(
#'   c(
#'     "study:",
#'     "  colour: blue",
#'     "  implementation:",
#'     "    project_prefix: example",
#'     "    version: v001"
#'   ),
#'   spec_path
#' )
#' tteplan_check_spec(spec_path)
#' unlink(spec_dir, recursive = TRUE)
tteplan_check_spec <- function(spec_path) {
  if (!is.character(spec_path) || length(spec_path) != 1L || is.na(spec_path)) {
    stop("spec_path must be one file path.", call. = FALSE)
  }

  parsed <- .tte_check_spec_parse(spec_path)
  if (!is.null(parsed$problem)) {
    return(.tte_check_spec_table(list(.tte_check_spec_row(
      "$",
      "unreadable",
      parsed$problem
    ))))
  }
  spec <- parsed$spec

  rows <- .tte_check_spec_keys(spec)
  key_gate_passes <- length(rows) == 0L
  id_value_rows <- .tte_check_spec_enrollment_id_values(spec)
  rows <- c(rows, id_value_rows)
  id_rows <- .tte_check_spec_enrollment_ids(spec)
  rows <- c(rows, id_rows)
  rows <- c(rows, .tte_check_spec_version(spec, spec_path))
  outcome_rows <- .tte_check_spec_outcome_exclusions(spec)
  rows <- c(rows, outcome_rows)
  family_found <- .tte_enrollment_family_problems(spec)
  rows <- c(rows, .tte_check_spec_family(family_found))
  exempt_found <- .tte_enrollment_exempt_problems(spec)
  rows <- c(rows, .tte_check_spec_exempt(exempt_found))
  feasibility_found <- .tte_feasibility_follow_up_problems(spec)
  rows <- c(rows, .tte_check_spec_feasibility_follow_up(feasibility_found))

  # `tteplan_read_spec()` runs the key gate first, so it can report a later
  # problem only when the gate passes. The keys above are its first stop.
  if (key_gate_passes) {
    read_error <- tryCatch(
      {
        tteplan_read_spec(spec_path)
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    # `tteplan_read_spec()` stops on a missing or bad enrollment id before it
    # reads the enrollments. It stops on the outcome-selecting exclusions,
    # then on the enrollment families, then on the bad exemption values, then
    # on a bad feasibility_follow_up_weeks, then on the repeated enrollment
    # ids, as its last five stops. The rows above already report each
    # finding.
    if (
      length(id_value_rows) > 0L &&
        identical(read_error, .tte_enrollment_id_message(id_value_rows))
    ) {
      read_error <- NULL
    }
    if (
      length(outcome_rows) > 0L &&
        identical(
          read_error,
          .tte_outcome_selecting_message(.tte_outcome_selecting_exclusions(
            spec
          ))
        )
    ) {
      read_error <- NULL
    }
    if (
      length(family_found) > 0L &&
        identical(read_error, .tte_enrollment_family_message(family_found))
    ) {
      read_error <- NULL
    }
    if (
      length(exempt_found) > 0L &&
        identical(read_error, .tte_enrollment_exempt_message(exempt_found))
    ) {
      read_error <- NULL
    }
    if (
      length(feasibility_found) > 0L &&
        identical(
          read_error,
          .tte_feasibility_follow_up_message(feasibility_found)
        )
    ) {
      read_error <- NULL
    }
    if (
      length(id_rows) > 0L &&
        identical(read_error, .tte_enrollment_id_message(id_rows))
    ) {
      read_error <- NULL
    }
    if (!is.null(read_error)) {
      rows <- c(rows, list(.tte_check_spec_row("$", "read_error", read_error)))
    }
  }

  return(.tte_check_spec_table(rows))
}


#' One problem row
#'
#' @param path,kind,message Character scalars.
#' @return A list with the three fields.
#' @noRd
.tte_check_spec_row <- function(path, kind, message) {
  return(list(path = path, kind = kind, message = message))
}


#' Bind problem rows into the returned table
#'
#' @param rows A list of rows from `.tte_check_spec_row()`.
#' @return A data.table with the character columns `path`, `kind` and
#'   `message`, and zero rows when `rows` is empty.
#' @noRd
.tte_check_spec_table <- function(rows) {
  return(data.table::data.table(
    path = vapply(rows, `[[`, character(1), "path"),
    kind = vapply(rows, `[[`, character(1), "kind"),
    message = vapply(rows, `[[`, character(1), "message")
  ))
}


#' Read and parse a specification file without stopping
#'
#' It decodes the file the way `tteplan_read_spec()` does: raw bytes, a UTF-8
#' byte order mark removed, and UTF-8 checked independently of the session
#' locale.
#'
#' @param spec_path Character scalar, the path of the specification file.
#' @return A list. `spec` holds the parsed YAML. `problem` holds `NULL`, or a
#'   message that says why the file cannot be read.
#' @noRd
.tte_check_spec_parse <- function(spec_path) {
  fail <- function(msg) {
    return(list(spec = NULL, problem = msg))
  }
  if (!file.exists(spec_path)) {
    return(fail(paste0("Spec file not found: ", spec_path)))
  }
  if (dir.exists(spec_path)) {
    return(fail(paste0("Spec path is a directory, not a file: ", spec_path)))
  }
  fsize <- file.info(spec_path)$size
  if (is.na(fsize)) {
    return(fail(paste0(
      "Cannot determine the size of the spec file: ",
      spec_path
    )))
  }
  spec_txt <- tryCatch(
    {
      spec_bytes <- readBin(spec_path, "raw", n = fsize)
      bom <- as.raw(c(0xEF, 0xBB, 0xBF))
      if (length(spec_bytes) >= 3L && identical(spec_bytes[1:3], bom)) {
        spec_bytes <- spec_bytes[-(1:3)]
      }
      rawToChar(spec_bytes)
    },
    error = function(e) e
  )
  if (inherits(spec_txt, "error")) {
    return(fail(paste0(
      "Cannot read the spec file ",
      spec_path,
      ": ",
      conditionMessage(spec_txt)
    )))
  }
  if (!validUTF8(spec_txt)) {
    return(fail(paste0(
      "Spec file is not valid UTF-8 (re-save it as UTF-8): ",
      spec_path
    )))
  }
  Encoding(spec_txt) <- "UTF-8"
  spec <- tryCatch(yaml::yaml.load(spec_txt), error = function(e) e)
  if (inherits(spec, "error")) {
    return(fail(paste0(
      "Spec file is not valid YAML: ",
      spec_path,
      ": ",
      conditionMessage(spec)
    )))
  }
  return(list(spec = spec, problem = NULL))
}


#' Take a nested element only through lists
#'
#' `x$a$b` stops when `x$a` is an atomic vector. A bad specification can hold
#' a scalar where a mapping belongs, and the checker MUST NOT stop on it.
#'
#' @param x A parsed YAML value.
#' @param keys Character vector, the names to follow in order.
#' @return The element, or `NULL` when a step is not a list or is absent.
#' @noRd
.tte_check_spec_get <- function(x, keys) {
  for (k in keys) {
    if (!is.list(x)) {
      return(NULL)
    }
    x <- x[[k]]
  }
  return(x)
}


#' One character scalar from a parsed YAML value
#'
#' @param x A parsed YAML value.
#' @return A character scalar, or `NA_character_` when `x` is not one atomic
#'   value.
#' @noRd
.tte_check_spec_scalar <- function(x) {
  if (!is.atomic(x) || length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  return(as.character(x))
}


#' Rows for every undeclared and every retired key path
#'
#' @param spec The parsed specification.
#' @return A list of rows, in the order of the walk.
#' @noRd
.tte_check_spec_keys <- function(spec) {
  paths <- unique(.tte_spec_walk_keys(spec))
  cls <- .tte_spec_key_class(paths)
  bad <- is.na(cls) | cls == "legacy"
  rows <- list()
  for (i in which(bad)) {
    path <- paths[i]
    # `.tte_spec_key_finding()` writes the path on its own line above the
    # detail. The row holds the path in its own column, so drop that line.
    finding <- .tte_spec_key_finding(path)
    prefix <- paste0("  ", path, "\n    ")
    if (startsWith(finding, prefix)) {
      finding <- substring(finding, nchar(prefix) + 1L)
    }
    kind <- if (is.na(cls[i])) "undeclared_key" else "retired_key"
    rows <- c(rows, list(.tte_check_spec_row(path, kind, finding)))
  }
  return(rows)
}


#' Whether a parsed YAML value is a valid enrollment id
#'
#' A valid id is one character string or one number. It is not `NA`, and a
#' string is not empty or whitespace only.
#'
#' @param x A parsed YAML value.
#' @return `TRUE` or `FALSE`.
#' @noRd
.tte_enrollment_id_ok <- function(x) {
  if (!(is.character(x) || is.numeric(x)) || length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  if (is.character(x) && !nzchar(trimws(x, whitespace = "[\\h\\v]"))) {
    return(FALSE)
  }
  return(TRUE)
}


#' Rows for every enrollment whose id is missing or is not one value
#'
#' `tteplan_read_spec()` stops with `.tte_enrollment_id_message()` of these
#' rows, before it reads any other key of the enrollments.
#'
#' @param spec The parsed specification.
#' @return A list of rows of kind `invalid_enrollment_id`, one for each such
#'   enrollment.
#' @noRd
.tte_check_spec_enrollment_id_values <- function(spec) {
  enrollments <- .tte_check_spec_get(spec, "enrollments")
  if (!is.list(enrollments)) {
    return(list())
  }
  rows <- list()
  for (i in seq_along(enrollments)) {
    value <- .tte_check_spec_get(enrollments[[i]], "id")
    if (.tte_enrollment_id_ok(value)) {
      next
    }
    what <- if (is.null(value)) {
      " is missing 'id'. "
    } else {
      paste0(" has id ", .tte_spec_value_show(value), ". ")
    }
    rows <- c(
      rows,
      list(.tte_check_spec_row(
        "$/enrollments[]/id",
        "invalid_enrollment_id",
        paste0(
          "enrollments[",
          i,
          "]",
          what,
          "Each enrollment MUST carry one id: one non-empty character ",
          "string or one number."
        )
      ))
    )
  }
  return(rows)
}


#' Rows for every enrollment id that more than one enrollment carries
#'
#' An id that `.tte_enrollment_id_ok()` refuses takes no part in the
#' comparison. `.tte_check_spec_enrollment_id_values()` reports it.
#'
#' @param spec The parsed specification.
#' @return A list of rows, one for each repeated id.
#' @noRd
.tte_check_spec_enrollment_ids <- function(spec) {
  enrollments <- .tte_check_spec_get(spec, "enrollments")
  if (!is.list(enrollments) || !is.null(names(enrollments))) {
    return(list())
  }
  ids <- vapply(
    enrollments,
    function(e) {
      value <- .tte_check_spec_get(e, "id")
      if (!.tte_enrollment_id_ok(value)) {
        return(NA_character_)
      }
      return(as.character(value))
    },
    character(1)
  )
  repeated <- unique(ids[!is.na(ids) & duplicated(ids)])
  rows <- list()
  for (id in repeated) {
    at <- which(ids == id)
    rows <- c(
      rows,
      list(.tte_check_spec_row(
        "$/enrollments[]/id",
        "duplicate_enrollment_id",
        paste0(
          "Enrollment id '",
          id,
          "' is used by ",
          length(at),
          " enrollments: ",
          paste0("enrollments[", at, "]", collapse = ", "),
          ". Each enrollment MUST carry its own id."
        )
      ))
    )
  }
  return(rows)
}


#' The error message of the bad or repeated enrollment ids
#'
#' `tteplan_read_spec()` stops with this message. `tteplan_check_spec()`
#' compares its `read_error` against it, so one finding never gives two rows.
#'
#' @param rows A list of rows from `.tte_check_spec_enrollment_id_values()`
#'   or from `.tte_check_spec_enrollment_ids()`.
#' @return A character scalar.
#' @noRd
.tte_enrollment_id_message <- function(rows) {
  return(paste(
    vapply(rows, `[[`, character(1), "message"),
    collapse = "\n"
  ))
}


#' A row when the version in the file differs from the file name
#'
#' @param spec The parsed specification.
#' @param spec_path Character scalar, the path of the specification file.
#' @return A list of zero rows or one row.
#' @noRd
.tte_check_spec_version <- function(spec, spec_path) {
  pattern <- "^spec_(v[0-9]+)\\.yaml$"
  file_name <- basename(spec_path)
  if (!grepl(pattern, file_name)) {
    return(list())
  }
  file_version <- sub(pattern, "\\1", file_name)
  version <- .tte_check_spec_scalar(
    .tte_check_spec_get(spec, c("study", "implementation", "version"))
  )
  if (identical(version, file_version)) {
    return(list())
  }
  stated <- if (is.na(version)) {
    "is missing"
  } else {
    paste0("is '", version, "'")
  }
  return(list(.tte_check_spec_row(
    "$/study/implementation/version",
    "version_mismatch",
    paste0(
      "study$implementation$version ",
      stated,
      ", and the file name ",
      file_name,
      " gives '",
      file_version,
      "'."
    )
  )))
}


#' Every exclusion criterion that selects on a follow-up outcome
#'
#' The window `lifetime_before_and_after_baseline` reads every week of the
#' person, after time zero included. When the criterion reads an outcome
#' variable of the spec, it removes each person who has the outcome during
#' follow-up. The analysis then selects on its own outcome. This finder reads
#' the global `exclusion_criteria` and each enrollment's
#' `additional_exclusion`. The spec declares outcomes at the top level only.
#'
#' The finder reads the parsed specification before or after
#' `tteplan_read_spec()` normalises it, and never stops on a bad shape.
#'
#' @param spec The parsed specification.
#' @return A list with one element per offending criterion and outcome. Each
#'   element holds `path`, the normalised key path of the window, and
#'   `message`, which names the criterion, the variable and the outcome.
#' @noRd
.tte_outcome_selecting_exclusions <- function(spec) {
  out <- .tte_spec_outcome_vars(spec)
  if (is.null(out)) {
    return(list())
  }
  found <- list()
  exclusions <- .tte_check_spec_get(spec, "exclusion_criteria")
  if (.tte_spec_is_sequence(exclusions)) {
    for (i in seq_along(exclusions)) {
      found <- c(
        found,
        .tte_outcome_selecting_one(
          exclusions[[i]],
          paste0(
            "exclusion_criteria[",
            i,
            "] '",
            .tte_spec_scalar_at(exclusions[[i]], "name"),
            "'"
          ),
          "$/exclusion_criteria[]/implementation/window",
          out
        )
      )
    }
  }
  enrollments <- .tte_check_spec_get(spec, "enrollments")
  if (!.tte_spec_is_sequence(enrollments)) {
    return(found)
  }
  for (i in seq_along(enrollments)) {
    extra <- .tte_check_spec_get(enrollments[[i]], "additional_exclusion")
    if (!.tte_spec_is_sequence(extra)) {
      next
    }
    for (j in seq_along(extra)) {
      found <- c(
        found,
        .tte_outcome_selecting_one(
          extra[[j]],
          paste0(
            "enrollments[",
            i,
            "] '",
            .tte_spec_scalar_at(enrollments[[i]], "id"),
            "' additional_exclusion[",
            j,
            "] '",
            .tte_spec_scalar_at(extra[[j]], "name"),
            "'"
          ),
          "$/enrollments[]/additional_exclusion[]/implementation/window",
          out
        )
      )
    }
  }
  return(found)
}


#' The variables and the name of each outcome of a parsed specification
#'
#' @param spec The parsed specification.
#' @return `NULL` when `outcomes` is not a sequence. Otherwise a list with
#'   `vars`, the variables of each outcome, and `names`, the name of each
#'   outcome, or its variables when it has no name.
#' @noRd
.tte_spec_outcome_vars <- function(spec) {
  outcomes <- .tte_check_spec_get(spec, "outcomes")
  if (!.tte_spec_is_sequence(outcomes)) {
    return(NULL)
  }
  vars <- lapply(outcomes, function(o) {
    return(.tte_spec_vars(.tte_check_spec_get(
      o,
      c("implementation", "variable")
    )))
  })
  nm <- vapply(outcomes, .tte_spec_scalar_at, character(1), "name")
  nm[is.na(nm)] <- vapply(vars[is.na(nm)], paste, character(1), collapse = ", ")
  return(list(vars = vars, names = nm))
}


#' One scalar of a parsed YAML entry, as text
#' @noRd
.tte_spec_scalar_at <- function(entry, key) {
  return(.tte_check_spec_scalar(.tte_check_spec_get(entry, key)))
}


#' The findings of one exclusion criterion
#'
#' One finding per outcome that shares a variable with a criterion of the
#' window `lifetime_before_and_after_baseline`.
#'
#' @param ec One exclusion criterion of the parsed specification.
#' @param label The criterion's label, for the message.
#' @param path The normalised key path of the window.
#' @param out A list: `vars`, the variables of each outcome, and `names`, the
#'   name of each outcome.
#' @return A list of findings, each `list(path = , message = )`.
#' @noRd
.tte_outcome_selecting_one <- function(ec, label, path, out) {
  window <- .tte_check_spec_scalar(
    .tte_check_spec_get(ec, c("implementation", "window"))
  )
  if (!identical(window, "lifetime_before_and_after_baseline")) {
    return(list())
  }
  sv <- .tte_spec_vars(
    .tte_check_spec_get(ec, c("implementation", "source_variable"))
  )
  rows <- list()
  for (k in seq_along(out$vars)) {
    shared <- intersect(sv, out$vars[[k]])
    if (length(shared) == 0L) {
      next
    }
    rows <- c(
      rows,
      list(list(
        path = path,
        message = paste0(
          label,
          " has the window lifetime_before_and_after_baseline on '",
          paste(shared, collapse = "', '"),
          "', which is the variable of outcome '",
          out$names[k],
          "'. That window reads the weeks after time zero, so the criterion ",
          "excludes each person who has the outcome during follow-up. Use ",
          "lifetime_before_baseline, or a window that ends before time zero."
        )
      ))
    )
  }
  return(rows)
}


#' `TRUE` when a parsed YAML value is a sequence, an unnamed list
#' @noRd
.tte_spec_is_sequence <- function(x) {
  return(is.list(x) && is.null(names(x)))
}


#' The variable names of one `variable` or `source_variable` value
#'
#' @param x A parsed YAML value: one name, or a sequence of names.
#' @return A character vector of the distinct non-missing names. It is empty
#'   when `x` holds no atomic value.
#' @noRd
.tte_spec_vars <- function(x) {
  if (!is.atomic(x) && !is.list(x)) {
    return(character(0))
  }
  v <- unlist(x, use.names = FALSE)
  if (!is.atomic(v)) {
    return(character(0))
  }
  return(unique(as.character(v[!is.na(v)])))
}


#' Rows for every exclusion criterion that selects on a follow-up outcome
#'
#' @param spec The parsed specification.
#' @return A list of rows of kind `outcome_selecting_exclusion`.
#' @noRd
.tte_check_spec_outcome_exclusions <- function(spec) {
  return(lapply(.tte_outcome_selecting_exclusions(spec), function(f) {
    return(.tte_check_spec_row(
      f$path,
      "outcome_selecting_exclusion",
      f$message
    ))
  }))
}


#' The error message of the outcome-selecting exclusions
#'
#' `tteplan_read_spec()` stops with this message. `tteplan_check_spec()`
#' compares its `read_error` against it, so one finding never gives two rows.
#'
#' @param found A list from `.tte_outcome_selecting_exclusions()`.
#' @return A character scalar.
#' @noRd
.tte_outcome_selecting_message <- function(found) {
  return(paste(
    vapply(found, `[[`, character(1), "message"),
    collapse = "\n"
  ))
}


#' Every problem with the `family` key of the enrollments
#'
#' `family` is a display label that groups enrollments. It MUST be one
#' non-empty character string. When one enrollment sets it, every enrollment
#' MUST set it. A key that is present with a null value counts as set, and
#' its value is bad.
#'
#' The finder reads the parsed specification before or after
#' `tteplan_read_spec()` normalises it, and never stops on a bad shape.
#'
#' @param spec The parsed specification.
#' @return A list with one element per finding. Each element holds `path`,
#'   the normalised key path, and `message`.
#' @noRd
.tte_enrollment_family_problems <- function(spec) {
  enrollments <- .tte_check_spec_get(spec, "enrollments")
  if (!.tte_spec_is_sequence(enrollments) || length(enrollments) == 0L) {
    return(list())
  }
  label <- vapply(
    seq_along(enrollments),
    function(i) {
      return(paste0(
        "enrollments[",
        i,
        "] '",
        .tte_spec_scalar_at(enrollments[[i]], "id"),
        "'"
      ))
    },
    character(1)
  )
  has_family <- vapply(
    enrollments,
    function(e) {
      return(is.list(e) && "family" %in% names(e))
    },
    logical(1)
  )
  found <- list()
  for (i in which(has_family)) {
    value <- enrollments[[i]][["family"]]
    ok <- is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(trimws(value))
    if (!ok) {
      found <- c(
        found,
        list(list(
          path = "$/enrollments[]/family",
          message = paste0(
            label[i],
            " has family ",
            .tte_spec_value_show(value),
            ". family MUST be one non-empty character string. Quote a ",
            "value that YAML reads as a number or a logical."
          )
        ))
      )
    }
  }
  if (any(has_family) && !all(has_family)) {
    found <- c(
      found,
      list(list(
        path = "$/enrollments[]/family",
        message = paste0(
          paste(label[!has_family], collapse = ", "),
          " set no family, and ",
          sum(has_family),
          " of ",
          length(has_family),
          " enrollments set one. When one enrollment sets family, every ",
          "enrollment MUST set it."
        )
      ))
    )
  }
  return(found)
}


#' Show a bad spec value, such as a `family` or an enrollment `id`, in a message
#'
#' @param value A parsed YAML value.
#' @return A character scalar.
#' @noRd
.tte_spec_value_show <- function(value) {
  if (is.null(value)) {
    return("null")
  }
  if (is.atomic(value) && length(value) == 1L && !is.na(value)) {
    if (is.character(value)) {
      return(paste0("'", value, "'"))
    }
    return(paste0(as.character(value), " (", class(value)[1], ")"))
  }
  return(paste0("a ", class(value)[1], " of length ", length(value)))
}


#' The error message of the enrollment family findings
#'
#' `tteplan_read_spec()` stops with this message. `tteplan_check_spec()`
#' compares its `read_error` against it, so one finding never gives two rows.
#'
#' @param found A list from `.tte_enrollment_family_problems()`.
#' @return A character scalar.
#' @noRd
.tte_enrollment_family_message <- function(found) {
  return(paste(
    vapply(found, `[[`, character(1), "message"),
    collapse = "\n"
  ))
}


#' Rows for every problem with the `family` key of the enrollments
#'
#' @param found A list from `.tte_enrollment_family_problems()`.
#' @return A list of rows of kind `enrollment_family`.
#' @noRd
.tte_check_spec_family <- function(found) {
  return(lapply(found, function(f) {
    return(.tte_check_spec_row(f$path, "enrollment_family", f$message))
  }))
}


#' Every bad value of the `minimum_events_rule_exempt` key of the enrollments
#'
#' The key is optional. When an enrollment sets it, the value MUST be one
#' logical, `true` or `false`. A number, a string, `NA`, a null value and a
#' value of length other than 1 are bad. The finder reads the parsed
#' specification before or after `tteplan_read_spec()` normalises it, and
#' never stops on a bad shape.
#'
#' @param spec The parsed specification.
#' @return A list with one element per bad value. Each element holds `path`,
#'   the normalised key path, and `message`, which names the enrollment id
#'   and the key.
#' @noRd
.tte_enrollment_exempt_problems <- function(spec) {
  enrollments <- .tte_check_spec_get(spec, "enrollments")
  if (!.tte_spec_is_sequence(enrollments) || length(enrollments) == 0L) {
    return(list())
  }
  key <- "minimum_events_rule_exempt"
  found <- list()
  for (i in seq_along(enrollments)) {
    e <- enrollments[[i]]
    if (!is.list(e) || !key %in% names(e)) {
      next
    }
    value <- e[[key]]
    if (is.logical(value) && length(value) == 1L && !is.na(value)) {
      next
    }
    shown <- if (is.atomic(value) && length(value) == 1L && is.na(value)) {
      "NA"
    } else {
      .tte_spec_value_show(value)
    }
    found <- c(
      found,
      list(list(
        path = paste0("$/enrollments[]/", key),
        message = paste0(
          "enrollments[",
          i,
          "] '",
          .tte_spec_scalar_at(e, "id"),
          "' has ",
          key,
          " ",
          shown,
          ". ",
          key,
          " MUST be one logical: true or false."
        )
      ))
    )
  }
  return(found)
}


#' The error message of the `minimum_events_rule_exempt` findings
#'
#' `tteplan_read_spec()` stops with this message. `tteplan_check_spec()`
#' compares its `read_error` against it, so one finding never gives two rows.
#'
#' @param found A list from `.tte_enrollment_exempt_problems()`.
#' @return A character scalar.
#' @noRd
.tte_enrollment_exempt_message <- function(found) {
  return(paste(
    vapply(found, `[[`, character(1), "message"),
    collapse = "\n"
  ))
}


#' Rows for every bad value of the `minimum_events_rule_exempt` key
#'
#' @param found A list from `.tte_enrollment_exempt_problems()`.
#' @return A list of rows of kind `enrollment_rule_exempt`.
#' @noRd
.tte_check_spec_exempt <- function(found) {
  return(lapply(found, function(f) {
    return(.tte_check_spec_row(f$path, "enrollment_rule_exempt", f$message))
  }))
}


#' Every bad value of the top-level key `feasibility_follow_up_weeks`
#'
#' The key is optional. When the spec sets it, the value MUST be one positive
#' whole number of weeks, as every other duration of the spec is. A logical,
#' a string, `NA` and a null value are bad. So are 0, a negative number, a
#' fraction, an infinite number and a value of length other than 1. The
#' finder never stops on a bad shape.
#'
#' @param spec The parsed specification.
#' @return A list with zero elements or one. The element holds `path`, the
#'   normalised key path, and `message`, which names the key.
#' @noRd
.tte_feasibility_follow_up_problems <- function(spec) {
  key <- "feasibility_follow_up_weeks"
  if (!is.list(spec) || !key %in% names(spec)) {
    return(list())
  }
  value <- spec[[key]]
  if (
    is.numeric(value) &&
      length(value) == 1L &&
      is.finite(value) &&
      value > 0 &&
      value == round(value)
  ) {
    return(list())
  }
  shown <- if (is.atomic(value) && length(value) == 1L && is.na(value)) {
    "NA"
  } else {
    .tte_spec_value_show(value)
  }
  return(list(list(
    path = paste0("$/", key),
    message = paste0(
      key,
      " has the value ",
      shown,
      ". ",
      key,
      " MUST be one positive whole number of weeks, for example ",
      key,
      ": 260."
    )
  )))
}


#' The error message of the `feasibility_follow_up_weeks` finding
#'
#' `tteplan_read_spec()` stops with this message. `tteplan_check_spec()`
#' compares its `read_error` against it, so one finding never gives two rows.
#'
#' @param found A list from `.tte_feasibility_follow_up_problems()`.
#' @return A character scalar.
#' @noRd
.tte_feasibility_follow_up_message <- function(found) {
  return(paste(
    vapply(found, `[[`, character(1), "message"),
    collapse = "\n"
  ))
}


#' Rows for a bad value of the `feasibility_follow_up_weeks` key
#'
#' @param found A list from `.tte_feasibility_follow_up_problems()`.
#' @return A list of rows of kind `feasibility_follow_up_weeks`.
#' @noRd
.tte_check_spec_feasibility_follow_up <- function(found) {
  return(lapply(found, function(f) {
    return(.tte_check_spec_row(
      f$path,
      "feasibility_follow_up_weeks",
      f$message
    ))
  }))
}
