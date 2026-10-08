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
#'     \item{`kind`}{The problem kind. The Details section lists the six
#'       kinds.}
#'     \item{`message`}{What is wrong, and the repair where swereg knows it.}
#'   }
#'   The table has zero rows when the checker finds no problem.
#'
#' @details
#' The checker reports six kinds of problem:
#' \describe{
#'   \item{`unreadable`}{The file is missing, is not valid UTF-8, or is not
#'     valid YAML. This row is the only row, because the checker has no
#'     parsed specification to check.}
#'   \item{`undeclared_key`}{The schema does not declare the key path. The
#'     message lists the keys that the context accepts.}
#'   \item{`retired_key`}{The schema refuses the key path. The message names
#'     the replacement.}
#'   \item{`duplicate_enrollment_id`}{Two or more enrollments carry the same
#'     `id`. The checker writes one row for each repeated id.}
#'   \item{`version_mismatch`}{The file name has the form `spec_vNNN.yaml`,
#'     and `study$implementation$version` is missing or is not `vNNN`. The
#'     checker skips this check for every other file name.}
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
  if (
    !is.character(spec_path) || length(spec_path) != 1L || is.na(spec_path)
  ) {
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
  rows <- c(rows, .tte_check_spec_enrollment_ids(spec))
  rows <- c(rows, .tte_check_spec_version(spec, spec_path))

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


#' Rows for every enrollment id that more than one enrollment carries
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
    function(e) .tte_check_spec_scalar(.tte_check_spec_get(e, "id")),
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
