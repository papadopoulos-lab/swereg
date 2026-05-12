#' Expand bracketed code patterns to literal prefixes (internal)
#'
#' Expands range and character-class brackets in code patterns to a vector
#' of explicit literal prefixes. Called from inside every \code{add_*()}
#' function via \code{.swereg_codes_pre()} so that bracket / character-class
#' / range patterns (e.g. \code{"I2[0-5]"}, \code{"!302[A-Z]"}) are accepted
#' directly at the call site. The matchers themselves use
#' \code{startsWith()} on literal prefixes.
#'
#' Supports character ranges (\code{[A-Z]}, \code{[0-9]}), enumerations
#' (\code{[ABC]}), mixed forms (\code{[2-57]}), multiple bracket groups
#' (Cartesian product), and preserves any leading \code{"!"} exclusion
#' prefix on every expanded literal. Patterns with no brackets pass
#' through unchanged.
#'
#' @param codes Character vector of code patterns.
#' @return A character vector of literal prefixes.
#' @keywords internal
#' @noRd
expand_codes <- function(codes) {
  if (length(codes) == 0L) return(character())
  unlist(lapply(codes, .expand_one_code), use.names = FALSE)
}

# Apply expand_codes() to every entry of a named code list. Internal helper;
# see expand_codes().
expand_code_list <- function(lst) {
  lapply(lst, expand_codes)
}

# Internal: expand a single code pattern, preserving leading "!"
.expand_one_code <- function(code) {
  excl <- ""
  body <- code
  if (startsWith(body, "!")) {
    excl <- "!"
    body <- substr(body, 2L, nchar(body))
  }
  out <- .expand_brackets(body)
  if (nzchar(excl)) out <- paste0(excl, out)
  out
}

# Internal: recursively expand the leftmost bracket group in `s`
.expand_brackets <- function(s) {
  m <- regexpr("\\[[^]]+\\]", s)
  if (m[1] == -1L) return(s)
  start <- m[1]
  end   <- start + attr(m, "match.length") - 1L
  prefix  <- substr(s, 1L, start - 1L)
  bracket <- substr(s, start + 1L, end - 1L)
  suffix  <- substr(s, end + 1L, nchar(s))
  chars <- .expand_char_class(bracket)
  unlist(lapply(chars, function(ch) .expand_brackets(paste0(prefix, ch, suffix))))
}

# Internal: expand the contents of a single bracket group
.expand_char_class <- function(cc) {
  out <- character()
  i <- 1L
  n <- nchar(cc)
  while (i <= n) {
    ch <- substr(cc, i, i)
    if (i + 2L <= n && substr(cc, i + 1L, i + 1L) == "-") {
      from <- utf8ToInt(ch)
      to   <- utf8ToInt(substr(cc, i + 2L, i + 2L))
      out <- c(out, vapply(from:to, intToUtf8, character(1)))
      i <- i + 3L
    } else {
      out <- c(out, ch)
      i <- i + 1L
    }
  }
  out
}
