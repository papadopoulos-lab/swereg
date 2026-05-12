#' Expand bracketed code patterns to literal prefixes
#'
#' Expands range and character-class brackets in code patterns to a vector
#' of explicit literal prefixes. This is a defensive convenience helper for
#' the \code{add_diagnoses() / add_operations() / add_rx() / add_cods()}
#' family: those functions match via \code{startsWith()} on literal
#' prefixes, so by pre-expanding bracket groups your call sites are robust
#' against any future regression in pattern-handling code, and the
#' resulting literals match identically under either \code{startsWith()} or
#' anchored regex.
#'
#' Supports:
#' \itemize{
#'   \item character ranges:        \code{[A-Z]}, \code{[a-z]}, \code{[0-9]}
#'   \item explicit enumerations:   \code{[ABC]}, \code{[01689]}
#'   \item mixed:                   \code{[2-57]}   (range 2-5 plus literal 7)
#'   \item multiple bracket groups: \code{F[1-3][0-9]}  -> Cartesian product
#'   \item \code{"!"} exclusion prefix preserved on every expanded code
#'   \item patterns with no brackets pass through unchanged
#' }
#'
#' Typical workflow:
#' \preformatted{
#' diags_list <- swereg::expand_code_list(list(
#'   diag_diabetes = c("E10[01689]", "E11[01689]")
#' ))
#' swereg::warn_unmatched_codes(diagnoses, diags_list, "add_diagnoses")
#' swereg::add_diagnoses(skeleton, diagnoses, "lopnr", diags = diags_list)
#' swereg::warn_empty_logical_cols(skeleton, diags_list, "add_diagnoses")
#' }
#'
#' @param codes Character vector of code patterns. Each element may
#'   contain zero or more bracket groups; one element with multiple
#'   bracket groups expands to the Cartesian product.
#' @return A character vector of literal prefixes. \code{"!"}-prefixed
#'   inputs keep their \code{"!"} on every expanded literal.
#' @examples
#' expand_codes("I2[0-5]")     # c("I20","I21","I22","I23","I24","I25")
#' expand_codes("H03[ABC]")    # c("H03A","H03B","H03C")
#' expand_codes("E14[01689]")  # c("E140","E141","E146","E148","E149")
#' expand_codes("E14[2-57]")   # c("E142","E143","E144","E145","E147")
#' expand_codes("FN[AB][0-9]") # 20-element vector
#' expand_codes("!302[A-Z]")   # 26-element vector with "!" preserved
#' expand_codes("302,31")      # "302,31"  (no brackets)
#' @seealso \code{\link{expand_code_list}},
#'   \code{\link{warn_unmatched_codes}},
#'   \code{\link{warn_empty_logical_cols}}
#' @export
expand_codes <- function(codes) {
  if (length(codes) == 0L) return(character())
  unlist(lapply(codes, .expand_one_code), use.names = FALSE)
}

#' Apply expand_codes() to every entry of a named code list
#'
#' Convenience wrapper that calls \code{\link{expand_codes}} on every
#' element of a named list, preserving the names. Use this on the
#' \code{codes = list(...)} argument before passing it to
#' \code{add_diagnoses() / add_operations() / add_rx() / add_cods()}.
#'
#' @param lst Named list of character vectors, each containing code
#'   patterns (with or without brackets).
#' @return A named list of the same shape with every value expanded
#'   to literal prefixes.
#' @examples
#' expand_code_list(list(
#'   diag_diabetes_uncomplicated = c("E10[01689]", "E11[01689]"),
#'   diag_acute_mi               = c("I2[0-5]")
#' ))
#' @seealso \code{\link{expand_codes}}
#' @export
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
