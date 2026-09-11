# `vignettes/r6-class-overview.Rmd` is the first page a reader copies from. A
# call there that names a method the class does not have, or an argument the
# method does not declare, is a defect the vignette build never reports: knitr
# does not run a plain ```r fence. These tests run the check the build skips.

.vmeth_vignette <- function() {
  p <- testthat::test_path("..", "..", "vignettes", "r6-class-overview.Rmd")
  if (!file.exists(p)) {
    return(NULL)
  }
  return(normalizePath(p))
}

# Every `enrollment$method(` call and every line-initial `$method(` call, with
# the argument names it passes. It walks the parentheses from the opening one,
# so a call that spans several lines is read whole. A `plan$` or `study$`
# receiver is not matched: those are other classes.
.vmeth_calls <- function(txt) {
  pat <- "(?m)(?:^[ \t]*|enrollment)\\$([A-Za-z0-9_.]+)[ \t]*\\("
  starts <- gregexpr(pat, txt, perl = TRUE)[[1]]
  if (starts[1] == -1L) {
    return(list())
  }
  lens <- attr(starts, "match.length")
  n <- nchar(txt)
  out <- list()
  for (k in seq_along(starts)) {
    head_txt <- substring(txt, starts[k], starts[k] + lens[k] - 1L)
    method <- sub("^.*\\$([A-Za-z0-9_.]+)[ \t]*\\($", "\\1", head_txt)
    open <- starts[k] + lens[k] - 1L
    depth <- 0L
    i <- open
    while (i <= n) {
      ch <- substring(txt, i, i)
      if (ch == "(") {
        depth <- depth + 1L
      }
      if (ch == ")") {
        depth <- depth - 1L
        if (depth == 0L) {
          break
        }
      }
      i <- i + 1L
    }
    args_txt <- substring(txt, open + 1L, i - 1L)
    arg_pat <- "[A-Za-z._][A-Za-z0-9._]*(?=[ \t]*=[^=])"
    nms <- regmatches(args_txt, gregexpr(arg_pat, args_txt, perl = TRUE))[[1]]
    out[[length(out) + 1L]] <- list(method = method, args = nms)
  }
  return(out)
}

.vmeth_read <- function() {
  p <- .vmeth_vignette()
  if (is.null(p)) {
    return(NULL)
  }
  return(.vmeth_calls(paste(readLines(p, warn = FALSE), collapse = "\n")))
}


test_that("r6-class-overview names only TTEEnrollment methods that exist", {
  calls <- .vmeth_read()
  skip_if(is.null(calls), "source tree not available")
  # The scan must find the calls it is meant to check. A scan that matches
  # nothing would pass every assertion below and prove nothing.
  expect_gte(length(calls), 20L)

  known <- names(TTEEnrollment$public_methods)
  unknown <- setdiff(unique(vapply(calls, `[[`, character(1), "method")), known)
  expect_identical(unknown, character(0))
})


test_that("r6-class-overview names only arguments those methods declare", {
  calls <- .vmeth_read()
  skip_if(is.null(calls), "source tree not available")
  expect_gte(length(calls), 20L)

  pm <- TTEEnrollment$public_methods
  bad <- character()
  for (cl in calls) {
    if (!cl$method %in% names(pm)) {
      next
    }
    formal_nms <- names(formals(pm[[cl$method]]))
    extra <- setdiff(cl$args, formal_nms)
    if (length(extra) > 0L) {
      bad <- c(bad, sprintf("$%s(%s)", cl$method, toString(extra)))
    }
  }
  expect_identical(bad, character(0))
})


test_that("every estimation call in r6-class-overview passes weight_col", {
  calls <- .vmeth_read()
  skip_if(is.null(calls), "source tree not available")

  est <- c("rates", "irr", "survival_curve", "heterogeneity_test")
  shown <- vapply(calls, `[[`, character(1), "method")
  # Each of the four is shown at least once, so none of the checks below is
  # vacuous.
  expect_identical(sort(intersect(est, shown)), sort(est))

  missing_weight <- character()
  for (cl in calls) {
    if (!cl$method %in% est) {
      next
    }
    if (!"weight_col" %in% cl$args) {
      missing_weight <- c(missing_weight, cl$method)
    }
  }
  expect_identical(missing_weight, character(0))
})
