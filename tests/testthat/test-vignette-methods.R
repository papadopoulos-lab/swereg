# Every file in `vignettes/` is a page a reader copies from. A call there can
# name a method no swereg R6 class has. It can pass an argument the method
# does not declare. The vignette build reports neither defect: knitr does not
# run a plain ```r fence, and it does not run an `eval = FALSE` chunk. These
# tests run the check the build skips.
#
# Two checks, over different sets. Every REFERENCE is checked for its name.
# Only a CALL is checked for its arguments. A reference is a call when it goes
# through a receiver (`enrollment$`), or when it passes an argument inside an
# R fence. A bare `$method()` with empty parentheses lists a method name, and a
# reference inside a plain fence is quoted output, not code.

.VMETH_GENERATORS <- c(
  "CandidatePath",
  "RegistryStudy",
  "Skeleton",
  "TTEDesign",
  "TTEEnrollment",
  "TTEPlan"
)

.vmeth_dir <- function() {
  p <- testthat::test_path("..", "..", "vignettes")
  if (!dir.exists(p)) {
    return(NULL)
  }
  return(normalizePath(p))
}

.vmeth_files <- function() {
  d <- .vmeth_dir()
  if (is.null(d)) {
    return(character(0))
  }
  return(sort(list.files(d, pattern = "[.]Rmd$", full.names = TRUE)))
}

.vmeth_public_methods <- function(generator) {
  return(names(get(generator, envir = asNamespace("swereg"))$public_methods))
}

# Every public method name the six generators declare, as one set.
.vmeth_known <- function() {
  return(sort(unique(unlist(lapply(
    .VMETH_GENERATORS,
    .vmeth_public_methods
  )))))
}

# The generators that declare `method`. Six names are declared by more than one
# generator (initialize, print, check_version, clone, save, pipeline_hash), so
# a bare reference can have several owners.
.vmeth_owners <- function(method) {
  keep <- vapply(
    .VMETH_GENERATORS,
    function(g) method %in% .vmeth_public_methods(g),
    logical(1)
  )
  return(.VMETH_GENERATORS[keep])
}

# TRUE for each line inside a fence whose language is R. The fence marker line
# itself is FALSE.
.vmeth_in_r_fence <- function(lines) {
  out <- logical(length(lines))
  open <- FALSE
  is_r <- FALSE
  for (i in seq_along(lines)) {
    if (grepl("^[ \t]*```", lines[i])) {
      if (open) {
        open <- FALSE
        is_r <- FALSE
      } else {
        open <- TRUE
        is_r <- grepl("^(r$|r[ ,}]|\\{r)", sub("^[ \t]*`+", "", lines[i]))
      }
      next
    }
    out[i] <- open && is_r
  }
  return(out)
}

# Every `enrollment$method(` reference and every line-initial `$method(`
# reference, with the argument text it passes. It walks the parentheses from
# the opening one, so a reference that spans several lines is read whole. A
# `plan$` or `study$` receiver is not matched: `$set(`, `$new(` and the maths
# token `$q(` in tte-methods.Rmd would come with it.
.vmeth_calls <- function(txt) {
  pat <- "(?m)(?:^[ \t]*|enrollment)\\$([A-Za-z0-9_.]+)[ \t]*\\("
  starts <- gregexpr(pat, txt, perl = TRUE)[[1]]
  if (starts[1] == -1L) {
    return(list())
  }
  lens <- attr(starts, "match.length")
  n <- nchar(txt)
  nl <- gregexpr("\n", txt, fixed = TRUE)[[1]]
  if (nl[1] == -1L) {
    nl <- integer(0)
  }
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
    out[[length(out) + 1L]] <- list(
      method = method,
      args = substring(txt, open + 1L, i - 1L),
      receiver = grepl("enrollment\\$", head_txt),
      line = sum(nl < starts[k]) + 1L
    )
  }
  return(out)
}

# Every reference in every vignette, each tagged with its file, its owners and
# whether it is a call.
.vmeth_references <- function() {
  files <- .vmeth_files()
  if (length(files) == 0L) {
    return(NULL)
  }
  out <- list()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    in_r <- .vmeth_in_r_fence(lines)
    for (cl in .vmeth_calls(paste(lines, collapse = "\n"))) {
      cl$file <- basename(f)
      cl$owners <- if (cl$receiver) "TTEEnrollment" else .vmeth_owners(cl$method)
      cl$is_call <- cl$receiver ||
        (isTRUE(in_r[cl$line]) && nzchar(trimws(cl$args)))
      out[[length(out) + 1L]] <- cl
    }
  }
  return(out)
}

# TRUE when `args_text` is a valid argument list for `method` on at least one
# owning class. Otherwise a reason string, naming every owner it failed on.
# R's own parser and `match.call()` decide, so a named argument nested inside
# another call is never read as an argument of the outer one.
.vmeth_check_call <- function(method, args_text, owners = NULL) {
  if (is.null(owners)) {
    owners <- .vmeth_owners(method)
  }
  if (length(owners) == 0L) {
    return(sprintf("$%s(): no swereg R6 class declares this method", method))
  }
  parsed <- tryCatch(
    parse(text = paste0("f(", args_text, ")"))[[1L]],
    error = function(e) e
  )
  if (inherits(parsed, "condition")) {
    return(sprintf(
      "$%s(): arguments do not parse: %s",
      method,
      conditionMessage(parsed)
    ))
  }
  # A vignette writes `...` where a value goes. Substitute a value, so the
  # argument NAMES still reach match.call().
  if (length(parsed) > 1L) {
    for (j in seq.int(2L, length(parsed))) {
      if (identical(parsed[[j]], as.name("..."))) {
        parsed[[j]] <- as.name("value")
      }
    }
  }
  reasons <- character(0)
  for (g in owners) {
    fn <- get(g, envir = asNamespace("swereg"))$public_methods[[method]]
    matched <- tryCatch(
      match.call(definition = fn, call = parsed),
      error = function(e) e
    )
    if (inherits(matched, "condition")) {
      reasons <- c(reasons, sprintf("%s %s", g, conditionMessage(matched)))
      next
    }
    fo <- formals(fn)
    required <- names(fo)[vapply(
      fo,
      function(z) is.name(z) && !nzchar(as.character(z)),
      logical(1)
    )]
    gap <- setdiff(setdiff(required, "..."), names(matched)[-1L])
    if (length(gap) > 0L) {
      reasons <- c(
        reasons,
        sprintf("%s missing required argument (%s)", g, toString(gap))
      )
      next
    }
    return(TRUE)
  }
  return(sprintf(
    "$%s(%s): %s",
    method,
    gsub("[\n\t ]+", " ", trimws(args_text)),
    paste(reasons, collapse = "; ")
  ))
}


test_that("every vignette names only R6 methods that exist", {
  refs <- .vmeth_references()
  skip_if(is.null(refs), "source tree not available")
  # The scan must find the references it is meant to check. A scan that
  # matched nothing would pass every assertion below and prove nothing.
  expect_gte(length(refs), 20L)

  named <- unique(vapply(refs, `[[`, character(1), "method"))
  unknown <- setdiff(named, .vmeth_known())
  expect_identical(unknown, character(0))
})


test_that("every vignette call passes arguments the method declares", {
  refs <- .vmeth_references()
  skip_if(is.null(refs), "source tree not available")
  expect_gte(length(refs), 20L)

  known <- .vmeth_known()
  bad <- character(0)
  checked <- 0L
  for (cl in refs) {
    if (!cl$is_call || !cl$method %in% known) {
      next
    }
    checked <- checked + 1L
    verdict <- .vmeth_check_call(cl$method, cl$args, cl$owners)
    if (!isTRUE(verdict)) {
      bad <- c(bad, sprintf("%s:%d %s", cl$file, cl$line, verdict))
    }
  }
  # Count calls, not references. A rule that classified every reference as a
  # listing would leave nothing to check.
  expect_gte(checked, 15L)
  expect_identical(bad, character(0))
})


test_that("every estimation call in a vignette passes weight_col", {
  refs <- .vmeth_references()
  skip_if(is.null(refs), "source tree not available")

  est <- c("rates", "irr", "survival_curve", "heterogeneity_test")
  shown <- vapply(refs, `[[`, character(1), "method")
  # Each of the four is shown at least once, so none of the checks below is
  # vacuous.
  expect_identical(sort(intersect(est, shown)), sort(est))

  missing_weight <- character(0)
  for (cl in refs) {
    if (!cl$is_call || !cl$method %in% est) {
      next
    }
    if (!"weight_col" %in% names(as.list(parse(
      text = paste0("f(", cl$args, ")")
    )[[1L]]))) {
      missing_weight <- c(
        missing_weight,
        sprintf("%s:%d $%s", cl$file, cl$line, cl$method)
      )
    }
  }
  expect_identical(missing_weight, character(0))
})


test_that(".vmeth_check_call() accepts a valid call and names each defect", {
  # A named argument inside a nested call belongs to that call. `df` is an
  # argument of `splines::ns()`, never of `$irr()`.
  expect_true(.vmeth_check_call("irr", "weight_col = f(df = 3)"))
  expect_true(.vmeth_check_call("rates", '"w"'))

  extra <- .vmeth_check_call("rates", '"w", "extra"')
  expect_type(extra, "character")
  expect_match(extra, "unused argument")

  gap <- .vmeth_check_call("rates", "")
  expect_type(gap, "character")
  expect_match(gap, "missing required argument \\(weight_col\\)")

  # `initialize` is declared by all six generators. `candidates` is
  # CandidatePath's and `batch_number` is Skeleton's, so no one generator
  # takes both.
  cross <- .vmeth_check_call("initialize", "candidates = 1, batch_number = 2")
  expect_type(cross, "character")
  expect_match(cross, "CandidatePath")
  expect_match(cross, "Skeleton")
})
