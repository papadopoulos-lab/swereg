# Regression tests for worker-script <-> internal-function arg parity.
#
# Background: each `inst/worker_*.R` script reads `params` from a qs2
# tempfile and calls a swereg internal function (e.g.
# `swereg:::.s2_worker(...)`). If the worker passes a kwarg that the
# function doesn't accept, R errors at the very first dispatched item
# with `unused argument (...)` -- and because workers run in subprocess
# isolation under `parallel_pool()`, the only signal at the call site
# is "Worker N failed (exit 1)". This is the bug class that produced
# the `sep_by_exp` vs `sep_by_tx` mismatch in `worker_s2.R`. This test
# parses each worker file and asserts every `<arg> = params$<field>`
# kwarg is in the corresponding function's formals.

# Map worker basename -> internal function name (must be kept in sync
# with the actual call site inside each worker script).
worker_to_fn <- list(
  "worker_s1a.R"            = ".s1a_worker",
  "worker_s1b.R"            = ".s1b_worker",
  "worker_s2.R"             = ".s2_worker",
  "worker_s3.R"             = ".s3_ett_worker",
  "worker_s3_enrollment.R"  = ".s3_enrollment_worker"
)

.parse_worker_kwargs <- function(path) {
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # Match `name = params$field` patterns. Does not handle positional
  # args or non-`params$` values, but those aren't legal in workers.
  pattern <- "([A-Za-z_][A-Za-z0-9_.]*)\\s*=\\s*params\\$([A-Za-z_][A-Za-z0-9_.]*)"
  m <- regmatches(src, gregexpr(pattern, src))[[1]]
  if (length(m) == 0L) return(list(arg = character(), field = character()))
  parsed <- regmatches(m, regexec(pattern, m))
  arg   <- vapply(parsed, `[[`, character(1), 2L)
  field <- vapply(parsed, `[[`, character(1), 3L)
  list(arg = arg, field = field)
}

test_that("every inst/worker_*.R is mapped in worker_to_fn", {
  inst_dir <- testthat::test_path("..", "..", "inst")
  skip_if_not(dir.exists(inst_dir), "inst/ not found (installed package?)")
  worker_files <- list.files(
    inst_dir, pattern = "^worker_.*\\.R$", full.names = FALSE
  )
  # worker_bootstrap.R is shared and not a dispatchable worker
  worker_files <- setdiff(worker_files, "worker_bootstrap.R")
  unmapped <- setdiff(worker_files, names(worker_to_fn))
  expect_equal(unmapped, character(0),
    info = paste("worker scripts missing from worker_to_fn map:",
                 paste(unmapped, collapse = ", ")))
})

test_that("worker scripts pass only kwargs that exist in the target function's formals", {
  inst_dir <- testthat::test_path("..", "..", "inst")
  skip_if_not(dir.exists(inst_dir), "inst/ not found (installed package?)")

  for (basename in names(worker_to_fn)) {
    path <- file.path(inst_dir, basename)
    if (!file.exists(path)) next
    fn_name <- worker_to_fn[[basename]]
    fn <- tryCatch(get(fn_name, envir = asNamespace("swereg")),
                   error = function(e) NULL)
    expect_false(is.null(fn),
      info = paste0("internal function not found in swereg namespace: ", fn_name))
    if (is.null(fn)) next

    parsed <- .parse_worker_kwargs(path)
    expect_gt(length(parsed$arg), 0L,
      label = paste0("no `<arg> = params$<field>` calls found in ", basename))

    valid_args <- names(formals(fn))
    bad <- setdiff(parsed$arg, valid_args)
    expect_equal(bad, character(0),
      info = paste0(basename, " passes args not accepted by ", fn_name,
                    "(): ", paste(bad, collapse = ", ")))
  }
})

test_that("worker kwarg names match the params field they read", {
  # Hygiene: `outcome = params$outcome` (good). `outcome = params$exposure`
  # (bad -- silent NULL). The plan-side item builders use names matching
  # the function formals, so the kwarg name and the field name should
  # match. Catches typos like `sep_by_tx = params$sep_by_exp` even when
  # both names happen to live in the same scope.
  inst_dir <- testthat::test_path("..", "..", "inst")
  skip_if_not(dir.exists(inst_dir), "inst/ not found (installed package?)")
  for (basename in names(worker_to_fn)) {
    path <- file.path(inst_dir, basename)
    if (!file.exists(path)) next
    parsed <- .parse_worker_kwargs(path)
    mismatched <- which(parsed$arg != parsed$field)
    expect_equal(mismatched, integer(0),
      info = paste0(basename, " has kwarg/field name mismatches: ",
                    paste(sprintf("%s = params$%s",
                                  parsed$arg[mismatched],
                                  parsed$field[mismatched]),
                          collapse = "; ")))
  }
})
