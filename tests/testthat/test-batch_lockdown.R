# THE step that makes "one dispatcher" real (PROJECT.md, Phase 3): no dispatch
# path other than the batch module exists in package code, and none can
# quietly come back. A package boundary is not access control (`:::` exists);
# what enforces a contract is making one dispatcher unavoidable, validating at
# both ends, and TESTING that no bypass exists. This is the enforcement.
#
# Parse-based, not grep: comments legitimately mention the dead engines as
# history ("the old callr engine serialized..."), so a line-based grep would
# either false-positive on them or need fragile filtering. parse() + an AST
# walk sees only code. Scope is R/ AND inst/ -- a worker script is package
# code too, and inst/ is where the eight hand-written dispatchers (plus the
# shared worker_bootstrap.R) used to hide.

# Collect every prohibited dispatch/process reference reachable from an
# expression. Prohibited: ANY `pkg::`/`pkg:::` MENTION of processx/callr/mirai
# -- a mention, not just a call head, because `processx::process$new(...)` and
# `x <- callr::r_bg; x(...)` both dispatch without the qualified name ever
# being the call head -- plus the parallel-package process spawners and their
# bare names. Deliberately NOT banned: system()/system2() -- metadata
# shell-outs (e.g. the git SHA in the summary TSV filename) are not work
# dispatch, and the engines are what this lock guards -- and
# parallel::detectCores(), a core COUNT routed through .safe_n_cores().
.lockdown_banned_bare <- c(
  "mcparallel", "mclapply", "mcmapply",
  "makeCluster", "makeForkCluster", "makePSOCKcluster",
  "clusterApply", "clusterApplyLB", "parLapply", "parSapply", "r_bg"
)

.lockdown_calls <- function(e, acc = character()) {
  if (is.call(e)) {
    f <- e[[1L]]
    if ((identical(f, quote(`::`)) || identical(f, quote(`:::`))) &&
        length(e) == 3L) {
      pkg <- as.character(e[[2L]])
      fn <- as.character(e[[3L]])
      if (pkg %in% c("processx", "callr", "mirai")) {
        acc <- c(acc, paste0(pkg, "::", fn))
      }
      if (pkg == "parallel" && fn %in% .lockdown_banned_bare) {
        acc <- c(acc, paste0(pkg, "::", fn))
      }
    }
    if (is.symbol(f) && as.character(f) %in% .lockdown_banned_bare) {
      acc <- c(acc, as.character(f))
    }
  }
  if (is.recursive(e)) {
    for (i in seq_along(e)) {
      # e[[i]] can be the empty symbol (a missing argument); touching it errors.
      acc <- tryCatch(.lockdown_calls(e[[i]], acc), error = function(err) acc)
    }
  }
  acc
}

test_that("only the batch module dispatches subprocesses (R/ and inst/)", {
  pkg_root <- testthat::test_path("..", "..")
  r_dir <- file.path(pkg_root, "R")
  inst_dir <- file.path(pkg_root, "inst")
  skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")

  allowlist <- "batch.R" # the ONE dispatcher module (basenames, R/ only)

  # recursive = TRUE: a dispatcher hidden in inst/scripts/ or an R/ subdir
  # must be parsed too -- a non-recursive scan would let an old-style worker
  # reappear one directory down without any obfuscation at all.
  files <- c(
    list.files(r_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE),
    list.files(inst_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  )
  # Allowlist matched by basename+parent (robust to path normalisation) -- so
  # only R/batch.R itself is exempt; an R/sub/batch.R would still be scanned.
  scan <- files[!(basename(files) %in% allowlist &
    basename(dirname(files)) == "R")]

  offenders <- character(0)
  for (f in scan) {
    exprs <- parse(f, keep.source = FALSE)
    hits <- unique(unlist(lapply(exprs, .lockdown_calls)))
    if (length(hits) > 0L) {
      offenders <- c(offenders, paste0(basename(f), ": ", paste(hits, collapse = ", ")))
    }
  }
  expect_equal(offenders, character(0),
    info = paste(
      "process/dispatch primitives outside the batch module:",
      paste(offenders, collapse = " | ")
    ))

  # Not vacuous: the same walker MUST find the engine calls inside batch.R
  # itself -- if it goes blind (a parse/walk regression), this fails loudly
  # instead of the ban passing by seeing nothing anywhere.
  batch_hits <- unique(unlist(lapply(
    parse(file.path(r_dir, "batch.R"), keep.source = FALSE), .lockdown_calls
  )))
  expect_true(any(grepl("^processx::", batch_hits)))
  expect_true(any(grepl("^mirai::", batch_hits)))
})

test_that("the hand-written workers, bootstrap, and callr are gone for good", {
  pkg_root <- testthat::test_path("..", "..")
  inst_dir <- file.path(pkg_root, "inst")
  skip_if_not(dir.exists(inst_dir), "inst/ not present (installed package?)")

  # The ONE worker script is all that remains; worker_*.R (eight dispatchers +
  # worker_bootstrap.R) must not reappear.
  expect_identical(
    list.files(inst_dir, pattern = "^worker_.*\\.R$"),
    character(0)
  )
  expect_true(file.exists(file.path(inst_dir, "batch_worker.R")))

  # callr left DESCRIPTION with its last caller.
  desc <- read.dcf(file.path(pkg_root, "DESCRIPTION"))
  deps <- paste(desc[1L, intersect(colnames(desc),
    c("Imports", "Suggests", "Depends"))], collapse = " ")
  expect_false(grepl("\\bcallr\\b", deps))

  # And parallel_pool is not merely unexported but GONE.
  expect_false(exists("parallel_pool", envir = asNamespace("swereg"),
    inherits = FALSE))
})
