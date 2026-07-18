# THE step that makes "the engine lives elsewhere" real (PROJECT.md, Phase 4):
# swereg now contains ZERO dispatch/engine code -- the one dispatcher was
# extracted into `batchit`, and swereg drives it through three thin adapter
# wrappers (R/batch_adapter.R). A package boundary is not access control (`:::`
# exists); what enforces the split is making the engine unavailable from swereg's
# own code and TESTING that no bypass exists. This is the enforcement.
#
# Parse-based, not grep: comments legitimately mention the dead/relocated engines
# as history ("the old callr engine serialized..."), so a line-based grep would
# either false-positive on them or need fragile filtering. parse() + an AST walk
# sees only code. Scope is R/ AND inst/ -- a worker script is package code too,
# and inst/ is where the old hand-written dispatchers used to hide.

# Collect every prohibited dispatch/process reference reachable from an
# expression. Prohibited: ANY `pkg::`/`pkg:::` MENTION of processx/callr/mirai
# -- a mention, not just a call head, because `processx::process$new(...)` and
# `x <- callr::r_bg; x(...)` both dispatch without the qualified name ever
# being the call head -- plus the parallel-package process spawners and their
# bare names. Deliberately NOT banned: system()/system2() -- metadata
# shell-outs (e.g. the git SHA in the summary TSV filename) are not work
# dispatch -- and parallel::detectCores(), a core COUNT routed through
# .safe_n_cores().
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

# Every `batchit::`/`batchit:::` qualified mention reachable from an expression.
.lockdown_batchit_mentions <- function(e, acc = character()) {
  if (is.call(e)) {
    f <- e[[1L]]
    if ((identical(f, quote(`::`)) || identical(f, quote(`:::`))) &&
        length(e) == 3L && identical(as.character(e[[2L]]), "batchit")) {
      acc <- c(acc, paste0("batchit::", as.character(e[[3L]])))
    }
  }
  if (is.recursive(e)) {
    for (i in seq_along(e)) {
      acc <- tryCatch(.lockdown_batchit_mentions(e[[i]], acc),
        error = function(err) acc)
    }
  }
  acc
}

test_that("no engine dispatch primitive appears anywhere in R/ or inst/", {
  pkg_root <- testthat::test_path("..", "..")
  r_dir <- file.path(pkg_root, "R")
  inst_dir <- file.path(pkg_root, "inst")
  skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")

  # NO allowlist: the engine lives in batchit now, so nothing in swereg's own
  # code -- not even the adapter -- names processx/callr/mirai or a spawner.
  # recursive = TRUE: a dispatcher hidden in inst/scripts/ or an R/ subdir must
  # be parsed too.
  files <- c(
    list.files(r_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE),
    list.files(inst_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  )

  offenders <- character(0)
  for (f in files) {
    exprs <- parse(f, keep.source = FALSE)
    hits <- unique(unlist(lapply(exprs, .lockdown_calls)))
    if (length(hits) > 0L) {
      offenders <- c(offenders, paste0(basename(f), ": ", paste(hits, collapse = ", ")))
    }
  }
  expect_equal(offenders, character(0),
    info = paste(
      "process/dispatch primitives in swereg (the engine belongs in batchit):",
      paste(offenders, collapse = " | ")
    ))

  # Not vacuous: the same walker MUST still see processx/mirai when they ARE
  # present. Prove it against a synthetic expression (swereg has no such code
  # left to point at), so a parse/walk regression fails loudly rather than the
  # ban passing by seeing nothing anywhere.
  probe <- parse(text = "{ processx::process$new(cmd); mirai::daemons(2L) }",
    keep.source = FALSE)
  probe_hits <- unique(unlist(lapply(probe, .lockdown_calls)))
  expect_true(any(grepl("^processx::", probe_hits)))
  expect_true(any(grepl("^mirai::", probe_hits)))
})

test_that("batchit is named ONLY from the adapter (R/batch_adapter.R)", {
  pkg_root <- testthat::test_path("..", "..")
  r_dir <- file.path(pkg_root, "R")
  skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")

  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  offenders <- character(0)
  for (f in files) {
    exprs <- parse(f, keep.source = FALSE)
    hits <- unique(unlist(lapply(exprs, .lockdown_batchit_mentions)))
    if (length(hits) > 0L && basename(f) != "batch_adapter.R") {
      offenders <- c(offenders, paste0(basename(f), ": ", paste(hits, collapse = ", ")))
    }
  }
  expect_equal(offenders, character(0),
    info = paste(
      "batchit:: named outside the adapter (target selection must go through",
      "the .batch_* wrappers):", paste(offenders, collapse = " | ")
    ))

  # And the adapter DOES name batchit -- otherwise the wrappers are hollow and
  # this guard is vacuous.
  adapter <- file.path(r_dir, "batch_adapter.R")
  expect_true(file.exists(adapter))
  adapter_hits <- unique(unlist(lapply(
    parse(adapter, keep.source = FALSE), .lockdown_batchit_mentions
  )))
  expect_true(any(grepl("^batchit::batch_run$", adapter_hits)))
})

test_that("the engine files are GONE from swereg for good", {
  pkg_root <- testthat::test_path("..", "..")
  r_dir <- file.path(pkg_root, "R")
  inst_dir <- file.path(pkg_root, "inst")
  skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")

  # The dispatcher module and the generic worker script both left swereg (they
  # live in batchit now); neither may reappear.
  expect_false(file.exists(file.path(r_dir, "batch.R")))
  expect_false(file.exists(file.path(inst_dir, "batch_worker.R")))

  # The eight hand-written workers + worker_bootstrap.R must not reappear either.
  expect_identical(
    list.files(inst_dir, pattern = "^worker_.*\\.R$"),
    character(0)
  )

  # processx and callr left DESCRIPTION with the dispatcher -- the transport is
  # batchit's now, so swereg's own CODE dispatches through neither: both are
  # banned from the HARD dependencies (Imports/Depends). processx may sit in
  # Suggests -- the qs2_write_atomic() "worker killed mid-write" test spawns a
  # real process directly (guarded by skip_if_not_installed), which is a test
  # HARNESS, not a transport, and R CMD check --as-cran needs it declared. callr
  # stays banned everywhere (Imports/Depends/Suggests) -- nothing uses it, tests
  # included.
  desc <- read.dcf(file.path(pkg_root, "DESCRIPTION"))
  hard_deps <- paste(desc[1L, intersect(colnames(desc),
    c("Imports", "Depends"))], collapse = " ")
  expect_false(grepl("\\bprocessx\\b", hard_deps))
  expect_false(grepl("\\bcallr\\b", hard_deps))
  suggests <- paste(desc[1L, intersect(colnames(desc), "Suggests")],
    collapse = " ")
  expect_false(grepl("\\bcallr\\b", suggests))

  # And parallel_pool is not merely unexported but GONE.
  expect_false(exists("parallel_pool", envir = asNamespace("swereg"),
    inherits = FALSE))
})
