# THE step that makes "the engine lives elsewhere" real (PROJECT.md, Phase 4):
# swereg now contains ZERO dispatch/engine code -- the one dispatcher was
# extracted into `batchit`, and swereg drives it through four thin adapter
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
# NOTE: this NORMALISES `:::` to `::`, so its output cannot distinguish the two.
# Anything that must treat `:::` differently uses .lockdown_batchit_refs below.
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

# Operator-preserving variant: returns a data.frame of one row per qualified
# batchit reference, with `sym` ("batchit::x", normalised, for set membership)
# and `internal` (TRUE when the source actually wrote `batchit:::x`). The
# classifier below needs the operator, and .lockdown_batchit_mentions() throws
# it away -- `batchit:::write_qs2_atomically` and
# `batchit::write_qs2_atomically` are indistinguishable by symbol name alone.
.lockdown_batchit_refs <- function(e,
                                   acc = data.frame(sym = character(),
                                                    internal = logical(),
                                                    stringsAsFactors = FALSE)) {
  if (is.call(e)) {
    f <- e[[1L]]
    is_dbl <- identical(f, quote(`::`))
    is_tpl <- identical(f, quote(`:::`))
    if ((is_dbl || is_tpl) && length(e) == 3L &&
        identical(as.character(e[[2L]]), "batchit")) {
      acc <- rbind(acc, data.frame(
        sym = paste0("batchit::", as.character(e[[3L]])),
        internal = is_tpl,
        stringsAsFactors = FALSE
      ))
    }
  }
  if (is.recursive(e)) {
    for (i in seq_along(e)) {
      acc <- tryCatch(.lockdown_batchit_refs(e[[i]], acc),
        error = function(err) acc)
    }
  }
  acc
}

# batchit's DISPATCH surface: adapter-only, so target selection stays behind
# mockable .batch_* wrappers. This is what the ban is actually for.
.lockdown_batchit_dispatch <- c(
  "batchit::package_function", "batchit::run", "batchit::run_and_collect",
  "batchit::run_and_write_files_atomically",
  "batchit::stream_from_parent_and_write_files_atomically",
  "batchit::where_to_write_output"
)
# batchit PRIMITIVES: plain utilities, no dispatch, nothing to mock -- callable
# from anywhere in swereg.
.lockdown_batchit_primitives <- c("batchit::write_qs2_atomically")

# The one adapter file, as a REPO-RELATIVE path. Comparing basename(f) would
# silently exempt a second file at R/subdir/batch_adapter.R.
.lockdown_adapter_relpath <- "R/batch_adapter.R"

# Classify one file's references. `relpath` is repo-relative (e.g. "R/qs2.R").
# A reference offends when:
#   * it is DISPATCH and the file is not the adapter, OR
#   * it uses `:::` (reaching into batchit's internals is never allowed,
#     primitive or not), OR
#   * it is in NEITHER set -- an unclassified batchit export must fail loudly,
#     so a new batchit export cannot be used here unreviewed.
.lockdown_batchit_offences <- function(refs, relpath) {
  if (nrow(refs) == 0L) return(character(0))
  known <- c(.lockdown_batchit_dispatch, .lockdown_batchit_primitives)
  is_adapter <- identical(relpath, .lockdown_adapter_relpath)
  bad <- character(0)
  for (i in seq_len(nrow(refs))) {
    sym <- refs$sym[i]
    if (refs$internal[i]) {
      bad <- c(bad, sub("^batchit::", "batchit:::", sym))
    } else if (!sym %in% known) {
      bad <- c(bad, paste0(sym, " [unclassified]"))
    } else if (sym %in% .lockdown_batchit_dispatch && !is_adapter) {
      bad <- c(bad, sym)
    }
  }
  unique(bad)
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

test_that("batchit DISPATCH is named only from the adapter; primitives anywhere", {
  pkg_root <- testthat::test_path("..", "..")
  r_dir <- file.path(pkg_root, "R")
  skip_if_not(dir.exists(r_dir), "R/ sources not present (installed package?)")

  # The two sets must not overlap, or a symbol's classification would depend on
  # lookup order rather than on what it is.
  expect_equal(
    intersect(.lockdown_batchit_dispatch, .lockdown_batchit_primitives),
    character(0)
  )
  # DISPATCH is exactly the required-forwards set asserted at the end of this
  # test: if the two drifted apart, a forward could be required AND unbanned.
  expect_setequal(
    .lockdown_batchit_dispatch,
    c("batchit::package_function", "batchit::run", "batchit::run_and_collect",
      "batchit::run_and_write_files_atomically",
      "batchit::stream_from_parent_and_write_files_atomically",
      "batchit::where_to_write_output")
  )

  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  offenders <- character(0)
  for (f in files) {
    exprs <- parse(f, keep.source = FALSE)
    refs <- Reduce(
      function(a, ex) .lockdown_batchit_refs(ex, a),
      exprs,
      init = data.frame(sym = character(), internal = logical(),
        stringsAsFactors = FALSE)
    )
    relpath <- file.path("R", substring(
      normalizePath(f, winslash = "/", mustWork = FALSE),
      nchar(normalizePath(r_dir, winslash = "/", mustWork = FALSE)) + 2L
    ))
    bad <- .lockdown_batchit_offences(refs, relpath)
    if (length(bad) > 0L) {
      offenders <- c(offenders, paste0(relpath, ": ", paste(bad, collapse = ", ")))
    }
  }
  expect_equal(offenders, character(0),
    info = paste(
      "batchit dispatch named outside the adapter, an unclassified batchit",
      "symbol, or a `:::` reach into batchit internals:",
      paste(offenders, collapse = " | ")
    ))

  # The classifier itself must be tested, or the ban above could pass by doing
  # nothing. Synthetic fixtures, parsed from text (as the processx/mirai probe
  # above does), one per branch.
  .refs_of <- function(txt) {
    Reduce(
      function(a, ex) .lockdown_batchit_refs(ex, a),
      parse(text = txt, keep.source = FALSE),
      init = data.frame(sym = character(), internal = logical(),
        stringsAsFactors = FALSE)
    )
  }
  # dispatch OUTSIDE the adapter -> offender
  expect_equal(
    .lockdown_batchit_offences(.refs_of("batchit::run(x)"), "R/qs2.R"),
    "batchit::run"
  )
  # dispatch INSIDE the adapter -> allowed
  expect_equal(
    .lockdown_batchit_offences(.refs_of("batchit::run(x)"), "R/batch_adapter.R"),
    character(0)
  )
  # primitive anywhere -> allowed
  expect_equal(
    .lockdown_batchit_offences(
      .refs_of("batchit::write_qs2_atomically(o, p)"), "R/qs2.R"),
    character(0)
  )
  # unclassified batchit symbol -> offender
  expect_equal(
    .lockdown_batchit_offences(.refs_of("batchit::brand_new_thing(1)"), "R/qs2.R"),
    "batchit::brand_new_thing [unclassified]"
  )
  # `:::` into a PRIMITIVE -> still an offender
  expect_equal(
    .lockdown_batchit_offences(
      .refs_of("batchit:::write_qs2_atomically(o, p)"), "R/qs2.R"),
    "batchit:::write_qs2_atomically"
  )
  # a subdirectory file named batch_adapter.R is NOT the adapter
  expect_equal(
    .lockdown_batchit_offences(.refs_of("batchit::run(x)"),
      "R/subdir/batch_adapter.R"),
    "batchit::run"
  )

  # And the adapter DOES name batchit -- otherwise the wrappers are hollow and
  # this guard is vacuous.
  adapter <- file.path(r_dir, "batch_adapter.R")
  expect_true(file.exists(adapter))
  adapter_hits <- unique(unlist(lapply(
    parse(adapter, keep.source = FALSE), .lockdown_batchit_mentions
  )))
  expect_true(any(grepl("^batchit::run$", adapter_hits)))

  # All FIVE `.batch_*` wrappers -- SIX batchit symbols, because `.batch_run`
  # forwards to both run() and run_and_collect() -- must actually go through
  # the adapter, not just run():
  # if a wrapper quietly stopped forwarding (a hollowed-out .batch_target,
  # .batch_stream, or .batch_where_to_write_output), its call sites would
  # reach batchit -- or fail to -- outside this one enforced seam, undetected.
  # `.batch_run` itself forwards to batchit::run() OR batchit::run_and_collect()
  # depending on its caller's `collect` -- both branches must still be present,
  # not just one, or a hollowed-out branch would go undetected. Assert every
  # forward is present (batchit 26.7.20 renamed batch_target -> package_function,
  # batch_run -> run/run_and_collect, batch_stream ->
  # stream_from_parent_and_write_files_atomically, batch_stage_path ->
  # where_to_write_output).
  expect_true(all(
    c("batchit::package_function", "batchit::run", "batchit::run_and_collect",
      "batchit::run_and_write_files_atomically",
      "batchit::stream_from_parent_and_write_files_atomically",
      "batchit::where_to_write_output")
      %in% adapter_hits
  ))
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
