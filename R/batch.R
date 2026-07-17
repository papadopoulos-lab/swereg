# One dispatcher, one contract. See PROJECT.md (Phase 2).
#
# This file is package-neutral by design: it imports nothing from swereg's R6
# classes or the TTE/skeleton domain, so it can later be lifted into `batchit`
# with a `git mv` (Phase 4). The only swereg helpers it leans on --
# .hash_function(), .validate_n_workers(), .safe_n_cores(), .pp_log_tail() --
# are themselves domain-free and would travel with it.
#
# Two frontends, ONE contract:
#   .batch_run(target, items, ...)     -- shape A: items already exist, fresh
#                                          subprocess per item (memory strategy).
#   .batch_stream(target, ids, ...)    -- shape B: the parent is the producer,
#                                          items are generated lazily under
#                                          backpressure (mirai bounded queue).
# They share target resolution, both-end validation, the result envelope, and
# failure semantics. They differ only in internal transport, which is private.

.BATCH_PROTOCOL <- 1L

# Generous per-item wall-clock default: a hang-catcher, not a deadline. Long
# enough that no legitimate pipeline item (skeleton, s1c, s2, or a ~20 GB s3
# panel) hits it, short enough that a deadlocked or infinite-looping worker does
# not sit forever. Callers with genuinely longer items must raise it explicitly.
.BATCH_DEFAULT_TIMEOUT <- 6 * 3600

# --- target descriptor -------------------------------------------------------

#' Describe a dispatch target
#'
#' A target is a *descriptor*, never a function object, name, or closure:
#' package + symbol + a hash of the function's body and formals. The hash is the
#' same `digest(list(body, formals))` that skeleton-phase replay already uses
#' (`.hash_function()`), so an edit to the target invalidates dispatch exactly as
#' it invalidates a cached phase. Package-name-plus-symbol alone is insufficient:
#' dev code, installed code and cache identity can differ (see
#' `r6_tteplan.R:5929`), and a contract that does not fix that preserves the
#' stale-code/resume problem under a new name.
#'
#' @param package Package holding the target (character scalar).
#' @param symbol Name of the target function in that package (character scalar).
#' @param version Optional recorded version; defaults to the package's installed
#'   version. Advisory only -- the hash is what the child actually checks.
#' @return A `batch_target` descriptor (a list): package, symbol, version, hash,
#'   formal_names.
#' @noRd
.batch_target <- function(package, symbol, version = NULL) {
  if (!is.character(package) || length(package) != 1L || !nzchar(package)) {
    stop(".batch_target(): `package` must be a non-empty string", call. = FALSE)
  }
  if (!is.character(symbol) || length(symbol) != 1L || !nzchar(symbol)) {
    stop(".batch_target(): `symbol` must be a non-empty string", call. = FALSE)
  }
  ns <- tryCatch(
    asNamespace(package),
    error = function(e) {
      stop(
        sprintf(".batch_target(): package '%s' is not available: %s",
          package, conditionMessage(e)),
        call. = FALSE
      )
    }
  )
  if (!exists(symbol, envir = ns, inherits = FALSE)) {
    stop(
      sprintf(".batch_target(): '%s' is not defined in package '%s'",
        symbol, package),
      call. = FALSE
    )
  }
  fn <- get(symbol, envir = ns, inherits = FALSE)
  if (!is.function(fn)) {
    stop(
      sprintf(".batch_target(): %s::%s is not a function", package, symbol),
      call. = FALSE
    )
  }
  # names(formals(fn)) is NULL for a zero-argument function; normalise so
  # formal_names is always a character vector (possibly empty), never NULL --
  # otherwise a legitimate no-arg target looks like a malformed descriptor.
  fmls <- names(formals(fn))
  if (is.null(fmls)) fmls <- character(0)
  if ("..." %in% fmls) {
    stop(
      sprintf(
        paste0(".batch_target(): %s::%s takes `...`, which is incompatible ",
          "with reliable argument validation. A dispatch target must have a ",
          "fixed formal list so a mistyped or missing argument can be caught."),
        package, symbol),
      call. = FALSE
    )
  }
  structure(
    list(
      package = package,
      symbol = symbol,
      version = version %||% as.character(utils::packageVersion(package)),
      hash = .hash_function(fn),
      formal_names = fmls
    ),
    class = "batch_target"
  )
}

# --- item validation (runs at BOTH ends) -------------------------------------

#' Validate one work item against its target's formals
#'
#' The contract, enforced identically in the parent (early UX) and the child
#' (correctness -- the child may have loaded a different code version). Every
#' rule here exists because its absence was a real bug:
#'
#' * **Every formal must be named, including optional ones.** `arm_labels` is an
#'   *optional* formal, and it was silently dropped for a year precisely because
#'   the old regex check only demanded the required ones. Demanding all of them
#'   makes "an optional arg silently absent" indistinguishable from a typo, which
#'   is the point.
#' * **No positional, duplicate, or blank names**, and **no argument that is not
#'   a formal** -- a typo'd field name must be rejected, not silently ignored.
#'
#' @param target A `batch_target` descriptor (its `formal_names` is the schema).
#' @param args The item: a fully-named list of arguments.
#' @param where "parent" or "child", for the error message.
#' @param id Optional item id, for the error message.
#' @return `TRUE`, invisibly; stops on any violation.
#' @noRd
.batch_validate_item <- function(target, args, where = "parent", id = NULL) {
  loc <- if (is.null(id)) "" else sprintf(" [item '%s']", id)
  lead <- sprintf(".batch %s-validation%s: %s::%s",
    where, loc, target$package, target$symbol)

  if (!is.list(args)) {
    stop(sprintf("%s -- item must be a list, got %s",
      lead, class(args)[1L]), call. = FALSE)
  }
  nms <- names(args)
  if (length(args) > 0L && (is.null(nms) || any(!nzchar(nms)))) {
    stop(sprintf("%s -- every argument must be named (no positional arguments)",
      lead), call. = FALSE)
  }
  if (anyDuplicated(nms)) {
    dup <- unique(nms[duplicated(nms)])
    stop(sprintf("%s -- duplicate argument name(s): %s",
      lead, paste(dup, collapse = ", ")), call. = FALSE)
  }
  extra <- setdiff(nms, target$formal_names)
  if (length(extra) > 0L) {
    stop(sprintf("%s -- argument(s) that are not formals of the target: %s",
      lead, paste(extra, collapse = ", ")), call. = FALSE)
  }
  missing <- setdiff(target$formal_names, nms)
  if (length(missing) > 0L) {
    stop(sprintf(
      paste0("%s -- formal(s) not supplied: %s. Every formal must be named ",
        "explicitly, including optional ones -- that is what catches a ",
        "silently-defaulted argument (this is the arm_labels bug's shape)."),
      lead, paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

# --- private IPC codec -------------------------------------------------------
# The runner owns its OWN transport, matched at both ends, deliberately separate
# from qs2_read()/qs2_write_atomic() (which are swereg persistence for
# scientific files). In particular the read side does NOT run qs2_read()'s R6
# `check_version()` duck-type: envelopes are always plain lists, and that hook is
# swereg policy, not generic transport. The wire format is qs2-standard, so the
# --vanilla worker can read/write it with bare `qs2::` before any package loads.

#' @noRd
.batch_write_envelope <- function(object, path) {
  dir <- dirname(path)
  tmp <- tempfile(pattern = paste0(basename(path), ".tmp"), tmpdir = dir)
  ok <- FALSE
  on.exit(if (!ok) unlink(tmp, force = TRUE), add = TRUE)
  qs2::qs_save(object, tmp)
  if (!file.rename(tmp, path)) {
    stop(".batch_write_envelope(): could not rename ", tmp, " -> ", path,
      call. = FALSE)
  }
  ok <- TRUE
  invisible(path)
}

#' @noRd
.batch_read_envelope <- function(path) {
  qs2::qs_read(path)
}

#' Extract a condition's message without ever throwing
#'
#' `conditionMessage()` dispatches, so a hostile condition (from a target, or a
#' classed object with a registered `conditionMessage` method) could itself throw
#' -- which would escape an error handler and defeat the "total" guarantee. Used
#' wherever a condition arising from untrusted code is rendered to text.
#' @noRd
.batch_condition_message <- function(e) {
  tryCatch(conditionMessage(e), error = function(e2) "<unprintable condition>")
}

#' Validate the STRUCTURE of an input envelope (not its arguments)
#'
#' Cheap structural gate run in the child before anything trusts the envelope:
#' protocol number, meta presence, and the identity fields must be well-formed
#' strings. Without this the protocol number is decorative and a version-skewed
#' or corrupt envelope produces a confusing error deep inside resolution instead
#' of a clear one here. Argument validation (against the target's formals) is a
#' separate step -- see [.batch_validate_item()].
#' @noRd
.batch_check_envelope <- function(env) {
  if (!is.list(env)) {
    stop(".batch envelope is not a list (got ", class(env)[1L], ")", call. = FALSE)
  }
  # Duplicate outer field names would let field selection pick the first of two;
  # and every field is read with EXACT `[[`, never `$` -- `$` partial-matches, so
  # `env$meta` would match an outer field named `metadata`, and `meta$dev_path` a
  # field named `dev_path_payload`, letting a noncanonical name control behaviour.
  if (anyDuplicated(names(env))) {
    stop(".batch envelope has duplicate field names", call. = FALSE)
  }
  if (!identical(env[["protocol"]], .BATCH_PROTOCOL)) {
    stop(sprintf(".batch envelope protocol mismatch: expected %s, got %s",
      .BATCH_PROTOCOL, format(env[["protocol"]] %||% "<none>")), call. = FALSE)
  }
  meta <- env[["meta"]]
  if (!is.list(meta)) {
    stop(".batch envelope has no meta list", call. = FALSE)
  }
  if (anyDuplicated(names(meta))) {
    stop(".batch envelope meta has duplicate field names", call. = FALSE)
  }
  for (f in c("package", "symbol", "hash", "id")) {
    v <- meta[[f]]
    if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v)) {
      stop(sprintf(".batch envelope meta$%s is missing or not a non-empty string", f),
        call. = FALSE)
    }
  }
  collect <- meta[["collect"]]
  if (!is.logical(collect) || length(collect) != 1L || is.na(collect)) {
    stop(".batch envelope meta$collect is missing or not a logical flag", call. = FALSE)
  }
  if (!is.list(env[["args"]])) {
    stop(".batch envelope args is not a list", call. = FALSE)
  }
  invisible(TRUE)
}

# --- child-side execution ----------------------------------------------------

#' Resolve and verify a target in the child process
#'
#' The child may have loaded a different code version than the parent hashed
#' (installed vs dev, or a stale dev tree). If the target's body/formals hash
#' differs, refuse: running a different version than the parent dispatched is the
#' stale-code hole the descriptor exists to close.
#'
#' Note the deliberate NARROWNESS (settled decision, matching `.hash_function()`
#' used for skeleton-phase replay): the hash covers the target's own body and
#' formals only. A changed HELPER the target calls, a namespace constant it
#' closes over, an S4/R6 method table, or a dependency's version are outside it.
#' So this guarantees "same target definition", not "provably identical
#' behaviour" -- the latter is not claimed.
#' @noRd
.batch_resolve_target <- function(meta) {
  # Exact `[[` on the untrusted meta (never `$`, which partial-matches).
  target <- .batch_target(meta[["package"]], meta[["symbol"]],
    version = meta[["version"]])
  if (!identical(target$hash, meta[["hash"]])) {
    stop(
      sprintf(
        paste0(".batch_worker: %s::%s resolved to a DIFFERENT code version ",
          "than the parent dispatched (parent hash %s, child hash %s). ",
          "Refusing to run -- check the dev path / installed package version."),
        meta[["package"]], meta[["symbol"]], meta[["hash"]], target$hash),
      call. = FALSE
    )
  }
  target
}

#' Execute one envelope in the child and build the result envelope
#'
#' Total by design: it always returns a result envelope, never throws. Every
#' failure the child can hit -- target resolution, the hash mismatch, child-side
#' item re-validation, and the target's own R-level errors -- is caught into ONE
#' structured error envelope (status "error", value NULL, `error$message`). That
#' uniformity is the point: both frontends (`.batch_run` reading a file,
#' `.batch_stream` reading a daemon return) surface every failure the same way,
#' instead of a resolve error crashing the worker while a target error returns an
#' envelope. `meta$collect == FALSE` drops the value entirely: shape-A
#' direct-writers put gigabytes on disk themselves, and the whole architecture
#' exists so those never cross back to the parent -- only the status does.
#' @noRd
.batch_execute <- function(env) {
  # The reported id lets the parent match a result to the item it dispatched;
  # extract it defensively so even a malformed envelope carries one (or NA).
  # Exact `[[` throughout (never `$`, which partial-matches an untrusted field).
  id <- tryCatch(env[["meta"]][["id"]], error = function(e) NA_character_)

  outcome <- tryCatch(
    {
      .batch_check_envelope(env)
      meta <- env[["meta"]]
      target <- .batch_resolve_target(meta)
      .batch_validate_item(target, env[["args"]], where = "child", id = meta[["id"]])
      fn <- get(meta[["symbol"]], envir = asNamespace(meta[["package"]]),
        inherits = FALSE)

      # Capture the target's warnings into the envelope instead of letting them
      # scroll off into a log the parent deletes on success. This matters for the
      # migrated caller: .s3_enrollment_worker() catches a Table 1 failure, WARNS,
      # and returns a partial (status "ok") result -- without this the incomplete
      # result would be stored with no word to the parent.
      warns <- character()
      value <- withCallingHandlers(
        do.call(fn, env[["args"]]),
        warning = function(w) {
          warns[[length(warns) + 1L]] <<- .batch_condition_message(w)
          invokeRestart("muffleWarning")
        }
      )
      list(
        status = "ok",
        value = if (isTRUE(meta[["collect"]])) value else NULL,
        error = NULL,
        warnings = utils::head(warns, 100L),
        target = list(package = target$package, symbol = target$symbol,
          hash = target$hash)
      )
    },
    error = function(e) list(
      status = "error",
      value = NULL,
      error = list(
        message = .batch_condition_message(e),
        call = tryCatch(paste(deparse(conditionCall(e)), collapse = " "),
          error = function(e2) "<unprintable call>")
      ),
      warnings = character(),
      target = NULL
    )
  )

  list(
    protocol = .BATCH_PROTOCOL,
    id = id,
    status = outcome$status,
    value = outcome$value,
    error = outcome$error,
    warnings = outcome$warnings,
    target = outcome$target
  )
}

#' Inspect a result envelope in the parent: accept it, or say why not
#'
#' Makes the result-envelope fields load-bearing rather than decorative, and is
#' TOTAL -- a non-list or otherwise malformed result becomes a `reason`, never a
#' throw, so it flows through the caller's uniform failure path (logging,
#' retention) like any other failure. Shared by both frontends so they
#' accept/reject identically. Returns `list(ok, reason, value, warnings)`.
#'
#' Checks, in order: the result is a list; protocol; status; the id matches the
#' dispatched id; and (on success) the FULL executed-target identity -- package,
#' symbol AND hash -- matches what was dispatched (the contract defines identity
#' as all three; a body/formals hash can collide across two functions), plus that
#' a successful envelope actually carries a `value` field.
#' @noRd
.batch_inspect_result <- function(envelope, expected_id, target) {
  # Total BY CONSTRUCTION: any error while inspecting a hostile or corrupt result
  # -- a classed object with a throwing `$`/`format` method, a field that errors
  # on access -- becomes a failure reason, so it flows through the caller's
  # uniform .fail()/retention path rather than crashing the pool.
  tryCatch(
    .batch_inspect_result_impl(envelope, expected_id, target),
    error = function(e) list(ok = FALSE,
      reason = paste0("malformed result envelope: ", .batch_condition_message(e)))
  )
}

#' @noRd
.batch_inspect_result_impl <- function(envelope, expected_id, target) {
  if (!is.list(envelope)) {
    return(list(ok = FALSE, reason = sprintf(
      "result is not a list (got %s)", class(envelope)[1L])))
  }
  # Reject missing / blank / DUPLICATE field names: `$` returns the first match,
  # so a result carrying both `protocol = 1L` and `protocol = 99L` (or duplicate
  # id/target fields) could otherwise smuggle a bad value behind a good one.
  nm <- names(envelope)
  if (is.null(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
    return(list(ok = FALSE,
      reason = "result envelope has missing, blank, or duplicate field names"))
  }
  # Every field is read with EXACT `[[`, never `$`: `$` partial-matches, so an
  # absent `status`/`id`/`target` beside a longer-named field (`status_x`) would
  # otherwise resolve to the wrong value. (`target` is the dispatched descriptor,
  # our own trusted list, so `target$...` stays `$`.)
  if (!identical(envelope[["protocol"]], .BATCH_PROTOCOL)) {
    return(list(ok = FALSE, reason = sprintf(
      "result envelope has wrong/missing protocol: %s",
      format(envelope[["protocol"]] %||% "<none>"))))
  }
  # id is checked BEFORE status and STRICTLY (a single character, identical -- no
  # numeric-to-string coercion): a result must be the one dispatched for THIS
  # item even when it carries an error. The worker echoes the dispatched id on
  # every path, including its load-failure fallback, so an error result still
  # gets id-validated here and its message surfaced at the status check below.
  eid <- envelope[["id"]]
  if (!is.character(eid) || length(eid) != 1L ||
      !identical(eid, as.character(expected_id))) {
    return(list(ok = FALSE, reason = sprintf(
      "result envelope id mismatch: expected '%s', got %s",
      expected_id, format(eid %||% "<none>"))))
  }
  if (!identical(envelope[["status"]], "ok")) {
    # `error` may be malformed (e.g. a bare string) -- do not let extracting the
    # message throw; the inspector stays total.
    msg <- tryCatch(envelope[["error"]][["message"]], error = function(e) NULL)
    if (!is.character(msg) || length(msg) != 1L) msg <- "failed with no error message"
    return(list(ok = FALSE, reason = sprintf("returned an error: %s", msg)))
  }
  tgt <- envelope[["target"]]
  # `anyDuplicated(names(tgt))`: a nested `target = list(package="swereg",
  # package="evil", ...)` must not let the first `package` win and leave the
  # executed identity ambiguous.
  if (!is.list(tgt) || anyDuplicated(names(tgt)) ||
      !identical(tgt[["package"]], target$package) ||
      !identical(tgt[["symbol"]], target$symbol) ||
      !identical(tgt[["hash"]], target$hash)) {
    return(list(ok = FALSE, reason = sprintf(
      "result came from a different target than dispatched (expected %s::%s, hash %s)",
      target$package, target$symbol, target$hash)))
  }
  if (!("value" %in% names(envelope))) {
    return(list(ok = FALSE, reason = "successful result envelope has no value field"))
  }
  warnings <- envelope[["warnings"]]
  # Never coerce an arbitrary object (as.character() on a closure throws); a
  # non-character warnings field is simply dropped, keeping the inspector total.
  if (!is.character(warnings)) warnings <- character()
  list(ok = TRUE, reason = NULL, value = envelope[["value"]], warnings = warnings)
}

#' Re-emit a completed item's captured warnings in the parent, tagged by id
#' @noRd
.batch_surface_warnings <- function(warnings, id) {
  for (w in warnings) {
    warning(sprintf("[batch item '%s'] %s", id, w), call. = FALSE)
  }
  invisible(NULL)
}

# --- worker-script + dev-path resolution -------------------------------------

#' The package this runner is compiled into ("swereg" now, "batchit" later)
#' @noRd
.batch_runner_package <- function() {
  utils::packageName(environment(.batch_runner_package)) %||% "swereg"
}

#' Validate a consumer dev path, or pass NULL through
#'
#' A dev path that was ASKED FOR but is wrong is an error, never a silent
#' fall-through to installed code (defect #5): the tree must exist, be an R
#' package source, and name the consumer package. Returns the normalised path, or
#' `NULL` for the installed-package case. Shared by [.batch_run()] (processx) and
#' [.batch_stream()] (mirai) so both enforce the same policy.
#' @noRd
.batch_validate_dev_path <- function(dev_path, consumer_package) {
  if (is.null(dev_path)) return(NULL)
  dev_path <- normalizePath(dev_path, mustWork = FALSE)
  if (!dir.exists(dev_path)) {
    stop(
      ".batch: dev_path was given but does not exist: ", dev_path,
      "\n  Refusing to fall back to the installed package, which would ",
      "silently run different code than you asked for.\n  Pass dev_path = NULL ",
      "to use the installed package deliberately.",
      call. = FALSE
    )
  }
  dcf_path <- file.path(dev_path, "DESCRIPTION")
  if (!file.exists(dcf_path)) {
    stop(".batch: dev_path is not an R package source tree ",
      "(no DESCRIPTION): ", dev_path, call. = FALSE)
  }
  dev_pkg <- tryCatch(
    unname(read.dcf(dcf_path, fields = "Package")[1L, 1L]),
    error = function(e) NA_character_
  )
  if (is.na(dev_pkg) || !identical(dev_pkg, consumer_package)) {
    stop(sprintf(
      ".batch: dev_path points at package '%s', not '%s': %s",
      dev_pkg, consumer_package, dev_path), call. = FALSE)
  }
  dev_path
}

#' Locate inst/batch_worker.R for an already-validated dev path (or installed)
#'
#' `dev_path` must already have passed [.batch_validate_dev_path()] (normalised,
#' or NULL for installed). Phase-2 seam: runner and consumer are the same package
#' (swereg), so one dev tree serves both the worker script (runner's inst) and
#' the loaded package (consumer). When `batchit` is extracted the worker script
#' comes from the installed runner while `dev_path` is the consumer's tree; that
#' split is a Phase-4 change.
#' @noRd
.batch_worker_script <- function(dev_path) {
  if (is.null(dev_path)) {
    script <- system.file("batch_worker.R", package = .batch_runner_package())
    if (!nzchar(script) || !file.exists(script)) {
      stop(".batch_run(): inst/batch_worker.R not found in installed package",
        call. = FALSE)
    }
    return(script)
  }
  script <- file.path(dev_path, "inst", "batch_worker.R")
  if (!file.exists(script)) {
    stop(".batch_run(): worker script not found in dev tree: ", script,
      call. = FALSE)
  }
  script
}

#' Retain the METADATA of a failed item
#'
#' The precise, honest guarantee (the contract states this too): **the runner
#' never persists the argument VALUES it was given** -- only their names, the
#' item id and the target descriptor. Replay is by regeneration (re-run the
#' producer for that id), so retention needs no payload, which matters because
#' shape-B items are patient data and a shared box is no home for them.
#'
#' What it does NOT promise, because it cannot: the `error` field is the target's
#' OWN failure message, passed through verbatim, and a target that writes an
#' argument value into its own `stop()` (e.g. `stop(payload)`) exposes that value
#' itself. The runner cannot tell a data value apart from a legitimate diagnostic
#' in a free-form message, so it does not try -- targets handling sensitive data
#' must not embed it in errors, and the `0700`/`0600` permissions below are the
#' safeguard for whatever a target does emit.
#'
#' Two deliberate tightenings over the naive version:
#' * **The worker's stdout/stderr tail is NOT persisted.** It routinely prints
#'   whole data slices -- a far larger and more certain leak than an error string
#'   -- so it is shown transiently on the console at failure time but never
#'   written to the diagnostic file.
#' * **Permissions are enforced whether or not we created the directory.** A
#'   caller-supplied secure location must not be left world-traversable, and the
#'   record file itself is written `0600` (umask here is 0002, so relying on
#'   defaults would leave it group-readable).
#' @noRd
.batch_retain_failure <- function(dir, id, seq, target, arg_names, message) {
  if (is.null(dir)) return(invisible(NULL))
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  }
  # Fail CLOSED. Sys.chmod() returns per-path success and the mount may ignore it
  # (CIFS, etc.), so both the return AND the resulting mode are checked: if the
  # directory cannot actually be secured to 0700, write NOTHING rather than drop a
  # target-authored message into a world-readable place.
  if (!isTRUE(Sys.chmod(dir, mode = "0700", use_umask = FALSE)) ||
      !identical(file.mode(dir), as.octmode("700"))) {
    warning(sprintf(paste0(".batch retention: could not secure %s to 0700; ",
      "skipping the failure record rather than writing it insecurely."), dir),
      call. = FALSE)
    return(invisible(NULL))
  }

  record <- list(
    protocol = .BATCH_PROTOCOL,
    id = id,
    target = target[c("package", "symbol", "hash")],
    arg_names = arg_names,
    error = message
  )
  # The sequence index prefixes the filename so two ids that slug to the same
  # string cannot overwrite each other's record.
  path <- file.path(dir, sprintf("batch_failure_%06d_%s.qs2", seq, .batch_id_slug(id)))
  ok <- tryCatch(
    {
      .batch_write_envelope(record, path)
      TRUE
    },
    error = function(e) {
      warning(sprintf(".batch retention: could not write failure record to %s: %s",
        path, conditionMessage(e)), call. = FALSE)
      FALSE
    }
  )
  if (!ok) return(invisible(NULL))
  # Secure the record file too; if that cannot be done, remove it (fail closed).
  if (!isTRUE(Sys.chmod(path, mode = "0600", use_umask = FALSE)) ||
      !identical(file.mode(path), as.octmode("600"))) {
    unlink(path, force = TRUE)
    warning(sprintf(".batch retention: could not secure %s to 0600; removed it.",
      path), call. = FALSE)
    return(invisible(NULL))
  }
  invisible(path)
}

#' @noRd
.batch_id_slug <- function(id) {
  s <- gsub("[^A-Za-z0-9._-]", "_", as.character(id))
  if (!nzchar(s)) "item" else s
}

#' Derive stable per-item ids for `.batch_run` (item names, else index)
#'
#' A named item keeps its name; an unnamed one gets its 1-based index. The
#' result must be unique so a failure/retention record identifies exactly one
#' item -- a duplicate name (or a name that collides with another item's index)
#' is a caller error, not something to paper over.
#' @noRd
.batch_item_ids <- function(items) {
  n <- length(items)
  ids <- names(items)
  if (is.null(ids)) ids <- rep_len("", n)
  ids[is.na(ids)] <- ""
  blank <- !nzchar(ids)
  ids[blank] <- as.character(seq_len(n))[blank]
  if (anyDuplicated(ids)) {
    stop(sprintf(paste0(".batch_run(): item ids are not unique: %s. Name items ",
      "uniquely, or leave them all unnamed to use positional indices."),
      paste(unique(ids[duplicated(ids)]), collapse = ", ")), call. = FALSE)
  }
  ids
}

#' Validate an explicit id vector for `.batch_stream` (non-empty, non-NA, unique)
#' @noRd
.batch_check_ids <- function(ids) {
  ids <- as.character(ids)
  if (any(is.na(ids)) || any(!nzchar(ids))) {
    stop(".batch_stream(): every id must be a non-empty, non-NA string", call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    stop(".batch_stream(): ids must be unique: ",
      paste(unique(ids[duplicated(ids)]), collapse = ", "), call. = FALSE)
  }
  ids
}

#' Validate the `timeout` config -- a single positive number of seconds, or Inf
#'
#' Rejected loudly rather than silently disabled: a vector, `NA`, a non-numeric,
#' zero or a negative would otherwise either turn the timeout OFF without a word
#' (`c(1, 2)`, `NA`) or make every item time out instantly (a negative). Validate
#' before any early return so an empty workload cannot hide a bad value.
#' @noRd
.batch_validate_timeout <- function(timeout, what) {
  if (length(timeout) != 1L || !is.numeric(timeout) || is.na(timeout) ||
      timeout <= 0) {
    stop(sprintf(paste0("%s: timeout must be a single positive number of seconds ",
      "(or Inf to disable); got: %s"), what,
      paste(utils::capture.output(utils::str(timeout)), collapse = " ")),
      call. = FALSE)
  }
  as.numeric(timeout)
}

#' Validate the `collect` flag -- a single TRUE/FALSE
#' @noRd
.batch_validate_collect <- function(collect, what) {
  if (!is.logical(collect) || length(collect) != 1L || is.na(collect)) {
    stop(sprintf("%s: collect must be a single TRUE or FALSE", what), call. = FALSE)
  }
  collect
}

# --- shape A: fresh subprocess per item, via processx ------------------------

#' Run a target on each of a fixed list of items, one subprocess per item
#'
#' Shape A of the contract: the items already exist (each is ~11 KB of paths and
#' config; the worker opens its own data), so a fresh R process per item is not a
#' cost to amortise but the memory strategy itself -- s3 peaks ~20 GB/worker and
#' R does not return that to the OS, so process exit is how the memory is
#' reclaimed. Evolved from `parallel_pool()`: same hardened scheduling, log
#' handling and dev-path policy, now driving the ONE generic worker over the
#' envelope contract instead of nine hand-written dispatch scripts.
#'
#' @param target A `batch_target` descriptor from [.batch_target()].
#' @param items List of items; each a fully-named list of the target's formals.
#' @param n_workers Concurrent subprocesses (validated: finite, whole, >= 1).
#' @param dev_path Consumer-package source tree for `devtools::load_all()` in the
#'   worker, or `NULL` for the installed package. A given-but-wrong path errors.
#' @param collect If `TRUE`, return each item's value; if `FALSE`, the worker
#'   still reports status but its value never crosses back (direct-writers).
#' @param p A [progressr::progressor()], or `NULL`.
#' @param label Optional short stage tag prefixed to the progress message.
#' @param timeout Per-item wall-clock limit in seconds; a worker that exceeds it
#'   is killed and reported as a failure. Defaults to a generous hang-catcher
#'   ([.BATCH_DEFAULT_TIMEOUT]); pass `Inf` to disable.
#' @param keep_failed_dir Optional secure directory in which to retain the
#'   METADATA (never the payload) of a failed item, for diagnosis/replay.
#' @return If `collect`, a list of values in item order; else `invisible(NULL)`.
#' @noRd
.batch_run <- function(
  target,
  items,
  n_workers,
  dev_path = NULL,
  collect = TRUE,
  p = NULL,
  label = NULL,
  timeout = .BATCH_DEFAULT_TIMEOUT,
  keep_failed_dir = NULL
) {
  if (!inherits(target, "batch_target")) {
    stop(".batch_run(): `target` must come from .batch_target()", call. = FALSE)
  }
  n_workers <- .validate_n_workers(n_workers, ".batch_run()")
  # Validate ALL config BEFORE the empty-workload early return -- otherwise a bad
  # dev_path/timeout/collect is silently accepted whenever there is no work, which
  # is exactly the Phase-1 "early return skips validation" bug shape.
  collect <- .batch_validate_collect(collect, ".batch_run()")
  timeout <- .batch_validate_timeout(timeout, ".batch_run()")
  dev_path <- .batch_validate_dev_path(dev_path, target$package)
  # `items` must be a LIST of items, checked before the empty-workload return so
  # an empty atomic (character(0)/numeric(0)) cannot slip past the container
  # contract while a non-empty atomic would be rejected.
  if (!is.list(items)) {
    stop(sprintf(".batch_run(): `items` must be a list, got %s", class(items)[1L]),
      call. = FALSE)
  }

  n_items <- length(items)
  if (n_items == 0L) return(if (collect) list() else invisible(NULL))

  # Stable per-item ids (item names, else the index), validated unique so
  # failures/retention identify the right item and retention files never collide.
  ids <- .batch_item_ids(items)

  # Validate EVERY item up front (not items[[1]]): item schemas are legitimately
  # heterogeneous, so a bad one hides behind a good first one.
  for (i in seq_len(n_items)) {
    .batch_validate_item(target, items[[i]], where = "parent", id = ids[i])
  }

  script_path <- .batch_worker_script(dev_path)
  rscript_bin <- file.path(R.home("bin"), "Rscript")

  input_paths <- vapply(seq_len(n_items), function(i) {
    tempfile(pattern = paste0("batch_in_", i, "_"), fileext = ".qs2")
  }, character(1))
  output_paths <- vapply(seq_len(n_items), function(i) {
    tempfile(pattern = paste0("batch_out_", i, "_"), fileext = ".qs2")
  }, character(1))
  # Per-item stdout/stderr goes to a file, not a pipe -- the pipe's fixed OS
  # buffer is what used to deadlock a chatty worker. "Bounded" here is about RAM:
  # only the last 64 KB is ever read back (.pp_log_tail), so a huge log never OOMs
  # the PARENT. The on-disk file is transient (unlinked per item) and its size is
  # bounded in practice by `timeout` (write-rate x wall-clock); a truly
  # pathological infinite-printer is caught by that, not by an fs-level cap.
  log_paths <- vapply(seq_len(n_items), function(i) {
    tempfile(pattern = paste0("batch_log_", i, "_"), fileext = ".log")
  }, character(1))

  on.exit({
    unlink(input_paths, force = TRUE)
    unlink(output_paths, force = TRUE)
    unlink(log_paths, force = TRUE)
  }, add = TRUE)

  # --vanilla does not reproduce the parent's library path, and .libPaths cannot
  # travel in the payload (the child needs qs2 to READ the payload). Force it via
  # R_LIBS before startup. "current" keeps the rest of the environment inherited.
  worker_env <- c(
    "current",
    R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)
  )

  for (i in seq_len(n_items)) {
    envelope <- list(
      protocol = .BATCH_PROTOCOL,
      meta = list(
        package = target$package,
        symbol = target$symbol,
        version = target$version,
        hash = target$hash,
        dev_path = dev_path,
        runner_package = .batch_runner_package(),
        id = ids[i],
        collect = collect
      ),
      args = items[[i]]
    )
    .batch_write_envelope(envelope, input_paths[i])
  }

  active <- list()
  n_done <- 0L
  next_item <- 1L
  results <- if (collect) vector("list", n_items) else NULL

  on.exit({
    for (entry in active) {
      tryCatch(entry$proc$kill_tree(), error = function(e) NULL)
    }
  }, add = TRUE, after = FALSE)

  if (is.null(p)) message(sprintf("  [0/%d] dispatching workers...", n_items))

  .launch <- function(idx) {
    proc <- processx::process$new(
      command = rscript_bin,
      args = c("--vanilla", script_path, input_paths[idx], output_paths[idx]),
      stdout = log_paths[idx],
      stderr = "2>&1",
      env = worker_env,
      cleanup_tree = TRUE
    )
    list(proc = proc, idx = idx, started = Sys.time())
  }

  # A worker failed -- surface its log tail and the retained-metadata note, then
  # stop. One place, so every failure path (nonzero exit, missing/unreadable
  # envelope, error status, timeout) reports the same way.
  .fail <- function(entry, what) {
    idx <- entry$idx
    tail_txt <- .pp_log_tail(log_paths[idx])
    if (!is.null(keep_failed_dir)) {
      .batch_retain_failure(
        keep_failed_dir, ids[idx], idx, target,
        names(items[[idx]]), what
      )
    }
    if (nzchar(trimws(tail_txt))) {
      message(sprintf(
        "\n--- item '%s' failed ---\nOUTPUT (stdout+stderr):\n%s\n---",
        ids[idx], tail_txt))
    }
    stop(sprintf(".batch_run(): item '%s' %s", ids[idx], what), call. = FALSE)
  }

  # Read + validate one finished item's result envelope while its log is still on
  # disk. A zero exit status is not a result: the worker can exit 0 having
  # written nothing (killed after opening the file), or the target can have
  # returned an error envelope. Both are failures here.
  .collect <- function(entry) {
    idx <- entry$idx
    exit_status <- entry$proc$get_exit_status()
    if (!is.null(exit_status) && exit_status != 0L) {
      .fail(entry, sprintf("worker exited %d before writing a result", exit_status))
    }
    path <- output_paths[idx]
    if (!file.exists(path)) {
      .fail(entry, sprintf("produced no result envelope: %s", path))
    }
    envelope <- tryCatch(
      .batch_read_envelope(path),
      error = function(e) {
        .fail(entry, sprintf("wrote an unreadable result envelope (%s): %s",
          path, conditionMessage(e)))
      }
    )
    insp <- .batch_inspect_result(envelope, ids[idx], target)
    if (!insp$ok) .fail(entry, insp$reason)
    .batch_surface_warnings(insp$warnings, ids[idx])
    insp$value
  }

  repeat {
    while (length(active) < n_workers && next_item <= n_items) {
      active[[length(active) + 1L]] <- .launch(next_item)
      next_item <- next_item + 1L
    }
    if (length(active) == 0L) break

    still_active <- list()
    for (entry in active) {
      if (!entry$proc$is_alive()) {
        value <- .collect(entry)
        # results[idx] <- list(value), NOT results[[idx]] <- value: assigning a
        # NULL value with [[<- DELETES the element, shortening the list and
        # shifting every result gathered after it. Completion is in worker-finish
        # order, so a NULL item finishing after a higher slot is filled corrupts
        # positions. Single-bracket-with-list() assigns the NULL in place.
        if (collect) results[entry$idx] <- list(value)
        unlink(log_paths[entry$idx], force = TRUE)
        n_done <- n_done + 1L
        if (!is.null(p)) {
          ts <- format(Sys.time(), "%H:%M:%S")
          p(message = if (is.null(label)) ts else paste(label, ts))
        } else if (n_done == n_items || n_done %% max(1L, n_items %/% 20L) == 0L) {
          message(sprintf("  [%d/%d] complete  %s",
            n_done, n_items, format(Sys.time(), "%H:%M:%S")))
        }
      } else if (is.finite(timeout) &&
                 as.numeric(difftime(Sys.time(), entry$started, units = "secs")) > timeout) {
        tryCatch(entry$proc$kill_tree(), error = function(e) NULL)
        .fail(entry, sprintf("exceeded the %g s timeout and was killed", timeout))
      } else {
        still_active[[length(still_active) + 1L]] <- entry
      }
    }
    active <- still_active

    if (length(active) > 0L) Sys.sleep(0.1)
  }

  if (collect) results else invisible(NULL)
}

# --- shape B: lazy producer, bounded queue, via mirai ------------------------

#' Stream a producer's items through a target under backpressure
#'
#' Shape B of the contract: the parent IS the producer. Each item is generated
#' lazily by `producer(id)` and is itself the payload (a data slice), so it must
#' NOT be materialised until there is a worker ready for it -- otherwise the whole
#' dataset lands in memory (or on disk twice) at once. mirai's persistent daemons
#' and in-memory transport are exactly this shape; `.batch_run`'s
#' materialise-every-item-to-a-tempfile model is exactly the wrong one for it.
#'
#' Same contract as [.batch_run()] -- target descriptor, both-end validation,
#' result envelope inspection ([.batch_inspect_result()]), warning surfacing,
#' metadata-only retention and loud failure -- over a different transport. At most
#' `2 * n_workers` items are in flight; `producer(id)` for the next id is not
#' called until an in-flight slot frees, which is the backpressure. Each task
#' carries a `timeout`, so a wedged daemon cannot block `call_mirai()` forever.
#'
#' Never touches mirai's DEFAULT compute profile: `daemons(n)` there would reset
#' and destroy any daemon configuration the caller had (defect #2). It claims its
#' own named `compute` profile, refuses to run if that profile is ALREADY active
#' (so it never hijacks a profile the caller owns), and tears only its own down.
#'
#' @param target A `batch_target` descriptor from [.batch_target()].
#' @param ids Vector of stable item ids (non-empty, non-NA, unique). Length =
#'   number of items; order is the order of production and of the results.
#' @param producer `function(id)` returning that item -- a fully-named list of
#'   the target's formals. Called once per id, in the parent, under backpressure.
#' @param n_workers Number of mirai daemons (validated).
#' @param dev_path Consumer-package source tree, loaded once per daemon via
#'   `devtools::load_all()`, or `NULL` for the installed package. A given-but-
#'   wrong path errors, even for an empty workload.
#' @param collect If `TRUE`, return each item's value (named by id, in id order);
#'   if `FALSE`, the daemon reports status but no value crosses back.
#' @param p A [progressr::progressor()], or `NULL`.
#' @param label Optional short stage tag prefixed to the progress message.
#' @param timeout Per-item wall-clock limit in seconds (generous default;
#'   `Inf` disables). A task exceeding it resolves to an error and is reported.
#' @param keep_failed_dir Optional secure directory for METADATA-only retention
#'   of a failed item (never the payload), matching [.batch_run()].
#' @param compute mirai compute-profile name. Must never be the default profile.
#' @return If `collect`, a named list of values in id order; else
#'   `invisible(NULL)`.
#' @noRd
.batch_stream <- function(
  target,
  ids,
  producer,
  n_workers,
  dev_path = NULL,
  collect = TRUE,
  p = NULL,
  label = NULL,
  timeout = .BATCH_DEFAULT_TIMEOUT,
  keep_failed_dir = NULL,
  compute = "swereg_batch_stream"
) {
  if (!inherits(target, "batch_target")) {
    stop(".batch_stream(): `target` must come from .batch_target()", call. = FALSE)
  }
  if (!is.function(producer)) {
    stop(".batch_stream(): `producer` must be a function of one id", call. = FALSE)
  }
  n_workers <- .validate_n_workers(n_workers, ".batch_stream()")
  if (!is.character(compute) || length(compute) != 1L || is.na(compute) ||
      !nzchar(compute)) {
    stop(".batch_stream(): `compute` must be a single non-empty, non-NA profile name",
      call. = FALSE)
  }
  compute <- unname(compute)  # a name on the vector must not smuggle a value past checks
  # NEVER the default profile: daemons(n)/daemons(0) there resets and destroys
  # whatever the caller had (defect #2). Its name is "default"; check the VALUE,
  # so `c(alias = "default")` cannot slip through.
  if (identical(compute, "default")) {
    stop(".batch_stream(): `compute` must not be mirai's default profile", call. = FALSE)
  }
  # Validate ALL config BEFORE the empty-workload early return (Phase-1 lesson).
  ids <- .batch_check_ids(ids)
  collect <- .batch_validate_collect(collect, ".batch_stream()")
  timeout <- .batch_validate_timeout(timeout, ".batch_stream()")
  dev_path <- .batch_validate_dev_path(dev_path, target$package)
  runner_pkg <- .batch_runner_package()

  n <- length(ids)
  if (n == 0L) return(if (collect) list() else invisible(NULL))
  if (!requireNamespace("mirai", quietly = TRUE)) {
    stop(".batch_stream() requires the 'mirai' package", call. = FALSE)
  }

  # Refuse to hijack a profile the caller already CONFIGURED -- daemons(n) would
  # reset and later destroy their configuration. `daemons_set()` is mirai's
  # documented, side-effect-free ownership predicate: TRUE iff the profile has
  # daemons configured, INDEPENDENT of live connection count (so a configured-but-
  # unconnected profile is caught). This is deliberately used instead of the
  # shape of status()$daemons -- mirai's own package-author guidance says not to
  # depend on status() shape, and that shape varied across releases (a daemon
  # matrix on older dispatchers), which both broke detection and, on <0.9.1, did
  # not exist at all. FAIL CLOSED if the predicate cannot be evaluated.
  already <- tryCatch(mirai::daemons_set(.compute = compute),
    error = function(e) structure(list(msg = conditionMessage(e)),
      class = "batch_ds_error"))
  if (inherits(already, "batch_ds_error")) {
    stop(sprintf(paste0(".batch_stream(): could not verify mirai profile '%s' is ",
      "free (%s); refusing to claim it. Pass a different `compute`."),
      compute, already$msg), call. = FALSE)
  }
  if (isTRUE(already)) {
    stop(sprintf(paste0(".batch_stream(): mirai compute profile '%s' is already ",
      "configured; refusing to hijack it. Pass a different `compute`."),
      compute), call. = FALSE)
  }

  mirai::daemons(n_workers, .compute = compute)
  on.exit(mirai::daemons(0L, .compute = compute), add = TRUE)

  # Load the consumer (and, once extracted, the runner) package ONCE per
  # persistent daemon -- not per task. The daemon needs .batch_execute resolvable
  # in the runner's namespace.
  if (is.null(dev_path)) {
    mirai::everywhere(
      {
        requireNamespace(.consumer, quietly = TRUE)
        if (!identical(.runner, .consumer)) requireNamespace(.runner, quietly = TRUE)
      },
      .consumer = target$package, .runner = runner_pkg, .compute = compute
    )
  } else {
    mirai::everywhere(
      suppressPackageStartupMessages(devtools::load_all(.dev, quiet = TRUE)),
      .dev = dev_path, .compute = compute
    )
  }

  # Double the workers, capped at the id count. Deliberately `2 * n_workers`
  # (double), NOT `2L * n_workers`: integer multiplication OVERFLOWS to NA for a
  # validated-but-absurd worker count near .Machine$integer.max, and NA in the
  # `length(inflight) >= max_inflight` guard would error. Double arithmetic can't
  # overflow here; the min() with n bounds it.
  max_inflight <- min(2 * n_workers, n)
  task_timeout_ms <- if (length(timeout) == 1L && is.finite(timeout)) {
    timeout * 1000
  } else {
    NULL
  }
  results <- if (collect) vector("list", n) else NULL
  inflight <- list()
  n_done <- 0L

  .stream_fail <- function(item, reason) {
    if (!is.null(keep_failed_dir)) {
      .batch_retain_failure(keep_failed_dir, item$id, item$pos, target,
        item$arg_names, reason)
    }
    stop(sprintf(".batch_stream(): id '%s' %s", item$id, reason), call. = FALSE)
  }

  # Drain the OLDEST in-flight task (FIFO). Two failure channels, identical in
  # spirit to .batch_run: a daemon-level error value (the task expression itself
  # blew up, the package would not load, or the per-task timeout fired) and a
  # target-level error envelope, both routed through the shared inspector.
  drain_one <- function() {
    item <- inflight[[1L]]
    v <- mirai::call_mirai(item$h)$data
    if (mirai::is_error_value(v)) {
      .stream_fail(item, sprintf("daemon/timeout error: %s", as.character(v)))
    }
    insp <- .batch_inspect_result(v, item$id, target)
    if (!insp$ok) .stream_fail(item, insp$reason)
    .batch_surface_warnings(insp$warnings, item$id)
    # results[pos] <- list(value), not [[<-: the same NULL-deletion trap as
    # .batch_run -- a target returning NULL must keep its slot, not vanish.
    if (collect) results[item$pos] <<- list(insp$value)
    inflight[[1L]] <<- NULL
    n_done <<- n_done + 1L
    if (!is.null(p)) {
      p(message = if (is.null(label)) as.character(item$id) else paste(label, item$id))
    }
  }

  for (i in seq_len(n)) {
    # Backpressure: block the producer until an in-flight slot frees. This is why
    # shape B does not blow up memory -- producer(id) is not even called until
    # there is somewhere to put its result.
    while (length(inflight) >= max_inflight) drain_one()

    id <- ids[[i]]
    args <- producer(id)
    .batch_validate_item(target, args, where = "parent", id = id)
    envelope <- list(
      protocol = .BATCH_PROTOCOL,
      meta = list(
        package = target$package,
        symbol = target$symbol,
        version = target$version,
        hash = target$hash,
        dev_path = dev_path,
        runner_package = runner_pkg,
        id = as.character(id),
        collect = collect
      ),
      args = args
    )
    h <- mirai::mirai(
      {
        get(".batch_execute", envir = asNamespace(.runner))(.env)
      },
      .env = envelope, .runner = runner_pkg, .compute = compute,
      .timeout = task_timeout_ms
    )
    # Retain only the argument NAMES for a possible failure record -- never the
    # produced values, which for shape B are patient data.
    inflight[[length(inflight) + 1L]] <- list(
      id = id, pos = i, h = h, arg_names = names(args)
    )
  }

  while (length(inflight) > 0L) drain_one()

  if (collect) {
    names(results) <- as.character(ids)
    results
  } else {
    invisible(NULL)
  }
}
