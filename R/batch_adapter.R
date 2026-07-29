# swereg is a thin ADAPTER over batchit's dispatchers (PROJECT.md, Phase 4
# step 3). The engine -- the target descriptor, both-ends validation, the result
# envelope, the processx/mirai transports, and the one generic worker script --
# lives in `batchit`; swereg keeps only the dispatch POLICY, at the call
# sites in R/r6_tteplan.R and R/r6_registrystudy.R: WHICH targets to run, the
# thread policy (.threads_per_worker), the dev-path selection (.swereg_dev_path),
# and the stable item ids.
#
# batchit 26.7.20 renamed its public surface (PUBLIC_API.md, "naming v2"):
#   batch_target(...)  -> package_function(...)
#   batch_run(...)      -> run(...) / run_and_collect(...) (split by `collect`)
#   batch_stream(...)   -> stream_from_parent_and_write_files_atomically(...)
#   batch_stage_path(...) -> where_to_write_output(...)
# and every one of them now takes the descriptor as `fn =`, not `target =`.
#
# These five wrappers exist for exactly two reasons and do nothing else:
#   * every call site and test keeps referring to the internal names
#     .batch_target / .batch_run / .batch_run_and_write / .batch_stream /
#     .batch_where_to_write_output,
#     so nothing downstream had to change when the engine moved or was renamed;
#     and
#   * a test can still swap the dispatcher out with
#     `testthat::local_mocked_bindings(.batch_run = ..., .package = "swereg")` --
#     which needs a binding of that name to exist in swereg's namespace.
#
# `.batch_where_to_write_output` exists so a `style = "staged_writer"` target
# (e.g. `.rawbatch_write_worker`, R/r6_registrystudy.R) can resolve its output
# path without naming `batchit::` itself -- test-batch_lockdown.R enforces
# that batchit is named ONLY from this file.
#
# `.batch_run`'s `collect` is NOT uniformly FALSE: most call sites (s1a-d, s2,
# process_skeletons) dispatch fire-and-forget with `collect = FALSE`, but the
# s3 enrollment/ETT loops omit `collect` entirely (default `TRUE`) because they
# need the per-item return values back (`results_enrollment`/`results_ett`).
# So `.batch_run` absorbs `target`->`fn` and dispatches to whichever of
# batchit's two no-`collect`-formal frontends matches the caller's `collect`,
# rather than assuming one direction. `.batch_stream`'s one call site always
# passes `collect = FALSE` (or omits it, same default here), so it maps
# 1:1 onto stream_from_parent_and_write_files_atomically().
#
# `.batch_run_and_write` is the ATOMIC-COMMIT dispatcher: the caller declares
# every file an item will write via `outputs`, batchit runs the item against a
# temporary staging name, and renames the whole set into place only once every
# declared output has been written. A crashed or half-finished item therefore
# leaves NO partial file behind for an incremental rebuild to mistake for a
# finished one. Two contract points the call sites must respect:
#   * batchit REJECTS a relative declared-output path, so every path handed to
#     `outputs` must be absolute (see `out_abs` / `.s1_work_dir()` in
#     R/r6_tteplan.R); and
#   * run_and_write_files_atomically() has NO `...` of its own -- its formals
#     are exactly fn/items/outputs/style/n_workers/dev_path/p/label/timeout.
#     So an unknown argument name passed through this wrapper's `...` errors
#     rather than being silently dropped, which is wanted. `target` is a live
#     DEPRECATED alias for `fn` in batchit and passing both errors, so this
#     wrapper forwards its own `target` as `fn =` and never as `target =`.

#' @noRd
.batch_target <- function(package, symbol, version = NULL) {
  batchit::package_function(package, symbol, version)
}

#' @noRd
.batch_run <- function(target, ..., collect = TRUE) {
  if (isTRUE(collect)) {
    batchit::run_and_collect(fn = target, ...)
  } else if (isFALSE(collect)) {
    batchit::run(fn = target, ...)
  } else {
    stop(".batch_run(): `collect` must be TRUE or FALSE", call. = FALSE)
  }
}

#' @noRd
.batch_run_and_write <- function(target, ..., style = "return") {
  batchit::run_and_write_files_atomically(fn = target, style = style, ...)
}

#' @noRd
.batch_stream <- function(target, ..., collect = FALSE) {
  stopifnot(isFALSE(collect))
  batchit::stream_from_parent_and_write_files_atomically(fn = target, ...)
}

#' @noRd
.batch_where_to_write_output <- function(name) batchit::where_to_write_output(name)
