# swereg is a thin ADAPTER over batchit's one dispatcher (PROJECT.md, Phase 4
# step 3). The engine -- the target descriptor, both-ends validation, the result
# envelope, the processx/mirai transports, and the one generic worker script --
# now lives in `batchit`; swereg keeps only the dispatch POLICY, at the call
# sites in R/r6_tteplan.R and R/r6_registrystudy.R: WHICH targets to run, the
# thread policy (.threads_per_worker), the dev-path selection (.swereg_dev_path),
# and the stable item ids.
#
# These three wrappers exist for exactly two reasons and do nothing else:
#   * every call site and test keeps referring to the internal names
#     .batch_target / .batch_run / .batch_stream, so nothing downstream had to
#     change when the engine moved; and
#   * a test can still swap the dispatcher out with
#     `testthat::local_mocked_bindings(.batch_run = ..., .package = "swereg")` --
#     which needs a binding of that name to exist in swereg's namespace.

#' @noRd
.batch_target <- function(package, symbol, version = NULL) batchit::batch_target(package, symbol, version)

#' @noRd
.batch_run <- function(...) batchit::batch_run(...)

#' @noRd
.batch_stream <- function(...) batchit::batch_stream(...)
