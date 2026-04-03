# Shared bootstrap for swereg worker scripts.
# Called with: Rscript --vanilla <worker.R> <bootstrap.R> <input.qs2> [<output.qs2>]
# After sourcing, provides: params (list), args (character vector).

params <- qs2::qs_read(args[2L])

data.table::setDTthreads(params$n_threads)
suppressPackageStartupMessages({
  if (!is.null(params$swereg_dev_path) && dir.exists(params$swereg_dev_path)) {
    devtools::load_all(params$swereg_dev_path, quiet = TRUE)
  } else {
    requireNamespace("swereg", quietly = TRUE)
  }
})
