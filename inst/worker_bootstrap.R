# Shared bootstrap for swereg worker scripts.
# Sets up: params (list from input qs2), args (command-line arguments).

args <- commandArgs(trailingOnly = TRUE)
params <- qs2::qs_read(args[1L])

data.table::setDTthreads(params$n_threads)
suppressPackageStartupMessages({
  if (!is.null(params$swereg_dev_path) && dir.exists(params$swereg_dev_path)) {
    devtools::load_all(params$swereg_dev_path, quiet = TRUE)
  } else {
    requireNamespace("swereg", quietly = TRUE)
  }
})
