# Worker: TTE pipeline loop 3b (single ETT analysis call)
# Args: <bootstrap.R> <input.qs2> <output.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

result <- swereg:::.s3_ett_worker(
  analysis_path = params$analysis_path,
  method        = params$method,
  weight_col    = params$weight_col,
  ett_id        = params$ett_id,
  n_threads     = params$n_threads
)

qs2::qs_save(result, args[3L])
