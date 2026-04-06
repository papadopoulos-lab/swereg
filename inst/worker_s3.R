# Worker: TTE pipeline loop 3b (ETT-level analysis)
# Args: <bootstrap.R> <input.qs2> <output.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

result <- swereg:::.s3_ett_worker(
  analysis_path = params$analysis_path,
  ett_id        = params$ett_id,
  enrollment_id = params$enrollment_id,
  description   = params$description,
  n_threads     = params$n_threads
)

qs2::qs_save(result, args[3L])
