# Worker: TTE pipeline loop 3a (enrollment-level baseline analysis)
# Args: <bootstrap.R> <input.qs2> <output.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

result <- swereg:::.s3_enrollment_worker(
  analysis_path = params$analysis_path,
  raw_path      = params$raw_path,
  enrollment_id = params$enrollment_id,
  n_threads     = params$n_threads
)

qs2::qs_save(result, args[3L])
