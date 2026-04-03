# Worker: TTE pipeline pass 1b (full enrollment)
# Args: <bootstrap.R> <input.qs2> <output.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

enrolled_ids <- swereg::qs2_read(params$enrolled_ids_path)

result <- swereg:::.s1b_worker(
  enrollment_spec = params$enrollment_spec,
  file_path       = params$file_path,
  spec            = params$spec,
  enrolled_ids    = enrolled_ids,
  cache_path      = params$cache_path
)

qs2::qs_save(result, args[3L])
