# Worker: TTE pipeline pass 1a (scout)
# Args: <bootstrap.R> <input.qs2> <output.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

result <- swereg:::.s1a_worker(
  enrollment_spec = params$enrollment_spec,
  file_path       = params$file_path,
  spec            = params$spec,
  cache_path      = params$cache_path
)

qs2::qs_save(result, args[3L])
