# Worker: TTE pipeline pass 1a (scout)
# Usage: Rscript --vanilla worker_s1a.R <input_path> <output_path>

this_script <- grep("--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("--file=", "", this_script)), "worker_bootstrap.R"))

result <- swereg:::.s1a_worker(
  enrollment_spec = params$enrollment_spec,
  file_path       = params$file_path,
  spec            = params$spec,
  cache_path      = params$cache_path
)

qs2::qs_save(result, args[2L])
