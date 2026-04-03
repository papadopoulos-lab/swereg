# Worker: TTE pipeline pass 1b (full enrollment)
# Usage: Rscript --vanilla worker_s1b.R <input_path> <output_path>

this_script <- grep("--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("--file=", "", this_script)), "worker_bootstrap.R"))

enrolled_ids <- swereg::qs2_read(params$enrolled_ids_path)

result <- swereg:::.s1b_worker(
  enrollment_spec = params$enrollment_spec,
  file_path       = params$file_path,
  spec            = params$spec,
  enrolled_ids    = enrolled_ids,
  cache_path      = params$cache_path
)

qs2::qs_save(result, args[2L])
