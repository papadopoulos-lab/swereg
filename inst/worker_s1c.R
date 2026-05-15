# Worker: TTE pipeline sub-step s1c (panel build, parallel per
# (enrollment, skeleton)). Reads s1a cache + s1b enrolled_ids, builds the
# trial-week panel, writes panel chunk + sentinel.
#
# Args: <bootstrap.R> <input.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

swereg:::.s1c_worker(
  enrollment_spec = params$enrollment_spec,
  file_path       = params$file_path,
  spec            = params$spec,
  work_dir        = params$work_dir
)
