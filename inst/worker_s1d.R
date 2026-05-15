# Worker: TTE pipeline sub-step s1d (post, single subprocess per enrollment)
# Pools per-(enrollment, skeleton) panel chunks, imputes, computes IPW,
# truncates, saves file_raw + file_imp + sentinel.
#
# Args: <bootstrap.R> <input.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

swereg:::.s1d_worker(
  enrollment_spec = params$enrollment_spec,
  spec            = params$spec,
  work_dir        = params$work_dir,
  skel_basenames  = params$skel_basenames,
  file_raw_path   = params$file_raw_path,
  file_imp_path   = params$file_imp_path,
  impute_fn       = params$impute_fn,
  stabilize       = params$stabilize
)
