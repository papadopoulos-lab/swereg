# Worker: TTE pipeline sub-step s1b (match, single subprocess per enrollment)
# Pools per-skeleton scout outputs for one enrollment, samples comparators
# at the matching ratio, writes enrolled_ids + attrition + sentinel.
#
# Args: <bootstrap.R> <input.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

swereg:::.s1b_worker(
  enrollment_spec        = params$enrollment_spec,
  spec                   = params$spec,
  work_dir               = params$work_dir,
  skel_basenames         = params$skel_basenames,
  enrollment_counts_path = params$enrollment_counts_path
)
