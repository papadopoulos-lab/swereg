# Worker: TTE pipeline sub-step s1a (multi-enrollment scout)
# Reads one canonical skeleton ONCE, then iterates over all enrollment_specs
# to compute tuples + attrition + per-enrollment cache for each, writing
# results to disk per (enrollment, skeleton). No payload returned to master
# (parallel_pool called with collect = FALSE).
#
# Args: <bootstrap.R> <input.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

swereg:::.s1a_worker_multi(
  file_path        = params$file_path,
  enrollment_specs = params$enrollment_specs,
  spec             = params$spec,
  work_dir         = params$work_dir
)
