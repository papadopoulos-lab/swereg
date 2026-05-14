# Worker: TTE pipeline pass 1a (multi-enrollment scout, Lever 2)
# Reads one canonical skeleton ONCE, then iterates over all enrollment_specs
# to compute tuples + attrition + per-enrollment cache for each. Replaces
# N separate worker_s1a.R calls (one per enrollment) with a single call,
# saving N-1 canonical reads per skeleton.
#
# Args: <bootstrap.R> <input.qs2> <output.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

result <- swereg:::.s1a_worker_multi(
  file_path             = params$file_path,
  enrollment_specs      = params$enrollment_specs,
  spec                  = params$spec,
  cache_paths           = params$cache_paths,
  scout_checkpoint_path = params$scout_checkpoint_path
)

qs2::qs_save(result, args[3L])
