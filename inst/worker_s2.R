# Worker: TTE pipeline pass 2 (IPCW-PP)
# Args: <bootstrap.R> <input.qs2>

args <- commandArgs(trailingOnly = TRUE)
source(args[1L])

swereg:::.s2_worker(
  outcome            = params$outcome,
  follow_up          = params$follow_up,
  file_imp_path      = params$file_imp_path,
  file_analysis_path = params$file_analysis_path,
  n_threads          = params$n_threads,
  sep_by_exp         = params$sep_by_exp,
  with_gam           = params$with_gam
)
