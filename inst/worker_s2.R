# Worker: TTE pipeline pass 2 (IPCW-PP)
# Usage: Rscript --vanilla worker_s2.R <input_path>

this_script <- grep("--file=", commandArgs(FALSE), value = TRUE)
source(file.path(dirname(sub("--file=", "", this_script)), "worker_bootstrap.R"))

swereg:::.s2_worker(
  outcome            = params$outcome,
  follow_up          = params$follow_up,
  file_imp_path      = params$file_imp_path,
  file_analysis_path = params$file_analysis_path,
  n_threads          = params$n_threads,
  sep_by_exp         = params$sep_by_exp,
  with_gam           = params$with_gam
)
