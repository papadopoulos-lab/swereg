# Phase 19: try TE with glm_function = "parglm". Despite the docs saying
# "deprecated", the argument is still in initiators(). parglm is on CRAN.

.libPaths(c("/home/raw996/R/x86_64-pc-linux-gnu-library/4.6", .libPaths()))
library(TrialEmulation)
library(data.table)
library(parglm)

set.seed(2026)
N <- 50000
T <- 30
TRUE_LOR <- -0.5

L0 <- rnorm(N, 0, 1)
period_list <- vector("list", T)
prev_A <- integer(N)
for (t in 0:(T - 1)) {
  logit_A <- if (t == 0) -0.3 + 0.6 * L0 else -3.0 + 0.6 * L0 + 6.0 * prev_A
  At_val <- rbinom(N, 1, plogis(logit_A))
  logit_Y <- -3.5 + TRUE_LOR * At_val + 0.4 * L0
  Yt_val <- rbinom(N, 1, plogis(logit_Y))
  period_list[[t + 1L]] <- data.table(id = 1:N, period = t, L0 = L0,
                                       A_t = At_val, Y_t = Yt_val)
  prev_A <- At_val
}
dt <- rbindlist(period_list)
setorder(dt, id, period)

te_in <- copy(dt)
setnames(te_in, c("A_t", "Y_t"), c("treatment", "outcome"))
te_in[, eligible := as.integer(period == 0)]

cat("=== TE with glm_function = 'glm' (default) ===\n")
t0 <- Sys.time()
res_glm <- initiators(
  data = te_in, id = "id", period = "period", eligible = "eligible",
  treatment = "treatment", estimand_type = "PP", outcome = "outcome",
  model_var = "assigned_treatment", outcome_cov = c("L0"),
  switch_n_cov = ~ L0, switch_d_cov = ~ L0,
  use_censor_weights = FALSE,
  glm_function = "glm",
  quiet = TRUE
)
glm_time <- as.numeric(Sys.time() - t0, units = "secs")
cat("elapsed:", round(glm_time, 1), "s\n")

cat("\n=== TE with glm_function = 'parglm' ===\n")
t0 <- Sys.time()
res_pglm <- tryCatch(
  initiators(
    data = te_in, id = "id", period = "period", eligible = "eligible",
    treatment = "treatment", estimand_type = "PP", outcome = "outcome",
    model_var = "assigned_treatment", outcome_cov = c("L0"),
    switch_n_cov = ~ L0, switch_d_cov = ~ L0,
    use_censor_weights = FALSE,
    glm_function = "parglm",
    nthreads = 4,   # parglm-specific
    quiet = TRUE
  ),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
pglm_time <- as.numeric(Sys.time() - t0, units = "secs")
cat("elapsed:", round(pglm_time, 1), "s\n")

cat("\n========================================================\n")
cat(sprintf("%-30s %8s  %8s\n", "method", "time", "estimate"))
trt <- res_glm$robust$summary[res_glm$robust$summary$names == "assigned_treatment", ]
cat(sprintf("%-30s %6.1f s  %8.4f\n", "glm", glm_time, trt$estimate))
if (!is.null(res_pglm)) {
  trt <- res_pglm$robust$summary[res_pglm$robust$summary$names == "assigned_treatment", ]
  cat(sprintf("%-30s %6.1f s  %8.4f\n", "parglm (4 threads)", pglm_time, trt$estimate))
  cat(sprintf("speedup: %.2fx\n", glm_time / pglm_time))
}
