# Regenerates vignettes/tte-validation-evidence.rds -- the hard-number artifact
# rendered as tables in vignette("tte-methods") section 3. Every cell is rerun
# through the SAME DGP/truth/fit helpers the testthat suite uses
# (tests/testthat/helper-tte_*.R), so the vignette numbers cannot drift from
# what the tests enforce. Rerun after any estimator change:
#   Rscript dev/generate_validation_evidence.R --max-workers=6
# `--max-workers=N` caps the forked workers of every step (each step asks for
# 3 to 10). Without it, the cap is the number of cores of the machine.
#
# `$meta$estimator_hash` is val_estimator_hash() over the estimator source
# files. test-validation-full.R fails when it differs from the current source,
# and test-validation-evidence-version.R fails when `$meta$swereg` differs from
# DESCRIPTION.

library(data.table)
devtools::load_all(".")
options(swereg.warn_prevalent_user = FALSE)
for (f in list.files("tests/testthat", "^helper-", full.names = TRUE)) {
  source(f)
}
EVIDENCE_PATH <- "vignettes/tte-validation-evidence.rds"
MAX_WORKERS <- local({
  arg <- grep("^--max-workers=", commandArgs(trailingOnly = TRUE), value = TRUE)
  n <- if (length(arg) > 0L) {
    suppressWarnings(as.integer(sub("^--max-workers=", "", arg[[1L]])))
  } else {
    parallel::detectCores()
  }
  if (is.na(n) || n < 1L) {
    stop("--max-workers must be a positive integer", call. = FALSE)
  }
  n
})

ev <- list(
  meta = list(
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    swereg = as.character(utils::packageVersion("swereg")),
    estimator_hash = val_estimator_hash("."),
    trialemulation = as.character(utils::packageVersion("TrialEmulation")),
    r = R.version.string
  )
)

# PART 0 -- EXACT-TRUTH VALIDATION (the fast and full test tiers)==============
# The cells of test-validation-fast.R and test-validation-full.R: s1 to s4,
# both estimands, 20 replicates at seeds 2100 + r, N = 20,000, truncated and
# untruncated weights. Each replicate holds the log-IRR and the risk
# difference at h = 5, 10 and 20; the truths are exact (val_truth()).
R_VAL <- 20L
val_cells <- CJ(scenario = c("s1", "s2", "s3", "s4"), estimand = c("pp", "itt"))
ev$validation_truth <- rbindlist(lapply(seq_len(nrow(val_cells)), function(i) {
  g <- val_cells[i]
  tr <- val_truth(g$scenario, g$estimand)
  tr[, `:=`(
    scenario = g$scenario,
    estimand = g$estimand,
    truth_uncensored = c(
      as.numeric(scen_truth_irr_exact(g$scenario, g$estimand, "uncensored")),
      rep(NA_real_, length(.VAL_HORIZONS))
    )
  )]
  tr[]
}))
ev$validation_reps <- rbindlist(lapply(seq_len(nrow(val_cells)), function(i) {
  g <- val_cells[i]
  val_replicates(
    g$scenario,
    g$estimand,
    R = R_VAL,
    weights = c("truncated", "untruncated"),
    cores = val_cores(min(6L, MAX_WORKERS))
  )
}))
ev$validation_summary <- rbindlist(lapply(seq_len(nrow(val_cells)), function(i) {
  g <- val_cells[i]
  val_summary(ev$validation_reps[
    scenario == g$scenario & estimand == g$estimand
  ])
}))
ev$validation_summary
saveRDS(ev, EVIDENCE_PATH, version = 2)

## risk-difference coverage: 200 replicates x 200 bootstrap replicates ====
# The coverage cells of test-validation-full.R: per-protocol in s1, s2 and
# s4, seeds 3100 + r, untruncated weights.
R_COV <- 200L
cov_cells <- list(
  list(scenario = "s1", weight = "untruncated"),
  list(scenario = "s2", weight = "untruncated"),
  list(scenario = "s4", weight = "untruncated")
)
ev$rd_coverage_reps <- rbindlist(lapply(cov_cells, function(cl) {
  val_rd_coverage(
    cl$scenario,
    "pp",
    R = R_COV,
    n_boot = 200L,
    seed0 = 3100L,
    weight = cl$weight,
    cores = val_cores(min(6L, MAX_WORKERS))
  )[, weight := cl$weight]
}))
ev$rd_coverage <- ev$rd_coverage_reps[,
  .(R = .N, covered_n = sum(covered), coverage = mean(covered)),
  by = .(scenario, estimand, weight, h)
]
ev$rd_coverage
saveRDS(ev, EVIDENCE_PATH, version = 2)

# PART 1 -- CROSS-PACKAGE TRIANGLE (truth vs swereg vs TrialEmulation)=========
# The three escalating scenarios of test-tte_validation_matrix.R, both
# estimands, N = 20000, seed 2026 (the suite's exact configuration).
tri <- parallel::mclapply(
  c("s1", "s2", "s3"),
  function(s) {
    d <- scen_simulate(s, N = 20000L)
    rows <- list()
    for (est in c("pp", "itt")) {
      tr <- scen_truth(s, est)
      sw <- scen_fit_swereg(d, est)
      te <- scen_fit_te(d, est, attr(tr, "p0"))
      rows[[est]] <- data.table(
        scenario = s,
        estimand = est,
        n = 20000L,
        seed = 2026L,
        truth = as.numeric(tr),
        truth_exact = as.numeric(scen_truth_irr_exact(s, est)),
        sw_est = sw[["est"]],
        sw_lo = sw[["lo"]],
        sw_hi = sw[["hi"]],
        te_est = te[["est"]],
        te_lo = te[["lo"]],
        te_hi = te[["hi"]]
      )
    }
    rbindlist(rows)
  },
  mc.cores = min(3L, MAX_WORKERS)
)
ev$triangle <- rbindlist(tri)
ev$triangle

## realized descriptives per scenario dataset ====
rows <- list()
for (s in c("s1", "s2", "s3")) {
  d <- scen_simulate(s, N = 20000L)
  rows[[s]] <- data.table(
    scenario = s,
    n_persons = uniqueN(d$id),
    person_periods = nrow(d),
    pct_periods_lost = 100 * (1 - nrow(d) / (20000 * 20)),
    pct_initiators = 100 * d[period == 0, mean(A_t)],
    first_event_persons = d[Y_t == 1, uniqueN(id)],
    event_risk_follow_up_interval_pct = 100 * mean(d$Y_t)
  )
}
ev$triangle_desc <- rbindlist(rows)
ev$triangle_desc

## replicated triangle: 20 independent datasets per scenario ====
# Single-dataset cells carry ~0.03-0.05 MC noise on the log-IRR scale; the
# replicated matrix shows the MEAN bias converging to zero (or, for the s3
# ITT cell, to its systematic displacement) with MC error ~ sd/sqrt(20).
tru <- list()
tru_exact <- list()
for (s in c("s1", "s2", "s3")) {
  for (est in c("pp", "itt")) {
    tru[[paste0(s, "_", est)]] <- scen_truth(s, est)
    tru_exact[[paste0(s, "_", est)]] <- as.numeric(scen_truth_irr_exact(s, est))
  }
}
R_TRI <- 20L
grid <- CJ(scenario = c("s1", "s2", "s3"), rep = 1:R_TRI)
reps <- parallel::mclapply(
  seq_len(nrow(grid)),
  function(i) {
    g <- grid[i]
    d <- scen_simulate(g$scenario, N = 20000L, seed = 2100L + g$rep)
    rows <- list()
    for (est in c("pp", "itt")) {
      tr <- tru[[paste0(g$scenario, "_", est)]]
      sw <- scen_fit_swereg(d, est)
      te <- scen_fit_te(d, est, attr(tr, "p0"))
      rows[[est]] <- data.table(
        scenario = g$scenario,
        rep = g$rep,
        seed = 2100L + g$rep,
        estimand = est,
        truth = as.numeric(tr),
        truth_exact = tru_exact[[paste0(g$scenario, "_", est)]],
        sw_est = sw[["est"]],
        sw_est_untrunc = sw[["est_untrunc"]],
        te_est = te[["est"]]
      )
    }
    rbindlist(rows)
  },
  mc.cores = min(6L, MAX_WORKERS)
)
ev$triangle_reps <- rbindlist(reps)
ev$triangle_reps[,
  .(mean_bias_sw = mean(sw_est - truth), mean_bias_te = mean(te_est - truth)),
  by = .(scenario, estimand)
]
saveRDS(ev, EVIDENCE_PATH, version = 2)

# PART 2 -- STRESS MATRIX======================================================
# The adversarial cells of test-tte_stress_matrix.R (always-on + opt-in tiers).

## single-dataset cells ====
cell <- function(name, estimand, tr, fit, note = "") {
  data.table(
    cell = name,
    estimand = estimand,
    truth = as.numeric(tr),
    est = fit[["est"]],
    lo = fit[["lo"]],
    hi = fit[["hi"]],
    note = note
  )
}

d_rare <- stress_sim(
  N = 40000L,
  T_periods = 20L,
  lor = -0.7,
  out_int = -6.0,
  loss = "none"
)
d_null <- stress_sim(N = 20000L, T_periods = 20L, lor = 0, loss = "independent")
d_attr <- stress_sim(
  N = 30000L,
  T_periods = 20L,
  lor = -0.7,
  loss = "informative",
  loss_int = -1.3,
  loss_L0 = 0.9
)
attr_lost <- sprintf(
  "%.0f%% of person-periods lost",
  100 * (1 - nrow(d_attr) / (30000 * 20))
)

ev$stress <- rbind(
  cell(
    "rare_outcome",
    "pp",
    stress_truth("pp", 20L, lor = -0.7, out_int = -6.0),
    scen_fit_swereg(d_rare, "pp"),
    sprintf("event risk %.2f%%/follow-up interval", 100 * mean(d_rare$Y_t))
  ),
  cell(
    "rare_outcome",
    "itt",
    stress_truth("itt", 20L, lor = -0.7, out_int = -6.0),
    scen_fit_swereg(d_rare, "itt")
  ),
  cell(
    "null_effect",
    "itt",
    stress_truth("itt", 20L, lor = 0),
    scen_fit_swereg(d_null, "itt"),
    "true log-IRR = 0"
  ),
  cell(
    "informative_attrition",
    "pp",
    stress_truth("pp", 20L, lor = -0.7),
    scen_fit_swereg(d_attr, "pp"),
    attr_lost
  ),
  cell(
    "informative_attrition",
    "itt",
    stress_truth("itt", 20L, lor = -0.7),
    scen_fit_swereg(d_attr, "itt"),
    "biased by design: no loss weight"
  )
)
ev$stress

## determinism ====
d8 <- stress_sim(N = 8000L, T_periods = 20L, lor = -0.7, loss = "independent")
f1 <- scen_fit_swereg(d8, "pp")
f2 <- scen_fit_swereg(d8, "pp")
ev$stress_determinism <- data.table(
  identical = identical(f1, f2),
  max_abs_delta = max(abs(f1 - f2))
)

## harmful effect under depletion of susceptibles, 3 seeds ====
tr_h <- stress_truth("itt", 20L, lor = +0.7)
rows <- list()
for (s in 1:3) {
  d <- stress_sim(
    N = 20000L,
    T_periods = 20L,
    lor = +0.7,
    loss = "independent",
    seed = 3000L + s
  )
  fit <- scen_fit_swereg(d, "itt")
  te <- scen_fit_te(d, "itt", attr(tr_h, "p0"))
  rows[[s]] <- data.table(
    seed = 3000L + s,
    truth = as.numeric(tr_h),
    sw_est = fit[["est"]],
    te_est = te[["est"]]
  )
}
ev$stress_harmful <- rbindlist(rows)
ev$stress_harmful

## near-positivity violation: truncation-attenuation dose response ====
d_pos <- stress_sim(
  N = 20000L,
  T_periods = 20L,
  lor = -0.7,
  a0_L0 = 2.5,
  loss = "none"
)
tr_pos <- as.numeric(stress_truth("itt", 20L, lor = -0.7))
rows <- list()
for (bd in list(c(0.005, 0.995), c(0.01, 0.99), c(0.05, 0.95))) {
  f <- stress_fit_itt_trunc(d_pos, bd[1], bd[2])
  rows[[length(rows) + 1L]] <- data.table(
    trunc_percentiles = sprintf("%.1f / %.1f", 100 * bd[1], 100 * bd[2]),
    truth = tr_pos,
    est = f[["est"]],
    wmax_raw = f[["wmax"]]
  )
}
ev$stress_trunc <- rbindlist(rows)
ev$stress_trunc

## treatment-confounder feedback: time-updated vs frozen IPCW covariates ====
d_tv <- tv_sim(N = 25000L, T_periods = 20L)
tr_pp <- as.numeric(tv_truth("pp", 20L))
tr_itt <- as.numeric(tv_truth("itt", 20L))
f_up <- tv_fit(tv_build_long(d_tv, "updated"), "pp")
f_fr <- tv_fit(tv_build_long(d_tv, "frozen"), "pp")
f_it <- tv_fit(tv_build_long(d_tv, "updated"), "itt")
ev$stress_tv <- data.table(
  fit = c(
    "pp, time-updated censoring covariate",
    "pp, covariate frozen at baseline",
    "itt"
  ),
  truth = c(tr_pp, tr_pp, tr_itt),
  est = c(f_up[["est"]], f_fr[["est"]], f_it[["est"]]),
  lo = c(f_up[["lo"]], f_fr[["lo"]], f_it[["lo"]]),
  hi = c(f_up[["hi"]], f_fr[["hi"]], f_it[["hi"]])
)
ev$stress_tv
saveRDS(ev, EVIDENCE_PATH, version = 2)

## truncation-tradeoff grid: does the s3-PP pattern generalize? ====
# One knob at a time off the s3 design: informativeness dose (0.45/0.9/1.5 on
# L0), reversed selection (-0.9), harmful effect (+0.7). PP estimand, 10
# paired replicates per cell, TrialEmulation as the conditional-adjustment
# comparator.
tg_cells <- list(
  list(name = "mild", lor = -0.7, loss_L0 = 0.45),
  list(name = "base", lor = -0.7, loss_L0 = 0.9),
  list(name = "harsh", lor = -0.7, loss_L0 = 1.5),
  list(name = "reversed", lor = -0.7, loss_L0 = -0.9),
  list(name = "harmful", lor = 0.7, loss_L0 = 0.9)
)
tg_tr <- list(
  protective = stress_truth("pp", 20L, lor = -0.7),
  harmful = stress_truth("pp", 20L, lor = 0.7)
)
tg_grid <- CJ(cell = seq_along(tg_cells), rep = 1:10)
tg <- parallel::mclapply(
  seq_len(nrow(tg_grid)),
  function(i) {
    g <- tg_grid[i]
    cl <- tg_cells[[g$cell]]
    tr <- if (cl$lor > 0) tg_tr$harmful else tg_tr$protective
    d <- stress_sim(
      N = 20000L,
      T_periods = 20L,
      lor = cl$lor,
      loss = "informative",
      loss_int = -2.4,
      loss_L0 = cl$loss_L0,
      seed = g$cell * 1000L + g$rep
    )
    sw <- scen_fit_swereg(d, "pp")
    te <- scen_fit_te(d, "pp", attr(tr, "p0"))
    data.table(
      cell = cl$name,
      lor = cl$lor,
      loss_L0 = cl$loss_L0,
      rep = g$rep,
      seed = g$cell * 1000L + g$rep,
      truth = as.numeric(tr),
      b_sw_trunc = sw[["est"]] - as.numeric(tr),
      b_sw_untrunc = sw[["est_untrunc"]] - as.numeric(tr),
      b_te = te[["est"]] - as.numeric(tr),
      pct_lost = 100 * (1 - nrow(d) / (20000 * 20))
    )
  },
  mc.cores = min(10L, MAX_WORKERS)
)
ev$trunc_grid <- rbindlist(tg)
ev$trunc_grid[,
  .(
    sw_trunc = mean(b_sw_trunc),
    sw_untrunc = mean(b_sw_untrunc),
    te = mean(b_te)
  ),
  by = cell
]

## unmeasured-driver cells: selection on a U no estimator can see ====
# U ~ N(0,1) raises the outcome hazard (0.4 U) but is unavailable to every
# fit (not in the propensity, censoring, or outcome models). Two mechanisms:
#   unmeasured_loss      : dropout hazard plogis(-2.4 + 0.9 U); U independent
#                          of treatment, so both arms lose high-U person-time
#                          at the same rate
#   unmeasured_adherence : treated discontinue more at high U (healthy-adherer
#                          mechanism, -0.8 U on the persistence logit); no loss
um_sim <- function(
  N,
  T_periods,
  lor = -0.7,
  u_adh = 0,
  loss = "none",
  seed = 2026
) {
  set.seed(seed)
  L0 <- stats::rnorm(N)
  U <- stats::rnorm(N)
  out <- vector("list", T_periods)
  prev_A <- integer(N)
  for (t in 0:(T_periods - 1L)) {
    logit_A <- if (t == 0) {
      -0.3 + 0.6 * L0
    } else {
      -3.0 + 0.4 * L0 + 8 * prev_A + u_adh * U * prev_A
    }
    A <- stats::rbinom(N, 1, stats::plogis(logit_A))
    Y <- stats::rbinom(N, 1, stats::plogis(-3.5 + lor * A + 0.4 * L0 + 0.4 * U))
    out[[t + 1L]] <- data.table(
      id = seq_len(N),
      period = t,
      L0 = L0,
      A_t = A,
      Y_t = Y
    )
    prev_A <- A
  }
  d <- rbindlist(out)
  setorder(d, id, period)
  if (loss == "informative") {
    set.seed(seed + 99L)
    haz <- stats::plogis(-2.4 + 0.9 * U)
    drop_at <- stats::qgeom(stats::runif(N), haz)
    d[, .drop := drop_at[id]]
    d <- d[period <= .drop]
    d[, .drop := NULL]
  }
  d[]
}
um_truth <- function(
  T_periods = 20L,
  lor = -0.7,
  N_truth = 200000L,
  seed = 999
) {
  # sustained-treatment (PP) first-event truth, standardised over L0 and U
  rate <- numeric(2)
  for (i in 1:2) {
    a <- c(0L, 1L)[i]
    set.seed(seed)
    L0 <- stats::rnorm(N_truth)
    U <- stats::rnorm(N_truth)
    at_risk <- rep(TRUE, N_truth)
    ev <- 0L
    pt <- 0
    for (t in 0:(T_periods - 1L)) {
      Y <- stats::rbinom(
        N_truth,
        1,
        stats::plogis(-3.5 + lor * a + 0.4 * L0 + 0.4 * U)
      )
      pt <- pt + sum(at_risk)
      new_ev <- at_risk & (Y == 1L)
      ev <- ev + sum(new_ev)
      at_risk <- at_risk & !new_ev
    }
    rate[i] <- ev / pt
  }
  out <- log(rate[2] / rate[1])
  attr(out, "p0") <- rate[1]
  out
}

um_cells <- list(
  list(name = "unmeasured_loss", u_adh = 0, loss = "informative"),
  list(name = "unmeasured_adherence", u_adh = -0.8, loss = "none")
)
tr_um <- um_truth(20L, lor = -0.7)
um_grid <- CJ(cell = seq_along(um_cells), rep = 1:10)
um <- parallel::mclapply(
  seq_len(nrow(um_grid)),
  function(i) {
    g <- um_grid[i]
    cl <- um_cells[[g$cell]]
    d <- um_sim(
      N = 20000L,
      T_periods = 20L,
      lor = -0.7,
      u_adh = cl$u_adh,
      loss = cl$loss,
      seed = 8000L + g$cell * 100L + g$rep
    )
    sw <- scen_fit_swereg(d, "pp")
    te <- scen_fit_te(d, "pp", attr(tr_um, "p0"))
    data.table(
      cell = cl$name,
      lor = -0.7,
      loss_L0 = NA_real_,
      rep = g$rep,
      seed = 8000L + g$cell * 100L + g$rep,
      truth = as.numeric(tr_um),
      b_sw_trunc = sw[["est"]] - as.numeric(tr_um),
      b_sw_untrunc = sw[["est_untrunc"]] - as.numeric(tr_um),
      b_te = te[["est"]] - as.numeric(tr_um),
      pct_lost = 100 * (1 - nrow(d) / (20000 * 20))
    )
  },
  mc.cores = min(10L, MAX_WORKERS)
)
ev$trunc_grid <- rbind(ev$trunc_grid, rbindlist(um))
ev$trunc_grid[,
  .(
    sw_trunc = mean(b_sw_trunc),
    sw_untrunc = mean(b_sw_untrunc),
    te = mean(b_te)
  ),
  by = cell
]
saveRDS(ev, EVIDENCE_PATH, version = 2)

## feedback grid: censoring driven by a time-varying, treatment-affected L_t ====
tr_tvg <- tv_truth("pp", 20L)
fb <- parallel::mclapply(
  1:10,
  function(rep) {
    d <- tv_sim(N = 20000L, T_periods = 20L, seed = 6000L + rep)
    f_up <- tv_fit(tv_build_long(d, "updated"), "pp")
    f_fr <- tv_fit(tv_build_long(d, "frozen"), "pp")
    d2 <- copy(d)
    d2[, L0 := L_t[period == 0L][1L], by = id] # TE conditions on baseline L only
    te <- scen_fit_te(
      d2[, .(id, period, L0, A_t, Y_t)],
      "pp",
      attr(tr_tvg, "p0")
    )
    data.table(
      rep = rep,
      seed = 6000L + rep,
      truth = as.numeric(tr_tvg),
      b_sw_updated = f_up[["est"]] - as.numeric(tr_tvg),
      b_sw_frozen = f_fr[["est"]] - as.numeric(tr_tvg),
      b_te_baseline = te[["est"]] - as.numeric(tr_tvg)
    )
  },
  mc.cores = min(10L, MAX_WORKERS)
)
ev$feedback_grid <- rbindlist(fb)
ev$feedback_grid[, .(
  updated = mean(b_sw_updated),
  frozen = mean(b_sw_frozen),
  te_baseline = mean(b_te_baseline)
)]
saveRDS(ev, EVIDENCE_PATH, version = 2)

# PART 3 -- PLAN-LAYER FULL PIPELINE===========================================
# The full-N factorial + discontinuation cells of test-tteplan_truth_matrix.R
# (opt-in tier), through the complete spec -> workers -> svyglm chain.
plan_cells <- list(
  list(
    name = "A_none",
    scenario = "A",
    loss = "none",
    n = 9000L,
    seed = 2026L,
    disc = 0
  ),
  list(
    name = "A_indep",
    scenario = "A",
    loss = "independent",
    n = 15000L,
    seed = 2027L,
    disc = 0
  ),
  list(
    name = "A_inform",
    scenario = "A",
    loss = "informative",
    n = 15000L,
    seed = 2028L,
    disc = 0
  ),
  list(
    name = "B_none",
    scenario = "B",
    loss = "none",
    n = 9000L,
    seed = 4242L,
    disc = 0
  ),
  list(
    name = "B_indep",
    scenario = "B",
    loss = "independent",
    n = 15000L,
    seed = 4243L,
    disc = 0
  ),
  list(
    name = "B_inform",
    scenario = "B",
    loss = "informative",
    n = 15000L,
    seed = 4244L,
    disc = 0
  ),
  list(
    name = "DISC",
    scenario = "A",
    loss = "none",
    n = 9000L,
    seed = 7777L,
    disc = 0.04
  )
)
run_plan <- function(cl) {
  confs <- if (cl$scenario == "A") {
    "rd_age_continuous"
  } else {
    c("rd_age_continuous", "ri_highrisk")
  }
  sk <- ttm_skeleton(
    cl$scenario,
    n_persons = cl$n,
    loss = cl$loss,
    disc_hazard = cl$disc,
    seed = cl$seed
  )
  sk_pw <- nrow(sk)
  sk_ev <- sum(sk$osd_a)
  sk_tx_pw <- sum(sk$rd_tx == "treated")
  r <- ttm_run_cell(sk, cl$name, confs)
  truth_pp <- if (cl$scenario == "B") 1.9818 else 2.0 # B: frailty-attenuated marginal truth
  truth_itt <- if (cl$disc > 0) ttm_disc_itt_truth(cl$disc) else truth_pp
  data.table(
    cell = cl$name,
    scenario = cl$scenario,
    loss = cl$loss,
    n_persons = cl$n,
    seed = cl$seed,
    sk_person_weeks = sk_pw,
    sk_events_n = sk_ev,
    sk_treated_person_weeks = sk_tx_pw,
    truth_pp = truth_pp,
    truth_itt = truth_itt,
    pp_irr = r$irr_pp$IRR,
    pp_lo = r$irr_pp$IRR_lower,
    pp_hi = r$irr_pp$IRR_upper,
    itt_irr = r$irr_itt$IRR,
    itt_lo = r$irr_itt$IRR_lower,
    itt_hi = r$irr_itt$IRR_upper,
    crude_rr = r$crude_rr,
    ipw_rr = r$ipw_rr
  )
}
rows <- list()
for (cl in plan_cells) {
  rows[[cl$name]] <- run_plan(cl)
}
ev$plan <- rbindlist(rows)
ev$plan
saveRDS(ev, EVIDENCE_PATH, version = 2)

## 8-seed Monte Carlo per scenario at N = 6000 ====
mc_grid <- CJ(scenario = c("A", "B"), seed = 5000L + 1:8)
mc <- parallel::mclapply(
  seq_len(nrow(mc_grid)),
  function(i) {
    g <- mc_grid[i]
    confs <- if (g$scenario == "A") {
      "rd_age_continuous"
    } else {
      c("rd_age_continuous", "ri_highrisk")
    }
    sk <- ttm_skeleton(g$scenario, n_persons = 6000L, seed = g$seed)
    r <- ttm_run_cell(sk, sprintf("mc_%s_%d", g$scenario, g$seed), confs)
    truth <- if (g$scenario == "B") 1.9818 else 2.0
    data.table(
      scenario = g$scenario,
      seed = g$seed,
      n_persons = 6000L,
      truth = truth,
      pp_irr = r$irr_pp$IRR,
      pp_lo = r$irr_pp$IRR_lower,
      pp_hi = r$irr_pp$IRR_upper,
      itt_irr = r$irr_itt$IRR,
      itt_lo = r$irr_itt$IRR_lower,
      itt_hi = r$irr_itt$IRR_upper
    )
  },
  mc.cores = min(4L, MAX_WORKERS)
)
ev$plan_mc <- rbindlist(mc)
ev$plan_mc
saveRDS(ev, EVIDENCE_PATH, version = 2)

# PART 4 -- MONTE CARLO COVERAGE (PP and ITT, M = 200 per cell)===============
# The opt-in coverage study of test-tte_coverage.R, extended to both
# estimands, with per-replicate estimates retained so the vignette can report
# bias, MC sd, and coverage x/M. Primary truncated weights throughout. The
# truth is the exact log-IRR, as in scen_coverage().
M <- 200L
rows <- list()
rows_reps <- list()
for (est_nm in c("pp", "itt")) {
  for (s in c("s1", "s2", "s3")) {
    truth <- as.numeric(scen_truth_irr_exact(s, est_nm))
    fits <- parallel::mclapply(
      seq_len(M),
      function(m) {
        d <- scen_simulate(s, N = 3000L, seed = 1000L + m)
        tryCatch(scen_fit_swereg(d, est_nm), error = function(e) NULL) # rare non-convergence -> drop replicate
      },
      mc.cores = min(10L, MAX_WORKERS)
    )
    ok <- !vapply(fits, is.null, logical(1))
    est <- vapply(fits[ok], `[[`, numeric(1), "est")
    lo <- vapply(fits[ok], `[[`, numeric(1), "lo")
    hi <- vapply(fits[ok], `[[`, numeric(1), "hi")
    rows_reps[[paste(est_nm, s)]] <- data.table(
      scenario = s,
      estimand = est_nm,
      rep = which(ok),
      truth = truth,
      est = est,
      lo = lo,
      hi = hi,
      covered = truth >= lo & truth <= hi
    )
    rows[[paste(est_nm, s)]] <- data.table(
      scenario = s,
      estimand = est_nm,
      M = M,
      n = 3000L,
      n_fit = sum(ok),
      truth = truth,
      mc_mean_bias = mean(est) - truth,
      mc_sd = sd(est),
      covered_n = sum(truth >= lo & truth <= hi),
      coverage = mean(truth >= lo & truth <= hi)
    )
  }
}
ev$coverage_reps <- rbindlist(rows_reps)
ev$coverage <- rbindlist(rows)
ev$coverage

saveRDS(ev, EVIDENCE_PATH, version = 2)
