# PLAN-layer truth matrix: the full TTEPlan sequential-trials pipeline
# (spec YAML -> skeleton file -> $s1_generate_enrollments_and_ipw ->
# $s2_generate_analysis_files_and_ipcw_pp -> $s3_analyze) is driven end to end
# against synthetic skeletons with KNOWN planted truth (marginal per-week
# IRR = 2.0 under sustained treatment; constant hazards).
#
# The TTEEnrollment layer is already truth-validated (test-tte_itt_correctness,
# test-tte_validation_matrix); this file validates what sits ABOVE it: trial-
# band assignment, sequential enrollment, per-band matching_ratio downsampling,
# pooling across ~55 bands, the PP/ITT dual analysis files, and the subprocess
# worker chain. DGP + drivers live in helper-tteplan_truth.R.
#
# Cells (factorial {no confounding "A", baseline confounding "B"} x
# {no loss, independent loss, informative loss} + discontinuation):
#   A_none  : PP == ITT == 2.0 (no confounding, full persistence)
#   B_none  : crude inflated by confounding; IPW recovers 2.0
#   A/B x independent/informative loss: truth unchanged (loss multiplies
#             person-time identically in both arms); machinery must tolerate
#             truncated panels + real censoring events in the IPCW model
#   DISC    : 4%/wk discontinuation -> PP (censor at deviation + IPCW) stays
#             at 2.0, ITT attenuates toward the simulated do(initiate) truth
#             (~1.42); encodes the post-fix rule that an event in the same
#             band as the deviation still counts as an event
#
# Always-on subset: 3 cells at reduced N (~1.5 min total; each cell spawns ~12
# worker subprocesses). Full factorial is opt-in:
#   SWEREG_RUN_PLAN_MATRIX=true Rscript -e \
#     'devtools::load_all("."); testthat::test_dir("tests/testthat", filter="tteplan_truth")'
#
# Tolerances are on the LOG scale, set from the observed Monte Carlo error of
# this exact pipeline (8-seed MC at N=6000: sd(log bias) ~0.030 no-confounding
# / ~0.066 confounded; single-run log-SE ~ sqrt(2/events-per-arm)) with a
# 2.5-3x margin, and are commented per assertion. Seeds are fixed, so realized
# gaps are deterministic; tolerances leave room for RNG-stream or estimator
# refinements without masking real bias.

.ttm_skip_if_missing <- function() {
  skip_on_cran()
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("qs2")
  skip_if_not_installed("yaml")
  skip_if_not_installed("processx")
  skip_if_not_installed("progressr")
}

test_that("plan-layer truth A_none: no confounding, full persistence -> PP == ITT == 2.0", {
  .ttm_skip_if_missing()

  sk <- ttm_skeleton("A", n_persons = 3000L, seed = 2026L)
  r <- ttm_run_cell(sk, "A_none", "rd_age_continuous")

  # ~330 events/arm at N=3000 -> log-SE ~ sqrt(2/330) ~ 0.078; tol = 0.22
  # (~2.8x SE). Realized |bias| at this seed ~0.02-0.06.
  expect_lt(abs(log(r$irr_pp$IRR) - log(2)), 0.22)
  expect_lt(abs(log(r$irr_itt$IRR) - log(2)), 0.22)
  # 95% CI covers the planted truth (deterministic at fixed seed)
  expect_true(r$irr_pp$IRR_lower <= 2 && 2 <= r$irr_pp$IRR_upper)
  expect_true(r$irr_itt$IRR_lower <= 2 && 2 <= r$irr_itt$IRR_upper)
  # With zero switching the PP and ITT panels differ only by IPCW ~ 1, so the
  # two estimates must be essentially identical (structural check, not MC):
  # realized gap < 1e-4; tol 0.02 allows minor IPCW fallback jitter.
  expect_lt(abs(log(r$irr_pp$IRR) - log(r$irr_itt$IRR)), 0.02)
})

test_that("plan-layer truth B_none: baseline confounding inflates crude, IPW recovers 2.0", {
  .ttm_skip_if_missing()

  sk <- ttm_skeleton("B", n_persons = 3000L, seed = 4242L)
  r <- ttm_run_cell(sk, "B_none", c("rd_age_continuous", "ri_highrisk"))

  # ~430 events/arm -> log-SE ~0.068; tol = 0.22 (~3x SE) also absorbs the
  # -0.009 frailty attenuation of the marginal truth (1.9818, see helper).
  expect_lt(abs(log(r$irr_pp$IRR) - log(2)), 0.22)
  expect_lt(abs(log(r$irr_itt$IRR) - log(2)), 0.22)
  expect_true(r$irr_itt$IRR_lower <= 2 && 2 <= r$irr_itt$IRR_upper)
  # The confounder is doing real work: the unadjusted rate ratio must be
  # inflated well above the IPW-weighted one. True crude ~2.6 vs weighted
  # ~2.0 (log gap ~0.27); require > 0.12 (~half the true gap).
  expect_gt(log(r$crude_rr) - log(r$ipw_rr), 0.12)
})

test_that("plan-layer truth DISC: discontinuation separates PP (2.0) from ITT (attenuated)", {
  .ttm_skip_if_missing()

  sk <- ttm_skeleton("A", n_persons = 4000L, disc_hazard = 0.04, seed = 7777L)
  r <- ttm_run_cell(sk, "DISC", "rd_age_continuous")

  # PP censors at the deviation band (post-fix: an event in that same band
  # still counts) and IPCW-reweights -> sustained-treatment truth 2.0.
  # Fewer treated person-weeks (mean ~25/enrollee) -> log-SE ~0.10;
  # tol = 0.28 (~2.8x SE).
  expect_lt(abs(log(r$irr_pp$IRR) - log(2)), 0.28)
  expect_true(r$irr_pp$IRR_lower <= 2 && 2 <= r$irr_pp$IRR_upper)

  # ITT keeps post-discontinuation follow-up in the treated arm, so it
  # attenuates toward the simulated do(initiate at week 0) truth (~1.42 at
  # disc = 0.04 over 52w). tol = 0.25 (~2.5x its SE).
  itt_truth <- ttm_disc_itt_truth(0.04)
  expect_lt(abs(log(r$irr_itt$IRR) - log(itt_truth)), 0.25)
  # And the two estimands must actually separate: true log gap
  # log(2/1.42) ~ 0.34 (realized ~0.46 at the full-N seed); require > 0.12.
  expect_gt(log(r$irr_pp$IRR) - log(r$irr_itt$IRR), 0.12)
})

test_that("plan-layer truth matrix: full factorial incl. loss to follow-up (opt-in)", {
  .ttm_skip_if_missing()
  skip_if_not(
    identical(Sys.getenv("SWEREG_RUN_PLAN_MATRIX"), "true"),
    "set SWEREG_RUN_PLAN_MATRIX=true to run the (slow, ~5 min) full plan-layer truth matrix"
  )

  cells <- list(
    list(
      name = "A_none",
      scenario = "A",
      loss = "none",
      n = 9000L,
      seed = 2026L
    ),
    list(
      name = "A_indep",
      scenario = "A",
      loss = "independent",
      n = 15000L,
      seed = 2027L
    ),
    list(
      name = "A_inform",
      scenario = "A",
      loss = "informative",
      n = 15000L,
      seed = 2028L
    ),
    list(
      name = "B_none",
      scenario = "B",
      loss = "none",
      n = 9000L,
      seed = 4242L
    ),
    list(
      name = "B_indep",
      scenario = "B",
      loss = "independent",
      n = 15000L,
      seed = 4243L
    ),
    list(
      name = "B_inform",
      scenario = "B",
      loss = "informative",
      n = 15000L,
      seed = 4244L
    )
  )

  for (cl in cells) {
    confs <- if (cl$scenario == "A") {
      "rd_age_continuous"
    } else {
      c("rd_age_continuous", "ri_highrisk")
    }
    sk <- ttm_skeleton(
      cl$scenario,
      n_persons = cl$n,
      loss = cl$loss,
      seed = cl$seed
    )
    r <- ttm_run_cell(sk, cl$name, confs)

    # Tolerance per cell type:
    #  - no-loss cells: ~1000+ events/arm -> log-SE ~0.05; tol 0.15 (~3x SE)
    #  - loss cells: loss (2-4%/wk from study start) cuts person-time to
    #    ~250-400 events/arm -> log-SE ~0.08; tol 0.25 (~3x SE)
    tol <- if (cl$loss == "none") 0.15 else 0.25
    info <- paste0("cell ", cl$name)
    expect_lt(abs(log(r$irr_pp$IRR) - log(2)), tol, label = info)
    expect_lt(abs(log(r$irr_itt$IRR) - log(2)), tol, label = info)
    # No switching in these 6 cells, but the tight PP == ITT structural
    # identity only holds WITHOUT loss: under loss the PP file carries real
    # censoring events, so IPCW reweights PP away from ITT slightly
    # (realized gap up to ~0.04 log in the B_inform cell at these seeds).
    # Keep the tight check for no-loss cells; allow the IPCW divergence
    # under loss (0.08 = ~2x the realized maximum).
    pp_itt_gap <- if (cl$loss == "none") 0.02 else 0.08
    expect_lt(
      abs(log(r$irr_pp$IRR) - log(r$irr_itt$IRR)),
      pp_itt_gap,
      label = info
    )
    if (cl$scenario == "B") {
      # Confounding present -> crude inflated relative to weighted. Under
      # INFORMATIVE loss the observable confounding genuinely shrinks:
      # highrisk person-time (which carries both the excess treatment and
      # the excess hazard) leaves the risk set 3x faster, so the crude gap
      # erodes (realized log gaps at these seeds: B_none 0.31, B_indep 0.36,
      # B_inform 0.11). Thresholds ~ 40% of the realized gap.
      crude_gap <- if (cl$loss == "informative") 0.05 else 0.12
      expect_gt(log(r$crude_rr) - log(r$ipw_rr), crude_gap, label = info)
    }
  }

  # Discontinuation cell at full N (the always-on version runs at N=4000).
  sk <- ttm_skeleton("A", n_persons = 9000L, disc_hazard = 0.04, seed = 7777L)
  r <- ttm_run_cell(sk, "DISC", "rd_age_continuous")
  itt_truth <- ttm_disc_itt_truth(0.04)
  # PP: ~350 treated-arm events -> log-SE ~0.08; tol 0.2 (~2.5x SE).
  expect_lt(abs(log(r$irr_pp$IRR) - log(2)), 0.20)
  expect_lt(abs(log(r$irr_itt$IRR) - log(itt_truth)), 0.20)
  expect_gt(log(r$irr_pp$IRR) - log(r$irr_itt$IRR), 0.15)
})
