# Fast validation tier. It has no skip_on_cran(), so it runs on every push,
# inside R CMD check under NOT_CRAN=true. The full tier is in
# test-validation-full.R.
#
# Each cell runs R replicates at seeds 2100 + r, N = 20,000, through
# val_replicates() (helper-tte_validation.R). It estimates the risk difference
# at h = 5, 10 and 20 weeks and the log-IRR. Each mean is compared with its
# exact truth, scen_truth_risk_exact() or scen_truth_irr_exact(). Each mean
# bias MUST be within 3.5 Monte Carlo standard errors, MC SE = sd / sqrt(R).
#
#   s1 per-protocol, 20 replicates, truncated weights (the primary analysis)
#   s1 ITT, 20 replicates, truncated weights
#   s4 per-protocol, 8 replicates, untruncated weights. s4 has loss,
#     deviation and time-zero discordance driven by different covariates, so
#     it checks the per-cause censoring weights. Truncation at the 1st and
#     99th percentiles biases s4 by design (see test-validation-full.R), so
#     this cell reads the untruncated weights.
#   s1 per-protocol built by enroll(), one simulation at seed 2101 and
#     N = 5,000. It checks the panel and not the bias: see the last paragraph.
#
# The replicates run in 2 forked processes (val_cores()), the most that R CMD
# check --as-cran allows.
#
# Runtime was measured twice on 2026-10-07 on uppsala: 6 cores, R 4.5.2,
# pkgload::load_all and no other load. The three cells took 166 and 169 s (s1
# per-protocol), 112 and 116 s (s1 ITT), and 57 and 58 s (s4 per-protocol).
# That is 336 s and 343 s in all, against a budget of 6 minutes. The
# enroll() cell came later. On 2026-10-09, with TESTTHAT_CPUS=1 while Slurm
# job 109 and the TTE daemon ran (load average 9.5 to 10.2 on 6 cores), the
# four cells took 189, 125, 65 and 8.5 s: 394 s in all, against a budget of
# 400 s.
#
# Margin against the old deviation rule, measured 2026-10-07. These panels
# reach s5 without `weeks_to_protocol_deviation`, so s5 reads the boundary
# from the per-interval treatment (R/r6_tteenrollment.R). A mutation moved
# that read back from `tstart` to `tstop`, which counts the deviation interval
# as per-protocol follow-up. Then 7 of the 12 assertions fail.
#   s1 per-protocol: z = 3.55, 4.76 and 4.49 (RD at h = 5, 10, 20) and 4.22
#     (log-IRR). The limit is 3.5, so h = 5 fails by the least.
#   s4 per-protocol: z = 8.74 and 4.22 (RD at h = 5, 10) and 4.14 (log-IRR).
# The weekly boundary of enroll() (`hit` in R/tte_boundaries.R) is not on the
# path of those three cells. The fourth cell builds its panel with enroll()
# from one row per person and week (val_enroll_weekly(),
# helper-tte_validation.R). It checks the boundary of each person-trial
# against the simulated treatment. It also checks that the estimates equal
# those of the tte_build_long() panel of the same simulation. With the right
# edge of releases 26.9.0 to 26.15.0, `hit + 1L` in place of `hit`, both
# checks fail: each of the 2,001 boundaries moves one week later, and the
# log-IRR moves by +0.0117 (measured 2026-10-09). test-deviation-left-edge.R
# pins the edge cases of the same boundary.

test_that("fast tier: s1 per-protocol risk difference and log-IRR match the exact truth", {
  reps <- val_replicates("s1", "pp", R = 20L, cores = val_cores(2L))
  val_expect_unbiased(val_summary(reps))
})

test_that("fast tier: s1 ITT risk difference and log-IRR match the exact truth", {
  reps <- val_replicates("s1", "itt", R = 20L, cores = val_cores(2L))
  val_expect_unbiased(val_summary(reps))
})

test_that("fast tier: s4 per-protocol risk difference and log-IRR match the exact truth", {
  reps <- val_replicates(
    "s4",
    "pp",
    R = 8L,
    weights = "untruncated",
    cores = val_cores(2L)
  )
  val_expect_unbiased(val_summary(reps))
})

test_that("fast tier: s1 per-protocol built by enroll() stops at the weekly deviation boundary", {
  d <- scen_simulate("s1", N = 5000L, seed = 2101L)
  want <- val_weekly_deviation(d)
  got <- unique(val_enroll_weekly(d, "s1")$data[, c("id", "weeks_to_protocol_deviation")])
  expect_identical(nrow(got), 5000L, label = "person-trials enrolled from the weekly rows")
  expect_gt(sum(!is.na(want$expected)), 1000L, label = "person-trials that deviate")
  got <- got[want, on = "id"]
  expect_identical(
    got$weeks_to_protocol_deviation,
    got$expected,
    label = "the weekly deviation boundary of each person-trial"
  )

  # The same simulation through both paths. Equal estimates mean that the
  # weekly boundary and the fallback read of s5_prepare_outcome() clip the same
  # rows.
  fits <- val_map(
    2L,
    function(i) {
      p <- c("enroll", "panel")[i]
      val_fit_replicate("s1", "pp", 2101L, N = 5000L, path = p)[, path := p]
    },
    cores = val_cores(2L)
  )
  est <- data.table::dcast(fits, quantity ~ path, value.var = "estimate")
  expect_lt(
    max(abs(est$enroll - est$panel)),
    1e-6,
    label = paste0(
      "largest difference between the enroll() and tte_build_long() estimates (",
      paste(sprintf("%s %+.5f", est$quantity, est$enroll - est$panel), collapse = ", "),
      ")"
    )
  )
})
