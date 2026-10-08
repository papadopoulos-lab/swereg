# Full validation tier. It runs only when SWEREG_RUN_VALIDATION=true, which
# the weekly workflow .github/workflows/validation.yml sets. Every block sits
# in one describe(), so the tier skips as one test. The fast tier is
# test-validation-fast.R.
#
# 1. s1 to s4, per-protocol and ITT: 20 replicates at seeds 2100 + r,
#    N = 20,000, truncated and untruncated weights. The quantities are the
#    log-IRR and the risk difference at h = 5, 10 and 20. Each mean bias
#    against the exact truth MUST be within 3.5 Monte Carlo standard errors.
#    The cells in .VAL_FULL_BANDS are the exception (see there).
# 2. Coverage of the 95% bootstrap risk-difference interval, per-protocol in
#    s1, s2 and s4. Each cell runs 200 replicates at seeds 3100 + r, with 200
#    bootstrap replicates each and untruncated weights. The coverage at each
#    horizon MUST be in
#    [0.90, 0.99]. At a true coverage of 0.95 and 200 replicates, the binomial
#    standard error is 0.015, so 0.90 is 3.2 standard errors below it.
#
# The bias cells run in up to 4 forked processes (val_cores(4L)).
#
# The check that the evidence matches the estimator source is always on, in
# test-validation-evidence-version.R.

# Cells whose bias is known by design. Each band is the mean bias measured on
# 2026-10-07, plus or minus 3.5 Monte Carlo standard errors, rounded outward
# to 0.001. The measurement used the seeds of the test, so the same
# replicates. There are two causes.
#
# - Truncated per-protocol weights in s2, s3 and s4. Truncation of
#   analysis_weight_pp at its 1st and 99th percentiles changes the weighted
#   population. Measured mean RD bias at h = 20, truncated against
#   untruncated: s2 +0.0097 / +0.0036, s3 +0.0247 / +0.0170, s4 +0.0186 /
#   -0.0015. In s1 truncation moves no single estimate by more than 0.0002,
#   so s1 has no band.
# - ITT in s3 and s4. ITT carries no loss weight, and the loss depends on a
#   covariate of the outcome (L0 in s3, L1 in s4). The exact limit of an ITT
#   fit without loss weights, minus the truth, is known. In s3 it is -0.0268
#   (log-IRR), and +0.0025, +0.0042 and +0.0003 (RD at h = 5, 10, 20). In s4
#   it is -0.0161, +0.0015, +0.0025 and +0.0002. Each band below contains
#   that value.
.VAL_FULL_BANDS <- data.table::fread(
  text = "
scenario,estimand,weight,quantity,lo,hi
s2,pp,truncated,log_irr,-0.014,0.055
s2,pp,truncated,rd_h10,-0.005,0.011
s2,pp,truncated,rd_h20,0.000,0.020
s2,pp,truncated,rd_h5,-0.002,0.007
s3,itt,truncated,log_irr,-0.047,0.014
s3,itt,truncated,rd_h10,-0.001,0.012
s3,itt,truncated,rd_h20,-0.005,0.011
s3,itt,truncated,rd_h5,0.000,0.010
s3,itt,untruncated,log_irr,-0.055,0.007
s3,itt,untruncated,rd_h10,-0.002,0.011
s3,itt,untruncated,rd_h20,-0.006,0.009
s3,itt,untruncated,rd_h5,-0.001,0.009
s3,pp,truncated,log_irr,0.010,0.078
s3,pp,truncated,rd_h10,0.004,0.019
s3,pp,truncated,rd_h20,0.013,0.036
s3,pp,truncated,rd_h5,-0.001,0.010
s4,itt,truncated,log_irr,-0.034,0.005
s4,itt,truncated,rd_h10,-0.001,0.009
s4,itt,truncated,rd_h20,-0.007,0.006
s4,itt,truncated,rd_h5,0.000,0.006
s4,itt,untruncated,log_irr,-0.040,-0.001
s4,itt,untruncated,rd_h10,-0.002,0.008
s4,itt,untruncated,rd_h20,-0.008,0.005
s4,itt,untruncated,rd_h5,0.000,0.005
s4,pp,truncated,log_irr,0.020,0.069
s4,pp,truncated,rd_h10,0.002,0.019
s4,pp,truncated,rd_h20,0.007,0.030
s4,pp,truncated,rd_h5,0.000,0.008
"
)

describe("full tier", {
  skip_if_not(
    identical(Sys.getenv("SWEREG_RUN_VALIDATION"), "true"),
    "set SWEREG_RUN_VALIDATION=true to run the full validation tier"
  )

  for (cell in list(
    c("s1", "pp"),
    c("s1", "itt"),
    c("s2", "pp"),
    c("s2", "itt"),
    c("s3", "pp"),
    c("s3", "itt"),
    c("s4", "pp"),
    c("s4", "itt")
  )) {
    local({
      scenario <- cell[1L]
      estimand <- cell[2L]
      it(
        sprintf(
          "%s %s: risk difference and log-IRR against the exact truths",
          scenario,
          estimand
        ),
        {
          reps <- val_replicates(
            scenario,
            estimand,
            R = 20L,
            weights = c("truncated", "untruncated"),
            cores = val_cores(4L)
          )
          val_expect_cells(val_summary(reps), .VAL_FULL_BANDS)
        }
      )
    })
  }

  for (scenario in c("s1", "s2", "s4")) {
    local({
      sc <- scenario
      it(sprintf("%s per-protocol: risk-difference interval coverage", sc), {
        cov <- val_rd_coverage(
          sc,
          "pp",
          R = 200L,
          n_boot = 200L,
          seed0 = 3100L,
          weight = "untruncated",
          cores = val_cores(4L)
        )
        val_expect_coverage(cov, lo = 0.90, hi = 0.99)
      })
    })
  }
})
