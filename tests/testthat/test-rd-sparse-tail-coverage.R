# Coverage of the risk-difference interval when an arm is sparse.
#
# The interval is the percentile interval of a person-level bootstrap. These
# tests run two designs, each with about 3 events in the intervention arm:
#
#   * full risk set: no censoring, so the whole arm is at risk at the horizon.
#   * sparse tail: censoring leaves about 3 persons of the intervention arm at
#     risk at the horizon.
#
# Without the head-count rule (`min_persons_at_risk = 0`) the sparse tail
# undercovers. Measured on 2026-10-19, with 200 datasets of 200 replicates
# each, the control covered 179 of 190 estimable intervals (0.942), and the
# sparse tail covered 161 of 194 (0.830). In a separate run of 300 datasets, a
# variant that carries the survival forward where a replicate empties a stop
# time gave 0.826 against 0.829, so the replicates that the percentile step
# drops are not the cause. The interval cannot reflect a late event that the
# sample does not hold.
#
# With the default rule (`min_persons_at_risk = 10`) the sparse tail gets no
# risk difference at the horizon, and the control keeps its interval.
#
# The test runs only when SWEREG_RUN_VALIDATION=true. It takes several minutes.

skip_on_cran()
skip_if_not(
  identical(Sys.getenv("SWEREG_RUN_VALIDATION"), "true"),
  "set SWEREG_RUN_VALIDATION=true to run the full validation tier"
)
skip_if_not_installed("data.table")

# One dataset: weekly stop times to K, a constant hazard in each arm, and
# geometric censoring with weekly probability `cens_prob` (0 for none).
.rst_data <- function(seed, n1, n0, r1, r0, k, cens_prob) {
  set.seed(seed)
  arm <- function(n, risk, exposed, prefix) {
    h <- 1 - (1 - risk)^(1 / k)
    ev_t <- stats::rgeom(n, h) + 1L
    c_t <- if (cens_prob > 0) stats::rgeom(n, cens_prob) + 1L else rep(k, n)
    stop_t <- pmin(ev_t, c_t, k)
    return(data.table::data.table(
      pid = paste0(prefix, seq_len(n)),
      ptid = paste0(prefix, seq_len(n), "_t"),
      exposed = exposed,
      tstart = 0L,
      tstop = stop_t,
      event = as.integer(ev_t <= pmin(c_t, k)),
      w = 1
    ))
  }
  return(rbind(arm(n1, r1, TRUE, "i"), arm(n0, r0, FALSE, "c")))
}

# Two curves per dataset at the last stop time. `k0` has no head-count rule
# and 200 replicates. `def` uses the default rule. The rule reads the head
# count only, so its curve needs one replicate.
.rst_run <- function(n_sim, n1, n0, r1, r0, k, cens_prob) {
  truth <- r1 - r0
  curve <- function(d, ...) {
    set.seed(1L)
    return(swereg:::.tte_rd_curve(d, "pid", "ptid", "exposed", "tstop", "w", ...))
  }
  one <- function(s) {
    d <- .rst_data(20261019L + s, n1, n0, r1, r0, k, cens_prob)
    k0 <- curve(d, n_boot = 200L, min_persons_at_risk = 0L)
    def <- curve(d, n_boot = 1L)
    last <- nrow(k0)
    return(c(
      lo = k0$rd_lo[last],
      hi = k0$rd_hi[last],
      at_risk = k0$n_persons_at_risk_intervention[last],
      events = sum(d$event * d$exposed),
      withheld = as.numeric(
        def$interval_status[last] == "too few at risk at horizon"
      ),
      withheld_rd_na = as.numeric(is.na(def$rd[last]))
    ))
  }
  res <- do.call(
    rbind,
    parallel::mclapply(
      seq_len(n_sim),
      one,
      mc.cores = getOption("mc.cores", 2L)
    )
  )
  ok <- !is.na(res[, "lo"])
  covered <- res[ok, "lo"] <= truth & truth <= res[ok, "hi"]
  out <- list(
    coverage_k0 = mean(covered),
    share_withheld = mean(res[, "withheld"]),
    withheld_rd_na = all(res[res[, "withheld"] == 1, "withheld_rd_na"] == 1)
  )
  message(sprintf(
    paste0(
      "k = 0 coverage %d/%d = %.3f; withheld at the default %d/%d; ",
      "mean intervention events %.2f; mean persons at risk at the horizon %.1f"
    ),
    sum(covered),
    sum(ok),
    out$coverage_k0,
    sum(res[, "withheld"]),
    n_sim,
    mean(res[, "events"]),
    mean(res[, "at_risk"])
  ))
  return(out)
}

test_that("RD interval: few events, full risk set (control)", {
  r <- .rst_run(
    n_sim = 200L,
    n1 = 1500L,
    n0 = 3000L,
    r1 = 0.002,
    r0 = 0.006,
    k = 52L,
    cens_prob = 0
  )
  expect_gte(r$coverage_k0, 0.90)
  expect_lte(r$coverage_k0, 0.99)
  # The whole arm is at risk at the horizon, so the default rule keeps it.
  expect_identical(r$share_withheld, 0)
})

test_that("RD interval: few events, sparse risk set at the horizon is withheld", {
  # About 3 of the 1500 intervention persons remain uncensored at week 52.
  r <- .rst_run(
    n_sim = 200L,
    n1 = 1500L,
    n0 = 3000L,
    r1 = 0.013,
    r0 = 0.039,
    k = 52L,
    cens_prob = 1 - (3 / 1500)^(1 / 52)
  )
  # Without the rule the interval undercovers. That is why the rule exists.
  expect_lt(r$coverage_k0, 0.90)
  # With the default rule, the horizon gets no risk difference.
  expect_gte(r$share_withheld, 0.95)
  expect_true(r$withheld_rd_na)
})
