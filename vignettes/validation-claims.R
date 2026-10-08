# The validation claims of vignette("tte-methods"): section 3, and the
# passages of sections 1 and 4 that cite it. Each claim is one named function
# of the evidence object, readRDS("tte-validation-evidence.rds"), and returns
# TRUE or FALSE. The vignette sources this file and stops building when a
# claim is FALSE, through stopifnot(claim("<name>")).
# tests/testthat/test-vignette-claims.R checks every claim on the shipped
# evidence, and checks that the vignette names each claim and no other.
#
# Base R only, so the file works whether or not data.table is attached.

# The Monte Carlo standard-error multiple of the test tiers
# (tests/testthat/test-validation-fast.R and test-validation-full.R).
.vc_k <- 3.5

# The exact limit of an ITT fit without loss weights, minus the exact
# log-IRR truth. Measured in phase 5e (2026-10-07) and stated in
# tests/testthat/test-validation-full.R. The ITT estimand carries no loss
# weight, so informative loss moves its estimate by this much in any correct
# implementation.
.vc_itt_design_limit <- c(s3 = -0.0268, s4 = -0.0161)

# Rows of `ev$validation_summary`, the exact-truth cells. NULL selects all.
.vc_exact <- function(
  ev,
  scenario = NULL,
  estimand = NULL,
  weight = NULL,
  quantity = NULL
) {
  s <- as.data.frame(ev$validation_summary)
  keep <- rep(TRUE, nrow(s))
  if (!is.null(scenario)) keep <- keep & s$scenario %in% scenario
  if (!is.null(estimand)) keep <- keep & s$estimand %in% estimand
  if (!is.null(weight)) keep <- keep & s$weight %in% weight
  if (!is.null(quantity)) keep <- keep & s$quantity %in% quantity
  s[keep, , drop = FALSE]
}

# One value of `ev$validation_summary`.
.vc_exact1 <- function(ev, scenario, estimand, weight, quantity, col = "bias") {
  s <- .vc_exact(ev, scenario, estimand, weight, quantity)
  stopifnot(nrow(s) == 1L)
  s[[col]]
}

# TRUE when `s` has exactly `n` rows and each mean bias is within .vc_k Monte
# Carlo standard errors of zero.
.vc_unbiased <- function(s, n) {
  nrow(s) == n && all(abs(s$bias) <= .vc_k * s$mc_se)
}

# Mean bias and Monte Carlo standard error of `col` in `ev$triangle_reps`,
# against the exact log-IRR truth, for one scenario and estimand.
.vc_tri <- function(ev, scenario, estimand, col) {
  r <- as.data.frame(ev$triangle_reps)
  r <- r[r$scenario == scenario & r$estimand == estimand, , drop = FALSE]
  b <- r[[col]] - r$truth_exact
  c(n = length(b), bias = mean(b), se = stats::sd(b) / sqrt(length(b)))
}

# The three fits of the cross-package matrix.
.vc_tri_fits <- c("sw_est", "sw_est_untrunc", "te_est")

# Per-cell summary of `ev$trunc_grid`: mean bias and Monte Carlo standard
# error of the truncated swereg fit (t), the untruncated swereg fit (u) and
# TrialEmulation (te), the spread of each (t_sd, u_sd, te_sd) and the
# root-mean-squared error of each (t_rmse, u_rmse, te_rmse). One row per
# cell, named by the cell.
.vc_grid <- function(ev) {
  g <- as.data.frame(ev$trunc_grid)
  cells <- unique(g$cell)
  out <- do.call(rbind, lapply(cells, function(cl) {
    x <- g[g$cell == cl, , drop = FALSE]
    n <- nrow(x)
    f <- function(b) c(mean(b), stats::sd(b) / sqrt(n), stats::sd(b), sqrt(mean(b^2)))
    v <- c(f(x$b_sw_trunc), f(x$b_sw_untrunc), f(x$b_te))
    data.frame(
      cell = cl,
      n = n,
      t = v[1], t_se = v[2], t_sd = v[3], t_rmse = v[4],
      u = v[5], u_se = v[6], u_sd = v[7], u_rmse = v[8],
      te = v[9], te_se = v[10], te_sd = v[11], te_rmse = v[12]
    )
  }))
  rownames(out) <- out$cell
  out
}

# The per-protocol cells of Figures 4 to 6: s1 to s3 of `ev$triangle_reps`
# against the exact truth, and the cells of `ev$trunc_grid`. Columns as in
# .vc_grid().
.vc_pp_cells <- function(ev) {
  r <- as.data.frame(ev$triangle_reps)
  r <- r[r$estimand == "pp", , drop = FALSE]
  tri <- data.frame(
    cell = r$scenario,
    b_sw_trunc = r$sw_est - r$truth_exact,
    b_sw_untrunc = r$sw_est_untrunc - r$truth_exact,
    b_te = r$te_est - r$truth_exact
  )
  g <- as.data.frame(ev$trunc_grid)[, c("cell", "b_sw_trunc", "b_sw_untrunc", "b_te")]
  .vc_grid(list(trunc_grid = rbind(tri, g)))
}

# TRUE when the 95% Monte Carlo interval of mean `m` with standard error `se`
# holds zero.
.vc_covers0 <- function(m, se) abs(m) <= stats::qnorm(0.975) * se

validation_claims <- list(
  # 3.3 -- cross-package matrix ----------------------------------------------

  # "In Table 4, every interval of both packages covers the exact truth."
  tri_single_covers = function(ev) {
    t <- as.data.frame(ev$triangle)
    nrow(t) == 6L &&
      all(t$sw_lo <= t$truth_exact & t$truth_exact <= t$sw_hi) &&
      all(t$te_lo <= t$truth_exact & t$truth_exact <= t$te_hi)
  },

  # "In the s3 ITT cell both estimates are below the truth."
  tri_single_s3_itt_below = function(ev) {
    t <- as.data.frame(ev$triangle)
    t <- t[t$scenario == "s3" & t$estimand == "itt", , drop = FALSE]
    nrow(t) == 1L && t$sw_est < t$truth_exact && t$te_est < t$truth_exact
  },

  # "In s1 and s2, every fit is within 3.5 Monte Carlo standard errors of the
  # exact truth, for both estimands."
  tri_s1_s2_unbiased = function(ev) {
    ok <- c()
    for (s in c("s1", "s2")) {
      for (e in c("pp", "itt")) {
        for (f in .vc_tri_fits) {
          x <- .vc_tri(ev, s, e, f)
          ok <- c(ok, x[["n"]] == 20 && abs(x[["bias"]]) <= .vc_k * x[["se"]])
        }
      }
    }
    length(ok) == 12L && all(ok)
  },

  # "In the s3 per-protocol cell, the untruncated swereg fit and
  # TrialEmulation are within 3.5 Monte Carlo standard errors of the truth.
  # The truncated swereg fit is not."
  tri_s3_pp = function(ev) {
    t <- .vc_tri(ev, "s3", "pp", "sw_est")
    u <- .vc_tri(ev, "s3", "pp", "sw_est_untrunc")
    te <- .vc_tri(ev, "s3", "pp", "te_est")
    abs(t[["bias"]]) > .vc_k * t[["se"]] &&
      abs(u[["bias"]]) <= .vc_k * u[["se"]] &&
      abs(te[["bias"]]) <= .vc_k * te[["se"]]
  },

  # "In the s3 ITT cell every fit is below the truth, and each of the three
  # means is within 3.5 Monte Carlo standard errors of the exact limit of an
  # ITT fit without loss weights."
  tri_s3_itt_design = function(ev) {
    ok <- vapply(
      .vc_tri_fits,
      function(f) {
        x <- .vc_tri(ev, "s3", "itt", f)
        x[["bias"]] < 0 &&
          abs(x[["bias"]] - .vc_itt_design_limit[["s3"]]) <= .vc_k * x[["se"]]
      },
      logical(1)
    )
    all(ok)
  },

  # 3.4 -- stress matrix -----------------------------------------------------

  # "At the event risk of the rare-outcome cell, the per-protocol fit
  # completes and its interval covers the truth."
  stress_rare_covers = function(ev) {
    st <- as.data.frame(ev$stress)
    x <- st[st$cell == "rare_outcome" & st$estimand == "pp", , drop = FALSE]
    nrow(x) == 1L && is.finite(x$est) && x$lo <= x$truth && x$truth <= x$hi
  },

  # "Under a true null the interval covers zero."
  stress_null_covers = function(ev) {
    st <- as.data.frame(ev$stress)
    x <- st[st$cell == "null_effect", , drop = FALSE]
    nrow(x) == 1L && x$truth == 0 && x$lo <= 0 && 0 <= x$hi
  },

  # "In the attrition cell the per-protocol interval covers the truth, and the
  # ITT estimate is displaced and its interval excludes the truth."
  stress_attrition = function(ev) {
    st <- as.data.frame(ev$stress)
    pp <- st[st$cell == "informative_attrition" & st$estimand == "pp", ]
    itt <- st[st$cell == "informative_attrition" & st$estimand == "itt", ]
    nrow(pp) == 1L &&
      nrow(itt) == 1L &&
      pp$lo <= pp$truth && pp$truth <= pp$hi &&
      (itt$truth < itt$lo || itt$truth > itt$hi)
  },

  # "Refitting on identical data reproduced the estimate exactly."
  stress_determinism = function(ev) {
    isTRUE(ev$stress_determinism$identical) &&
      ev$stress_determinism$max_abs_delta == 0
  },

  # "The pooled IRR lies above the cumulative-rate truth on every seed, and
  # swereg and TrialEmulation agree with each other more closely than either
  # agrees with that truth."
  stress_harmful = function(ev) {
    sh <- as.data.frame(ev$stress_harmful)
    nrow(sh) == 3L &&
      all(sh$sw_est > sh$truth) &&
      max(abs(sh$sw_est - sh$te_est)) < min(sh$sw_est - sh$truth)
  },

  # "Each tightening of the truncation percentiles moves the ITT estimate
  # further toward the null." (Table 9, and 1.8.3)
  stress_trunc_monotone = function(ev) {
    tc <- as.data.frame(ev$stress_trunc)
    nrow(tc) == 3L &&
      all(tc$truth < 0) &&
      all(diff(tc$est) > 0) &&
      all(tc$est > tc$truth)
  },

  # "Time-updated censoring covariates give a smaller absolute bias than
  # covariates frozen at baseline, but a bias remains: the interval of the
  # time-updated fit excludes the truth. The ITT interval covers its truth."
  # (Table 10, and 1.7)
  stress_tv = function(ev) {
    tv <- as.data.frame(ev$stress_tv)
    up <- tv[1, ]
    fr <- tv[2, ]
    itt <- tv[3, ]
    grepl("time-updated", up$fit) &&
      grepl("frozen", fr$fit) &&
      grepl("itt", itt$fit) &&
      abs(up$est - up$truth) < abs(fr$est - fr$truth) &&
      (up$truth < up$lo || up$truth > up$hi) &&
      itt$lo <= itt$truth && itt$truth <= itt$hi
  },

  # 3.5 -- plan layer --------------------------------------------------------

  # "In B_none, the weighting removes more than 90% of the planted
  # confounding on the log scale."
  plan_ipw_removes_confounding = function(ev) {
    pl <- as.data.frame(ev$plan)
    b <- pl[pl$cell == "B_none", , drop = FALSE]
    nrow(b) == 1L &&
      abs(log(b$ipw_rr) - log(b$truth_pp)) <
        0.1 * abs(log(b$crude_rr) - log(b$truth_pp))
  },

  # "In the discontinuation cell the two estimands separate in the direction
  # of the truths, and the per-protocol interval covers the
  # sustained-treatment truth of 2.0."
  plan_disc = function(ev) {
    pl <- as.data.frame(ev$plan)
    d <- pl[pl$cell == "DISC", , drop = FALSE]
    nrow(d) == 1L &&
      d$truth_pp == 2 &&
      d$truth_pp > d$truth_itt &&
      d$pp_irr > d$itt_irr &&
      d$pp_lo <= d$truth_pp && d$truth_pp <= d$pp_hi
  },

  # "In both plan-layer Monte Carlo scenarios and for both estimands, the
  # mean log-scale bias is within 3.5 Monte Carlo standard errors of zero, and
  # at least 6 of the 8 intervals cover the truth."
  plan_mc = function(ev) {
    mc <- as.data.frame(ev$plan_mc)
    ok <- c()
    for (sc in c("A", "B")) {
      x <- mc[mc$scenario == sc, , drop = FALSE]
      for (e in c("pp", "itt")) {
        est <- x[[paste0(e, "_irr")]]
        lo <- x[[paste0(e, "_lo")]]
        hi <- x[[paste0(e, "_hi")]]
        b <- log(est) - log(x$truth)
        se <- stats::sd(log(est)) / sqrt(nrow(x))
        ok <- c(
          ok,
          nrow(x) == 8L &&
            abs(mean(b)) <= .vc_k * se &&
            sum(lo <= x$truth & x$truth <= hi) >= 6L
        )
      }
    }
    length(ok) == 4L && all(ok)
  },

  # 3.6 -- coverage calibration ---------------------------------------------

  # "The per-protocol interval stays close to nominal in all three scenarios,
  # and so does the ITT interval in s1 and s2." Close to nominal is the rule
  # of the full tier: coverage in [0.90, 0.99]. The s3 ITT row is left out:
  # its claim is the design bias (coverage_itt_s3_design), not coverage.
  coverage_nominal = function(ev) {
    cv <- as.data.frame(ev$coverage)
    x <- cv[!(cv$scenario == "s3" & cv$estimand == "itt"), , drop = FALSE]
    nrow(cv) == 6L &&
      nrow(x) == 5L &&
      all(x$M == 200L) &&
      all(x$coverage >= 0.90 & x$coverage <= 0.99)
  },

  # "Each per-protocol mean bias is less than a quarter of the spread of
  # single estimates."
  coverage_pp_bias_small = function(ev) {
    cv <- as.data.frame(ev$coverage)
    cv <- cv[cv$estimand == "pp", , drop = FALSE]
    nrow(cv) == 3L && all(abs(cv$mc_mean_bias) < 0.25 * cv$mc_sd)
  },

  # "The s3 ITT point estimate carries the design bias: it is below the truth
  # and within 3.5 Monte Carlo standard errors of the exact limit of an ITT
  # fit without loss weights. That bias is less than a third of the spread of
  # single estimates."
  coverage_itt_s3_design = function(ev) {
    cv <- as.data.frame(ev$coverage)
    x <- cv[cv$scenario == "s3" & cv$estimand == "itt", , drop = FALSE]
    se <- x$mc_sd / sqrt(x$n_fit)
    nrow(x) == 1L &&
      x$mc_mean_bias < 0 &&
      abs(x$mc_mean_bias - .vc_itt_design_limit[["s3"]]) <= .vc_k * se &&
      abs(x$mc_mean_bias) < x$mc_sd / 3
  },

  # 3.7 -- marginal versus conditional ---------------------------------------

  # "The swereg - TrialEmulation gaps of Table 4 are smaller in the ITT cells
  # of s2 and s3 than in the per-protocol cells. In s1 both gaps are below
  # 0.001."
  tri_gap_pattern = function(ev) {
    t <- as.data.frame(ev$triangle)
    g <- function(sc, e) {
      x <- t[t$scenario == sc & t$estimand == e, , drop = FALSE]
      abs(x$sw_est - x$te_est)
    }
    max(g("s2", "itt"), g("s3", "itt")) < min(g("s2", "pp"), g("s3", "pp")) &&
      g("s1", "pp") < 0.001 &&
      g("s1", "itt") < 0.001
  },

  # 3.8 -- truncation-tradeoff grid --------------------------------------------

  # "The truncated fit is above the truth in the mild, base and harsh cells,
  # and its 95% Monte Carlo interval excludes zero in each. Its bias grows
  # from the mild to the harsh cell." (also 1.8.3)
  grid_dose_truncated = function(ev) {
    g <- .vc_grid(ev)[c("mild", "base", "harsh"), ]
    nrow(g) == 3L &&
      all(g$t > 0) &&
      !any(.vc_covers0(g$t, g$t_se)) &&
      all(diff(g$t) > 0)
  },

  # "At the harshest setting the untruncated fit degrades: the spread of its
  # estimates is more than twice that of the truncated fit."
  grid_harsh_unstable = function(ev) {
    g <- .vc_grid(ev)["harsh", ]
    g$u_sd > 2 * g$t_sd
  },

  # "The 95% Monte Carlo interval of TrialEmulation holds zero in the mild,
  # base and harsh cells."
  grid_te_measured = function(ev) {
    g <- .vc_grid(ev)[c("mild", "base", "harsh"), ]
    nrow(g) == 3L && all(.vc_covers0(g$te, g$te_se))
  },

  # "With the selection reversed, only the truncated fit's 95% Monte Carlo
  # interval holds zero, and the other two fits are below zero. With a
  # harmful effect, only the untruncated fit's
  # interval holds zero, and the other two fits are above the truth."
  # (also the Figure 4 caption)
  grid_reversed_harmful = function(ev) {
    g <- .vc_grid(ev)
    r <- g["reversed", ]
    h <- g["harmful", ]
    .vc_covers0(r$t, r$t_se) &&
      r$u < 0 &&
      r$te < 0 &&
      !.vc_covers0(r$u, r$u_se) &&
      !.vc_covers0(r$te, r$te_se) &&
      .vc_covers0(h$u, h$u_se) &&
      !.vc_covers0(h$t, h$t_se) &&
      !.vc_covers0(h$te, h$te_se) &&
      h$t > 0 &&
      h$te > 0
  },

  # "No fit has the smallest absolute mean bias in every cell." Over the
  # informative-loss cells of Figure 2 (s3 and the grid cells other than
  # base) and over the grid of Table 16.
  grid_no_dominance = function(ev) {
    p <- .vc_pp_cells(ev)
    best <- function(cells) {
      x <- p[cells, c("t", "u", "te")]
      unique(apply(abs(x), 1, which.min))
    }
    fig2 <- c("s3", "mild", "harsh", "reversed", "harmful", "unmeasured_loss", "unmeasured_adherence")
    t16 <- c("mild", "base", "harsh", "reversed", "harmful", "unmeasured_loss", "unmeasured_adherence")
    all(c(fig2, t16) %in% rownames(p)) &&
      length(best(fig2)) > 1L &&
      length(best(t16)) > 1L
  },

  # "In both unmeasured-driver cells every fit is below the truth.
  # TrialEmulation is displaced the most and the truncated swereg fit the
  # least. The TrialEmulation interval excludes zero in both."
  grid_unmeasured = function(ev) {
    g <- .vc_grid(ev)[c("unmeasured_loss", "unmeasured_adherence"), ]
    nrow(g) == 2L &&
      all(g$te < g$u & g$u < g$t & g$t < 0) &&
      !any(.vc_covers0(g$te, g$te_se))
  },

  # "Truncated and untruncated mean biases differ less in the two
  # unmeasured-driver cells than in any measured-covariate cell."
  grid_unmeasured_silent = function(ev) {
    g <- .vc_grid(ev)
    d <- abs(g$t - g$u)
    unm <- g$cell %in% c("unmeasured_loss", "unmeasured_adherence")
    sum(unm) == 2L && sum(!unm) == 5L && max(d[unm]) < min(d[!unm])
  },

  # "Every approach in the feedback cell is biased, more than 3.5 Monte Carlo
  # standard errors from zero, and by more than twice the largest absolute
  # mean bias of the truncated swereg fit and of TrialEmulation in Table 16.
  # The time-updated covariate gives the smallest bias of the three." (also
  # 1.7)
  feedback_boundary = function(ev) {
    fb <- as.data.frame(ev$feedback_grid)
    n <- nrow(fb)
    b <- sapply(fb[, c("b_sw_updated", "b_sw_frozen", "b_te_baseline")], mean)
    se <- sapply(fb[, c("b_sw_updated", "b_sw_frozen", "b_te_baseline")], stats::sd) / sqrt(n)
    g <- .vc_grid(ev)
    n == 10L &&
      all(abs(b) > .vc_k * se) &&
      min(abs(b)) > 2 * max(abs(c(g$t, g$te))) &&
      which.min(abs(b)) == 1L
  },

  # "Truncation moves the mean bias up in the mild, base, reversed and
  # harmful-effect cells, and down in the harsh cell, where the untruncated
  # weights are unstable. In s1 it changes no estimate." (Figure 4)
  grid_truncation_direction = function(ev) {
    p <- .vc_pp_cells(ev)
    r <- as.data.frame(ev$triangle_reps)
    r <- r[r$scenario == "s1" & r$estimand == "pp", , drop = FALSE]
    all(p[c("mild", "base", "reversed", "harmful"), "t"] >
      p[c("mild", "base", "reversed", "harmful"), "u"]) &&
      p["harsh", "t"] < p["harsh", "u"] &&
      max(abs(r$sw_est - r$sw_est_untrunc)) < 0.0005
  },

  # "The truncated fit never has a larger spread than the untruncated fit,
  # to 0.001, and its largest advantage is in the harsh cell." (Figure 5, and
  # 1.8.3)
  pp_spread = function(ev) {
    p <- .vc_pp_cells(ev)
    nrow(p) == 10L &&
      all(p$t_sd <= p$u_sd + 0.001) &&
      rownames(p)[which.max(p$u_sd / p$t_sd)] == "harsh"
  },

  # "The truncated fit has the lower root-mean-squared error in most cells.
  # Its two largest advantages are in the harsh and harmful-effect cells.
  # The untruncated fit has the lower error in the remaining cells, where it
  # is less biased. No
  # estimation route has the lowest error in every cell. The largest error of
  # the truncated fit is below that of the untruncated fit." (Figure 6, the
  # recommendation, and 1.8.3)
  pp_rmse = function(ev) {
    p <- .vc_pp_cells(ev)
    tl <- p$t_rmse < p$u_rmse
    ratio <- p$u_rmse / p$t_rmse
    top2 <- rownames(p)[order(ratio, decreasing = TRUE)][1:2]
    ul <- !tl & p$u_rmse < p$t_rmse
    winners <- unique(apply(p[, c("t_rmse", "u_rmse", "te_rmse")], 1, which.min))
    nrow(p) == 10L &&
      sum(tl) + sum(ul) == nrow(p) &&
      sum(tl) > nrow(p) / 2 &&
      sum(ul) >= 1L &&
      setequal(top2, c("harsh", "harmful")) &&
      all(abs(p$u[ul]) < abs(p$t[ul])) &&
      length(winners) > 1L &&
      max(p$t_rmse) < max(p$u_rmse)
  },

  # 3.9 -- exact truths -------------------------------------------------------

  # "In s1 the hazard ratio is constant, and the stabilised and uncensored
  # log-IRR truths are equal."
  exact_s1_truths_equal = function(ev) {
    vt <- as.data.frame(ev$validation_truth)
    vt <- vt[vt$quantity == "log_irr" & vt$scenario == "s1", , drop = FALSE]
    nrow(vt) == 2L && all(abs(vt$truth - vt$truth_uncensored) < 1e-8)
  },

  # "In s1, every estimate is within 3.5 Monte Carlo standard errors of the
  # exact truth: both estimands, both weights, the log-IRR and the risk
  # difference at every horizon."
  exact_s1_unbiased = function(ev) {
    .vc_unbiased(.vc_exact(ev, "s1"), 16L)
  },

  # "With untruncated weights, the per-protocol estimates are within 3.5 Monte
  # Carlo standard errors of the exact truth in all four scenarios."
  exact_pp_untruncated_unbiased = function(ev) {
    .vc_unbiased(
      .vc_exact(ev, c("s1", "s2", "s3", "s4"), "pp", "untruncated"),
      16L
    )
  },

  # "The ITT estimates in s2 are within 3.5 Monte Carlo standard errors of the
  # exact truth with both weights."
  exact_itt_s2_unbiased = function(ev) {
    .vc_unbiased(.vc_exact(ev, "s2", "itt"), 8L)
  },

  # "Truncation moves the per-protocol risk difference at h = 20 up, away
  # from the truth, in s2, s3 and s4. In s1 it moves no estimate by more than
  # 0.0005."
  exact_pp_truncation_shift = function(ev) {
    tb <- vapply(
      c("s2", "s3", "s4"),
      function(s) .vc_exact1(ev, s, "pp", "truncated", "rd_h20"),
      numeric(1)
    )
    ub <- vapply(
      c("s2", "s3", "s4"),
      function(s) .vc_exact1(ev, s, "pp", "untruncated", "rd_h20"),
      numeric(1)
    )
    d <- tb - ub
    t1 <- .vc_exact(ev, "s1", "pp", "truncated")
    u1 <- .vc_exact(ev, "s1", "pp", "untruncated")
    u1 <- u1[match(t1$quantity, u1$quantity), , drop = FALSE]
    all(d > 0) &&
      all(abs(tb) > abs(ub)) &&
      nrow(t1) == 4L &&
      all(abs(t1$mean_estimate - u1$mean_estimate) < 0.0005)
  },

  # "With truncated weights, the per-protocol risk difference at h = 20 is
  # more than 3.5 Monte Carlo standard errors above the truth in s3 and s4."
  exact_pp_truncated_biased_s3_s4 = function(ev) {
    s <- .vc_exact(ev, c("s3", "s4"), "pp", "truncated", "rd_h20")
    nrow(s) == 2L && all(s$z > .vc_k)
  },

  # "The ITT log-IRR in s3 and s4 is below the truth with both weights. With
  # untruncated weights it is within 3.5 Monte Carlo standard errors of the
  # exact limit of an ITT fit without loss weights."
  exact_itt_design_bias = function(ev) {
    s <- .vc_exact(ev, c("s3", "s4"), "itt", NULL, "log_irr")
    u <- s[s$weight == "untruncated", , drop = FALSE]
    lim <- .vc_itt_design_limit[u$scenario]
    nrow(s) == 4L &&
      all(s$bias < 0) &&
      all(abs(u$bias - lim) <= .vc_k * u$mc_se)
  },

  # "The 95% bootstrap interval of the per-protocol risk difference covers the
  # exact truth in 90% to 99% of 200 replicates at every horizon, in s1, s2
  # and s4."
  rd_coverage_nominal = function(ev) {
    cv <- as.data.frame(ev$rd_coverage)
    nrow(cv) == 9L &&
      setequal(cv$scenario, c("s1", "s2", "s4")) &&
      all(cv$R == 200L) &&
      all(cv$coverage >= 0.90 & cv$coverage <= 0.99)
  }
)
