# Validation helpers shared by the fast tier (test-validation-fast.R), the full
# tier (test-validation-full.R), the evidence version test and the evidence
# generator (dev/generate_validation_evidence.R). Each replicate runs the
# scenario helpers of helper-tte_scenarios.R through swereg. Each estimate is
# compared with an exact truth. The risk difference uses
# scen_truth_risk_exact(), and the log-IRR uses scen_truth_irr_exact().

# The horizons, in weeks, of every risk-difference check.
.VAL_HORIZONS <- c(5L, 10L, 20L)

# Replicates per cell of the exact-truth bias check. test-validation-full.R
# and dev/generate_validation_evidence.R both read val_n_replicates(), so the
# two cannot disagree. s3 runs 60 to make the Monte Carlo standard error of
# its per-protocol residual bias smaller.
.VAL_R <- 20L
.VAL_R_S3 <- 60L
val_n_replicates <- function(scenario) {
  if (identical(scenario, "s3")) {
    return(.VAL_R_S3)
  }
  return(.VAL_R)
}

# The weight column of each estimand, primary (truncated at the 1st and 99th
# percentiles) and untruncated.
.VAL_WEIGHTS <- list(
  pp = c(truncated = "analysis_weight_pp_trunc", untruncated = "analysis_weight_pp"),
  itt = c(truncated = "ipw_trunc", untruncated = "ipw")
)

# The estimator source files. A change to any of them makes the validation
# evidence stale.
.VAL_ESTIMATOR_FILES <- c(
  "R/tte_boundaries.R",
  "R/r6_tteenrollment.R",
  "R/r6_tteenrollment_weighting.R",
  "R/tte_ipcw_helpers.R",
  "R/tte_estimation.R",
  "R/tte_risk_difference.R"
)

# The md5 of the estimator source files, read from the package root `root`.
# It is the md5 of one line per file, `<path> <md5 of the file>`, in the order
# of .VAL_ESTIMATOR_FILES. A missing file is an error, never a skipped file.
val_estimator_hash <- function(root) {
  paths <- file.path(root, .VAL_ESTIMATOR_FILES)
  missing <- .VAL_ESTIMATOR_FILES[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop(
      "estimator file(s) not found under ",
      root,
      ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  lines <- paste(.VAL_ESTIMATOR_FILES, unname(tools::md5sum(paths)))
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(lines, tmp)
  unname(tools::md5sum(tmp))
}

# The package root as seen from the tests: the source tree under
# testthat::test_local() and devtools::test(), or the unpacked source in
# <pkg>.Rcheck/00_pkg_src/swereg under R CMD check. NULL when neither holds
# DESCRIPTION.
val_package_root <- function() {
  cands <- c(
    testthat::test_path("..", ".."),
    testthat::test_path("..", "..", "00_pkg_src", "swereg")
  )
  for (p in cands) {
    if (file.exists(file.path(p, "DESCRIPTION"))) {
      return(normalizePath(p))
    }
  }
  NULL
}

# The design of a scenario panel. s4 adjusts for both baseline covariates.
val_design <- function(scenario, long) {
  TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    person_id_var = "enrollment_person_trial_id",
    treatment_var = "treatment_baseline",
    time_treatment_var = "time_treatment",
    outcome_vars = "event",
    confounder_vars = if (identical(scenario, "s4")) {
      c("baseline_L0", "baseline_L1")
    } else {
      "baseline_L0"
    },
    follow_up_time = .SCEN_T
  )
}

# One prepared enrollment: stabilised treatment weights, truncated at the
# 1st and 99th percentiles, then s4 for the estimand. Per-protocol uses the
# default censoring weights: a GAM per arm. `path = "panel"` builds the panel
# with tte_build_long() or scen_long_s4(). `path = "enroll"` builds it with
# enroll() from weekly rows (val_enroll_weekly()).
val_prepare <- function(d, scenario, estimand, path = c("panel", "enroll")) {
  path <- match.arg(path)
  if (path == "enroll") {
    trial <- val_enroll_weekly(d, scenario)
    outcome <- "died"
  } else {
    long <- if (identical(scenario, "s4")) scen_long_s4(d) else tte_build_long(d)
    trial <- TTEEnrollment$new(long, val_design(scenario, long))
    outcome <- "event"
  }
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  trial$s4_prepare_for_analysis(
    outcome = outcome,
    follow_up = .SCEN_T,
    estimand = estimand
  )
  trial
}

# --- the weekly path through enroll() -----------------------------------------
# A panel from tte_build_long() has no weekly rows, so s5_prepare_outcome()
# reads the deviation from the treatment of each follow-up interval. The
# helpers below turn the same simulation into one row per person and week.
# enroll() then builds the panel and places the weekly deviation boundary
# (.tte_deviation_boundary(), R/tte_boundaries.R). One week is one enrollment
# period and one follow-up interval, so the panel holds the rows of
# tte_build_long(). Every person is eligible in the first week only, and the
# comparator draw takes every comparator. Period t of the simulation is the
# follow-up week [t, t + 1). s1 to s3 only: s4 has time-zero discordance.

# One row per person and week: the enrollment week, then the .SCEN_T weeks of
# follow-up. A person keeps the rows of every period the simulation holds.
val_weekly_rows <- function(d) {
  wk <- cstime::dates_by_isoyearweek$isoyearweek
  wk <- wk[wk >= "2020-01"][seq_len(.SCEN_T + 1L)]
  base <- d[period == 0L, list(id, arm = A_t == 1L, baseline_L0 = L0)]
  enr <- base[, list(
    id,
    isoyearweek = wk[1L],
    exposed = arm,
    eligible = TRUE,
    on_tx = arm,
    died = FALSE,
    baseline_L0
  )]
  fu <- d[base, on = "id"][, list(
    id,
    isoyearweek = wk[period + 2L],
    exposed = arm,
    eligible = FALSE,
    on_tx = A_t == 1L,
    died = Y_t == 1L,
    baseline_L0
  )]
  out <- data.table::rbindlist(list(enr, fu))
  data.table::setkeyv(out, c("id", "isoyearweek"))
  return(out[])
}

# The design of the weekly rows. The `row_presence` sentinel makes enroll()
# read the weekly boundaries.
val_weekly_design <- function() {
  design <- TTEDesign$new(
    person_id_var = "id",
    treatment_var = "exposed",
    time_treatment_var = "on_tx",
    eligible_var = "eligible",
    observed_var = list(sentinel = "row_presence"),
    outcome_vars = "died",
    confounder_vars = "baseline_L0",
    follow_up_time = .SCEN_T,
    period_width = 1L
  )
  return(design)
}

# The enrolled panel of the simulation `d`. A ratio of 100 draws every
# comparator.
val_enroll_weekly <- function(d, scenario) {
  stopifnot(scenario %in% c("s1", "s2", "s3"))
  trial <- TTEEnrollment$new(
    val_weekly_rows(d),
    val_weekly_design(),
    ratio = 100,
    seed = 1L,
    own_data = TRUE
  )
  return(trial)
}

# The deviation boundary that enroll() MUST give each person of `d`, from the
# simulated treatment alone: the first period whose treatment differs from the
# arm. Period t is the follow-up week [t, t + 1), so the boundary is its left
# edge, t. NA when the treatment never differs. One row per person.
val_weekly_deviation <- function(d) {
  x <- d[d[period == 0L, list(id, arm = A_t)], on = "id"]
  dev <- x[A_t != arm, list(expected = min(period)), by = "id"]
  out <- data.table::data.table(id = sort(unique(d$id)))
  out[dev, expected := i.expected, on = "id"]
  return(out[])
}

# One replicate: simulate at `seed`, prepare along `path` (val_prepare()), and
# estimate the log-IRR and the risk difference at .VAL_HORIZONS for each
# weight in `weights`. The risk difference takes one bootstrap replicate,
# because only its point estimate is used. Returns one row per weight and
# quantity.
val_fit_replicate <- function(
  scenario,
  estimand,
  seed,
  N = 20000L,
  weights = "truncated",
  path = "panel"
) {
  d <- scen_simulate(scenario, N = N, seed = seed)
  trial <- val_prepare(d, scenario, estimand, path = path)
  out <- list()
  for (w in weights) {
    col <- .VAL_WEIGHTS[[estimand]][[w]]
    irr <- trial$irr(col)
    rd <- trial$risk_difference(col, n_boot = 1L, seed = 1L)
    out[[w]] <- data.table::data.table(
      scenario = scenario,
      estimand = estimand,
      seed = as.integer(seed),
      weight = w,
      quantity = c("log_irr", paste0("rd_h", .VAL_HORIZONS)),
      estimate = c(log(irr$IRR), rd$rd[match(.VAL_HORIZONS, rd$tstop)])
    )
  }
  data.table::rbindlist(out)
}

# The number of worker processes: `max`, or fewer when the machine has fewer
# cores, and 1 where forking is unavailable. R CMD check --as-cran sets
# _R_CHECK_LIMIT_CORES_ and then stops a process that spawns more than 2, so
# the count is at most 2 there. Each replicate sets its own seeds, so the
# result does not depend on the count.
val_cores <- function(max = 2L) {
  if (.Platform$OS.type != "unix") {
    return(1L)
  }
  limit <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(limit) && limit != "false") {
    max <- min(max, 2L)
  }
  as.integer(max(1L, min(max, parallel::detectCores(), na.rm = TRUE)))
}

# FUN over seq_len(R) in `cores` forked processes. A replicate that fails
# stops the whole run with its error message.
val_map <- function(R, FUN, cores) {
  res <- if (cores > 1L) {
    parallel::mclapply(seq_len(R), FUN, mc.cores = cores, mc.preschedule = FALSE)
  } else {
    lapply(seq_len(R), FUN)
  }
  bad <- vapply(res, inherits, logical(1), what = "try-error")
  if (any(bad)) {
    stop("replicate ", which(bad)[1L], " failed: ", res[[which(bad)[1L]]])
  }
  data.table::rbindlist(res)
}

# R replicates at seeds seed0 + 1, ..., seed0 + R.
val_replicates <- function(
  scenario,
  estimand,
  R,
  seed0 = 2100L,
  N = 20000L,
  weights = "truncated",
  cores = 1L
) {
  val_map(
    R,
    function(r) {
      val_fit_replicate(scenario, estimand, seed0 + r, N = N, weights = weights)
    },
    cores
  )
}

# The exact truth of each quantity: the log-IRR from scen_truth_irr_exact()
# (stabilised person-time) and the risk difference at .VAL_HORIZONS from
# scen_truth_risk_exact().
val_truth <- function(scenario, estimand) {
  rk <- scen_truth_risk_exact(scenario, estimand)
  data.table::data.table(
    quantity = c("log_irr", paste0("rd_h", .VAL_HORIZONS)),
    truth = c(
      as.numeric(scen_truth_irr_exact(scenario, estimand)),
      rk$rd[match(.VAL_HORIZONS, rk$h)]
    )
  )
}

# The exact limit of an ITT fit without loss weights, minus the exact log-IRR
# truth scen_truth_irr_exact(scenario, "itt"). ITT carries no loss weight, so
# where loss depends on a covariate of the outcome (L0 in s3, L1 in s4) its
# estimate converges here and not to the truth. The cells are those of
# scen_irr_cells_exact(): the treatment-weighted population in each arm and
# week, with each row kept with its probability of not yet being lost. The
# knots and the model are those of scen_truth_irr_exact(). It is zero where
# loss is absent or independent. In phase 5e (2026-10-07) it measured
# -0.0268 in s3 and -0.0161 in s4.
val_itt_design_limit <- function(scenario) {
  g <- scen_grid_exact(scenario)
  wq <- g$wq
  out <- vector("list", 2L * .SCEN_T)
  for (a in 0:1) {
    p_a <- if (a == 1L) g$p_arm else 1 - g$p_arm
    pi_a <- sum(wq * p_a)
    v1 <- g$p_a0(a)
    v0 <- 1 - v1
    for (t in 0:(.SCEN_T - 1L)) {
      if (t > 0L) {
        n1 <- v0 * g$s0 + v1 * g$s1
        n0 <- v0 * (1 - g$s0) + v1 * (1 - g$s1)
        v0 <- n0
        v1 <- n1
      }
      kept <- (1 - g$hl)^t
      out[[a * .SCEN_T + t + 1L]] <- data.table::data.table(
        arm = a,
        tstart = t,
        pt = pi_a * sum(wq * (v0 + v1) * kept),
        events = pi_a * sum(wq * (v0 * g$h0 + v1 * g$h1) * kept),
        rows = sum(wq * p_a * (v0 + v1) * kept)
      )
      v0 <- v0 * (1 - g$h0)
      v1 <- v1 * (1 - g$h1)
    }
  }
  cells <- data.table::rbindlist(out)
  knots <- scen_irr_knots_exact(cells)
  fit <- stats::glm(
    events ~ arm +
      splines::ns(tstart, knots = knots, Boundary.knots = c(0, .SCEN_T - 1L)) +
      offset(log(pt)),
    data = cells,
    family = stats::quasipoisson(),
    control = stats::glm.control(epsilon = 1e-14, maxit = 100L)
  )
  limit <- unname(stats::coef(fit)[["arm"]])
  return(limit - as.numeric(scen_truth_irr_exact(scenario, "itt")))
}

# Mean bias against the exact truth, with the Monte Carlo standard error
# mc_se = sd(estimates) / sqrt(R) and z = bias / mc_se. One row per weight and
# quantity.
val_summary <- function(reps, truth = NULL) {
  if (is.null(truth)) {
    truth <- val_truth(reps$scenario[1L], reps$estimand[1L])
  }
  x <- merge(reps, truth, by = "quantity")
  x[,
    list(
      R = .N,
      truth = truth[1L],
      mean_estimate = mean(estimate),
      bias = mean(estimate - truth),
      sd = stats::sd(estimate),
      mc_se = stats::sd(estimate) / sqrt(.N),
      z = mean(estimate - truth) / (stats::sd(estimate) / sqrt(.N))
    ),
    by = c("scenario", "estimand", "weight", "quantity")
  ]
}

# Each mean bias in `s` within k Monte Carlo standard errors.
val_expect_unbiased <- function(s, k = 3.5) {
  for (i in seq_len(nrow(s))) {
    r <- s[i]
    testthat::expect_lte(
      abs(r$bias),
      k * r$mc_se,
      label = sprintf(
        "|bias| of %s %s %s %s (bias %.5f, MC SE %.5f, z %.2f)",
        r$scenario,
        r$estimand,
        r$weight,
        r$quantity,
        r$bias,
        r$mc_se,
        r$z
      )
    )
  }
}

# The mean bias of each row of `b` inside its stated band [lo, hi], for a
# cell whose bias is known by design. `b` is a val_summary() result with the
# columns lo and hi added.
val_expect_band <- function(b) {
  for (i in seq_len(nrow(b))) {
    r <- b[i]
    lab <- sprintf(
      "bias of %s %s %s %s (bias %.5f, MC SE %.5f, band [%.3f, %.3f])",
      r$scenario,
      r$estimand,
      r$weight,
      r$quantity,
      r$bias,
      r$mc_se,
      r$lo,
      r$hi
    )
    testthat::expect_gte(r$bias, r$lo, label = lab)
    testthat::expect_lte(r$bias, r$hi, label = lab)
  }
}

# Checks every row of the summary `s`. A row with a stated band in `bands`
# has its mean bias inside [lo, hi]. Every other row is unbiased within k
# Monte Carlo standard errors. `bands` has the columns scenario, estimand,
# weight, quantity, lo and hi.
val_expect_cells <- function(s, bands, k = 3.5) {
  key <- c("scenario", "estimand", "weight", "quantity")
  banded <- merge(s, bands, by = key)
  val_expect_unbiased(s[!banded, on = key], k = k)
  val_expect_band(banded)
}

# The coverage of each horizon of a val_rd_coverage() result inside
# [lo, hi]. Returns the coverage table.
val_expect_coverage <- function(cov, lo = 0.90, hi = 0.99) {
  tab <- cov[,
    list(R = .N, covered_n = sum(covered), coverage = mean(covered)),
    by = c("scenario", "estimand", "h")
  ]
  message(sprintf(
    "RD coverage %s %s: %s",
    tab$scenario[1L],
    tab$estimand[1L],
    paste(sprintf("h = %d %d/%d", tab$h, tab$covered_n, tab$R), collapse = ", ")
  ))
  for (i in seq_len(nrow(tab))) {
    r <- tab[i]
    lab <- sprintf(
      "RD coverage of %s %s at h = %d (%d of %d)",
      r$scenario,
      r$estimand,
      r$h,
      r$covered_n,
      r$R
    )
    testthat::expect_gte(r$coverage, lo, label = lab)
    testthat::expect_lte(r$coverage, hi, label = lab)
  }
  tab
}

# Monte Carlo coverage of the bootstrap risk-difference interval: R
# replicates at seeds seed0 + r, each with n_boot bootstrap replicates. Returns
# one row per replicate and horizon, with the interval and whether it holds
# the exact truth.
val_rd_coverage <- function(
  scenario,
  estimand,
  R = 200L,
  n_boot = 200L,
  seed0 = 3100L,
  N = 20000L,
  weight = "truncated",
  cores = 1L
) {
  rk <- scen_truth_risk_exact(scenario, estimand)
  truth <- rk$rd[match(.VAL_HORIZONS, rk$h)]
  col <- .VAL_WEIGHTS[[estimand]][[weight]]
  val_map(R, cores = cores, FUN = function(r) {
    d <- scen_simulate(scenario, N = N, seed = seed0 + r)
    trial <- val_prepare(d, scenario, estimand)
    rd <- trial$risk_difference(col, n_boot = n_boot, seed = seed0 + r)
    i <- match(.VAL_HORIZONS, rd$tstop)
    data.table::data.table(
      scenario = scenario,
      estimand = estimand,
      seed = as.integer(seed0 + r),
      h = .VAL_HORIZONS,
      truth = truth,
      rd = rd$rd[i],
      rd_lo = rd$rd_lo[i],
      rd_hi = rd$rd_hi[i],
      covered = rd$rd_lo[i] <= truth & truth <= rd$rd_hi[i]
    )
  })
}
