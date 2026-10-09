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
# default censoring weights: a GAM per arm.
val_prepare <- function(d, scenario, estimand) {
  long <- if (identical(scenario, "s4")) scen_long_s4(d) else tte_build_long(d)
  trial <- TTEEnrollment$new(long, val_design(scenario, long))
  trial$s2_ipw(stabilize = TRUE)
  trial$s3_truncate_weights(lower = 0.01, upper = 0.99)
  trial$s4_prepare_for_analysis(
    outcome = "event",
    follow_up = .SCEN_T,
    estimand = estimand
  )
  trial
}

# One replicate: simulate at `seed`, prepare, and estimate the log-IRR and the
# risk difference at .VAL_HORIZONS for each weight in `weights`. The risk
# difference takes one bootstrap replicate, because only its point estimate is
# used. Returns one row per weight and quantity.
val_fit_replicate <- function(
  scenario,
  estimand,
  seed,
  N = 20000L,
  weights = "truncated"
) {
  d <- scen_simulate(scenario, N = N, seed = seed)
  trial <- val_prepare(d, scenario, estimand)
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
