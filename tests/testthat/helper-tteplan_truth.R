# Shared DGP + spec + pipeline-driver helpers for the PLAN-layer truth matrix
# (test-tteplan_truth_matrix.R).
#
# Everything here drives the full TTEPlan sequential-trials pipeline --
# spec YAML -> skeleton .qs2 -> $s1_generate_enrollments_and_ipw() (eligibility,
# per-band matching_ratio downsampling, IPW) -> $s2_generate_analysis_files_
# and_ipcw_pp() (PP + ITT files, IPCW) -> $s3_analyze() (svyglm IRR) -- against
# a synthetic skeleton with KNOWN planted truth. The TTEEnrollment layer is
# already truth-validated (helper-tte_itt.R / helper-tte_scenarios.R); these
# helpers validate the *plan* layer on top: trial-band assignment, sequential
# enrollment, matching, pooling across bands, and the subprocess worker chain.
#
# Planted truth: constant per-week outcome hazard, TTM_H0 untreated and
# TTM_H0 * TTM_IRR_TRUE while treated => the marginal per-week rate ratio
# among enrolled trials equals TTM_IRR_TRUE (exactly, when hazards do not vary
# by any other factor; see the frailty note on scenario "B" below).

TTM_IRR_TRUE <- 2.0
TTM_H0 <- 0.0025 # untreated per-week hazard
TTM_RR_HR <- 2.0 # highrisk hazard multiplier (scenario "B")

# ---- DGP ---------------------------------------------------------------------
# Two disjoint populations; switching only via optional discontinuation:
#   * never-treaters : rd_tx = "control" every week -> comparator pool
#   * initiators     : rd_tx = "none" until a random initiation band start,
#                      then "treated" for dur ~ 1 + Geom(disc_hazard) weeks,
#                      then "none" again. Post-discontinuation "none" maps to
#                      rd_intervention = NA, which s5_prepare_outcome() counts
#                      as a protocol deviation: PP censors there, ITT does not.
# "none" also fails eligible_valid_treatment, and the spec's new-user
# (no_prior_intervention, lifetime) exclusion blocks re-enrollment, so each
# initiator enrolls exactly once, as intervention, at their initiation band.
# disc_hazard = 0 -> full persistence -> PP truth == ITT truth == TTM_IRR_TRUE.
#
# scenario "B" adds a baseline confounder ri_highrisk (30% prevalence) that
# doubles BOTH the initiation probability and the outcome hazard. The
# conditional IRR stays TTM_IRR_TRUE; the *marginal* first-event rate ratio is
# frailty-attenuated to 1.9818 (analytic: rate = h exactly for a constant
# discrete hazard, mixture-weighted over highrisk/lowrisk person-time with
# first-event depletion), i.e. -0.009 on the log scale -- absorbed in the
# test tolerances.
#
# loss: persons stop contributing skeleton rows after drop ~ 1 + Geom(h_loss).
#   "independent" : h_loss = 0.02 for everyone
#   "informative" : h_loss = 0.01 lowrisk / 0.03 highrisk (3x)
# Loss multiplies person-time by the same factor in both arms, so the marginal
# IRR truth is unchanged -- it is a nuisance the machinery must tolerate, not
# part of the estimand.
ttm_skeleton <- function(
  scenario = c("A", "B"),
  n_persons,
  loss = c("none", "independent", "informative"),
  disc_hazard = 0,
  date_min = "2016-01-01",
  date_max = "2021-06-30",
  n_init_bands = 56L,
  seed = 2026L
) {
  scenario <- match.arg(scenario)
  loss <- match.arg(loss)
  set.seed(seed)

  ids <- seq_len(n_persons)
  sk <- swereg::create_skeleton(ids, as.Date(date_min), as.Date(date_max))
  sk <- sk[is_isoyear == FALSE] # weekly rows only

  wk <- data.table::data.table(isoyearweek = sort(unique(sk$isoyearweek)))
  wk[,
    week_index := match(
      isoyearweek,
      cstime::dates_by_isoyearweek$isoyearweek
    )
  ]
  wk[, trial_id := (week_index - 1L) %/% 4L] # .assign_trial_ids banding rule
  wk[, wrank := seq_len(.N)] # 1-based week rank within the study window
  sk[wk, `:=`(trial_id = i.trial_id, .wrank = i.wrank), on = "isoyearweek"]
  min_band <- min(wk$trial_id)
  band_start <- wk[, .(start_rank = min(wrank)), by = trial_id]

  highrisk <- stats::rbinom(n_persons, 1L, 0.30)
  p_init <- if (scenario == "A") {
    rep(0.50, n_persons)
  } else {
    data.table::fifelse(highrisk == 1L, 0.90, 0.45) # highrisk ~2x initiation
  }
  is_init <- stats::rbinom(n_persons, 1L, p_init)
  init_band <- min_band +
    (sample.int(n_init_bands, n_persons, replace = TRUE) - 1L)
  init_rank <- band_start$start_rank[match(init_band, band_start$trial_id)]
  # dur is numeric so that Inf (never discontinue) survives `.ir + .dur`
  # without integer overflow
  dur <- if (disc_hazard > 0) {
    1 + stats::rgeom(n_persons, disc_hazard)
  } else {
    rep(Inf, n_persons)
  }
  baseline_age <- stats::runif(n_persons, 50, 60)

  pd <- data.table::data.table(
    id = ids,
    highrisk = highrisk,
    is_init = is_init,
    init_rank = init_rank,
    dur = dur,
    baseline_age = baseline_age
  )
  sk[
    pd,
    `:=`(
      .hr = i.highrisk,
      .init = i.is_init,
      .ir = i.init_rank,
      .dur = i.dur,
      .age = i.baseline_age
    ),
    on = "id"
  ]

  sk[,
    rd_tx := data.table::fcase(
      .init == 0L                                       , "control" ,
      .init == 1L & .wrank >= .ir & .wrank < .ir + .dur , "treated" ,
      default = "none"
    )
  ]
  sk[, rd_age_continuous := .age]
  sk[, ri_highrisk := as.integer(.hr)]

  # outcome hazard depends on CURRENT treatment (+ frailty in scenario B)
  hz <- data.table::fifelse(
    sk$rd_tx == "treated",
    TTM_H0 * TTM_IRR_TRUE,
    TTM_H0
  )
  if (scenario == "B") {
    hz <- hz * data.table::fifelse(sk$ri_highrisk == 1L, TTM_RR_HR, 1)
  }
  sk[, osd_a := stats::rbinom(.N, 1L, hz)]

  if (loss != "none") {
    h_loss <- if (loss == "independent") {
      rep(0.02, n_persons)
    } else {
      data.table::fifelse(highrisk == 1L, 0.03, 0.01)
    }
    drop_rank <- 1L + stats::rgeom(n_persons, h_loss) # >= 1 observed week
    sk[, .drop := drop_rank[id]]
    sk <- sk[.wrank <= .drop]
    sk[, .drop := NULL]
  }

  sk[, c(".hr", ".init", ".ir", ".dur", ".age", ".wrank", "trial_id") := NULL]
  data.table::setkey(sk, id, isoyearweek)
  sk[]
}

# ---- ITT truth under discontinuation -----------------------------------------
# Marginal do(initiate at week 0, then natural discontinuation) vs do(control)
# first-event rate ratio over `weeks`, person-time-at-risk denominator --
# the estimand the plan's ITT arm targets in the discontinuation cell.
ttm_disc_itt_truth <- function(
  disc_hazard,
  weeks = 52L,
  n_truth = 2e5,
  seed = 999
) {
  set.seed(seed)
  dur <- 1L + stats::rgeom(n_truth, disc_hazard)
  rate <- numeric(2)
  for (i in 1:2) {
    treated_arm <- (i == 2L)
    at_risk <- rep(TRUE, n_truth)
    ev <- 0
    pt <- 0
    for (t in seq_len(weeks)) {
      h <- if (treated_arm) {
        data.table::fifelse(t <= dur, TTM_H0 * TTM_IRR_TRUE, TTM_H0)
      } else {
        rep(TTM_H0, n_truth)
      }
      y <- stats::rbinom(n_truth, 1L, h)
      pt <- pt + sum(at_risk)
      new_ev <- at_risk & (y == 1L)
      ev <- ev + sum(new_ev)
      at_risk <- at_risk & !new_ev
    }
    rate[i] <- ev / pt
  }
  rate[2] / rate[1]
}

# ---- spec YAML ---------------------------------------------------------------
ttm_write_spec <- function(path, project_prefix, confounder_vars) {
  confs <- lapply(confounder_vars, function(v) {
    list(name = v, implementation = list(variable = v))
  })
  spec <- list(
    study = list(
      title = "Plan-layer truth matrix",
      design = "Sequential target trial emulation",
      principal_investigator = "truth-matrix test",
      implementation = list(project_prefix = project_prefix, version = "v001")
    ),
    inclusion_criteria = list(isoyears = c(2016L, 2021L)),
    exclusion_criteria = list(
      list(
        name = "No prior intervention (new-user)",
        implementation = list(
          type = "no_prior_intervention",
          source_variable = "rd_tx",
          intervention_value = "treated",
          window = "lifetime_before_baseline",
          computed = TRUE
        )
      )
    ),
    confounders = confs,
    outcomes = list(
      list(
        name = "Outcome A",
        description = "planted constant-hazard event",
        implementation = list(variable = "osd_a")
      )
    ),
    follow_up = list(list(label = "1 year", weeks = 52L)),
    enrollments = list(
      list(
        id = "01",
        name = "Treated vs control",
        additional_inclusion = list(
          list(
            name = "Age 40-80",
            type = "age_range",
            min = 40,
            max = 80,
            implementation = list(variable = "rd_age_continuous")
          )
        ),
        treatment = list(
          arms = list(intervention = "Treated", comparator = "Control"),
          implementation = list(
            matching_ratio = 2L,
            variable = "rd_tx",
            intervention_value = "treated",
            comparator_value = "control",
            seed = 1L
          )
        )
      )
    )
  )
  yaml::write_yaml(spec, path)
  invisible(path)
}

# ---- dev-path detection for the subprocess workers ---------------------------
# The s1/s2/s3 workers run as fresh Rscript subprocesses (batchit's ONE generic
# worker) that either devtools::load_all() swereg's source tree (when
# swereg_dev_path is given) or requireNamespace() the installed package. Under
# pkgload::load_all in the test session, the workers must be pointed at the
# source tree -- the INSTALLED copy may be a stale prior release, and the batch
# worker fails loudly on it (.batch_execute hash mismatch) rather than silently
# running old code.
#
# This is exactly what the production resolver .swereg_dev_path() decides, so we
# defer to it: it returns swereg's source root under load_all() and NULL under an
# installed package (R CMD check), where the workers use the installed namespace.
# (It used to probe inst/batch_worker.R directly, but that worker script now
# ships with batchit, not swereg -- Phase 4.)
ttm_dev_path <- function() {
  swereg:::.swereg_dev_path()
}

# ---- drive the full plan pipeline for one cell --------------------------------
# Builds the spec/tteplan/results/meta directory layout under a tempdir, runs
# s1 -> s2 -> s3 with 1 worker, and returns the recovered IRRs plus the crude
# and IPW-weighted marginal rate ratios off the ITT analysis file.
ttm_run_cell <- function(sk, cell_name, confounder_vars) {
  root <- tempfile(paste0("ttm_", cell_name, "_"))
  dir_spec <- file.path(root, "spec")
  dir_tteplan <- file.path(root, "tteplan")
  dir_results <- file.path(root, "results")
  dir_meta <- file.path(root, "meta")
  for (d in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  skel_path <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  global_max <- sk[, max(isoyearweek, na.rm = TRUE)]
  ttm_write_spec(
    file.path(dir_spec, "spec_v001.yaml"),
    paste0("ttm", gsub("[^A-Za-z0-9]", "", cell_name)),
    confounder_vars
  )

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = global_max
  )

  dev_path <- ttm_dev_path()
  invisible(utils::capture.output(
    {
      plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = dev_path
      )
      plan$s2_generate_analysis_files_and_ipcw_pp(
        n_workers = 1L,
        swereg_dev_path = dev_path
      )
      plan$s3_analyze(n_workers = 1L, swereg_dev_path = dev_path)
    },
    type = "output"
  ))

  ett_id <- plan$ett$ett_id[1]
  res <- plan$results_ett[[ett_id]]

  itt_file <- file.path(plan$dir_tteplan, plan$ett$file_analysis_itt[1])
  crude <- NA_real_
  ipw <- NA_real_
  if (file.exists(itt_file)) {
    en <- swereg::qs2_read(itt_file, nthreads = 1L)
    d <- en$data
    tv <- en$design$treatment_var
    rr <- d[,
      .(
        ev = sum(event),
        pw = sum(person_weeks),
        ev_w = sum(event * ipw_trunc),
        pw_w = sum(person_weeks * ipw_trunc)
      ),
      by = tv
    ]
    data.table::setnames(rr, tv, "arm")
    r_int <- rr[arm == TRUE]
    r_cmp <- rr[arm == FALSE]
    crude <- (r_int$ev / r_int$pw) / (r_cmp$ev / r_cmp$pw)
    ipw <- (r_int$ev_w / r_int$pw_w) / (r_cmp$ev_w / r_cmp$pw_w)
  }

  list(
    cell = cell_name,
    irr_pp = res$irr_pp_trunc,
    irr_itt = res$irr_itt,
    crude_rr = crude,
    ipw_rr = ipw
  )
}
