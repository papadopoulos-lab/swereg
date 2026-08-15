# =============================================================================
# Fixture and capture harness for the export parity test
# =============================================================================
# `test-export_tables_accessor_parity.R` uses everything below. The functions
# build a TTEPlan whose stored results sweep the states `$export_tables()`
# branches on, run one export, and read the workbook and the sidecar images
# back as comparable data.
#
# The names all start with `.xp_`, so nothing here can collide with another
# test file.
#
# The snapshot fixture is `fixtures/export_tables_snapshot.qs2`. Call
# `.xp_regenerate_snapshot()` to rewrite it. Do that only when a change to the
# export path is INTENDED, and never to make a red test green.
# =============================================================================

.xp_rates <- function(ei, pi, ri, ec, pc, rc) {
  dt <- data.table::data.table(
    rd_tx = c(TRUE, FALSE),
    n_persons = c(300, 700),
    n_trials = c(3000, 7000),
    events_weighted = c(ei, ec),
    py_weighted = c(pi, pc),
    rate_per_100000py = c(ri, rc)
  )
  data.table::setattr(dt, "swereg_type", "rates")
  data.table::setattr(dt, "treatment_var", "rd_tx")
  dt
}

# `stored = TRUE` runs the producer-side marker, so `irr_estimable` is stored.
# `stored = FALSE` is a result cached before that column existed.
.xp_irr <- function(irr, lo, hi, p, stored = TRUE, warn = FALSE) {
  dt <- data.table::data.table(
    IRR = irr, IRR_lower = lo, IRR_upper = hi, IRR_pvalue = p, warn = warn
  )
  data.table::setattr(dt, "swereg_type", "irr")
  if (stored) swereg:::.s3_mark_irr_estimable(dt) else dt
}

.xp_curve <- function(rd, conf_level = 0.95, status = "ok") {
  cv <- data.table::data.table(
    tstop = c(26, 52),
    surv_comparator = c(0.990, 0.980),
    surv_intervention = c(0.990 + rd / 2, 0.980 + rd),
    rd = c(rd / 2, rd),
    rd_lo = c(rd / 2 - 0.003, rd - 0.004),
    rd_hi = c(rd / 2 + 0.003, rd + 0.004),
    interval_status = c(status, status),
    nnt = c(-1 / (rd / 2), -1 / rd),
    nnt_direction = rep(if (rd < 0) "benefit" else "harm", 2L),
    n_persons_with_event_comparator = c(10, 20),
    n_persons_with_event_intervention = c(5, 11)
  )
  data.table::setattr(cv, "conf_level", conf_level)
  cv
}

.xp_table1 <- function(seed) {
  set.seed(seed)
  d <- data.table::data.table(
    rd_tx = rep(c(TRUE, FALSE), each = 60L),
    rd_age_continuous = as.numeric(rep(40:99, times = 2L)),
    rd_edu = rep(c("low", "mid", "high"), times = 40L)
  )
  swereg:::.swereg_table1(
    data = d,
    vars = c("rd_age_continuous", "rd_edu"),
    strata = "rd_tx",
    include_smd = TRUE,
    show_missing = "always",
    arm_labels = c(comparator = "Untreated", intervention = "Treated")
  )
}

.xp_table1_main <- function(seed) {
  set.seed(seed)
  d <- data.table::data.table(
    rd_tx = rep(c(TRUE, FALSE), each = 60L),
    rd_age_continuous = as.numeric(rep(40:99, times = 2L)),
    rd_edu = rep(c("low", "mid", "high"), times = 40L)
  )
  swereg:::.swereg_table1(
    data = d,
    vars = c("rd_age_continuous", "rd_edu"),
    strata = "rd_tx",
    include_smd = TRUE,
    show_missing = "none",
    arm_labels = c(comparator = "Untreated", intervention = "Treated")
  )
}

.xp_subgroup <- function(levels, irr, lo, hi, p) {
  dt <- data.table::data.table(
    level = levels, IRR = irr, IRR_lower = lo, IRR_upper = hi,
    IRR_pvalue = p, warn = rep(FALSE, length(levels))
  )
  data.table::setattr(dt, "em_pvalue", 0.99)
  data.table::setattr(dt, "ratio_of_irrs", 9.99)
  data.table::setattr(dt, "swereg_type", "irr_by_subgroup")
  dt
}

.xp_emtest <- function(p, ratio, lo, hi, var) {
  list(
    p_value = p, subgroup_var = var, n_levels = 2L,
    interaction_coefs = data.table::data.table(
      term = "x", estimate = log(ratio), se = 0.1, p = p
    ),
    ratio_of_irrs = ratio, ratio_lower = lo, ratio_upper = hi
  )
}

# --- the plan ---------------------------------------------------------------
#
# Eight emulated trials over two enrollments, chosen to sweep the states the
# export path branches on:
#
#   ETT00001 enr 01 fu 52  full result, six subgroup variables
#   ETT00002 enr 01 fu 52  full result, one interaction-only subgroup
#   ETT00003 enr 02 fu 52  full result
#   ETT00004 enr 02 fu 52  rates stored, incidence rate ratio SKIPPED
#   ETT00005 enr 01 fu 156 full result, a SECOND follow-up horizon
#   ETT00006 enr 02 fu 52  no entry in results_ett at all
#   ETT00007 enr 01 fu 52  ratio of exactly 0, which is not estimable
#   ETT00008 enr 01 fu 52  ratio of 150, above the display cap
#
# Enrollment 01 stores every baseline panel. Enrollment 02 stores NO raw panel
# and NO matching block, and one of its attrition criteria has no global row.
# That last one costs enrollment 02 its attrition sheet and its CONSORT
# sidecars, so the export path is exercised on an enrollment it SKIPS.
.xp_plan <- function(fixture = c("new", "legacy"), subgroups = TRUE) {
  fixture <- match.arg(fixture)
  stored <- identical(fixture, "new")

  sg <- if (subgroups) {
    list(
      c("rd_age_band", "rd_bmi", "rd_absent"),
      c("rd_parity"),
      character(0), character(0), character(0),
      character(0), character(0), character(0)
    )
  } else {
    rep(list(character(0)), 8L)
  }

  ett <- data.table::data.table(
    enrollment_id = c("01", "01", "02", "02", "01", "02", "01", "01"),
    ett_id = sprintf("ETT0000%d", 1:8),
    age_group = "50_59",
    age_min = 50L,
    age_max = 59L,
    follow_up = c(52L, 52L, 52L, 52L, 156L, 52L, 52L, 52L),
    outcome_var = c(
      "osd_a", "osd_b", "osd_a", "osd_b", "osd_c", "osd_d", "osd_e", "osd_f"
    ),
    outcome_name = c(
      "Outcome A", "Outcome B", "Outcome A", "Outcome B",
      "Outcome C", "Outcome D", "Outcome E", "Outcome F"
    ),
    outcome_description = c(
      "first outcome", "second outcome", "first outcome", "second outcome",
      "third outcome", "fourth outcome", "fifth outcome", "sixth outcome"
    ),
    outcome_role = c(
      "primary", "secondary", "primary", "secondary",
      "primary", "secondary", "secondary", "secondary"
    ),
    description = sprintf("ETT0000%d", 1:8),
    confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    comparator_to_intervention_ratio = 2L,
    file_raw = "raw_01.qs2",
    file_imp = "imp_01.qs2",
    file_analysis = sprintf("analysis_00%d.qs2", 1:8),
    subgroup_vars = sg
  )
  ett[, treatment_impl := list(rep(list(list(
    variable = "rd_tx", intervention_value = TRUE, comparator_value = FALSE
  )), 8L))]

  plan <- swereg::TTEPlan$new(
    project_prefix = "xp",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  # Fixed, so the Provenance sheet carries no clock reading but the one the
  # capture masks.
  plan$created_at <- as.POSIXct("2026-01-04 05:06:07", tz = "UTC")
  plan$registry_study_created_at <- as.POSIXct("2026-01-02 03:04:05", tz = "UTC")
  plan$skeleton_created_at <- as.POSIXct("2026-01-03 04:05:06", tz = "UTC")
  plan$expected_n_ids <- 123456L
  plan$expected_skeleton_file_count <- 1L
  plan$spec <- list(
    study = list(
      title = "Phase six parity fixture",
      implementation = list(project_prefix = "xp", conf_level = 0.95)
    ),
    enrollments = list(
      list(
        id = "01", name = "Enrollment one",
        treatment = list(
          description = "treated versus untreated",
          arms = list(intervention = "Treated", comparator = "Untreated")
        )
      ),
      list(
        id = "02", name = "Enrollment two",
        treatment = list(
          description = "treated versus untreated",
          arms = list(intervention = "Treated", comparator = "Untreated")
        )
      )
    )
  )

  rates1 <- .xp_rates(10.4, 62816, 16.9, 20.6, 98765, 12.0)
  rates2 <- .xp_rates(4.2, 31000, 13.5, 9.1, 51000, 17.8)

  mk_rd <- function(slot, rd, ett_id) {
    if (!stored) {
      return(list())
    }
    swereg:::.s3_rd_result(slot, .xp_curve(rd), ett_id, "tstop")
  }

  r1 <- c(
    list(
      enrollment_id = "01",
      description = "ETT00001",
      summary = list(n_events = 42L),
      rates_pp_trunc = rates1,
      rates_pp = rates1,
      rates_itt = rates2,
      irr_pp_trunc = .xp_irr(0.54, 0.40, 0.71, 1e-7, stored),
      irr_pp = .xp_irr(0.58, 0.43, 0.78, 3e-6, stored),
      irr_itt = .xp_irr(0.66, 0.50, 0.87, 0.003, stored),
      subgroup_rd_age_band_pp = .xp_subgroup(
        c("all", "younger", "older"),
        c(0.54, 0.61, 0.48), c(0.40, 0.42, 0.31),
        c(0.71, 0.88, 0.74), c(1e-7, 0.008, 0.001)
      ),
      emtest_rd_age_band_pp = .xp_emtest(0.42, 0.79, 0.55, 1.14, "rd_age_band"),
      subgroup_rd_age_band_itt = .xp_subgroup(
        c("all", "younger", "older"),
        c(0.66, 0.70, 0.59), c(0.50, 0.49, 0.39),
        c(0.87, 1.00, 0.89), c(0.003, 0.049, 0.012)
      ),
      emtest_rd_age_band_itt = .xp_emtest(
        0.61, 0.88, 0.60, 1.29, "rd_age_band"
      ),
      # Stratified only: no companion interaction test.
      subgroup_rd_bmi_pp = .xp_subgroup(
        c("all", "low", "high"),
        c(0.54, 0.52, 0.57), c(0.40, 0.33, 0.38),
        c(0.71, 0.81, 0.85), c(1e-7, 0.004, 0.006)
      )
    ),
    mk_rd("rd_pp_trunc", -0.010, "ETT00001"),
    mk_rd("rd_itt", -0.006, "ETT00001")
  )

  r2 <- c(
    list(
      enrollment_id = "01",
      description = "ETT00002",
      summary = list(n_events = 18L),
      rates_pp_trunc = rates2,
      rates_pp = rates2,
      rates_itt = rates2,
      irr_pp_trunc = .xp_irr(1.35, 0.98, 1.86, 0.064, stored, warn = TRUE),
      irr_pp = .xp_irr(1.31, 0.95, 1.81, 0.098, stored),
      irr_itt = .xp_irr(1.22, 0.90, 1.65, 0.201, stored),
      # Interaction only: no stratified table.
      emtest_rd_parity_pp = .xp_emtest(0.07, 1.35, 0.97, 1.88, "rd_parity")
    ),
    mk_rd("rd_pp_trunc", 0.004, "ETT00002")
  )

  r3 <- c(
    list(
      enrollment_id = "02",
      description = "ETT00003",
      summary = list(n_events = 31L),
      rates_pp_trunc = rates1,
      rates_pp = rates1,
      rates_itt = rates1,
      irr_pp_trunc = .xp_irr(0.81, 0.62, 1.06, 0.121, stored),
      irr_pp = .xp_irr(0.83, 0.63, 1.09, 0.180, stored),
      irr_itt = .xp_irr(0.90, 0.70, 1.16, 0.415, stored)
    ),
    mk_rd("rd_pp_trunc", -0.002, "ETT00003"),
    mk_rd("rd_itt", -0.001, "ETT00003")
  )

  r4 <- list(
    enrollment_id = "02",
    description = "ETT00004",
    summary = list(n_events = 5L),
    rates_pp_trunc = rates2,
    rates_pp = rates2,
    irr_pp_trunc = list(skipped = TRUE, reason = "no events"),
    irr_pp = list(skipped = TRUE, reason = "no events"),
    irr_itt = list(skipped = TRUE, reason = "no events")
  )

  r5 <- c(
    list(
      enrollment_id = "01",
      description = "ETT00005",
      summary = list(n_events = 77L),
      rates_pp_trunc = rates1,
      rates_pp = rates1,
      rates_itt = rates1,
      irr_pp_trunc = .xp_irr(0.72, 0.55, 0.94, 0.016, stored),
      irr_pp = .xp_irr(0.74, 0.56, 0.98, 0.035, stored),
      irr_itt = .xp_irr(0.79, 0.61, 1.02, 0.071, stored)
    ),
    mk_rd("rd_pp_trunc", -0.015, "ETT00005")
  )

  # Ratio of exactly zero: finite, and not estimable.
  r7 <- list(
    enrollment_id = "01",
    description = "ETT00007",
    summary = list(n_events = 2L),
    rates_pp_trunc = .xp_rates(0, 21000, 0, 6.0, 42000, 14.3),
    rates_pp = .xp_rates(0, 21000, 0, 6.0, 42000, 14.3),
    rates_itt = .xp_rates(0, 21000, 0, 6.0, 42000, 14.3),
    irr_pp_trunc = .xp_irr(0, 0, 0, NA_real_, stored),
    irr_pp = .xp_irr(0, 0, 0, NA_real_, stored),
    irr_itt = .xp_irr(0, 0, 0, NA_real_, stored)
  )

  # Ratio above the display cap.
  r8 <- list(
    enrollment_id = "01",
    description = "ETT00008",
    summary = list(n_events = 9L),
    rates_pp_trunc = .xp_rates(9.0, 1000, 900, 0.5, 40000, 1.25),
    rates_pp = .xp_rates(9.0, 1000, 900, 0.5, 40000, 1.25),
    rates_itt = .xp_rates(9.0, 1000, 900, 0.5, 40000, 1.25),
    irr_pp_trunc = .xp_irr(150, 21.0, 1071.0, 1e-9, stored),
    irr_pp = .xp_irr(150, 21.0, 1071.0, 1e-9, stored),
    irr_itt = .xp_irr(140, 20.0, 980.0, 1e-9, stored)
  )

  plan$results_ett <- list(
    ETT00001 = r1, ETT00002 = r2, ETT00003 = r3, ETT00004 = r4,
    ETT00005 = r5, ETT00007 = r7, ETT00008 = r8
  )

  t1a <- .xp_table1(1L)
  t1b <- .xp_table1(2L)
  plan$results_enrollment <- list(
    `01` = list(
      table1_raw = t1b,
      table1_unweighted = t1a,
      table1_ipw = t1a,
      table1_ipw_trunc = t1a,
      table1_ipw_trunc_main = .xp_table1_main(1L),
      n_baseline = 1000L,
      n_baseline_intervention = 300L,
      n_baseline_comparator = 700L
    ),
    # No raw panel at all.
    `02` = list(
      table1_raw = NULL,
      table1_unweighted = t1b,
      table1_ipw = t1b,
      table1_ipw_trunc = t1b,
      table1_ipw_trunc_main = .xp_table1_main(2L),
      n_baseline = 640L,
      n_baseline_intervention = 210L,
      n_baseline_comparator = 430L
    )
  )

  plan$enrollment_counts <- list(
    `01` = list(
      attrition = data.table::data.table(
        trial_id = c(1L, 2L, NA, 1L, 2L, NA, 1L, 2L, NA),
        criterion = c(
          "before_exclusions", "before_exclusions", "before_exclusions",
          "age", "age", "age",
          "prior_disease", "prior_disease", "prior_disease"
        ),
        n_persons = c(3000, 3200, 5000, 2500, 2600, 4000, 1800, 1900, 3100),
        n_person_trials = c(
          25000, 25000, 50000, 20000, 20000, 40000, 15000, 15000, 30000
        ),
        n_intervention = c(500, 500, 1000, 450, 450, 900, 400, 400, 800),
        n_comparator = c(2000, 2000, 4000, 1550, 1550, 3100, 1100, 1100, 2200)
      ),
      matching = data.table::data.table(
        trial_id = c(1L, 2L),
        n_intervention_total = c(800, 820),
        n_comparator_total = c(2200, 2300),
        n_intervention_enrolled = c(150, 150),
        n_comparator_enrolled = c(350, 350)
      )
    ),
    # LEGACY attrition: `prior_disease` has no global row, so
    # `.attrition_overall()` returns NULL and this enrollment gets no attrition
    # sheet, no table-of-contents row and no CONSORT sidecar. And no matching.
    `02` = list(
      attrition = data.table::data.table(
        trial_id = c(1L, 2L, NA, 1L, 2L, NA, 1L, 2L),
        criterion = c(
          "before_exclusions", "before_exclusions", "before_exclusions",
          "age", "age", "age",
          "prior_disease", "prior_disease"
        ),
        n_persons = c(1500, 1600, 2600, 1300, 1350, 2100, 900, 950),
        n_person_trials = c(
          12000, 12000, 24000, 10000, 10000, 20000, 7000, 7000
        ),
        n_intervention = c(250, 250, 500, 220, 220, 440, 200, 200),
        n_comparator = c(1000, 1000, 2000, 800, 800, 1600, 550, 550)
      )
    )
  )
  plan
}

# --- the four project shapes -------------------------------------------------
#
# `protocol_ett_id` names the trial the Target trial protocol sheet documents,
# and `subgroups` decides whether the effect-modification sheet exists at all.
# The four shapes mirror the four projects that call `$export_tables()`.
#
# `protocol_explicit` and `protocol_explicit_alt` name DIFFERENT trials, so the
# protocol sheet and its table-of-contents row differ between them.
.XP_SHAPES <- list(
  list(
    name = "protocol_explicit",
    subgroups = TRUE,
    protocol = "ETT00002"
  ),
  list(name = "protocol_default", subgroups = TRUE, protocol = NULL),
  list(
    name = "protocol_explicit_alt",
    subgroups = TRUE,
    protocol = "ETT00001"
  ),
  list(
    name = "protocol_default_no_subgroups",
    subgroups = FALSE,
    protocol = NULL
  )
)

# --- capture -----------------------------------------------------------------

#' Pixel width and height of a PNG, read from its IHDR chunk.
.xp_png_dim <- function(path) {
  raw <- readBin(path, "raw", n = 33L)
  wh <- as.integer(raw[17:24])
  c(
    width = sum(wh[1:4] * c(16777216, 65536, 256, 1)),
    height = sum(wh[5:8] * c(16777216, 65536, 256, 1))
  )
}

#' Page width and height of a PDF, in points, from the first /MediaBox.
#'
#' `cairo_pdf()` and `rsvg::rsvg_pdf()` write PDF 1.5 or later, and both put
#' the page dictionary inside a FlateDecode object stream. `/MediaBox` is
#' therefore NOT visible in the raw bytes, and a reader that searches them only
#' returns `NA` for every file. An `NA` compared against an `NA` passes, so
#' that reader is a check that cannot go red.
#'
#' This one searches the raw bytes first, then inflates each stream and
#' searches again.
.xp_pdf_dim <- function(path) {
  media_box <- function(bytes) {
    at <- grepRaw("/MediaBox", bytes, fixed = TRUE, all = FALSE)
    if (length(at) == 0L) {
      return(NULL)
    }
    chunk <- rawToChar(bytes[at:min(length(bytes), at + 120L)])
    m <- regmatches(
      chunk,
      regexpr("\\[ *[-0-9.]+ +[-0-9.]+ +[-0-9.]+ +[-0-9.]+ *\\]", chunk)
    )
    if (length(m) == 0L) {
      return(NULL)
    }
    n <- as.numeric(strsplit(trimws(gsub("[][]", " ", m)), " +")[[1L]])
    c(width = n[3L] - n[1L], height = n[4L] - n[2L])
  }
  raw <- readBin(path, "raw", n = file.size(path))
  found <- media_box(raw)
  if (!is.null(found)) {
    return(found)
  }
  starts <- grepRaw("stream\r\n|stream\n", raw, all = TRUE)
  for (st in starts) {
    off <- if (identical(rawToChar(raw[st + 6L]), "\r")) st + 8L else st + 7L
    en <- grepRaw("endstream", raw, offset = off, all = FALSE)
    if (length(en) == 0L) {
      next
    }
    body <- raw[off:(en - 1L)]
    inflated <- tryCatch(
      memDecompress(body, type = "gzip"),
      error = function(e) NULL
    )
    if (is.null(inflated)) {
      next
    }
    found <- media_box(inflated)
    if (!is.null(found)) {
      return(found)
    }
  }
  c(width = NA_real_, height = NA_real_)
}


# The `Provenance` rows whose value is a property of the MACHINE, not of the
# export path. `.write_provenance()` writes a clock reading and three version
# strings. Every one of them changes without any change to the code under test,
# so the capture masks the value and keeps the label under comparison.
#
# `swereg version` is the one that bites. It reads `DESCRIPTION`, so a release
# bump turned the snapshot red on all eight cases, once per sheet. `R version`
# and `data.table version` do the same on any upgrade.
.XP_MASKED_PROVENANCE_ROWS <- c(
  "Exported at",
  "R version",
  "swereg version",
  "data.table version"
)

.xp_read_sheets <- function(path) {
  wb <- openxlsx::loadWorkbook(path)
  nms <- names(wb)
  out <- lapply(nms, function(s) {
    d <- openxlsx::read.xlsx(
      path,
      sheet = s,
      colNames = FALSE,
      skipEmptyRows = FALSE,
      skipEmptyCols = FALSE
    )
    if (is.null(d)) {
      return(NULL)
    }
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    rownames(d) <- NULL
    names(d) <- paste0("C", seq_len(ncol(d)))
    # Mask the four machine-dependent values named in
    # `.XP_MASKED_PROVENANCE_ROWS`. Every other cell of the sheet, and every
    # label in column 1, stays under comparison.
    if (identical(s, "Provenance") && ncol(d) >= 2L) {
      hit <- which(!is.na(d[[1L]]) & d[[1L]] %in% .XP_MASKED_PROVENANCE_ROWS)
      if (length(hit) > 0L) d[hit, 2L] <- "<masked>"
    }
    d
  })
  names(out) <- nms
  list(sheet_names = nms, sheets = out)
}

#' Record the DATA each figure renderer receives, without changing what it
#' draws. The wrappers are installed for one export and removed afterwards.
.xp_with_render_capture <- function(expr) {
  cap <- new.env(parent = emptyenv())
  cap$forest <- list()
  cap$overlay <- list()
  cap$love <- list()
  cap$consort <- list()
  ns <- "swereg"
  orig_love <- swereg:::.render_love_plot
  orig_forest <- swereg:::.render_combined_forest_plot
  orig_overlay <- swereg:::.render_itt_vs_pp_overlay
  orig_dot <- swereg:::.build_consort_dot
  on.exit(
    {
      utils::assignInNamespace(".render_love_plot", orig_love, ns)
      utils::assignInNamespace(
        ".render_combined_forest_plot",
        orig_forest,
        ns
      )
      utils::assignInNamespace(".render_itt_vs_pp_overlay", orig_overlay, ns)
      utils::assignInNamespace(".build_consort_dot", orig_dot, ns)
    },
    add = TRUE
  )
  utils::assignInNamespace(
    ".render_love_plot",
    function(df, ...) {
      cap$love[[length(cap$love) + 1L]] <- df
      orig_love(df, ...)
    },
    ns
  )
  utils::assignInNamespace(
    ".render_combined_forest_plot",
    function(df, ...) {
      cap$forest[[length(cap$forest) + 1L]] <- df
      orig_forest(df, ...)
    },
    ns
  )
  utils::assignInNamespace(
    ".render_itt_vs_pp_overlay",
    function(df, ...) {
      cap$overlay[[length(cap$overlay) + 1L]] <- df
      orig_overlay(df, ...)
    },
    ns
  )
  utils::assignInNamespace(
    ".build_consort_dot",
    function(...) {
      out <- orig_dot(...)
      cap$consort[[length(cap$consort) + 1L]] <- out
      out
    },
    ns
  )
  force(expr)
  list(
    forest = cap$forest,
    overlay = cap$overlay,
    love = cap$love,
    consort = cap$consort
  )
}

.xp_capture_one <- function(fixture, shape, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  plan <- .xp_plan(fixture, subgroups = shape$subgroups)
  path <- file.path(dir, "tables.xlsx")
  renderer <- .xp_with_render_capture(
    suppressMessages(suppressWarnings(plan$export_tables(
      path = path,
      protocol_ett_id = shape$protocol
    )))
  )
  sheets <- .xp_read_sheets(path)
  files <- sort(setdiff(list.files(dir), "tables.xlsx"))
  # NO BYTE DIGEST. `ggplot2::ggsave()` picks its rasteriser by probing for
  # `ragg`, and `ragg` is in neither Imports nor Suggests. `R CMD check` hides
  # it, so the same figure rasterises through cairo there and through ragg in an
  # ordinary session. The two files differ in 3.62% of their pixels with an
  # identical `IHDR`. The capture therefore records what an artefact IS, not
  # what its bytes are.
  #
  # AND NO BYTE SIZE IN THE SNAPSHOT. `size` below is LIVE ONLY:
  # `.xp_snapshot()` drops it, and `.xp_strip_image_size()` says why. Do not
  # add it back to the stored side.
  imgs <- lapply(files, function(f) {
    p <- file.path(dir, f)
    dim <- if (grepl("\\.png$", f)) .xp_png_dim(p) else .xp_pdf_dim(p)
    list(
      name = f,
      dim = dim,
      size = file.size(p)
    )
  })
  names(imgs) <- files
  list(
    sheet_names = sheets$sheet_names,
    sheets = sheets$sheets,
    image_names = files,
    images = imgs,
    renderer = renderer
  )
}

.xp_capture_all <- function(root) {
  out <- list()
  for (fixture in c("legacy", "new")) {
    for (shape in .XP_SHAPES) {
      key <- paste(fixture, shape$name, sep = "/")
      out[[key]] <- .xp_capture_one(
        fixture,
        shape,
        file.path(root, gsub("/", "_", key))
      )
    }
  }
  out
}


# The snapshot fixture, and how to rewrite it.

.xp_snapshot_path <- function() {
  testthat::test_path("fixtures", "export_tables_snapshot.qs2")
}

# Drop `size` from every captured image.
#
# THE SNAPSHOT MUST HOLD NO FIELD THAT TWO RUNS OF ONE TREE CAN DISAGREE ON.
# `file.size()` on a PDF is not reproducible here. Cairo writes a creation
# timestamp into the file, so the compressed length moves by one byte between
# runs. Two runs of unchanged code were compared on this fixture. Nine stored
# image entries moved, on `tables_consort_01.pdf` and on
# `tables_love_plot.pdf`, and the same field moved in both directions.
#
# A field that moves on its own reports a change that nobody made. It cost one
# review round: a diff of the enrollment-01 sidecar looked like a defect in a
# change that never touched enrollment 01.
#
# The LIVE capture keeps `size`, and
# `test-export_tables_accessor_parity.R` reads it there to prove that every
# image carries bytes. Only the STORED side drops it.
.xp_strip_image_size <- function(cases) {
  lapply(cases, function(case) {
    case$images <- lapply(case$images, function(img) {
      img[setdiff(names(img), "size")]
    })
    case
  })
}

# Run one export per case and package the result as the snapshot.
#
# The snapshot records no rendering stack. It recorded one while a PNG digest
# was under comparison, to gate that comparison on the stack it was measured
# on. The gate never worked: it listed ggplot2, patchwork, rsvg and
# `capabilities("cairo")`, and it listed neither `ragg` nor any font, so it
# could not see the two things that decide the pixels.
.xp_snapshot <- function(root) {
  list(cases = .xp_strip_image_size(.xp_capture_all(root)))
}

# Rewrite `fixtures/export_tables_snapshot.qs2` from the CURRENT code.
#
# Run this only when a change to the export path is intended. The test then
# reports what moved, and a reviewer reads that report before the snapshot is
# rewritten. Rewriting it to clear a red test destroys the record the test
# exists to keep.
.xp_regenerate_snapshot <- function(path = .xp_snapshot_path()) {
  root <- tempfile("xp-snapshot-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  qs2::qs_save(.xp_snapshot(root), path)
  path
}
