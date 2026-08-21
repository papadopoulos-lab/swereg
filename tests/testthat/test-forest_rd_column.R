# The forest plot carries the signed cause-specific risk difference.
#
# Everything phases 1-5 computed is invisible until it reaches a figure. This
# file pins the last link: the renderer draws a SIGNED risk-difference column
# with its interval and the number needed to treat, and the export path
# computes those numbers and hands them over.
#
# Six properties, each of which fails silently if it breaks:
#
#   1. The displayed value is signed, per 10,000 people. A benefit is negative
#      and stays negative. `abs()` anywhere in the formatting would turn a
#      protective effect into a harmful-looking one, and the figure would still
#      render.
#   2. A row with no risk difference renders EMPTY. Not "NA", not 0, not a dash
#      that reads as a value.
#   3. The composed column ORDER is fixed. A reader compares the two arms, then
#      the difference between them, then the ratio. Columns that exist in the
#      wrong order still draw.
#   4. The three time-referenced headers state a horizon DERIVED from the rows.
#      A literal keeps printing 156 weeks on a 52-week figure.
#   5. The EXPORT PATH computes the numbers and passes them to the renderer. A
#      column only a unit test can populate is not wired.
#   6. The per-arm distinct-person event counts reach the `PP results` and
#      `ITT results` sheets. The figure stopped drawing them on 2026-08-06, so
#      the workbook is now the only place they appear, and they are NOT the
#      weighted `Events (int)` / `Events (cmp)` columns beside them.
#
# Plus the declared ylim repair: the survival figure is drawn on the
# cumulative-failure scale, so a survival-scale window applied as given would
# clip the whole curve out of view.

skip_if_not_installed("ggplot2")
skip_if_not_installed("data.table")
skip_if_not_installed("patchwork")

# --- fixtures --------------------------------------------------------------

# Three ETTs under one exposure, built through the REAL `.build_forest_df()`
# so the df the renderer sees is the one production builds.
rd_forest_df <- function() {
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = c("ETT00001", "ETT00002", "ETT00003"),
    outcome_var = c("osd_a", "osd_b", "osd_c"),
    outcome_name = c("Outcome A", "Outcome B", "Outcome C"),
    outcome_role = c("primary", "secondary", "secondary"),
    follow_up = 52L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = c("a1.qs2", "a2.qs2", "a3.qs2"),
    description = c("ETT00001", "ETT00002", "ETT00003")
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  one <- function(eid) {
    rt <- data.table::data.table(
      rd_tx = c(TRUE, FALSE),
      events_weighted = c(10.4, 20.6),
      py_weighted = c(1000, 2000),
      rate_per_100000py = c(1040, 1030)
    )
    data.table::setattr(rt, "treatment_var", "rd_tx")
    list(
      enrollment_id = "01",
      description = eid,
      irr_pp_trunc = list(
        IRR = 1.5,
        IRR_lower = 0.9,
        IRR_upper = 2.5,
        IRR_pvalue = 0.1,
        skipped = FALSE
      ),
      rates_pp_trunc = rt
    )
  }
  plan$results_ett <- list(
    ETT00001 = one("ETT00001"),
    ETT00002 = one("ETT00002"),
    ETT00003 = one("ETT00003")
  )
  swereg:::.build_forest_df(
    plan,
    rates_slot = "rates_pp_trunc",
    irr_slot = "irr_pp_trunc",
    keep_ett_ids = c("ETT00001", "ETT00002", "ETT00003"),
    group_labels = rep("Exposure A", 3)
  )
}

# ETT00001 is a benefit (negative), ETT00002 the mirror-image harm (positive),
# ETT00003 has no risk difference at all.
#
# The two risk differences are synthetic and round: -5.00 and +5.00 per 10,000.
# Round matters twice. The pair is an exact mirror, which is what a stray
# `abs()` cannot survive. And -1/rd is 2,000, a four-digit number needed to
# treat, so the drawn figure pins the thousands separator.
#
# The lookup carries the DECISION columns, because `.forest_rd_row()` copies
# them off the curve and the renderer reads them. `rd_lookup_legacy()` is the
# same rows without those two columns, which is the shape a result cached before
# the decision columns existed still has on disk.
rd_lookup_fixture <- function() {
  data.table::data.table(
    ett_id = c("ETT00001", "ETT00002"),
    band = 52L,
    rd = c(-5.0e-4, 5.0e-4),
    rd_lo = c(-8.0e-4, 2.0e-4),
    rd_hi = c(-2.0e-4, 8.0e-4),
    nnt = c(2000, -2000),
    nnt_direction = c("benefit", "harm"),
    n_persons_with_event_intervention = c(12, 41),
    n_persons_with_event_comparator = c(30, 17)
  )
}

rd_lookup_legacy <- function() {
  dt <- rd_lookup_fixture()
  dt[, c("nnt", "nnt_direction") := NULL]
  dt[]
}

rd_render <- function(rd_lookup = rd_lookup_fixture(), ...) {
  swereg:::.render_combined_forest_plot(
    rd_forest_df(),
    arm_labels = c(comparator = "Comparator", intervention = "Intervention"),
    rd_lookup = rd_lookup,
    ...
  )
}

# Every text label the composed figure actually draws, across every panel.
rd_drawn_labels <- function(rendered) {
  p <- rendered$plot
  out <- character(0)
  for (i in seq_len(length(p))) {
    built <- ggplot2::ggplot_build(p[[i]])
    for (d in built$data) {
      if ("label" %in% names(d)) {
        out <- c(out, as.character(d$label))
      }
    }
  }
  out
}

# One header per composed panel, IN THE ORDER patchwork lays them out. The
# header row is the only text drawn at `y_num == 0`, and `scale_y_reverse()`
# maps that row to `y == 0`. The forest panel draws no text and yields NA.
#
# This is what makes the column ORDER assertable. Collecting the labels as a
# set, the way `rd_drawn_labels()` does, passes whatever order the columns are
# composed in.
rd_column_headers <- function(rendered) {
  p <- rendered$plot
  out <- character(0)
  for (i in seq_len(length(p))) {
    built <- ggplot2::ggplot_build(p[[i]])
    hdr <- NA_character_
    for (d in built$data) {
      if (all(c("label", "y") %in% names(d))) {
        h <- as.character(d$label[d$y == 0])
        if (length(h) == 1L) {
          hdr <- h
        }
      }
    }
    out <- c(out, hdr)
  }
  out
}

# The 9-row canonical trial panel, one person per trial. Weighted survivals are
# S_int(4) = 2/3, S_int(8) = 1/3, S_cmp(4) = 1, S_cmp(8) = 1/2, so the
# cumulative failures are 1/3, 2/3, 0 and 1/2.
rd_trial_panel <- function(weight_name) {
  dt <- data.table::data.table(
    enrollment_person_trial_id = 1:9,
    id = 1:9,
    exposed = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    tstop = c(4L, 4L, 4L, 4L, 4L, 8L, 8L, 8L, 8L),
    event = c(0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L),
    w = c(1, 1, 1, 2, 2, 1, 1, 2, 2),
    age = 50,
    death = 0L
  )
  data.table::setnames(dt, "w", weight_name)
  design <- swereg::TTEDesign$new(
    id_var = "enrollment_person_trial_id",
    treatment_var = "exposed",
    outcome_vars = "death",
    confounder_vars = "age",
    follow_up_time = 52L
  )
  swereg::TTEEnrollment$new(dt, design)
}

# A one-ETT plan whose analysis panel is on disk, so the export path can load
# it exactly as it does in production.
rd_export_plan <- function(dir, weight_name = "analysis_weight_pp_trunc") {
  trial <- rd_trial_panel(weight_name)
  swereg::qs2_write_atomic(
    list(data = trial$data, design = trial$design),
    file.path(dir, "analysis_001.qs2")
  )
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = "ETT00001",
    outcome_var = "death",
    outcome_name = "Death",
    outcome_role = "primary",
    follow_up = 52L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "age",
    person_id_var = "id",
    treatment_var = "exposed",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = "analysis_001.qs2",
    file_analysis_itt = "analysis_itt_001.qs2",
    description = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  plan$output_dir <- dir
  rt <- data.table::data.table(
    exposed = c(TRUE, FALSE),
    events_weighted = c(2, 2),
    py_weighted = c(1, 2),
    rate_per_100000py = c(200000, 100000)
  )
  data.table::setattr(rt, "treatment_var", "exposed")
  plan$results_ett <- list(
    ETT00001 = list(
      enrollment_id = "01",
      description = "ETT00001",
      irr_pp_trunc = list(
        IRR = 1.5,
        IRR_lower = 0.9,
        IRR_upper = 2.5,
        IRR_pvalue = 0.1,
        skipped = FALSE
      ),
      rates_pp_trunc = rt
    )
  )
  plan
}

# --- assertion 1: signed, per 10,000 ---------------------------------------

test_that("risk difference renders signed, per 10,000, and the mirror row flips sign", {
  r <- rd_render()
  txt <- r$text
  benefit <- txt$txt_rd[txt$ett_id == "ETT00001" & !is.na(txt$ett_id)]
  harm <- txt$txt_rd[txt$ett_id == "ETT00002" & !is.na(txt$ett_id)]

  # rd = -5.0e-04 is -5.00 per 10,000. The minus sign is the result.
  expect_identical(benefit, "-5.00 (-8.00 to -2.00)")
  # The exact mirror image, and it must NOT render as the same string.
  expect_identical(harm, "+5.00 (+2.00 to +8.00)")
  expect_false(identical(benefit, harm))

  # And the two strings reach the drawn figure, not just the layout table.
  drawn <- rd_drawn_labels(r)
  expect_true("-5.00 (-8.00 to -2.00)" %in% drawn)
  expect_true("+5.00 (+2.00 to +8.00)" %in% drawn)
})

test_that("the risk-difference cell formatter keeps the sign of the point estimate", {
  expect_identical(
    swereg:::.ff_rd_ci(-5.0e-4, -8.0e-4, -2.0e-4),
    "-5.00 (-8.00 to -2.00)"
  )
  expect_identical(
    swereg:::.ff_rd_ci(5.0e-4, 2.0e-4, 8.0e-4),
    "+5.00 (+2.00 to +8.00)"
  )
})

test_that("a risk difference with no estimable interval prints no point estimate either", {
  # `.tte_rd_curve()` returns NA bounds when an arm carries no event through the
  # horizon. The POINT estimate is unusable in that case too, which is why none
  # is printed.
  #
  # The weighted product-limit estimate for an event-free arm is
  # `cumprod(1 - 0/D)`, exactly 1. So RD = S_comparator - 1, which is the
  # comparator's own cumulative incidence negated. It is a one-arm quantity
  # wearing a two-arm label, and a reader who sees a number under a
  # risk-difference header will quote it as an effect.
  #
  # The failure this pins is therefore the OPPOSITE of the obvious one: not a
  # missing number, but a number that should not be there.
  expect_identical(
    swereg:::.ff_rd_ci(-1.20e-4, NA_real_, NA_real_),
    "not estimable"
  )
  expect_identical(
    swereg:::.ff_rd_ci(1.20e-4, NA_real_, NA_real_),
    "not estimable"
  )
  # One bound alone is enough to make the interval unusable.
  expect_identical(
    swereg:::.ff_rd_ci(-1.20e-4, -2.0e-4, NA_real_),
    "not estimable"
  )
  # No digit of the point estimate may survive anywhere in the cell.
  expect_false(grepl(
    "1.20",
    swereg:::.ff_rd_ci(-1.20e-4, NA_real_, NA_real_),
    fixed = TRUE
  ))
  # A non-finite POINT estimate renders empty, not "not estimable": there is no
  # horizon-specific finding to report at all, which is a different state.
  expect_identical(swereg:::.ff_rd_ci(NA_real_, NA_real_, NA_real_), "")
})

# --- assertion 2: a row with no risk difference renders empty ---------------

test_that("a row with no risk difference renders an empty cell", {
  txt <- rd_render()$text
  none <- txt$txt_rd[txt$ett_id == "ETT00003" & !is.na(txt$ett_id)]
  expect_identical(none, "")
  # Spell out what it must not be. Each of these renders as a value a reader
  # would take at face value.
  expect_false(none %in% c("NA", "NA (NA to NA)", "0", "0.00", "-"))
})

test_that("a NULL risk-difference lookup renders every cell blank and does not error", {
  r <- expect_no_error(rd_render(rd_lookup = NULL))
  txt <- r$text
  expect_identical(unique(txt$txt_rd), "")
  expect_identical(unique(txt$txt_nnt), "")
  # Nothing computed the quantity, so no header claims it either.
  drawn <- rd_drawn_labels(r)
  expect_false(any(grepl("Risk difference", drawn, fixed = TRUE)))
  expect_false(any(grepl("Number needed to treat", drawn, fixed = TRUE)))
})

# --- assertion 3: the number needed to treat, labelled by sign -------------

test_that("the number needed to treat sits beside every risk difference", {
  r <- rd_render()
  txt <- r$text
  has_rd <- nzchar(txt$txt_rd)
  expect_gt(sum(has_rd), 0L)
  # Every row that shows a risk difference also shows its number needed to
  # treat, and the sign of the risk difference chooses the label.
  expect_true(all(nzchar(txt$txt_nnt[has_rd])))
  # Four digits, so the thousands separator is pinned on the drawn figure and
  # not only in the unit test of the cell builder.
  #
  # The INTERVAL is pinned here too, and that is the point of asserting the
  # whole string rather than a prefix. The figure reads its cell through
  # `.tte_nntb_cell(nntb, nntb_lo, nntb_hi)`, whose bound arguments are
  # optional. Drop them at the call site and this renders a bare point estimate,
  # which still satisfies every other assertion in this block.
  expect_identical(
    txt$txt_nnt[txt$ett_id == "ETT00001" & !is.na(txt$ett_id)],
    "NNTB 2,000 (1,250 to 5,000)"
  )
  expect_identical(
    txt$txt_nnt[txt$ett_id == "ETT00002" & !is.na(txt$ett_id)],
    "NNTH 2,000 (1,250 to 5,000)"
  )
  # A row with no risk difference shows no number needed to treat either.
  expect_true(all(!nzchar(txt$txt_nnt[!has_rd])))

  drawn <- rd_drawn_labels(r)
  expect_true("NNTB 2,000 (1,250 to 5,000)" %in% drawn)
  expect_true("NNTH 2,000 (1,250 to 5,000)" %in% drawn)
})

test_that("the figure hands the cell builder the stored direction", {
  # The reachability witness. Every other assertion in this block passes on a
  # cell builder that decides the direction itself, because the fixture's signs
  # and directions agree. This one reads the argument the production caller
  # actually supplied.
  seen <- list()
  real_cell <- swereg:::.tte_nntb_cell
  testthat::local_mocked_bindings(
    .tte_nntb_cell = function(
      nntb,
      nntb_lo = NULL,
      nntb_hi = NULL,
      nnt_direction
    ) {
      seen[[length(seen) + 1L]] <<- nnt_direction
      real_cell(nntb, nntb_lo, nntb_hi, nnt_direction)
    },
    .package = "swereg"
  )

  swereg:::.forest_rd_map(
    c("ETT00001", "ETT00002", "ETT00003"),
    rd_lookup_fixture()
  )

  expect_length(seen, 1L)
  # ETT00001 is the benefit row and ETT00002 the mirror-image harm row.
  expect_identical(seen[[1L]], c("benefit", "harm"))
})

# --- the cached row carries the decision, and the map does not rebuild it ---
#
# `.tte_rd_curve()` decides the direction. `.forest_rd_row()` copies it onto the
# cached row. `.forest_rd_map()` reads it. Nothing on that path decides again.
#
# The assertion that can see a rebuild is the one that feeds a row whose stored
# direction DISAGREES with the sign of its own risk difference. Production never
# builds such a row, which is exactly why the rebuild was invisible before.

test_that("the map reads the stored direction, never one rebuilt from rd", {
  lk <- rd_lookup_fixture()
  # Swapped against the sign of `rd`, on purpose. Row 1 has rd < 0 and now
  # stores "harm"; row 2 has rd > 0 and now stores "benefit".
  lk[, nnt_direction := c("harm", "benefit")]

  txt <- swereg:::.forest_rd_map(c("ETT00001", "ETT00002"), lk)$txt_nnt

  expect_match(txt[1], "^NNTH ")
  expect_match(txt[2], "^NNTB ")
  # Spelled out, so a rebuild cannot pass by matching the other prefix.
  expect_false(grepl("NNTB", txt[1], fixed = TRUE))
  expect_false(grepl("NNTH", txt[2], fixed = TRUE))
})

test_that("a legacy lookup without the decision columns renders no direction", {
  # A result cached before the decision columns existed. It MUST still render,
  # and it MUST NOT get a direction derived from the sign of `rd`.
  lk <- rd_lookup_legacy()
  expect_false(any(c("nnt", "nnt_direction") %in% names(lk)))

  r <- expect_no_error(swereg:::.forest_rd_map(c("ETT00001", "ETT00002"), lk))

  # The risk difference still renders. Only the decision is absent.
  expect_identical(r$txt_rd[1], "-5.00 (-8.00 to -2.00)")
  expect_identical(r$txt_rd[2], "+5.00 (+2.00 to +8.00)")

  # No label and no guess. The same fixture WITH the columns renders
  # "NNTB 2,000 (1,250 to 5,000)", so the empty cell is the fallback and not an
  # accident of the numbers.
  expect_identical(r$txt_nnt, c("", ""))
  expect_false(any(grepl("NNT", r$txt_nnt, fixed = TRUE)))
})

test_that("a lookup missing a required column is still an error", {
  # The exemption covers the two decision columns and nothing else.
  lk <- rd_lookup_fixture()
  lk[, rd_lo := NULL]
  expect_error(
    swereg:::.forest_rd_map("ETT00001", lk),
    "rd_lookup is missing column\\(s\\): rd_lo"
  )
})

test_that("the column contract names the decision columns and exempts only those", {
  expect_true(all(
    c("nnt", "nnt_direction") %in% swereg:::.FOREST_RD_COLS
  ))
  expect_identical(
    swereg:::.FOREST_RD_DECISION_COLS,
    c("nnt", "nnt_direction")
  )
})

test_that("the cached row copies the decision off the curve", {
  # Driven through the real curve, not a hand-built one. RD at the last band is
  # 1/2 - 1/3, which is positive, so the curve decides harm and -1/rd is -6.
  trial <- rd_trial_panel("analysis_weight_pp_trunc")
  curve <- trial$risk_difference(
    weight_col = "analysis_weight_pp_trunc",
    n_boot = 20L,
    seed = 1L
  )
  row <- swereg:::.forest_rd_row("ETT00001", curve, "tstop")

  last <- nrow(curve)
  expect_identical(row$nnt, curve$nnt[last])
  expect_identical(row$nnt_direction, curve$nnt_direction[last])
  expect_identical(row$nnt_direction, "harm")
  expect_equal(row$nnt, -6, tolerance = 1e-12)
})

test_that("a curve without the decision columns gives a row with NA, not a guess", {
  # A curve written before the columns existed. `.forest_rd_row()` MUST NOT
  # derive a direction from `rd` to fill the gap.
  trial <- rd_trial_panel("analysis_weight_pp_trunc")
  curve <- trial$risk_difference(
    weight_col = "analysis_weight_pp_trunc",
    n_boot = 20L,
    seed = 1L
  )
  curve[, c("nnt", "nnt_direction") := NULL]

  row <- swereg:::.forest_rd_row("ETT00001", curve, "tstop")

  expect_true(is.na(row$nnt))
  expect_true(is.na(row$nnt_direction))
  expect_identical(row$nnt_direction, NA_character_)
  # The rest of the row is untouched.
  expect_equal(row$rd, 1 / 2 - 1 / 3)
})

test_that("the figure no longer draws the per-arm person counts", {
  # Retired from the figure on 2026-08-06. The counts now live on the
  # `PP results` / `ITT results` sheets; see the workbook test below.
  drawn <- rd_drawn_labels(rd_render())
  expect_false(any(grepl("People with event", drawn, fixed = TRUE)))
  expect_false("12 vs 30" %in% drawn)
  expect_false("41 vs 17" %in% drawn)
})

# --- assertion 6: the header states the level the interval was computed at ---

test_that("a non-default confidence level renders a matching header", {
  # 0.90 must head the column "90% CI". A hard-coded "95% CI" here would put a
  # 90% interval under a 95% label: the number is right and only the label
  # lies, which is exactly the defect that survives into a manuscript.
  drawn <- rd_drawn_labels(rd_render(rd_conf_level = 0.90))
  expect_true("Risk difference per 10,000\nat 52 wks (90% CI)" %in% drawn)
  expect_false("Risk difference per 10,000\nat 52 wks (95% CI)" %in% drawn)

  # The IRR column next door is a separate, genuinely fixed 95% interval, so
  # it must NOT follow the risk difference's level.
  expect_true("IRR over 52 wks\n(95% CI)" %in% drawn)
})

test_that("the risk-difference header follows other non-default levels too", {
  d99 <- rd_drawn_labels(rd_render(rd_conf_level = 0.99))
  expect_true("Risk difference per 10,000\nat 52 wks (99% CI)" %in% d99)
  expect_false("Risk difference per 10,000\nat 52 wks (95% CI)" %in% d99)

  # A non-integer level keeps the digits it needs and no more.
  d975 <- rd_drawn_labels(rd_render(rd_conf_level = 0.975))
  expect_true("Risk difference per 10,000\nat 52 wks (97.5% CI)" %in% d975)
})

test_that("0.95 is the default, so an existing caller's level is unchanged", {
  drawn <- rd_drawn_labels(rd_render())
  expect_true("Risk difference per 10,000\nat 52 wks (95% CI)" %in% drawn)
})

test_that("the confidence-level percentage formatter drops nothing and invents nothing", {
  expect_identical(swereg:::.ff_conf_pct(0.95), "95")
  expect_identical(swereg:::.ff_conf_pct(0.9), "90")
  expect_identical(swereg:::.ff_conf_pct(0.99), "99")
  expect_identical(swereg:::.ff_conf_pct(0.975), "97.5")
  expect_identical(swereg:::.ff_conf_pct(0.9973), "99.73")
  expect_error(swereg:::.ff_conf_pct(1), "strictly between 0 and 1")
  expect_error(swereg:::.ff_conf_pct(0), "strictly between 0 and 1")
})

test_that("a header that would contradict the computed interval is refused", {
  # The lookup carries the level its bounds were computed at. Asking the
  # renderer to print a different one is not a formatting preference, it is a
  # false statement about the numbers.
  lk <- rd_lookup_fixture()
  lk$conf_level <- 0.90
  expect_error(
    rd_render(rd_lookup = lk, rd_conf_level = 0.95),
    "disagrees with the level the intervals were computed at"
  )
  # Agreeing is fine, and prints the level they agree on.
  drawn <- rd_drawn_labels(rd_render(rd_lookup = lk, rd_conf_level = 0.90))
  expect_true("Risk difference per 10,000\nat 52 wks (90% CI)" %in% drawn)
})

test_that("the weighted-events header still names its own quantity", {
  drawn <- rd_drawn_labels(rd_render())
  # `events_weighted` is a weighted sum over event ROWS, not a head count of
  # people. `PY` names the exposure measure, so this header takes no time
  # reference: five repetitions of the horizon would be noise.
  expect_true(any(grepl("weighted events / PY", drawn, fixed = TRUE)))
  expect_false(any(grepl("weighted events / PY at", drawn, fixed = TRUE)))
})

# --- assertion 3: the composed column order --------------------------------

test_that("the columns are composed in the prescribed reading order", {
  # Order, left to right: description, each arm's contribution, the absolute
  # difference between them, how many people that difference is, the ratio,
  # then the panel that draws the ratio. Two adjacent columns swapped still
  # draws, and every label is still present, so only the ORDER catches it.
  expect_identical(
    rd_column_headers(rd_render()),
    c(
      "",
      "Intervention\nweighted events / PY",
      "Comparator\nweighted events / PY",
      "Risk difference per 10,000\nat 52 wks (95% CI)",
      "Number needed to treat\nat 52 wks",
      "IRR over 52 wks\n(95% CI)",
      NA_character_
    )
  )
})

test_that("without a risk difference the remaining columns keep their order", {
  expect_identical(
    rd_column_headers(rd_render(rd_lookup = NULL)),
    c(
      "",
      "Intervention\nweighted events / PY",
      "Comparator\nweighted events / PY",
      "IRR over 52 wks\n(95% CI)",
      NA_character_
    )
  )
})

# --- assertion 4: the horizon is derived, never a literal ------------------

test_that("the three time-referenced headers state the rows' own horizon", {
  # The fixture follows people for 52 weeks. A literal 156 would print a
  # horizon no row has, and the figure would still draw.
  hdr <- rd_column_headers(rd_render())
  expect_true("IRR over 52 wks\n(95% CI)" %in% hdr)
  expect_true("Risk difference per 10,000\nat 52 wks (95% CI)" %in% hdr)
  expect_true("Number needed to treat\nat 52 wks" %in% hdr)
  expect_false(any(grepl("156", hdr[!is.na(hdr)], fixed = TRUE)))
})

test_that("a 156-week panel heads its columns 156, not 52", {
  # The same fixture at the horizon the production figure uses. One of the two
  # tests must fail for any literal, whichever number the literal is.
  df <- rd_forest_df()
  df$follow_up <- 156L
  r <- swereg:::.render_combined_forest_plot(
    df,
    arm_labels = c(comparator = "Comparator", intervention = "Intervention"),
    rd_lookup = rd_lookup_fixture()
  )
  hdr <- rd_column_headers(r)
  expect_true("IRR over 156 wks\n(95% CI)" %in% hdr)
  expect_true("Risk difference per 10,000\nat 156 wks (95% CI)" %in% hdr)
  expect_true("Number needed to treat\nat 156 wks" %in% hdr)
  expect_false(any(grepl("52 wks", hdr[!is.na(hdr)], fixed = TRUE)))
})

test_that("rows that mix horizons still render, with no horizon in the headers", {
  # One header covers the whole column, so it must be true of every row.
  # Printing 52 over a column half of which ran to 156 is a false statement
  # about the numbers, and that is the defect being prevented.
  #
  # The FIGURE is not the defect. An earlier version raised here, and two of
  # the four production callers pass no horizon filter, so their forest plots
  # stopped rendering altogether. Dropping four characters from three headers
  # satisfies the same invariant without destroying the exhibit.
  df <- rd_forest_df()
  df$follow_up <- c(52L, 156L, 52L)
  r <- swereg:::.render_combined_forest_plot(
    df,
    arm_labels = c(comparator = "Comparator", intervention = "Intervention"),
    rd_lookup = rd_lookup_fixture()
  )
  hdr <- rd_column_headers(r)
  hdr <- hdr[!is.na(hdr)]

  # It rendered.
  expect_true(length(hdr) > 0L)
  # And it states NO horizon: not 52, not 156, and no bare "wks" either.
  expect_false(any(grepl("52 wks", hdr, fixed = TRUE)))
  expect_false(any(grepl("156 wks", hdr, fixed = TRUE)))
  expect_false(any(grepl("wks", hdr, fixed = TRUE)))
  # The columns themselves are still there and still labelled.
  expect_true(any(grepl("IRR", hdr, fixed = TRUE)))
  expect_true(any(grepl("Risk difference", hdr, fixed = TRUE)))
  expect_true(any(grepl("Number needed to treat", hdr, fixed = TRUE)))
  # The risk-difference header keeps its confidence level, which does not
  # depend on the horizon and must not be lost with it.
  expect_true(any(grepl("95% CI", hdr, fixed = TRUE)))
})

test_that("the horizon resolver returns one horizon, or NULL when none governs", {
  # NULL is the signal that no time reference may be printed. Every path that
  # cannot name a single horizon MUST take it, because the caller's only job is
  # to decide whether the headers carry a horizon at all.
  expect_equal(
    swereg:::.forest_horizon(data.table::data.table(follow_up = c(156L, 156L))),
    156
  )
  # Rows disagree.
  expect_null(
    swereg:::.forest_horizon(data.table::data.table(follow_up = c(52L, 156L)))
  )
  # No column at all.
  expect_null(swereg:::.forest_horizon(data.table::data.table(x = 1L)))
  # Column present but empty of usable values.
  expect_null(
    swereg:::.forest_horizon(data.table::data.table(follow_up = NA_integer_))
  )
  # A single horizon still resolves when NAs sit beside it, because the NAs
  # carry no competing claim.
  expect_equal(
    swereg:::.forest_horizon(
      data.table::data.table(follow_up = c(156L, NA_integer_))
    ),
    156
  )
})

# --- assertion 4: the export path is wired ---------------------------------

# s3 computes the risk difference and the export path only formats it. Run the
# REAL s3 worker on the fixture panel, merge its result exactly as
# `$s3_analyze()` does, then export.
#
# This block asserted the opposite contract until 26.8.20. It asserted that
# `.export_figure()` loaded the analysis panel off disk and computed the risk
# difference there. That was the defect: the computation sat behind a figure
# option, so a script that did not set the option produced every figure without
# it. The numbers below are the same numbers, from the same fixture panel. Only
# the stage that produces them moved.
rd_export_run_s3 <- function(plan, dir, conf_level = 0.95) {
  # `rd_export_plan()` WRITES the analysis file, so the promise must be forced
  # before the worker reads it. Passed lazily, the read runs first and fails.
  force(plan)
  res <- swereg:::.s3_ett_worker(
    analysis_path = file.path(dir, "analysis_001.qs2"),
    method = "risk_difference",
    weight_col = "analysis_weight_pp_trunc",
    ett_id = "ETT00001",
    n_threads = 1L,
    subgroup_var = NULL,
    conf_level = conf_level
  )
  for (k in names(res)) {
    plan$results_ett[["ETT00001"]][[k]] <- res[[k]]
  }
  plan
}

test_that("export path reads the cached risk difference and passes it to the renderer", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("openxlsx")

  dir <- tempfile("rd_export")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  plan <- rd_export_plan(dir)
  plan <- rd_export_run_s3(plan, dir)

  real_renderer <- swereg:::.render_combined_forest_plot
  got_rd <- NULL
  got_out <- NULL
  testthat::local_mocked_bindings(
    .render_combined_forest_plot = function(..., rd_lookup = NULL) {
      got_rd <<- rd_lookup
      got_out <<- real_renderer(..., rd_lookup = rd_lookup)
      got_out
    },
    # The export path MUST NOT open an analysis file. This mock makes the
    # removed disk read impossible to reintroduce quietly: any read during the
    # export raises. It is the strongest form of "s4 only formats".
    qs2_read = function(...) {
      stop("the forest export path must not read an analysis file")
    },
    .package = "swereg"
  )

  spec <- list(
    type = "forest",
    exposures = list("Exposure A" = "ETT00001"),
    estimands = "pp",
    label = "forest",
    risk_difference = TRUE
  )
  out <- swereg:::.plan_export_figure(plan, spec, file.path(dir, "fig"))

  # RUNTIME proof, not a static parse. The plan's own figure producer ran, it
  # opened no file, it read the cached row, and the renderer received it.
  expect_true(file.exists(out))
  expect_false(is.null(got_rd))
  expect_identical(got_rd$ett_id, "ETT00001")
  expect_true(all(
    c(
      "rd",
      "rd_lo",
      "rd_hi",
      "n_persons_with_event_intervention",
      "n_persons_with_event_comparator"
    ) %in%
      names(got_rd)
  ))
  # S_cmp(8) = 1/2 and S_int(8) = 1/3, so RD = 1/2 - 1/3 at the last band.
  expect_equal(got_rd$rd, 1 / 2 - 1 / 3)
  # Distinct PEOPLE with the event by band 8: two exposed, one unexposed.
  expect_equal(got_rd$n_persons_with_event_intervention, 2)
  expect_equal(got_rd$n_persons_with_event_comparator, 1)

  # And it survived the whole way into the rendered figure.
  row <- got_out$text$ett_id == "ETT00001" & !is.na(got_out$text$ett_id)
  expect_true(nzchar(got_out$text$txt_rd[row]))
  # 500 bootstrap replicates on 9 person-trials give an interval that spans the
  # null, so the number needed to treat is undefined and its cell is EMPTY.
  # A finite-looking number here would come from a loosened guard.
  expect_lt(got_rd$rd_lo, 0)
  expect_gt(got_rd$rd_hi, 0)
  expect_identical(got_out$text$txt_nnt[row], "")

  # The row s3 stored is the row the sheet reads. That cache is the only source
  # the `PP results` sheet has for these counts. s3 now fills it whether or not
  # any figure asks for it.
  cached <- plan$results_ett[["ETT00001"]][["rd_pp_trunc"]]
  expect_false(is.null(cached))
  expect_equal(cached$n_persons_with_event_intervention, 2)
  expect_equal(cached$n_persons_with_event_comparator, 1)
  expect_equal(cached$rd, 1 / 2 - 1 / 3)

  # The cache carries the DECISION, not only the numbers a reader would have to
  # decide from again. RD is positive here, so the curve decided harm.
  expect_true(all(c("nnt", "nnt_direction") %in% names(cached)))
  expect_identical(cached$nnt_direction, "harm")
  expect_equal(cached$nnt, -6, tolerance = 1e-12)

  # The lookup the renderer received carries it too, from the same row.
  expect_identical(got_rd$nnt_direction, "harm")

  # s3 records what produced the interval, beside the interval.
  expect_identical(as.integer(cached$n_boot), 500L)
  expect_identical(as.integer(cached$seed), 1L)
  expect_equal(as.numeric(cached$conf_level), 0.95)
  expect_identical(as.character(cached$interval_status), "spans null")

  # The band-by-band curve is stored too, under its own slot.
  curve <- plan$results_ett[["ETT00001"]][["rd_curve_pp_trunc"]]
  expect_true(data.table::is.data.table(curve))
  expect_equal(curve$surv_comparator, c(1, 1 / 2))
  expect_equal(curve$surv_intervention, c(2 / 3, 1 / 3))
})

test_that("export path: the STUDY confidence level reaches the header, and a per-exhibit one is ignored", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("openxlsx")

  dir <- tempfile("rd_export_cl")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # The study asks for 90 percent. The exhibit asks for 99 percent. The study
  # wins, because s3 computed the interval at the study's level long before
  # this figure existed. This block asserted the exhibit's level until
  # 26.8.20, when the level moved to the study.
  plan <- rd_export_plan(dir)
  plan$spec <- list(study = list(implementation = list(conf_level = 0.90)))
  plan <- rd_export_run_s3(plan, dir, conf_level = 0.90)

  real_renderer <- swereg:::.render_combined_forest_plot
  got_rd <- NULL
  got_level <- "unset"
  got_out <- NULL
  testthat::local_mocked_bindings(
    .render_combined_forest_plot = function(
      ...,
      rd_lookup = NULL,
      rd_conf_level = 0.95
    ) {
      got_rd <<- rd_lookup
      got_level <<- rd_conf_level
      got_out <<- real_renderer(
        ...,
        rd_lookup = rd_lookup,
        rd_conf_level = rd_conf_level
      )
      got_out
    },
    .package = "swereg"
  )

  spec <- list(
    type = "forest",
    exposures = list("Exposure A" = "ETT00001"),
    estimands = "pp",
    label = "forest",
    risk_difference = TRUE,
    conf_level = 0.99
  )
  # The exhibit field is not silently dropped. It warns, and the warning names
  # where the level does belong.
  expect_warning(
    out <- swereg:::.plan_export_figure(plan, spec, file.path(dir, "fig")),
    "study\\$implementation\\$conf_level"
  )
  expect_true(file.exists(out))

  # One value, one source: the level s3 computed at is the level the renderer
  # was told to print, and the level the curve itself recorded.
  expect_equal(got_level, 0.90)
  expect_equal(got_rd$conf_level, 0.90)

  # And it is what the figure actually says. A hard-coded header would print
  # "95% CI" over a 90% interval, and the exhibit's 0.99 must reach nothing.
  drawn <- rd_drawn_labels(got_out)
  expect_true("Risk difference per 10,000\nat 52 wks (90% CI)" %in% drawn)
  expect_false("Risk difference per 10,000\nat 52 wks (95% CI)" %in% drawn)
  expect_false("Risk difference per 10,000\nat 52 wks (99% CI)" %in% drawn)

  # The bounds are the 90 percent bounds, not the 99 percent ones. A header
  # alone cannot prove the level reached the estimator.
  expect_equal(got_rd$rd_lo, -0.75)
  expect_equal(got_rd$rd_hi, 1)
})

test_that("the export path leaves the risk difference out unless it is asked for", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("openxlsx")

  dir <- tempfile("rd_export_off")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  plan <- rd_export_plan(dir)

  asked <- "unset"
  testthat::local_mocked_bindings(
    .render_combined_forest_plot = function(df, ..., rd_lookup = NULL) {
      asked <<- rd_lookup
      list(
        plot = ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
          ggplot2::geom_point(),
        width = 4,
        height = 4,
        text = df
      )
    },
    .package = "swereg"
  )
  spec <- list(
    type = "forest",
    exposures = list("Exposure A" = "ETT00001"),
    estimands = "pp",
    label = "forest"
  )
  swereg:::.plan_export_figure(plan, spec, file.path(dir, "fig"))
  # Computing it costs minutes per ETT, so an existing caller must not pay.
  expect_null(asked)
})

# --- assertion 6: the workbook keeps what the figure stopped drawing -------
#
# The figure dropped its person-count column on 2026-08-06. The counts and the
# risk difference now reach the reader through the `PP results` / `ITT results`
# sheets, so these tests are the only thing standing between that decision and
# losing both quantities from the deliverable.

# One ETT, with a cached risk-difference row of the shape `.forest_rd_row()`
# builds. Every number below is synthetic. Two of them carry a property the
# sheet must not round away. `rd` keeps four decimal places. The two person
# counts differ from the weighted event totals in the plan fixture, so a reader
# cannot mistake `Events` for `Persons with event`.
rd_results_row <- function() {
  data.table::data.table(
    ett_id = "ETT00001",
    band = 156L,
    rd = -5.1234e-4,
    rd_lo = -8.0e-4,
    rd_hi = -2.0e-4,
    n_persons_with_event_intervention = 20,
    n_persons_with_event_comparator = 200,
    conf_level = 0.95
  )
}

rd_results_plan <- function(rd_row = NULL) {
  ett <- data.table::data.table(
    enrollment_id = "01",
    ett_id = "ETT00001",
    outcome_var = "osd_a",
    outcome_name = "Outcome A",
    follow_up = 156L,
    age_min = 50L,
    age_max = 59L,
    age_group = "50_59",
    confounder_vars = "rd_age_continuous",
    person_id_var = "lopnr",
    treatment_var = "rd_tx",
    file_imp = "imp_01.qs2",
    file_raw = "raw_01.qs2",
    file_analysis = "analysis_001.qs2",
    description = "ETT00001"
  )
  plan <- swereg::TTEPlan$new(
    project_prefix = "test",
    skeleton_files = "skel.qs2",
    global_max_isoyearweek = "2020-52",
    ett = ett
  )
  # Synthetic. `events_weighted` keeps four decimal places on purpose. It is a
  # weighted sum over event ROWS. Printed as a whole number, it would read as a
  # person count.
  rates <- data.table::data.table(
    rd_tx = c(TRUE, FALSE),
    events_weighted = c(20.1234, 200.4321),
    py_weighted = c(100000, 400000),
    rate_per_100000py = c(20.1, 50.1)
  )
  data.table::setattr(rates, "treatment_var", "rd_tx")
  res <- list(
    enrollment_id = "01",
    description = "ETT00001",
    rates_pp_trunc = rates,
    rates_itt = rates,
    irr_pp_trunc = list(
      IRR = 0.40,
      IRR_lower = 0.20,
      IRR_upper = 0.80,
      IRR_pvalue = 0.01,
      skipped = FALSE
    ),
    irr_itt = list(
      IRR = 0.40,
      IRR_lower = 0.20,
      IRR_upper = 0.80,
      IRR_pvalue = 0.01,
      skipped = FALSE
    )
  )
  if (!is.null(rd_row)) {
    res[["rd_pp_trunc"]] <- rd_row
    res[["rd_itt"]] <- rd_row
  }
  plan$results_ett <- list(ETT00001 = res)
  plan
}

# The written sheet, read back as raw cells. `read.xlsx()` with column names on
# mangles every space to a dot, so the header row is read as data instead.
rd_sheet_cells <- function(plan, sheet_name, rates_slot, irr_slot, rd_slot) {
  wb <- openxlsx::createWorkbook()
  swereg:::.write_results_single(
    wb,
    sheet_name,
    plan,
    rates_slot = rates_slot,
    irr_slot = irr_slot,
    rd_slot = rd_slot,
    title = sheet_name
  )
  p <- tempfile(fileext = ".xlsx")
  on.exit(unlink(p), add = TRUE)
  openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
  raw <- openxlsx::read.xlsx(
    p,
    sheet = sheet_name,
    startRow = 3,
    colNames = FALSE
  )
  list(
    header = as.character(unlist(raw[1, ])),
    row = as.character(unlist(raw[2, ]))
  )
}

test_that("PP results carries the person counts and the signed risk difference", {
  skip_if_not_installed("openxlsx")
  got <- rd_sheet_cells(
    rd_results_plan(rd_results_row()),
    "PP results",
    "rates_pp_trunc",
    "irr_pp_trunc",
    "rd_pp_trunc"
  )

  expect_identical(
    got$header[15:18],
    c(
      "Persons with event (int)",
      "Persons with event (cmp)",
      "Risk difference per 10,000",
      "Risk difference 95% CI"
    )
  )
  # Distinct PEOPLE, unweighted.
  expect_identical(got$row[15:16], c("20", "200"))
  # Signed, per 10,000, at the horizon the `Follow-up (weeks)` column states.
  expect_identical(got$row[17], "-5.1234")
  expect_identical(got$row[18], "-8.00 to -2.00")
})

test_that("the decision columns add no column to the results sheet", {
  skip_if_not_installed("openxlsx")
  # The cache STORES the decision. The sheet RENDERS nothing new from it. A
  # rendered benefit-or-harm column is a separate decision for a later phase,
  # and this assertion is what stops it arriving by accident.
  without <- rd_results_row()
  with <- data.table::copy(without)
  with[, nnt := 1951.4]
  with[, nnt_direction := "benefit"]

  got_without <- rd_sheet_cells(
    rd_results_plan(without),
    "PP results",
    "rates_pp_trunc",
    "irr_pp_trunc",
    "rd_pp_trunc"
  )
  got_with <- rd_sheet_cells(
    rd_results_plan(with),
    "PP results",
    "rates_pp_trunc",
    "irr_pp_trunc",
    "rd_pp_trunc"
  )

  expect_identical(got_with$header, got_without$header)
  expect_identical(got_with$row, got_without$row)
  expect_length(got_with$header, 18L)
  expect_false(any(grepl("NNT", got_with$header, fixed = TRUE)))
  expect_false(any(grepl("benefit", got_with$row, fixed = TRUE)))
})

test_that("the sheet keeps the weighted events distinct from the person counts", {
  skip_if_not_installed("openxlsx")
  got <- rd_sheet_cells(
    rd_results_plan(rd_results_row()),
    "PP results",
    "rates_pp_trunc",
    "irr_pp_trunc",
    "rd_pp_trunc"
  )
  # `Events (int)` is a WEIGHTED sum over event ROWS and reads 20.1234 on the
  # same row where 20 distinct people had the outcome. Two columns that read as
  # the same quantity and are not is the defect; the headers are what separate
  # them.
  expect_identical(got$header[6], "Events (int)")
  expect_identical(got$row[6], "20.1234")
  expect_identical(got$header[15], "Persons with event (int)")
  expect_identical(got$row[15], "20")
  expect_false(identical(got$row[6], got$row[15]))
})

test_that("ITT results carries the same four columns from its own slot", {
  skip_if_not_installed("openxlsx")
  got <- rd_sheet_cells(
    rd_results_plan(rd_results_row()),
    "ITT results",
    "rates_itt",
    "irr_itt",
    "rd_itt"
  )
  expect_identical(got$header[15], "Persons with event (int)")
  expect_identical(got$header[17], "Risk difference per 10,000")
  expect_identical(got$row[17], "-5.1234")
})

test_that("with no risk difference cached the sheet keeps its 14 columns", {
  skip_if_not_installed("openxlsx")
  # Computing the risk difference costs minutes per ETT, so most exports have
  # none. Four empty columns would claim a quantity nobody computed.
  got <- rd_sheet_cells(
    rd_results_plan(NULL),
    "PP results",
    "rates_pp_trunc",
    "irr_pp_trunc",
    "rd_pp_trunc"
  )
  expect_length(got$header, 14L)
  expect_false(any(grepl("Risk difference", got$header, fixed = TRUE)))
  expect_false(any(grepl("Persons with event", got$header, fixed = TRUE)))
})

test_that("the interval header states the level the bounds were computed at", {
  skip_if_not_installed("openxlsx")
  rd90 <- rd_results_row()
  rd90$conf_level <- 0.90
  got <- rd_sheet_cells(
    rd_results_plan(rd90),
    "PP results",
    "rates_pp_trunc",
    "irr_pp_trunc",
    "rd_pp_trunc"
  )
  expect_identical(got$header[18], "Risk difference 90% CI")

  # And two levels under one header is refused, not averaged or picked from.
  expect_error(
    swereg:::.rd_sheet_conf_pct(c(0.95, 0.90)),
    "mix confidence levels"
  )
  expect_identical(swereg:::.rd_sheet_conf_pct(c(0.9, 0.9)), "90")
  expect_identical(swereg:::.rd_sheet_conf_pct(numeric(0)), "95")
})

# --- assertion 5: ylim on a cumulative-failure figure ----------------------

rd_export_survival <- function(dir, spec_extra) {
  # s3 first, because the figure reads the STORED curve. `rd_export_run_s3()`
  # runs the production worker, so the plan holds `rd_curve_pp_trunc` with the
  # per-arm head count of people at risk. The export path opens no analysis
  # file at all.
  plan <- rd_export_run_s3(rd_export_plan(dir), dir)
  spec <- c(
    list(
      type = "survival",
      enrollment = "01",
      outcome = "death",
      follow_up = 52L,
      age_group = "50_59",
      estimands = "pp",
      label = "surv"
    ),
    spec_extra
  )
  swereg:::.plan_export_figure(plan, spec, file.path(dir, "fig"))
}

test_that("a survival-scale ylim is translated onto the cumulative-failure scale", {
  skip_if_not_installed("qs2")
  dir <- tempfile("rd_ylim")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  real_renderer <- swereg:::.render_survival_curve
  got_ylim <- "unset"
  testthat::local_mocked_bindings(
    .render_survival_curve = function(..., ylim = NULL) {
      got_ylim <<- ylim
      real_renderer(..., ylim = ylim)
    },
    .package = "swereg"
  )
  rd_export_survival(dir, list(ylim = c(0.95, 1), ylim_scale = "survival"))
  # The figure plots 1 - survival, so the window has to be inverted AND its
  # bounds swapped. Passed through as given it would read c(0.95, 1).
  expect_equal(got_ylim, c(0, 0.05))
})

test_that("a survival-scale ylim does not blank the cumulative-failure figure", {
  skip_if_not_installed("qs2")
  dir <- tempfile("rd_ylim_blank")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  real_renderer <- swereg:::.render_survival_curve
  got_ylim <- "unset"
  got_plot <- NULL
  testthat::local_mocked_bindings(
    .render_survival_curve = function(..., ylim = NULL) {
      got_ylim <<- ylim
      got_plot <<- real_renderer(..., ylim = ylim)
      got_plot
    },
    .package = "swereg"
  )
  rd_export_survival(dir, list(ylim = c(0.95, 1), ylim_scale = "survival"))

  # Cumulative failures plotted are 0, 1/3, 1/2 and 2/3. Applied as given, the
  # window c(0.95, 1) contains NONE of them: coord_cartesian() clips the whole
  # curve and the panel comes out blank, with no error and no warning.
  y <- ggplot2::layer_data(got_plot, 1L)$y
  visible <- sum(y >= got_ylim[1] & y <= got_ylim[2])
  expect_gt(visible, 0L)
})

test_that("a cumulative-failure ylim is passed through untranslated", {
  skip_if_not_installed("qs2")
  dir <- tempfile("rd_ylim_cf")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  got_ylim <- "unset"
  real_renderer <- swereg:::.render_survival_curve
  testthat::local_mocked_bindings(
    .render_survival_curve = function(..., ylim = NULL) {
      got_ylim <<- ylim
      real_renderer(..., ylim = ylim)
    },
    .package = "swereg"
  )
  rd_export_survival(
    dir,
    list(ylim = c(0, 0.05), ylim_scale = "cumulative_failure")
  )
  expect_equal(got_ylim, c(0, 0.05))
})

test_that("an undeclared ylim scale is an error, not a guess", {
  skip_if_not_installed("qs2")
  dir <- tempfile("rd_ylim_undeclared")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  expect_error(
    rd_export_survival(dir, list(ylim = c(0.95, 1))),
    "ylim_scale"
  )
})

test_that("no ylim at all still renders, with no window applied", {
  skip_if_not_installed("qs2")
  dir <- tempfile("rd_ylim_none")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  got_ylim <- "unset"
  real_renderer <- swereg:::.render_survival_curve
  testthat::local_mocked_bindings(
    .render_survival_curve = function(..., ylim = NULL) {
      got_ylim <<- ylim
      real_renderer(..., ylim = ylim)
    },
    .package = "swereg"
  )
  out <- rd_export_survival(dir, list())
  expect_true(file.exists(out))
  expect_null(got_ylim)
})

# --- the results sheet and the figure must agree on an inestimable IRR -------
#
# An arm with no event gives an incidence rate ratio of exactly 0. That value is
# FINITE, so a guard written as `is.finite(irr)` passes it through and the sheet
# prints "0.00" beside "0.00 to 0.00" -- a point estimate of no risk, known
# perfectly. The figure has always refused this via `.ff_irr_ci()`, so before
# this test the two displays of the same result disagreed.

test_that("an inestimable IRR is blank on the figure", {
  expect_identical(swereg:::.ff_irr_ci(0, 0, 0), "")
  expect_identical(swereg:::.ff_irr_ci(0.005, 0.001, 0.02), "")
  # A real ratio is unaffected.
  expect_identical(
    swereg:::.ff_irr_ci(0.49, 0.30, 0.81),
    "0.49 (0.30 to 0.81)"
  )
})

test_that("an inestimable IRR is blank in the results sheet too", {
  zero <- list(
    events_intervention = 0,
    py_intervention = 100,
    rate_intervention = 0,
    events_cmp = 8,
    py_cmp = 100,
    rate_cmp = 8,
    irr = 0,
    lo = 0,
    hi = 0,
    pvalue = NA_real_
  )
  cells <- swereg:::.sensitivity_row_fmt(zero, "")
  irr_cell <- cells[[which(grepl("^IRR$", names(cells)))]]
  ci_cell <- cells[[which(grepl("CI", names(cells)))]]
  expect_true(is.na(irr_cell))
  expect_true(is.na(ci_cell))

  # A real ratio still renders, so the guard cannot be a blanket suppression.
  ok <- utils::modifyList(zero, list(irr = 0.49, lo = 0.30, hi = 0.81))
  cells_ok <- swereg:::.sensitivity_row_fmt(ok, "")
  expect_identical(cells_ok[[which(grepl("^IRR$", names(cells_ok)))]], "0.49")
  expect_identical(
    cells_ok[[which(grepl("CI", names(cells_ok)))]],
    "0.30 to 0.81"
  )
})
