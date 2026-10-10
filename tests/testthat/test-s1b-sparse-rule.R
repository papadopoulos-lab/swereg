# The sparse_rule and the s1b step (#52), through the real s1 dispatch.
#
# The rule decides once per study, from the s1a tuples of the driving
# enrollments, before the comparator draw. The plan freezes the decision, and
# every enrollment applies it: the person-trials that hold an excluded
# condition at their recruiting week leave in the order of the spec, and the
# condition leaves every confounder set. Without the block, s1 writes what
# 27.2.0 wrote.
#
# The fixture is in helper-sparse_rule.R. Two real s1 runs drive every test
# below, and each run is made once per file. test-s1b-sparse-rule-spec.R holds
# the tests that need no s1 run.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")

spr_runs <- new.env(parent = emptyenv())

# `rule`: the three candidates, two of them rare. `none`: no sparse_rule.
spr_get <- function(name) {
  if (is.null(spr_runs[[name]])) {
    rule <- if (identical(name, "rule")) spr_rule() else NULL
    spr_runs[[name]] <- spr_run_s1(rule)
  }
  return(spr_runs[[name]])
}

# The global rows of one enrollment's attrition, in pipeline order.
spr_global <- function(plan, eid) {
  att <- plan$enrollment_counts[[eid]]$attrition
  return(att[is.na(att$enrollment_period_id)])
}

# The step-2 rows: every global row after the last s1a cascade row.
spr_step2 <- function(plan, eid) {
  g <- spr_global(plan, eid)
  last_s1a <- which(g$criterion == "landmark_event_free")
  return(g[seq_len(nrow(g)) > last_s1a])
}

SPR_SOURCES <- c("osd_rare_b", "osd_common", "osd_rare_a")
SPR_KEPT <- c("rd_age_continuous", "rd_no_osd_common_everbefore")

# ---------------------------------------------------------------------------
# s1a: the baseline columns on the tuples
# ---------------------------------------------------------------------------

test_that("s1a writes each candidate's baseline value on every tuple", {
  r <- spr_get("rule")
  # The fixture MUST hold a tuple whose recruiting week directly follows the
  # mid-2018 event, or a read of the wrong week could not show.
  mid_idx0 <- match(spr_mid_week(), cstime::dates_by_isoyearweek$isoyearweek) -
    1L
  tp01 <- r$pre[["01"]]$tuples
  expect_true(any(
    tp01$recruit_week_index == mid_idx0 + 1L &
      spr_holds(r$sk, tp01, "osd_common")
  ))
  for (eid in c("01", "02")) {
    tp <- r$pre[[eid]]$tuples
    for (src in SPR_SOURCES) {
      expect_identical(
        tp[[spr_var(src)]],
        !spr_holds(r$sk, tp, src),
        info = paste(eid, src)
      )
    }
  }
})

# ---------------------------------------------------------------------------
# The decision
# ---------------------------------------------------------------------------

test_that("s1b excludes the rare conditions and adjusts for the common one", {
  r <- spr_get("rule")
  d <- r$plan$get_sparse_decision()
  tp <- r$pre[["01"]]$tuples

  expect_identical(d$variable, spr_var(SPR_SOURCES))
  expect_identical(
    d$label,
    c("Prior rare condition B", "Prior common condition", "Prior rare condition A")
  )
  # The hand count: unique persons per arm with a holding tuple in 01.
  hand <- function(src, arm) {
    held <- spr_holds(r$sk, tp, src) & tp$intervention == arm
    return(length(unique(tp$id[held])))
  }
  expect_identical(
    d$n_01_intervention,
    vapply(SPR_SOURCES, hand, integer(1), arm = TRUE, USE.NAMES = FALSE)
  )
  expect_identical(
    d$n_01_comparator,
    vapply(SPR_SOURCES, hand, integer(1), arm = FALSE, USE.NAMES = FALSE)
  )
  expect_identical(d$smallest, pmin(d$n_01_intervention, d$n_01_comparator))
  expect_identical(d$decision, c("excluded", "adjusted", "excluded"))
  expect_true(all(d$threshold == 10L))
  expect_true(all(d$decided_by == "01"))
  expect_s3_class(d$decided_at, "POSIXct")
  expect_identical(
    r$plan$sparse_rule_hash,
    swereg:::.tte_sparse_rule_hash(r$plan$spec$sparse_rule)
  )
})

test_that("excluded conditions leave every enrollment's confounder set", {
  r <- spr_get("rule")
  for (cv in r$plan$ett$confounder_vars) {
    expect_identical(cv, SPR_KEPT)
  }
  for (i in seq_len(2L)) {
    expect_identical(r$plan$enrollment_spec(i)$design$confounder_vars, SPR_KEPT)
  }
})

test_that("the non-driving enrollment inherits the exclusions and the confounder set", {
  r <- spr_get("rule")
  d <- r$plan$get_sparse_decision()
  # Enrollment 02 did not drive: the decision holds no count of it.
  expect_false(any(grepl("^n_02_", names(d))))

  # 02 alone would have excluded the common condition: fewer than 10 persons
  # per arm of 02 hold it.
  tp02 <- r$pre[["02"]]$tuples
  common <- spr_holds(r$sk, tp02, "osd_common")
  for (arm in c(TRUE, FALSE)) {
    expect_lt(length(unique(tp02$id[common & tp02$intervention == arm])), 10L)
  }

  s01 <- spr_step2(r$plan, "01")$criterion
  s02 <- spr_step2(r$plan, "02")$criterion
  expect_length(s02, 2L)
  expect_identical(s02, s01)
  expect_true(all(endsWith(
    s02,
    c("Prior rare condition B", "Prior rare condition A")
  )))
  cv <- r$plan$ett$confounder_vars[r$plan$ett$enrollment_id == "02"]
  expect_identical(cv[[1]], SPR_KEPT)
})

test_that("the CONSORT step-2 counts match a hand count, in spec order", {
  r <- spr_get("rule")
  for (eid in c("01", "02")) {
    tp <- r$pre[[eid]]$tuples
    g <- spr_global(r$plan, eid)
    s2 <- spr_step2(r$plan, eid)
    # rare B first, then rare A, as `candidates` orders them.
    expect_true(all(endsWith(
      s2$criterion,
      c("Prior rare condition B", "Prior rare condition A")
    )))
    after_b <- tp[!spr_holds(r$sk, tp, "osd_rare_b")]
    after_a <- after_b[!spr_holds(r$sk, after_b, "osd_rare_a")]
    expect_identical(
      s2$n_persons,
      c(length(unique(after_b$id)), length(unique(after_a$id))),
      info = eid
    )
    expect_identical(
      s2$n_person_trials,
      c(nrow(after_b), nrow(after_a)),
      info = eid
    )
    expect_identical(
      s2$n_comparator,
      c(sum(!after_b$intervention), sum(!after_a$intervention)),
      info = eid
    )
    # Step 2 starts from the last step-1 row, which counts every tuple.
    expect_identical(
      g$n_person_trials[g$criterion == "landmark_event_free"],
      nrow(tp),
      info = eid
    )

    # The cohort flow renders both as exclusions, with their own deltas.
    flow <- swereg:::.build_cohort_flow(r$plan$enrollment_counts[[eid]])
    rows <- flow[flow$step %in% s2$criterion]
    expect_identical(rows$kind, c("exclusion", "exclusion"), info = eid)
    expect_identical(
      as.numeric(rows$change_persons),
      as.numeric(c(
        length(unique(tp$id)) - length(unique(after_b$id)),
        length(unique(after_b$id)) - length(unique(after_a$id))
      )),
      info = eid
    )
  }
})

test_that("each step-2 criterion is the exact label string", {
  r <- spr_get("rule")
  expect_identical(
    spr_step2(r$plan, "01")$criterion,
    c(
      "Excluded (fewer than 10 at baseline): Prior rare condition B",
      "Excluded (fewer than 10 at baseline): Prior rare condition A"
    )
  )
})

# ---------------------------------------------------------------------------
# Parity without the rule, against what 27.2.0 wrote
# ---------------------------------------------------------------------------

spr_ref <- function() {
  return(readRDS(test_path(
    "fixtures",
    "sparse-rule-27.2.0",
    "reference.rds"
  )))
}

test_that("without a sparse_rule the enrollment counts equal 27.2.0's", {
  r <- spr_get("none")
  plain <- lapply(r$plan$enrollment_counts, function(ec) {
    return(lapply(ec, as.data.frame))
  })
  expect_identical(plain, spr_ref()$enrollment_counts)
})

test_that("without a sparse_rule the confounder sets equal 27.2.0's", {
  r <- spr_get("none")
  expect_identical(r$plan$ett$confounder_vars, spr_ref()$confounder_vars)
  expect_null(r$plan$get_sparse_decision())
  expect_null(r$plan$sparse_rule_hash)
})

test_that("without a sparse_rule the s1a pre files carry 27.2.0's columns", {
  r <- spr_get("none")
  ref <- spr_ref()
  expect_identical(lapply(r$pre, function(p) names(p$tuples)), ref$tuple_cols)
  expect_identical(
    lapply(r$pre, function(p) names(p$attrition)),
    ref$attrition_cols
  )
})
