# Fill of the plain (follow-up) confounder columns of a trial panel.
#
# The fill carries the last observed value forward inside each person-trial and
# seeds the carry with the `.tte_entry__` snapshot. Every fixture below is
# hand-built at trial level, so the expected values are readable off the page.

.tef_key <- c("enrollment_person_trial_id", "tstart", "tstop")

.tef_design <- function(confounder_vars = "band") {
  TTEDesign$new(
    treatment_var = "exposed",
    outcome_vars = "event",
    confounder_vars = confounder_vars,
    follow_up_time = 12L
  )
}

.tef_trial <- function(d, design = .tef_design()) {
  TTEEnrollment$new(d, design)
}

# One person-trial. Entry snapshot "A", plain c(NA, "B", NA, NA).
.tef_panel_one <- function() {
  data.table::data.table(
    enrollment_person_trial_id = rep("t1", 4L),
    tstart = c(0L, 4L, 8L, 12L),
    tstop = c(4L, 8L, 12L, 16L),
    exposed = TRUE,
    event = 0L,
    band = c(NA, "B", NA, NA),
    .tte_entry__band = "A"
  )
}

# Two person-trials. The second opens on NA with entry snapshot "C", so a carry
# that crossed person-trials would hand it "B" from the first.
.tef_panel_two <- function() {
  data.table::rbindlist(list(
    .tef_panel_one(),
    data.table::data.table(
      enrollment_person_trial_id = rep("t2", 2L),
      tstart = c(0L, 4L),
      tstop = c(4L, 8L),
      exposed = FALSE,
      event = 0L,
      band = NA_character_,
      .tte_entry__band = "C"
    )
  ))
}

.tef_aggregates <- function(
  rows_na_n,
  trials_na_n,
  trials_entry_na_n,
  rows_leading,
  key_digest = "k1",
  confounder = "band"
) {
  out <- data.table::data.table(
    confounder = confounder,
    rows_n = rep(6L, length(confounder)),
    trials_n = rep(2L, length(confounder)),
    rows_na_n = rows_na_n,
    trials_na_n = trials_na_n,
    trials_entry_na_n = trials_entry_na_n,
    rows_leading_na_in_entry_na_trials_n = rows_leading,
    key_digest = key_digest
  )
  data.table::setattr(
    out,
    "class",
    c("tteenrollment_fill_aggregates", "data.table", "data.frame")
  )
  out[]
}

# Three confounders, and the two tables list them in opposite order. Every
# per-confounder count is chosen so that a positional subtraction and a
# name-aligned subtraction give different answers.
.tef_reorder_pair <- function() {
  list(
    raw = .tef_aggregates(
      rows_na_n = c(10L, 20L, 30L),
      trials_na_n = c(5L, 6L, 7L),
      trials_entry_na_n = c(1L, 2L, 3L),
      rows_leading = c(1L, 2L, 3L),
      confounder = c("a", "b", "c")
    ),
    imp = .tef_aggregates(
      rows_na_n = c(3L, 2L, 1L),
      trials_na_n = c(3L, 2L, 1L),
      trials_entry_na_n = c(3L, 2L, 1L),
      rows_leading = c(3L, 2L, 1L),
      confounder = c("c", "b", "a")
    )
  )
}

test_that("the fill seeds the first row from the entry snapshot", {
  trial <- .tef_trial(.tef_panel_one())
  tteenrollment_fill_followup_confounders(trial)

  expect_identical(trial$data$band[1L], "A")
  expect_identical(trial$data$band, c("A", "B", "B", "B"))
})

test_that("the fill never carries a value across person-trials", {
  trial <- .tef_trial(.tef_panel_two())
  tteenrollment_fill_followup_confounders(trial)

  d <- trial$data
  expect_identical(
    d$band[d$enrollment_person_trial_id == "t2"],
    c("C", "C")
  )
  expect_identical(d$band, c("A", "B", "B", "B", "C", "C"))
})

test_that("the fill orders the panel before it carries", {
  sorted <- .tef_trial(.tef_panel_two())
  tteenrollment_fill_followup_confounders(sorted)

  shuffled <- .tef_trial(.tef_panel_two()[c(5L, 2L, 6L, 1L, 4L, 3L)])
  tteenrollment_fill_followup_confounders(shuffled)

  a <- data.table::copy(sorted$data)
  b <- data.table::copy(shuffled$data)
  joined <- a[b, on = .tef_key]

  expect_identical(nrow(joined), 6L)
  expect_identical(joined$band, joined$i.band)
  expect_identical(sorted$data$band, c("A", "B", "B", "B", "C", "C"))
})

test_that("the fill changes no observed value", {
  before <- .tef_panel_two()
  trial <- .tef_trial(before)
  tteenrollment_fill_followup_confounders(trial)

  data.table::setkeyv(before, .tef_key)
  after <- data.table::copy(trial$data)
  data.table::setkeyv(after, .tef_key)
  observed <- !is.na(before$band)

  expect_true(any(observed))
  expect_identical(after$band[observed], before$band[observed])
})

test_that("the fill keeps the row count and marks the step once", {
  trial <- .tef_trial(.tef_panel_two())
  rows_before <- nrow(trial$data)

  tteenrollment_fill_followup_confounders(trial)
  first <- data.table::copy(trial$data)
  tteenrollment_fill_followup_confounders(trial)

  expect_identical(nrow(trial$data), rows_before)
  expect_identical(sum(trial$steps_completed == "fill_followup"), 1L)
  expect_identical(trial$data$band, first$band)
})

test_that("the fill stops when the entry snapshot cannot seed a person-trial", {
  d <- .tef_panel_one()
  d[, .tte_entry__band := NA_character_]
  trial <- .tef_trial(d)

  expect_error(tteenrollment_fill_followup_confounders(trial), "'band'")
  expect_error(
    tteenrollment_fill_followup_confounders(trial),
    "s1_impute_confounders"
  )
})

test_that("an NA entry snapshot is harmless when the plain column is full", {
  d <- .tef_panel_one()
  d[, band := c("A", "B", "C", "D")]
  d[, .tte_entry__band := NA_character_]
  trial <- .tef_trial(d)

  expect_silent(tteenrollment_fill_followup_confounders(trial))
  expect_identical(trial$data$band, c("A", "B", "C", "D"))
})

test_that("the summary subtracts the imputed aggregates from the raw ones", {
  raw <- .tef_aggregates(
    rows_na_n = 5L,
    trials_na_n = 2L,
    trials_entry_na_n = 1L,
    rows_leading = 3L
  )
  imp <- .tef_aggregates(
    rows_na_n = 1L,
    trials_na_n = 1L,
    trials_entry_na_n = 1L,
    rows_leading = 3L
  )
  s <- tteenrollment_fill_summary(raw, imp)

  expect_identical(s$rows_filled_n, 4L)
  expect_identical(s$trials_filled_n, 1L)
  expect_identical(s$trials_entry_imputed_n, 1L)
  expect_identical(s$rows_from_imputed_entry_n, 3L)
  expect_identical(s$rows_n, 6L)
})

test_that("the fill handles character, integer and numeric confounders", {
  d <- data.table::data.table(
    enrollment_person_trial_id = rep("t1", 3L),
    tstart = c(0L, 4L, 8L),
    tstop = c(4L, 8L, 12L),
    exposed = TRUE,
    event = 0L,
    band = c(NA, "B", NA),
    count = c(NA_integer_, 7L, NA_integer_),
    score = c(NA_real_, 1.5, NA_real_),
    .tte_entry__band = "A",
    .tte_entry__count = 3L,
    .tte_entry__score = 0.5
  )
  trial <- .tef_trial(d, .tef_design(c("band", "count", "score")))
  tteenrollment_fill_followup_confounders(trial)

  expect_identical(trial$data$band, c("A", "B", "B"))
  expect_identical(trial$data$count, c(3L, 7L, 7L))
  expect_identical(trial$data$score, c(0.5, 1.5, 1.5))
})

test_that("rows_from_imputed_entry_n counts the leading NA run only", {
  d <- data.table::data.table(
    enrollment_person_trial_id = rep("t1", 3L),
    tstart = c(0L, 4L, 8L),
    tstop = c(4L, 8L, 12L),
    exposed = TRUE,
    event = 0L,
    band = c(NA, "B", NA),
    .tte_entry__band = NA_character_
  )
  agg <- tteenrollment_fill_aggregates(.tef_trial(d))

  expect_identical(agg$rows_leading_na_in_entry_na_trials_n, 1L)
  expect_identical(agg$rows_na_n, 2L)
  expect_identical(agg$trials_entry_na_n, 1L)
  expect_identical(
    tteenrollment_fill_summary(agg, agg)$rows_from_imputed_entry_n,
    1L
  )
})

test_that("the summary stops on two aggregates from different panels", {
  a <- tteenrollment_fill_aggregates(.tef_trial(.tef_panel_one()))
  b <- tteenrollment_fill_aggregates(.tef_trial(.tef_panel_two()))

  expect_false(identical(a$key_digest, b$key_digest))
  expect_error(tteenrollment_fill_summary(a, b), "not the same panel")
})

test_that("the aggregates match hand-computed counts on the two-trial panel", {
  trial <- .tef_trial(.tef_panel_two())
  agg <- tteenrollment_fill_aggregates(trial)

  expect_identical(agg$confounder, "band")
  expect_identical(agg$rows_n, 6L)
  expect_identical(agg$trials_n, 2L)
  expect_identical(agg$rows_na_n, 5L)
  expect_identical(agg$trials_na_n, 2L)
  expect_identical(agg$trials_entry_na_n, 0L)
  expect_identical(agg$rows_leading_na_in_entry_na_trials_n, 0L)

  tteenrollment_fill_followup_confounders(trial)
  after <- tteenrollment_fill_aggregates(trial)

  expect_identical(after$rows_na_n, 0L)
  expect_identical(after$trials_na_n, 0L)
  expect_identical(after$key_digest, agg$key_digest)
})

test_that("the summary aligns the two aggregate tables by confounder name", {
  p <- .tef_reorder_pair()
  s <- tteenrollment_fill_summary(p$raw, p$imp)

  expect_identical(s$confounder, c("a", "b", "c"))
  # Name-aligned: 10-1, 20-2, 30-3. Positional would give 10-3, 20-2, 30-1.
  expect_identical(s$rows_filled_n, c(9L, 18L, 27L))
  # Name-aligned: 5-1, 6-2, 7-3. Positional would give 5-3, 6-2, 7-1.
  expect_identical(s$trials_filled_n, c(4L, 4L, 4L))
  # These two columns come from `raw` alone, so they carry raw's order.
  expect_identical(s$trials_entry_imputed_n, c(1L, 2L, 3L))
})

test_that("the aggregates leave the row order of trial$data alone", {
  trial <- .tef_trial(.tef_panel_two()[c(5L, 2L, 6L, 1L, 4L, 3L)])
  before <- data.table::copy(trial$data)
  agg <- tteenrollment_fill_aggregates(trial)

  expect_identical(trial$data$tstart, before$tstart)
  expect_identical(
    trial$data$enrollment_person_trial_id,
    before$enrollment_person_trial_id
  )
  expect_s3_class(agg, "tteenrollment_fill_aggregates")
  expect_identical(agg$rows_na_n, 5L)
})

test_that("a panel with no entry snapshot comes back untouched", {
  d <- .tef_panel_one()
  d[, .tte_entry__band := NULL]
  trial <- .tef_trial(d)
  tteenrollment_fill_followup_confounders(trial)

  expect_identical(trial$data$band, c(NA, "B", NA, NA))
  expect_identical(trial$steps_completed, character())
  expect_identical(nrow(tteenrollment_fill_aggregates(trial)), 0L)
})

test_that("$s1b_fill_followup_confounders() runs the same fill", {
  trial <- .tef_trial(.tef_panel_two())
  trial$s1b_fill_followup_confounders()

  expect_identical(trial$data$band, c("A", "B", "B", "B", "C", "C"))
  expect_true("fill_followup" %in% trial$steps_completed)
  expect_null(trial$fill_summary)
})

test_that("the summary accepts TTEEnrollment objects on both sides", {
  trial <- .tef_trial(.tef_panel_two())
  raw <- tteenrollment_fill_aggregates(trial)
  tteenrollment_fill_followup_confounders(trial)
  s <- tteenrollment_fill_summary(raw, trial)

  expect_identical(s$rows_filled_n, 5L)
  expect_identical(s$trials_filled_n, 2L)
  expect_error(tteenrollment_fill_summary(raw, "not a panel"), "`imp`")
})

# --- The exported surface and the two fields that carry it --------------------

test_that("NAMESPACE exports the three fill functions", {
  fns <- c(
    "tteenrollment_fill_followup_confounders",
    "tteenrollment_fill_aggregates",
    "tteenrollment_fill_summary"
  )

  for (f in fns) {
    expect_true(exists(f, envir = asNamespace("swereg"), inherits = FALSE))
  }
  expect_true(all(fns %in% getNamespaceExports("swereg")))

  # NAMESPACE is the file an installed build reads, so assert that file. A
  # namespace check alone passes on a build whose NAMESPACE was never
  # regenerated.
  ns_path <- file.path(system.file(package = "swereg"), "NAMESPACE")
  expect_true(file.exists(ns_path))
  expect_true(all(
    paste0("export(", fns, ")") %in% readLines(ns_path, warn = FALSE)
  ))
})

test_that("fill_summary and ps_fit are public fields and start NULL", {
  fields <- names(TTEEnrollment$public_fields)
  expect_true("fill_summary" %in% fields)
  expect_true("ps_fit" %in% fields)

  trial <- .tef_trial(.tef_panel_two())
  expect_null(trial$fill_summary)
  expect_null(trial$ps_fit)

  # R6 locks the instance environment, so a successful assignment proves the
  # binding already exists. A renamed field stops here.
  trial$fill_summary <- data.table::data.table(confounder = "band")
  trial$ps_fit <- data.table::data.table(n_fit = 1L)
  expect_identical(trial$fill_summary$confounder, "band")
  expect_identical(trial$ps_fit$n_fit, 1L)
})

test_that("an object saved before the two fields existed reads NULL", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("withr")

  # A stand-in for a release before the two fields were added. It carries the
  # same class name and goes through the same serialiser, and it holds neither
  # binding.
  old <- R6::R6Class(
    "TTEEnrollment",
    public = list(
      data = NULL,
      design = NULL,
      data_level = "trial",
      steps_completed = character()
    )
  )$new()
  path <- withr::local_tempfile(fileext = ".qs2")
  qs2::qs_save(old, path)
  back <- qs2::qs_read(path)

  expect_s3_class(back, "TTEEnrollment")
  expect_false("fill_summary" %in% ls(back, all.names = TRUE))
  expect_false("ps_fit" %in% ls(back, all.names = TRUE))
  expect_null(back$fill_summary)
  expect_null(back$ps_fit)
})

test_that("a second fill appends no second token and refills nothing", {
  trial <- .tef_trial(.tef_panel_two())

  tteenrollment_fill_followup_confounders(trial)
  band_after_first <- trial$data$band
  steps_after_first <- trial$steps_completed
  agg_after_first <- tteenrollment_fill_aggregates(trial)

  tteenrollment_fill_followup_confounders(trial)

  expect_identical(sum(trial$steps_completed == "fill_followup"), 1L)
  expect_identical(trial$steps_completed, steps_after_first)
  expect_identical(trial$data$band, band_after_first)
  expect_identical(
    as.data.frame(tteenrollment_fill_aggregates(trial)),
    as.data.frame(agg_after_first)
  )
})

test_that("the fill counts the person-trials it cannot seed", {
  d <- .tef_panel_two()
  d[enrollment_person_trial_id == "t2", .tte_entry__band := NA_character_]
  trial <- .tef_trial(d)

  # Only t2 loses its seed, so the message names one person-trial.
  expect_error(
    tteenrollment_fill_followup_confounders(trial),
    "s1_impute_confounders"
  )
  expect_error(
    tteenrollment_fill_followup_confounders(trial),
    "1 person-trial\\(s\\)"
  )

  # The same panel with the seed in place raises nothing.
  expect_silent(
    tteenrollment_fill_followup_confounders(.tef_trial(.tef_panel_two()))
  )
})

test_that("the summary stops on two tables covering different confounders", {
  raw <- .tef_aggregates(
    rows_na_n = c(10L, 20L),
    trials_na_n = c(5L, 6L),
    trials_entry_na_n = c(1L, 2L),
    rows_leading = c(1L, 2L),
    confounder = c("a", "b")
  )
  imp <- .tef_aggregates(
    rows_na_n = c(3L, 4L),
    trials_na_n = c(3L, 4L),
    trials_entry_na_n = c(1L, 2L),
    rows_leading = c(1L, 2L),
    confounder = c("a", "c")
  )

  # The key digest matches, so only the confounder-set guard can stop this.
  expect_identical(unique(raw$key_digest), unique(imp$key_digest))
  expect_error(
    tteenrollment_fill_summary(raw, imp),
    "different confounder sets"
  )
  expect_error(tteenrollment_fill_summary(raw, imp), "b, c")
})
