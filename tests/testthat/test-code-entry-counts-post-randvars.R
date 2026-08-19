# The `counts` on an applied code entry MUST describe the skeleton that was
# written, not an intermediate state of it.
#
# A phase-3 randvar may delete rows. `Skeleton$apply_code_entry()` used to
# count at apply time. A delete that ran afterwards then left the stored
# count describing rows the written skeleton no longer held.
# `$compute_summary()` reads those counts back out of the meta sidecar, so
# the error reached `summary.qs2`.
#
# `RegistryStudy$save_skeleton()` now calls
# `Skeleton$refresh_code_entry_counts()` before it writes either file.

library(data.table)

# 6 persons, 4 weekly rows each: 24 rows in one batch.
.cec_weeks <- c("2020-01", "2020-02", "2020-03", "2020-04")

.cec_study <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = "grp1",
    batch_size = 6L
  )
  study$set_ids(1:6)
  study$save_rawbatch(
    "grp1",
    data.table::data.table(lopnr = 1:6, val = letters[1:6])
  )
  study
}

.cec_framework <- function(batch_data, config) {
  ids <- batch_data[["grp1"]]$lopnr
  d <- data.table::CJ(id = ids, isoyearweek = .cec_weeks)
  d[, `:=`(isoyear = 2020L, is_isoyear = FALSE)]
  d[]
}

# Marks `my_code` TRUE on weeks 2020-01 and 2020-02. That is 12 TRUE cells
# over 6 persons, out of the batch's 24 rows.
.cec_code_fn <- function(skeleton, dataset, id_name, codes, ...) {
  for (nm in names(codes)) {
    skeleton[, (nm) := isoyearweek %in% c("2020-01", "2020-02")]
  }
  invisible(skeleton)
}

# Deletes the rows of persons 1 to 3 in weeks 2020-01 and 2020-02. That is
# 6 rows of 24, and every one of them carries a TRUE `my_code` cell. The
# batch keeps 18 rows and 6 TRUE cells over 3 persons.
#
# The fn returns a new data.table rather than mutating in place, which is
# the form `Skeleton$sync_randvars()` rebinds `self$data` to.
.cec_filter_fn <- function(skeleton, batch_data, config) {
  skeleton[!(id <= 3L & isoyearweek %in% c("2020-01", "2020-02"))]
}

.cec_register_codes <- function(study) {
  study$register_codes(
    codes = list(my_code = "X"),
    fn = .cec_code_fn,
    groups = list("grp1"),
    label = "mycode"
  )
}

# Recompute from the final skeleton with the two shipped helpers. This is
# the ground truth the stored counts must match.
.cec_recompute <- function(sk, fp) {
  stored <- sk$applied_registry[[fp]]
  cols <- intersect(swereg:::.entry_columns(stored), names(sk$data))
  swereg:::.compute_entry_column_counts(sk$data, cols)
}


test_that("stored counts survive a randvar registered after the code entry", {
  study <- .cec_study()
  study$register_framework(.cec_framework)
  .cec_register_codes(study)

  # Run 1. No randvar yet, so the code entry counts the full 24 rows.
  study$process_skeletons()

  sk1 <- study$load_skeleton(1L)
  fp <- names(sk1$applied_registry)[[1L]]
  expect_equal(nrow(sk1$data), 24L)
  expect_equal(sk1$applied_registry[[fp]]$counts$my_code$n_persons_with, 6L)
  expect_equal(
    sk1$applied_registry[[fp]]$counts$my_code$n_person_weeks_with,
    12L
  )

  # Run 2. The randvar is new, so phase 3 replays it over a skeleton that
  # already holds `my_code`. Phase 2 sees no registry change and never
  # calls $apply_code_entry() again.
  study$register_randvars("rv_filter", .cec_filter_fn)
  study$process_skeletons()

  sk2 <- study$load_skeleton(1L)
  expect_equal(nrow(sk2$data), 18L)
  expect_equal(sum(sk2$data$my_code), 6L)

  stored <- sk2$applied_registry[[fp]]$counts
  expect_equal(stored, .cec_recompute(sk2, fp))

  # The post-filter numbers, stated outright. Pre-filter they are 6 and 12.
  expect_equal(stored$my_code$n_persons_with, 3L)
  expect_equal(stored$my_code$n_person_weeks_with, 6L)
  expect_equal(stored$my_code$n_person_years_with, 0L)

  # The meta sidecar is what $compute_summary() reads.
  meta <- study$load_skeleton_meta(1L)
  expect_equal(meta$applied_registry[[fp]]$counts, stored)
})


test_that("stored counts match the final skeleton when both are registered at once", {
  # Phase 3 currently runs before phase 2, so a randvar registered up front
  # deletes its rows before any code column exists. This test therefore
  # passes under apply-time counting too. It is here to hold the invariant
  # when the phase order is swapped.
  study <- .cec_study()
  study$register_framework(.cec_framework)
  study$register_randvars("rv_filter", .cec_filter_fn)
  .cec_register_codes(study)

  study$process_skeletons()

  sk <- study$load_skeleton(1L)
  fp <- names(sk$applied_registry)[[1L]]
  expect_equal(nrow(sk$data), 18L)
  expect_equal(sum(sk$data$my_code), 6L)

  stored <- sk$applied_registry[[fp]]$counts
  expect_equal(stored, .cec_recompute(sk, fp))
  expect_equal(stored$my_code$n_persons_with, 3L)
  expect_equal(stored$my_code$n_person_weeks_with, 6L)
})
