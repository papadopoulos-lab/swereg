# Stamp a hand-built plan as the real stages do (#52).
#
# s1 sets `plan$s1_run_id` last. s2 copies it into the `s1_run_id` column of
# every ETT, and writes a new s2 run id into the `s2_run_id` column. s3 copies
# both into each result it stores. s2, s3 and the writers stop without these
# stamps. A test that builds a plan without the real stages calls this after
# it sets the decision and the stored results. The return value is the plan.
# A list plan MUST be reassigned from it.
tte_stamp_stages <- function(plan) {
  id <- swereg:::.tte_s1_run_new_id(plan)
  plan$s1_run_id <- id
  s2_id <- swereg:::.tte_s2_run_new_id(plan)
  if (!is.null(plan$ett)) {
    ett <- data.table::copy(plan$ett)
    data.table::set(ett, j = "s1_run_id", value = rep(id, nrow(ett)))
    data.table::set(ett, j = "s2_run_id", value = rep(s2_id, nrow(ett)))
    plan$ett <- ett
  }
  for (k in names(plan$results_enrollment)) {
    plan$results_enrollment[[k]][["s1_run_id"]] <- id
    plan$results_enrollment[[k]][["s2_run_id"]] <- s2_id
  }
  # s3 also stores a horizon record for each estimand, and the writers stop
  # without one. This record holds the decision "nothing withheld" and the
  # rule of the spec. It holds no window table and no status, so no sheet
  # changes. The intention-to-treat record carries the per-protocol decision,
  # as `.s3_apply_horizon_rule()` writes it. A record that is already there
  # stays.
  rule <- plan$spec$min_expected_events_per_arm_per_year
  for (k in names(plan$results_ett)) {
    plan$results_ett[[k]][["s1_run_id"]] <- id
    plan$results_ett[[k]][["s2_run_id"]] <- s2_id
    for (est in c("pp", "itt")) {
      slot <- paste0("horizon_", est)
      if (!is.null(rule) && is.null(plan$results_ett[[k]][[slot]])) {
        plan$results_ett[[k]][[slot]] <- list(
          rule = list(min_expected_events = as.numeric(rule)),
          withheld = FALSE,
          max_passing_cut = max(swereg:::.HORIZON_CUT_GRID)
        )
        # The per-year decision that s3 stores since swereg 27.3.1, also
        # "nothing withheld".
        fu <- as.numeric(plan$ett$follow_up[plan$ett$ett_id == k])
        if (length(fu) == 1L && is.finite(fu) && fu > 0) {
          plan$results_ett[[k]][[slot]]$by_year <- data.table::data.table(
            year = seq_len(ceiling(fu / 52)),
            reportable = TRUE,
            max_passing_cut = max(swereg:::.HORIZON_CUT_GRID)
          )
        }
        if (est == "itt") {
          plan$results_ett[[k]][[slot]]$decided_by <- "pp"
        }
      }
    }
  }
  return(invisible(plan))
}
