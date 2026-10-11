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
  for (k in names(plan$results_ett)) {
    plan$results_ett[[k]][["s1_run_id"]] <- id
    plan$results_ett[[k]][["s2_run_id"]] <- s2_id
  }
  return(invisible(plan))
}
