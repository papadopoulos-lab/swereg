# Times swereg's PRE2DUP port, .pre2dup_periods(), against
# PRE2DUPR::pre2dup(data_to_return = "periods") on one synthetic population:
# the scenario 11 mix of tests/testthat/test-pre2dup_equivalence.R, scaled to
# 900 persons and 110,000 purchases, with hospital stays. Each side runs three
# times, and the script prints the medians.
#
# Run from the package root, with PRE2DUPR in a scratch library:
#   R_LIBS=~/R-scratch/pre2dupr-43b6d89 Rscript dev/pre2dup_benchmark.R

stopifnot(requireNamespace("PRE2DUPR", quietly = TRUE))
pkgload::load_all(".", quiet = TRUE)
library(data.table)

# The fixture helpers of the equivalence test: every top-level assignment to
# a name that starts with ".p2d_". The test_that() blocks do not run.
test_file <- "tests/testthat/test-pre2dup_equivalence.R"
for (e in parse(test_file, keep.source = FALSE)) {
  if (
    is.call(e) &&
      identical(e[[1L]], as.name("<-")) &&
      startsWith(as.character(e[[2L]]), ".p2d_")
  ) {
    eval(e, envir = globalenv())
  }
}

n_persons <- 900L
n_purchases <- 110000L

# The scenario 11 mix with 900 persons. The regular and early-refill users
# have long purchase histories, so the population reaches 110,000 purchases.
# Every id has six digits, so PRE2DUP-R's stale selection does not apply.
seed <- 211L
parts <- list(
  .p2d_gen_regular(192L, .p2d_id0 + 0L, seed + 1L, k_range = 300:380),
  .p2d_gen_stockpiling(78L, .p2d_id0 + 1000L, seed + 2L),
  .p2d_gen_early(116L, .p2d_id0 + 2000L, seed + 3L, k_range = 300:380),
  .p2d_gen_single(78L, .p2d_id0 + 3000L, seed + 4L),
  .p2d_gen_irregular(78L, .p2d_id0 + 4000L, seed + 5L),
  .p2d_gen_same_day(62L, .p2d_id0 + 5000L, seed + 6L),
  .p2d_gen_zero_ddd(62L, .p2d_id0 + 6000L, seed + 7L),
  .p2d_gen_hosp(78L, .p2d_id0 + 7000L, seed + 8L),
  .p2d_gen_two_atc(78L, .p2d_id0 + 8000L, seed + 9L),
  .p2d_gen_unmatched(78L, .p2d_id0 + 9000L, seed + 10L)
)
pur <- rbindlist(lapply(parts, function(x) return(x$purchases)))
hosp <- rbindlist(lapply(parts, function(x) return(x$hosp)))

# Trim to exactly 110,000 purchases: drop the last purchases of the regular
# users, one per person in turn, so each keeps a long history.
excess <- nrow(pur) - n_purchases
stopifnot(excess >= 0L)
pur[, row := .I]
reg <- pur[id <= .p2d_id0 + 1000L]
reg[, from_end := rev(seq_len(.N)), by = id]
setorder(reg, from_end, id)
pur <- pur[!row %in% reg$row[seq_len(excess)]][, row := NULL]
setorder(pur, id, atc, date)
fx <- .p2d_fixture(pur, hosp)

stopifnot(uniqueN(fx$purchases$id) == n_persons, nrow(fx$purchases) == n_purchases)
cat(sprintf(
  "Population: %d persons, %d purchases, %d hospital stays.\n",
  uniqueN(fx$purchases$id),
  nrow(fx$purchases),
  nrow(fx$hosp)
))

time_it <- function(f) {
  return(unname(system.time(out <- f())["elapsed"]))
}
run_port <- function() {
  return(.pre2dup_periods(
    fx$purchases,
    fx$packages,
    pre2dup_atc_parameters,
    hosp = fx$hosp
  ))
}
run_reference <- function() {
  return(.p2d_reference(fx))
}

t_port <- numeric(3L)
t_ref <- numeric(3L)
for (i in 1:3) {
  t_port[i] <- time_it(run_port)
  t_ref[i] <- time_it(run_reference)
}
port <- run_port()
ref <- run_reference()

cat(sprintf("Periods: port %d, PRE2DUP-R %d.\n", nrow(port), nrow(ref)))
cat(sprintf(
  "Port, .pre2dup_periods(): runs %s s, median %.2f s.\n",
  paste(sprintf("%.2f", t_port), collapse = ", "),
  stats::median(t_port)
))
cat(sprintf(
  "PRE2DUP-R, PRE2DUPR::pre2dup(): runs %s s, median %.2f s.\n",
  paste(sprintf("%.2f", t_ref), collapse = ", "),
  stats::median(t_ref)
))
cat(sprintf(
  "Ratio of medians, PRE2DUP-R / port: %.1f.\n",
  stats::median(t_ref) / stats::median(t_port)
))
cat(sprintf(
  "R %s, PRE2DUPR %s, swereg %s, data.table %s.\n",
  getRversion(),
  utils::packageVersion("PRE2DUPR"),
  utils::packageVersion("swereg"),
  utils::packageVersion("data.table")
))
