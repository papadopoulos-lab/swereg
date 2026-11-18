# The purchase trace of .pre2dup_periods(trace = TRUE). No test here needs
# PRE2DUPR. The expected joined_by values are what the port's steps wrote on
# these hand-built purchases.

# One person and one package. Each purchase holds the package's DDDs.
trace_fixture <- function(days, atc, package, hosp_days = NULL) {
  d0 <- as.Date("2020-01-01")
  purchases <- data.table::data.table(
    id = 1L,
    atc = atc,
    package_id = 1L,
    date = d0 + days,
    ratio = 1,
    ddd = package$pack_ddd
  )
  packages <- data.table::data.table(
    package_id = 1L,
    atc = atc,
    dur_min = package$dur_min,
    dur_usual = package$dur_usual,
    dur_max = package$dur_max,
    ddd_low = package$pack_ddd / package$dur_max,
    ddd_usual = package$pack_ddd / package$dur_usual
  )
  hosp <- NULL
  if (!is.null(hosp_days)) {
    hosp <- data.table::data.table(
      id = 1L,
      admission = d0 + hosp_days[1L],
      discharge = d0 + hosp_days[2L]
    )
  }
  return(list(purchases = purchases, packages = packages, hosp = hosp))
}

run_trace <- function(fx, trace = TRUE) {
  return(.pre2dup_periods(
    fx$purchases,
    fx$packages,
    pre2dup_atc_parameters,
    hosp = fx$hosp,
    trace = trace
  ))
}

# The example of vignette("rxpre2dup"): 11 purchases of a 30-DDD package and
# one hospital stay. Block 1 holds periods 1 to 3 and block 2 holds period 4.
fx_example <- trace_fixture(
  days = c(0, 65, 95, 180, 219, 300, 400, 495, 547, 860, 883),
  atc = "C10AA01",
  package = list(pack_ddd = 30, dur_min = 20, dur_usual = 30, dur_max = 60),
  hosp_days = c(453, 493)
)

# 46 refills 30 days apart, then a purchase 48 days later and two more. The
# gap after the long period is under 1% of its length, so post-processing
# joins the two periods.
fx_merge <- trace_fixture(
  days = c((0:45) * 30, 1398 + c(0, 30, 60)),
  atc = "C10AA05",
  package = list(pack_ddd = 30, dur_min = 20, dur_usual = 30, dur_max = 36)
)

# The trace's periods, with their first date and their count of purchases.
trace_periods <- function(out) {
  tr <- attr(out, "purchases")
  return(tr[,
    list(start = min(date), n_purchases = .N),
    by = c("id", "atc", "block", "period")
  ])
}

test_that("the trace names the step that joined each purchase", {
  out <- run_trace(fx_example)
  tr <- attr(out, "purchases")
  expect_identical(
    names(tr),
    c("id", "atc", "date", "block", "period", "duration", "joined_by")
  )
  expect_identical(
    tr$joined_by,
    c(
      "first",
      "extended",
      "duration",
      "stockpiling",
      "duration",
      "first",
      "first",
      "hospital",
      "duration",
      "first",
      "duration"
    )
  )
  expect_identical(tr$period, c(1L, 1L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 4L, 4L))
  expect_identical(out$n_purchases, c(5L, 1L, 3L, 2L))
  expect_identical(out$block, c(1L, 1L, 1L, 2L))
})

test_that("the trace marks a join by post-processing as merge", {
  out <- run_trace(fx_merge)
  tr <- attr(out, "purchases")
  expect_identical(
    tr$joined_by,
    c("first", rep("duration", 45L), "merge", "duration", "duration")
  )
  expect_identical(tr$period, rep(1L, 49L))
  expect_identical(nrow(out), 1L)
})

test_that("the periods of the trace are the periods of the output", {
  for (fx in list(fx_example, fx_merge)) {
    out <- run_trace(fx)
    tp <- trace_periods(out)
    expect_identical(tp$period, out$period)
    expect_identical(tp$block, out$block)
    expect_identical(tp$start, out$start)
    expect_identical(tp$n_purchases, out$n_purchases)
    expect_identical(
      sum(attr(out, "purchases")$joined_by == "first"),
      nrow(out)
    )
  }
})

test_that("trace = FALSE returns the same periods without the attribute", {
  for (fx in list(fx_example, fx_merge)) {
    with_trace <- run_trace(fx)
    without <- run_trace(fx, trace = FALSE)
    expect_null(attr(without, "purchases"))
    data.table::setattr(with_trace, "purchases", NULL)
    expect_identical(without, with_trace)
  }
})
