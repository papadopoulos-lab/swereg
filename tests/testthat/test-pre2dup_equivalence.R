# Equivalence of swereg's PRE2DUP port, .pre2dup_periods(), with PRE2DUP-R
# (PRE2DUPR 0.2.2, commit 43b6d89). Every scenario runs both on the same
# synthetic purchases, packages and stays. The expected values are always
# PRE2DUP-R's own output on that fixture.

# Packages: pack_ddd is the DDDs of one dispensing, as LMED's fddd.
.p2d_packages <- function() {
  k <- data.table::data.table(
    package_id = 1:6,
    atc = c("C10AA01", "C10AA01", "N06AB06", "N06AB06", "A10BA02", "C10AA05"),
    pack_ddd = c(30, 100, 28, 98, 50, 30),
    dur_min = c(20, 60, 14, 49, 25, 20),
    dur_usual = c(30, 100, 28, 98, 50, 30),
    dur_max = c(60, 200, 56, 196, 100, 36)
  )
  k[, ddd_usual := pack_ddd / dur_usual]
  k[, ddd_low := pack_ddd / dur_max]
  return(k[])
}

# Packages without a row in .p2d_packages(), one per ATC, for scenario 10.
.p2d_unmatched <- c(C10AA01 = 901L, N06AB06 = 903L)

# PRE2DUP-R re-sorts its rows by the factor of the person id after it took a
# row selection in the string order of paste(id, atc) (pre2dup.R:535 and
# :584), and its stockpiling step then reads the stale selection. The port
# fixes that defect. Ids of one width keep the two orders equal, so every
# fixture given to PRE2DUP-R uses ids from 100001 to 999999. Scenario 13
# gives the port ids of mixed widths.
.p2d_id0 <- 100000L

# One row per purchase. `day` counts from 2015-01-01.
.p2d_rows <- function(id, package_id, day, ratio = 1) {
  k <- .p2d_packages()
  atc <- k$atc[match(package_id, k$package_id)]
  ddd <- k$pack_ddd[match(package_id, k$package_id)] * ratio
  um <- match(package_id, .p2d_unmatched)
  atc[!is.na(um)] <- names(.p2d_unmatched)[um[!is.na(um)]]
  ddd[!is.na(um)] <- 30 * ratio[1L]
  return(data.table::data.table(
    id = as.integer(id),
    atc = atc,
    package_id = as.integer(package_id),
    date = as.Date("2015-01-01") + as.integer(day),
    ratio = as.numeric(ratio),
    ddd = as.numeric(ddd)
  ))
}

.p2d_stays <- function(
  id = integer(),
  admission = integer(),
  discharge = integer()
) {
  return(data.table::data.table(
    id = as.integer(id),
    admission = as.Date("2015-01-01") + as.integer(admission),
    discharge = as.Date("2015-01-01") + as.integer(discharge)
  ))
}

.p2d_fixture <- function(purchases, hosp = NULL) {
  return(list(purchases = purchases, packages = .p2d_packages(), hosp = hosp))
}

# The generators. Each takes the number of persons, the first id and a seed.
# A loop over persons builds the synthetic input; the port under test has no
# such loop.

# 1: regular users, one package each, refills at the usual duration.
.p2d_gen_regular <- function(n, id0, seed, k_range = 8:15) {
  set.seed(seed)
  k <- .p2d_packages()
  out <- lapply(seq_len(n), function(j) {
    pk <- sample(c(1L, 3L, 5L), 1L)
    m <- sample(k_range, 1L)
    start <- sample(0:1000, 1L)
    return(.p2d_rows(id0 + j, pk, start + (seq_len(m) - 1L) * k$dur_usual[pk]))
  })
  return(.p2d_fixture(data.table::rbindlist(out)))
}

# 2: stockpiling. Regular refills, an early refill, then a long gap. The
# early refill's ERFL does not reach the next purchase, and the DDDs of the
# early refill and the purchase before it cover both gaps.
.p2d_gen_stockpiling <- function(n, id0, seed) {
  set.seed(seed)
  out <- lapply(seq_len(n), function(j) {
    early <- 60L + sample(3:8, 1L)
    after <- early + sample(80:95, 1L)
    day <- c(0L, 30L, 60L, early, after, after + 30L, after + 60L, after + 90L)
    return(.p2d_rows(id0 + j, 1L, sample(0:900, 1L) + day))
  })
  return(.p2d_fixture(data.table::rbindlist(out)))
}

# 3: repeated early refills, with refill intervals and DDDs that vary inside
# blocks of three or more purchases. A pause of 90 to 250 days, under
# global_gap_max, splits the refills into two periods inside one block, so
# the end of the first period reads the sliding average of a purchase in the
# middle of its block.
.p2d_gen_early <- function(n, id0, seed, k_range = 6:12) {
  set.seed(seed)
  out <- lapply(seq_len(n), function(j) {
    pair <- if (stats::runif(1L) < 0.5) c(1L, 2L) else c(3L, 4L)
    m <- sample(k_range, 1L)
    pk <- sample(pair, m, replace = TRUE, prob = c(0.7, 0.3))
    dur <- .p2d_packages()$dur_usual[pk]
    gap <- round(dur * stats::runif(m, 0.55, 0.9))
    gap[sample.int(m - 5L, 1L) + 2L] <- sample(90:250, 1L)
    day <- sample(0:800, 1L) + cumsum(c(0, gap[-m]))
    return(.p2d_rows(id0 + j, pk, day))
  })
  return(.p2d_fixture(data.table::rbindlist(out)))
}

# 4: one purchase per person and ATC.
.p2d_gen_single <- function(n, id0, seed) {
  set.seed(seed)
  pk <- sample(1:5, n, replace = TRUE)
  ratio <- sample(c(1, 1, 1, 2), n, replace = TRUE)
  out <- lapply(seq_len(n), function(j) {
    return(.p2d_rows(id0 + j, pk[j], sample(0:1500, 1L), ratio = ratio[j]))
  })
  return(.p2d_fixture(data.table::rbindlist(out)))
}

# 5: irregular users: clusters of purchases with gaps over global_gap_max
# between them.
.p2d_gen_irregular <- function(n, id0, seed) {
  set.seed(seed)
  k <- .p2d_packages()
  out <- lapply(seq_len(n), function(j) {
    pk <- sample(c(1L, 3L, 5L), 1L)
    day <- integer()
    at <- sample(0:300, 1L)
    for (cl in seq_len(sample(2:3, 1L))) {
      m <- sample(2:5, 1L)
      gap <- round(k$dur_usual[pk] * stats::runif(m, 0.8, 1.5))
      day <- c(day, at + cumsum(c(0, gap[-m])))
      at <- max(day) + sample(320:600, 1L)
    }
    return(.p2d_rows(id0 + j, pk, day))
  })
  return(.p2d_fixture(data.table::rbindlist(out)))
}

# 6: several packages on the same day: two packages, the same package twice,
# and three dispensings with ratios 1 and 2.
.p2d_gen_same_day <- function(n, id0, seed) {
  set.seed(seed)
  out <- lapply(seq_len(n), function(j) {
    pair <- if (j %% 2L == 0L) c(1L, 2L) else c(3L, 4L)
    m <- sample(6:10, 1L)
    day <- sample(0:600, 1L) + cumsum(c(0, sample(25:60, m - 1L, TRUE)))
    rows <- list(.p2d_rows(id0 + j, pair[1L], day))
    pick <- sample(seq_len(m), 3L)
    rows[[2L]] <- .p2d_rows(id0 + j, pair[2L], day[pick[1L]])
    rows[[3L]] <- .p2d_rows(id0 + j, pair[1L], day[pick[2L]])
    rows[[4L]] <- .p2d_rows(id0 + j, pair[1L], day[pick[3L]], ratio = 2)
    rows[[5L]] <- .p2d_rows(id0 + j, pair[2L], day[pick[3L]])
    return(data.table::rbindlist(rows))
  })
  return(.p2d_fixture(data.table::rbindlist(out)))
}

# 7: purchases with ddd = 0, one in twelve per person, so PRE2DUP-R's 90%
# DDD coverage rule holds.
.p2d_gen_zero_ddd <- function(n, id0, seed) {
  set.seed(seed)
  out <- lapply(seq_len(n), function(j) {
    pk <- sample(c(1L, 3L), 1L)
    gap <- .p2d_packages()$dur_usual[pk]
    day <- sample(0:700, 1L) +
      cumsum(c(0, round(gap * stats::runif(11L, 0.7, 1.2))))
    x <- .p2d_rows(id0 + j, pk, day)
    x[sample(2:11, 1L), ddd := 0]
    return(x)
  })
  return(.p2d_fixture(data.table::rbindlist(out)))
}

# 8: hospital stays during a period, in a gap between purchases, longer
# than global_hosp_max, and overlapping or touching each other.
.p2d_gen_hosp <- function(n, id0, seed) {
  set.seed(seed)
  k <- .p2d_packages()
  pur <- list()
  hosp <- list()
  for (j in seq_len(n)) {
    pk <- sample(c(1L, 3L), 1L)
    usual <- k$dur_usual[pk]
    start <- sample(0:500, 1L)
    stay_len <- sample(c(10L, 20L, 45L, 60L), 1L)
    gap <- c(rep(usual, 4L), usual + stay_len, rep(usual, 3L))
    day <- start + cumsum(c(0, gap))
    pur[[j]] <- .p2d_rows(id0 + j, pk, day)
    in_gap <- day[5L] + sample(2:8, 1L)
    a <- c(day[2L] + 3L, in_gap, in_gap + stay_len - 3L, day[8L] + 5L)
    d <- c(
      day[2L] + 10L,
      in_gap + stay_len - 2L,
      in_gap + stay_len + 4L,
      day[8L] + 50L
    )
    if (j %% 3L == 0L) {
      a <- c(a, d[1L])
      d <- c(d, d[1L] + 4L)
    }
    if (j %% 4L == 0L) {
      a <- c(a, max(day) + 200L)
      d <- c(d, max(day) + 260L)
    }
    hosp[[j]] <- .p2d_stays(id0 + j, a, d)
  }
  return(.p2d_fixture(data.table::rbindlist(pur), data.table::rbindlist(hosp)))
}

# 9: a person with two ATCs.
.p2d_gen_two_atc <- function(n, id0, seed) {
  set.seed(seed)
  out <- lapply(seq_len(n), function(j) {
    a <- sample(0:300, 1L) + cumsum(c(0, sample(25:40, sample(5:9, 1L), TRUE)))
    b <- sample(0:300, 1L) + cumsum(c(0, sample(20:35, sample(5:9, 1L), TRUE)))
    return(data.table::rbindlist(list(
      .p2d_rows(id0 + j, 1L, a),
      .p2d_rows(id0 + j, 3L, b)
    )))
  })
  return(.p2d_fixture(data.table::rbindlist(out)))
}

# 10: purchases whose package has no row, one in fifteen per person, so they
# use the ATC-level rule and PRE2DUP-R's 90% coverage rule holds.
.p2d_gen_unmatched <- function(n, id0, seed) {
  set.seed(seed)
  out <- lapply(seq_len(n), function(j) {
    pk <- sample(c(1L, 3L), 1L)
    m <- 15L
    pkv <- rep(pk, m)
    pkv[sample(seq_len(m), 1L)] <- .p2d_unmatched[[
      if (pk == 1L) "C10AA01" else "N06AB06"
    ]]
    gap <- .p2d_packages()$dur_usual[pk]
    day <- sample(0:600, 1L) +
      cumsum(c(0, round(gap * stats::runif(m - 1L, 0.8, 1.1))))
    return(.p2d_rows(id0 + j, pkv, day))
  })
  return(.p2d_fixture(data.table::rbindlist(out)))
}

# 12: a long period of regular refills of package 6, then a purchase 48 or 49
# days later that starts a new period. Its gap from the end of the long
# period is under 1% of that period's length, so post-processing joins them.
.p2d_gen_merge <- function(n, id0, seed) {
  set.seed(seed)
  out <- lapply(seq_len(n), function(j) {
    m <- sample(46:50, 1L)
    day <- (seq_len(m) - 1L) * 30L
    brk <- max(day) + sample(48:49, 1L)
    day <- c(day, brk + c(0L, 30L, 60L))
    return(.p2d_rows(id0 + j, 6L, sample(0:100, 1L) + day))
  })
  return(.p2d_fixture(data.table::rbindlist(out)))
}

# 11: a mixed population drawn from the generators of scenarios 1 to 10.
.p2d_gen_mixed <- function(seed) {
  set.seed(seed)
  parts <- list(
    .p2d_gen_regular(250L, .p2d_id0 + 0L, seed + 1L, k_range = 30:50),
    .p2d_gen_stockpiling(100L, .p2d_id0 + 1000L, seed + 2L),
    .p2d_gen_early(150L, .p2d_id0 + 2000L, seed + 3L, k_range = 25:40),
    .p2d_gen_single(100L, .p2d_id0 + 3000L, seed + 4L),
    .p2d_gen_irregular(100L, .p2d_id0 + 4000L, seed + 5L),
    .p2d_gen_same_day(80L, .p2d_id0 + 5000L, seed + 6L),
    .p2d_gen_zero_ddd(80L, .p2d_id0 + 6000L, seed + 7L),
    .p2d_gen_hosp(100L, .p2d_id0 + 7000L, seed + 8L),
    .p2d_gen_two_atc(100L, .p2d_id0 + 8000L, seed + 9L),
    .p2d_gen_unmatched(100L, .p2d_id0 + 9000L, seed + 10L)
  )
  pur <- data.table::rbindlist(lapply(parts, function(x) return(x$purchases)))
  hosp <- data.table::rbindlist(lapply(parts, function(x) return(x$hosp)))
  return(.p2d_fixture(pur, hosp))
}

# The share per ATC of the rows that `ok` marks, as a named vector.
.p2d_share <- function(atc, ok) {
  return(tapply(ok, atc, mean))
}

# The conditions under which PRE2DUP-R accepts the fixture, so the
# reference never refuses one.
.p2d_expect_reference_accepts <- function(fx) {
  p <- fx$purchases
  ids <- unique(p$id)
  expect_true(all(ids > .p2d_id0 & ids <= 999999L))
  opt <- pre2dup_atc_parameters$partial_atc
  prefix_ok <- vapply(
    unique(p$atc),
    function(a) return(any(startsWith(a, opt))),
    logical(1L)
  )
  expect_true(all(prefix_ok))
  # PRE2DUP-R measures the package coverage after its same-day aggregation.
  agg <- unique(p, by = c("id", "atc", "date", "package_id"))
  expect_true(all(
    .p2d_share(agg$atc, agg$package_id %in% fx$packages$package_id) >= 0.9
  ))
  # PRE2DUP-R refuses ddd = 0, and reads a missing ddd as 0. The reference
  # gets NA where the port gets 0 (.p2d_reference_purchases()), and at most
  # 10% per ATC may be missing.
  expect_true(all(.p2d_share(p$atc, p$ddd > 0) >= 0.9))
  return(invisible(NULL))
}

# The purchases as PRE2DUP-R gets them. These are corresponding inputs:
# PRE2DUP-R refuses ddd = 0 (check_purchases.R:154 accepts NA but not 0) and
# turns NA into 0 (pre2dup.R:300, pre_data[is.na(ddd_pre), ddd_pre := 0]), so
# the reference gets ddd = NA where the port gets ddd = 0.
.p2d_reference_purchases <- function(fx) {
  pre <- data.table::copy(fx$purchases)
  pre[ddd == 0, ddd := NA_real_]
  return(pre[])
}

.p2d_reference <- function(fx, ...) {
  pre <- .p2d_reference_purchases(fx)
  hosp <- fx$hosp
  if (!is.null(hosp) && nrow(hosp) == 0L) {
    hosp <- NULL
  }
  ref <- suppressMessages(PRE2DUPR::pre2dup(
    pre_data = pre,
    pre_person_id = "id",
    pre_atc = "atc",
    pre_package_id = "package_id",
    pre_date = "date",
    pre_ratio = "ratio",
    pre_ddd = "ddd",
    package_parameters = fx$packages,
    pack_atc = "atc",
    pack_id = "package_id",
    pack_ddd_low = "ddd_low",
    pack_ddd_usual = "ddd_usual",
    pack_dur_min = "dur_min",
    pack_dur_usual = "dur_usual",
    pack_dur_max = "dur_max",
    atc_parameters = PRE2DUPR::ATC_parameters,
    atc_class = "partial_atc",
    atc_ddd_low = "lower_ddd_atc",
    atc_ddd_usual = "usual_ddd_atc",
    atc_dur_min = "minimum_dur_atc",
    atc_dur_max = "maximum_dur_atc",
    hosp_data = hosp,
    hosp_person_id = if (is.null(hosp)) NULL else "id",
    hosp_admission = if (is.null(hosp)) NULL else "admission",
    hosp_discharge = if (is.null(hosp)) NULL else "discharge",
    data_to_return = "periods",
    ...
  ))
  return(ref)
}

# PRE2DUP-R's dup_end holds a fraction of a day. Its Date prints the day
# that holds it, which is the floor.
.p2d_as_date <- function(x) {
  return(as.Date(floor(as.numeric(x))))
}

.p2d_normalise <- function(
  id,
  atc,
  start,
  end,
  n_purchases,
  hospital_days,
  total_ddd
) {
  x <- data.frame(
    id = as.integer(as.character(id)),
    atc = as.character(atc),
    start = .p2d_as_date(start),
    end = .p2d_as_date(end),
    n_purchases = n_purchases,
    hospital_days = hospital_days,
    total_ddd = total_ddd,
    stringsAsFactors = FALSE
  )
  x <- x[order(x$id, x$atc, x$start), , drop = FALSE]
  rownames(x) <- NULL
  return(x)
}

# Runs both implementations and compares them. Returns the number of periods.
.p2d_expect_equivalent <- function(fx, ...) {
  .p2d_expect_reference_accepts(fx)
  ref <- .p2d_reference(fx, ...)
  hosp <- fx$hosp
  if (!is.null(hosp) && nrow(hosp) == 0L) {
    hosp <- NULL
  }
  port <- .pre2dup_periods(
    fx$purchases,
    fx$packages,
    pre2dup_atc_parameters,
    hosp = hosp,
    ...
  )
  r <- .p2d_normalise(
    ref$id,
    ref$atc,
    ref$dup_start,
    ref$dup_end,
    ref$dup_n_purchases,
    ref$dup_hospital_days,
    ref$dup_total_DDD
  )
  s <- .p2d_normalise(
    port$id,
    port$atc,
    port$start,
    port$end,
    port$n_purchases,
    port$hospital_days,
    port$total_ddd
  )
  cols <- c("id", "atc", "start", "end", "n_purchases", "hospital_days")
  expect_identical(s[, cols], r[, cols])
  expect_equal(s$total_ddd, r$total_ddd, tolerance = 1e-8)
  return(invisible(nrow(r)))
}

test_that("scenario 1: regular users, refills at the usual duration", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_regular(40L, .p2d_id0, 101L)
  n <- .p2d_expect_equivalent(fx)
  expect_gt(n, 0L)
})

test_that("scenario 2: stockpiling joins a purchase inside a block", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_stockpiling(30L, .p2d_id0, 102L)
  .p2d_expect_equivalent(fx)
})

test_that("scenario 3: repeated early refills", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_early(40L, .p2d_id0, 103L)
  .p2d_expect_equivalent(fx)
})

test_that("scenario 4: single purchases", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_single(40L, .p2d_id0, 104L)
  n <- .p2d_expect_equivalent(fx)
  expect_identical(n, 40L)
})

test_that("scenario 5: irregular users with gaps over global_gap_max", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_irregular(40L, .p2d_id0, 105L)
  .p2d_expect_equivalent(fx)
})

test_that("scenario 6: several packages on the same day", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_same_day(30L, .p2d_id0, 106L)
  .p2d_expect_equivalent(fx)
})

test_that("scenario 7: purchases with ddd = 0, as corresponding inputs", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_zero_ddd(30L, .p2d_id0, 107L)
  expect_true(any(fx$purchases$ddd == 0))
  # Corresponding inputs: the port gets ddd = 0, and PRE2DUP-R gets ddd = NA
  # on exactly those rows. PRE2DUP-R refuses 0 and turns NA into 0
  # (pre2dup.R:300).
  ref_in <- .p2d_reference_purchases(fx)
  expect_identical(is.na(ref_in$ddd), fx$purchases$ddd == 0)
  expect_identical(
    ref_in$ddd[!is.na(ref_in$ddd)],
    fx$purchases$ddd[fx$purchases$ddd != 0]
  )
  .p2d_expect_equivalent(fx)
})

test_that("scenario 8: hospital stays during and between periods", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_hosp(30L, .p2d_id0, 108L)
  expect_true(any(fx$hosp$discharge - fx$hosp$admission > 30))
  .p2d_expect_equivalent(fx)
})

test_that("scenario 9: a person with two ATCs", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_two_atc(30L, .p2d_id0, 109L)
  .p2d_expect_equivalent(fx)
})

test_that("scenario 10: purchases whose package has no row", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_unmatched(30L, .p2d_id0, 110L)
  expect_true(any(!fx$purchases$package_id %in% fx$packages$package_id))
  .p2d_expect_equivalent(fx)
})

test_that("scenario 11: a mixed population of 1,000 persons or more", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_mixed(111L)
  expect_gte(data.table::uniqueN(fx$purchases$id), 1000L)
  expect_gte(nrow(fx$purchases), 20000L)
  .p2d_expect_equivalent(fx)
})

test_that("scenario 12: post-processing joins two close periods", {
  skip_if_not_installed("PRE2DUPR")
  fx <- .p2d_gen_merge(20L, .p2d_id0, 112L)
  .p2d_expect_equivalent(fx)
})

# The scenario 11 fixture with the ids replaced through `map`, a function
# from the old ids to the new ones.
.p2d_relabel <- function(fx, map) {
  out <- fx
  out$purchases <- data.table::copy(fx$purchases)[, id := map(id)]
  out$hosp <- data.table::copy(fx$hosp)[, id := map(id)]
  return(out)
}

test_that("scenario 13: relabelling the ids leaves the port's periods unchanged", {
  skip_if_not_installed("PRE2DUPR")
  # PRE2DUP-R is not compared here. On this fixture relabelled 1..N it
  # returned 1593 periods, against 1590 under the original ids and from the
  # port under every labelling (measured 2026-11-18, PRE2DUPR 0.2.2). That is
  # the stale selection that the port fixes.
  fx <- .p2d_gen_mixed(111L)
  ids <- sort(unique(fx$purchases$id))
  # Two order-preserving relabellings: 1..N, of 1 to 4 digits, and
  # 5 + 37 * rank, of 2 to 5 digits.
  maps <- list(
    rank = function(x) return(match(x, ids)),
    spread = function(x) return(5L + 37L * match(x, ids))
  )
  run <- function(f) {
    out <- .pre2dup_periods(
      f$purchases,
      f$packages,
      pre2dup_atc_parameters,
      hosp = f$hosp
    )
    return(out)
  }
  base <- run(fx)
  expect_gt(nrow(base), 0L)
  for (m in names(maps)) {
    new_ids <- maps[[m]](ids)
    expect_gt(length(unique(nchar(new_ids))), 1L)
    got <- run(.p2d_relabel(fx, maps[[m]]))
    got[, id := ids[match(id, new_ids)]]
    expect_identical(as.data.frame(got), as.data.frame(base), label = m)
  }
})
