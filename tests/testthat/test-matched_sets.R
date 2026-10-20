# RegistryStudy$set_matched_sets(): a roster of matched sets, packed whole into
# batches, with one copy of a person's rawbatch rows per membership.
#
# The invariant: the code columns and the one-time columns of each skeleton_id
# equal those that $process_skeletons() gives its lopnr in a one-person study,
# and the roster columns of each skeleton_id equal the roster's own values.

# ---- Persons ---------------------------------------------------------------

# Every person is a copy of one of 10 archetypes. Archetypes 1 to 8 have one
# outpatient row each, with a code and an ISO week no other archetype has.
# Archetype 9 has two rows. Archetype 10 has none.
.ms_codes <- sprintf("X%02d", 1:9)
.ms_code_cols <- paste0("ov_", tolower(.ms_codes))
.ms_onetime_cols <- c("sex", "birth_year")
.ms_compare_cols <- c(.ms_code_cols, .ms_onetime_cols)
.ms_filler_archetype <- 8L

.ms_archetype_rows <- data.table::data.table(
  archetype = c(1:8, 9L, 9L),
  hdia = c(paste0(.ms_codes[1:8], "0"), "X090", "X091"),
  indatum = as.Date("2020-01-06") + 7L * c(0:7, 1L, 7L)
)

# The persons and outpatient tables of people with these lopnr and archetypes.
# `raw_id` is the lopnr. It is not the id column, so the re-keying leaves it
# alone and the skeleton shows which person a skeleton_id belongs to.
.ms_raw <- function(lopnr, archetype) {
  persons <- data.table::data.table(
    lopnr = lopnr,
    sex = c("f", "m")[archetype %% 2L + 1L],
    birth_year = 1980L + archetype,
    raw_id = lopnr
  )
  who <- data.table::data.table(lopnr = lopnr, archetype = archetype)
  outpatient <- .ms_archetype_rows[
    who,
    on = "archetype",
    nomatch = NULL,
    allow.cartesian = TRUE
  ][, list(lopnr, indatum, hdia)]
  return(list(persons = persons, outpatient = outpatient))
}

# ---- The pipeline ----------------------------------------------------------

# The base is the 10 weekly rows of 2020 per person. The 120 annual rows
# before 2020 hold no event of these tests, so the base leaves them out.
.ms_framework <- function(batch_data, config) {
  skeleton <- swereg::create_skeleton(
    ids = batch_data[["persons"]]$lopnr,
    isoyear_min = 2020,
    isoyearweek_max = "2020-10"
  )
  return(skeleton[is_isoyear == FALSE])
}

# The same base, plus the process id of the process that built it.
.ms_framework_pid <- function(batch_data, config) {
  skeleton <- swereg::create_skeleton(
    ids = batch_data[["persons"]]$lopnr,
    isoyear_min = 2020,
    isoyearweek_max = "2020-10"
  )
  skeleton <- skeleton[is_isoyear == FALSE]
  skeleton[, worker_pid := Sys.getpid()]
  return(skeleton)
}

.ms_onetime_step <- function(skeleton, batch_data, config) {
  skeleton <- swereg::add_onetime(skeleton, batch_data[["persons"]], "lopnr")
  return(skeleton)
}

# The roster columns. A study with no roster gets none.
.ms_roster_step <- function(skeleton, batch_data, config) {
  roster <- batch_data[["roster"]]
  if (is.null(roster)) {
    return(skeleton)
  }
  skeleton[
    roster,
    on = c(id = "skeleton_id"),
    `:=`(set_id = i.set_id, role = i.role, roster_lopnr = i.lopnr)
  ]
  return(skeleton)
}

.ms_study <- function(dir, framework = .ms_framework) {
  study <- RegistryStudy$new(
    data_rawbatch_dir = dir,
    group_names = c("persons", "outpatient")
  )
  study$register_framework(framework)
  study$register_codes(
    codes = stats::setNames(as.list(.ms_codes), tolower(.ms_codes)),
    fn = swereg::add_diagnoses,
    groups = list(ov = "outpatient")
  )
  study$register_randvars("onetime", .ms_onetime_step)
  study$register_randvars("roster", .ms_roster_step)
  return(study)
}

.ms_save_rawbatch <- function(study, raw, n_workers = 1L) {
  invisible(utils::capture.output({
    study$save_rawbatch("persons", raw$persons, n_workers = n_workers)
    study$save_rawbatch("outpatient", raw$outpatient, n_workers = n_workers)
  }))
  return(invisible(study))
}

.ms_process <- function(study, n_workers = 1L) {
  invisible(utils::capture.output(study$process_skeletons(
    n_workers = n_workers
  )))
  return(invisible(study))
}

# Every skeleton of a study, with its batch number.
.ms_skeletons <- function(study) {
  return(data.table::rbindlist(lapply(seq_len(study$n_batches), function(b) {
    d <- data.table::copy(study$load_skeleton(b)$data)
    d[, batch := b]
    return(d)
  }), fill = TRUE))
}

# ---- The ground truth: one one-person study per archetype ------------------

# Archetype a is lopnr a in a study that holds that person alone, with no
# roster. A person of archetype a has the same rawbatch rows except the lopnr.
.ms_reference <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) {
      return(cached)
    }
    cached <<- data.table::rbindlist(lapply(1:10, function(a) {
      dir <- withr::local_tempdir()
      study <- .ms_study(dir)
      study$set_batches(list(a))
      .ms_save_rawbatch(study, .ms_raw(a, a))
      if (a == 10L) {
        # Archetype 10 has no outpatient row, and add_diagnoses() warns on
        # the empty table.
        suppressWarnings(.ms_process(study))
      } else {
        .ms_process(study)
      }
      d <- data.table::copy(study$load_skeleton(1L)$data)
      stopifnot(identical(unique(d$id), a), identical(unique(d$raw_id), a))
      d[, `:=`(archetype = a, .r = seq_len(.N))]
      return(d[, c("archetype", ".r", "isoyearweek", .ms_compare_cols),
        with = FALSE
      ])
    }))
    return(cached)
  }
})

# ---- Rosters ---------------------------------------------------------------

# Every assignment of `m` controls to `k` cases, each control in at least one
# set. Case i is lopnr i, set i and skeleton_id i. Control j is lopnr 3 + j,
# and its membership in set i has skeleton_id 10 * (3 + j) + i. Returns one
# long table with a roster number `r`.
.ms_exhaustive_rosters <- function(cases = 1:3, controls = 1:4) {
  out <- list()
  for (k in cases) {
    for (m in controls) {
      grid <- as.matrix(expand.grid(rep(list(seq_len(2^k - 1)), m)))
      for (g in seq_len(nrow(grid))) {
        rows <- list()
        for (i in seq_len(k)) {
          in_set <- which(bitwAnd(grid[g, ], 2L^(i - 1L)) > 0L)
          rows[[i]] <- data.table::data.table(
            lopnr = c(i, 3L + in_set),
            set_id = i,
            role = c("case", rep("control", length(in_set))),
            skeleton_id = c(i, 10L * (3L + in_set) + i),
            archetype = c(i, 3L + in_set)
          )
        }
        out[[length(out) + 1L]] <- data.table::rbindlist(rows)
      }
    }
  }
  return(data.table::rbindlist(out, idcol = "r"))
}

# Put many rosters into one study, each with its own sets_per_batch, so that
# every roster keeps the batch boundaries it would have alone.
#
# Roster r alone puts its sets into chunks of spb[r], in order of first
# appearance. Batch j of the combined study holds chunk j of every roster,
# padded to S = max(spb) sets with filler sets of one person each. A roster
# with fewer chunks gets chunks of filler sets only. So the combined study has
# sets_per_batch = S * R, and set_matched_sets() puts chunk j of roster r into
# batch j.
#
# The ids become global: lopnr r * 10000 + lopnr, skeleton_id r * 1e5 +
# skeleton_id, set_id "r:set_id". A filler person is lopnr 5000 + k and
# skeleton_id 90000 + k of its roster. Returns the combined roster, with the
# batch each membership MUST land in.
.ms_interleave <- function(x, spb) {
  x <- data.table::copy(x)
  x[, row := seq_len(.N)]
  x[, set_rank := match(set_id, unique(set_id)), by = r]
  x[, chunk := as.integer(ceiling(set_rank / spb[r]))]
  n_rosters <- length(spb)
  big_s <- max(spb)
  n_chunks <- max(x$chunk)

  real <- unique(x[, list(r, chunk, set_id)])[, list(n_real = .N), by = list(r, chunk)]
  slots <- data.table::CJ(r = seq_len(n_rosters), chunk = seq_len(n_chunks))
  slots <- real[slots, on = c("r", "chunk")]
  slots[is.na(n_real), n_real := 0L]
  slots[, n_filler := big_s - n_real]
  filler <- slots[rep(seq_len(.N), n_filler), list(r, chunk)]
  filler[, k := seq_len(.N), by = r]
  stopifnot(max(x$lopnr) < 5000L, max(x$skeleton_id) < 90000L)
  stopifnot(nrow(filler) == 0L || max(filler$k) < 5000L)
  filler[, `:=`(
    lopnr = 5000L + k,
    set_id = paste0("filler", k),
    role = "filler",
    skeleton_id = 90000L + k,
    archetype = .ms_filler_archetype,
    row = nrow(x) + seq_len(.N)
  )]

  x[, set_id := as.character(set_id)]
  out <- data.table::rbindlist(
    list(
      x[, list(r, chunk, row, lopnr, set_id, role, skeleton_id, archetype)],
      filler[, list(r, chunk, row, lopnr, set_id, role, skeleton_id, archetype)]
    )
  )
  data.table::setorder(out, chunk, r, row)
  out[, `:=`(
    lopnr = r * 10000L + lopnr,
    skeleton_id = r * 1e5 + skeleton_id,
    set_id = paste0(r, ":", set_id)
  )]
  out[, n_memberships := .N, by = lopnr]
  return(list(
    roster = out,
    sets_per_batch = big_s * n_rosters,
    n_batches = n_chunks
  ))
}

# Build, save and process a combined study. Returns the study.
.ms_run_combined <- function(combined, n_workers = 1L, framework = .ms_framework) {
  study <- .ms_study(withr::local_tempdir(.local_envir = parent.frame()), framework)
  roster <- combined$roster
  study$set_matched_sets(
    roster[, list(lopnr, set_id, role, skeleton_id)],
    sets_per_batch = combined$sets_per_batch
  )
  people <- unique(roster[, list(lopnr, archetype)])
  .ms_save_rawbatch(study, .ms_raw(people$lopnr, people$archetype), n_workers)
  .ms_process(study, n_workers)
  return(study)
}

# Assert the invariant for every membership of a combined study.
.ms_expect_invariant <- function(study, combined, label) {
  roster <- combined$roster
  expect_identical(study$n_batches, combined$n_batches, info = label)
  got <- .ms_skeletons(study)

  # Each membership has its rows in the batch its chunk names, and no other
  # id is in any batch.
  want_keys <- roster[, list(batch = chunk, id = skeleton_id)]
  got_keys <- unique(got[, list(batch, id = as.numeric(id))])
  data.table::setorder(want_keys, batch, id)
  data.table::setorder(got_keys, batch, id)
  expect_identical(got_keys, want_keys, info = label)

  # The roster columns, and the person each skeleton_id belongs to.
  got_roster <- unique(got[, list(
    batch,
    id = as.numeric(id),
    set_id,
    role,
    roster_lopnr,
    raw_id
  )])
  check <- got_roster[roster, on = c(batch = "chunk", id = "skeleton_id")]
  bad_roster <- check[
    is.na(set_id) | set_id != i.set_id | role != i.role |
      roster_lopnr != lopnr | raw_id != lopnr
  ]
  expect(
    nrow(bad_roster) == 0L && nrow(got_roster) == nrow(roster),
    paste0(
      label, ": ", nrow(bad_roster), " memberships have wrong roster columns\n",
      paste(utils::capture.output(print(utils::head(bad_roster, 10L))), collapse = "\n")
    )
  )

  # The code and one-time columns, row by row, against the one-person study
  # of the archetype.
  got[, .r := seq_len(.N), by = list(batch, id)]
  got[, id := as.numeric(id)]
  got <- roster[, list(batch = chunk, id = skeleton_id, archetype)][
    got,
    on = c("batch", "id")
  ]
  ref <- .ms_reference()
  joined <- ref[got, on = c("archetype", ".r")]
  # A row differs when a value differs, or when one side is NA and the other
  # is not. A column of another type differs on every row.
  same <- function(a, b) {
    if (!identical(class(a), class(b))) {
      return(rep(FALSE, length(a)))
    }
    return((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
  }
  row_ok <- same(joined$isoyearweek, joined$i.isoyearweek)
  for (col in .ms_compare_cols) {
    row_ok <- row_ok & same(joined[[col]], joined[[paste0("i.", col)]])
  }
  joined[, row_ok := row_ok]
  n_ref <- ref[, list(n_ref = .N), by = archetype]
  per_id <- joined[, list(ok = all(row_ok), n = .N, archetype = archetype[1L]),
    by = list(batch, id)
  ]
  per_id <- n_ref[per_id, on = "archetype"]
  differs <- per_id[!ok | is.na(n_ref) | n != n_ref]
  bad <- roster[differs, on = c(chunk = "batch", skeleton_id = "id"), nomatch = NULL]
  expect(
    nrow(differs) == 0L,
    paste0(
      label, ": ", nrow(differs), " of ", nrow(roster),
      " memberships differ from the one-person study of their person\n",
      paste(
        utils::capture.output(print(utils::head(
          bad[, list(chunk, skeleton_id, lopnr, set_id, role, n_memberships, archetype)],
          10L
        ))),
        collapse = "\n"
      )
    )
  )
  return(invisible(nrow(roster)))
}

# ---- Test 1: exhaustive ----------------------------------------------------

test_that("every membership of every roster of 1-3 cases and 1-4 controls matches a one-person study", {
  rosters <- .ms_exhaustive_rosters()
  # (2^k - 1)^m assignments for k cases and m controls.
  n_expected <- sum(outer(1:3, 1:4, function(k, m) (2^k - 1)^m))
  expect_identical(data.table::uniqueN(rosters$r), as.integer(n_expected))
  expect_identical(as.integer(n_expected), 2924L)
  n_rosters <- data.table::uniqueN(rosters$r)

  # A control in two sets is the case the invariant exists for.
  n_multi <- rosters[role == "control", list(n = .N), by = list(r, lopnr)][n > 1L, .N]
  expect_gt(n_multi, 0L)

  for (s in 1:3) {
    combined <- .ms_interleave(rosters, rep(s, n_rosters))
    study <- .ms_run_combined(combined)
    .ms_expect_invariant(study, combined, sprintf("sets_per_batch = %d", s))
  }
})

# ---- Test 2: random --------------------------------------------------------

# One random roster. Cases are lopnr 1 to n_sets. Some cases are siblings, and
# then share sibling controls. Some cases are controls in another set. A person
# with `shared` keeps one skeleton_id in every batch, so that skeleton_id
# repeats across batches. The rows come in random order.
.ms_random_roster <- function(spb) {
  n_sets <- sample(5:30, 1L)
  sets <- seq_len(n_sets)
  rows <- list(data.table::data.table(lopnr = sets, set_id = sets, role = "case"))
  next_id <- n_sets
  n_controls <- sample(n_sets:(2L * n_sets), 1L)
  for (j in seq_len(n_controls)) {
    next_id <- next_id + 1L
    in_sets <- sample(sets, sample(1:4, 1L, prob = c(0.4, 0.3, 0.2, 0.1)))
    rows[[length(rows) + 1L]] <- data.table::data.table(
      lopnr = next_id,
      set_id = in_sets,
      role = "control"
    )
  }
  family <- sample(seq_len(ceiling(n_sets / 2)), n_sets, replace = TRUE)
  for (f in unique(family)) {
    if (sum(family == f) > 1L && stats::runif(1L) < 0.7) {
      next_id <- next_id + 1L
      rows[[length(rows) + 1L]] <- data.table::data.table(
        lopnr = next_id,
        set_id = sets[family == f],
        role = "sibling"
      )
    }
  }
  for (i in sample(sets, 2L)) {
    rows[[length(rows) + 1L]] <- data.table::data.table(
      lopnr = i,
      set_id = sample(setdiff(sets, i), 1L),
      role = "control"
    )
  }
  x <- data.table::rbindlist(rows)
  x <- x[sample(.N)]
  x[, set_id := as.character(set_id)]

  x[, chunk := ceiling(match(set_id, unique(set_id)) / spb)]
  people <- unique(x$lopnr)
  shared <- people[stats::runif(length(people)) < 0.5]
  x[, k_in_batch := seq_len(.N), by = list(lopnr, chunk)]
  x[, k_all := seq_len(.N), by = lopnr]
  x[, skeleton_id := lopnr * 100L +
    data.table::fifelse(lopnr %in% shared, k_in_batch, 50L + k_all)]
  x[, archetype := sample(1:10, length(people), replace = TRUE)[match(lopnr, people)]]
  return(x[, list(lopnr, set_id, role, skeleton_id, archetype)])
}

test_that("every membership of 200 random rosters matches a one-person study", {
  withr::local_seed(1)
  spb <- sample(1:5, 200L, replace = TRUE)
  rosters <- data.table::rbindlist(
    lapply(spb, .ms_random_roster),
    idcol = "r"
  )
  expect_identical(data.table::uniqueN(rosters$r), 200L)
  combined <- .ms_interleave(rosters, spb)

  # The generator reaches each shape the test is for.
  real <- combined$roster[role != "filler"]
  per_lopnr <- real[, list(
    n_batches = data.table::uniqueN(chunk),
    n_ids = data.table::uniqueN(skeleton_id),
    n_in_one_batch = max(table(chunk))
  ), by = lopnr]
  expect_gt(per_lopnr[n_in_one_batch > 1L, .N], 0L)
  expect_gt(real[role == "sibling", .N], 0L)
  repeated <- real[, list(n = data.table::uniqueN(chunk)), by = skeleton_id][n > 1L]
  expect_gt(nrow(repeated), 0L)

  study <- .ms_run_combined(combined)
  .ms_expect_invariant(study, combined, "200 random rosters")
})

# ---- Test 3: degenerate rosters --------------------------------------------

test_that("a roster with every skeleton_id equal to its lopnr writes the rawbatch files of set_batches", {
  roster <- data.table::data.table(
    lopnr = 1:6,
    set_id = c("a", "a", "b", "c", "c", "c"),
    role = c("case", "control", "case", "case", "control", "control"),
    skeleton_id = 1:6
  )
  raw <- .ms_raw(1:6, c(1:5, 9L))
  write <- function(set) {
    dir <- withr::local_tempdir(.local_envir = parent.frame())
    study <- RegistryStudy$new(
      data_rawbatch_dir = dir,
      group_names = c("persons", "outpatient")
    )
    set(study)
    invisible(utils::capture.output({
      study$save_rawbatch("persons", data.table::copy(raw$persons))
      study$save_rawbatch(
        "outpatient",
        list(op = data.table::copy(raw$outpatient), lookup = data.frame(code = "X01"))
      )
    }))
    return(list(dir = dir, study = study))
  }
  by_roster <- write(function(s) s$set_matched_sets(roster, sets_per_batch = 2L))
  expect_identical(
    unname(by_roster$study$batch_id_list),
    list(1:3, 4:6)
  )
  by_batches <- write(function(s) s$set_batches(list(1:3, 4:6)))

  files <- sort(list.files(by_batches$dir, pattern = "_rawbatch_"))
  expect_length(files, 4L)
  expect_identical(
    sort(list.files(by_roster$dir, pattern = "_rawbatch_")),
    sort(c(files, sprintf("%05d_rawbatch_roster.qs2", 1:2)))
  )
  expect_identical(
    unname(tools::md5sum(file.path(by_roster$dir, files))),
    unname(tools::md5sum(file.path(by_batches$dir, files)))
  )
})

test_that("save_rawbatch writes a row once per skeleton_id of its lopnr, in input order, with the other columns unchanged", {
  # Lopnr 2 is a control in sets a and b, which share batch 1, and in set c,
  # which is batch 2. Lopnr 9 is in no set.
  roster <- data.table::data.table(
    lopnr = c(1L, 2L, 3L, 2L, 4L, 2L),
    set_id = c("a", "a", "b", "b", "c", "c"),
    role = c("case", "control", "case", "control", "case", "control"),
    skeleton_id = c(1, 2001, 3, 2003, 4, 2001)
  )
  op <- data.table::data.table(
    lopnr = c(2L, 9L, 1L, 2L, 4L),
    hdia = c("X010", "X020", "X030", "X040", "X050"),
    indatum = as.Date("2020-01-06") + 0:4,
    n = c(1L, 2L, 3L, 4L, 5L)
  )
  data.table::setkey(op, lopnr)
  op_before <- data.table::copy(op)
  lookup <- data.frame(code = "X01")

  dir <- withr::local_tempdir()
  study <- RegistryStudy$new(data_rawbatch_dir = dir, group_names = "outpatient")
  study$set_matched_sets(roster, sets_per_batch = 2L)
  invisible(utils::capture.output(
    study$save_rawbatch("outpatient", list(op = op, lookup = lookup))
  ))
  expect_identical(op, op_before)

  b1 <- study$load_rawbatch(1L)
  # op is keyed on lopnr, so its rows come as lopnr 1, 2, 2, 4, 9.
  expect_identical(
    as.data.frame(b1$op),
    data.frame(
      lopnr = c(1, 2001, 2003, 2001, 2003),
      hdia = c("X030", "X010", "X010", "X040", "X040"),
      indatum = as.Date("2020-01-06") + c(2L, 0L, 0L, 3L, 3L),
      n = c(3L, 1L, 1L, 4L, 4L)
    )
  )
  expect_null(data.table::key(b1$op))
  expect_identical(b1$lookup, lookup)
  expect_identical(b1$roster, roster[1:4])

  b2 <- study$load_rawbatch(2L)
  expect_identical(
    as.data.frame(b2$op),
    data.frame(
      lopnr = c(2001, 2001, 4),
      hdia = c("X010", "X040", "X050"),
      indatum = as.Date("2020-01-06") + c(0L, 3L, 4L),
      n = c(1L, 4L, 5L)
    )
  )
  expect_identical(b2$roster, roster[5:6])
  expect_identical(
    unname(study$batch_id_list),
    list(c(1, 2001, 3, 2003), c(4, 2001))
  )
})

# ---- Test 4: refusals ------------------------------------------------------

test_that("set_matched_sets stops on a skeleton_id repeated within a batch or mapped to two lopnr", {
  study <- RegistryStudy$new(
    data_rawbatch_dir = withr::local_tempdir(),
    group_names = "outpatient"
  )
  # Lopnr 3 is in sets a and b with one skeleton_id.
  roster <- data.table::data.table(
    lopnr = c(1L, 3L, 2L, 3L),
    set_id = c("a", "a", "b", "b"),
    role = c("case", "control", "case", "control"),
    skeleton_id = c(1L, 30L, 2L, 30L)
  )
  expect_error(
    study$set_matched_sets(roster, sets_per_batch = 2L),
    "skeleton_id 30 is in batch 1 more than once"
  )
  expect_identical(study$batch_id_list, list())
  expect_null(study$matched_set_roster)

  # One set per batch puts the two memberships into two batches.
  expect_no_error(study$set_matched_sets(roster, sets_per_batch = 1L))
  expect_identical(unname(study$batch_id_list), list(c(1L, 30L), c(2L, 30L)))

  two_lopnr <- data.table::copy(roster)
  two_lopnr[4L, lopnr := 4L]
  expect_error(
    study$set_matched_sets(two_lopnr, sets_per_batch = 1L),
    "skeleton_id 30 maps to more than one lopnr"
  )
  expect_identical(unname(study$batch_id_list), list(c(1L, 30L), c(2L, 30L)))

  expect_error(
    study$set_matched_sets(roster[, -"role"], 1L),
    "`roster` has no column role"
  )
  expect_error(study$set_matched_sets(roster, 0), "sets_per_batch")
  reserved <- RegistryStudy$new(
    data_rawbatch_dir = withr::local_tempdir(),
    group_names = c("outpatient", "roster")
  )
  expect_error(reserved$set_matched_sets(roster, 1L), "holds \"roster\"")
})

# ---- Test 5: whole sets ----------------------------------------------------

test_that("set_matched_sets never splits a set across batches", {
  study <- RegistryStudy$new(
    data_rawbatch_dir = withr::local_tempdir(),
    group_names = "outpatient"
  )
  # Each roster, with each sets_per_batch: every set is in one batch, and the
  # batches hold the sets in order of first appearance, sets_per_batch each.
  # Returns NULL, or why the packing is wrong. The generators give no
  # skeleton_id twice in one set, so a correct packing never stops.
  check_packing <- function(roster, s) {
    refused <- tryCatch(
      {
        study$set_matched_sets(roster, sets_per_batch = s)
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    if (!is.null(refused)) {
      return(paste("refused:", refused))
    }
    got <- data.table::rbindlist(study$matched_set_roster, idcol = "batch")
    split_sets <- got[, list(n = data.table::uniqueN(batch)), by = set_id][n > 1L]
    if (nrow(split_sets) > 0L) {
      return(paste("set split across batches:", split_sets$set_id[1L]))
    }
    sets <- unique(roster$set_id)
    want <- data.table::data.table(
      set_id = sets,
      batch = as.character(ceiling(seq_along(sets) / s))
    )
    ok <- identical(unique(got[, list(set_id, batch)]), want) &&
      nrow(got) == nrow(roster) &&
      identical(
        unname(study$batch_id_list),
        unname(lapply(study$matched_set_roster, function(d) d$skeleton_id))
      )
    if (!ok) {
      return("sets out of order, or the wrong number per batch")
    }
    return(NULL)
  }

  # The 441 rosters of 1-3 cases and 1-3 controls. Test 1 checks the batch of
  # every membership of the 2924 rosters with up to 4 controls.
  exhaustive <- .ms_exhaustive_rosters(controls = 1:3)
  expect_identical(data.table::uniqueN(exhaustive$r), 441L)
  by_roster <- split(exhaustive[, list(lopnr, set_id, role, skeleton_id)], exhaustive$r)
  failed <- character(0)
  for (s in 1:3) {
    for (r in names(by_roster)) {
      why <- check_packing(by_roster[[r]], s)
      if (!is.null(why)) {
        failed <- c(failed, sprintf("exhaustive roster %s, sets_per_batch %d: %s", r, s, why))
      }
    }
  }

  # Random rosters, with the rows of a set scattered through the roster.
  withr::local_seed(1)
  for (r in 1:200) {
    s <- sample(1:5, 1L)
    roster <- .ms_random_roster(s)[, list(lopnr, set_id, role, skeleton_id)]
    why <- check_packing(roster, s)
    if (!is.null(why)) {
      failed <- c(failed, sprintf("random roster %d, sets_per_batch %d: %s", r, s, why))
    }
  }
  expect(
    length(failed) == 0L,
    paste0(
      length(failed), " rosters are packed wrong:\n",
      paste(utils::head(failed, 10L), collapse = "\n")
    )
  )
})

# ---- Test 6: workers -------------------------------------------------------

test_that("with two workers, every membership of every roster of 2 cases and 3 controls matches a one-person study", {
  rosters <- .ms_exhaustive_rosters(cases = 2L, controls = 3L)
  expect_identical(data.table::uniqueN(rosters$r), 27L)
  # One combined roster per (roster, sets_per_batch) pair.
  pairs <- data.table::rbindlist(lapply(1:3, function(s) {
    return(data.table::copy(rosters)[, s := s])
  }))
  pairs[, r := (s - 1L) * 27L + r]
  combined <- .ms_interleave(pairs[, -"s"], rep(1:3, each = 27L))

  # A daemon of save_rawbatch() unserializes its slice of the persons table.
  # The slice carries the probe attribute of the table. mirai calls the
  # registered unserialize function on the probe, in the daemon, and that
  # function records the process id. A serial save_rawbatch() unserializes
  # nothing, so it records no process id.
  pid_dir <- withr::local_tempdir()
  # mirai::register_serial() cannot remove a registration. The test restores
  # the internal registry of mirai when it stops.
  mirai_state <- get(".", envir = asNamespace("mirai"))
  old_serial <- mirai_state[["serial"]]
  withr::defer(assign("serial", old_serial, envir = mirai_state))
  # The first call in a daemon waits 1 second. The dispatcher then gives the
  # next slice to the other daemon, so a fast daemon cannot take every slice.
  record_pid <- eval(bquote(function(raw) {
    f <- file.path(.(pid_dir), paste0("pid_", Sys.getpid()))
    if (!file.exists(f)) {
      writeLines("", f)
      Sys.sleep(1)
    }
    return(new.env())
  }))
  send_probe <- function(x) {
    return(serialize(NULL, NULL))
  }
  # mirai sends both functions to each daemon. A function defined here
  # encloses the test frame, which holds the probe, and save_rawbatch() hangs.
  environment(record_pid) <- baseenv()
  environment(send_probe) <- baseenv()
  mirai::register_serial("swereg_ms_pid_probe", send_probe, record_pid)

  dir <- withr::local_tempdir()
  study <- .ms_study(dir, framework = .ms_framework_pid)
  roster <- combined$roster
  study$set_matched_sets(
    roster[, list(lopnr, set_id, role, skeleton_id)],
    sets_per_batch = combined$sets_per_batch
  )
  people <- unique(roster[, list(lopnr, archetype)])
  raw <- .ms_raw(people$lopnr, people$archetype)
  data.table::setattr(
    raw$persons,
    "pid_probe",
    structure(new.env(), class = "swereg_ms_pid_probe")
  )
  .ms_save_rawbatch(study, raw, 2L)
  .ms_process(study, n_workers = 2L)

  rawbatch_pids <- as.integer(sub("^pid_", "", list.files(pid_dir)))
  expect_gte(length(rawbatch_pids), 2L)
  expect_false(Sys.getpid() %in% rawbatch_pids)
  skeleton_pids <- unique(.ms_skeletons(study)$worker_pid)
  expect_gte(length(skeleton_pids), 1L)
  expect_false(anyNA(skeleton_pids))
  expect_false(Sys.getpid() %in% skeleton_pids)

  .ms_expect_invariant(study, combined, "two workers")
})

# ---- Test 7: a changed roster ----------------------------------------------

test_that("a changed roster stops while the skeleton files stay", {
  # Lopnr 3 is a control in sets a and b. The changed roster gives skeleton_id
  # 32 to lopnr 4 and calls lopnr 3 a sibling, with the same batches.
  roster <- data.table::data.table(
    lopnr = c(1L, 3L, 2L, 3L),
    set_id = c("a", "a", "b", "b"),
    role = c("case", "control", "case", "control"),
    skeleton_id = c(1L, 31L, 2L, 32L)
  )
  changed <- data.table::copy(roster)
  changed[4L, `:=`(lopnr = 4L, role = "sibling")]
  raw <- .ms_raw(1:4, 1:4)

  dir <- withr::local_tempdir()
  study <- .ms_study(dir)
  study$set_matched_sets(roster, sets_per_batch = 1L)
  .ms_save_rawbatch(study, raw)
  .ms_process(study)
  skeletons <- sort(list.files(dir, pattern = "^skeleton_", full.names = TRUE))
  expect_length(skeletons, 2L)
  md5_before <- tools::md5sum(skeletons)
  batches_before <- study$batch_id_list

  # The changed roster has the same batch ids. Only the roster differs.
  probe <- RegistryStudy$new(withr::local_tempdir(), group_names = "outpatient")
  probe$set_matched_sets(changed, sets_per_batch = 1L)
  expect_identical(probe$batch_id_list, batches_before)

  invisible(utils::capture.output(study$delete_rawbatches()))
  expect_error(
    study$set_matched_sets(changed, sets_per_batch = 1L),
    "differs .* 0 rawbatch and 2 skeleton files"
  )

  # Rewrite the rawbatch files with the recorded roster. The changed roster
  # still stops.
  expect_no_error(study$set_matched_sets(roster, sets_per_batch = 1L))
  .ms_save_rawbatch(study, raw)
  expect_error(
    study$set_matched_sets(changed, sets_per_batch = 1L),
    "differs .* 6 rawbatch and 2 skeleton files"
  )

  # A new study that adopts the saved state stops too.
  invisible(utils::capture.output(study$save_meta()))
  fresh <- .ms_study(dir)
  fresh$adopt_runtime_state_from(qs2_read(study$meta_file))
  expect_identical(fresh$matched_set_roster, study$matched_set_roster)
  expect_error(
    fresh$set_matched_sets(changed, sets_per_batch = 1L),
    "differs .* 6 rawbatch and 2 skeleton files"
  )

  # Nothing ran on the skeletons, and a run with the recorded roster
  # rebuilds none of them.
  expect_identical(tools::md5sum(skeletons), md5_before)
  .ms_process(fresh)
  expect_identical(tools::md5sum(skeletons), md5_before)
})
