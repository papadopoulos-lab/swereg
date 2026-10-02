# `isoyear_range` limits one enrollment to trials registered in a range of ISO
# years. It writes `eligible_isoyears_<min>_<max>`, a column of its own, so the
# enrollment's `age_range` still applies. A second `age_range` would overwrite
# `eligible_age` and drop the age limit without an error.
#
# The range restricts enrollment only. A 4-week band that crosses the new year
# recruits from the weeks inside the range, and from no other week.

skip_if_not_installed("data.table")
skip_if_not_installed("yaml")
skip_if_not_installed("qs2")

# --- fixtures --------------------------------------------------------------

# One `isoyear_range` entry, in the shape `additional_inclusion` accepts.
.iyr_entry <- function(
  min = 2016L,
  max = 2017L,
  name = "Trial registration 2016-2017"
) {
  entry <- list(name = name, type = "isoyear_range", min = min, max = max)
  return(entry[!vapply(entry, is.null, logical(1))])
}

# One enrollment with an age range, followed by the entries in `extra`.
.iyr_enrollment <- function(
  id = "01",
  extra = list(),
  age_variable = "rd_age_continuous",
  age_min = 40,
  age_max = 80
) {
  return(list(
    id = id,
    name = paste("Enrollment", id),
    observed_var = list(sentinel = "row_presence"),
    intervention_tolerance_weeks = 0L,
    comparator_tolerance_weeks = 0L,
    additional_inclusion = c(
      list(list(
        name = "Age range",
        type = "age_range",
        min = age_min,
        max = age_max,
        implementation = list(variable = age_variable)
      )),
      extra
    ),
    treatment = list(
      description = "Initiation of systemic MHT.",
      arms = list(intervention = "Systemic", comparator = "Local or none"),
      implementation = list(
        comparator_to_intervention_ratio = 2L,
        variable = "rd_approach1_single",
        intervention_value = "systemic_mht",
        comparator_value = "local_or_none_mht",
        seed = 1L
      )
    )
  ))
}

# A minimal readable spec with the global ISO-year range 2015 to 2018.
.iyr_spec_list <- function(
  enrollments = list(.iyr_enrollment(extra = list(.iyr_entry()))),
  isoyears = c(2015L, 2018L)
) {
  return(list(
    study = list(
      title = "isoyear_range",
      implementation = list(project_prefix = "iyr", version = "v001")
    ),
    inclusion_criteria = list(isoyears = isoyears),
    enrollments = enrollments,
    outcomes = list(list(
      name = "Outcome A",
      implementation = list(variable = "osd_a")
    )),
    follow_up = list(list(label = "1 year", weeks = 52L))
  ))
}

.iyr_write <- function(spec, dir = tempfile("iyr_spec_")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "spec_v001.yaml")
  yaml::write_yaml(spec, path)
  return(path)
}

# Write a spec and read it back through the production reader.
.iyr_read <- function(spec) {
  path <- .iyr_write(spec)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)
  return(suppressMessages(swereg::tteplan_read_spec(path)))
}

# Every ISO week of the calendar from the first week of `from` to the last
# week of `to`.
.iyr_weeks <- function(from, to) {
  w <- cstime::dates_by_isoyearweek$isoyearweek
  y <- as.integer(substr(w, 1L, 4L))
  return(w[y >= from & y <= to])
}

# One row per person and week. `isoyear` is the ISO year of `isoyearweek`.
.iyr_skeleton <- function(weeks, ids = 1:2) {
  sk <- data.table::CJ(id = as.integer(ids), isoyearweek = weeks)
  sk[, isoyear := as.integer(substr(isoyearweek, 1L, 4L))]
  sk[, is_isoyear := FALSE]
  sk[, rd_age_continuous := 55]
  sk[, rd_approach1_single := "local_or_none_mht"]
  sk[, osd_a := FALSE]
  return(sk)
}

.iyr_apply <- function(spec, skel, id = "01") {
  return(swereg::tteplan_apply_exclusions(
    data.table::copy(skel),
    spec,
    list(enrollment_id = id)
  ))
}

# --- 1. the reader accepts a valid entry -------------------------------------

test_that("the reader accepts a valid isoyear_range entry", {
  spec <- .iyr_read(.iyr_spec_list())
  ai <- spec$enrollments[[1]]$additional_inclusion
  expect_length(ai, 2L)
  expect_identical(ai[[2]]$type, "isoyear_range")
  expect_identical(ai[[2]]$min, 2016L)
  expect_identical(ai[[2]]$max, 2017L)
  expect_identical(ai[[1]]$type, "age_range")
})

# --- 2. the reader refuses each invalid entry --------------------------------

.iyr_read_with <- function(...) {
  return(.iyr_read(.iyr_spec_list(
    enrollments = list(.iyr_enrollment(extra = list(...)))
  )))
}

.iyr_label <- "enrollment '01' additional_inclusion\\[2\\] 'Trial registration 2016-2017'"

test_that("the reader refuses an isoyear_range with no min", {
  expect_error(
    .iyr_read_with(.iyr_entry(min = NULL)),
    regexp = paste0(.iyr_label, " is missing 'min'")
  )
})

test_that("the reader refuses an isoyear_range with no max", {
  expect_error(
    .iyr_read_with(.iyr_entry(max = NULL)),
    regexp = paste0(.iyr_label, " is missing 'max'")
  )
})

test_that("the reader refuses an isoyear_range bound that is not whole", {
  expect_error(
    .iyr_read_with(.iyr_entry(min = 2016.5)),
    regexp = paste0(
      .iyr_label,
      " has min '2016.5'\\. min MUST be one whole number"
    )
  )
})

test_that("the reader refuses an isoyear_range bound that is text", {
  expect_error(
    .iyr_read_with(.iyr_entry(max = "2017")),
    regexp = paste0(
      .iyr_label,
      " has max '2017'\\. max MUST be one whole number"
    )
  )
})

test_that("the reader refuses an isoyear_range with min above max", {
  expect_error(
    .iyr_read_with(.iyr_entry(min = 2017L, max = 2016L)),
    regexp = paste0(.iyr_label, " has min 2017 above max 2016")
  )
})

test_that("the reader refuses an isoyear_range that starts before the global range", {
  expect_error(
    .iyr_read_with(.iyr_entry(min = 2014L, max = 2016L)),
    regexp = paste0(
      .iyr_label,
      " has the range 2014 to 2016, which is not inside ",
      "inclusion_criteria\\$isoyears \\(2015 to 2018\\)"
    )
  )
})

test_that("the reader refuses an isoyear_range that ends after the global range", {
  expect_error(
    .iyr_read_with(.iyr_entry(min = 2016L, max = 2019L)),
    regexp = paste0(
      .iyr_label,
      " has the range 2016 to 2019, which is not inside ",
      "inclusion_criteria\\$isoyears \\(2015 to 2018\\)"
    )
  )
})

test_that("the reader refuses a second isoyear_range in one enrollment", {
  expect_error(
    .iyr_read_with(
      .iyr_entry(),
      .iyr_entry(min = 2017L, max = 2018L, name = "Second range")
    ),
    regexp = paste0(
      "enrollment '01' additional_inclusion\\[3\\] 'Second range' is the ",
      "second isoyear_range entry of this enrollment"
    )
  )
})

test_that("the reader refuses isoyear_range in the global criteria", {
  # The entry in the shape an enrollment accepts. The key gate refuses `min`
  # and `max`, which no global criterion declares.
  spec <- .iyr_spec_list()
  spec$inclusion_criteria$criteria <- list(.iyr_entry())
  expect_error(
    .iyr_read(spec),
    regexp = paste0(
      "Unknown key 'min'\\. \\$/inclusion_criteria/criteria\\[\\] accepts: ",
      "implementation, name, rationale, type"
    )
  )

  # The entry without its bounds reaches the type check. The global container
  # accepts `has_event` alone.
  spec$inclusion_criteria$criteria <- list(
    list(name = "Trial registration 2016-2017", type = "isoyear_range")
  )
  expect_error(
    .iyr_read(spec),
    regexp = paste0(
      "inclusion_criteria\\$criteria\\[1\\] 'Trial registration 2016-2017' ",
      "has type 'isoyear_range'\\. The types this container accepts are ",
      "'has_event'"
    )
  )
})

# --- 3. the apply step writes a column of its own ----------------------------

test_that("the apply step writes eligible_isoyears_2016_2017 beside eligible_age", {
  spec <- .iyr_read(.iyr_spec_list())
  sk <- .iyr_skeleton(.iyr_weeks(2015L, 2018L))
  # Person 2 is outside the age range, so `eligible_age` separates the two.
  sk[id == 2L, rd_age_continuous := 85]

  out <- .iyr_apply(spec, sk)

  expect_identical(
    out$eligible_isoyears_2016_2017,
    out$isoyear %in% 2016:2017
  )
  expect_true(any(out$eligible_isoyears_2016_2017))
  expect_false(all(out$eligible_isoyears_2016_2017))

  # `eligible_age` is the age range alone, as it is without the year range.
  ref <- .iyr_apply(
    .iyr_read(.iyr_spec_list(enrollments = list(.iyr_enrollment()))),
    sk
  )
  expect_identical(out$eligible_age, ref$eligible_age)
  expect_identical(out$eligible_age, out$id == 1L)

  cols <- attr(out, "eligible_cols")
  expect_identical(sum(cols == "eligible_age"), 1L)
  expect_identical(sum(cols == "eligible_isoyears_2016_2017"), 1L)
  expect_identical(
    cols,
    c("eligible_isoyears", "eligible_age", "eligible_isoyears_2016_2017")
  )
})

# --- 4. the overwrite trap ---------------------------------------------------

test_that("an enrolled week satisfies both the age range and the year range", {
  # A second `age_range` in place of `isoyear_range` overwrites
  # `eligible_age`, so row 3 (2016, age 60) would read TRUE.
  spec <- .iyr_read(.iyr_spec_list(
    enrollments = list(.iyr_enrollment(
      extra = list(.iyr_entry()),
      age_variable = "age",
      age_min = 45,
      age_max = 59
    ))
  ))
  d <- data.table::data.table(
    id = 1:3,
    isoyear = c(2016L, 2018L, 2016L),
    age = c(50, 50, 60)
  )

  out <- swereg::tteplan_apply_exclusions(d, spec, list(enrollment_id = "01"))

  expect_identical(out$eligible, c(TRUE, FALSE, FALSE))
})

# --- 5. the production preparation step --------------------------------------

# The plan `.s1a_worker_multi()` reads, built from the same specification and
# skeleton it would meet in production.
.iyr_plan <- function(env, spec, sk) {
  root <- tempfile("iyr_plan_")
  dir_spec <- file.path(root, "spec")
  dir_tteplan <- file.path(root, "tteplan")
  dir_results <- file.path(root, "results")
  dir_meta <- file.path(root, "meta")
  for (d in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  withr::defer(unlink(root, recursive = TRUE, force = TRUE), envir = env)

  skel_path <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  .iyr_write(spec, dir_spec)
  plan <- suppressMessages(swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = max(sk$isoyearweek),
    check_skeletons = FALSE
  ))
  return(list(plan = plan, skel_path = skel_path))
}

# The two production steps `.s1a_worker_multi()` runs for one enrollment: the
# projection to the columns any enrollment needs, then the preparation.
.iyr_prepare <- function(built, i) {
  canonical <- swereg:::.s1_load_skeleton(built$skel_path, 1L)
  needed <- swereg:::.tte_canonical_needed_cols(
    built$plan$spec,
    list(built$plan[[i]]),
    names(canonical)
  )
  drop_cols <- setdiff(names(canonical), needed)
  if (length(drop_cols) > 0L) {
    canonical[, (drop_cols) := NULL]
  }
  return(swereg:::.s1_prepare_loaded(
    canonical,
    built$plan[[i]],
    built$plan$spec,
    derive_confounders = FALSE
  ))
}

test_that("every week the preparation step keeps lies in the range", {
  built <- .iyr_plan(
    environment(),
    .iyr_spec_list(),
    .iyr_skeleton(.iyr_weeks(2015L, 2018L))
  )
  expect_identical(built$plan[[1]]$enrollment_id, "01")

  prepared <- .iyr_prepare(built, 1L)

  expect_true("eligible_isoyears_2016_2017" %in% attr(prepared, "eligible_cols"))
  expect_true(any(prepared$eligible))
  expect_true(all(prepared[eligible == TRUE]$isoyear %in% 2016:2017))
  # The skeleton holds 2015 and 2018 weeks, and the step keeps none of them.
  expect_true(any(prepared$isoyear %in% c(2015L, 2018L)))
  expect_false(any(prepared[isoyear %in% c(2015L, 2018L)]$eligible))
})

# --- 6. a band that crosses the new year -------------------------------------

test_that("a band that crosses the new year recruits only from in-range weeks", {
  # Band 1526 of a 4-week grid holds 2016-52, 2017-01, 2017-02 and 2017-03.
  weeks <- c(sprintf("2016-%02d", 50:52), sprintf("2017-%02d", 1:3))
  band <- data.table::data.table(isoyearweek = weeks)
  swereg:::.assign_trial_ids(band, 4L)
  expect_identical(band$trial_id, c(1525L, 1525L, rep(1526L, 4L)))

  # Person 2 starts systemic MHT in 2017-02, the third week of the band.
  sk <- .iyr_skeleton(weeks)
  sk[id == 2L & isoyearweek >= "2017-02", rd_approach1_single := "systemic_mht"]

  spec <- .iyr_spec_list(enrollments = list(
    .iyr_enrollment(
      id = "early",
      extra = list(.iyr_entry(2015L, 2016L, "Trial registration 2015-2016"))
    ),
    .iyr_enrollment(
      id = "late",
      extra = list(.iyr_entry(2017L, 2018L, "Trial registration 2017-2018"))
    )
  ))
  built <- .iyr_plan(environment(), spec, sk)
  ids <- vapply(
    seq_along(built$plan$spec$enrollments),
    function(i) built$plan[[i]]$enrollment_id,
    character(1)
  )
  expect_identical(ids, c("early", "late"))

  expected <- list(
    early = list(
      range = 2015:2016,
      weeks = "2016-52",
      recruit = c("2016-52", "2016-52"),
      n_intervention = 0L
    ),
    late = list(
      range = 2017:2018,
      weeks = c("2017-01", "2017-02", "2017-03"),
      recruit = c("2017-01", "2017-01"),
      n_intervention = 1L
    )
  )

  for (i in seq_along(ids)) {
    want <- expected[[ids[i]]]
    prepared <- .iyr_prepare(built, i)
    swereg:::.assign_trial_ids(prepared, 4L)

    # The band reads only the weeks inside the enrollment's own range.
    in_band <- prepared[trial_id == 1526L & eligible == TRUE]
    expect_identical(
      sort(unique(in_band$isoyearweek)),
      want$weeks,
      info = ids[i]
    )

    # The attrition counts each person-band once.
    att <- swereg:::.s1_compute_attrition(
      prepared,
      attr(prepared, "eligible_cols"),
      pid = "id"
    )
    last <- att[
      trial_id == 1526L &
        criterion == utils::tail(attr(prepared, "eligible_cols"), 1L)
    ]
    expect_identical(nrow(last), 1L, info = ids[i])
    expect_identical(last$n_persons, 2L, info = ids[i])
    expect_identical(last$n_person_trials, 2L, info = ids[i])
    expect_identical(last$n_intervention, want$n_intervention, info = ids[i])

    # The recruiting week lies inside the enrollment's own range.
    tuples <- swereg:::.band_baseline_treatment(
      prepared,
      person_id_col = "id",
      treatment_col = "rd_intervention",
      eligible_col = "eligible",
      out_col = "intervention"
    )[trial_id == 1526L][order(id)]
    recruit <- cstime::dates_by_isoyearweek$isoyearweek[
      tuples$recruit_week_index + 1L
    ]
    expect_identical(recruit, want$recruit, info = ids[i])
    expect_true(
      all(as.integer(substr(recruit, 1L, 4L)) %in% want$range),
      info = ids[i]
    )
  }
})

# --- 7. a skeleton with no spare column slot ---------------------------------

test_that("the apply step returns the year-range column out of slots", {
  spec <- .iyr_read(.iyr_spec_list())
  sk <- .iyr_skeleton(.iyr_weeks(2015L, 2018L))
  # Serialization drops data.table's over-allocation, and `setalloccol()`
  # then sets the exact number of spare slots back.
  sk <- data.table::setalloccol(unserialize(serialize(sk, NULL)), 0L)
  expect_identical(data.table::truelength(sk) - ncol(sk), 0L)

  out <- swereg::tteplan_apply_exclusions(sk, spec, list(enrollment_id = "01"))

  expect_true("eligible_isoyears_2016_2017" %in% names(out))
  expect_false("eligible_isoyears_2016_2017" %in% names(sk))
  expect_identical(
    out$eligible_isoyears_2016_2017,
    out$isoyear %in% 2016:2017
  )
  expect_true("eligible_isoyears_2016_2017" %in% attr(out, "eligible_cols"))
})
