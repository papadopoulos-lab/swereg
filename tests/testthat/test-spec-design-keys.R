# The two design keys of a specification, `period_width` and
# `admin_censoring_isoyearweek`, are required top-level keys.
#
#   (a) tteplan_read_spec() refuses a spec without one or both keys, with one
#       message that names both keys and states which are missing;
#   (b) it refuses a bad value of either key;
#   (c) tteplan_check_spec() reports each problem as one read_error row on
#       the path `$`, also when the key gate fails;
#   (d) the plan, the OSF text and the Study Specification sheet take both
#       values from the spec, and the three entry points have no argument for
#       either value;
#   (e) a plan built before swereg 27.3.10 stores a spec without the keys.
#       tteplan_load() refuses it. A plan whose stored spec has the keys
#       reloads strictly.
#
# The spec is spr_spec_list() of helper-sparse_rule.R.

skip_if_not_installed("yaml")
skip_if_not_installed("withr")

DK_KEYS <- c("period_width", "admin_censoring_isoyearweek")

# The spec of helper-sparse_rule.R with the two design values replaced.
# `NULL` removes a key.
dk_spec <- function(period_width = 2L, admin = "2020-10") {
  spec <- spr_spec_list()
  spec$period_width <- period_width
  spec$admin_censoring_isoyearweek <- admin
  return(spec)
}

# Write `spec` as spec_v001.yaml in a new directory, and return the path.
dk_write <- function(spec, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  return(spr_write_spec(spec, dir))
}

# The message of the error of `expr`, or NA when it does not stop.
dk_error <- function(expr) {
  return(tryCatch(
    {
      force(expr)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  ))
}

# The directories of one plan under `root`.
dk_dirs <- function(root) {
  dirs <- file.path(root, c("spec", "tteplan", "results", "meta"))
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  return(dirs)
}

# Build a plan through s0 from `spec`. `skeleton` is the name of the first
# skeleton file under the tteplan directory. `quiet = FALSE` lets the
# messages of s0 through.
dk_plan <- function(spec, root, skeleton = "absent.qs2", quiet = TRUE) {
  dirs <- dk_dirs(root)
  spr_write_spec(spec, dirs[1])
  run <- if (quiet) suppressMessages else identity
  return(run(tteplan_from_spec_and_registrystudy(
    study = list(
      skeleton_files = file.path(dirs[2], skeleton),
      data_meta_dir = dirs[4]
    ),
    candidate_dir_spec = dirs[1],
    candidate_dir_tteplan = dirs[2],
    candidate_dir_results = dirs[3],
    spec_version = "v001",
    check_skeletons = FALSE
  )))
}

# Every cell of one sheet, as text.
dk_cells <- function(path, sheet) {
  return(openxlsx::read.xlsx(
    path,
    sheet = sheet,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  ))
}

# The value in column 2 of the row labelled `label`.
dk_value <- function(cells, label) {
  return(cells[[2]][which(cells[[1]] == label)])
}


# ---------------------------------------------------------------------------
# (a) the missing keys
# ---------------------------------------------------------------------------

test_that("a spec without period_width stops, naming both keys", {
  path <- dk_write(dk_spec(period_width = NULL))
  expect_error(tteplan_read_spec(path), "period_width is missing", fixed = TRUE)
  msg <- dk_error(tteplan_read_spec(path))
  expect_match(msg, "admin_censoring_isoyearweek", fixed = TRUE)
  expect_false(grepl("admin_censoring_isoyearweek is missing", msg, fixed = TRUE))
  expect_match(
    msg,
    "For example: period_width: 4 and admin_censoring_isoyearweek: \"2025-01\".",
    fixed = TRUE
  )
})

test_that("a spec without admin_censoring_isoyearweek stops, naming both keys", {
  path <- dk_write(dk_spec(admin = NULL))
  expect_error(
    tteplan_read_spec(path),
    "admin_censoring_isoyearweek is missing",
    fixed = TRUE
  )
  msg <- dk_error(tteplan_read_spec(path))
  expect_match(msg, "period_width", fixed = TRUE)
  expect_false(grepl("period_width is missing", msg, fixed = TRUE))
})

test_that("a spec without both keys stops with one message that lists both", {
  msg <- dk_error(tteplan_read_spec(dk_write(dk_spec(NULL, NULL))))
  expect_identical(
    msg,
    paste0(
      "The spec MUST declare both top-level design keys, period_width and ",
      "admin_censoring_isoyearweek.\n",
      "  period_width is missing.\n",
      "  admin_censoring_isoyearweek is missing.\n",
      "For example: period_width: 4 and admin_censoring_isoyearweek: ",
      "\"2025-01\"."
    )
  )
})

test_that("s0 refuses a spec without the keys", {
  expect_error(
    dk_plan(dk_spec(NULL, NULL), withr::local_tempdir()),
    "period_width is missing",
    fixed = TRUE
  )
})


# ---------------------------------------------------------------------------
# (b) the values
# ---------------------------------------------------------------------------

test_that("period_width 0 and admin_censoring_isoyearweek 2025-54 each stop", {
  msg <- dk_error(tteplan_read_spec(dk_write(dk_spec(period_width = 0L))))
  expect_match(msg, "period_width MUST be at least 1. It holds: 0.", fixed = TRUE)
  expect_match(msg, "admin_censoring_isoyearweek", fixed = TRUE)
  msg <- dk_error(tteplan_read_spec(dk_write(dk_spec(admin = "2025-54"))))
  expect_match(
    msg,
    "admin_censoring_isoyearweek MUST be one string YYYY-WW, with the week from 01 to 53. It holds: \"2025-54\".",
    fixed = TRUE
  )
  expect_match(msg, "period_width", fixed = TRUE)
})

test_that("every other bad value of either key stops, naming both keys", {
  bad_width <- list(TRUE, "4", c(2L, 4L), 2.5, -1L, Inf)
  for (v in bad_width) {
    msg <- dk_error(tteplan_read_spec(dk_write(dk_spec(period_width = v))))
    expect_match(msg, "period_width MUST be", fixed = TRUE, info = format(v))
    expect_match(msg, "admin_censoring_isoyearweek", fixed = TRUE)
  }
  bad_week <- list("2025-00", "2025-1", "25-01", 202501L, c("2025-01", "2025-02"))
  for (v in bad_week) {
    msg <- dk_error(tteplan_read_spec(dk_write(dk_spec(admin = v))))
    expect_match(
      msg,
      "admin_censoring_isoyearweek MUST be one string YYYY-WW",
      fixed = TRUE,
      info = format(v)
    )
    expect_match(msg, "period_width", fixed = TRUE)
  }
})

test_that("a valid spec reads, and period_width becomes an integer", {
  spec <- tteplan_read_spec(dk_write(dk_spec(period_width = 2, admin = "2020-53")))
  expect_identical(spec$period_width, 2L)
  expect_identical(spec$admin_censoring_isoyearweek, "2020-53")
  spec <- tteplan_read_spec(dk_write(dk_spec(4L, "2025-01")))
  expect_identical(spec$period_width, 4L)
})

test_that("a spec with standing_methods$admin_censoring and both keys still builds", {
  spec <- dk_spec()
  spec$standing_methods <- list(
    admin_censoring = list(note = "ISO week 2025-01")
  )
  path <- dk_write(spec)
  expect_identical(nrow(tteplan_check_spec(path)), 0L)
  plan <- dk_plan(spec, withr::local_tempdir())
  expect_identical(plan$global_max_isoyearweek, "2020-10")
  expect_identical(plan$spec$standing_methods$admin_censoring$note, "ISO week 2025-01")
})


# ---------------------------------------------------------------------------
# (c) the checker
# ---------------------------------------------------------------------------

test_that("check_spec reports a missing or bad design key as one read_error row on $", {
  for (spec in list(
    dk_spec(period_width = NULL),
    dk_spec(admin = NULL),
    dk_spec(period_width = 0L),
    dk_spec(admin = "2025-54")
  )) {
    path <- dk_write(spec)
    r <- tteplan_check_spec(path)
    expect_identical(r$path, "$")
    expect_identical(r$kind, "read_error")
    expect_identical(r$message, dk_error(tteplan_read_spec(path)))
    for (k in DK_KEYS) {
      expect_match(r$message, k, fixed = TRUE)
    }
  }
})

test_that("check_spec reports a missing design key also when the key gate fails", {
  spec <- dk_spec(period_width = NULL)
  spec$study$colour <- "blue"
  r <- tteplan_check_spec(dk_write(spec))
  expect_setequal(r$kind, c("undeclared_key", "read_error"))
  msg <- r$message[r$kind == "read_error"]
  expect_length(msg, 1L)
  expect_identical(r$path[r$kind == "read_error"], "$")
  expect_match(msg, "period_width is missing", fixed = TRUE)
  expect_match(msg, "admin_censoring_isoyearweek", fixed = TRUE)
})


# ---------------------------------------------------------------------------
# (d) the plan, the OSF text and the Study Specification sheet
# ---------------------------------------------------------------------------

test_that("the three entry points take neither design value as an argument", {
  root <- withr::local_tempdir()
  path <- dk_write(dk_spec())
  for (arg in list(
    list(period_width = 4L),
    list(global_max_isoyearweek = "2025-01")
  )) {
    expect_error(
      do.call(
        tteplan_from_spec_and_registrystudy,
        c(
          list(
            study = list(skeleton_files = file.path(root, "absent.qs2")),
            candidate_dir_spec = dirname(path),
            candidate_dir_tteplan = root,
            candidate_dir_results = root,
            spec_version = "v001",
            check_skeletons = FALSE
          ),
          arg
        )
      ),
      "unused argument",
      fixed = TRUE
    )
    expect_error(
      do.call(tteplan_osf_text, c(list(spec_path = path), arg)),
      "unused argument",
      fixed = TRUE
    )
    expect_error(
      do.call(
        tteplan_excel_spec_summary,
        c(list(spec_path = path, path = file.path(root, "x.xlsx")), arg)
      ),
      "unused argument",
      fixed = TRUE
    )
  }
})

test_that("s0 takes period_width 2 and admin_censoring_isoyearweek 2020-10 from the spec", {
  plan <- dk_plan(dk_spec(), withr::local_tempdir())
  expect_identical(plan$period_width, 2L)
  expect_identical(plan$global_max_isoyearweek, "2020-10")
  design <- plan$enrollment_spec(1L)$design
  expect_identical(design$period_width, 2L)
  expect_identical(design$admin_censor_isoyearweek, "2020-10")
})

test_that("s0 with a skeleton validates it, and does not take the week from it", {
  skip_if_not_installed("qs2")
  root <- withr::local_tempdir()
  dirs <- dk_dirs(root)
  sk <- spr_skeleton(n_persons = 40L)
  qs2::qs_save(sk, file.path(dirs[2], SPR_SKELETON))
  validated <- 0L
  testthat::local_mocked_bindings(
    tteplan_validate_spec = function(spec, skeleton) {
      validated <<- validated + 1L
      return(invisible(TRUE))
    },
    .package = "swereg"
  )
  msgs <- testthat::capture_messages(
    plan <- dk_plan(dk_spec(), root, skeleton = SPR_SKELETON, quiet = FALSE)
  )
  expect_identical(validated, 1L)
  expect_false(any(grepl("prevalent-user check skipped", msgs, fixed = TRUE)))
  # The skeleton ends in 2019-26. The plan keeps the week of the spec.
  expect_identical(max(sk$isoyearweek), "2019-26")
  expect_identical(plan$global_max_isoyearweek, "2020-10")
  # Without the file, s0 says that it skipped the check.
  msgs <- testthat::capture_messages(
    dk_plan(dk_spec(), withr::local_tempdir(), quiet = FALSE)
  )
  expect_identical(validated, 1L)
  expect_true(any(grepl(
    "prevalent-user check skipped: no skeleton loaded",
    msgs,
    fixed = TRUE
  )))
})

test_that("the OSF text takes the width and the week from the spec", {
  osf <- suppressWarnings(tteplan_osf_text(dk_write(dk_spec())))
  expect_match(
    osf[["Study design"]],
    "is cut into consecutive enrollment periods of 2 weeks",
    fixed = TRUE
  )
  expect_match(
    osf[["Study design"]],
    "at the end of ISO week 2020-10 (8 March 2020)",
    fixed = TRUE
  )
  expect_match(
    osf[["Transformations"]],
    "Follow-up is split into intervals of 2 weeks from time zero.",
    fixed = TRUE
  )
})

test_that("the Study Specification sheet takes the week and the width from the spec", {
  skip_if_not_installed("openxlsx")
  root <- withr::local_tempdir()
  path <- dk_write(dk_spec())
  out <- file.path(root, "spec_only.xlsx")
  suppressMessages(tteplan_excel_spec_summary(spec_path = path, path = out))
  ss <- dk_cells(out, "Study Specification")
  expect_identical(dk_value(ss, "Admin censoring:"), "2020-10")
  tp <- dk_cells(out, "Target trial protocol")
  expect_true(any(grepl(
    "the 2-week enrollment period",
    unlist(tp),
    fixed = TRUE
  )))
  # The workbook of a plan from the same spec holds the same week.
  plan <- dk_plan(dk_spec(), file.path(root, "plan"))
  plan_out <- file.path(root, "plan.xlsx")
  suppressMessages(plan$excel_spec_summary(path = plan_out))
  expect_identical(
    dk_value(dk_cells(plan_out, "Study Specification"), "Admin censoring:"),
    "2020-10"
  )
})


# ---------------------------------------------------------------------------
# (e) the reload
# ---------------------------------------------------------------------------

test_that("tteplan_load() refuses a plan saved by 27.3.9", {
  skip_if_not_installed("qs2")
  root <- withr::local_tempdir()
  plan <- dk_plan(dk_spec(4L, "2025-01"), root)
  # The state that swereg 27.3.9 left: the stored spec has no design key, and
  # the two values sit only in the plan fields.
  plan$spec[DK_KEYS] <- NULL
  plan$period_width <- 4L
  plan$global_max_isoyearweek <- "2025-01"
  saved <- plan$save()
  expect_error(tteplan_load(saved), "older than 27.3.10", fixed = TRUE)
})

test_that("a plan built with the keys refuses a reload of a spec file that lost period_width", {
  root <- withr::local_tempdir()
  spec <- dk_spec(4L, "2025-01")
  plan <- dk_plan(spec, root)
  expect_identical(plan$spec$period_width, 4L)
  spec$period_width <- NULL
  spr_write_spec(spec, plan$dir_spec)
  expect_error(plan$reload_spec(quiet = TRUE), "period_width is missing", fixed = TRUE)
})

test_that("a reload that changes either design key is structural and is not applied", {
  root <- withr::local_tempdir()
  plan <- dk_plan(dk_spec(4L, "2025-01"), root)
  spr_write_spec(dk_spec(4L, "2024-52"), plan$dir_spec)
  expect_warning(
    plan$reload_spec(quiet = TRUE),
    "admin_censoring_isoyearweek: 2025-01 -> 2024-52",
    fixed = TRUE
  )
  expect_identical(
    plan$spec_reload_skipped_diffs,
    "admin_censoring_isoyearweek: 2025-01 -> 2024-52"
  )
  expect_identical(plan$spec$admin_censoring_isoyearweek, "2025-01")
  expect_identical(plan$global_max_isoyearweek, "2025-01")
})
