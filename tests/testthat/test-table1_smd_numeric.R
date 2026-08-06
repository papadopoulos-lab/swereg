# =============================================================================
# smd_numeric: the unrounded standardised difference as a real column
# =============================================================================
# The fixture below is hand-computable, which is the point. Both SMDs are
# closed-form, and both differ from their three-decimal display string by more
# than 1e-5, so a value parsed back out of the `SMD` column cannot satisfy the
# assertions here.
#
#   age: |2.5 - 1.0| / sqrt((s0^2 + s1^2) / 2), s0^2 = 4, s1^2 = 5/3
#   edu: |0.75 - 0.25| / sqrt((0.75*0.25 + 0.25*0.75) / 2)
# =============================================================================

love_fixture_t1 <- function(include_smd = TRUE) {
  d <- data.table::data.table(
    exp = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    age = c(0, 0, 0, 4, 1, 2, 3, 4),
    edu = factor(c("a", "b", "b", "b", "a", "a", "a", "b"), levels = c("a", "b"))
  )
  swereg:::.swereg_table1(
    d,
    vars = c("age", "edu"),
    strata = "exp",
    include_smd = include_smd,
    show_missing = "none"
  )
}

# Hand-computed, not read back off the object under test.
SMD_AGE <- 1.5 / sqrt((4 + 5 / 3) / 2)
SMD_EDU <- 0.5 / sqrt(0.1875)


test_that("smd_numeric is a row-parallel column holding the unrounded double", {
  t1 <- love_fixture_t1()

  expect_true("smd_numeric" %in% names(t1))
  expect_type(t1$smd_numeric, "double")
  expect_identical(length(t1$smd_numeric), nrow(t1))
  expect_identical(nrow(t1), 5L)

  # Rows 1 (N), 2 (Sum of weights) and 5 (second level of edu) carry no SMD.
  expect_true(is.na(t1$smd_numeric[1]))
  expect_true(is.na(t1$smd_numeric[2]))
  expect_true(is.na(t1$smd_numeric[5]))

  expect_equal(t1$smd_numeric[3], SMD_AGE, tolerance = 1e-12)
  expect_equal(t1$smd_numeric[4], SMD_EDU, tolerance = 1e-12)

  # It sits beside the display string, it does not replace it.
  expect_identical(t1$SMD[3], "0.891")
  expect_identical(t1$SMD[4], "1.155")
})


test_that("smd_numeric is not the SMD string parsed back to a number", {
  t1 <- love_fixture_t1()
  # as.numeric("0.891") would land exactly on 0.891; the real double does not.
  expect_gt(abs(t1$smd_numeric[3] - 0.891), 1e-5)
  expect_gt(abs(t1$smd_numeric[4] - 1.155), 1e-5)
})


test_that("smd_numeric survives subsetting, reordering, rbindlist and qs2", {
  t1 <- love_fixture_t1()

  # Row subsetting: the SMD stays attached to its own row.
  sub <- t1[c(4, 3)]
  expect_equal(sub$smd_numeric[1], SMD_EDU, tolerance = 1e-12)
  expect_equal(sub$smd_numeric[2], SMD_AGE, tolerance = 1e-12)

  # Row reordering by a different column.
  reordered <- t1[order(t1$Variable)]
  expect_equal(
    reordered$smd_numeric[reordered$Variable == "edu"],
    SMD_EDU,
    tolerance = 1e-12
  )

  # rbindlist of two copies.
  stacked <- data.table::rbindlist(list(t1, t1), use.names = TRUE)
  expect_identical(nrow(stacked), 10L)
  expect_equal(stacked$smd_numeric[3], SMD_AGE, tolerance = 1e-12)
  expect_equal(stacked$smd_numeric[8], SMD_AGE, tolerance = 1e-12)

  # qs2 save/read cycle.
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "t1.qs2")
  swereg::qs2_write_atomic(t1, f)
  back <- swereg::qs2_read(f)
  expect_true("smd_numeric" %in% names(back))
  expect_equal(back$smd_numeric[3], SMD_AGE, tolerance = 1e-12)
  expect_equal(back$smd_numeric[4], SMD_EDU, tolerance = 1e-12)
})


test_that("a cached baseline panel without smd_numeric is marked stale", {
  panel <- love_fixture_t1()
  expect_true("smd_numeric" %in% names(panel))

  fresh <- list(table1_ipw_trunc = panel)
  aged <- list(
    table1_ipw_trunc = data.table::copy(panel)[, smd_numeric := NULL]
  )
  expect_false("smd_numeric" %in% names(aged$table1_ipw_trunc))
  # The aged panel still carries the class, which is what the pre-repair
  # predicate tested. Only the column test can tell it apart.
  expect_s3_class(aged$table1_ipw_trunc, "swereg_table1")

  # Reach the predicate export_tables() uses, not a copy of its expression.
  expect_false(swereg:::.baseline_panel_is_stale(fresh))
  expect_true(swereg:::.baseline_panel_is_stale(aged))

  # A pre-swereg_table1 cache stays stale, and an empty result stays current.
  plain <- list(table1_ipw_trunc = data.frame(Variable = "N"))
  expect_true(swereg:::.baseline_panel_is_stale(plain))
  expect_false(swereg:::.baseline_panel_is_stale(NULL))
  expect_false(swereg:::.baseline_panel_is_stale(list(n_baseline = 10L)))

  # And export_tables() reaches it: the vapply target is this function.
  export_src <- paste(
    deparse(body(utils::removeSource(swereg::TTEPlan$public_methods$export_tables))),
    collapse = " "
  )
  expect_match(export_src, ".baseline_panel_is_stale", fixed = TRUE)
})


test_that(".render_love_plot draws the 0.1 line and both weighting series", {
  skip_if_not_installed("ggplot2")

  unw <- love_fixture_t1()
  # A second, better-balanced panel standing in for the weighted series.
  d <- data.table::data.table(
    exp = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    age = c(1, 2, 3, 4, 1, 2, 3, 4),
    edu = factor(c("a", "b", "a", "b", "a", "b", "a", "b"), levels = c("a", "b"))
  )
  wtd <- swereg:::.swereg_table1(
    d,
    vars = c("age", "edu"),
    strata = "exp",
    include_smd = TRUE,
    show_missing = "none"
  )

  df <- swereg:::.build_love_df(unw, wtd)
  p <- swereg:::.render_love_plot(df, title = "balance")
  expect_s3_class(p, "ggplot")

  # The 0.1 reference line.
  vlines <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)
  expect_length(vlines, 1L)
  xis <- unname(unlist(lapply(vlines, function(l) l$data$xintercept)))
  expect_equal(xis, 0.1, tolerance = 1e-12)

  # Both weighting series, on the same covariates.
  expect_setequal(
    as.character(unique(p$data$weighting)),
    c("Unweighted", "IPW truncated")
  )
  expect_setequal(as.character(unique(p$data$variable)), c("age", "edu"))
  expect_identical(nrow(p$data), 4L)

  # The plotted values are the unrounded doubles, not the display strings.
  unw_age <- p$data[
    p$data$weighting == "Unweighted" & p$data$variable == "age",
  ]$smd
  expect_equal(unw_age, SMD_AGE, tolerance = 1e-12)

  # It builds without error, which is what insertImage() needs.
  expect_silent(invisible(ggplot2::ggplot_build(p)))
})


test_that(".write_love_plot adds a sheet and writes PNG + PDF sidecars", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("openxlsx")

  t1 <- love_fixture_t1()
  tmp <- withr::local_tempdir()
  wb <- openxlsx::createWorkbook()
  paths <- swereg:::.write_love_plot(
    wb,
    "Love plot",
    t1_unweighted = t1,
    t1_weighted = t1,
    title = "Love plot: covariate balance",
    img_dir = tmp,
    img_basename = "tables_love_plot"
  )
  expect_true("Love plot" %in% openxlsx::sheets(wb))
  expect_true(file.exists(file.path(tmp, "tables_love_plot.png")))
  expect_true(file.exists(file.path(tmp, "tables_love_plot.pdf")))
  expect_identical(basename(paths$png), "tables_love_plot.png")
})


test_that("smd_numeric never reaches a rendered table", {
  t1 <- love_fixture_t1()
  expect_true("smd_numeric" %in% names(t1))

  stripped <- swereg:::.t1_drop_numeric(t1)
  expect_false("smd_numeric" %in% names(stripped))
  expect_true("SMD" %in% names(stripped))
  # The input is not modified by reference.
  expect_true("smd_numeric" %in% names(t1))
  # A panel that never had the column is returned untouched.
  no_smd <- love_fixture_t1(include_smd = FALSE)
  expect_identical(swereg:::.t1_drop_numeric(no_smd), no_smd)

  wb <- openxlsx::createWorkbook()
  swereg:::.write_tableone_sheet(wb, "Table 1", t1, title = "T1")
  written <- openxlsx::readWorkbook(wb, "Table 1", startRow = 3L)
  expect_false("smd_numeric" %in% names(written))
  expect_true("SMD" %in% names(written))
})


test_that("export_tables appends a TOC name and a TOC description in lockstep", {
  # The sheet ritual is three steps: write the sheet, append to toc_names,
  # append to toc_desc. Dropping the third step misaligns every later row of
  # the table of contents and raises no error, so count the appends in the
  # method body directly. This is a syntax-tree count of assignments of the
  # form `x <- c(x, ...)`; it proves the two vectors grow the same number of
  # times, not that each pair describes the same sheet.
  count_appends <- function(expr, target) {
    n <- 0L
    sym <- as.name(target)
    walk <- function(e) {
      if (!is.call(e)) {
        return(invisible(NULL))
      }
      if (
        length(e) >= 3L &&
          identical(e[[1L]], quote(`<-`)) &&
          identical(e[[2L]], sym) &&
          is.call(e[[3L]]) &&
          identical(e[[3L]][[1L]], quote(c)) &&
          length(e[[3L]]) >= 2L &&
          identical(e[[3L]][[2L]], sym)
      ) {
        n <<- n + 1L
      }
      for (i in seq_along(e)) {
        part <- e[[i]]
        ok <- tryCatch(
          {
            is.call(part)
            TRUE
          },
          error = function(...) FALSE
        )
        if (ok) walk(part)
      }
      invisible(NULL)
    }
    walk(expr)
    n
  }

  fn <- utils::removeSource(swereg::TTEPlan$public_methods$export_tables)
  n_names <- count_appends(body(fn), "toc_names")
  n_desc <- count_appends(body(fn), "toc_desc")

  expect_gt(n_names, 10L)
  expect_identical(n_names, n_desc)
})
