# The rule-form registry, `.TTE_SPEC_RULE_FORMS` in R/tteplan_spec_forms.R, is
# the one list of what a specification rule computes. The vignette
# tte-spec-schema renders its section "What each rule computes" from it. These
# tests hold the registry to the code in both directions:
#
#   (a) every rule type the reader dispatches on has a form, and every form
#       names a type the reader knows;
#   (b) every case of the batch evaluator has a form, and every form names a
#       case the evaluator has;
#   (c) every example is accepted in every block its form names, and writes
#       the column its worked case states;
#   (d) every window form converts, or is refused, as the registry states;
#   (e) the vignette renders the registry, so a form the registry holds
#       cannot be missing from it.
#
# A behaviour change to the reader or the compiler fails (b) to (d) until the
# registry, and with it the vignette, states the new behaviour.

forms <- .TTE_SPEC_RULE_FORMS
form_ids <- vapply(forms, `[[`, character(1), "id")

# One form by id.
sf_form <- function(id) {
  return(forms[[match(id, form_ids)]])
}

# The case names of the first `switch()` call in a function body.
sf_switch_cases <- function(fn) {
  find <- function(x) {
    if (is.call(x)) {
      if (identical(x[[1L]], as.name("switch"))) {
        return(x)
      }
      for (el in as.list(x)[-1L]) {
        hit <- find(el)
        if (!is.null(hit)) {
          return(hit)
        }
      }
    }
    return(NULL)
  }
  sw <- find(body(fn))
  if (is.null(sw)) {
    return(character(0))
  }
  nms <- names(as.list(sw))
  return(sort(nms[nzchar(nms)]))
}

# The message of the error of `expr`, or NA when it does not stop.
sf_error <- function(expr) {
  return(tryCatch(
    {
      force(expr)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  ))
}


test_that("every form carries every field, under a unique id", {
  fields <- c(
    "id", "title", "type", "evaluator", "blocks", "source", "computes",
    "unobserved", "column", "yaml", "written", "case"
  )
  expect_false(anyDuplicated(form_ids) > 0L)
  for (f in forms) {
    expect_true(all(fields %in% names(f)), label = f[["id"]])
    expect_true(
      all(f[["blocks"]] %in% names(.TTE_SPEC_FORM_BLOCKS)),
      label = paste("blocks of", f[["id"]])
    )
    expect_length(f[["case"]][["expect"]], length(.TTE_SPEC_FORM_WEEKS))
    for (col in f[["case"]][["columns"]]) {
      expect_length(col, length(.TTE_SPEC_FORM_WEEKS))
    }
  }
})

test_that("the forms name exactly the rule types the reader dispatches on", {
  declared <- sort(unique(stats::na.omit(
    vapply(forms, `[[`, character(1), "type")
  )))
  expect_identical(
    declared,
    sort(unique(c(.TTE_INCLUSION_OUTER_TYPES, .TTE_WASHOUT_TYPES)))
  )
})

test_that("the forms name exactly the cases of the batch evaluator", {
  cases <- sf_switch_cases(.tte_apply_eligibility_batch)
  expect_true(length(cases) > 0L, label = "the evaluator holds a switch()")
  used <- sort(setdiff(
    unique(vapply(forms, `[[`, character(1), "evaluator")),
    "vectorised"
  ))
  expect_identical(used, cases)
})

test_that("every example writes its stated column, in every block it names", {
  for (f in forms) {
    for (block in f[["blocks"]]) {
      label <- paste0(f[["id"]], " in ", block)
      out <- .tte_spec_form_run(f, block)
      expect_identical(
        out[[f[["written"]]]],
        f[["case"]][["expect"]],
        label = label
      )
      if (!identical(block, "confounders")) {
        expect_true(
          f[["written"]] %in% attr(out, "eligible_cols"),
          label = paste(label, "enters the combined eligibility")
        )
      }
    }
  }
})

test_that("the side claims of the forms hold", {
  # has_event follows any(): an NA week in the window, and no TRUE week, gives
  # NA.
  f <- sf_form("has_event")
  f$case$columns$osd_f64 <- c(FALSE, NA, rep(FALSE, 6L))
  expect_identical(
    .tte_spec_form_run(f)[[f$written]],
    c(FALSE, FALSE, rep(NA, 6L))
  )

  # A computed confounder's own `variable` key is replaced.
  f <- sf_form("computed_confounder")
  f$yaml <- paste0(f$yaml, "  variable: my_name\n")
  out <- .tte_spec_form_run(f)
  expect_false("my_name" %in% names(out))

  # A list in a washout passes the reader and stops in the compiler.
  f <- sf_form("no_prior_value")
  f$yaml <- sub(
    "source_variable: rd_tx",
    "source_variable: [rd_tx, rd_tx2]",
    f$yaml,
    fixed = TRUE
  )
  f$case$columns$rd_tx2 <- rep("control", 8L)
  msg <- sf_error(.tte_spec_form_run(f, "exclusion_criteria"))
  expect_false(is.na(msg))
  expect_false(grepl("exclusion_criteria[", msg, fixed = TRUE))
})

test_that("every window form converts or is refused as the registry states", {
  for (w in .TTE_SPEC_WINDOW_FORMS) {
    label <- format(w[["written"]])
    got <- tryCatch(.convert_window(w[["written"]]), error = function(e) NULL)
    if (identical(w[["accepted"]], "refused")) {
      expect_null(got, label = label)
    } else if (is.na(w[["weeks"]])) {
      # The person-level window is not converted. The exclusion compiler
      # reads it by name; person_exclusion is its form.
      expect_identical(w[["written"]], "lifetime_before_and_after_baseline")
      expect_null(got, label = label)
    } else {
      expect_equal(got, w[["weeks"]], label = label)
      expect_identical(.window_label(got), w[["label"]], label = label)
    }
  }
})

test_that("the window defaults hold", {
  # An inclusion rule without a window reads lifetime_before_baseline.
  f <- sf_form("has_event")
  f$yaml <- sub("  window: lifetime_before_baseline\n", "", f$yaml, fixed = TRUE)
  expect_identical(
    .tte_spec_form_run(f)[[f$written]],
    f$case$expect
  )

  # An exclusion rule without a window is refused.
  f <- sf_form("event_exclusion")
  f$yaml <- sub("  window: 2\n", "", f$yaml, fixed = TRUE)
  expect_match(sf_error(.tte_spec_form_run(f)), "missing implementation$window",
    fixed = TRUE
  )

  # The person-level window is refused in an inclusion rule.
  f <- sf_form("has_event")
  f$yaml <- sub(
    "lifetime_before_baseline",
    "lifetime_before_and_after_baseline",
    f$yaml,
    fixed = TRUE
  )
  expect_false(is.na(sf_error(.tte_spec_form_run(f))))
})

test_that("the vignette renders the registry", {
  root <- val_package_root()
  expect_false(is.null(root), label = "a package root holding DESCRIPTION")
  rmd_path <- file.path(root, "vignettes", "tte-spec-schema.Rmd")
  expect_true(file.exists(rmd_path), label = rmd_path)
  rmd <- paste(readLines(rmd_path, warn = FALSE), collapse = "\n")
  for (sym in c(
    ".TTE_SPEC_RULE_FORMS",
    ".tte_spec_form_table",
    ".tte_spec_form_run",
    ".tte_spec_window_table",
    ".TTE_SPEC_WINDOW_DEFAULTS",
    ".TTE_SPEC_NOT_EXPRESSIBLE"
  )) {
    expect_true(
      grepl(paste0("swereg:::", sym), rmd, fixed = TRUE),
      label = paste("the vignette reads", sym)
    )
  }
})
