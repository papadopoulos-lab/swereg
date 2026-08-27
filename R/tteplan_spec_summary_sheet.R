# =============================================================================
# export_tables helpers (internal)
# =============================================================================

#' @noRd
.write_provenance <- function(wb, plan) {
  openxlsx::addWorksheet(wb, "Provenance")
  spec <- plan$spec
  impl <- if (!is.null(spec)) spec$study$implementation else NULL

  rows <- list()
  add <- function(item, value) {
    return(rows[[length(rows) + 1L]] <<- data.table::data.table(
      Item = item,
      Value = as.character(value)
    ))
  }

  # An absent timestamp prints as an empty cell. `format(NA, "%Y-%m-%d")` reads
  # the format string as the `trim` argument of `format.default()` and stops
  # with `invalid 'trim' argument`, so a plan built without a RegistryStudy
  # could not export at all.
  fmt_time <- function(x) {
    if (is.null(x) || length(x) == 0L || !inherits(x, c("POSIXct", "Date"))) {
      return(NA_character_)
    }
    return(format(x, "%Y-%m-%d %H:%M:%S"))
  }

  add("Exported at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  add("Project", plan$project_prefix)
  if (!is.null(spec)) {
    add("Study title", spec$study$title)
    add("Principal investigator", spec$study$principal_investigator)
    if (!is.null(impl$version)) {
      add("Spec version", impl$version)
    }
    if (!is.null(impl$date)) {
      add("Spec date", impl$date)
    }
    if (!is.null(impl$status)) add("Spec status", impl$status)
  }
  add("", "")
  add("RegistryStudy created", fmt_time(plan$registry_study_created_at))
  add("Skeletons created", fmt_time(plan$skeleton_created_at))
  add("TTEPlan created", fmt_time(plan$created_at))
  add("", "")
  add("Skeleton files", as.character(length(plan$skeleton_files)))
  n_exp <- plan$expected_skeleton_file_count
  if (!is.null(n_exp)) {
    add("Expected skeleton files", as.character(n_exp))
  }
  if (!is.null(plan$expected_n_ids)) {
    add("Individuals", format(plan$expected_n_ids, big.mark = ","))
  }
  add("Admin censoring (isoyearweek)", plan$global_max_isoyearweek)
  add("Period width (weeks)", as.character(plan$period_width))
  add("Enrollments", as.character(length(unique(plan$ett$enrollment_id))))
  add("ETTs", as.character(nrow(plan$ett)))
  add("", "")
  add("R version", R.version.string)
  add("swereg version", as.character(utils::packageVersion("swereg")))
  add("data.table version", as.character(utils::packageVersion("data.table")))

  if (!is.null(plan$spec_reloaded_at)) {
    add("", "")
    add("Spec reloaded at", format(plan$spec_reloaded_at, "%Y-%m-%d %H:%M:%S"))
    if (length(plan$spec_reload_skipped_diffs) > 0L) {
      add(
        "Spec reload - skipped (structural)",
        paste(plan$spec_reload_skipped_diffs, collapse = "; ")
      )
    }
  }

  dt <- data.table::rbindlist(rows)
  openxlsx::writeData(
    wb,
    "Provenance",
    dt,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold"
    )
  )
  return(openxlsx::setColWidths(wb, "Provenance", cols = 1:2, widths = c(30, 60)))
}

#' Build a code lookup environment and variable formatter from a plan's
#' code_registry.
#'
#' @param plan A TTEPlan object with an optional `code_registry` field.
#' @param colorize Logical. If TRUE, wrap variable/code strings in ANSI
#'   color escapes (for terminal). If FALSE, return plain text (for Excel).
#' @return A list with `lookup` (environment or NULL) and `fmt_var` (function).
#' @noRd
.build_code_lookup <- function(plan, colorize = FALSE) {
  code_lookup <- NULL
  st <- plan$code_registry
  if (!is.null(st) && nrow(st) > 0) {
    code_lookup <- new.env(parent = emptyenv())
    for (i in seq_len(nrow(st))) {
      cols <- strsplit(st$generated_columns[i], ", ")[[1]]
      for (col in cols) {
        code_lookup[[col]] <- paste0(st$codes[i], " (", st$label[i], ")")
      }
    }
  }

  # Resolve combined variable names (e.g., "osd_c__can_c")
  .resolve_combined <- function(var) {
    if (is.null(code_lookup)) {
      return(NULL)
    }
    parts <- strsplit(var, "__", fixed = TRUE)[[1]]
    if (length(parts) <= 1L) {
      return(NULL)
    }
    infos <- vapply(
      parts,
      function(p) {
        return(code_lookup[[p]] %||% p)
      },
      character(1)
    )
    return(paste(infos, collapse = " + "))
  }

  if (colorize) {
    cyan <- function(x) paste0("\033[36m", x, "\033[0m")
    magenta <- function(x) paste0("\033[95m", x, "\033[0m")
    green <- function(x) paste0("\033[92m", x, "\033[0m")
    fmt_one <- function(v) {
      if (is.null(code_lookup)) {
        return(v)
      }
      info <- code_lookup[[v]]
      if (is.null(info)) {
        info <- .resolve_combined(v)
      }
      if (!is.null(info)) {
        return(paste0(cyan(v), " <- ", magenta(info)))
      } else {
        return(green(v))
      }
    }
    fmt_var <- function(var) {
      return(paste(vapply(var, fmt_one, character(1)), collapse = " + "))
    }
  } else {
    fmt_one <- function(v) {
      if (is.null(code_lookup)) {
        return(v)
      }
      info <- code_lookup[[v]]
      if (is.null(info)) {
        info <- .resolve_combined(v)
      }
      if (!is.null(info)) return(paste0(v, " <- ", info)) else return(v)
    }
    fmt_var <- function(var) {
      return(paste(vapply(var, fmt_one, character(1)), collapse = " + "))
    }
  }

  return(list(lookup = code_lookup, fmt_var = fmt_var))
}

#' @noRd
.write_spec_summary <- function(wb, plan) {
  sht <- "Study Specification"
  openxlsx::addWorksheet(wb, sht)
  spec <- plan$spec
  if (is.null(spec)) {
    openxlsx::writeData(wb, sht, "No spec available.")
    return(invisible(NULL))
  }

  cl <- .build_code_lookup(plan, colorize = FALSE)
  fmt_var <- cl$fmt_var

  # -- styles (matching console ANSI colours) --------------------------------
  # -- code lookup helpers ---------------------------------------------------
  code_lookup <- cl$lookup
  .resolve_combined <- function(var) {
    if (is.null(code_lookup)) {
      return(NULL)
    }
    parts <- strsplit(var, "__", fixed = TRUE)[[1]]
    if (length(parts) <= 1L) {
      return(NULL)
    }
    infos <- vapply(
      parts,
      function(p) {
        return(code_lookup[[p]] %||% p)
      },
      character(1)
    )
    return(paste(infos, collapse = " + "))
  }
  resolve_one <- function(v) {
    if (is.null(code_lookup)) {
      return(list(var = v, codes = NA_character_))
    }
    info <- code_lookup[[v]]
    if (is.null(info)) {
      combined <- .resolve_combined(v)
      if (!is.null(combined)) {
        return(list(var = v, codes = combined))
      }
      return(list(var = v, codes = NA_character_))
    }
    return(list(var = v, codes = info))
  }
  # -- styles (matching console ANSI colours) --------------------------------
  st_header <- openxlsx::createStyle(textDecoration = "bold", fontSize = 13)
  st_item <- openxlsx::createStyle(textDecoration = "bold", indent = 1)
  st_sub_item <- openxlsx::createStyle(textDecoration = "bold", indent = 3)
  st_label <- openxlsx::createStyle(indent = 3)
  st_sub_label <- openxlsx::createStyle(indent = 5)
  st_cyan <- openxlsx::createStyle(fontColour = "#008B8B")
  st_magenta <- openxlsx::createStyle(fontColour = "#8B008B")
  st_green <- openxlsx::createStyle(fontColour = "#006400")
  st_yellow <- openxlsx::createStyle(fontColour = "#B8860B")
  st_codes <- openxlsx::createStyle(fontColour = "#8B008B", indent = 5)
  # Inclusion (green) / exclusion (red) col-A styles
  st_incl_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 1,
    fontColour = "#006400"
  )
  st_incl_sub_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 3,
    fontColour = "#006400"
  )
  st_incl_label <- openxlsx::createStyle(indent = 3, fontColour = "#006400")
  st_incl_sub_label <- openxlsx::createStyle(indent = 5, fontColour = "#006400")
  st_excl_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 1,
    fontColour = "#8B0000"
  )
  st_excl_sub_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 3,
    fontColour = "#8B0000"
  )
  st_excl_label <- openxlsx::createStyle(indent = 3, fontColour = "#8B0000")
  st_excl_sub_label <- openxlsx::createStyle(indent = 5, fontColour = "#8B0000")
  # Sub-sub level (one indent deeper): used for named criteria nested under
  # additional_inclusion / additional_exclusion section headers, and their
  # child key-value rows. Without this level the criterion names render at
  # the same indent as their parent header.
  st_sub_sub_item <- openxlsx::createStyle(textDecoration = "bold", indent = 5)
  st_sub_sub_label <- openxlsx::createStyle(indent = 7)
  st_incl_sub_sub_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 5,
    fontColour = "#006400"
  )
  st_incl_sub_sub_label <- openxlsx::createStyle(
    indent = 7,
    fontColour = "#006400"
  )
  st_excl_sub_sub_item <- openxlsx::createStyle(
    textDecoration = "bold",
    indent = 5,
    fontColour = "#8B0000"
  )
  st_excl_sub_sub_label <- openxlsx::createStyle(
    indent = 7,
    fontColour = "#8B0000"
  )

  # -- accumulator (2 columns: a=label, b=value) ----------------------------
  rows <- list()
  r <- 0L

  # tint: NULL (default), "incl" (green), "excl" (red)
  # sub_sub overrides sub. sub_sub = TRUE → indent 7. sub = TRUE → indent 5.
  # Neither → indent 3.
  pick_sa <- function(sub, tint, sub_sub = FALSE) {
    if (sub_sub) {
      if (identical(tint, "incl")) {
        return(st_incl_sub_sub_label)
      } else if (identical(tint, "excl")) {
        return(st_excl_sub_sub_label)
      } else {
        return(st_sub_sub_label)
      }
    } else if (identical(tint, "incl")) {
      if (sub) return(st_incl_sub_label) else return(st_incl_label)
    } else if (identical(tint, "excl")) {
      if (sub) return(st_excl_sub_label) else return(st_excl_label)
    } else {
      if (sub) return(st_sub_label) else return(st_label)
    }
  }

  add_header <- function(text) {
    r <<- r + 1L
    return(rows[[r]] <<- list(a = text, b = NA_character_, sa = st_header, sb = NULL))
  }
  add_item <- function(text, tint = NULL) {
    sa <- if (identical(tint, "incl")) {
      st_incl_item
    } else if (identical(tint, "excl")) {
      st_excl_item
    } else {
      st_item
    }
    r <<- r + 1L
    return(rows[[r]] <<- list(a = text, b = NA_character_, sa = sa, sb = NULL))
  }
  add_sub_item <- function(text, tint = NULL) {
    sa <- if (identical(tint, "incl")) {
      st_incl_sub_item
    } else if (identical(tint, "excl")) {
      st_excl_sub_item
    } else {
      st_sub_item
    }
    r <<- r + 1L
    return(rows[[r]] <<- list(a = text, b = NA_character_, sa = sa, sb = NULL))
  }
  # Bold criterion-name row one indent deeper than add_sub_item. Optionally
  # carries an inline value in column B (used for "Age range: 54 - 60" style
  # rows where the criterion has no further child key-value pairs).
  add_sub_sub_item <- function(text, value = NA_character_, tint = NULL) {
    sa <- if (identical(tint, "incl")) {
      st_incl_sub_sub_item
    } else if (identical(tint, "excl")) {
      st_excl_sub_sub_item
    } else {
      st_sub_sub_item
    }
    r <<- r + 1L
    return(rows[[r]] <<- list(a = text, b = value, sa = sa, sb = NULL))
  }
  add_blank <- function() {
    r <<- r + 1L
    return(rows[[r]] <<- list(a = "", b = NA_character_, sa = NULL, sb = NULL))
  }
  add_kv <- function(label, value, sub = FALSE, sub_sub = FALSE, tint = NULL) {
    r <<- r + 1L
    return(rows[[r]] <<- list(
      a = label,
      b = value,
      sa = pick_sa(sub, tint, sub_sub),
      sb = NULL
    ))
  }
  add_yellow <- function(
    label,
    value,
    sub = FALSE,
    sub_sub = FALSE,
    tint = NULL
  ) {
    r <<- r + 1L
    return(rows[[r]] <<- list(
      a = label,
      b = value,
      sa = pick_sa(sub, tint, sub_sub),
      sb = st_yellow
    ))
  }
  add_var <- function(label, var, sub = FALSE, sub_sub = FALSE, tint = NULL) {
    # First row gets the label
    p1 <- resolve_one(var[1])
    has_codes <- !is.na(p1$codes)
    r <<- r + 1L
    rows[[r]] <<- list(
      a = label,
      b = p1$var,
      sa = pick_sa(sub, tint, sub_sub),
      sb = if (has_codes) st_cyan else st_green
    )
    if (has_codes) {
      r <<- r + 1L
      rows[[r]] <<- list(
        a = NA_character_,
        b = paste0("\u21b3 ", p1$codes),
        sa = NULL,
        sb = st_codes
      )
    }
    # Remaining vars on their own rows
    if (length(var) > 1L) {
      for (v in var[-1L]) {
        pv <- resolve_one(v)
        hc <- !is.na(pv$codes)
        r <<- r + 1L
        rows[[r]] <<- list(
          a = NA_character_,
          b = pv$var,
          sa = NULL,
          sb = if (hc) st_cyan else st_green
        )
        if (hc) {
          r <<- r + 1L
          rows[[r]] <<- list(
            a = NA_character_,
            b = paste0("\u21b3 ", pv$codes),
            sa = NULL,
            sb = st_codes
          )
        }
      }
      return(invisible(NULL))
    }
  }
  add_derived_var <- function(
    label,
    derived,
    source_var,
    sub = FALSE,
    sub_sub = FALSE,
    tint = NULL
  ) {
    # First source var with "derived <- var" on the label row
    p1 <- resolve_one(source_var[1])
    has_codes <- !is.na(p1$codes)
    r <<- r + 1L
    rows[[r]] <<- list(
      a = label,
      b = paste0(derived, " <- ", p1$var),
      sa = pick_sa(sub, tint, sub_sub),
      sb = if (has_codes) st_cyan else st_green
    )
    if (has_codes) {
      r <<- r + 1L
      rows[[r]] <<- list(
        a = NA_character_,
        b = paste0("\u21b3 ", p1$codes),
        sa = NULL,
        sb = st_codes
      )
    }
    # Remaining source vars on their own rows
    if (length(source_var) > 1L) {
      for (v in source_var[-1L]) {
        pv <- resolve_one(v)
        hc <- !is.na(pv$codes)
        r <<- r + 1L
        rows[[r]] <<- list(
          a = NA_character_,
          b = pv$var,
          sa = NULL,
          sb = if (hc) st_cyan else st_green
        )
        if (hc) {
          r <<- r + 1L
          rows[[r]] <<- list(
            a = NA_character_,
            b = paste0("\u21b3 ", pv$codes),
            sa = NULL,
            sb = st_codes
          )
        }
      }
      return(invisible(NULL))
    }
  }

  # -- Colour legend --------------------------------------------------------
  add_row <- function(a, b, sa, sb) {
    r <<- r + 1L
    return(rows[[r]] <<- list(a = a, b = b, sa = sa, sb = sb))
  }
  add_header("Colour legend")
  add_row("Variable name (resolved)", "e.g. osd_f64", NULL, st_cyan)
  add_row(
    "Code annotation",
    paste0("\u21b3 F64 (swereg::add_diagnoses)"),
    NULL,
    st_codes
  )
  add_row(
    "Variable name (unresolved)",
    "e.g. rd_age_continuous",
    NULL,
    st_green
  )
  add_row("Categories / arm values", "e.g. drug_a", NULL, st_yellow)
  add_row("Inclusion criterion", NA_character_, st_incl_item, NULL)
  add_row("Exclusion criterion", NA_character_, st_excl_item, NULL)
  add_blank()

  # -- Study ----------------------------------------------------------------
  add_header("Study")
  add_kv("Title:", spec$study$title)
  add_kv("PI:", spec$study$principal_investigator)
  if (!is.null(spec$study$design)) {
    add_kv("Design:", spec$study$design)
  }
  impl <- spec$study$implementation
  if (!is.null(impl$version)) {
    add_kv("Version:", impl$version)
  }
  if (!is.null(plan$global_max_isoyearweek)) {
    add_kv("Admin censoring:", plan$global_max_isoyearweek)
  }
  add_blank()

  # -- Follow-up ------------------------------------------------------------
  add_header("Follow-up")
  for (fu in spec$follow_up) {
    add_kv(fu$label, paste0(fu$weeks, " weeks"))
  }
  add_blank()

  # -- Inclusion criteria ---------------------------------------------------
  add_header("Inclusion criteria (global)")
  iso <- spec$inclusion_criteria$isoyears
  add_kv("Isoyears:", paste0(iso[1], " - ", iso[2]), tint = "incl")
  add_blank()

  # -- Exclusion criteria ---------------------------------------------------
  add_header("Exclusion criteria (global)")
  for (ec in spec$exclusion_criteria) {
    add_item(ec$name, tint = "excl")
    add_var(
      "Variable:",
      ec$implementation$source_variable_combined %||%
        ec$implementation$source_variable,
      tint = "excl"
    )
    add_kv("Window:", .format_window_human(ec$implementation), tint = "excl")
  }
  add_blank()

  # -- Confounders ----------------------------------------------------------
  add_header("Confounders")
  # Surface standing_methods.calendar_time as the first confounder entry: it
  # IS a confounder, but one that swereg auto-adjusts for via the IPW/IPCW
  # models. Showing it here so readers don't keep asking "what about calendar
  # year?" on every protocol review.
  sm_ct <- spec$standing_methods$calendar_time
  if (!is.null(sm_ct) && identical(sm_ct$handling, "auto-adjusted")) {
    add_item("Calendar time at trial registration")
    add_kv(
      "Handling:",
      sm_ct$note %||%
        "auto-adjusted by swereg (IPW/IPCW models); no explicit covariate needed"
    )
  }
  for (conf in spec$confounders) {
    cimpl <- conf$implementation
    add_item(conf$name)
    if (isTRUE(cimpl$computed)) {
      sv_display <- cimpl$source_variable_combined %||% cimpl$source_variable
      derived <- cimpl$variable %||% sv_display
      add_derived_var("Variable:", derived, sv_display)
      add_kv("Window:", .format_window_human(cimpl))
    } else {
      add_var("Variable:", cimpl$variable)
    }
    if (!is.null(conf$categories)) {
      add_yellow("Categories:", paste(conf$categories, collapse = ", "))
    }
  }
  add_blank()

  # -- Outcomes -------------------------------------------------------------
  add_header("Outcomes")
  for (out in spec$outcomes) {
    add_item(out$name)
    add_var("Variable:", out$implementation$variable)
  }
  add_blank()

  # -- Enrollments ----------------------------------------------------------
  add_header("Enrollments")
  for (enr in spec$enrollments) {
    add_item(paste0(enr$id, ": ", enr$name))

    # Treatment
    add_sub_item("Treatment:")
    tx <- enr$treatment
    add_var("Variable:", tx$implementation$variable, sub = TRUE)
    add_yellow(
      "Intervention:",
      paste0(
        tx$arms$intervention,
        " <- ",
        tx$implementation$intervention_value
      ),
      sub = TRUE
    )
    add_yellow(
      "Comparator:",
      paste0(tx$arms$comparator, " <- ", tx$implementation$comparator_value),
      sub = TRUE
    )
    add_kv(
      "Comparator-to-intervention ratio:",
      paste0(tx$implementation$comparator_to_intervention_ratio, ":1"),
      sub = TRUE
    )

    # Additional inclusion
    # Each named criterion (age_range, has_event, ...) is rendered one indent
    # deeper than its parent "Additional inclusion:" header. Child key-value
    # rows (Variable/Window) drop another indent further so the tree reads
    # cleanly.
    if (!is.null(enr$additional_inclusion)) {
      add_sub_item("Additional inclusion:", tint = "incl")
      for (ai in enr$additional_inclusion) {
        if (identical(ai$type, "age_range")) {
          add_sub_sub_item(
            "Age range:",
            paste0(ai$min, " - ", ai$max),
            tint = "incl"
          )
        } else if (identical(ai$type, "has_event")) {
          add_sub_sub_item(ai$name, tint = "incl")
          add_var(
            "Variable:",
            ai$implementation$source_variable_combined %||%
              ai$implementation$source_variable,
            sub_sub = TRUE,
            tint = "incl"
          )
          add_kv(
            "Window:",
            .format_window_human(ai$implementation),
            sub_sub = TRUE,
            tint = "incl"
          )
        } else {
          add_sub_sub_item(ai$name, tint = "incl")
        }
      }
    }

    # Additional exclusion (same indent rule as additional_inclusion)
    if (!is.null(enr$additional_exclusion)) {
      add_sub_item("Additional exclusion:", tint = "excl")
      for (ae in enr$additional_exclusion) {
        add_sub_sub_item(ae$name, tint = "excl")
        add_var(
          "Variable:",
          ae$implementation$source_variable_combined %||%
            ae$implementation$source_variable,
          sub_sub = TRUE,
          tint = "excl"
        )
        add_kv(
          "Window:",
          .format_window_human(ae$implementation),
          sub_sub = TRUE,
          tint = "excl"
        )
      }
    }
  }

  # -- write to sheet -------------------------------------------------------
  col_a <- vapply(rows, function(x) x$a %||% NA_character_, character(1))
  col_b <- vapply(rows, function(x) x$b %||% NA_character_, character(1))
  dt <- data.table::data.table(` ` = col_a, `  ` = col_b)
  openxlsx::writeData(wb, sht, dt, colNames = FALSE)

  for (i in seq_along(rows)) {
    rw <- rows[[i]]
    if (!is.null(rw$sa)) {
      openxlsx::addStyle(wb, sht, rw$sa, rows = i, cols = 1L)
    }
    if (!is.null(rw$sb)) openxlsx::addStyle(wb, sht, rw$sb, rows = i, cols = 2L)
  }
  return(openxlsx::setColWidths(wb, sht, cols = 1:2, widths = c(35, 70)))
}
