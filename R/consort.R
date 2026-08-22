# =============================================================================
# CONSORT participant-flow rendering (sidecar-only)
# =============================================================================
# `.render_consort_sidecars()` builds a Graphviz DOT diagram via DiagrammeR
# + DiagrammeRsvg + rsvg and writes a high-resolution PNG plus a vector PDF
# next to the workbook. No worksheet is inserted - the rendered CONSORT
# diagrams live only as standalone image files (so reviewers open them in
# a viewer, not in Excel).
#
# Those three packages are Suggests, not Imports: this file is their only
# consumer. The DOT -> SVG step runs viz.js (a JavaScript port of Graphviz)
# inside V8, which on Linux needs a system libnode-dev. Keeping them optional
# means a box without it still installs swereg.
#
# `.require_consort_stack()` guards every render path and stops when one of
# the three is absent. It stops rather than warns. The caller at
# R/tteplan_export.R returns the sidecar path whatever this file returns. A
# warning therefore left the caller naming a PNG that was never written.
# =============================================================================

#' Collapse a long-format attrition table (one row per trial_id + criterion,
#' plus one global row per criterion with `trial_id = NA`) to one row per
#' criterion, preserving original criterion order.
#'
#' ONE source set: the global rows. They hold the true overall
#' `uniqueN(persons)` for their criterion.
#'
#' Every criterion MUST carry a global row. This returns NULL when one
#' criterion does not. The caller then writes no attrition sheet and no
#' CONSORT diagram for that enrollment.
#'
#' This MUST NOT read both sets. A criterion with a global row and per-trial
#' rows would contribute both. The sum then counts every person and every
#' person-trial of that criterion twice.
#'
#' A per-trial fallback stood here and is removed. It summed the per-trial
#' rows of every criterion, which counts one person once per trial she enters.
#' Under a mixed input that sum sat one row away from a `uniqueN` count. The
#' CONSORT delta between the two rows could then be negative. A legacy
#' attrition file, written before the global rows existed, now produces no
#' diagram: a missing number is safer than a wrong one.
#'
#' @noRd
.attrition_overall <- function(att) {
  n_persons <- n_person_trials <- n_intervention <- n_comparator <-
    criterion <- trial_id <- NULL
  if (is.null(att) || nrow(att) == 0L) {
    return(NULL)
  }

  att <- data.table::copy(att)
  att[, criterion := as.character(criterion)]
  # Preserve first-appearance order of criteria (pipeline application
  # order); data.table grouping by `criterion` re-sorts alphabetically,
  # which would scramble the CONSORT steps.
  crit_order <- unique(att$criterion)

  # All or nothing. Every criterion needs a NA-trial_id row, so that the
  # `n_persons` column reports one unit across every row. A legacy attrition
  # file (pre-global-row) has a NA row for some criteria and not for others.
  # There is no source set that reports one unit for that input, so this
  # reports nothing.
  has_na_per_crit <- att[, any(is.na(trial_id)), by = criterion]
  if (nrow(has_na_per_crit) == 0L || !all(has_na_per_crit$V1)) {
    return(NULL)
  }

  # The global rows and NOTHING else. `att` on its own would add the global
  # rows to the per-trial rows for every criterion, which counts that
  # criterion twice.
  src <- att[is.na(trial_id)]
  overall <- src[,
    .(
      n_persons = sum(n_persons),
      n_person_trials = sum(n_person_trials),
      n_intervention = sum(n_intervention),
      n_comparator = sum(n_comparator)
    ),
    by = criterion
  ]
  overall <- overall[match(crit_order, criterion)]
  overall
}


#' Build the unified cohort-derivation flow for one enrollment.
#'
#' Single source of truth for participant flow. Both the CONSORT diagram
#' (`.build_consort_dot`) and the attrition worksheet
#' (`.write_attrition_sheet`) render from this one ordered table, so they
#' cannot disagree. Each step carries a `kind` telling renderers how to
#' display it:
#'   - `start`     : the before-exclusions cohort.
#'   - `exclusion` : an eligibility criterion (red box / "excluded" delta).
#'   - `selection` : the comparator draw (a sampling step; a comparator the
#'                   draw did not take is NOT "excluded", and persons are not
#'                   cleanly removed). The draw is incidence density sampling
#'                   inside one entry band, and it reads no other variable.
#'   - `analysis`  : the per-protocol analysis dataset (the enrolled
#'                   person-trials minus those censored in the first period
#'                   for protocol deviation or loss to follow-up). This is
#'                   analytic censoring handled by IPCW, NOT an eligibility
#'                   exclusion.
#'
#' Counts are remaining-after-step. `n_persons` is meaningful only for the
#' eligibility cascade. The comparator draw and the analysis step are
#' person-trial operations, so their `n_persons` (and the analysis per-arm
#' counts) are NA.
#'
#' @param ec Enrollment counts list with `$attrition` (required) and
#'   optional `$matching`. `$matching` holds the comparator-draw counts under
#'   its released name.
#' @param analysis_n Optional per-protocol analysis-set size after the
#'   comparator draw (`n_baseline`); appended as the terminal `analysis` step
#'   when > 0.
#' @param analysis_n_intervention,analysis_n_comparator Optional per-arm
#'   analysis-set counts (`n_baseline_intervention`/`n_baseline_comparator`)
#'   for the analysis step; NA when unavailable.
#' @return An ordered data.table (one row per step) with columns `step`,
#'   `kind`, `n_persons`, `n_person_trials`, `n_intervention`,
#'   `n_comparator`, `change_persons`, `change_person_trials`,
#'   `change_kind`. NULL when no attrition data is available, and NULL when a
#'   criterion carries no global row (see `.attrition_overall()`).
#' @noRd
.build_cohort_flow <- function(
  ec,
  analysis_n = NULL,
  analysis_n_intervention = NULL,
  analysis_n_comparator = NULL
) {
  n_persons <- n_person_trials <- n_intervention <- n_comparator <-
    criterion <- change_persons <- change_person_trials <- change_kind <-
      kind <- NULL # nolint
  if (is.null(ec) || is.null(ec$attrition) || nrow(ec$attrition) == 0L) {
    return(NULL)
  }
  overall <- .attrition_overall(ec$attrition)
  if (is.null(overall) || nrow(overall) == 0L) {
    return(NULL)
  }

  flow <- data.table::data.table(
    step = as.character(overall$criterion),
    kind = c("start", rep("exclusion", nrow(overall) - 1L)),
    n_persons = as.numeric(overall$n_persons),
    n_person_trials = as.numeric(overall$n_person_trials),
    n_intervention = as.numeric(overall$n_intervention),
    n_comparator = as.numeric(overall$n_comparator)
  )

  # The comparator draw: all intervention person-trials plus the drawn
  # comparator person-trials. Selection, not exclusion; n_persons is NA.
  if (!is.null(ec$matching)) {
    n_int <- sum(ec$matching$n_intervention_enrolled, na.rm = TRUE)
    n_cmp <- sum(ec$matching$n_comparator_enrolled, na.rm = TRUE)
    if ((n_int + n_cmp) > 0L) {
      flow <- rbind(
        flow,
        data.table::data.table(
          step = "enrolled_after_comparator_draw",
          kind = "selection",
          n_persons = NA_real_,
          n_person_trials = as.numeric(n_int + n_cmp),
          n_intervention = as.numeric(n_int),
          n_comparator = as.numeric(n_cmp)
        )
      )
    }
  }

  # Per-protocol analysis dataset (analytic censoring, handled by IPCW).
  if (!is.null(analysis_n) && is.numeric(analysis_n) && analysis_n > 0L) {
    flow <- rbind(
      flow,
      data.table::data.table(
        step = "analysis_dataset",
        kind = "analysis",
        n_persons = NA_real_,
        n_person_trials = as.numeric(analysis_n),
        n_intervention = if (is.null(analysis_n_intervention)) {
          NA_real_
        } else {
          as.numeric(analysis_n_intervention)
        },
        n_comparator = if (is.null(analysis_n_comparator)) {
          NA_real_
        } else {
          as.numeric(analysis_n_comparator)
        }
      )
    )
  }

  # Per-step reduction from the previous step's remaining counts.
  n_pt <- flow$n_person_trials
  n_p <- flow$n_persons
  flow[, change_person_trials := c(NA_real_, n_pt[-length(n_pt)] - n_pt[-1L])]
  flow[, change_persons := c(NA_real_, n_p[-length(n_p)] - n_p[-1L])]
  flow[,
    change_kind := data.table::fcase(
      kind == "exclusion" , "excluded"                    ,
      kind == "selection" , "not drawn (comparator draw)" ,
      kind == "analysis"  , "censored (per-protocol)"     ,
      default = NA_character_
    )
  ]
  flow[]
}


#' Build a Graphviz DOT string for one enrollment's CONSORT flow.
#'
#' Renders the unified cohort-derivation flow from `.build_cohort_flow()`
#' (the single source of truth shared with the attrition worksheet) into a
#' vertical diagram:
#'
#'   - Starting cohort box (`before_exclusions`) showing total persons and
#'     person-trials.
#'   - One lumped red side-box listing every exclusion criterion as a
#'     bullet, with (persons / person-trials) per bullet.
#'   - Eligible-cohort box showing final persons, person-trials, and
#'     per-arm person-trial breakdown.
#'   - Optional terminal box (blue) for the comparator draw, when
#'     `ec$matching` is present. Its second line names the sampling scheme
#'     and the stratum, from `period_width`.
#'
#' The dual-count display (persons vs. person-trials) matters for
#' sequential target-trial emulation: one person enters many weekly
#' trials, so person-trial counts can look ~60x larger than the underlying
#' participant pool. Showing both numbers makes that explicit.
#'
#' @param period_width Integer band width in weeks, or `NULL`. `NULL` drops
#'   the stratum line from the comparator-draw box.
#' @noRd
.build_consort_dot <- function(
  flow,
  eid,
  label,
  intervention_label,
  comparator_label,
  box_width = 3.6,
  criterion_labels = character(),
  period_width = NULL
) {
  kind <- NULL # nolint
  if (is.null(flow) || nrow(flow) == 0L) {
    return(NULL)
  }

  fmt <- function(x) format(x, big.mark = ",")
  esc <- function(s) {
    s <- gsub("'", "", s, fixed = TRUE)
    s <- gsub("\"", "", s, fixed = TRUE)
    s
  }
  display_crit <- function(k) {
    if (length(criterion_labels) > 0L && k %in% names(criterion_labels)) {
      esc(criterion_labels[[k]])
    } else {
      esc(k)
    }
  }
  # For bullet-list rendering: `criterion_labels` entries may contain a
  # literal "\n(window)" suffix (for box-label use). Flatten that onto a
  # single line for the bullet list by replacing the 2-char sequence
  # backslash-n with a space.
  display_crit_inline <- function(k) {
    gsub("\\n", " ", display_crit(k), fixed = TRUE)
  }
  # Split "Name (description)" at the first " (" into two lines so long
  # enrollment titles don't force the top node to blow out horizontally.
  split_label <- function(s) {
    s <- esc(s)
    idx <- regexpr(" \\(", s)
    if (idx[[1L]] > 0L) {
      name <- substr(s, 1L, idx[[1L]] - 1L)
      desc <- substr(s, idx[[1L]] + 1L, nchar(s))
      paste(c(name, desc), collapse = "\\n")
    } else {
      s
    }
  }

  int_lbl <- esc(intervention_label %||% "intervention")
  cmp_lbl <- esc(comparator_label %||% "comparator")

  lines <- character()
  add <- function(...) lines <<- c(lines, sprintf(...))

  add("digraph CONSORT_%s {", gsub("[^a-zA-Z0-9]", "_", eid))
  add("  rankdir = TB; splines = ortho; nodesep = 0.4; ranksep = 0.5;")
  add(
    "  node [shape = box, fontname = 'Helvetica', fontsize = 10, margin = '0.2,0.1', width = %.1f];",
    box_width
  )
  add("  edge [arrowsize = 0.7];")

  # Title
  add(
    "  title [label = '%s\\nEnrollment %s', shape = plaintext, fontsize = 13];",
    split_label(label),
    esc(eid)
  )
  add("  title -> n1 [style = invis];")

  # Eligibility cascade (start + exclusion steps from the flow).
  elig <- flow[kind %in% c("start", "exclusion")]
  first <- elig[1L]
  add(
    "  n1 [label = '%s\\n%s persons\\n%s person-trials'];",
    display_crit(as.character(first$step)),
    fmt(first$n_persons),
    fmt(first$n_person_trials)
  )
  prev_node <- "n1"

  # Lump every exclusion criterion into one red bullet-list box (CONSORT-2010
  # convention: one "Excluded (n=...)" box with bulleted reasons).
  if (nrow(elig) > 1L) {
    bullet_lines <- character()
    for (j in 2:nrow(elig)) {
      bullet_lines <- c(
        bullet_lines,
        sprintf(
          "- %s (n = %s persons / %s person-trials)",
          display_crit_inline(as.character(elig$step[j])),
          fmt(elig$change_persons[j]),
          fmt(elig$change_person_trials[j])
        )
      )
    }
    total_d_persons <- elig$n_persons[1L] - elig$n_persons[nrow(elig)]
    total_d_pt <- elig$n_person_trials[1L] - elig$n_person_trials[nrow(elig)]
    # `\l` = left-justified newline in Graphviz; using it inside the
    # bullet list left-aligns every bullet instead of centring each line.
    bullet_body <- paste(bullet_lines, collapse = "\\l")
    excl_label <- sprintf(
      "Excluded (n = %s persons / %s person-trials):\\l%s\\l",
      fmt(total_d_persons),
      fmt(total_d_pt),
      bullet_body
    )
    add(
      "  e1 [label = '%s', style = filled, fillcolor = '#FDEAEA', width = %.1f];",
      excl_label,
      box_width * 1.4
    )

    # n2: eligible cohort (final eligibility row). n_intervention /
    # n_comparator are person-trial counts, surfaced as person-trials.
    last <- elig[nrow(elig)]
    add(
      "  n2 [label = 'Eligible cohort\\n%s persons\\n%s person-trials\\n(%s: %s person-trials, %s: %s person-trials)'];",
      fmt(last$n_persons),
      fmt(last$n_person_trials),
      int_lbl,
      fmt(last$n_intervention),
      cmp_lbl,
      fmt(last$n_comparator)
    )

    add("  n1 -> e1 [constraint = false];")
    add("  {rank = same; n1; e1}")
    add("  n1 -> n2;")
    prev_node <- "n2"
  }

  # The comparator draw: distinct (non-red) selection box. The draw is
  # sampling, not exclusion. The box names the sampling scheme and the band it
  # stratifies on. Naming both stops a reader of the figure taking the draw
  # for matching on a covariate, or for matching on the week.
  sel <- flow[kind == "selection"]
  if (nrow(sel) > 0L) {
    s <- sel[1L]
    stratum <- if (is.null(period_width) || length(period_width) == 0L) {
      ""
    } else if (
      is.na(as.integer(period_width)[1]) ||
        as.integer(period_width)[1] <= 1L
    ) {
      "\\nincidence density sampling, stratified by the entry week"
    } else {
      sprintf(
        "\\nincidence density sampling, stratified by the %d-week entry band",
        as.integer(period_width)[1]
      )
    }
    add(
      "  drawn [label = 'Enrolled after the comparator draw%s\\n%s person-trials\\n(%s: %s person-trials, %s: %s person-trials)', style = filled, fillcolor = '#E8F4FD'];",
      stratum,
      fmt(s$n_person_trials),
      int_lbl,
      fmt(s$n_intervention),
      cmp_lbl,
      fmt(s$n_comparator)
    )
    add("  %s -> drawn;", prev_node)
    prev_node <- "drawn"
  }

  # Per-protocol analysis dataset: distinct (non-red) terminal box. First-
  # period censoring (protocol deviation or loss to follow-up) is analytic
  # censoring handled by IPCW, never part of the red "Excluded" box.
  ana <- flow[kind == "analysis"]
  if (nrow(ana) > 0L) {
    a <- ana[1L]
    # Show the per-arm split when the worker recorded it; otherwise total.
    ana_label <- if (!is.na(a$n_intervention) && !is.na(a$n_comparator)) {
      sprintf(
        "Analysis dataset (per-protocol)\\n%s person-trials\\n(%s: %s person-trials, %s: %s person-trials)",
        fmt(a$n_person_trials),
        int_lbl,
        fmt(a$n_intervention),
        cmp_lbl,
        fmt(a$n_comparator)
      )
    } else {
      sprintf(
        "Analysis dataset (per-protocol)\\n%s person-trials",
        fmt(a$n_person_trials)
      )
    }
    add(
      "  analysis [label = '%s', style = filled, fillcolor = '#EAF6EA'];",
      ana_label
    )
    add("  %s -> analysis;", prev_node)
  }

  add("}")
  paste(lines, collapse = "\n")
}


#' Build a lookup from internal eligibility column names (as they appear
#' in `ec$attrition$criterion`) to human-readable display labels taken
#' from the study spec.
#'
#' Matches each observed criterion name against the spec's
#' `exclusion_criteria` and the enrollment's `additional_exclusion` block
#' using a forgiving strategy: extract the "core" variable name from the
#' criterion column (the part between `eligible_no_` and the window
#' suffix) and compare against each spec criterion's core
#' `source_variable`. Normalisation strips an optional trailing `c` on
#' `osdc`-style prefixes so this works even when the cached plan uses
#' `osdc_*` and the current spec uses `osd_*`.
#'
#' @noRd
#' Render a spec `window` value as a short human-readable line suitable
#' for the second row of a CONSORT box.
#'
#' Accepts both the string forms (`"lifetime_before_baseline"`,
#' `"lifetime_before_and_after_baseline"`) and numeric weeks (52, 156, ...),
#' plus `Inf` for "ever before". Returns NA when no window info is
#' available.
#'
#' @noRd
.format_window_label <- function(window, window_weeks = NULL) {
  if (identical(window, "lifetime_before_and_after_baseline")) {
    return("lifetime before and after baseline")
  }
  if (identical(window, "lifetime_before_baseline")) {
    return("lifetime before baseline")
  }
  w <- window_weeks %||% window
  if (is.null(w)) {
    return(NA_character_)
  }
  if (is.character(w)) {
    w_num <- suppressWarnings(as.numeric(w))
    if (is.na(w_num)) {
      return(NA_character_)
    }
    w <- w_num
  }
  if (!is.numeric(w) || is.na(w)) {
    return(NA_character_)
  }
  if (is.infinite(w)) {
    return("ever before baseline")
  }
  w_int <- as.integer(w)
  sprintf("%d weeks before baseline", w_int)
}


.build_criterion_label_lookup <- function(
  plan,
  enrollment_id,
  observed_criteria = character()
) {
  spec <- plan$spec

  # Second-line suffixes for the fixed criteria. `eligible_isoyears` and
  # `eligible_age` take their window from the spec's inclusion config.
  isoyear_range <- NA_character_
  if (!is.null(spec) && !is.null(spec$inclusion_criteria$isoyears)) {
    iy <- spec$inclusion_criteria$isoyears
    if (length(iy) == 2L) {
      isoyear_range <- sprintf("%s - %s", iy[[1]], iy[[2]])
    }
  }
  age_range <- NA_character_
  if (!is.null(spec)) {
    enr <- NULL
    for (e in (spec$enrollments %||% list())) {
      if (isTRUE(e$id == enrollment_id)) {
        enr <- e
        break
      }
    }
    if (!is.null(enr) && !is.null(enr$additional_inclusion)) {
      for (ai in enr$additional_inclusion) {
        if (
          identical(ai$type, "age_range") &&
            !is.null(ai$min) &&
            !is.null(ai$max)
        ) {
          age_range <- sprintf("%s - %s years", ai$min, ai$max)
          break
        }
      }
    }
  }

  fmt_line <- function(name, window_line) {
    if (is.na(window_line) || !nzchar(window_line)) {
      return(name)
    }
    paste0(name, "\\n(", window_line, ")")
  }

  labels <- c(
    before_exclusions = "Before exclusions",
    eligible_isoyears = fmt_line("Outside of study years", isoyear_range),
    eligible_valid_treatment = "Has invalid treatment",
    eligible_age = fmt_line("Outside of age range", age_range)
  )
  if (is.null(spec)) {
    return(labels)
  }

  # Collect spec criterion specs in pipeline order.
  ec_specs <- list()
  if (!is.null(spec$exclusion_criteria)) {
    for (ec in spec$exclusion_criteria) {
      ec_specs <- c(ec_specs, list(ec))
    }
  }
  enr <- NULL
  for (e in (spec$enrollments %||% list())) {
    if (isTRUE(e$id == enrollment_id)) {
      enr <- e
      break
    }
  }
  if (!is.null(enr) && !is.null(enr$additional_exclusion)) {
    for (ec in enr$additional_exclusion) {
      ec_specs <- c(ec_specs, list(ec))
    }
  }

  # Normalise a string by dropping a common numeric-prefix marker ('c'
  # right before the first underscore, e.g. osdc_ -> osd_).
  normalise <- function(s) {
    s <- gsub("(^|_)([a-z]+)c_", "\\1\\2_", s)
    s
  }

  spec_cores <- list()
  for (ec in ec_specs) {
    impl <- ec$implementation
    if (is.null(impl)) {
      next
    }
    sv <- impl$source_variable_combined %||%
      {
        sv0 <- impl$source_variable
        if (is.list(sv0)) {
          sv0 <- unlist(sv0)
        }
        if (length(sv0) > 1L) paste(sv0, collapse = "__") else sv0
      }
    if (is.null(sv) || !nzchar(sv)) {
      next
    }
    window_line <- .format_window_label(
      window = impl$window,
      window_weeks = impl$window_weeks
    )
    spec_cores[[length(spec_cores) + 1L]] <- list(
      sv = sv,
      sv_norm = normalise(sv),
      name = ec$name %||% sv,
      window_line = window_line
    )
  }

  for (crit in unique(observed_criteria)) {
    if (crit %in% names(labels)) {
      next
    }
    if (!startsWith(crit, "eligible_no_")) {
      next
    }

    crit_stripped <- sub("^eligible_no_", "", crit)
    crit_stripped <- sub("_[0-9]+wk$", "", crit_stripped)
    crit_stripped <- sub("_everbefore$", "", crit_stripped)
    crit_stripped <- sub(
      "_lifetime_before_and_after_baseline$",
      "",
      crit_stripped
    )
    crit_stripped <- sub("_lifetime_before_baseline$", "", crit_stripped)
    crit_norm <- normalise(crit_stripped)

    matched <- NULL
    for (s in spec_cores) {
      if (
        identical(crit_norm, s$sv_norm) ||
          identical(crit_stripped, s$sv)
      ) {
        matched <- s
        break
      }
    }
    if (is.null(matched)) {
      for (s in spec_cores) {
        if (grepl(s$sv_norm, crit_norm, fixed = TRUE)) {
          matched <- s
          break
        }
      }
    }
    if (!is.null(matched)) {
      labels[crit] <- fmt_line(matched$name, matched$window_line)
    }
  }

  labels
}


#' The three optional CONSORT packages that this installation does not carry.
#'
#' Separate from `.require_consort_stack()` so a test can replace it. A test
#' cannot mock `requireNamespace()`, because `testthat::local_mocked_bindings()`
#' only replaces a binding the package namespace itself defines.
#'
#' @return A character vector, empty when all three packages are installed.
#' @noRd
.consort_stack_absent <- function() {
  pkgs <- c("DiagrammeR", "DiagrammeRsvg", "rsvg")
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}


#' Stop unless the optional CONSORT diagram stack is installed.
#'
#' `DiagrammeR`, `DiagrammeRsvg` and `rsvg` are Suggests, so a plain install
#' does not carry them. Every path that renders a CONSORT diagram calls this
#' first. The error names the packages that are absent, so a missing Suggests
#' package stops with install instructions. Without the guard the `::` call
#' raises `there is no package called ...` inside a `tryCatch()`. That demotes
#' the error to a warning. The caller then returns a path to a file that was
#' never written.
#'
#' @param eid Enrollment identifier. The error names it. `NULL` names none.
#' @return `invisible(TRUE)` when all three packages are installed.
#' @noRd
.require_consort_stack <- function(eid = NULL) {
  absent <- .consort_stack_absent()
  if (length(absent) == 0L) {
    return(invisible(TRUE))
  }
  stop(
    "CONSORT rendering needs the optional diagram stack. Not installed: ",
    paste(absent, collapse = ", "),
    if (is.null(eid)) "." else paste0(". Enrollment: ", eid, "."),
    ' Install with pak::pak(c("DiagrammeR", "DiagrammeRsvg", "rsvg")).',
    " On Linux DiagrammeRsvg needs V8, which needs the system libnode-dev.",
    " On Windows and macOS the CRAN binaries are self-contained.",
    call. = FALSE
  )
}


#' Render CONSORT sidecars (PNG + PDF) for one enrollment without touching
#' any workbook. Returns the sidecar paths (or NULL when rendering is not
#' possible).
#'
#' @noRd
.render_consort_sidecars <- function(
  plan,
  ec,
  eid,
  label,
  output_dir,
  img_basename = NULL
) {
  .require_consort_stack(eid)

  arms <- .lookup_arm_labels(plan$spec, eid)
  intervention_label <- if (!is.null(arms)) {
    arms[["intervention"]]
  } else {
    "intervention"
  }
  comparator_label <- if (!is.null(arms)) arms[["comparator"]] else "comparator"
  observed_crits <- if (!is.null(ec$attrition)) {
    unique(as.character(ec$attrition$criterion))
  } else {
    character()
  }
  criterion_labels <- .build_criterion_label_lookup(
    plan,
    eid,
    observed_criteria = observed_crits
  )

  # Per-protocol analysis-set size after the comparator draw (n_baseline),
  # read through
  # `$get_baselines()`. `NA` when the stage has not run.
  baselines <- tryCatch(plan$get_baselines(), error = function(e) NULL)
  analysis_n <- .baseline_count(baselines, eid, "n_baseline")
  # Single source of truth: the diagram and the attrition sheet both render
  # from this one flow.
  flow <- .build_cohort_flow(
    ec,
    analysis_n = if (is.na(analysis_n)) NULL else analysis_n,
    analysis_n_intervention = .baseline_count(
      baselines,
      eid,
      "n_baseline_intervention"
    ),
    analysis_n_comparator = .baseline_count(
      baselines,
      eid,
      "n_baseline_comparator"
    )
  )

  dot <- tryCatch(
    .build_consort_dot(
      flow = flow,
      eid = eid,
      label = label,
      intervention_label = intervention_label,
      comparator_label = comparator_label,
      criterion_labels = criterion_labels,
      period_width = plan$period_width
    ),
    error = function(e) {
      warning(
        "CONSORT DOT build failed for enrollment ",
        eid,
        ": ",
        conditionMessage(e)
      )
      NULL
    }
  )
  if (is.null(dot)) {
    return(NULL)
  }

  if (is.null(output_dir) || !nzchar(output_dir)) {
    warning("output_dir must be set to write CONSORT sidecars")
    return(NULL)
  }
  if (is.null(img_basename)) {
    img_basename <- sprintf(
      "%s_consort_%s",
      plan$project_prefix %||% "consort",
      eid
    )
  }
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  png_path <- file.path(output_dir, paste0(img_basename, ".png"))
  pdf_path <- file.path(output_dir, paste0(img_basename, ".pdf"))

  rendered <- tryCatch(
    {
      g <- DiagrammeR::grViz(dot)
      svg <- DiagrammeRsvg::export_svg(g)
      rsvg::rsvg_png(charToRaw(svg), png_path, width = 1600)
      rsvg::rsvg_pdf(charToRaw(svg), pdf_path)
      TRUE
    },
    error = function(e) {
      warning(
        "CONSORT render failed for enrollment ",
        eid,
        ": ",
        conditionMessage(e)
      )
      FALSE
    }
  )
  if (!isTRUE(rendered)) {
    return(NULL)
  }

  invisible(list(png = png_path, pdf = pdf_path))
}

# (Legacy `.write_consort_flowchart()` and `.write_consort_text()` helpers
# were removed when the workbook stopped embedding CONSORT sheets.
# PNG/PDF sidecars are now produced by `.render_consort_sidecars()` above.)
