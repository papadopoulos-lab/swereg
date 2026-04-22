# =============================================================================
# CONSORT participant-flow rendering (sidecar-only)
# =============================================================================
# `.render_consort_sidecars()` builds a Graphviz DOT diagram via DiagrammeR
# + DiagrammeRsvg + rsvg and writes a high-resolution PNG plus a vector PDF
# next to the workbook. No worksheet is inserted - the rendered CONSORT
# diagrams live only as standalone image files (so reviewers open them in
# a viewer, not in Excel).
# =============================================================================

#' Collapse a long-format attrition table (one row per trial_id + criterion,
#' plus one global row per criterion with `trial_id = NA`) to one row per
#' criterion, preserving original criterion order.
#'
#' For each criterion, prefers the NA-trial_id rows (true overall
#' `uniqueN(persons)`) when present, and sums across per-trial rows
#' otherwise. The per-trial fallback over-counts `n_persons` for anyone
#' who enters more than one sequential trial; legacy attrition files
#' predating the global-row change trigger this path, so the inflated
#' number is better than no number at all.
#'
#' @noRd
.attrition_overall <- function(att) {
  n_persons <- n_person_trials <- n_intervention <- n_comparator <-
    criterion <- trial_id <- NULL
  if (is.null(att) || nrow(att) == 0L) return(NULL)

  att <- data.table::copy(att)
  att[, criterion := as.character(criterion)]
  # Preserve first-appearance order of criteria (pipeline application
  # order); data.table grouping by `criterion` re-sorts alphabetically,
  # which would scramble the CONSORT steps.
  crit_order <- unique(att$criterion)

  # All-or-nothing: use NA-trial_id rows only when every criterion has
  # at least one, so the `n_persons` column reports consistent units
  # across rows. Legacy attrition files (pre-global-row) have NA rows
  # for some criteria but not others; mixing would produce negative
  # CONSORT deltas where a per-trial sum immediately follows a uniqueN
  # count.
  has_na_per_crit <- att[, any(is.na(trial_id)), by = criterion]
  use_global <- nrow(has_na_per_crit) > 0L && all(has_na_per_crit$V1)

  src <- if (use_global) att[is.na(trial_id)] else att
  overall <- src[, .(
    n_persons = sum(n_persons),
    n_person_trials = sum(n_person_trials),
    n_intervention = sum(n_intervention),
    n_comparator = sum(n_comparator)
  ), by = criterion]
  overall <- overall[match(crit_order, criterion)]
  overall
}


#' Build a Graphviz DOT string for one enrollment's CONSORT flow.
#'
#' Uses the cached `enrollment_counts$attrition` table (criterion /
#' n_persons / n_person_trials / n_intervention / n_comparator) and
#' optional `matching` table to construct a vertical flow:
#'
#'   - Starting cohort box (`before_exclusions`) showing total persons and
#'     person-trials.
#'   - One lumped red side-box listing every exclusion criterion as a
#'     bullet, with (persons / person-trials) per bullet.
#'   - Eligible-cohort box showing final persons, person-trials, and
#'     per-arm person-trial breakdown.
#'   - Optional post-matching terminal box (blue) when `ec$matching` is
#'     present.
#'
#' The dual-count display (persons vs. person-trials) matters for
#' sequential target-trial emulation: one person enters many weekly
#' trials, so person-trial counts can look ~60x larger than the underlying
#' participant pool. Showing both numbers makes that explicit.
#'
#' @noRd
.build_consort_dot <- function(ec, eid, label,
                               intervention_label, comparator_label,
                               n_step_label = "n",
                               box_width = 3.6,
                               criterion_labels = character()) {
  n_persons <- n_person_trials <- n_intervention <- n_comparator <- criterion <- NULL  # nolint
  att <- ec$attrition
  if (is.null(att) || nrow(att) == 0L) return(NULL)
  overall <- .attrition_overall(att)
  if (is.null(overall)) return(NULL)

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
    split_label(label), esc(eid)
  )
  add("  title -> n1 [style = invis];")

  # n1: starting cohort (before exclusions).
  first <- overall[1L]
  add(
    "  n1 [label = '%s\\n%s persons\\n%s person-trials'];",
    display_crit(as.character(first$criterion)),
    fmt(first$n_persons), fmt(first$n_person_trials)
  )
  prev_node <- "n1"

  # Lump every subsequent criterion into one red bullet-list box. This
  # follows the CONSORT-2010 published convention (one "Excluded (n=...)"
  # box with bulleted reasons) instead of stacking a separate red box per
  # criterion.
  if (nrow(overall) > 1L) {
    bullet_lines <- character()
    for (j in 2:nrow(overall)) {
      d_persons <- overall$n_persons[j - 1L] - overall$n_persons[j]
      d_pt <- overall$n_person_trials[j - 1L] - overall$n_person_trials[j]
      crit <- as.character(overall$criterion[j])
      bullet_lines <- c(bullet_lines, sprintf(
        "- %s (n = %s persons / %s person-trials)",
        display_crit_inline(crit),
        fmt(d_persons), fmt(d_pt)
      ))
    }
    total_d_persons <- overall$n_persons[1L] -
      overall$n_persons[nrow(overall)]
    total_d_pt <- overall$n_person_trials[1L] -
      overall$n_person_trials[nrow(overall)]
    # `\l` = left-justified newline in Graphviz; using it inside the
    # bullet list left-aligns every bullet instead of centring each line.
    bullet_body <- paste(bullet_lines, collapse = "\\l")
    excl_label <- sprintf(
      "Excluded (n = %s persons / %s person-trials):\\l%s\\l",
      fmt(total_d_persons), fmt(total_d_pt), bullet_body
    )
    add(
      "  e1 [label = '%s', style = filled, fillcolor = '#FDEAEA', width = %.1f];",
      excl_label, box_width * 1.4
    )

    # n2: eligible cohort (final row after all exclusions applied).
    # `n_intervention` / `n_comparator` in the attrition table are
    # person-trial counts (per-person arm assignment is not meaningful
    # without a trial entry), so we surface them as person-trials here.
    last <- overall[nrow(overall)]
    add(
      "  n2 [label = 'Eligible cohort\\n%s persons\\n%s person-trials\\n(%s: %s person-trials, %s: %s person-trials)'];",
      fmt(last$n_persons), fmt(last$n_person_trials),
      int_lbl, fmt(last$n_intervention),
      cmp_lbl, fmt(last$n_comparator)
    )

    add("  n1 -> e1 [constraint = false];")
    add("  {rank = same; n1; e1}")
    add("  n1 -> n2;")
    prev_node <- "n2"
  }

  if (!is.null(ec$matching)) {
    m <- ec$matching
    n_int <- sum(m$n_intervention_enrolled, na.rm = TRUE)
    n_cmp <- sum(m$n_comparator_enrolled, na.rm = TRUE)
    add(
      "  matched [label = 'Enrolled after matching\\n%s person-trials\\n(%s: %s person-trials, %s: %s person-trials)', style = filled, fillcolor = '#E8F4FD'];",
      fmt(n_int + n_cmp), int_lbl, fmt(n_int), cmp_lbl, fmt(n_cmp)
    )
    add("  %s -> matched;", prev_node)
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
  if (is.null(w)) return(NA_character_)
  if (is.character(w)) {
    w_num <- suppressWarnings(as.numeric(w))
    if (is.na(w_num)) return(NA_character_)
    w <- w_num
  }
  if (!is.numeric(w) || is.na(w)) return(NA_character_)
  if (is.infinite(w)) return("ever before baseline")
  w_int <- as.integer(w)
  sprintf("%d weeks before baseline", w_int)
}


.build_criterion_label_lookup <- function(plan, enrollment_id,
                                          observed_criteria = character()) {
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
      if (isTRUE(e$id == enrollment_id)) { enr <- e; break }
    }
    if (!is.null(enr) && !is.null(enr$additional_inclusion)) {
      for (ai in enr$additional_inclusion) {
        if (identical(ai$type, "age_range") &&
            !is.null(ai$min) && !is.null(ai$max)) {
          age_range <- sprintf("%s - %s years", ai$min, ai$max)
          break
        }
      }
    }
  }

  fmt_line <- function(name, window_line) {
    if (is.na(window_line) || !nzchar(window_line)) return(name)
    paste0(name, "\\n(", window_line, ")")
  }

  labels <- c(
    before_exclusions       = "Before exclusions",
    eligible_isoyears       = fmt_line("Outside of study years", isoyear_range),
    eligible_valid_treatment = "Has invalid treatment",
    eligible_age            = fmt_line("Outside of age range", age_range)
  )
  if (is.null(spec)) return(labels)

  # Collect spec criterion specs in pipeline order.
  ec_specs <- list()
  if (!is.null(spec$exclusion_criteria)) {
    for (ec in spec$exclusion_criteria) ec_specs <- c(ec_specs, list(ec))
  }
  enr <- NULL
  for (e in (spec$enrollments %||% list())) {
    if (isTRUE(e$id == enrollment_id)) { enr <- e; break }
  }
  if (!is.null(enr) && !is.null(enr$additional_exclusion)) {
    for (ec in enr$additional_exclusion) ec_specs <- c(ec_specs, list(ec))
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
    if (is.null(impl)) next
    sv <- impl$source_variable_combined %||% {
      sv0 <- impl$source_variable
      if (is.list(sv0)) sv0 <- unlist(sv0)
      if (length(sv0) > 1L) paste(sv0, collapse = "__") else sv0
    }
    if (is.null(sv) || !nzchar(sv)) next
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
    if (crit %in% names(labels)) next
    if (!startsWith(crit, "eligible_no_")) next

    crit_stripped <- sub("^eligible_no_", "", crit)
    crit_stripped <- sub("_[0-9]+wk$", "", crit_stripped)
    crit_stripped <- sub("_everbefore$", "", crit_stripped)
    crit_stripped <- sub("_lifetime_before_and_after_baseline$", "",
                         crit_stripped)
    crit_stripped <- sub("_lifetime_before_baseline$", "", crit_stripped)
    crit_norm <- normalise(crit_stripped)

    matched <- NULL
    for (s in spec_cores) {
      if (identical(crit_norm, s$sv_norm) ||
          identical(crit_stripped, s$sv)) {
        matched <- s; break
      }
    }
    if (is.null(matched)) {
      for (s in spec_cores) {
        if (grepl(s$sv_norm, crit_norm, fixed = TRUE)) {
          matched <- s; break
        }
      }
    }
    if (!is.null(matched)) {
      labels[crit] <- fmt_line(matched$name, matched$window_line)
    }
  }

  labels
}


#' Render CONSORT sidecars (PNG + PDF) for one enrollment without touching
#' any workbook. Returns the sidecar paths (or NULL when rendering is not
#' possible).
#'
#' @noRd
.render_consort_sidecars <- function(plan, ec, eid, label,
                                     output_dir, img_basename = NULL) {
  ok <- all(vapply(
    c("DiagrammeR", "DiagrammeRsvg", "rsvg"),
    requireNamespace, logical(1), quietly = TRUE
  ))
  if (!ok) {
    warning(
      "CONSORT sidecars not written for enrollment ", eid,
      " - install DiagrammeR, DiagrammeRsvg, and rsvg."
    )
    return(NULL)
  }

  arms <- .lookup_arm_labels(plan$spec, eid)
  intervention_label <- if (!is.null(arms)) arms[["intervention"]] else "intervention"
  comparator_label <- if (!is.null(arms)) arms[["comparator"]] else "comparator"
  observed_crits <- if (!is.null(ec$attrition)) {
    unique(as.character(ec$attrition$criterion))
  } else {
    character()
  }
  criterion_labels <- .build_criterion_label_lookup(
    plan, eid, observed_criteria = observed_crits
  )

  dot <- tryCatch(
    .build_consort_dot(
      ec = ec, eid = eid, label = label,
      intervention_label = intervention_label,
      comparator_label = comparator_label,
      criterion_labels = criterion_labels
    ),
    error = function(e) {
      warning("CONSORT DOT build failed for enrollment ", eid, ": ",
              conditionMessage(e))
      NULL
    }
  )
  if (is.null(dot)) return(NULL)

  if (is.null(output_dir) || !nzchar(output_dir)) {
    warning("output_dir must be set to write CONSORT sidecars")
    return(NULL)
  }
  if (is.null(img_basename)) {
    img_basename <- sprintf("%s_consort_%s",
                            plan$project_prefix %||% "consort", eid)
  }
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  png_path <- file.path(output_dir, paste0(img_basename, ".png"))
  pdf_path <- file.path(output_dir, paste0(img_basename, ".pdf"))

  rendered <- tryCatch({
    g <- DiagrammeR::grViz(dot)
    svg <- DiagrammeRsvg::export_svg(g)
    rsvg::rsvg_png(charToRaw(svg), png_path, width = 1600)
    rsvg::rsvg_pdf(charToRaw(svg), pdf_path)
    TRUE
  }, error = function(e) {
    warning("CONSORT render failed for enrollment ", eid, ": ",
            conditionMessage(e))
    FALSE
  })
  if (!isTRUE(rendered)) return(NULL)

  invisible(list(png = png_path, pdf = pdf_path))
}


# (Legacy `.write_consort_flowchart()` and `.write_consort_text()` helpers
# were removed when the workbook stopped embedding CONSORT sheets.
# PNG/PDF sidecars are now produced by `.render_consort_sidecars()` above.)
