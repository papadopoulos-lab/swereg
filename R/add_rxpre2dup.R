# PRE2DUP drug use periods on the skeleton: the design file reader and
# add_rxpre2dup(). The periods algorithm is .pre2dup_periods() in
# R/pre2dup_periods.R. The mapping of periods to skeleton rows is add_rx().

# The columns of the `packages` sheet of a design file, in order.
.RXPRE2DUP_DESIGN_COLUMNS <- c(
  "atc",
  "produkt",
  "lnmn",
  "pack_ddd",
  "form",
  "dur_min",
  "dur_usual",
  "dur_max",
  "status",
  "source",
  "note"
)

# The key that matches a dispensing to a design row compares lnmn in this
# form. The reader applies the same form to its duplicate-key check.
.rxpre2dup_lnmn_key <- function(lnmn) {
  return(tolower(trimws(lnmn)))
}

# Stops with the sheet rows of a design file that break one rule. `i` holds
# data row numbers; the sheet row is one more, because row 1 is the header.
.rxpre2dup_design_stop <- function(path, i, rule) {
  stop(
    "The design file ",
    path,
    " breaks a rule on sheet row ",
    paste(i + 1L, collapse = ", "),
    ": ",
    rule,
    ".",
    call. = FALSE
  )
}

# The data rows, among `rows`, whose cell in sheet column `j` holds no
# number. A column that mixes number cells and text cells reads as text, and
# there a text cell "12" reads like a number cell 12. Each cell is read
# alone here, where a number cell reads as a number. The reader skips no
# column, so `j` is the sheet column. This runs only before a refusal.
.rxpre2dup_text_cells <- function(path, j, rows) {
  not_number <- vapply(
    rows,
    function(i) {
      v <- suppressWarnings(openxlsx::read.xlsx(
        path,
        sheet = "packages",
        rows = i + 1L,
        cols = j,
        colNames = FALSE
      ))
      return(length(v) > 0L && !is.numeric(v[[1L]]))
    },
    logical(1L)
  )
  return(rows[not_number])
}

#' Read a PRE2DUP package design file
#'
#' Reads the `packages` sheet of a package design file and checks every row
#' against the design file format. [add_rxpre2dup()] matches LMED dispensings
#' to the rows it returns.
#'
#' @section Design file format:
#' The file is an `.xlsx` workbook with one sheet, `packages`, and one row per
#' package. The columns are, in order:
#' \itemize{
#'   \item `atc`: the full ATC code.
#'   \item `produkt`: the product name, as LMED writes it.
#'   \item `lnmn`: the product name and strength, matched after
#'     `tolower(trimws())`.
#'   \item `pack_ddd`: the DDDs that one dispensing of this package holds, as
#'     LMED's `fddd`. It MUST be greater than 0.
#'   \item `form`: the dose form, as free text, for example `"tablet"`.
#'   \item `dur_min`, `dur_usual`, `dur_max`: durations of one dispensing, in
#'     days. Each MUST be greater than 0, and
#'     `dur_min <= dur_usual <= dur_max` MUST hold.
#'   \item `status`: `"auto"` or `"approved"`.
#'   \item `source`, `note`: text. Both MAY be empty.
#' }
#' Every column except `source` and `note` MUST hold a value on every row.
#' Two rows with the same `atc`, `produkt` and normalised `lnmn` MUST have
#' values of `pack_ddd` that differ by 0.01 or more. One dispensing therefore
#' matches at most one row.
#'
#' The reader stops on the first rule that a row breaks, and names every sheet
#' row that breaks it. Row 1 of the sheet is the header. The reader ignores
#' every column that is not in the list above or in the section "Design
#' dates". It does not check the column order.
#'
#' @section Design dates:
#' The sheet MAY have one more column, `design_date`. The workbook can then
#' grow by appended rows. Each row MUST hold the date of its design, as text
#' in the form `YYYY-MM-DD`. An Excel date cell reads as a number, and the
#' reader refuses it.
#'
#' The reader first drops the rows with `design_date` after `as_of`. Then, for
#' each `atc`, it keeps only the rows with the latest remaining date of that
#' `atc`. An `atc` that a later date leaves out keeps the rows of its own
#' latest date. The reader stops when it keeps no row.
#'
#' The rules of the list above apply only to the rows that the reader keeps.
#' A dropped row therefore takes no part in the duplicate-key rule. A
#' refusal names the sheet rows of the file.
#'
#' @param path The path to the `.xlsx` design file.
#' @param as_of NULL, or one date that [base::as.Date()] reads. When it is
#'   not NULL, the reader drops every row with `design_date` after `as_of`.
#'   It MUST be NULL for a file without the column `design_date`.
#' @return A data.table with one row per package that the reader keeps. The
#'   first column is an integer `package_id`, 1 to N in sheet order. The 11
#'   columns of the format follow it. The next two columns are
#'   `ddd_usual = pack_ddd / dur_usual` and `ddd_low = pack_ddd / dur_max`.
#'   A file with `design_date` gives a 15th column, `design_date`, of class
#'   `Date`.
#' @seealso [add_rxpre2dup()]
#' @export
rxpre2dup_read_design <- function(path, as_of = NULL) {
  package_id <- ddd_usual <- ddd_low <- pack_ddd <- dur_usual <- NULL
  dur_max <- NULL
  if (!is.null(as_of)) {
    as_of_date <- tryCatch(as.Date(as_of), error = function(e) as.Date(NA))
    if (length(as_of) != 1L || length(as_of_date) != 1L || is.na(as_of_date)) {
      stop(
        "as_of MUST be NULL or one date that as.Date() reads.",
        call. = FALSE
      )
    }
  }
  sheet <- tryCatch(
    openxlsx::read.xlsx(
      path,
      sheet = "packages",
      skipEmptyRows = FALSE,
      skipEmptyCols = FALSE
    ),
    error = function(e) {
      stop(
        "Cannot read the sheet 'packages' of the design file ",
        path,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  # A column outside the format is ignored, and the column order is not
  # checked. Only a missing column refuses the file.
  miss <- setdiff(.RXPRE2DUP_DESIGN_COLUMNS, names(sheet))
  if (length(miss) > 0L) {
    stop(
      "The design file ",
      path,
      " is missing these columns: ",
      paste(miss, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  # The data rows that the date choice keeps. Every refusal below maps its
  # positions in d back to these rows, so it names the sheet rows.
  data_rows <- seq_len(nrow(sheet))
  has_date <- "design_date" %in% names(sheet)
  if (!has_date && !is.null(as_of)) {
    stop(
      "as_of MUST be NULL, because the design file ",
      path,
      " has no column design_date.",
      call. = FALSE
    )
  }
  if (has_date) {
    x <- sheet[["design_date"]]
    if (all(is.na(x))) {
      x <- rep(NA_character_, length(x))
    } else if (!is.character(x)) {
      .rxpre2dup_design_stop(
        path,
        which(!is.na(x)),
        "column design_date MUST hold text"
      )
    }
    dates <- as.Date(x, "%Y-%m-%d")
    bad <- which(is.na(dates) | format(dates) != x)
    if (length(bad) > 0L) {
      .rxpre2dup_design_stop(
        path,
        bad,
        "design_date MUST hold a date as text YYYY-MM-DD"
      )
    }
    # The choice runs before every row check below, so a row of an older
    # date never meets those checks.
    keep <- rep(TRUE, length(dates))
    if (!is.null(as_of)) {
      keep <- dates <= as_of_date
    }
    # The latest remaining date of each atc, as a day number.
    day <- as.numeric(dates)
    latest <- rep(NA_real_, length(day))
    latest[keep] <- ave(day[keep], sheet[["atc"]][keep], FUN = max)
    keep <- keep & day == latest
    data_rows <- data_rows[keep]
    if (length(data_rows) == 0L) {
      stop(
        "The design file ",
        path,
        if (is.null(as_of)) {
          " has no data row."
        } else {
          paste0(" has no row with design_date on or before ", as_of_date, ".")
        },
        call. = FALSE
      )
    }
  }
  refuse <- function(i, rule) {
    .rxpre2dup_design_stop(path, data_rows[i], rule)
  }
  d <- as.data.table(sheet[data_rows, .RXPRE2DUP_DESIGN_COLUMNS, drop = FALSE])

  # An empty cell reads as NA. A column with no value at all reads as
  # logical or numeric, and it is an empty column of its own type here.
  text_cols <- c("atc", "produkt", "lnmn", "form", "status", "source", "note")
  for (col in text_cols) {
    x <- d[[col]]
    if (all(is.na(x))) {
      set(d, j = col, value = rep(NA_character_, nrow(d)))
    } else if (!is.character(x)) {
      refuse(which(!is.na(x)), paste0("column ", col, " MUST hold text"))
    }
  }
  num_cols <- c("pack_ddd", "dur_min", "dur_usual", "dur_max")
  for (col in num_cols) {
    x <- d[[col]]
    if (all(is.na(x))) {
      set(d, j = col, value = rep(NA_real_, nrow(d)))
    } else if (!is.numeric(x)) {
      # A text cell in a row that the date choice drops also makes the
      # column text. The column is refused only for a text cell in a kept
      # row. .rxpre2dup_text_cells() takes and returns data rows.
      bad <- .rxpre2dup_text_cells(
        path,
        match(col, names(sheet)),
        data_rows[which(!is.na(x))]
      )
      if (length(bad) > 0L) {
        .rxpre2dup_design_stop(
          path,
          bad,
          paste0("column ", col, " MUST hold numbers")
        )
      }
    }
    set(d, j = col, value = as.numeric(d[[col]]))
  }

  for (col in c("atc", "produkt", "lnmn", "form", "status")) {
    bad <- which(is.na(d[[col]]) | !nzchar(trimws(d[[col]])))
    if (length(bad) > 0L) {
      refuse(bad, paste0("column ", col, " is empty"))
    }
  }
  for (col in num_cols) {
    bad <- which(!is.finite(d[[col]]) | d[[col]] <= 0)
    if (length(bad) > 0L) {
      refuse(bad, paste0(col, " MUST be a number greater than 0"))
    }
  }
  bad <- which(d$dur_min > d$dur_usual | d$dur_usual > d$dur_max)
  if (length(bad) > 0L) {
    refuse(bad, "dur_min <= dur_usual <= dur_max MUST hold")
  }
  bad <- which(!d$status %chin% c("auto", "approved"))
  if (length(bad) > 0L) {
    refuse(bad, "status MUST be 'auto' or 'approved'")
  }

  # Duplicate keys. Inside one key, sorted by pack_ddd, two rows closer than
  # 0.01 are neighbours whenever any two rows are. The 1e-9 margin accepts
  # rows exactly 0.01 apart: 40.00 - 39.99 is 0.00999... in floating point.
  k <- data.table(
    row = seq_len(nrow(d)),
    dup_key = paste(d$atc, d$produkt, .rxpre2dup_lnmn_key(d$lnmn), sep = "\r"),
    pack_ddd = d$pack_ddd
  )
  setorderv(k, c("dup_key", "pack_ddd"))
  close <- which(
    k$dup_key[-1L] == k$dup_key[-nrow(k)] & diff(k$pack_ddd) < 0.01 - 1e-9
  )
  if (length(close) > 0L) {
    bad <- sort(unique(c(k$row[close], k$row[close + 1L])))
    refuse(
      bad,
      paste0(
        "two rows with the same atc, produkt and lnmn MUST have values of ",
        "pack_ddd that differ by 0.01 or more"
      )
    )
  }

  d[, package_id := seq_len(.N)]
  setcolorder(d, "package_id")
  d[, ddd_usual := pack_ddd / dur_usual]
  d[, ddd_low := pack_ddd / dur_max]
  if (has_date) {
    set(d, j = "design_date", value = dates[data_rows])
  }
  return(d[])
}

# TRUE on every ATC code that matches the patterns of one codes entry, with
# the rule of add_rx(source = "atc"): a prefix match on any pattern, and no
# prefix match on a "!" pattern.
.rxpre2dup_atc_hits <- function(atc, patterns) {
  is_neg <- startsWith(patterns, "!")
  pos <- patterns[!is_neg]
  neg <- sub("^!", "", patterns[is_neg])
  none <- rep(FALSE, length(atc))
  hits <- Reduce(`|`, lapply(pos, function(p) startsWith(atc, p)), none)
  veto <- Reduce(`|`, lapply(neg, function(p) startsWith(atc, p)), none)
  return(which(hits & !veto))
}

# The package_id of the design row that each dispensing matches, or NA.
.rxpre2dup_match_design <- function(atc, produkt, lnmn, fddd, design) {
  pack_ddd <- NULL
  d <- data.table(
    row = seq_along(atc),
    atc = atc,
    produkt = produkt,
    lnmn_key = .rxpre2dup_lnmn_key(lnmn),
    fddd = fddd
  )
  k <- data.table(
    package_id = as.integer(design$package_id),
    atc = as.character(design$atc),
    produkt = as.character(design$produkt),
    lnmn_key = .rxpre2dup_lnmn_key(design$lnmn),
    pack_ddd = as.numeric(design$pack_ddd)
  )
  m <- k[
    d,
    on = c("atc", "produkt", "lnmn_key"),
    nomatch = NULL,
    allow.cartesian = TRUE
  ]
  # A dispensing with fddd = 0 matches no design row, even a row whose
  # pack_ddd is less than 0.005.
  m <- m[fddd > 0 & abs(fddd - pack_ddd) < 0.005]
  # A design from rxpre2dup_read_design() lets a dispensing match one row at
  # most. A hand-built design can break that, and then no row is chosen.
  if (anyDuplicated(m$row) > 0L) {
    stop(
      "A dispensing matches more than one row of design. Read design with ",
      "rxpre2dup_read_design(), which refuses such rows.",
      call. = FALSE
    )
  }
  out <- rep(NA_integer_, length(atc))
  out[m$row] <- m$package_id
  return(out)
}

# The add_rx() warnings that count the skeleton ids present in its input.
# Here that input is the table of periods, which holds only the persons with a
# period. The count describes the periods, not the caller's LMED.
.rxpre2dup_id_warning <- paste0(
  "(^No matching IDs found between skeleton and prescription data)|",
  "(skeleton IDs found in prescription data)"
)

#' Add PRE2DUP drug use periods to skeleton
#'
#' Runs PRE2DUP on the LMED dispensings that match each code list, and adds
#' one logical column per code list to the skeleton. A column is TRUE on every
#' week or annual row that overlaps a drug use period, and FALSE elsewhere.
#'
#' @section Dispensings:
#' A dispensing matches a design row when `atc`, `produkt` and
#' `tolower(trimws(lnmn))` are equal and `abs(fddd - pack_ddd) < 0.005`. Every
#' dispensing enters PRE2DUP with `ratio = 1` and `ddd = fddd`. A dispensing
#' with no design row uses the ATC-level rule of PRE2DUP, with the parameters
#' of `atc_parameters`.
#'
#' Rows of `lmed` whose `fddd` is missing, not finite or negative are dropped.
#' A row with `fddd = 0` is kept and enters PRE2DUP with `ddd = 0`. Such a row
#' matches no design row.
#'
#' @section Periods on the skeleton:
#' Each code list gets its own PRE2DUP run, on the dispensings whose ATC code
#' matches it. Each period becomes the closed interval from its start date to
#' its end date. [add_rx()] then maps these intervals to the skeleton, with
#' the period dates as `start_date` and `stop_date`. Weekly and annual rows
#' therefore behave exactly as in [add_rx()].
#'
#' @param skeleton A data.table created by [create_skeleton()].
#' @param lmed A data.table of LMED dispensings, with the columns `id_name`,
#'   `edatum` (Date), `atc`, `produkt`, `lnmn` and `fddd`.
#' @param hosp A data.table of hospital stays, with the columns `id_name`,
#'   `indatum` (Date) and `utdatum` (Date), or NULL. Stays with
#'   `utdatum <= indatum` are dropped.
#' @param id_name The name of the person id column in `lmed` and `hosp`.
#' @param codes A named list of ATC patterns, as in [add_rx()] with
#'   `source = "atc"`. Every name MUST start with `rxpre2dup_`. A pattern
#'   matches every ATC code that starts with it. A pattern that starts with
#'   `"!"` excludes the codes that start with the rest of it.
#' @param design A data.table returned by [rxpre2dup_read_design()].
#' @param atc_parameters The ATC-level parameters of PRE2DUP, with the
#'   columns of [pre2dup_atc_parameters].
#' @param ... The global arguments of PRE2DUP, passed to the PRE2DUP run:
#'   `global_gap_max`, `global_min`, `global_max`, `global_max_single`,
#'   `global_ddd_high`, `global_hosp_max`, `days_covered`, `weight_past`,
#'   `weight_current`, `weight_next`, `weight_first_last` and
#'   `post_process_perc`.
#' @return The skeleton, invisibly. The function adds the columns by
#'   reference, and it grows the column allocation as [add_rx()] does.
#'
#'   The attribute `package_coverage` of the skeleton is a data.table with one
#'   row per code list and ATC code. Its columns are `name`, `atc`,
#'   `n_purchases` and `share_without_package`. The share counts the
#'   purchases with no design row, after PRE2DUP adds the dispensings of one
#'   package on one day together. One message line shows each share.
#' @seealso [rxpre2dup_read_design()], [add_rx()], [pre2dup_atc_parameters]
#' @export
add_rxpre2dup <- function(
  skeleton,
  lmed,
  hosp = NULL,
  id_name = "lopnr",
  codes,
  design,
  atc_parameters = swereg::pre2dup_atc_parameters,
  ...
) {
  # The default of atc_parameters names the package. A bare name of lazy data
  # does not resolve from the namespace when swereg is loaded but not
  # attached, as in a call of swereg::add_rxpre2dup().
  fddd <- utdatum <- indatum <- atc <- produkt <- lnmn <- package_id <- NULL
  skeleton_expr <- substitute(skeleton)
  caller_env <- parent.frame()

  validate_skeleton_structure(skeleton)
  validate_id_column(lmed, id_name)
  .pre2dup_check_columns(
    lmed,
    c(id_name, "edatum", "atc", "produkt", "lnmn", "fddd"),
    "lmed"
  )
  validate_date_columns(lmed, "edatum", "lmed")
  validate_pattern_list(codes, "codes")
  bad_names <- names(codes)[!startsWith(names(codes), "rxpre2dup_")]
  if (length(bad_names) > 0L) {
    stop(
      "Every name in codes MUST start with 'rxpre2dup_'. These do not: ",
      paste(bad_names, collapse = ", "),
      call. = FALSE
    )
  }
  .pre2dup_check_columns(
    design,
    c(
      "package_id",
      "atc",
      "produkt",
      "lnmn",
      "pack_ddd",
      "dur_min",
      "dur_usual",
      "dur_max",
      "ddd_low",
      "ddd_usual"
    ),
    "design"
  )
  codes <- expand_code_list(codes)

  work <- lmed[,
    c(id_name, "edatum", "atc", "produkt", "lnmn", "fddd"),
    with = FALSE
  ]
  # The rows with a missing, non-finite or negative fddd go without a
  # warning. A row with fddd = 0 stays.
  work <- work[is.finite(fddd) & fddd >= 0]
  work[, atc := as.character(atc)]
  work[,
    package_id := .rxpre2dup_match_design(atc, produkt, lnmn, fddd, design)
  ]
  # The port takes integer person ids. `ids` maps them back.
  ids <- unique(work[[id_name]])

  stays <- NULL
  if (!is.null(hosp)) {
    validate_id_column(hosp, id_name)
    validate_date_columns(hosp, c("indatum", "utdatum"), "hosp")
    # A stay with a missing date, or with utdatum <= indatum, is dropped here.
    # The port stops on such a stay, and a skeleton build MUST NOT stop on a
    # data condition.
    h <- hosp[!is.na(utdatum) & !is.na(indatum) & utdatum > indatum]
    stays <- data.table(
      id = match(h[[id_name]], ids),
      admission = h$indatum,
      discharge = h$utdatum
    )
    # A person with no dispensing has no integer id here, and no period for a
    # stay to change, so the stays of that person are dropped.
    stays <- stays[!is.na(stays$id)]
  }
  packages <- design[,
    c(
      "package_id",
      "atc",
      "dur_min",
      "dur_usual",
      "dur_max",
      "ddd_low",
      "ddd_usual"
    ),
    with = FALSE
  ]

  skeleton <- .ensure_dt_alloc(
    skeleton,
    n_new = sum(!names(codes) %in% names(skeleton)),
    x_expr = skeleton_expr,
    env = caller_env,
    fn_name = "add_rxpre2dup()"
  )

  coverage <- vector("list", length(codes))
  for (i in seq_along(codes)) {
    name <- names(codes)[i]
    sub <- work[.rxpre2dup_atc_hits(work$atc, codes[[i]])]
    if (nrow(sub) == 0L) {
      # No dispensing matches, so no period exists. add_rx() writes the same
      # all-FALSE column, but it warns on an empty table.
      skeleton[, (name) := FALSE]
      next
    }
    per <- .pre2dup_periods(
      purchases = data.table(
        id = match(sub[[id_name]], ids),
        atc = sub$atc,
        package_id = sub$package_id,
        date = sub$edatum,
        ratio = 1,
        ddd = sub$fddd
      ),
      packages = packages,
      atc_parameters = atc_parameters,
      hosp = stays,
      ...
    )
    # One coverage row per codes entry and ATC code. The port counts its
    # purchases after it adds the same-day dispensings of a package together.
    coverage[[i]] <- cbind(
      data.table(name = name),
      attr(per, "package_coverage")
    )
    intervals <- data.table(
      id = ids[per$id],
      edatum = as.Date(per$start),
      fddd = NA_real_,
      atc = per$atc,
      start_date = as.Date(per$start),
      stop_date = as.Date(per$end)
    )
    setnames(intervals, "id", id_name)
    withCallingHandlers(
      skeleton <- add_rx(
        skeleton,
        intervals,
        id_name = id_name,
        codes = codes[i],
        source = "atc"
      ),
      warning = function(w) {
        if (grepl(.rxpre2dup_id_warning, conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  coverage <- rbindlist(coverage)
  setattr(skeleton, "package_coverage", coverage)
  # The line shows the shares only. A count of purchases per ATC can be
  # small, so the line prints no count.
  if (nrow(coverage) == 0L) {
    message("add_rxpre2dup(): no dispensing matches any code list.")
  } else {
    message(
      "add_rxpre2dup(): share of purchases without a design row: ",
      paste0(
        coverage$name,
        " ",
        coverage$atc,
        " ",
        sprintf("%.1f%%", 100 * coverage$share_without_package),
        collapse = "; "
      ),
      "."
    )
  }
  return(invisible(skeleton))
}
