#!/usr/bin/env Rscript

# Report every descriptive sentence longer than 25 words in a Markdown or R
# Markdown file. The limit is the one in ~/.claude/skills/rw-technical-prose,
# which is ASD-STE100 adapted. The script exits 1 when any file is over.
#
# Usage: Rscript dev/check_sentence_length.R <file> [<file> ...]
#
# dev/ is in .Rbuildignore, so this file does not ship with the package.
#
# The segmenter is stateful, because a line-by-line filter measures the wrong
# thing. It drops YAML front matter, fenced code bodies and display-math
# blocks. It also drops a horizontal rule, and any line that opens with `<`.
# It then cuts the remaining lines into authored units. A heading, a table row
# and a list item are each one unit, and a list item keeps its continuation
# lines. A blank line closes the open unit, and every other line joins the
# open paragraph or list item. Without that, one missing full stop at the end
# of a list item merges several units. The merged reading is not a sentence
# anyone wrote.

.CSL_LIMIT <- 25L

# Sentence boundary: terminal punctuation, then whitespace. The lookbehinds
# keep an abbreviation from ending a sentence. Without them a long sentence
# holding `e.g.` can read as two short ones and pass. That is the silent
# direction of the error.
.CSL_SPLIT <- paste0(
  "(?<=[.!?])",
  "(?<!\\be\\.g\\.)(?<!\\bi\\.e\\.)(?<!\\bcf\\.)(?<!\\bvs\\.)",
  "(?<!\\betc\\.)(?<!\\bal\\.)(?<!\\bFig\\.)(?<!\\bNo\\.)",
  "(?<!\\bDr\\.)(?<!\\bSt\\.)(?<!\\bvol\\.)(?<!\\bpp\\.)",
  "\\s+"
)

.csl_is_fence <- function(x) grepl("^\\s*(`{3,}|~{3,})", x)
.csl_is_heading <- function(x) grepl("^\\s*#{1,6}\\s", x)
.csl_is_table <- function(x) grepl("^\\s*\\|", x)
.csl_is_blank <- function(x) grepl("^\\s*$", x)
.csl_is_item <- function(x) grepl("^\\s*([-*+]|[0-9]+[.)])\\s+\\S", x)
.csl_is_html <- function(x) grepl("^\\s*<", x)
.csl_is_rule <- function(x) grepl("^\\s*([-*_=]\\s*){3,}$", x)
.csl_is_math <- function(x) grepl("^\\s*\\$\\$\\s*$", x)


# Cut one file into authored units. Each element of the result is one heading,
# one table row, one list item with its continuation lines, or one paragraph.
.csl_units <- function(lines) {
  units <- character(0)
  buf <- character(0)

  flush <- function() {
    if (length(buf) > 0L) {
      units <<- c(units, paste(trimws(buf), collapse = " "))
      buf <<- character(0)
    }
  }

  in_yaml <- FALSE
  in_fence <- FALSE
  in_math <- FALSE
  in_item <- FALSE

  for (i in seq_along(lines)) {
    x <- lines[[i]]

    # YAML front matter, which opens with `---` on the very first line.
    if (i == 1L && grepl("^---\\s*$", x)) {
      in_yaml <- TRUE
      next
    }
    if (in_yaml) {
      if (grepl("^(---|\\.\\.\\.)\\s*$", x)) in_yaml <- FALSE
      next
    }

    # Fenced code. The fence lines and the body both go.
    if (.csl_is_fence(x)) {
      flush()
      in_item <- FALSE
      in_fence <- !in_fence
      next
    }
    if (in_fence) next

    # Display math. `$$` on its own line opens and closes it.
    if (.csl_is_math(x)) {
      flush()
      in_item <- FALSE
      in_math <- !in_math
      next
    }
    if (in_math) next

    if (.csl_is_blank(x)) {
      flush()
      in_item <- FALSE
      next
    }
    if (.csl_is_rule(x)) {
      flush()
      in_item <- FALSE
      next
    }
    if (.csl_is_html(x)) {
      flush()
      in_item <- FALSE
      next
    }

    if (.csl_is_heading(x)) {
      flush()
      in_item <- FALSE
      units <- c(units, trimws(sub("^\\s*#{1,6}\\s+", "", x)))
      next
    }

    if (.csl_is_table(x)) {
      flush()
      in_item <- FALSE
      units <- c(units, trimws(x))
      next
    }

    if (.csl_is_item(x)) {
      flush()
      in_item <- TRUE
      buf <- sub("^\\s*([-*+]|[0-9]+[.)])\\s+", "", x)
      next
    }

    # A plain line. It continues the open list item, or the open paragraph.
    buf <- c(buf, sub("^\\s*>\\s?", "", x))
  }
  flush()
  units[nzchar(trimws(units))]
}


# Split one unit into sentences. A unit that holds no terminal punctuation
# stays one sentence.
.csl_sentences <- function(unit) {
  s <- unlist(strsplit(unit, .CSL_SPLIT, perl = TRUE), use.names = FALSE)
  s <- trimws(s)
  s[nzchar(s)]
}


.csl_words <- function(sentence) {
  # A table row is one unit, and a pipe inside it separates two cells. Replace
  # each pipe with a space before counting, or `alpha|beta` counts as one word.
  x <- gsub("\\|", " ", sentence)
  w <- unlist(strsplit(x, "\\s+"), use.names = FALSE)
  sum(nzchar(w) & grepl("[[:alnum:]]", w))
}


# Every over-limit sentence of one file, as a data frame. The `sentence`
# column is truncated to 70 characters.
csl_check_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  units <- .csl_units(lines)
  s <- unlist(lapply(units, .csl_sentences), use.names = FALSE)
  n <- vapply(s, .csl_words, integer(1), USE.NAMES = FALSE)
  over <- which(n > .CSL_LIMIT)
  data.frame(
    words = n[over],
    sentence = substr(s[over], 1L, 70L),
    stringsAsFactors = FALSE
  )
}


.csl_main <- function(paths) {
  if (length(paths) == 0L) {
    cat("usage: Rscript dev/check_sentence_length.R <file> [<file> ...]\n")
    quit(status = 2L)
  }
  bad <- 0L
  for (p in paths) {
    if (!file.exists(p)) {
      cat(sprintf("%s: MISSING\n", p))
      bad <- bad + 1L
      next
    }
    r <- csl_check_file(p)
    worst <- if (nrow(r) > 0L) max(r$words) else 0L
    cat(sprintf(
      "%s: %d sentence(s) over %d words (worst %d)\n",
      p,
      nrow(r),
      .CSL_LIMIT,
      worst
    ))
    if (nrow(r) > 0L) {
      r <- r[order(-r$words), , drop = FALSE]
      for (k in seq_len(nrow(r))) {
        cat(sprintf("  %3dw  %s\n", r$words[k], r$sentence[k]))
      }
      bad <- bad + nrow(r)
    }
  }
  quit(status = if (bad > 0L) 1L else 0L)
}

# `sys.nframe()` is 0 at the top level of an R evaluation, and greater than 0
# under `source()`. So an `Rscript` run calls `.csl_main()`, and a `source()`
# call loads the functions and runs nothing. That is what lets another script
# source this checker.
if (!interactive() && sys.nframe() == 0L) {
  .csl_main(commandArgs(trailingOnly = TRUE))
}
