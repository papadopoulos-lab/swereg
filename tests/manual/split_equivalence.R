# Check that two swereg trees behave the same. testthat does not run it.
#
#   Rscript tests/manual/split_equivalence.R <baseline-sha> [<head-ref>]
#
# Run it from inside the repository. With no <head-ref>, the second tree is the
# working tree: every tracked file and every untracked file that git does not
# ignore. The script installs each tree into its own temporary library and
# compares two things.
#
#   1. Code. Every function of the swereg namespace, internal ones included,
#      and every method and field list of each R6 generator, deparsed. Every
#      other object of the namespace, deparsed, and the export list. Each
#      source tree MUST define no top-level name twice, and `list.files("R")`
#      MUST equal the files of `Collate:`.
#   2. Outputs. One fixture plan runs s3 and `$export_tables()` under each
#      library. The script then compares these outputs:
#      - every cell of every sheet of `tables.xlsx` and `feasibility.xlsx`
#      - `results.qs2`, with `identical()`
#      - the text of the TARGET checklist and of `$results_summary()`
#      - `tteplan_osf_text()`
#      - the md5 of each file that `$export()` writes for three exhibits
#
# It prints each difference and exits with status 1 when there is one.

args <- commandArgs(trailingOnly = TRUE)
CTRL <- c("keepNA", "keepInteger", "niceNames", "showAttributes")
SELF <- normalizePath(sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]
))
RSCRIPT <- file.path(R.home("bin"), "Rscript")

# ---- child mode 1: dump the namespace -----------------------------------------

# One object as text. A function loses its srcref first.
dep <- function(x) {
  if (is.function(x)) {
    x <- utils::removeSource(x)
  }
  return(paste(deparse(x, control = CTRL), collapse = "\n"))
}

# The methods and field lists of one R6 generator, keyed by member.
dump_r6 <- function(nm, gen) {
  res <- list()
  for (slot in c("public_methods", "private_methods", "active")) {
    for (m in sort(names(gen[[slot]]))) {
      res[[paste0("r6:", nm, "$", slot, "$", m)]] <- dep(gen[[slot]][[m]])
    }
  }
  for (slot in c("public_fields", "private_fields")) {
    res[[paste0("r6:", nm, "$", slot)]] <- dep(gen[[slot]])
  }
  return(res)
}

# Every object of the swereg namespace as text, keyed by kind and name.
dump_namespace <- function(out) {
  ns <- asNamespace("swereg")
  res <- list()
  for (nm in sort(ls(ns, all.names = TRUE))) {
    obj <- get(nm, envir = ns, inherits = FALSE)
    if (is.function(obj)) {
      res[[paste0("fn:", nm)]] <- dep(obj)
    } else if (inherits(obj, "R6ClassGenerator")) {
      res <- c(res, dump_r6(nm, obj))
    } else if (is.environment(obj)) {
      res[[paste0("env:", nm)]] <- "<environment>"
    } else {
      res[[paste0("obj:", nm)]] <- dep(obj)
    }
  }
  res[["exports"]] <- paste(sort(getNamespaceExports(ns)), collapse = "\n")
  qs2::qs_save(
    list(
      objects = res,
      lib = normalizePath(find.package("swereg")),
      version = as.character(utils::packageVersion("swereg"))
    ),
    out
  )
  return(invisible(NULL))
}

# ---- child mode 2: run the fixture ---------------------------------------------

# The fixture plan, with s3 run. Part-year follow-up (26 weeks), a follow-up
# that only enrollment "01" gets (78 weeks, also part-year),
# `feasibility_follow_up_weeks`, and enrollment "02" exempt from the
# minimum-events rule.
build_fixture <- function(helpers, root) {
  for (f in c("helper-sparse_rule.R", "helper-s1_run.R")) {
    sys.source(file.path(helpers, f), envir = globalenv())
  }
  sys.source(
    file.path(helpers, "helper-feasibility_follow_up.R"),
    envir = globalenv()
  )
  spec <- .ff_spec(w = 260L)
  spec$follow_up <- list(
    list(label = "6 months", weeks = 26L),
    list(label = "1 year", weeks = 52L),
    list(label = "18 months", weeks = 78L, only_enrollments = list("01"))
  )
  spec$enrollments[[2]]$minimum_events_rule_exempt <- TRUE
  unlink(root, recursive = TRUE)
  dir.create(root, recursive = TRUE)
  plan <- .ff_plan(spec, root)
  dir <- file.path(root, "tteplan")
  .ff_write_files(plan, dir, .ff_panel())
  plan$output_dir <- dir
  tte_stamp_stages(plan)
  # Fixed run ids and creation time, so the two libraries stamp the same.
  s1 <- paste0(swereg:::.tte_s1_run_basis(plan), ":fixed-s1")
  plan$s1_run_id <- s1
  plan$created_at <- as.POSIXct("2026-11-07 12:00:00", tz = "UTC")
  ett <- data.table::copy(plan$ett)
  data.table::set(ett, j = "s1_run_id", value = s1)
  data.table::set(ett, j = "s2_run_id", value = paste0(s1, ":s2:fixed-s2"))
  plan$ett <- ett
  utils::capture.output(suppressWarnings(plan$s3_analyze(
    output_dir = dir,
    n_workers = 2L,
    swereg_dev_path = NULL
  )))
  return(plan)
}

# Each exhibit of the manifest on its own: the md5 of each written file, or
# the error message.
run_exhibits <- function(plan, dir) {
  ids <- plan$ett[follow_up == 52L & outcome_var == "osd_a"]
  manifest <- list(
    list(
      type = "survival",
      enrollment = "01",
      outcome = "osd_a",
      follow_up = 52L,
      age_group = ids[enrollment_id == "01"]$age_group,
      estimands = c("pp", "itt")
    ),
    list(
      type = "forest",
      exposures = stats::setNames(as.list(ids$ett_id), ids$enrollment_id)
    ),
    list(type = "table1", enrollment = "01")
  )
  res <- list()
  for (i in seq_along(manifest)) {
    d <- file.path(dir, paste0("exhibit_", i))
    res[[manifest[[i]]$type]] <- tryCatch(
      {
        paths <- utils::capture.output(
          p <- suppressMessages(suppressWarnings(plan$export(
            manifest[i],
            dir = d
          )))
        )
        stats::setNames(unname(tools::md5sum(p)), basename(p))
      },
      error = function(e) paste("error:", conditionMessage(e))
    )
  }
  return(res)
}

run_fixture <- function(helpers, root, out) {
  suppressPackageStartupMessages(library(data.table))
  plan <- build_fixture(helpers, root)
  xlsx <- file.path(root, "results", "tables.xlsx")
  testthat::with_mocked_bindings(
    suppressMessages(suppressWarnings(plan$export_tables(path = xlsx))),
    .export_timestamp = function() FF_EXPORT_TIME,
    .package = "swereg"
  )
  osf <- suppressWarnings(swereg::tteplan_osf_text(
    file.path(root, "spec", "spec_v001.yaml")
  ))
  # A subprocess started from here resolves swereg to this library, so the
  # s3 workers did too.
  worker_lib <- system2(
    RSCRIPT,
    c("-e", shQuote("cat(normalizePath(find.package('swereg')))")),
    stdout = TRUE
  )
  qs2::qs_save(
    list(
      checklist = utils::capture.output(plan$print_target_checklist()),
      results_summary = utils::capture.output(plan$results_summary()),
      osf = osf,
      osf_text = utils::capture.output(print(osf)),
      ett = plan$ett,
      exhibits = run_exhibits(plan, file.path(root, "exhibits")),
      lib = normalizePath(find.package("swereg")),
      worker_lib = worker_lib
    ),
    file.path(out, "text.qs2")
  )
  for (f in c("tables.xlsx", "feasibility.xlsx", "results.qs2")) {
    stopifnot(file.copy(file.path(root, "results", f), file.path(out, f)))
  }
  unlink(root, recursive = TRUE)
  return(invisible(NULL))
}

if (length(args) >= 1L && args[1L] == "--dump") {
  dump_namespace(args[2L])
  quit(status = 0L)
}
if (length(args) >= 1L && args[1L] == "--fixture") {
  run_fixture(args[2L], args[3L], args[4L])
  quit(status = 0L)
}

# ---- parent mode ----------------------------------------------------------------

DIFFS <- new.env()
DIFFS$found <- character(0)

note <- function(...) {
  DIFFS$found <- c(DIFFS$found, paste0(...))
  cat("DIFF: ", ..., "\n", sep = "")
  return(invisible(NULL))
}

run <- function(cmd, cmd_args, env = character(0)) {
  status <- system2(cmd, cmd_args, env = env)
  if (!identical(status, 0L)) {
    stop(
      "command failed (",
      status,
      "): ",
      cmd,
      " ",
      paste(cmd_args, collapse = " "),
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

git_commit <- function(repo, ref) {
  return(system2(
    "git",
    c("-C", repo, "rev-parse", "--verify", paste0(ref, "^{commit}")),
    stdout = TRUE
  ))
}

# Extract the tree of `sha` into `dest`, or the working tree when `sha` is NULL.
extract_tree <- function(repo, sha, dest) {
  dir.create(dest)
  cmd <- if (is.null(sha)) {
    sprintf(
      "cd %s && git ls-files -z -c -o --exclude-standard | tar --null -T - -cf - | tar -x -m -C %s",
      shQuote(repo),
      shQuote(dest)
    )
  } else {
    sprintf(
      "git -C %s archive %s | tar -x -m -C %s",
      shQuote(repo),
      sha,
      shQuote(dest)
    )
  }
  run("sh", c("-c", shQuote(cmd)))
  return(invisible(dest))
}

# The name one top-level expression defines, or NULL. `Class$set(type, name,
# ...)` defines `Class$set(type, name)`.
defined_name <- function(e) {
  if (!is.call(e)) {
    return(NULL)
  }
  head <- e[[1L]]
  if (
    is.symbol(head) &&
      as.character(head) %in% c("<-", "=", "<<-") &&
      is.symbol(e[[2L]])
  ) {
    return(as.character(e[[2L]]))
  }
  if (
    is.call(head) &&
      identical(head[[1L]], as.name("$")) &&
      identical(head[[3L]], as.name("set"))
  ) {
    return(paste0(
      deparse(head[[2L]]),
      "$set(",
      as.character(e[[2L]]),
      ", ",
      as.character(e[[3L]]),
      ")"
    ))
  }
  return(NULL)
}

# Part 1a: Collate against R/, and the parse probe for a name defined twice.
check_tree <- function(side, src) {
  rdir <- file.path(src, "R")
  files <- list.files(rdir)
  collate <- read.dcf(file.path(src, "DESCRIPTION"), "Collate")[1L, 1L]
  collate <- gsub("'", "", strsplit(trimws(collate), "[[:space:]]+")[[1L]])
  for (f in setdiff(files, collate)) {
    note(side, ": R/", f, " is not in Collate:")
  }
  for (f in setdiff(collate, files)) {
    note(side, ": Collate: names ", f, ", which R/ does not hold")
  }
  defs <- character(0)
  where <- character(0)
  for (f in files[grepl("\\.[Rr]$", files)]) {
    for (e in parse(file.path(rdir, f), keep.source = FALSE)) {
      key <- defined_name(e)
      defs <- c(defs, key)
      where <- c(where, rep(f, length(key)))
    }
  }
  for (d in unique(defs[duplicated(defs)])) {
    note(
      side,
      ": ",
      d,
      " is defined in more than one place: ",
      paste(where[defs == d], collapse = ", ")
    )
  }
  cat(
    side,
    ": ",
    length(files),
    " files in R/, ",
    length(collate),
    " in Collate:, ",
    length(defs),
    " top-level definitions\n",
    sep = ""
  )
  return(invisible(NULL))
}

# Part 1b: the two namespace dumps.
compare_namespaces <- function(ns) {
  nb <- names(ns$base$objects)
  nh <- names(ns$head$objects)
  for (n in setdiff(nb, nh)) {
    note("namespace: ", n, " is in the baseline only")
  }
  for (n in setdiff(nh, nb)) {
    note("namespace: ", n, " is in head only")
  }
  both <- intersect(nb, nh)
  for (n in both) {
    if (!identical(ns$base$objects[[n]], ns$head$objects[[n]])) {
      note("namespace: ", n, " differs")
    }
  }
  cat(
    "compared",
    length(both),
    "namespace entries,",
    sum(startsWith(both, "fn:")),
    "of them functions and",
    sum(startsWith(both, "r6:")),
    "R6 members\n"
  )
  return(invisible(NULL))
}

# Every cell of every sheet, as a character matrix per sheet.
cells <- function(path) {
  sheets <- openxlsx::getSheetNames(path)
  res <- lapply(sheets, function(s) {
    m <- openxlsx::read.xlsx(
      path,
      sheet = s,
      colNames = FALSE,
      skipEmptyRows = FALSE,
      skipEmptyCols = FALSE
    )
    m[] <- lapply(m, as.character)
    m <- as.matrix(m)
    m[is.na(m)] <- "<NA>"
    return(m)
  })
  names(res) <- sheets
  return(res)
}

compare_workbook <- function(f, outs) {
  a <- cells(file.path(outs$base, f))
  b <- cells(file.path(outs$head, f))
  if (!identical(names(a), names(b))) {
    note(f, ": the sheet names differ")
  }
  for (s in intersect(names(a), names(b))) {
    if (!identical(dim(a[[s]]), dim(b[[s]]))) {
      note(f, " sheet '", s, "': the dimensions differ")
      next
    }
    hit <- which(a[[s]] != b[[s]], arr.ind = TRUE)
    for (k in seq_len(min(nrow(hit), 5L))) {
      r <- hit[k, 1L]
      cc <- hit[k, 2L]
      note(
        f,
        " sheet '",
        s,
        "' cell [",
        r,
        ",",
        cc,
        "]: '",
        a[[s]][r, cc],
        "' vs '",
        b[[s]][r, cc],
        "'"
      )
    }
    if (nrow(hit) > 5L) {
      note(f, " sheet '", s, "': ", nrow(hit) - 5L, " more cells differ")
    }
  }
  cat(
    f,
    ": compared ",
    length(a),
    " sheets, ",
    sum(lengths(a)),
    " cells\n",
    sep = ""
  )
  return(invisible(NULL))
}

# Part 2: the outputs of the fixture.
compare_outputs <- function(outs, lib) {
  for (f in c("tables.xlsx", "feasibility.xlsx")) {
    compare_workbook(f, outs)
  }
  ra <- qs2::qs_read(file.path(outs$base, "results.qs2"))
  rb <- qs2::qs_read(file.path(outs$head, "results.qs2"))
  if (!identical(ra, rb)) {
    note(
      "results.qs2: not identical(); ",
      paste(all.equal(ra, rb), collapse = "; ")
    )
  }
  cat(
    "results.qs2: compared ",
    length(ra),
    " elements: ",
    paste(names(ra), collapse = ", "),
    "\n",
    sep = ""
  )
  ta <- qs2::qs_read(file.path(outs$base, "text.qs2"))
  tb <- qs2::qs_read(file.path(outs$head, "text.qs2"))
  keys <- c(
    "checklist",
    "results_summary",
    "osf",
    "osf_text",
    "ett",
    "exhibits"
  )
  for (k in keys) {
    if (identical(ta[[k]], tb[[k]])) {
      next
    }
    note(k, ": not identical()")
    if (is.character(ta[[k]]) && is.character(tb[[k]])) {
      n <- min(length(ta[[k]]), length(tb[[k]]))
      first <- which(ta[[k]][seq_len(n)] != tb[[k]][seq_len(n)])[1L]
      if (!is.na(first)) {
        note(
          k,
          " element ",
          first,
          ": '",
          ta[[k]][first],
          "' vs '",
          tb[[k]][first],
          "'"
        )
      }
    }
  }
  cat(
    "checklist: ",
    length(ta$checklist),
    " lines; results_summary: ",
    length(ta$results_summary),
    " lines; osf: ",
    length(ta$osf),
    " fields; ett: ",
    nrow(ta$ett),
    " rows\n",
    sep = ""
  )
  for (k in names(ta$exhibits)) {
    cat(
      "exhibit ",
      k,
      ": ",
      paste(names(ta$exhibits[[k]]), ta$exhibits[[k]], collapse = "; "),
      "\n",
      sep = ""
    )
  }
  for (side in c("base", "head")) {
    t <- if (side == "base") ta else tb
    cat(
      side,
      ": fixture loaded ",
      t$lib,
      ", a subprocess loaded ",
      t$worker_lib,
      "\n",
      sep = ""
    )
    for (l in c(t$lib, t$worker_lib)) {
      if (!identical(l, normalizePath(file.path(lib[[side]], "swereg")))) {
        note(side, ": the fixture ran swereg from ", l)
      }
    }
  }
  return(invisible(NULL))
}

# The parent: extract, install, compare. Returns the exit status.
main <- function() {
  if (length(args) < 1L || length(args) > 2L) {
    stop(
      "usage: Rscript tests/manual/split_equivalence.R <baseline-sha> [<head-ref>]",
      call. = FALSE
    )
  }
  repo <- system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE)
  base_sha <- git_commit(repo, args[1L])
  head_sha <- if (length(args) == 2L) git_commit(repo, args[2L]) else NULL
  work <- tempfile("split_equivalence_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  src <- c(
    base = file.path(work, "src_base"),
    head = file.path(work, "src_head")
  )
  lib <- c(
    base = file.path(work, "lib_base"),
    head = file.path(work, "lib_head")
  )
  extract_tree(repo, base_sha, src[["base"]])
  extract_tree(repo, head_sha, src[["head"]])
  cat(
    "baseline: ",
    base_sha,
    "\nhead:     ",
    head_sha %||% paste("working tree on", git_commit(repo, "HEAD")),
    "\n",
    sep = ""
  )
  user_libs <- paste(.libPaths(), collapse = ":")
  ns <- list()
  for (side in names(src)) {
    check_tree(side, src[[side]])
    dir.create(lib[[side]])
    run(
      file.path(R.home("bin"), "R"),
      c(
        "CMD",
        "INSTALL",
        "--no-docs",
        "--no-html",
        "--no-multiarch",
        "-l",
        shQuote(lib[[side]]),
        shQuote(src[[side]])
      )
    )
    out <- file.path(work, paste0("ns_", side, ".qs2"))
    env <- c(paste0("R_LIBS=", lib[[side]], ":", user_libs), "NOT_CRAN=true")
    run(RSCRIPT, c(shQuote(SELF), "--dump", shQuote(out)), env)
    ns[[side]] <- qs2::qs_read(out)
    cat(
      side,
      ": swereg ",
      ns[[side]]$version,
      " from ",
      ns[[side]]$lib,
      ", ",
      length(ns[[side]]$objects),
      " namespace entries\n",
      sep = ""
    )
    if (
      !identical(
        ns[[side]]$lib,
        normalizePath(file.path(lib[[side]], "swereg"))
      )
    ) {
      note(
        side,
        ": swereg loaded from ",
        ns[[side]]$lib,
        ", not the temporary library"
      )
    }
  }
  compare_namespaces(ns)
  outs <- list()
  for (side in names(src)) {
    outs[[side]] <- file.path(work, paste0("out_", side))
    dir.create(outs[[side]])
    env <- c(paste0("R_LIBS=", lib[[side]], ":", user_libs), "NOT_CRAN=true")
    # Both sides run the helpers of the baseline, and one fixture path.
    run(
      RSCRIPT,
      c(
        shQuote(SELF),
        "--fixture",
        shQuote(file.path(src[["base"]], "tests", "testthat")),
        shQuote(file.path(work, "fixture")),
        shQuote(outs[[side]])
      ),
      env
    )
  }
  compare_outputs(outs, lib)
  if (length(DIFFS$found) > 0L) {
    cat("FAIL:", length(DIFFS$found), "difference(s)\n")
    return(1L)
  }
  cat("PASS: no difference\n")
  return(0L)
}

quit(status = main())
