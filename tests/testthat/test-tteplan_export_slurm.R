# =============================================================================
# tteplan_export_slurm(): the generated chain, read back off disk
# =============================================================================
# The exporter writes text and returns paths. Every check here reads the files
# it wrote, so the suite runs on a host with no scheduler installed.
#
# Two reading helpers, and the difference between them is load-bearing.
# `.slx_lines()` returns every line, `#SBATCH` directives included.
# `.slx_code()` returns the lines bash executes, so a comment can never satisfy
# an assertion about what the job does.
#
# The mount-check order test reads `.slx_code()` for that reason. The comment
# above the mount check names the order in words, and a test that read the raw
# text would pass on the comment alone.
# =============================================================================

skip_if_not_installed("withr")

.slx_stages <- c("s0_init.R", "s1.R", "s2.R", "s3.R", "s4_export.R")
.slx_ids <- c("s0_init", "s1", "s2", "s3", "s4_export")
.slx_prefix <- "proj_002"

.slx_root <- withr::local_tempdir(.local_envir = teardown_env())


# One fixture per test, under its own sub-directory of `.slx_root`.
#
# `project_prefix` holds a space, which no Slurm job name may hold. The
# exporter replaces it, so every generated file name below reads `proj_002`.
# The data directory holds `tteplan.qs2`, which is the path the mount check
# reads before it reads the filesystem type.
.slx_fixture <- function(tag, stages = .slx_stages) {
  base <- file.path(.slx_root, tag)
  data_dir <- file.path(base, "data")
  repo <- file.path(base, "repo")
  out <- file.path(base, "out")
  dir.create(data_dir, recursive = TRUE)
  dir.create(repo, recursive = TRUE)
  dir.create(out, recursive = TRUE)
  file.create(file.path(data_dir, "tteplan.qs2"))
  for (s in stages) {
    dir.create(
      dirname(file.path(repo, s)),
      showWarnings = FALSE,
      recursive = TRUE
    )
    file.create(file.path(repo, s))
  }

  plan <- swereg::TTEPlan$new(
    project_prefix = "proj 002",
    skeleton_files = "skeleton_001.qs2",
    global_max_isoyearweek = "2023-52"
  )
  plan$spec_version <- "v003"
  plan$expected_n_ids <- 123456L
  plan$expected_skeleton_file_count <- 4L
  plan$dir_tteplan_cp <- swereg::CandidatePath$new(data_dir, "dir_tteplan")

  norm <- function(p) normalizePath(p, winslash = "/", mustWork = TRUE)
  list(
    plan = plan,
    base = norm(base),
    data = norm(data_dir),
    repo = norm(repo),
    dir = norm(out),
    stages = stages
  )
}


# Every line of one generated file.
.slx_lines <- function(path) readLines(path, warn = FALSE)

# The lines bash executes: comments and blank lines removed.
.slx_code <- function(path) {
  l <- .slx_lines(path)
  l[!grepl("^\\s*(#|$)", l)]
}

# The driver's sbatch lines, in submission order.
.slx_submissions <- function(driver_path) {
  grep("sbatch ", .slx_code(driver_path), value = TRUE, fixed = TRUE)
}

# Write the whole chain to the console, one file after another.
.slx_show <- function(paths) {
  for (p in paths) {
    cat("=== ", basename(p), " ===\n", sep = "")
    cat(.slx_lines(p), sep = "\n")
    cat("\n")
  }
}

# Mask what changes between hosts: the three fixture directories, and the
# filesystem type the exporter observed. Everything else in the chain is fixed
# by the fixture, so the snapshot pins it.
.slx_mask <- function(fx) {
  function(lines) {
    lines <- gsub(fx$data, "<DIR_TTEPLAN>", lines, fixed = TRUE)
    lines <- gsub(fx$repo, "<DIR_REPO>", lines, fixed = TRUE)
    lines <- gsub(fx$dir, "<DIR_OUT>", lines, fixed = TRUE)
    sub(
      "^swereg_expect_fstype='.*'$",
      "swereg_expect_fstype='<FSTYPE>'",
      lines
    )
  }
}


test_that("the exporter returns the job scripts then the driver, invisibly", {
  fx <- .slx_fixture("return")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  expect_identical(
    basename(paths),
    c(paste0(.slx_prefix, "_", .slx_ids, ".sh"), "proj_002_submit.sh")
  )
  expect_true(all(file.exists(paths)))
  expect_identical(dirname(paths), rep(file.path(fx$dir, "slurm"), 6L))

  # No two generated files share a name. Two that did would leave one file on
  # disk holding the other's text, and the returned paths would still look
  # complete. `anyDuplicated()` returns the index of the first repeat, or 0.
  expect_identical(anyDuplicated(paths), 0L)
  expect_length(list.files(file.path(fx$dir, "slurm")), length(paths))

  expect_invisible(tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  ))
})


test_that("the job name replaces each run of unusable characters with one _", {
  fx <- .slx_fixture("sanitize")
  # Three characters that no Slurm job name may hold, in one run.
  fx$plan$project_prefix <- "proj / 002"
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = "s1.R"
  )
  expect_identical(basename(paths), c("proj_002_s1.sh", "proj_002_submit.sh"))
})


test_that("the driver chains every dependant on the job id of the stage before it", {
  fx <- .slx_fixture("chain")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  subs <- .slx_submissions(paths[[6L]])
  expect_length(subs, 5L)

  # The first stage waits for nothing.
  expect_false(grepl("--dependency", subs[[1L]], fixed = TRUE))

  # Each later stage names the shell variable the line before it assigned, so
  # the test reads the chain topology and not the naming convention.
  assigned <- sub("^([A-Za-z_][A-Za-z0-9_]*)=.*$", "\\1", subs)
  for (i in 2:5) {
    expect_true(
      grepl(
        paste0("--dependency=afterok:\"$", assigned[[i - 1L]], "\""),
        subs[[i]],
        fixed = TRUE
      ),
      info = .slx_ids[[i]]
    )
  }
})


test_that("the driver submits each no_requeue stage with --no-requeue", {
  fx <- .slx_fixture("norequeue")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages,
    no_requeue = c("s1", "s3")
  )
  subs <- .slx_submissions(paths[[6L]])
  expect_identical(
    grepl("--no-requeue", subs, fixed = TRUE),
    c(FALSE, TRUE, FALSE, TRUE, FALSE)
  )
})


test_that("the driver submits every dependant with --kill-on-invalid-dep=yes", {
  fx <- .slx_fixture("killdep")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  subs <- .slx_submissions(paths[[6L]])
  # The first stage has no dependency, so the flag would mean nothing on it.
  expect_identical(
    grepl("--kill-on-invalid-dep=yes", subs, fixed = TRUE),
    c(FALSE, TRUE, TRUE, TRUE, TRUE)
  )
})


test_that("each job script reads a data path before it reads the filesystem type", {
  fx <- .slx_fixture("mountorder")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  for (i in 1:5) {
    code <- .slx_code(paths[[i]])
    read_at <- grep("head -c 1 -- \"$swereg_touch_path\"", code, fixed = TRUE)
    type_at <- grep("stat -f -c %T", code, fixed = TRUE)
    expect_length(read_at, 1L)
    expect_length(type_at, 1L)
    # An automounted share stays unmounted until something reads a path under
    # it. A type read that comes first reports the local disk and fails a
    # healthy job.
    expect_true(read_at < type_at, info = .slx_ids[[i]])
  }
})


test_that("the embedded filesystem type is the one observed at generation time", {
  fx <- .slx_fixture("fstype")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  observed <- system2(
    "stat",
    c("-f", "-c", "%T", "--", shQuote(fx$data)),
    stdout = TRUE
  )
  want <- paste0("swereg_expect_fstype='", observed, "'")
  for (i in 1:5) {
    expect_true(want %in% .slx_code(paths[[i]]), info = .slx_ids[[i]])
  }
})


test_that("every job script samples the kernel peak-memory counter into its own log", {
  fx <- .slx_fixture("memory")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  for (i in 1:5) {
    code <- .slx_code(paths[[i]])
    expect_true(
      any(grepl("/sys/fs/cgroup/memory.peak", code, fixed = TRUE)),
      info = .slx_ids[[i]]
    )
    expect_true(
      any(grepl("/proc/self/status", code, fixed = TRUE)),
      info = .slx_ids[[i]]
    )
    expect_true(any(grepl("VmHWM", code, fixed = TRUE)), info = .slx_ids[[i]])
    expect_true(
      any(grepl(
        paste0(.slx_prefix, "_", .slx_ids[[i]], "_mem.log"),
        code,
        fixed = TRUE
      )),
      info = .slx_ids[[i]]
    )
  }
})


test_that("no generated script reads the scheduler's own memory accounting", {
  # The tag names this fixture's directories, and those directories reach the
  # generated text. It MUST NOT hold either string this test forbids.
  fx <- .slx_fixture("accounting")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  # Raw text, comments included. `sacct` MaxRSS is empty on some Slurm builds,
  # so nothing generated here may read it, in a command or in a comment.
  for (p in paths) {
    txt <- .slx_lines(p)
    expect_false(any(grepl("sacct", txt, fixed = TRUE)), info = basename(p))
    expect_false(any(grepl("MaxRSS", txt, fixed = TRUE)), info = basename(p))
  }
})


test_that("each job script requests the cpus and memory its own stage was given", {
  fx <- .slx_fixture("resources")
  cpus <- c(s0_init = 2, s1 = 16, s2 = 6, s3 = 32, s4_export = 4)
  mem <- c(
    s0_init = "8G",
    s1 = "85G",
    s2 = "40G",
    s3 = "180G",
    s4_export = "16G"
  )
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages,
    cpus = cpus,
    mem = mem
  )
  for (i in 1:5) {
    lines <- .slx_lines(paths[[i]])
    expect_true(
      paste0("#SBATCH --cpus-per-task=", cpus[[i]]) %in% lines,
      info = .slx_ids[[i]]
    )
    expect_true(
      paste0("#SBATCH --mem=", mem[[i]]) %in% lines,
      info = .slx_ids[[i]]
    )
  }
})


test_that("only the driver names sbatch", {
  fx <- .slx_fixture("onlydriver")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  for (i in 1:5) {
    expect_false(
      any(grepl("sbatch", .slx_lines(paths[[i]]), fixed = TRUE)),
      info = .slx_ids[[i]]
    )
  }
  expect_true(any(grepl("sbatch", .slx_lines(paths[[6L]]), fixed = TRUE)))
})


test_that("the exporter never runs sbatch", {
  fx <- .slx_fixture("nosubmit")
  bin <- file.path(fx$base, "bin")
  dir.create(bin)
  sentinel <- file.path(fx$base, "sbatch-was-called")
  stub <- file.path(bin, "sbatch")
  writeLines(c("#!/bin/sh", paste("touch", shQuote(sentinel))), stub)
  Sys.chmod(stub, "0755")
  withr::local_path(bin, action = "prefix")

  # The stub answers first on this PATH, so a call would leave the sentinel.
  expect_identical(Sys.which("sbatch")[["sbatch"]], stub)
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  expect_false(file.exists(sentinel))
  expect_true(all(file.exists(paths)))
})


test_that("each job script carries the plan's spec version and progress denominators", {
  fx <- .slx_fixture("header")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  for (i in 1:6) {
    lines <- .slx_lines(paths[[i]])
    expect_true("# spec version: v003" %in% lines, info = basename(paths[[i]]))
  }
  for (i in 1:5) {
    lines <- .slx_lines(paths[[i]])
    expect_true(
      "# progress denominators: 123456 ids across 4 skeleton files" %in% lines,
      info = .slx_ids[[i]]
    )
    expect_true(
      any(grepl(
        "tteplan_locate_and_load(commandArgs(TRUE)[1])$check_version()",
        .slx_code(paths[[i]]),
        fixed = TRUE
      )),
      info = .slx_ids[[i]]
    )
  }
})


test_that("the exporter stops on a stage set it cannot turn into a chain", {
  fx <- .slx_fixture("badstages")
  ok <- function(...) {
    tteplan_export_slurm(fx$plan, repo = fx$repo, dir = fx$dir, ...)
  }
  expect_error(ok(stages = character(0)), "at least one stage script")
  expect_error(
    ok(stages = c("s1.R", "sub/s1.R")),
    "Two stages share one identity"
  )
  expect_error(ok(stages = c("s0_init.R", "absent.R")), "No stage script at")

  # The prefix is sanitized, so a bad job name can only come from the stage
  # file name. `s+1.R` exists in this fixture's repo.
  file.create(file.path(fx$repo, "s+1.R"))
  expect_error(ok(stages = "s+1.R"), "A stage script file name MUST match")

  # A space in the file name is caught earlier, by the path rule.
  file.create(file.path(fx$repo, "s 1.R"))
  expect_error(ok(stages = "s 1.R"), "holds a whitespace character")
})


test_that("the exporter stops on a stage that claims the driver's file name", {
  # A stage identity of `submit` derives `<prefix>_submit.sh`, which is the
  # driver's own file name. The exporter writes the driver last, so the job
  # script would lose its text to the driver, and the chain would submit the
  # driver to itself.
  fx <- .slx_fixture("reserved")
  file.create(file.path(fx$repo, "submit.R"))
  expect_error(
    tteplan_export_slurm(
      fx$plan,
      repo = fx$repo,
      dir = fx$dir,
      stages = c("s0_init.R", "submit.R"),
      no_requeue = character(0)
    ),
    "reserved for the driver script"
  )
  # The path it takes to the identity does not matter.
  dir.create(file.path(fx$repo, "sub"), showWarnings = FALSE)
  file.create(file.path(fx$repo, "sub", "submit.R"))
  expect_error(
    tteplan_export_slurm(
      fx$plan,
      repo = fx$repo,
      dir = fx$dir,
      stages = "sub/submit.R",
      no_requeue = character(0)
    ),
    "reserved for the driver script"
  )
})


# One injector per field of `swereg:::.SLURM_EMBEDDED_FIELDS`. Each takes the
# fixture and returns the argument list for one `tteplan_export_slurm()` call
# that carries a line break in that field, changing nothing else.
#
# `fstype` has no injector: it is the output of `stat -f -c %T`, which this
# test cannot make return two lines. Its carve-out is named below.
.slx_newline_injectors <- list(
  project_prefix = function(fx) {
    fx$plan$project_prefix <- "proj\nsbatch evil.sh"
    list(plan = fx$plan)
  },
  spec_version = function(fx) {
    fx$plan$spec_version <- "1.0\nsbatch evil.sh"
    list(plan = fx$plan)
  },
  expected_n_ids = function(fx) {
    fx$plan$expected_n_ids <- "42\nsbatch evil.sh"
    list(plan = fx$plan)
  },
  expected_skeleton_file_count = function(fx) {
    fx$plan$expected_skeleton_file_count <- "7\nsbatch evil.sh"
    list(plan = fx$plan)
  },
  dir_tteplan = function(fx) {
    evil <- file.path(fx$base, "data\nsbatch evil.sh")
    dir.create(evil, showWarnings = FALSE)
    file.create(file.path(evil, "tteplan.qs2"))
    fx$plan$dir_tteplan_cp <- swereg::CandidatePath$new(evil, "dir_tteplan")
    list(plan = fx$plan)
  },
  repo = function(fx) {
    evil <- file.path(fx$base, "repo\nsbatch evil.sh")
    dir.create(evil, showWarnings = FALSE)
    file.create(file.path(evil, .slx_stages))
    list(repo = evil)
  },
  dir = function(fx) {
    list(dir = file.path(fx$base, "out\nsbatch evil.sh"))
  },
  stages = function(fx) {
    # The file name and the identity are both valid. Only the path carries
    # the break, which is the vector the comment header exposes.
    evil <- "x\nsbatch --wrap=true #/s1.R"
    dir.create(
      file.path(fx$repo, dirname(evil)),
      recursive = TRUE,
      showWarnings = FALSE
    )
    file.create(file.path(fx$repo, evil))
    list(stages = evil, no_requeue = character(0))
  },
  cpus = function(fx) {
    list(cpus = "6\nsbatch evil.sh")
  },
  mem = function(fx) {
    list(mem = "85G\nsbatch evil.sh")
  }
)


# One injector per PATH-tagged field. A path reaches a `#SBATCH` directive,
# which ends at the first whitespace character, so a path holds none.
.slx_whitespace_injectors <- list(
  dir_tteplan = function(fx) {
    evil <- file.path(fx$base, "data dir")
    dir.create(evil, showWarnings = FALSE)
    file.create(file.path(evil, "tteplan.qs2"))
    fx$plan$dir_tteplan_cp <- swereg::CandidatePath$new(evil, "dir_tteplan")
    list(plan = fx$plan)
  },
  repo = function(fx) {
    evil <- file.path(fx$base, "repo dir")
    dir.create(evil, showWarnings = FALSE)
    file.create(file.path(evil, .slx_stages))
    list(repo = evil)
  },
  dir = function(fx) {
    list(dir = file.path(fx$base, "a b"))
  },
  stages = function(fx) {
    # The file name is valid and the identity is valid. The directory
    # component carries the space, and it reaches `#SBATCH --output=`.
    evil <- "sub dir/s1.R"
    dir.create(
      file.path(fx$repo, "sub dir"),
      recursive = TRUE,
      showWarnings = FALSE
    )
    file.create(file.path(fx$repo, evil))
    list(stages = evil, no_requeue = character(0))
  }
)


test_that("the sweep covers every field the generator embeds, under its tag", {
  # The generator and this test read one constant, and the constant carries
  # the rule tag. Add a field, or retag one, without a matching injector and
  # one of these expectations fails, so the sweep cannot go stale.
  fields <- swereg:::.SLURM_EMBEDDED_FIELDS
  expect_length(fields, 11L)
  expect_setequal(
    unique(unname(fields)),
    c("line", "path", "count", "cpus", "mem")
  )

  # Every field takes the newline injector, apart from the one carve-out.
  expect_setequal(c(names(.slx_newline_injectors), "fstype"), names(fields))

  # Every PATH-tagged field takes the whitespace injector as well.
  expect_setequal(
    names(.slx_whitespace_injectors),
    names(fields)[fields == "path"]
  )
})


test_that("whitespace in any path field stops the exporter", {
  for (field in names(.slx_whitespace_injectors)) {
    fx <- .slx_fixture(paste0("ws_", gsub("[^a-z_]", "", field)))
    args <- .slx_whitespace_injectors[[field]](fx)
    call_args <- utils::modifyList(
      list(
        plan = fx$plan,
        repo = fx$repo,
        dir = fx$dir,
        stages = fx$stages
      ),
      args
    )
    expect_error(
      do.call(tteplan_export_slurm, call_args),
      "holds a whitespace character",
      info = field
    )
  }
})


test_that("a line break in any embedded field stops the exporter", {
  for (field in names(.slx_newline_injectors)) {
    fx <- .slx_fixture(paste0("nl_", gsub("[^a-z_]", "", field)))
    args <- .slx_newline_injectors[[field]](fx)
    call_args <- utils::modifyList(
      list(
        plan = fx$plan,
        repo = fx$repo,
        dir = fx$dir,
        stages = fx$stages
      ),
      args
    )
    expect_error(
      do.call(tteplan_export_slurm, call_args),
      paste(
        "holds a line break",
        "holds a whitespace character",
        "MUST be NULL, NA, or one non-negative whole number",
        "MUST match \\^",
        sep = "|"
      ),
      info = field
    )
  }
})


test_that("the exporter stops on a resource value outside its own pattern", {
  fx <- .slx_fixture("resource_pattern")
  ok <- function(...) {
    tteplan_export_slurm(
      fx$plan,
      repo = fx$repo,
      dir = fx$dir,
      stages = fx$stages,
      ...
    )
  }
  expect_error(ok(cpus = "6; rm -rf /"), "`cpus` MUST match")
  expect_error(ok(cpus = "six"), "`cpus` MUST match")
  expect_error(ok(mem = "85GB"), "`mem` MUST match")
  expect_error(ok(mem = "$(sbatch evil.sh)"), "`mem` MUST match")
  # The documented forms pass.
  expect_silent(ok(cpus = 6, mem = "85G"))
  expect_silent(ok(cpus = 1, mem = "512"))
  expect_silent(ok(cpus = 32, mem = "180T"))
})


test_that("the exporter stops on a progress denominator that is not a count", {
  fx <- .slx_fixture("counts")
  ok <- function() {
    tteplan_export_slurm(
      fx$plan,
      repo = fx$repo,
      dir = fx$dir,
      stages = fx$stages
    )
  }
  for (bad in list(-1L, 1.5, c(1L, 2L), "12", Inf)) {
    fx$plan$expected_n_ids <- bad
    expect_error(ok(), "`expected_n_ids` MUST be", info = format(bad)[[1L]])
  }
  # NULL and NA are the states a plan holds before a count is measured.
  fx$plan$expected_n_ids <- NULL
  fx$plan$expected_skeleton_file_count <- NA_integer_
  paths <- ok()
  lines <- .slx_lines(paths[[1L]])
  expect_true(
    "# progress denominators: unknown ids across unknown skeleton files" %in%
      lines
  )
})


test_that("the exporter stops on a name that would read as a command-line option", {
  # A name starting with `-` is a legal file name and a legal Slurm job name.
  # A command that receives it positionally reads it as an option instead.
  # `Rscript --version.R` prints `file name is missing` and exits 1, and
  # `cd -d` reports an invalid option.
  fx <- .slx_fixture("hyphen")
  ok <- function(...) {
    tteplan_export_slurm(fx$plan, repo = fx$repo, dir = fx$dir, ...)
  }

  # A leading hyphen on the stage script file name. `no_requeue` is emptied so
  # the name check is the only thing that can stop this call.
  file.create(file.path(fx$repo, "-s9.R"))
  expect_error(
    ok(stages = "-s9.R", no_requeue = character(0)),
    "A stage script file name MUST match"
  )

  # A leading hyphen on the project prefix, which reaches every job name, the
  # driver name, the two log names and the memory log.
  fx$plan$project_prefix <- "-proj"
  expect_error(ok(stages = "s1.R"), "A derived name MUST match")

  # A leading dot would hide every generated file from a plain `ls`.
  fx$plan$project_prefix <- ".proj"
  expect_error(ok(stages = "s1.R"), "A derived name MUST match")

  # A prefix of punctuation alone sanitizes to `_`, which is also rejected.
  fx$plan$project_prefix <- "!!!"
  expect_error(ok(stages = "s1.R"), "A derived name MUST match")

  # The boundary holds the other way: a digit is a legal first character.
  fx$plan$project_prefix <- "002proj"
  paths <- ok(stages = "s1.R")
  expect_identical(
    basename(paths),
    c("002proj_s1.sh", "002proj_submit.sh")
  )
})


test_that("every generated command that takes a path terminates option parsing", {
  fx <- .slx_fixture("terminators")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )

  for (i in 1:5) {
    code <- .slx_code(paths[[i]])
    for (want in c(
      "head -c 1 -- \"$swereg_touch_path\"",
      "stat -f -c %T -- \"$swereg_dir_tteplan\"",
      "cat -- /sys/fs/cgroup/memory.peak",
      "awk -- '/^VmHWM:/ { print $2 }' /proc/self/status",
      "cd -- \"$swereg_repo\""
    )) {
      expect_true(
        any(grepl(want, code, fixed = TRUE)),
        info = paste(.slx_ids[[i]], want)
      )
    }
    # The stage script path is absolute, so no command can read it as an
    # option. `Rscript` takes no `--` before it: Rscript passes `--` through
    # as the argument itself, which would break the plan-directory argument.
    expect_true(
      any(grepl(
        paste0("swereg_stage_script='", fx$repo, "/", fx$stages[[i]], "'"),
        code,
        fixed = TRUE
      )),
      info = .slx_ids[[i]]
    )
  }

  driver <- .slx_code(paths[[6L]])
  expect_true(any(grepl(
    "cd -- \"$(dirname -- \"$0\")\"",
    driver,
    fixed = TRUE
  )))
  # Every submission passes `--` and then an absolute job script path.
  subs <- .slx_submissions(paths[[6L]])
  for (i in 1:5) {
    expect_true(
      grepl(
        paste0(
          " -- '",
          fx$dir,
          "/slurm/",
          .slx_prefix,
          "_",
          .slx_ids[[i]],
          ".sh')"
        ),
        subs[[i]],
        fixed = TRUE
      ),
      info = .slx_ids[[i]]
    )
  }
})


test_that("the exporter stops on a resource name that matches no stage", {
  fx <- .slx_fixture("badresources")
  ok <- function(...) {
    tteplan_export_slurm(
      fx$plan,
      repo = fx$repo,
      dir = fx$dir,
      stages = fx$stages,
      ...
    )
  }
  expect_error(ok(cpus = c(s9 = 4)), "`cpus` names no stage of this chain")
  expect_error(ok(mem = c(s9 = "4G")), "`mem` names no stage of this chain")
  expect_error(ok(no_requeue = "s9"), "`no_requeue` names no stage")

  # A named vector that skips a stage would leave that stage on a value
  # nobody chose.
  expect_error(ok(cpus = c(s1 = 16)), "`cpus` names no value for")
  expect_error(ok(cpus = c(2, 16)), "unnamed")
})


test_that("the generated chain matches its snapshot", {
  testthat::local_edition(3)
  fx <- .slx_fixture("snapshot")
  paths <- tteplan_export_slurm(
    fx$plan,
    repo = fx$repo,
    dir = fx$dir,
    stages = fx$stages
  )
  # `cran = TRUE` keeps the snapshot live under a plain `R CMD check`. The
  # transform masks the two things that differ between hosts, so nothing here
  # depends on where the check runs.
  expect_snapshot(
    .slx_show(paths),
    transform = .slx_mask(fx),
    cran = TRUE
  )
})
