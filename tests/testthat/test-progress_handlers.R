# The non-interactive progressr handler writes a job log, not a terminal
# repaint. These tests pin that log format: no carriage return, at most one
# line per interval, and one line at the finish.

# `signals` is the number of `p()` calls, and it defaults to `steps`. A caller
# that asks for more than `steps` wants the overshoot. progressr warns on the
# extra call, from its own handler rather than from `p()`, so the caller mutes
# that warning around the whole probe.
run_progress_probe <- function(steps, sleep_s, interval_s, signals = steps) {
  withr::local_options(list(
    swereg.progress_interval_s = interval_s,
    progressr.enable = TRUE
  ))
  handler <- progress_line_handler()
  path <- withr::local_tempfile(fileext = ".log")
  con <- file(path, open = "wt")
  withr::defer({
    if (sink.number(type = "message") != 2L) sink(type = "message")
    close(con)
  })

  started <- Sys.time()
  sink(con, type = "message")
  progressr::with_progress(
    {
      p <- progressr::progressor(steps)
      for (i in seq_len(signals)) {
        Sys.sleep(sleep_s)
        p(sprintf("step_%03d", i))
      }
    },
    handlers = handler
  )
  sink(type = "message")
  seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  flush(con)

  raw <- readChar(path, file.info(path)$size, useBytes = TRUE)
  list(
    raw = raw,
    lines = grep("/", readLines(path, warn = FALSE), value = TRUE),
    seconds = seconds
  )
}

test_that("the non-interactive progress log carries no carriage return", {
  probe <- run_progress_probe(steps = 200L, sleep_s = 0.0025, interval_s = 0.1)
  expect_gte(length(probe$lines), 2L)
  expect_false(grepl("\r", probe$raw, fixed = TRUE))
})

test_that("the non-interactive progress log holds one line per interval", {
  interval_s <- 0.1
  probe <- run_progress_probe(
    steps = 200L,
    sleep_s = 0.0025,
    interval_s = interval_s
  )
  expect_gte(length(probe$lines), 2L)
  expect_lte(length(probe$lines), ceiling(probe$seconds / interval_s) + 1L)
})

test_that("the non-interactive progress log ends on a full-count line", {
  probe <- run_progress_probe(steps = 200L, sleep_s = 0.0025, interval_s = 0.1)
  expect_match(utils::tail(probe$lines, 1L), " 200/200 (100.0%) ", fixed = TRUE)
  expect_length(grep(" 200/200 ", probe$lines, fixed = TRUE), 1L)
})

test_that("a non-interactive progress line matches the log format", {
  probe <- run_progress_probe(steps = 200L, sleep_s = 0.0025, interval_s = 0.1)
  pattern <- paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2} ",
    "[0-9]+/[0-9]+ \\([0-9.]+%\\) ",
    "elapsed [0-9]{2}:[0-9]{2}:[0-9]{2} last: "
  )
  expect_true(all(grepl(pattern, probe$lines)))
  expect_match(utils::tail(probe$lines, 1L), paste0(pattern, "step_200$"))
})

test_that("the default progress interval is 600 seconds", {
  withr::local_options(list(swereg.progress_interval_s = NULL))
  expect_identical(eval(formals(progress_line_handler)$interval), 600)
})


# ---------------------------------------------------------------------------
# The finish line never counts above the total
# ---------------------------------------------------------------------------
#
# A step above the total prints `201/200 (100.5%)`. That reads as a defect in
# the run, and it is a defect in the counter.
#
# progressr's own dispatch never delivers such a step. `with_progress()` caps
# the count at the total and warns on the extra signal, so the clamp is
# reachable only from the reporter. The two tests below therefore call the
# INSTALLED reporter: `environment(handler)$reporter` is the closure
# `progress_line_handler()` handed to progressr, not a copy of it.

test_that("the finish line clamps a step above the total", {
  reporter <- environment(progress_line_handler())$reporter

  line <- utils::capture.output(
    reporter$finish(
      config = list(max_steps = 200),
      state = list(step = 201, message = "overshoot")
    ),
    type = "message"
  )

  expect_length(line, 1L)
  expect_match(line, " 200/200 (100.0%) ", fixed = TRUE)
})

test_that("the finish line leaves a step below the total alone", {
  # The clamp is `min()`, not `config$max_steps`. An unfinished run must still
  # report where it stopped.
  reporter <- environment(progress_line_handler())$reporter

  line <- utils::capture.output(
    reporter$finish(
      config = list(max_steps = 200),
      state = list(step = 150, message = "stopped")
    ),
    type = "message"
  )

  expect_length(line, 1L)
  expect_match(line, " 150/200 (75.0%) ", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# The finish line reports the last SIGNALLED step
# ---------------------------------------------------------------------------
#
# `with_progress()` sends synthetic progressions of type "shutdown" when the
# expression ends, and each adds a step the caller never signalled. A 200-step
# progressor signalled 100 times receives shutdown steps 101 and 102. An
# unfiltered reporter therefore ends the run on `101/200`, which reads as a
# defect in the run rather than in the counter.
#
# Both tests below drive the real `progressr::with_progress()`.

test_that("a 200-step progressor signalled 100 times ends on 100/200", {
  # The interval is far longer than the run, so no update line is printed and
  # the single line comes from the finish.
  probe <- run_progress_probe(
    steps = 200L,
    sleep_s = 0,
    interval_s = 1e6,
    signals = 100L
  )
  expect_length(probe$lines, 1L)
  expect_match(probe$lines, " 100/200 (50.0%) ", fixed = TRUE)
  expect_match(probe$lines, "last: step_100$")
})

test_that("a 200-step progressor signalled 150 times ends on 150/200", {
  # The count the finish reports tracks the signals, so it moves with them.
  probe <- run_progress_probe(
    steps = 200L,
    sleep_s = 0,
    interval_s = 1e6,
    signals = 150L
  )
  expect_length(probe$lines, 1L)
  expect_match(probe$lines, " 150/200 (75.0%) ", fixed = TRUE)
  expect_match(probe$lines, "last: step_150$")
})

test_that("a 200-step progressor signalled 0 times ends on 0/200", {
  # A run that signals nothing reached step 0. progressr still sends shutdown
  # steps 1 and 2. An unfiltered reporter then reports `1/200`, and a reader
  # takes a job that did nothing for a job that started.
  probe <- run_progress_probe(
    steps = 200L,
    sleep_s = 0,
    interval_s = 1e6,
    signals = 0L
  )
  expect_length(probe$lines, 1L)
  expect_match(probe$lines, " 0/200 (0.0%) ", fixed = TRUE)
  expect_false(any(grepl(" 1/200 ", probe$lines, fixed = TRUE)))
})

test_that("no shutdown step reaches the log", {
  # Interval 0 prints every update, so a shutdown step that slipped through
  # would appear as its own line.
  probe <- run_progress_probe(
    steps = 200L,
    sleep_s = 0,
    interval_s = 0,
    signals = 100L
  )
  counts <- as.integer(sub("^.* ([0-9]+)/200 .*$", "\\1", probe$lines))
  expect_identical(max(counts), 100L)
  expect_false(any(grepl(" 101/200 ", probe$lines, fixed = TRUE)))
  expect_false(any(grepl(" 102/200 ", probe$lines, fixed = TRUE)))
})

test_that("a progressor signalled past its total still ends on the total", {
  probe <- withCallingHandlers(
    run_progress_probe(
      steps = 200L,
      sleep_s = 0.0025,
      interval_s = 0.1,
      signals = 201L
    ),
    warning = function(w) {
      # progressr warns on the 201st signal. The overshoot is what this test
      # asks for, so mute that warning and no other.
      if (grepl("no longer listening", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_match(utils::tail(probe$lines, 1L), " 200/200 (100.0%) ", fixed = TRUE)
})


# ---------------------------------------------------------------------------
# setup_progress_handlers() itself, in a subprocess
# ---------------------------------------------------------------------------
#
# THE REAL FUNCTION CANNOT RUN INSIDE A TESTTHAT RUN. Its second line is
# `progressr::handlers(global = TRUE)`, which calls
# `base::globalCallingHandlers()`. That function stops with "should not be
# called with handlers on the stack", and testthat keeps calling handlers on
# the stack throughout a run. A `test_that()` body and a file's top level both
# hit it.
#
# So the test spawns a plain non-interactive R session, which is the session
# the function is written for, and reads what it installed. That covers the
# global path a mock of this function hid: the carriage-return defect reached a
# release through it.

test_that("setup_progress_handlers installs the job-log handler in a plain session", {
  skip_on_cran()
  dev_tree <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(dev_tree, "DESCRIPTION")),
    "source tree only"
  )
  skip_if_not_installed("pkgload")

  script <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      'suppressMessages(pkgload::load_all(commandArgs(TRUE)[1], quiet = TRUE))',
      'swereg::setup_progress_handlers()',
      'cat("GLOBAL:", progressr::handlers(global = NA), "\\n")',
      'cat("ENABLE:", isTRUE(getOption("progressr.enable")), "\\n")',
      'cat("HANDLER:", paste(vapply(progressr::handlers(),',
      '  function(h) class(h)[1], character(1)), collapse = ","), "\\n")',
      '# A progressor at top level is refused, so wrap it. Nothing else here',
      '# wraps the emission: the GLOBAL handler is what reports it.',
      'local({',
      '  p <- progressr::progressor(steps = 4L)',
      '  for (i in 1:4) p(sprintf("item_%02d", i))',
      '})'
    ),
    script
  )

  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(script), shQuote(dev_tree)),
    stdout = TRUE,
    stderr = TRUE
  ))
  info <- paste(out, collapse = "\n")

  expect_identical(attr(out, "status"), NULL, info = info)
  expect_true("GLOBAL: TRUE " %in% out, info = info)
  expect_true("ENABLE: TRUE " %in% out, info = info)
  expect_true(
    "HANDLER: swereg_line_progression_handler " %in% out,
    info = info
  )

  # One log line, on the total, and no carriage return in it.
  line <- grep("^[0-9]{4}-[0-9]{2}-[0-9]{2}T", out, value = TRUE)
  expect_length(line, 1L)
  expect_match(line, " 4/4 (100.0%) ", fixed = TRUE)
  expect_match(line, "last: item_04$")
  expect_false(grepl("\r", info, fixed = TRUE))
})
