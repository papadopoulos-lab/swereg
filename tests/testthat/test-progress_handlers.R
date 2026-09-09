# The non-interactive progressr handler writes a job log, not a terminal
# repaint. These tests pin that log format: no carriage return, at most one
# line per interval, and one line at the finish.

run_progress_probe <- function(steps, sleep_s, interval_s) {
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
      for (i in seq_len(steps)) {
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
