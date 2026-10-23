# tte_s0_flags() reads exactly two flags, --check and --feasibility, and
# refuses everything else.

skip_if_not_installed("optparse")

test_that("no argument gives both flags FALSE", {
  expect_identical(
    tte_s0_flags(character(0)),
    list(check = FALSE, feasibility = FALSE)
  )
})

test_that("--check alone sets check", {
  expect_identical(
    tte_s0_flags("--check"),
    list(check = TRUE, feasibility = FALSE)
  )
})

test_that("--feasibility alone sets feasibility", {
  expect_identical(
    tte_s0_flags("--feasibility"),
    list(check = FALSE, feasibility = TRUE)
  )
})

test_that("--check with --feasibility is an error that names both flags", {
  for (args in list(
    c("--check", "--feasibility"),
    c("--feasibility", "--check")
  )) {
    expect_error(
      tte_s0_flags(args),
      "--check and --feasibility were given together",
      fixed = TRUE
    )
  }
})

test_that("an unknown flag is an error that names the two valid flags", {
  for (args in list("--foo", "-x", "--help", "--check=TRUE")) {
    expect_error(
      tte_s0_flags(args),
      "The valid flags are --check and --feasibility",
      fixed = TRUE
    )
  }
})

test_that("an abbreviation or a positional argument is an error", {
  # optparse accepts both. The exact match in tte_s0_flags() refuses them.
  for (args in list("--chec", "--feas", "check", c("--check", "extra"))) {
    expect_error(
      tte_s0_flags(args),
      "Unknown argument(s)",
      fixed = TRUE
    )
  }
})
