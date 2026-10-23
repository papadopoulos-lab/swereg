#' Read the command-line flags of a project's `s0_init.R`
#'
#' Parses the trailing arguments of `Rscript s0_init.R` with `optparse`. The
#' two valid flags are `--check` and `--feasibility`, and each defaults to
#' `FALSE`.
#'
#' @details
#' The function stops on any other argument, an abbreviation of a valid flag
#' included. It also stops when `--check` and `--feasibility` both appear.
#' Each error message names the two valid flags.
#'
#' @param args Character vector of arguments. The default reads the trailing
#'   arguments of the running `Rscript`.
#' @return A list with two logical elements, `check` and `feasibility`.
#' @family tte_plan
#' @export
#' @examples
#' tte_s0_flags(character(0))
#' tte_s0_flags("--check")
#' tte_s0_flags("--feasibility")
tte_s0_flags <- function(args = commandArgs(trailingOnly = TRUE)) {
  valid <- c("--check", "--feasibility")
  fail <- function(what) {
    stop(
      "tte_s0_flags(): ",
      what,
      " The valid flags are --check and --feasibility, and at most one ",
      "of them.",
      call. = FALSE
    )
  }
  args <- as.character(args)
  parser <- optparse::OptionParser(
    usage = "%prog [--check | --feasibility]",
    option_list = list(
      optparse::make_option(
        "--check",
        action = "store_true",
        default = FALSE,
        help = "Check the plan and stop."
      ),
      optparse::make_option(
        "--feasibility",
        action = "store_true",
        default = FALSE,
        help = "Run the feasibility stage."
      )
    ),
    add_help_option = FALSE
  )
  parsed <- tryCatch(
    optparse::parse_args(parser, args = args, positional_arguments = TRUE),
    error = function(e) fail(paste0(conditionMessage(e), "."))
  )
  # optparse accepts an abbreviation such as `--chec` and returns any
  # positional argument, so the exact match below rejects both.
  unknown <- setdiff(args, valid)
  if (length(unknown) > 0L) {
    fail(paste0("Unknown argument(s): ", paste(unknown, collapse = ", "), "."))
  }
  check <- isTRUE(parsed$options$check)
  feasibility <- isTRUE(parsed$options$feasibility)
  if (check && feasibility) {
    fail("--check and --feasibility were given together.")
  }
  return(list(check = check, feasibility = feasibility))
}
