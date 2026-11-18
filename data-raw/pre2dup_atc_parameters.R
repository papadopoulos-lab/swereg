# Writes data/pre2dup_atc_parameters.rda: PRE2DUP-R's ATC_parameters with
# unchanged values.
#
# Source: PRE2DUP-R (https://github.com/piavat/PRE2DUP-R), commit 43b6d89,
# PRE2DUPR 0.2.2, MIT licence, Copyright (c) 2025 PRE2DUPR authors.
#
# Run it from the package root, with PRE2DUPR 0.2.2 installed from that
# commit:
#   R_LIBS=~/R-scratch/pre2dupr-43b6d89 Rscript data-raw/pre2dup_atc_parameters.R

stopifnot(
  requireNamespace("PRE2DUPR", quietly = TRUE),
  as.character(utils::packageVersion("PRE2DUPR")) == "0.2.2"
)
env <- new.env()
utils::data("ATC_parameters", package = "PRE2DUPR", envir = env)
pre2dup_atc_parameters <- env$ATC_parameters
stopifnot(
  is.data.frame(pre2dup_atc_parameters),
  nrow(pre2dup_atc_parameters) == 175L,
  identical(
    names(pre2dup_atc_parameters),
    c(
      "partial_atc",
      "lower_ddd_atc",
      "usual_ddd_atc",
      "minimum_dur_atc",
      "maximum_dur_atc"
    )
  )
)
save(
  pre2dup_atc_parameters,
  file = "data/pre2dup_atc_parameters.rda",
  compress = "bzip2"
)
