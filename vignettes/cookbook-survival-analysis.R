## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(data.table)
library(survival)
library(ggplot2)
library(zoo)  # For na.locf function

## -----------------------------------------------------------------------------
# Use subset for demonstration (need adequate sample size for survival analysis)
study_ids <- swereg::fake_person_ids[1:500]
cat("Study population:", length(study_ids), "individuals\n")

## -----------------------------------------------------------------------------
# Create skeleton for 11-year follow-up
skeleton <- swereg::create_skeleton(
  ids = study_ids,
  date_min = "1995-01-01",
  date_max = "2005-12-31"
)

cat("Skeleton created:", nrow(skeleton), "rows\n")
cat("Time structure: weeks =", sum(!skeleton$is_isoyear), ", years =", sum(skeleton$is_isoyear), "\n")

