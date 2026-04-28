# Verifies that every package the TTE pipeline depends on at runtime is
# actually installed. Each of these is referenced unconditionally from
# R/ source -- not behind a `requireNamespace()` guard -- so a missing
# entry doesn't fail at install time, it fails inside a worker
# subprocess at runtime, gets caught by tryCatch, and surfaces as a
# `skipped = TRUE` placeholder (or a CONSORT sidecar warning). That
# silent-skip cascade is what produced the 135/135 IRRs reading
# `"there is no package called 'survey'"` and the empty forest plot.
#
# Promoting these from Suggests to Imports (in 26.4.27) means CRAN's
# install will pull them in. This test ensures we don't regress.

test_that("runtime-required packages are installed", {
  pkgs <- c(
    "survey",        # IRR / KM via svyglm / svykm in r6_tteenrollment.R
    "survival",      # survival::Surv()
    "mgcv",          # mgcv::bam() for IPCW with GAM
    "MASS",          # MASS::ginv() in table1.R
    "scales",        # scales::percent in r6_tteenrollment.R
    "glue",          # glue::glue() in mht-specifics
    "openxlsx",      # workbook export
    "patchwork",     # forest plot composition
    "DiagrammeR",    # CONSORT sidecars
    "DiagrammeRsvg", # CONSORT SVG export
    "rsvg"           # CONSORT PNG/PDF rasterization
  )
  for (p in pkgs) {
    expect_true(
      requireNamespace(p, quietly = TRUE),
      info = paste0(
        "runtime-required package not installed: ", p,
        " -- if intentionally moved back to Suggests, this test must be ",
        "removed AND the call site guarded with requireNamespace()."
      )
    )
  }
})
