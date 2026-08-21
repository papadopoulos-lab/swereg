# Where swereg itself was loaded from: a dev source tree, or an installed package.

# Decide whether a resolved package path is a dev SOURCE tree the dispatcher can
# load_all(), returning its root, or NULL for an installed package. The markers
# are STRUCTURAL, not a .libPaths() string-prefix heuristic: an installed package
# always carries Meta/package.rds (R writes it at install) and its R/ holds
# compiled bytecode (swereg.rdb / swereg.rdx, no .R files); a source tree has no
# Meta/package.rds and its R/ holds the actual .R sources load_all() needs. The
# old heuristic (`any(startsWith(system.file(), .libPaths()))`) was fragile under
# R CMD check -- there the package is loaded from a check library whose
# realpath-normalized form is not a string-prefix of system.file()'s recorded
# path, so `in_library` came out FALSE and the installed .Rcheck/swereg dir was
# returned as a dev tree.
#
# The batch worker (inst/batch_worker.R) used to be the source-tree marker, but
# that file now ships with `batchit`, not swereg (Phase 4). Using it as swereg's
# discriminator would make .swereg_dev_path() return NULL under load_all() -- the
# workers would then silently load the STALE INSTALLED swereg. So the marker is
# swereg's own source instead: a DESCRIPTION naming swereg, no Meta/package.rds,
# and an R/ directory holding .R source files (an installed package has none).
# Split out so the discriminator is unit-testable without a real R CMD check
# layout (see test-swereg_dev_path.R).
.dev_source_root <- function(pkg_path) {
  if (!nzchar(pkg_path)) {
    return(NULL)
  }
  # devtools::load_all() can report system.file() as the inst/ subdir; the loader
  # wants the package root, so strip a trailing /inst.
  if (basename(pkg_path) == "inst") {
    pkg_path <- dirname(pkg_path)
  }
  # Installed packages carry Meta/package.rds (R writes it at install); a source
  # tree never has it.
  if (file.exists(file.path(pkg_path, "Meta", "package.rds"))) {
    return(NULL)
  }
  # Must be swereg's OWN source tree: a DESCRIPTION that names the package ...
  desc_path <- file.path(pkg_path, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    return(NULL)
  }
  dev_pkg <- tryCatch(
    unname(read.dcf(desc_path, fields = "Package")[1L, 1L]),
    error = function(e) NA_character_
  )
  if (is.na(dev_pkg) || !identical(dev_pkg, "swereg")) {
    return(NULL)
  }
  # ... and an R/ directory holding actual .R source (an installed package's R/
  # holds swereg.rdb/.rdx bytecode instead, no .R files), which load_all() needs.
  r_dir <- file.path(pkg_path, "R")
  if (
    !dir.exists(r_dir) ||
      length(list.files(r_dir, pattern = "\\.R$")) == 0L
  ) {
    return(NULL)
  }
  pkg_path
}

# Source root when swereg was loaded via devtools::load_all(), else NULL.
.swereg_dev_path <- function() {
  .dev_source_root(system.file(package = "swereg"))
}
