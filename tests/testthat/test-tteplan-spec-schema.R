# The schema is the only list of legal specification paths, so the test that
# matters reads the real specifications and asserts the schema covers them.
# A hand-typed set of paths would drift from the fleet the moment a
# specification changes. Deriving the set from the specifications cannot.

# The study specification fleet. It lives outside the package, so the fleet
# tests skip where it is absent. The tests below them need no fleet and run
# everywhere, including CI.
#
# The checkout sits at a different path on each machine, so take the first
# candidate that exists rather than one hard-coded path. A single path meant
# these tests ran on one box and skipped silently everywhere else, including
# CI. `~` expands per user, so one entry serves every user of a box.
SPEC_FLEET_CANDIDATES <- c(
  "~/code/structural-mht-registry-data",
  "~/skalkidou/structural-mht-registry-data"
)
SPEC_FLEET <- Find(dir.exists, path.expand(SPEC_FLEET_CANDIDATES))
# `Find()` gives NULL when nothing exists. `dir.exists(NULL)` is logical(0),
# which `skip_if_not()` errors on, so carry a path that cannot exist instead.
if (is.null(SPEC_FLEET)) {
  SPEC_FLEET <- path.expand(SPEC_FLEET_CANDIDATES[1])
}

# The specifications this test reads: the HIGHEST version in each project
# directory, and no other. A superseded version is frozen. It is never
# edited, so it keeps whatever key vocabulary it was written against, and
# a renamed key makes it fail this test forever. Reading every version
# ever written asserts that a rename can never happen.
#
# Highest equals the version each `s0_init.R` pins, on all 8 projects as
# of 2026-09-22. Highest is derived here rather than read from that file,
# because this package must not depend on another repository's script
# format, and because a specification written but not yet wired up still
# has to satisfy the schema.
spec_fleet_active_files <- function() {
  dirs <- sort(Sys.glob(file.path(SPEC_FLEET, "*")))
  dirs <- dirs[dir.exists(dirs)]
  out <- character(0)
  for (d in dirs) {
    files <- Sys.glob(file.path(d, "spec_v*.yaml"))
    if (length(files) == 0) {
      next
    }
    v <- as.integer(sub("^.*spec_v([0-9]+)\\.yaml$", "\\1", files))
    out <- c(out, files[which.max(v)])
  }
  return(sort(out))
}

# Walk a parsed specification to normalised key paths. The root is `$`, a
# mapping key appends `/<key>`, and a sequence index becomes `[]`. This is the
# same normalisation the schema is written in.
spec_walk <- function(x, path, acc) {
  if (!is.list(x)) {
    return(acc)
  }
  nms <- names(x)
  if (is.null(nms)) {
    for (el in x) {
      acc <- spec_walk(el, paste0(path, "[]"), acc)
    }
    return(acc)
  }
  acc$contexts <- c(acc$contexts, path)
  for (k in nms) {
    child <- paste0(path, "/", k)
    acc$paths <- c(acc$paths, child)
    acc <- spec_walk(x[[k]], child, acc)
  }
  return(acc)
}

# Read every parseable specification in the fleet and return its key paths and
# its mapping contexts.
spec_fleet_inventory <- function() {
  files <- spec_fleet_active_files()
  acc <- list(paths = character(0), contexts = character(0))
  for (f in files) {
    acc <- spec_walk(yaml::yaml.load_file(f), "$", acc)
  }
  return(list(
    files = files,
    paths = sort(unique(acc$paths)),
    contexts = sort(unique(acc$contexts))
  ))
}


test_that("the schema classifies every key path the specification fleet uses", {
  skip_if_not(dir.exists(SPEC_FLEET), "specification fleet not present")
  inv <- spec_fleet_inventory()

  # Floors, not pins. They fail an empty or truncated walk, which would
  # otherwise pass the classification check without measuring anything.
  # One file per project, so the floor is the project count, not the count
  # of every version ever written.
  expect_gte(length(inv$files), 8L)
  expect_gte(length(inv$paths), 100L)

  # A witness that the walk reaches depth 4 inside a real specification.
  # It was `matching_ratio`, which no specification holds any more: the
  # key is `comparator_to_intervention_ratio`, because the draw takes one
  # sample per trial and builds no matched set.
  expect_true(
    "$/enrollments[]/treatment/implementation/comparator_to_intervention_ratio" %in%
      inv$paths
  )

  unclassified <- inv$paths[is.na(.tte_spec_key_class(inv$paths))]
  expect_identical(unclassified, character(0))
})


test_that("the schema declares every mapping context the fleet uses", {
  skip_if_not(dir.exists(SPEC_FLEET), "specification fleet not present")
  inv <- spec_fleet_inventory()

  expect_gte(length(inv$contexts), 30L)
  expect_identical(
    setdiff(inv$contexts, names(.TTE_SPEC_SCHEMA)),
    character(0)
  )
})


# The specifications swereg reads today MUST keep reading once the schema
# refuses an undeclared key. This list was hand-typed and had gone stale:
# all five entries were superseded, and one named a file that is not valid
# YAML. It is derived now, for the reason this file states at the top.


test_that("no readable specification carries a refused or undeclared key", {
  skip_if_not(dir.exists(SPEC_FLEET), "specification fleet not present")
  files <- spec_fleet_active_files()
  skip_if_not(length(files) > 0, "readable specifications not present")
  # A shrunken glob would otherwise pass this test by reading nothing.
  expect_gte(length(files), 8L)

  for (f in files) {
    acc <- spec_walk(
      yaml::yaml.load_file(f),
      "$",
      list(
        paths = character(0),
        contexts = character(0)
      )
    )
    paths <- sort(unique(acc$paths))
    cls <- .tte_spec_key_class(paths)
    expect_identical(paths[is.na(cls) | cls == "legacy"], character(0))
  }
})


test_that("the two matching_ratio keys carry different classes", {
  expect_identical(
    .tte_spec_key_class(
      "$/enrollments[]/treatment/implementation/matching_ratio"
    ),
    "legacy"
  )
  expect_identical(
    .tte_spec_key_class("$/standing_methods/matching_ratio_default"),
    "metadata"
  )
  expect_identical(
    .tte_spec_key_class("$/standing_methods/matching_ratio_default/handling"),
    "metadata"
  )
})


# TRUE when `path` has a migration message that contains `needle`. Returns
# FALSE rather than NA for a path that carries no message, so a broken schema
# fails the assertion instead of erroring inside it.
msg_names <- function(path, needle) {
  msg <- .tte_spec_legacy_message(path)
  if (length(msg) != 1L || is.na(msg)) {
    return(FALSE)
  }
  return(grepl(needle, msg, fixed = TRUE))
}


test_that("each legacy key carries a migration message naming its replacement", {
  expect_true(msg_names(
    "$/enrollments[]/treatment/implementation/matching_ratio",
    "comparator_to_intervention_ratio"
  ))
  for (p in .tte_spec_paths("legacy")) {
    if (startsWith(p, "$/inclusion_criteria")) {
      expect_true(msg_names(p, "inclusion_criteria$criteria"))
    }
  }
  # A consumed key has no migration message.
  expect_true(is.na(
    .tte_spec_legacy_message("$/study/implementation/project_prefix")
  ))
})


test_that("the schema leaves an undeclared key path unclassified", {
  expect_true(is.na(.tte_spec_key_class("$/study/implementation/not_a_key")))
  expect_true(is.na(.tte_spec_key_class("$/not_a_section")))
  expect_true(is.na(.tte_spec_key_class("$/not_a_section/child")))
})


test_that("the schema pins the measured legacy and metadata sets", {
  # 15 paths are refused and 10 are accepted without being read. The 15 and 4
  # of them are measured across the 34 parseable specifications of the fleet.
  # The other 6 are the two `standing_methods` blocks no code in `R/` reads.
  # This test needs no fleet, so it is the CI-visible gate on the
  # classification.
  legacy <- c(
    "$/enrollments[]/treatment/implementation/matching_ratio",
    "$/inclusion_criteria/additional_inclusion",
    "$/inclusion_criteria/additional_inclusion[]/implementation",
    "$/inclusion_criteria/additional_inclusion[]/implementation/computed",
    "$/inclusion_criteria/additional_inclusion[]/implementation/source_variable",
    "$/inclusion_criteria/additional_inclusion[]/implementation/window",
    "$/inclusion_criteria/additional_inclusion[]/name",
    "$/inclusion_criteria/additional_inclusion[]/rationale",
    "$/inclusion_criteria/additional_inclusion[]/type",
    "$/inclusion_criteria/implementation",
    "$/inclusion_criteria/implementation/computed",
    "$/inclusion_criteria/implementation/source_variable",
    "$/inclusion_criteria/implementation/window",
    "$/inclusion_criteria/name",
    "$/inclusion_criteria/rationale"
  )
  metadata <- c(
    "$/open_questions[]/resolution",
    "$/standing_methods/admin_censoring",
    "$/standing_methods/admin_censoring/handling",
    "$/standing_methods/admin_censoring/note",
    "$/standing_methods/comparator_to_intervention_ratio_default",
    "$/standing_methods/comparator_to_intervention_ratio_default/handling",
    "$/standing_methods/comparator_to_intervention_ratio_default/note",
    "$/standing_methods/matching_ratio_default",
    "$/standing_methods/matching_ratio_default/handling",
    "$/standing_methods/matching_ratio_default/note"
  )

  expect_identical(.tte_spec_key_class(legacy), rep("legacy", length(legacy)))
  expect_identical(
    .tte_spec_key_class(metadata),
    rep("metadata", length(metadata))
  )
  expect_identical(.tte_spec_paths("legacy"), sort(legacy))
  expect_identical(.tte_spec_paths("metadata"), sort(metadata))

  # 37 mapping contexts are measured across the 34 parseable specifications.
  # The schema declares those plus the contexts no specification uses yet.
  expect_gte(length(.TTE_SPEC_SCHEMA), 37L)
})


test_that("the schema declares the keys swereg reads but no specification uses", {
  # These are the 14 paths the schema declares beyond the 124 the fleet uses.
  # `inclusion_criteria$criteria` is the replacement container. `subgroups`,
  # `observed_var$column` and `study$implementation$conf_level` are read by
  # swereg. No specification in the fleet carries any of them, so the fleet
  # tests above cannot see them. Deleting one from the table would leave every
  # fleet assertion green, so this test is the only thing that pins them.
  declared <- c(
    "$/inclusion_criteria/criteria",
    "$/inclusion_criteria/criteria[]/name",
    "$/inclusion_criteria/criteria[]/rationale",
    "$/inclusion_criteria/criteria[]/type",
    "$/inclusion_criteria/criteria[]/implementation",
    "$/inclusion_criteria/criteria[]/implementation/computed",
    "$/inclusion_criteria/criteria[]/implementation/source_variable",
    "$/inclusion_criteria/criteria[]/implementation/window",
    "$/subgroups",
    "$/subgroups[]/name",
    "$/subgroups[]/implementation",
    "$/subgroups[]/implementation/variable",
    "$/enrollments[]/observed_var/column",
    "$/study/implementation/conf_level"
  )
  expect_identical(
    .tte_spec_key_class(declared),
    rep("consumed", length(declared))
  )
})
