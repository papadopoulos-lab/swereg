# The TARGET checklist is manuscript prose. A paper carries item 7a-h
# paragraph 6g and item 11, so a wrong sentence there is a false methods claim
# that no other test has an opinion about.
#
# These tests read the GENERATED text. They never read the paste0() that
# builds it. The plan below runs no stage: `$print_target_checklist()` needs
# only `plan$spec`, so the spec and the skeleton are enough.

.tfp_cache <- new.env(parent = emptyenv())

.tfp_build <- function(dir) {
  dir_spec <- file.path(dir, "spec")
  dir_tteplan <- file.path(dir, "tteplan")
  dir_results <- file.path(dir, "results")
  dir_meta <- file.path(dir, "meta")
  for (d in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  sk <- ttm_skeleton(
    "A",
    n_persons = 20L,
    date_max = "2016-12-31",
    n_init_bands = 4L
  )
  skel <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel)
  ttm_write_spec(
    file.path(dir_spec, "spec_v001.yaml"),
    "tfp",
    "ri_highrisk"
  )
  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = max(sk$isoyearweek, na.rm = TRUE)
  )
  return(utils::capture.output(plan$print_target_checklist()))
}

# One build for the file. `$print_target_checklist()` prints and returns
# nothing, so the captured lines are the only artefact.
.tfp_lines <- function() {
  if (is.null(.tfp_cache$lines)) {
    dir <- tempfile("tfp_")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
    .tfp_cache$lines <- .tfp_build(dir)
  }
  return(.tfp_cache$lines)
}

# The 6g segment of the item 7a-h paragraph, from its label to the 6h label.
.tfp_6g <- function(lines) {
  txt <- paste(lines, collapse = "\n")
  m <- regexpr(
    "Confounders \\(6g\\):.*?Analysis \\(6h\\):",
    txt,
    perl = TRUE
  )
  if (m[1] == -1L) {
    return(NA_character_)
  }
  return(regmatches(txt, m))
}

# One printed item, from its own title line to the next item's title line.
.tfp_item <- function(lines, n) {
  i <- grep(paste0("Item ", n, "\\. "), lines)[1]
  j <- grep(paste0("Item ", n + 1L, "\\. "), lines)[1]
  if (is.na(i) || is.na(j)) {
    return(NA_character_)
  }
  return(paste(lines[i:(j - 1L)], collapse = "\n"))
}


test_that("the generated 6g paragraph describes carry-forward through follow-up", {
  g <- .tfp_6g(.tfp_lines())
  expect_false(is.na(g))

  # The invariant: single hot-deck at entry, then carry-forward seeded from
  # the entry value, then the reported counts.
  expect_match(g, "singly imputed at trial entry", fixed = TRUE)
  expect_match(g, "hot-deck sampling", fixed = TRUE)
  expect_match(g, "carried forward", fixed = TRUE)
  expect_match(g, "seeded from the entry value", fixed = TRUE)
  expect_match(g, "tteenrollment_fill_summary()", fixed = TRUE)

  # The 26.10.16 sentence. It described one draw per person-trial and no
  # carry-forward at all.
  expect_false(grepl(
    "imputed by sampling from the observed distribution",
    g,
    fixed = TRUE
  ))
})

test_that("checklist item 11 names the fill method and the summary function", {
  it <- .tfp_item(.tfp_lines(), 11L)
  expect_false(is.na(it))
  expect_match(it, "Missing data.", fixed = TRUE)
  expect_match(it, "$s1_impute_confounders()", fixed = TRUE)
  expect_match(it, "$s1b_fill_followup_confounders()", fixed = TRUE)
  expect_match(it, "tteenrollment_fill_summary()", fixed = TRUE)
  expect_false(grepl("(sampling from observed)", it, fixed = TRUE))
})
