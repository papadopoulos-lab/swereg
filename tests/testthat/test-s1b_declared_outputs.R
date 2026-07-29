# s1b on batchit's declared-output commit engine. Two independent halves, both
# needed:
#   * the WORKER returns list(enrolled_ids = , counts = ) and writes nothing --
#     a captured-dispatch test never runs the worker, so it cannot see this;
#   * the DISPATCH goes through .batch_run_and_write(style = "return") with TWO
#     absolute declared outputs per item -- a direct worker test never touches
#     the call site.
#
# s1b keeps its `work_dir` formal: it READS the s1a `s1a_pre_*` chunks from
# there, so work_dir is an input, not an output-path source. What it loses is
# `enrollment_counts_path` -- a final path handed to a worker -- and the dead
# `s1b_attrition_enr{eid}.qs2` write, which nothing ever read back.

skip_if_not_installed("data.table")
skip_if_not_installed("qs2")
skip_if_not_installed("yaml")

# s1a moved onto batchit's declared-output commit engine with
# `style = "staged_writer"`: it resolves each of its `2 x n_enrollments`
# destinations by NAME through .batch_where_to_write_output(), takes no
# `work_dir`, and is therefore NOT callable outside a
# run_and_write_files_atomically() dispatch. So this fixture issues the REAL
# s1a dispatch instead of calling the worker in-process.
s1a_run_real <- function(skel_path, es_list, spec, work_dir) {
  bn <- basename(skel_path)
  id <- paste0("s1a_", bn)
  items <- list(list(
    file_path = skel_path,
    enrollment_specs = es_list,
    spec = spec
  ))
  names(items) <- id
  eids <- unlist(lapply(es_list, function(e) e$enrollment_id))
  outputs <- list(swereg:::.s1a_outputs_for_skeleton(work_dir, eids, bn))
  names(outputs) <- id
  invisible(utils::capture.output(
    swereg:::.batch_run_and_write(
      target = swereg:::.batch_target("swereg", ".s1a_worker_multi"),
      items = items,
      outputs = outputs,
      style = "staged_writer",
      n_workers = 1L,
      dev_path = swereg:::.swereg_dev_path(),
      label = "s1a"
    ),
    type = "output"
  ))
}

# Build a small real plan (spec YAML + one skeleton .qs2) plus its s1 work
# directory, and run s1a in-process so the s1b inputs exist on disk.
s1b_fixture <- function(env = parent.frame(), run_s1a = TRUE) {
  root <- withr::local_tempdir(.local_envir = env)
  dir_spec <- file.path(root, "spec")
  dir_tteplan <- file.path(root, "tteplan")
  dir_results <- file.path(root, "results")
  dir_meta <- file.path(root, "meta")
  for (d in c(dir_spec, dir_tteplan, dir_results, dir_meta)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  sk <- ttm_skeleton(
    scenario = "A",
    n_persons = 40L,
    date_min = "2018-01-01",
    date_max = "2019-06-30",
    n_init_bands = 8L,
    seed = 4242L
  )
  skel_path <- file.path(dir_tteplan, "skel_a.qs2")
  qs2::qs_save(sk, skel_path)
  ttm_write_spec(
    file.path(dir_spec, "spec_v001.yaml"),
    "s1bdecl",
    "rd_age_continuous"
  )

  plan <- swereg::tteplan_from_spec_and_registrystudy(
    study = list(skeleton_files = skel_path, data_meta_dir = dir_meta),
    candidate_dir_spec = dir_spec,
    candidate_dir_tteplan = dir_tteplan,
    candidate_dir_results = dir_results,
    spec_version = "v001",
    global_max_isoyearweek = sk[, max(isoyearweek, na.rm = TRUE)]
  )

  work_dir <- swereg:::.s1_work_dir(plan, ensure_exists = FALSE)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  es <- plan$enrollment_spec(1)
  es$n_threads <- 1L

  if (run_s1a) {
    s1a_run_real(skel_path, list(es), plan$spec, work_dir)
  }

  list(
    plan = plan,
    spec = plan$spec,
    es = es,
    skel_path = skel_path,
    work_dir = work_dir,
    dir_tteplan = dir_tteplan
  )
}

test_that(".s1b_worker returns enrolled_ids + counts and writes no file", {
  fx <- s1b_fixture()
  eid <- fx$es$enrollment_id

  # work_dir stays: s1b READS the s1a pre-chunks from it. The final counts
  # path does NOT -- a worker is handed no destination.
  need <- names(formals(swereg:::.s1b_worker))
  expect_setequal(
    need,
    c("enrollment_spec", "spec", "work_dir", "skel_basenames")
  )
  expect_false("enrollment_counts_path" %in% need)

  # The dead attrition-file constructor is gone from the namespace entirely.
  expect_false(exists(
    ".s1b_attrition_path",
    envir = asNamespace("swereg"),
    inherits = FALSE
  ))

  enrolled_path <- swereg:::.s1b_enrolled_ids_path(fx$work_dir, eid)
  attrition_path <- file.path(
    fx$work_dir,
    sprintf("s1b_attrition_enr%s.qs2", eid)
  )
  expect_false(file.exists(enrolled_path))
  before <- sort(list.files(fx$work_dir))

  res <- swereg:::.s1b_worker(
    enrollment_spec = fx$es,
    spec = fx$spec,
    work_dir = fx$work_dir,
    skel_basenames = basename(fx$skel_path)
  )

  expect_named(res, c("enrolled_ids", "counts"))

  expect_s3_class(res$enrolled_ids, "data.table")
  expect_true(nrow(res$enrolled_ids) > 0L)
  expect_true(all(
    c("trial_id", "intervention") %in% names(res$enrolled_ids)
  ))

  expect_named(res$counts, c("attrition", "matching"))
  expect_s3_class(res$counts$attrition, "data.table")
  expect_s3_class(res$counts$matching, "data.table")
  expect_true(nrow(res$counts$attrition) > 0L)
  expect_true(all(
    c("trial_id", "criterion", "n_persons", "n_person_trials") %in%
      names(res$counts$attrition)
  ))
  expect_true(all(
    c(
      "trial_id",
      "n_intervention_total",
      "n_comparator_total",
      "n_intervention_enrolled",
      "n_comparator_enrolled"
    ) %in%
      names(res$counts$matching)
  ))

  # The old code wrote all three files itself; the new one must not, and the
  # attrition file must not reappear under any code path.
  expect_false(file.exists(enrolled_path))
  expect_false(file.exists(attrition_path))
  expect_identical(sort(list.files(fx$work_dir)), before)
})

test_that("s1b dispatch declares two absolute outputs via run_and_write", {
  fx <- s1b_fixture(run_s1a = FALSE)

  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    # All four sub-steps now dispatch through .batch_run_and_write(), so the
    # symbol guard below is what selects s1b; every other target returns NULL
    # and runs no subprocess. `.batch_run` is stubbed only because nothing
    # should reach it any more -- if something does, it must not fork.
    .batch_run = function(...) invisible(NULL),
    .batch_run_and_write = function(target, ...) {
      if (!identical(target$symbol, ".s1b_worker")) {
        return(invisible(NULL))
      }
      cap$target <- target
      cap$args <- list(...)
      stop("__CAPTURED__")
    },
    .package = "swereg"
  )

  expect_error(
    suppressWarnings(utils::capture.output(
      fx$plan$s1_generate_enrollments_and_ipw(
        n_workers = 1L,
        swereg_dev_path = "/tmp/fake_dev_path"
      ),
      type = "output"
    )),
    "__CAPTURED__"
  )

  expect_identical(cap$target$symbol, ".s1b_worker")
  expect_identical(cap$target$package, "swereg")

  a <- cap$args
  expect_identical(a$style, "return")
  # s1b runs one item per call, sequentially, with a single subworker.
  expect_identical(a$n_workers, 1L)
  expect_identical(a$dev_path, "/tmp/fake_dev_path")
  expect_identical(a$label, "s1b")
  expect_true(is.function(a$p))

  items <- a$items
  outputs <- a$outputs
  expect_length(items, 1L)
  expect_length(outputs, 1L)
  expect_identical(names(outputs), names(items))
  expect_true(all(grepl("^s1b_", names(items))))

  # Item formals: complete for .s1b_worker (work_dir included, because s1b
  # reads from it), and no output path among them.
  need <- names(formals(swereg:::.s1b_worker))
  expect_setequal(need, c("enrollment_spec", "spec", "work_dir", "skel_basenames"))
  for (it in items) {
    expect_setequal(names(it), need)
    expect_false("enrollment_counts_path" %in% names(it))
  }

  # Two declared outputs per item, in different directories, both absolute.
  # `style = "return"` requires these names to equal the returned list's.
  o <- outputs[[1L]]
  expect_true(is.character(o))
  expect_named(o, c("enrolled_ids", "counts"))
  expect_match(o[["enrolled_ids"]], "^/")
  expect_match(o[["counts"]], "^/")

  it <- items[[1L]]
  eid <- it$enrollment_spec$enrollment_id
  expect_identical(
    o[["enrolled_ids"]],
    swereg:::.s1b_enrolled_ids_path(it$work_dir, eid)
  )
  expect_identical(
    o[["counts"]],
    swereg:::.enrollment_counts_path(
      normalizePath(fx$plan$dir_tteplan, mustWork = FALSE),
      fx$plan$project_prefix,
      eid
    )
  )
  expect_false(identical(dirname(o[["enrolled_ids"]]), dirname(o[["counts"]])))
})
