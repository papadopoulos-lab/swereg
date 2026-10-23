# tteplan_excel_spec_summary() writes the specification workbook from the spec
# alone. Its Study Specification and Target trial protocol sheets MUST hold the
# cells of plan$excel_spec_summary() for the same spec and code registry. The
# Admin censoring row is the exception. It MUST read no skeleton, rawbatch or
# registry data file.
#
# The spec is spr_report_spec() of helper-sparse_report.R, which carries a
# rare_condition_rule, plus one exclusion criterion on an operation code variable.

skip_if_not_installed("openxlsx")
skip_if_not_installed("yaml")
skip_if_not_installed("withr")

SSX_ADMIN <- "2019-26"
SSX_SPEC_ONLY <- "not built (spec only)"
SSX_SHEETS <- c("Study Specification", "Target trial protocol")

# The expected arrow line is a literal. It is never built from the registry
# entry below, so a code dropped from that entry fails the arrow-line test.
SSX_ARROW <- paste0(
  "↳ LAE20, LAE21, LAE22, LAF10, LAF11, LAF12, LAF30 ",
  "(swereg::add_operations)"
)

# A RegistryStudy whose data directory does not exist. Registration reads no
# data.
ssx_registry <- function(data_dir) {
  study <- swereg::RegistryStudy$new(data_rawbatch_dir = data_dir)
  study$register_codes(
    codes = list(
      oophorectomy = c("LAE20", "LAE21", "LAE22", "LAF10", "LAF11", "LAF12", "LAF30")
    ),
    fn = swereg::add_operations,
    groups = list(os = c("inpatient", "outpatient"))
  )
  study$register_codes(
    codes = list(common = "I10", rare_b = "D50"),
    fn = swereg::add_diagnoses,
    groups = list(osd = c("inpatient", "outpatient"))
  )
  return(study)
}

ssx_spec <- function() {
  spec <- spr_report_spec()
  spec$exclusion_criteria[[2L]] <- list(
    name = "Prior bilateral oophorectomy",
    implementation = list(
      source_variable = "os_oophorectomy",
      window = "lifetime_before_baseline"
    )
  )
  return(spec)
}

# A root directory that holds spec/spec_v001.yaml and nothing else.
ssx_root <- function(spec = ssx_spec(), env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(root, "spec"))
  spr_write_spec(spec, file.path(root, "spec"))
  return(root)
}

# Every cell of every sheet, as text, with the empty rows and columns kept.
ssx_cells <- function(path) {
  sheets <- openxlsx::getSheetNames(path)
  out <- lapply(sheets, function(s) {
    return(openxlsx::read.xlsx(
      path,
      sheet = s,
      colNames = FALSE,
      skipEmptyRows = FALSE,
      skipEmptyCols = FALSE
    ))
  })
  names(out) <- sheets
  return(out)
}

# plan$excel_spec_summary() at s0. The plan names a skeleton file that does
# not exist: with global_max_isoyearweek supplied, s0 reads no skeleton.
ssx_plan_workbook <- function(
  root,
  study,
  protocol_ett_id = NULL,
  period_width = 4L
) {
  dirs <- file.path(root, c("tteplan", "results", "meta"))
  for (d in dirs) {
    dir.create(d, showWarnings = FALSE)
  }
  st <- list(
    skeleton_files = file.path(dirs[1], "absent.qs2"),
    data_meta_dir = dirs[3]
  )
  if (!is.null(study)) {
    st$summary_table <- function() study$summary_table()
  }
  plan <- suppressMessages(swereg::tteplan_from_spec_and_registrystudy(
    study = st,
    candidate_dir_spec = file.path(root, "spec"),
    candidate_dir_tteplan = dirs[1],
    candidate_dir_results = dirs[2],
    spec_version = "v001",
    global_max_isoyearweek = SSX_ADMIN,
    period_width = period_width,
    check_skeletons = FALSE
  ))
  path <- file.path(root, "plan.xlsx")
  suppressMessages(plan$excel_spec_summary(
    path = path,
    protocol_ett_id = protocol_ett_id
  ))
  return(path)
}

ssx_spec_only <- function(root, code_registry, ..., name = "spec_only.xlsx") {
  path <- file.path(root, name)
  suppressMessages(swereg::tteplan_excel_spec_summary(
    spec_path = file.path(root, "spec", "spec_v001.yaml"),
    path = path,
    code_registry = code_registry,
    ...
  ))
  return(path)
}

test_that("the arrow line of an operation variable lists every registry code and names swereg::add_operations", {
  root <- ssx_root()
  path <- ssx_spec_only(root, ssx_registry(file.path(root, "data")))
  ss <- ssx_cells(path)[["Study Specification"]]
  at <- which(ss[[2]] == "os_oophorectomy")
  expect_length(at, 1L)
  expect_identical(ss[[1]][at], "Variable:")
  expect_identical(ss[[2]][at + 1L], SSX_ARROW)
})

test_that("the spec-only workbook equals plan$excel_spec_summary() cell for cell outside the Admin censoring row", {
  root <- ssx_root()
  study <- ssx_registry(file.path(root, "data"))
  plan <- ssx_cells(ssx_plan_workbook(root, study, "ETT00002"))
  only <- ssx_cells(ssx_spec_only(root, study, protocol_ett_id = "ETT00002"))

  expect_identical(names(plan), SSX_SHEETS)
  expect_identical(names(only), SSX_SHEETS)
  expect_identical(
    only[["Target trial protocol"]],
    plan[["Target trial protocol"]]
  )
  expect_match(
    only[["Target trial protocol"]][[1]][1L],
    "ETT00002",
    fixed = TRUE
  )

  a <- plan[["Study Specification"]]
  b <- only[["Study Specification"]]
  row <- which(a[[1]] == "Admin censoring:")
  expect_length(row, 1L)
  expect_identical(a[[2]][row], SSX_ADMIN)
  expect_identical(b[[1]][row], "Admin censoring:")
  expect_identical(b[[2]][row], SSX_SPEC_ONLY)
  expect_identical(b[-row, ], a[-row, ])
  # The registry reached both workbooks.
  expect_true(SSX_ARROW %in% a[[2]])
})

test_that("a supplied global_max_isoyearweek and period_width make every cell equal", {
  root <- ssx_root()
  study <- ssx_registry(file.path(root, "data"))
  plan <- ssx_cells(ssx_plan_workbook(root, study, period_width = 2L))
  only <- ssx_cells(ssx_spec_only(
    root,
    study,
    global_max_isoyearweek = SSX_ADMIN,
    period_width = 2L
  ))
  expect_identical(only, plan)
  # The protocol sheet states the width, so a width of 4 differs.
  four <- ssx_cells(ssx_spec_only(
    root,
    study,
    global_max_isoyearweek = SSX_ADMIN,
    name = "four.xlsx"
  ))
  expect_false(identical(
    four[["Target trial protocol"]],
    plan[["Target trial protocol"]]
  ))
})

test_that("a data.table code registry gives the workbook of the RegistryStudy", {
  root <- ssx_root()
  study <- ssx_registry(file.path(root, "data"))
  from_study <- ssx_cells(ssx_spec_only(root, study, name = "study.xlsx"))
  from_table <- ssx_cells(ssx_spec_only(
    root,
    study$summary_table(),
    name = "table.xlsx"
  ))
  expect_identical(from_table, from_study)
})

test_that("a NULL code registry writes every variable unresolved, as a plan without a registry does", {
  root <- ssx_root()
  plan <- ssx_cells(ssx_plan_workbook(root, NULL))
  only <- ssx_cells(ssx_spec_only(root, NULL))
  a <- plan[["Study Specification"]]
  b <- only[["Study Specification"]]
  row <- which(a[[1]] == "Admin censoring:")
  expect_identical(b[-row, ], a[-row, ])
  expect_identical(
    only[["Target trial protocol"]],
    plan[["Target trial protocol"]]
  )
  # The only arrow line left is the one of the colour legend.
  v <- b[[2]][!is.na(b[[2]])]
  expect_identical(
    v[startsWith(v, "↳")],
    "↳ F64 (swereg::add_diagnoses)"
  )
})

test_that("the spec-only route reads no skeleton, rawbatch or registry data file", {
  root <- ssx_root()
  creg <- ssx_registry(file.path(root, "data"))$summary_table()
  expect_length(
    list.files(root, pattern = "\\.qs2$", recursive = TRUE, all.files = TRUE),
    0L
  )
  before <- list.files(root, recursive = TRUE, all.files = TRUE)
  read_paths <- character()
  testthat::local_mocked_bindings(
    qs2_read = function(file, ...) {
      read_paths <<- c(read_paths, file)
      stop("qs2_read() was called: ", file, call. = FALSE)
    },
    .package = "swereg"
  )
  path <- ssx_spec_only(root, creg)
  after <- list.files(root, recursive = TRUE, all.files = TRUE)
  expect_identical(read_paths, character())
  expect_identical(setdiff(after, before), "spec_only.xlsx")
  expect_identical(openxlsx::getSheetNames(path), SSX_SHEETS)
})

test_that("a RegistryStudy over a data directory reads none of its files", {
  root <- ssx_root()
  data_dir <- file.path(root, "data")
  dir.create(data_dir)
  # Files with the names of a skeleton, a rawbatch and a meta file of batch 1.
  # No qs2 reader can parse them, so a read of any of them stops.
  for (f in c(
    "skeleton_00001.qs2",
    "00001_rawbatch_inpatient.qs2",
    "meta_00001.qs2"
  )) {
    writeBin(as.raw(c(0x00, 0xff, 0x13)), file.path(data_dir, f))
  }
  study <- ssx_registry(data_dir)
  path <- ssx_spec_only(root, study)
  ss <- ssx_cells(path)[["Study Specification"]]
  expect_true(SSX_ARROW %in% ss[[2]])
})

test_that("the workbook has no Rare conditions sheet and states the rule as not decided", {
  root <- ssx_root()
  path <- ssx_spec_only(root, NULL)
  expect_identical(openxlsx::getSheetNames(path), SSX_SHEETS)
  ss <- ssx_cells(path)[["Study Specification"]]
  expect_identical(
    ss[[2]][which(ss[[1]] == "Decision:")],
    "not decided yet (s1 has not run)"
  )
})

test_that("a code_registry that is neither NULL, a RegistryStudy nor a summary table stops", {
  root <- ssx_root()
  expect_error(
    ssx_spec_only(root, list(name = "x")),
    "must be NULL, a RegistryStudy, or a data.table",
    fixed = TRUE
  )
  expect_error(
    ssx_spec_only(root, data.table::data.table(name = "x", codes = "y")),
    "lacks the columns: label, generated_columns",
    fixed = TRUE
  )
  expect_false(file.exists(file.path(root, "spec_only.xlsx")))
})

test_that("both routes document the same ETT for each protocol_ett_id of a 12-ETT grid", {
  spec <- yaml::read_yaml(testthat::test_path("fixtures", "spec_3x2x2.yaml"))
  root <- ssx_root(spec)
  for (ett in c("ETT00001", "ETT00007", "ETT00012")) {
    plan <- ssx_cells(ssx_plan_workbook(root, NULL, ett))
    only <- ssx_cells(ssx_spec_only(root, NULL, protocol_ett_id = ett))
    expect_identical(
      only[["Target trial protocol"]],
      plan[["Target trial protocol"]]
    )
    expect_match(only[["Target trial protocol"]][[1]][1L], ett, fixed = TRUE)
  }
  expect_warning(
    ssx_spec_only(root, NULL, protocol_ett_id = "ETT00013"),
    "not an ETT id of this plan",
    fixed = TRUE
  )
})
