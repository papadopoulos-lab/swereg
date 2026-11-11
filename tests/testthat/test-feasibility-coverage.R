# The coverage sheets of feasibility.xlsx: a spec column that the data does
# not hold is flagged, in the whole dataset and in each enrollment and ETT.

.cv_spec <- function() {
  return(list(
    outcomes = list(
      list(name = "Fracture", implementation = list(
        variable = list("os_fracture"), variable_combined = "os_fracture"
      )),
      list(name = "Osteoporosis", implementation = list(
        variable = list("os_m81"), variable_combined = "os_m81"
      ))
    ),
    exclusion_criteria = list(
      list(name = "Prior fracture", implementation = list(
        source_variable = list("os_fracture", "os_m80")
      ))
    ),
    confounders = list(
      list(name = "Diabetes", implementation = list(
        variable = "rd_no_os_e11_everbefore",
        source_variable = list("os_e11")
      )),
      list(name = "Lupus", implementation = list(
        variable = "rd_no_os_m32_everbefore",
        source_variable = list("os_m32")
      )),
      list(
        name = "Civil status",
        categories = c("married", "widowed"),
        implementation = list(variable = "rd_civil_status")
      )
    ),
    rare_condition_rule = list(
      min_n = 100,
      candidates = list("rd_no_os_m32_everbefore", "rd_no_os_e11_everbefore")
    ),
    enrollments = list(list(
      id = "A",
      treatment = list(
        arms = list(intervention = "Oral", comparator = "Transdermal"),
        implementation = list(
          variable = "rd_route",
          intervention_value = "oral",
          comparator_value = "transdermal"
        )
      )
    ))
  ))
}

.cv_summary <- function() {
  return(data.table::data.table(
    column_name = c(
      "os_fracture", "os_m81", "os_m80", "os_e11", "os_m32",
      "rd_civil_status=married", "rd_route=oral", "rd_route=transdermal"
    ),
    n_persons_with = c(0L, 5000L, 3L, 20000L, 40L, 9000L, 800L, 0L)
  ))
}

test_that("the spec columns carry their role, sources and levels", {
  tab <- swereg:::.tte_spec_columns(.cv_spec())
  expect_true(all(c("os_fracture", "os_m80", "os_e11", "rd_route") %in% tab$column))
  expect_identical(
    sort(tab[column == "rd_civil_status"]$level),
    c("married", "widowed")
  )
  expect_identical(
    unique(tab[column == "os_m32"]$role),
    "Confounder (rare-condition candidate)"
  )
  expect_identical(unique(tab[column == "rd_route"]$scope), "A")
})

test_that("whole-dataset coverage flags absent and rare columns and links the rare-condition rule", {
  plan <- list(spec = .cv_spec(), sparse_decision = NULL)
  raw <- swereg:::.tte_coverage_raw(plan, .cv_summary())$table
  f <- function(col, lv = NA_character_) {
    r <- raw[column == col & (is.na(level) & is.na(lv) | level %in% lv)]
    return(unique(r$flag))
  }
  absent <- swereg:::.COVERAGE_FLAGS[["absent"]]
  expect_identical(f("os_fracture"), absent)
  expect_identical(f("os_m80"), swereg:::.COVERAGE_FLAGS[["rare"]])
  expect_identical(f("os_m81"), "")
  # A declared level the summary does not hold has no person.
  expect_identical(f("rd_civil_status", "widowed"), absent)
  expect_identical(f("rd_route", "transdermal"), absent)
  expect_match(unique(raw[column == "os_m32"]$rare_rule), "^Certain exclusion")
  expect_match(unique(raw[column == "os_e11"]$rare_rule), "^Decided at s1b")
})

test_that("ETT coverage flags a criterion that removes nobody, an empty arm and an outcome that passes no cut", {
  att <- data.table::data.table(
    enrollment_id = "A",
    enrollment_period_id = NA_integer_,
    step_order = 1:3,
    step_name = c("before_exclusions", "eligible_no_os_fracture_lifetime", "eligible_no_os_m80_lifetime"),
    n_persons = c(1000, 1000, 990),
    n_person_trials = c(5000, 5000, 4950),
    n_arm_intervention = c(500, 500, 495),
    n_arm_comparator = c(4500, 4500, 4455)
  )
  plan <- list(
    spec = .cv_spec(),
    sparse_decision = data.table::data.table(
      variable = "rd_no_os_m32_everbefore",
      decision = "excluded"
    ),
    ett = data.table::data.table(
      ett_id = c("ETT1", "ETT2"),
      enrollment_id = "A",
      outcome_var = c("os_fracture", "os_m81"),
      outcome_name = c("Fracture", "Osteoporosis"),
      follow_up = 52
    ),
    get_attrition = function() {
      return(att)
    }
  )
  cov <- list(A = data.table::data.table(
    variable = c("rd_no_os_e11_everbefore", "rd_no_os_m32_everbefore", "rd_civil_status"),
    level = c("TRUE", "TRUE", "married"),
    n_intervention = c(40, 0, 300),
    n_comparator = c(0, 0, 3000)
  ))
  records <- list(
    ETT1 = list(max_passing_cut = NA_real_, status = "withheld"),
    ETT2 = list(max_passing_cut = 5, status = "reported")
  )
  tab <- swereg:::.tte_coverage_ett(plan, cov, records)
  fl <- swereg:::.COVERAGE_FLAGS
  elig <- tab[section == "Eligibility"]
  expect_identical(elig$flag, c(fl[["removes_nobody"]], ""))
  conf <- tab[section == "Confounder at baseline"]
  expect_identical(conf[variable == "rd_no_os_e11_everbefore"]$flag, fl[["arm_absent"]])
  # Excluded by the rare-condition rule: 0 is expected, so no flag.
  expect_identical(conf[variable == "rd_no_os_m32_everbefore"]$flag, "")
  # The declared category widowed has no baseline row.
  expect_identical(conf[level == "widowed"]$flag, fl[["arm_absent"]])
  out <- tab[section == "Outcome"]
  expect_identical(out[ett_id == "ETT1"]$flag, fl[["no_cut"]])
  expect_identical(out[ett_id == "ETT2"]$flag, "")
})

test_that("baseline confounder counts are per arm at time zero", {
  d <- data.table::data.table(
    tstart = c(0, 1, 0, 0),
    tx = c(TRUE, TRUE, FALSE, FALSE),
    dm = c(TRUE, TRUE, TRUE, FALSE),
    civ = c("married", "married", "widowed", NA),
    age = c(50, 50, NA, 55)
  )
  enr <- list(
    data = d,
    design = list(
      tstart_var = "tstart",
      treatment_var = "tx",
      confounder_vars = c("dm", "civ", "age")
    )
  )
  cov <- swereg:::.tte_confounder_coverage(enr)
  expect_identical(cov[variable == "dm"]$n_intervention, 1L)
  expect_identical(cov[variable == "dm"]$n_comparator, 1L)
  expect_identical(cov[variable == "civ" & level == "widowed"]$n_comparator, 1L)
  expect_identical(cov[variable == "age"]$n_comparator, 1L)
})

test_that("the two coverage sheets are written when nothing is stored", {
  skip_if_not_installed("openxlsx")
  plan <- list(
    spec = .cv_spec(),
    sparse_decision = NULL,
    registrystudy = NULL,
    ett = data.table::data.table(),
    get_attrition = function() {
      return(data.table::data.table())
    }
  )
  wb <- openxlsx::createWorkbook()
  swereg:::.write_coverage_sheets(wb, plan, list(), list(), openxlsx::createStyle())
  expect_identical(names(wb), c("Coverage (whole dataset)", "Coverage (ETT)"))
})
