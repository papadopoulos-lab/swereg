#' Add cause of death data to skeleton
#'
#' Searches for specific ICD-10 cause of death codes in Swedish death registry data
#' and adds corresponding boolean variables to the skeleton. Can search in underlying
#' cause of death, multiple causes, or both.
#'
#' @param skeleton A data.table containing the main skeleton structure created by \code{\link{create_skeleton}}
#' @param dataset A data.table containing death registry data with cause of death codes.
#'   Must have columns for person ID, death date (dodsdat), and cause codes (ulorsak, morsak variables)
#' @param id_name Character string specifying the name of the ID variable in the dataset
#' @param cod_type Character string specifying which cause types to search:
#'   \itemize{
#'     \item "both" (default) - Search in both underlying (ulorsak) and multiple (morsak) causes
#'     \item "underlying" - Search only in underlying cause of death (ulorsak)
#'     \item "multiple" - Search only in multiple/contributing causes (morsak variables)
#'   }
#' @param cods Named list of ICD-10 code patterns to search for. Names become variable names in skeleton.
#'   Patterns should NOT include "^" prefix (automatically added). Use exclusions with "!" prefix.
#'   Example: \code{list("cardiovascular_death" = c("I21", "I22"), "external_causes" = c("X60", "X70"))}
#' @return The skeleton data.table is modified by reference with cause of death variables added.
#'   New boolean variables are created for each cause pattern, TRUE when cause is present.
#' @examples
#' # Load fake data
#' data("fake_person_ids", package = "swereg")
#' data("fake_cod", package = "swereg")
#' swereg::make_lowercase_names(fake_cod, date_columns = "dodsdat")
#' 
#' # Create skeleton
#' skeleton <- create_skeleton(fake_person_ids[1:10], "2020-01-01", "2020-12-31")
#' 
#' # Add cause of death data
#' cod_patterns <- list(
#'   "cardiovascular_death" = c("I21", "I22"),
#'   "external_causes" = c("X60", "X70")
#' )
#' add_cods(skeleton, fake_cod, "lopnr", "both", cod_patterns)
#' @seealso \code{\link{create_skeleton}} for creating the skeleton structure,
#'   \code{\link{add_diagnoses}} for diagnosis codes,
#'   \code{\link{add_rx}} for prescription data,
#'   \code{\link{make_lowercase_names}} for data preprocessing
#' @family data_integration
#' @export
add_cods <- function(
    skeleton,
    dataset,
    id_name,
    cod_type = "both",
    cods = list(
      "icd10_F64_0" = c("F640"),
      "icd10_F64_89" = c("F6489"),
      "icd10_F64_089" = c("F640", "F648", "F649")
    )
){
  # Declare variables for data.table non-standard evaluation
  isoyearweek <- is_isoyear <- indatum <- dodsdat <- XXX_EXCLUDE <- NULL
  
  # Validate inputs
  validate_skeleton_structure(skeleton)
  validate_id_column(dataset, id_name)
  validate_death_data(dataset)
  validate_pattern_list(cods, "cause of death patterns")
  validate_date_columns(dataset, c("dodsdat"), "death registry data")
  
  if (!cod_type %in% c("both", "underlying", "multiple")) {
    stop("cod_type must be 'both', 'underlying', or 'multiple', got: '", cod_type, "'")
  }
  
  
  add_diagnoses_or_operations_or_cods(
    skeleton = skeleton,
    dataset = dataset,
    id_name = id_name,
    diags_or_ops_or_cods = cods,
    type = "cods",
    cod_type = cod_type
  )
}

#' Add diagnosis data to skeleton
#'
#' Searches for specific ICD-10 diagnosis codes in Swedish hospital registry data
#' and adds corresponding boolean variables to the skeleton. Can search in main
#' diagnoses only or both main and secondary diagnoses.
#'
#' @param skeleton A data.table containing the main skeleton structure created by \code{\link{create_skeleton}}
#' @param dataset A data.table containing hospital registry data with diagnosis codes.
#'   Must have columns for person ID, date variables, and diagnosis codes (hdia, dia1, dia2, etc.)
#' @param id_name Character string specifying the name of the ID variable in the dataset
#' @param diag_type Character string specifying which diagnosis types to search:
#'   \itemize{
#'     \item "both" (default) - Search in both main (hdia) and secondary (dia1, dia2, etc.) diagnoses
#'     \item "main" - Search only in main diagnoses (hdia)
#'   }
#' @param diags Named list of ICD-10 code patterns to search for. Names become variable names in skeleton.
#'   Patterns should NOT include "^" prefix (automatically added). Use exclusions with "!" prefix.
#'   Example: \code{list("depression" = c("F32", "F33"), "anxiety" = c("F40", "F41"))}
#' @return The skeleton data.table is modified by reference with diagnosis variables added.
#'   New boolean variables are created for each diagnosis pattern, TRUE when diagnosis is present.
#' @examples
#' # Load fake data
#' data("fake_person_ids", package = "swereg")
#' data("fake_inpatient_diagnoses", package = "swereg")
#' swereg::make_lowercase_names(fake_inpatient_diagnoses, date_columns = "indatum")
#' 
#' # Create skeleton
#' skeleton <- create_skeleton(fake_person_ids[1:10], "2020-01-01", "2020-12-31")
#' 
#' # Add diagnoses
#' diag_patterns <- list(
#'   "depression" = c("F32", "F33"),
#'   "anxiety" = c("F40", "F41")
#' )
#' add_diagnoses(skeleton, fake_inpatient_diagnoses, "lopnr", "both", diag_patterns)
#' @seealso \code{\link{create_skeleton}} for creating the skeleton structure,
#'   \code{\link{add_operations}} for surgical procedures,
#'   \code{\link{add_rx}} for prescription data,
#'   \code{\link{make_lowercase_names}} for data preprocessing
#' @family data_integration
#' @export
add_diagnoses <- function(
    skeleton,
    dataset,
    id_name,
    diag_type = "both",
    diags = list(
      "icd10_F64_0" = c("F640"),
      "icd10_F64_89" = c("F6489"),
      "icd10_F64_089" = c("F640", "F648", "F649")
    )
){
  # Declare variables for data.table non-standard evaluation
  isoyearweek <- is_isoyear <- indatum <- dodsdat <- XXX_EXCLUDE <- NULL
  
  # Validate inputs
  validate_skeleton_structure(skeleton)
  validate_id_column(dataset, id_name)
  validate_data_structure(dataset, data_type = "diagnosis data")
  validate_pattern_list(diags, "diagnosis patterns")
  validate_date_columns(dataset, c("indatum"), "diagnosis data")
  
  if (!diag_type %in% c("both", "main")) {
    stop("diag_type must be 'both' or 'main', got: '", diag_type, "'")
  }
  
  # Check for diagnosis code columns
  diag_cols <- c(
    stringr::str_subset(names(dataset), "^hdia"),
    stringr::str_subset(names(dataset), "^dia"),
    stringr::str_subset(names(dataset), "^ekod"),
    stringr::str_subset(names(dataset), "^icdo10")
  )
  
  if (length(diag_cols) == 0) {
    stop("Diagnosis data must have diagnosis code columns (hdia, dia1, dia2, etc.).\n",
         "Available columns: ", paste(names(dataset), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(diagnosis_data)?")
  }

  add_diagnoses_or_operations_or_cods(
    skeleton = skeleton,
    dataset = dataset,
    id_name = id_name,
    diags_or_ops_or_cods = diags,
    type = "diags",
    diag_type = diag_type
  )
}

#' Add surgical operation data to skeleton
#'
#' Searches for specific surgical operation codes in Swedish hospital registry data
#' and adds corresponding boolean variables to the skeleton. Includes predefined
#' operation codes relevant to gender-affirming procedures.
#'
#' @param skeleton A data.table containing the main skeleton structure created by \code{\link{create_skeleton}}
#' @param dataset A data.table containing hospital registry data with operation codes.
#'   Must have columns for person ID, date variables, and operation codes (op1, op2, etc.)
#' @param id_name Character string specifying the name of the ID variable in the dataset
#' @param ops Named list of operation code patterns to search for. Names become variable names in skeleton.
#'   Default includes comprehensive gender-affirming surgery codes:
#'   \itemize{
#'     \item Mastectomy procedures (HAC10, HAC20, etc.)
#'     \item Breast reconstruction (HAD20, HAD30, etc.)
#'     \item Genital operations (various KFH, KGV, LCD, LED, LEE codes)
#'     \item Larynx operations (DQD40)
#'   }
#' @return The skeleton data.table is modified by reference with operation variables added.
#'   New boolean variables are created for each operation pattern, TRUE when operation is present.
#' @examples
#' # Load fake data
#' data("fake_person_ids", package = "swereg")
#' data("fake_inpatient_diagnoses", package = "swereg")
#' swereg::make_lowercase_names(fake_inpatient_diagnoses, date_columns = "indatum")
#' 
#' # Create skeleton
#' skeleton <- create_skeleton(fake_person_ids[1:10], "2020-01-01", "2020-12-31")
#' 
#' # Add operations (using default gender-affirming surgery codes)
#' add_operations(skeleton, fake_inpatient_diagnoses, "lopnr")
#' 
#' # Or specify custom operation codes
#' custom_ops <- list("mastectomy" = c("HAC10", "HAC20"))
#' add_operations(skeleton, fake_inpatient_diagnoses, "lopnr", custom_ops)
#' @seealso \code{\link{create_skeleton}} for creating the skeleton structure,
#'   \code{\link{add_diagnoses}} for diagnosis codes,
#'   \code{\link{add_rx}} for prescription data,
#'   \code{\link{make_lowercase_names}} for data preprocessing
#' @family data_integration
#' @export
add_operations <- function(
    skeleton,
    dataset,
    id_name,
    ops = list(
      "op_afab_mastectomy"= c(
        "HAC10",
        "HAC20",
        "HAC99",
        "HAC15"
      ),
      "op_afab_breast_reconst_and_other_breast_ops" = c(
        "HAD20",
        "HAD30",
        "HAD35",
        "HAD99",
        "HAE99"
      ),
      "op_afab_penis_test_prosth" = c(
        "KFH50",
        "KGV30",
        "KGW96",
        "KGH96"
      ),
      "op_afab_internal_genital" = c(
        "LCD00",
        "LCD01",
        "LCD04",
        "LCD10",
        "LCD11",
        "LCD96",
        "LCD97"
      ),
      "op_afab_colpectomy" = c(
        "LED00"
      ),
      "op_amab_breast_reconst_and_other_breast_ops" = c(
        "HAD00",
        "HAD10",
        "HAD99",
        "HAE00",
        "HAE20",
        "HAE99"
      ),
      "op_amab_reconst_vag" = c(
        "LEE10",
        "LEE40",
        "LEE96",
        "LFE10",
        "LFE96"
      ),
      "op_amab_penis_amp" = c(
        "KGC10"
      ),
      "op_amab_larynx" = c(
        "DQD40"
      )
    )
){
  # Declare variables for data.table non-standard evaluation
  isoyearweek <- is_isoyear <- indatum <- dodsdat <- XXX_EXCLUDE <- NULL
  
  # Validate inputs
  validate_skeleton_structure(skeleton)
  validate_id_column(dataset, id_name)
  validate_data_structure(dataset, data_type = "operation data")
  validate_pattern_list(ops, "operation patterns")
  validate_date_columns(dataset, c("indatum"), "operation data")
  
  # Check for operation code columns
  op_cols <- c(
    stringr::str_subset(names(dataset), "^op")
  )
  
  if (length(op_cols) == 0) {
    stop("Operation data must have operation code columns (op1, op2, etc.).\n",
         "Available columns: ", paste(names(dataset), collapse = ", "), "\n",
         "Did you forget to run make_lowercase_names(operation_data)?")
  }

  add_diagnoses_or_operations_or_cods(
    skeleton = skeleton,
    dataset = dataset,
    id_name = id_name,
    diags_or_ops_or_cods = ops,
    type = "ops"
  )
}


add_diagnoses_or_operations_or_cods <- function(
    skeleton,
    dataset,
    id_name,
    diags_or_ops_or_cods,
    type,
    cod_type = NULL,
    diag_type = NULL
){
  stopifnot(type %in% c("diags", "ops", "cods"))

  if(type == "diags"){
    if(diag_type == "both"){
      variables_containing_codes <- c(
        stringr::str_subset(names(dataset), "^HDIA"),
        stringr::str_subset(names(dataset), "^hdia"),

        stringr::str_subset(names(dataset), "^DIA"),
        stringr::str_subset(names(dataset), "^dia"),

        stringr::str_subset(names(dataset), "^EKOD"),
        stringr::str_subset(names(dataset), "^ekod"),

        stringr::str_subset(names(dataset), "^ICDO10"),
        stringr::str_subset(names(dataset), "^icdo10")
      )
    } else if(diag_type=="main"){
      variables_containing_codes <- c(
        stringr::str_subset(names(dataset), "^HDIA"),
        stringr::str_subset(names(dataset), "^hdia")
      )
    } else {
      stop("invalid diag_type")
    }

    dataset[, isoyearweek := cstime::date_to_isoyearweek_c(indatum)]
    min_isoyearweek <- min(skeleton[is_isoyear==FALSE]$isoyearweek)
    dataset[isoyearweek<min_isoyearweek, isoyearweek := paste0(cstime::date_to_isoyear_c(indatum),"-**")]

  } else if(type == "ops") {
    variables_containing_codes <- c(
      stringr::str_subset(names(dataset), "^OP"),
      stringr::str_subset(names(dataset), "^op")
    )
    dataset[, isoyearweek := cstime::date_to_isoyearweek_c(indatum)]
    min_isoyearweek <- min(skeleton[is_isoyear==FALSE]$isoyearweek)
    dataset[isoyearweek<min_isoyearweek, isoyearweek := paste0(cstime::date_to_isoyear_c(indatum),"-**")]
  } else if(type == "cods") {
    variables_containing_codes_multiple <- c(
      stringr::str_subset(names(dataset), "^MORSAK"),
      stringr::str_subset(names(dataset), "^morsak")
    )
    variables_containing_codes_underlying <- c(
      stringr::str_subset(names(dataset), "^ULORSAK"),
      stringr::str_subset(names(dataset), "^ulorsak")
    )
    if(cod_type == "both"){
      variables_containing_codes <- c(variables_containing_codes_multiple, variables_containing_codes_underlying)
    } else if(cod_type == "underlying"){
      variables_containing_codes <- c(variables_containing_codes_underlying)
    } else if(cod_type == "multiple"){
      variables_containing_codes <- c(variables_containing_codes_multiple)
    } else {
      stop("invalid cod_type")
    }
    dataset[, isoyearweek := cstime::date_to_isoyearweek_c(dodsdat)]
    min_isoyearweek <- min(skeleton[is_isoyear==FALSE]$isoyearweek)
    dataset[isoyearweek<min_isoyearweek, isoyearweek := paste0(cstime::date_to_isoyear_c(dodsdat),"-**")]
  } else stop("")

  for(i in seq_along(diags_or_ops_or_cods)){
    nam <- names(diags_or_ops_or_cods)[i]

    dataset[, (nam) := FALSE]
    dataset[, XXX_EXCLUDE := FALSE]

    for(ii in variables_containing_codes) for(iii in diags_or_ops_or_cods[[i]]){
      # check to see if it is an EXCLUSION factor or not
      if(stringr::str_detect(iii, "^!")){
        iii <- stringr::str_remove(iii, "!")
        iii <- paste0("^", iii)
        dataset[stringr::str_detect(get(ii), iii), XXX_EXCLUDE :=TRUE]
      } else {
        iii <- paste0("^", iii)
        dataset[stringr::str_detect(get(ii), iii), (nam):=TRUE]
      }
    }
    dataset[, (nam) := get(nam)==TRUE & XXX_EXCLUDE==FALSE]
    dataset[, XXX_EXCLUDE := NULL]
  }



  nam <- names(diags_or_ops_or_cods)
  txt <- paste0("reduced <- dataset[, .(", paste0(nam,"=as.logical(max(",nam,"))", collapse=", "),"), keyby=.(",id_name,", isoyearweek)]")
  eval(parse(text = txt))

  nam_left <- paste0(nam,collapse='","')
  nam_left <- paste0('"',nam_left, '"')
  nam_right <- paste0(nam,collapse=',')
  txt <- paste0('skeleton[reduced,on = c("id==',id_name,'","isoyearweek"),c(',nam_left,'):=.(',nam_right,')]')
  eval(parse(text = txt))

  for(i in nam){
    skeleton[is.na(get(i)), (i) := FALSE]
  }
}

