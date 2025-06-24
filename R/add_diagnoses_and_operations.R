#' Add cause of death data to skeleton
#'
#' Searches for specific ICD-10 cause of death codes in Swedish death registry data
#' and adds corresponding boolean variables to the skeleton. Can search in underlying
#' cause of death, multiple causes, or both.
#'
#' @param skeleton A data.table containing the main skeleton structure
#' @param dataset A data.table containing death registry data with cause of death codes
#' @param id_name Character string specifying the name of the ID variable
#' @param cod_type Character string specifying which cause types to search:
#'   "both" (default), "underlying", or "multiple"
#' @param cods Named list of ICD-10 code patterns to search for. Names become variable names in skeleton.
#'   Patterns can use regex (e.g., "^F640") and exclusions with "!" prefix
#' @return The skeleton data.table is modified by reference with cause of death variables added
#' @examples
#' \dontrun{
#' cod_list <- list("gender_dysphoria" = c("^F64"))
#' add_cods(skeleton, death_data, "lopnr", "both", cod_list)
#' }
#' @export
add_cods <- function(
    skeleton,
    dataset,
    id_name,
    cod_type = "both",
    cods = list(
      "icd10_F64_0" = c("^F640"),
      "icd10_F64_89" = c("^F6489"),
      "icd10_F64_089" = c("^F640", "^F648", "^F649")
    )
){

  stopifnot(cod_type %in% c("both", "underlying", "multiple"))
  # if dodsdat is a character
  # -> replace missing months with 0701
  # -> replace missing days with 15
  # -> turn into date
  if(inherits(dataset$dodsdat, "character")){
    d[, dodsdat := stringr::str_replace(dodsdat, "0000$", "0701")]
    d[, dodsdat := stringr::str_replace(dodsdat, "00$", "15")]
    d[, dodsdat := lubridate::ymd(dodsdat)]
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
#' @param skeleton A data.table containing the main skeleton structure
#' @param dataset A data.table containing hospital registry data with diagnosis codes
#' @param id_name Character string specifying the name of the ID variable
#' @param diag_type Character string specifying which diagnosis types to search:
#'   "both" (default) for main and secondary, or "main" for main diagnoses only
#' @param diags Named list of ICD-10 code patterns to search for. Names become variable names in skeleton.
#'   Patterns can use regex (e.g., "^F640") and exclusions with "!" prefix
#' @return The skeleton data.table is modified by reference with diagnosis variables added
#' @examples
#' \dontrun{
#' diag_list <- list("gender_dysphoria" = c("^F64"))
#' add_diagnoses(skeleton, hospital_data, "lopnr", "both", diag_list)
#' }
#' @export
add_diagnoses <- function(
    skeleton,
    dataset,
    id_name,
    diag_type = "both",
    diags = list(
      "icd10_F64_0" = c("^F640"),
      "icd10_F64_89" = c("^F6489"),
      "icd10_F64_089" = c("^F640", "^F648", "^F649")
    )
){
  stopifnot(diag_type %in% c("both", "main"))

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
#' @param skeleton A data.table containing the main skeleton structure
#' @param dataset A data.table containing hospital registry data with operation codes
#' @param id_name Character string specifying the name of the ID variable
#' @param ops Named list of operation code patterns to search for. Names become variable names in skeleton.
#'   Default includes gender-affirming surgery codes (mastectomy, genital operations, etc.)
#' @return The skeleton data.table is modified by reference with operation variables added
#' @examples
#' \dontrun{
#' op_list <- list("mastectomy" = c("HAC10", "HAC20"))
#' add_operations(skeleton, hospital_data, "lopnr", op_list)
#' }
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

