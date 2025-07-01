read_large_files <- function(){
  large_files <- list()
  # get a list of all the files in "SCB" ---------------------------------
  fs::dir_ls(fs::path(org::project$data_raw, "SCB"))

  # SAME ASSIGNED SeX AT BIRTH FOR CONTROLS
  cat("SCB/fp_lev_fall_och_kontroller_1.sas7bdat", "\n")
  large_files[["SCB/fp_lev_fall_och_kontroller_1.sas7bdat"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "SCB/fp_lev_fall_och_kontroller_1.sas7bdat")
  ) %>%
    swereg::make_lowercase_names() %>%
    setDT()
  # 153 controls who are also cases
  large_files[["SCB/fp_lev_fall_och_kontroller_1.sas7bdat"]] <- large_files[["SCB/fp_lev_fall_och_kontroller_1.sas7bdat"]][!lopnr_kontroll %in% lopnr_fall]

  # OPPOSITE ASSIGNED SeX AT BIRTH FOR CONTROLS
  cat("SCB/fp_lev_fall_och_kontroller_2.sas7bdat", "\n")
  large_files[["SCB/fp_lev_fall_och_kontroller_2.sas7bdat"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "SCB/fp_lev_fall_och_kontroller_2.sas7bdat")
  ) %>%
    swereg::make_lowercase_names() %>%
    setDT()

  # SIBLING CONTROLS
  cat("SCB/fp_lev_helsyskon.sas7bdat", "\n")
  large_files[["SCB/fp_lev_helsyskon.sas7bdat"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "SCB/fp_lev_helsyskon.sas7bdat")
  ) %>%
    swereg::make_lowercase_names() %>%
    setDT()

  # get parent data
  cat("SCB/fp_lev_bioforaldrar.sas7bdat", "\n")
  large_files[["SCB/fp_lev_bioforaldrar.sas7bdat"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "SCB/fp_lev_bioforaldrar.sas7bdat")
  ) %>%
    swereg::make_lowercase_names() %>%
    setDT()

  ## DOB & country of birth----
  cat("SCB/fp_lev_grunduppgifter.sas7bdat", "\n")
  large_files[["SCB/fp_lev_grunduppgifter.sas7bdat"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "SCB/fp_lev_grunduppgifter.sas7bdat"),
    col_select = c("lopnr", "fodelselandgrupp", "fodelseman", "DodDatum")
  ) %>%
    swereg::make_lowercase_names() %>%
    setDT()

  ##adding in migration dates---------------------------
  cat("SCB/fp_lev_migrationer.sas7bdat", "\n")
  large_files[["SCB/fp_lev_migrationer.sas7bdat"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "SCB/fp_lev_migrationer.sas7bdat"),
    col_select = c("LopNr", "Datum", "Posttyp")
  ) %>%
    swereg::make_lowercase_names() %>%
    setDT()

  ## Sex assigned at birth --------------------------------
  cat("SCB/fp_lev_kon_vid_fodelse.sas7bdat", "\n")
  large_files[["SCB/fp_lev_kon_vid_fodelse.sas7bdat"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "SCB/fp_lev_kon_vid_fodelse.sas7bdat")
  ) %>%
    swereg::make_lowercase_names() %>%
    setDT()

  ## Legal Sex Change -------------------------------
  cat("SCB/fp_lev_grunduppgifter.sas7bdat", "\n")
  large_files[["SCB/fp_lev_grunduppgifter.sas7bdat"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "SCB/fp_lev_grunduppgifter.sas7bdat")
  ) %>%
    swereg::make_lowercase_names() %>%
    setDT()

  ## Date of legal sex change
  cat("SCB/fp_lev_konsbyten_datum_ny.sas7bdat", "\n")
  large_files[["SCB/fp_lev_konsbyten_datum_ny.sas7bdat"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "SCB/fp_lev_konsbyten_datum_ny.sas7bdat")
  ) %>%
    swereg::make_lowercase_names() %>%
    setDT()

  # Annual demographics -----------------------------------------------
  ## Family type ----
  for(i in 1990:2023){
    filename <- paste0("SCB/fp_lev_famtyp",i,".sas7bdat")
    cat(filename, "\n")

    d <- haven::read_sas(
      fs::path(org::project$data_raw, filename)
    ) %>%
      swereg::make_lowercase_names() %>%
      setDT()

    # renaming FTYP90 to FamTyp (if it exists)
    if("ftyp90" %in% names(d)) setnames(d, "ftyp90", "famtyp")
    if("ftyp91" %in% names(d)) setnames(d, "ftyp91", "famtyp")
    if("ftyp92" %in% names(d)) setnames(d, "ftyp92", "famtyp")
    if("ftyp93" %in% names(d)) setnames(d, "ftyp93", "famtyp")
    if("ftyp94" %in% names(d)) setnames(d, "ftyp94", "famtyp")
    if("ftyp95" %in% names(d)) setnames(d, "ftyp95", "famtyp")
    if("ftyp96" %in% names(d)) setnames(d, "ftyp96", "famtyp")
    if("ftyp97" %in% names(d)) setnames(d, "ftyp97", "famtyp")

    large_files[[filename]] <- d
  }

  ##Participant education ----
  for(i in 1990:2023){
    if(i==2023){
      filename_in <- paste0("SCB/fp_lev_utbniva",2022,".sas7bdat")
    } else {
      filename_in <- paste0("SCB/fp_lev_utbniva",i,".sas7bdat")
    }
    filename_out <- paste0("SCB/fp_lev_utbniva",i,".sas7bdat")

    cat(filename_out, "\n")

    d <- haven::read_sas(
      fs::path(org::project$data_raw, filename_in)
    ) %>%
      swereg::make_lowercase_names() %>%
      setDT()

    if("sun2000niva_old" %in% names(d)) setnames(d, "sun2000niva_old", "edu_utbniva")
    if("sun2020niva_old" %in% names(d)) setnames(d, "sun2020niva_old", "edu_utbniva")

    large_files[[filename_out]] <- d
  }

  ##Participant income ----
  for(i in 1990:2023){
    if(i==2023){
      filename_in <- paste0("SCB/fp_lev_cdisp",2022,".sas7bdat")
    } else {
      filename_in <- paste0("SCB/fp_lev_cdisp",i,".sas7bdat")
    }
    filename_out <- paste0("SCB/fp_lev_cdisp",i,".sas7bdat")

    cat(filename_out, "\n")

    d <- haven::read_sas(
      fs::path(org::project$data_raw, filename_in)
    ) %>%
      swereg::make_lowercase_names() %>%
      setDT()

    large_files[[filename_out]] <- d
  }

  # Annual ICD-10 (long running time)-------------------------------------------
  ## Dx and surgeries ----
  # outpatient
  cat("diagnoses_and_operations", "\n")

  ov <- haven::read_sas(fs::path(org::project$data_raw, "Sos", "ut_r_par_ov_37133_2022.sas7bdat")) %>%
    swereg::make_lowercase_names() %>%
    setDT() # convert to data.table
  # inpatient
  sv <- haven::read_sas(fs::path(org::project$data_raw, "Sos", "ut_r_par_sv_37133_2022.sas7bdat")) %>%
    swereg::make_lowercase_names() %>%
    setDT()
  diagnoses_and_operations <- rbindlist(list(ov, sv), use.names=T, fill=T) # put ov and sv on top of each other
  rm("ov", "sv") # remove ov and sv from the RAM of the computer

  # names(diagnoses_and_operations)
  suppressWarnings(diagnoses_and_operations[, indatum := lubridate::ymd(indatuma)])
  # get rid of the 16 ones that dont have a date
  large_files[["diagnoses_and_operations"]] <- diagnoses_and_operations[!is.na(indatum)]
  rm("diagnoses_and_operations")

  ## GA Meds (long running time)-------------------------------------------
  cat("lmed", "\n")

  large_files[["lmed"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "Sos", "ut_r_lmed_fk_62015_2024.sas7bdat"),
    n_max = Inf,
    col_select = dplyr::contains(c("atc", "lopnr", "edatum","fdatum", "fddd"))
    ) %>%
    swereg::make_lowercase_names() %>%
    setDT()

  ## Cause of death-------------------------------------------
  #1952-2022, UT_R_DORS_FK_37133_2022 has date of death
  #2023, UT_R_DORS_AVI_FK_37133_2022 has cause and date
  # ulorsak = underlying cause of death, morsak1-48 are icd codes

  # causedeath <- haven::read_sas(fs::path(org::project$data_raw, "Sos", "ut_r_dors_fk_37133_2022.sas7bdat")) %>%
  #   swereg::make_lowercase_names() %>%
  #   dplyr::filter(lopnr %in% ids) %>%
  #   dplyr::mutate(dodsdat1 = stringr::str_replace(dodsdat, "^195410$", "19541015")) %>%
  #   dplyr::mutate(dodsdat1 = stringr::str_replace(dodsdat1, "0000$", "0701")) %>%
  #   dplyr::mutate(dodsdat1 = stringr::str_replace(dodsdat1, "00$", "15")) %>%
  #   dplyr::mutate(dodsdat1 = lubridate::ymd(dodsdat1)) %>%
  #   setDT()
  # causedeath[is.na(dodsdat1)]

  cat("causedeath", "\n")

  large_files[["causedeath"]] <- haven::read_sas(fs::path(org::project$data_raw, "Sos", "ut_r_dors_fk_37133_2022.sas7bdat")) %>%
    swereg::make_lowercase_names() %>%
    dplyr::mutate(dodsdat = stringr::str_replace(dodsdat, "^195410$", "19541015")) %>%
    dplyr::mutate(dodsdat = stringr::str_replace(dodsdat, "0000$", "0701")) %>%
    dplyr::mutate(dodsdat = stringr::str_replace(dodsdat, "00$", "15")) %>%
    dplyr::mutate(dodsdat = lubridate::ymd(dodsdat)) %>%
    setDT()

  # get a list of all the files in "NKCx" ---------------------------------
  fs::dir_ls(fs::path(org::project$data_raw, "NKCx"))

  for(i in fs::dir_ls(fs::path(org::project$data_raw, "NKCx"))){
    filename <- paste0("NKCx/", fs::path_file(i))

    cat(filename, "\n")
    large_files[[filename]] <- haven::read_sas(
      fs::path(org::project$data_raw, filename)
    ) %>%
      swereg::make_lowercase_names() %>%
      setDT()
  }

  head(large_files[["NKCx/fp_lev_nkcx_nkc_cell_6923.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_ext_hpv.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_hpv.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_hpv_all.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_inv_9323.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_inv_type.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_klartext.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_pad_6923.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_pad_translated.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_snomed_cyt.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_snomed_hpv.sas7bdat"]])
  head(large_files[["NKCx/fp_lev_nkcx_nkc_snomed_pad.sas7bdat"]])

  # FHM/vaccines ---------------------------------
  cat("FHM/fp_lev_fhm_nvr.sas7bdat", "\n")
  large_files[["FHM/fp_lev_fhm_nvr.sas7bdat"]] <- haven::read_sas(
    fs::path(org::project$data_raw, "FHM/fp_lev_fhm_nvr.sas7bdat")
  ) %>%
    swereg::make_lowercase_names() %>%
    setDT()

  # final cleaning ----
  for(i in names(large_files)) setDT(large_files[[i]])

  return(large_files)
}

