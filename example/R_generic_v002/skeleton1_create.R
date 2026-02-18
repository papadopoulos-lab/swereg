skeleton1_create <- function(file_number = 1, ids_batch, id_master, large_files){
  # this will only run if you directly highlight it from inside here
  if(plnr::is_run_directly()){
    file_number <- 1
    ids_batch = ids_gd[[file_number]]
  }

  # get a list of all the files in "SCB" ---------------------------------
  #fs::dir_ls(fs::path(org::project$data_raw, "SCB"))

  # convert ids_batch into the different groups of people
  ids_control_same <- ids_batch$control_same
  ids_control_opposite <- ids_batch$control_opposite
  ids_control_sibling <- ids_batch$control_sibling
  ids_gd <- ids_batch$case
  ids_father <- ids_batch$father
  ids_mother <- ids_batch$mother
  ids <- ids_batch$all

  # Initial setup----
  # create the initial skeleton
  skeleton <- swereg::create_skeleton(
    ids = ids,
    date_min = "2000-01-01",
    date_max = "2023-12-31"
  )

  #Richard to fix in swereg
  skeleton[is_isoyear==FALSE, isoyearweeksun := cstime::isoyearweek_to_last_date(isoyearweek)]
  skeleton[is_isoyear==TRUE, isoyearweeksun := cstime::isoyearweek_to_last_date(paste0(isoyear,"-26"))]
  skeleton[is.na(isoyearweeksun), isoyearweeksun := as.Date(paste0(isoyear,"-06-28"))]

  # tag with register definitions of who the people are
  skeleton[id %in% ids_control_same, rowind_register_tag := "control_same_sex"]
  skeleton[id %in% ids_control_opposite, rowind_register_tag := "control_opposite_sex"]
  skeleton[id %in% ids_control_sibling, rowind_register_tag := "control_sibling"]
  skeleton[id %in% ids_gd, rowind_register_tag := "case"]
  skeleton[id %in% ids_father, rowind_register_tag := "father"]
  skeleton[id %in% ids_mother, rowind_register_tag := "mother"]

  skeleton[, uniqueN(id), by=rowind_register_tag]

  # check how many GDs are also fathers
  sum(ids_gd %in% ids_father)
  # check how many GDs are also mothers
  sum(ids_gd %in% ids_mother)

  #looking at what column names are in the grunduppgifter file
  #sas_file_path <- fs::path(org::project$data_raw, "SCB/fp_lev_migrationer.sas7bdat")
  #data_preview <- haven::read_sas(sas_file_path, n_max = 1)
  #column_names <- colnames(data_preview)
  #print(column_names)

  # One-time demos -----------------------------
  ## DOB & country of birth----
  d <- large_files[["SCB/fp_lev_grunduppgifter.sas7bdat"]] %>%
    dplyr::filter(lopnr %in% ids)

  swereg::add_onetime(
    skeleton,
    d,
    id_name = "lopnr"
  )
  ##adding in migration dates---------------------------
  d <- large_files[["SCB/fp_lev_migrationer.sas7bdat"]][lopnr %in% ids]

  setorder(d, lopnr, datum)
  d[, obs_id := 1:.N, by=.(lopnr, posttyp)]
  d <- d[obs_id==1]
  d <- dcast.data.table(
    d,
    lopnr ~ posttyp,
    value.var = "datum"
  )
  d[Utv < Inv, Inv := NA]
  d[Inv < Utv]
  d[is.na(Inv)]
  d[is.na(Utv)]
  d[, rowind_isoyearweek_immigration := cstime::date_to_isoyearweek_c(lubridate::ymd(Inv))]
  d[, rowind_isoyearweek_emmigration := cstime::date_to_isoyearweek_c(lubridate::ymd(Utv))]
  d[, Inv := NULL]
  d[, Utv := NULL]

  swereg::add_onetime(
    skeleton,
    d, # we already loaded this in up above
    id_name = "lopnr"
  )
  skeleton[1,]

  ## Parents data -----------------------------------
  d_parent <- large_files[["SCB/fp_lev_bioforaldrar.sas7bdat"]][lopnr %in% ids]

  # adding 2 new columns:^
  # - mother id number
  # - father id number
  swereg::add_onetime(
    skeleton,
    d_parent, # we already loaded this in up above
    id_name = "lopnr"
  )
  skeleton[1,]

  ## Sex assigned at birth --------------------------------
  d <- large_files[["SCB/fp_lev_kon_vid_fodelse.sas7bdat"]][lopnr %in% ids]

  # binary for analysis
  d[, is_amab := kon==1]
  d[, kon := NULL]

  # pretty version for tables/graphs
  d[, saab := "Female"]
  d[is_amab==T, saab := "Male"]

  swereg::add_onetime(
    skeleton,
    d,
    id_name = "lopnr"
  )

  ## Legal Sex Change -------------------------------
  d <- large_files[["SCB/fp_lev_grunduppgifter.sas7bdat"]][lopnr %in% ids]

  # binary for analysis
  d[, is_legal_male := kon==1]
  d[, kon := NULL]

  swereg::add_onetime(
    skeleton,
    d,
    id_name = "lopnr"
  )

  # making legal sex change variable============================
  skeleton[, legal_sex_change := is_legal_male != is_amab]

  #print(skeleton)
  ## Date of legal sex change
  # here i don't restrict it, because it's a very small dataset
  # and we are reshaping it to wide, so i need to maintain the same
  # wide format, which is much easier if i don't restrict it
  d <- large_files[["SCB/fp_lev_konsbyten_datum_ny.sas7bdat"]] %>%
    copy()

  # make it into wide format
  setorder(d, lopnr, konsbyte_datum)
  d[, event_number := 1:.N, by=.(lopnr)]
  d <- dcast.data.table(
    d,
    lopnr ~ event_number,
    value.var = "konsbyte_datum"
  )
  setnames(d, c("lopnr", "sexchange", "sexreversal"))
  d[, rowind_isoyearweek_legal_sex_change_first := cstime::date_to_isoyearweek_c(lubridate::ymd(sexchange))]
  d[, rowind_isoyearweek_legal_sex_change_reversal := cstime::date_to_isoyearweek_c(lubridate::ymd(sexreversal))]
  d[, rowind_isoyearweeksun_legal_sex_change_first := cstime::isoyearweek_to_last_date(rowind_isoyearweek_legal_sex_change_first)]
  d[, rowind_isoyearweeksun_legal_sex_change_reversal := cstime::isoyearweek_to_last_date(rowind_isoyearweek_legal_sex_change_reversal)]
  d[, sexchange := NULL]
  d[, sexreversal := NULL]

  swereg::add_onetime(
    skeleton,
    d,
    id_name = "lopnr"
  )

  ## Parents data ---------------------------------------------------
  # adding 2 new columns:^
  # - mother id number
  # - father id number
  swereg::add_onetime(
    skeleton,
    d_parent, # we already loaded this in up above
    id_name = "lopnr"
  )
  skeleton[1,]

  # Annual demographics -----------------------------------------------
  ## Family type ----
  for(i in 1990:2023){
    filename <- paste0("SCB/fp_lev_famtyp",i,".sas7bdat")
    #}
    d <- large_files[[filename]][lopnr %in% ids]

    swereg::add_annual(
      skeleton,
      d,
      id_name = "lopnr",
      isoyear = i
    )
  }

  names(skeleton)

  ##Participant education ----
  for(i in 1990:2023){
    filename <- paste0("SCB/fp_lev_utbniva",i,".sas7bdat")

    d <- large_files[[filename]][lopnr %in% ids]

    swereg::add_annual(
      skeleton,
      d,
      id_name = "lopnr",
      isoyear = i
    )
  }

  names(skeleton)

  ##Participant income ----
  for(i in 1990:2023){
    filename <- paste0("SCB/fp_lev_cdisp",i,".sas7bdat")

    d <- large_files[[filename]][lopnr %in% ids]

    swereg::add_annual(
      skeleton,
      d,
      id_name = "lopnr",
      isoyear = i
    )
  }

  names(skeleton)

  # Annual ICD-10 (long running time)-------------------------------------------
  ## Dx and surgeries ----
  # outpatient
  # ov <- haven::read_sas(fs::path(org::project$data_raw, "Sos", "ut_r_par_ov_37133_2022.sas7bdat")) %>%
  #   swereg::make_lowercase_names() %>%
  #   dplyr::filter(lopnr %in% ids) %>%
  #   setDT() # convert to data.table
  # # inpatient
  # sv <- haven::read_sas(fs::path(org::project$data_raw, "Sos", "ut_r_par_sv_37133_2022.sas7bdat")) %>%
  #   swereg::make_lowercase_names() %>%
  #   dplyr::filter(lopnr %in% ids) %>%
  #   setDT()
  # diagnoses_and_operations <- rbindlist(list(ov, sv), use.names=T, fill=T) # put ov and sv on top of each other
  # rm("ov", "sv") # remove ov and sv from the RAM of the computer



  # # names(diagnoses_and_operations)
  # suppressWarnings(diagnoses_and_operations[, indatum := lubridate::ymd(indatuma)])
  # # get rid of the 16 ones that dont have a date
  # diagnoses_and_operations <- diagnoses_and_operations[!is.na(indatum)]

  names(large_files)
  diagnoses_and_operations <- large_files[["diagnoses_and_operations"]][lopnr %in% ids]

  swereg::add_diagnoses(
    skeleton,
    diagnoses_and_operations,
    id_name = "lopnr",
    diags = list(
      "diag_gd_icd10_F64_0" = c("F640"),
      "diag_gd_icd10_F64_89" = c("F6489"),
      "diag_gd_icd10_F64_089" = c("F640", "F648", "F649"),
      "diag_gd_icd89_transsexual" = c("302[A-Z]", "302,31", "302,99"),

      "diag_psychiatric_not_gd" = c(
        "F", "!F640", "!F648", "!F649", # ICD10: F00-F99
        "29[0-9][A-Z]", "3[0-1][0-9][A-Z]", "!302[A-Z]", # ICD9: 290-319,
        "29[0-9],", "30[0-9],", "31[0-5],", "!302,31", "!302,99" # ICD8: 290-315
      ),

      "diag_intellectual_disability" = c(
        "F7", # ICD10
        "31[7-9][A-Z]",  # ICD9
        "31[0-5]," # ICD8
      ),

      "diag_speech_language" = c(
        "F80", "R47",  # ICD10
        "315D", # ICD9
        "360,00", "781,59" # ICD8
      ),

      "diag_psychotic" = c(
        "F2", # ICD10
        "29[578][A-Z]", "!298A", # ICD9
        "29[5789],", "!298,00" # ICD8
      ),

      "diag_bipolar" = c(
        "F3[01]", # ICD10
        "296[A-Z]", "!296[BX]", # ICD9
        "296,", "!296,00" # ICD8
      ),

      "diag_depression" = c(
        "F3[23]", # ICD10
        "298A", "300E", "311[A-Z]", # ICD9
        "298,00", "300,40", "790,20" # ICD8
      ),

      "diag_eating" = c(
        "F50", # ICD10
        "307[BF]", # ICD9
        "306,50" # ICD8
      ),

      "diag_asd" = c(
        "F84", # ICD10
        "299[A-Z]" # ICD9
      ),

      "diag_adhd" = c(
        "F90", # ICD10
        "F314[A-Z]" # ICD9
      ),

      "diag_asd_or_adhd" = c(
        "F84", "F90", # ICD10
        "299[A-Z]", "F314[A-Z]" # ICD9
      ),

      "diag_other_behavioral_emotional_onset_in_childhood" = c(
        "F9[1-8]", # ICD10
        "F31[23][A-Z]", # ICD9
        "F308,99" # ICD8
      ),

      "diag_neurotic_stress_related_or_somatoform" = c(
        "F4[0-8]", # ICD10
        "300[A-Z]", "!300E", "30[689][A-Z]", "307W", # ICD9
        "300,", "!300,40", "305,", "306,80", "306,98", "307,99" # ICD8
      ),

      "diag_alcohol_substance_use" = c(
        "F1[0-689]", # ICD10
        "29[12][A-Z]", "30[34][A-Z]", "305[AX]", # ICD9
        "291,", "294,30", "30[34],", "971," # ICD8
      ),

      "diag_personality" = c(
        "F60", # ICD10
        "301[A-Z]", # ICD9
        "301," # ICD8
      ),

      "diag_suicide_attempt" = c(
        "X[67]", "X8[0-4]", # ICD10
        "E95[0-9][A-Z]", # ICD9
        "E95[0-9]," # ICD8
      )
    )
  )

  swereg::add_operations(
    skeleton,
    diagnoses_and_operations,
    id_name = "lopnr",
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

      "op_amab_orchidectomy" = c(
        "KFC10",
        "KFC15"
      ),

      "op_amab_larynx" = c(
        "DQD40"
      )
    )
  )

  rm("diagnoses_and_operations")

  names(skeleton)


  ## Medications (long running time)-------------------------------------------
  # lmed <- haven::read_sas(fs::path(org::project$data_raw, "Sos", "ut_r_lmed_fk_62015_2024.sas7bdat")) %>%
  #   swereg::make_lowercase_names() %>%
  #   dplyr::filter(lopnr %in% ids) %>%
  #   setDT()

  lmed <- large_files[["lmed"]][lopnr %in% ids]

  swereg::add_rx(
    skeleton,
    lmed,
    id_name = "lopnr",
    rxs = list(
      "rx_hormones_pubblock"= c(
        "L02AE",
        "H01CA"
      ),
      "rx_hormones_testosterone" = c(
        "G03B"
      ),
      "rx_hormones_prog_estandro"= c(
        "G03C",
        "L02AA",
        "G03D",
        "L02AB",
        "G03H",
        "L02BB",
        "G04CB",
        "C03DA01"
      )
  #   "rx_hormones_androgens"= c(
  #      "G03H",
  #      "L02BB",
  #      "G04CB",
  #      "C03DA01"
  # )
   )
  )

  # FHM vaccines ----
  vax <- large_files[["FHM/fp_lev_fhm_nvr.sas7bdat"]][lopnr %in% ids]
  vax <- vax[!is.na(vaccination_date)]
  vax[, isoyearweek := cstime::date_to_isoyearweek_c(vaccination_date)]
  skeleton[
    vax,
    on = c("id==lopnr", "isoyearweek"),
    vx_hpv := TRUE
    ]
  skeleton[is.na(vx_hpv), vx_hpv := FALSE]

  # hpv registry
  # lmed <- large_files[["lmed"]][lopnr %in% ids]
  #
  # swereg::add_rx(
  #   skeleton,
  #   lmed,
  #   id_name = "lopnr",
  #   rxs = list(
  #
  # "rx_vaccines_dose1"= c(
  #   "J07BM01"
  # ),
  # "rx_vaccines_dose2"= c(
  #   "J07BM02"
  # ),
  # "rx_vaccines_dose3"= c(
  #   "J07BM03"
  # )


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

  # causedeath <- haven::read_sas(fs::path(org::project$data_raw, "Sos", "ut_r_dors_fk_37133_2022.sas7bdat")) %>%
  #   swereg::make_lowercase_names() %>%
  #   dplyr::filter(lopnr %in% ids) %>%
  #   dplyr::mutate(dodsdat = stringr::str_replace(dodsdat, "^195410$", "19541015")) %>%
  #   dplyr::mutate(dodsdat = stringr::str_replace(dodsdat, "0000$", "0701")) %>%
  #   dplyr::mutate(dodsdat = stringr::str_replace(dodsdat, "00$", "15")) %>%
  #   dplyr::mutate(dodsdat = lubridate::ymd(dodsdat)) %>%
  #   setDT()
#---------causes of death per WHO ICD 10-------------
  #    https://platform.who.int/mortality/about/list-of-causes-and-corresponding-icd-10-codes
   causedeath <- large_files[["causedeath"]][lopnr %in% ids]

  swereg::add_cods(
    skeleton,
    causedeath,
    id_name = "lopnr",
    cod_type = "both", # "underlying", "multiple", "both"
    cods = list(
      "death_external_causes"= c(
        "V",
        "W",
        sprintf("X%02d", 0:40),
        "X43",
        sprintf("X%02d", 46:99),
        sprintf("Y%02d", 0:89),
        "Y870",
        "Y871",
        "Y872"
      )
    )
  )

 swereg::add_cods(
    skeleton,
    causedeath,
    id_name = "lopnr",
    cod_type = "underlying", # "underlying", "multiple", "both"
    cods = list(
      "death_who_certain_infectious_parasitic_diseases"= c(
        "A",
        "B",
        sprintf("G%02d", 0:04),
        "G14",
        sprintf("N%02d", 70:73),
        "P37.3",
        "P37.4"
      ),
      "death_who_respiratory_infections"= c(
        sprintf("H%02d", 65:66),
        sprintf("J%02d", 0:22),
        "P23",
        "U04",
        "U07.01",
        "U07.02",
        "U09.9",
        "U10.9"
      ),
      "death_who_maternal_conditions"= c(
        sprintf("O%02d", 0:99)
      ),
      "death_who_perinatal_conditions"= c(
        sprintf("P%02d", 0:22),
        sprintf("P%02d", 24:36),
        sprintf("P%02d", 38:96)
      ),
      "death_who_nutritional_deficiencies"= c(
        sprintf("P%02d", 0:22),
        sprintf("P%02d", 24:36),
        sprintf("P%02d", 38:96)
      ),
     "death_who_neoplasms" = c(
       "C",
       sprintf("D%02d", 0:48)
     ),
     "death_who_diabetes_endocrine_disorders"= c(
       sprintf("E%02d", 03:07),
       sprintf("E%02d", 10:14),
       sprintf("E%02d", 15:16),
       sprintf("E%02d", 20:34),
       sprintf("E%02d", 65:88),
       sprintf("D%02d", 55:64)
     ),
     "death_who_neuro_psychiatric_conditions"= c(
       "F",
       sprintf("G%02d", 06:13),
       sprintf("G%02d", 15:98),
       "U07.0",
       "X41",
       "X42",
       "X44",
       "X45"
     ),
     "death_who_sense_organ_diseases"= c(
       sprintf("H%02d", 0:61),
       sprintf("H%02d", 68:93)
     ),
     "death_who_cardiovascular_diseases"= c(
      "I"
     ),
      "death_who_respiratory_diseases"= c(
      sprintf("J%02d", 30:98)
      ),
      "death_who_digestive_diseases"= c(
        sprintf("K%02d", 20:92)
      ),
     "death_who_genitourinary_diseases"= c(
       sprintf("N%02d", 0:64),
       sprintf("N%02d", 75:98)
      ),
      "death_who_skin_diseases"= c(
        "L"
      ),
      "death_who_musculoskeletal_diseases"= c(
        "M"
      ),
      "death_who_congenital_anomalies"= c(
        "Q"
      ),
     "death_who_oral_conditions"= c(
       sprintf("K%02d", 0:14)
     ),
       "death_who_unintentional_injuries"= c(
       "V",
       sprintf("X%02d", 0:40),
       "X43",
       sprintf("X%02d", 46:59),
       sprintf("Y%02d", 40:86),
       "Y88",
       "Y89",
       "U12.9"
     ),
     "death_who_intentional_injuries"= c(
       sprintf("X%02d", 60:99),
       sprintf("Y%02d", 35:36),
       "Y870",
       "Y871"
     ),
     "death_who_ill_defined_injuries_accidents"= c(
       sprintf("Y%02d", 10:34),
       "Y872"
     ),
     "death_who_ill_defined_diseases"= c(
       sprintf("R%02d", 0:99)
     )
        )
    )

 # MAkE SURE THAT YOU DO THIS FOR ALL OF THE CAUSE OF DEATHS OF THE ONES THAT ARE
 # ABOVE, TO MAKE SURE IT'S ALL MUTUALLY EXCLUSIVE
 skeleton[death_external_causes==T, death_who_certain_infectious_parasitic_diseases := FALSE]
 skeleton[death_external_causes==T, death_who_respiratory_infections := FALSE]
 skeleton[death_external_causes==T, death_who_maternal_conditions := FALSE]
 skeleton[death_external_causes==T, death_who_perinatal_conditions := FALSE]
 skeleton[death_external_causes==T, death_who_nutritional_deficiencies := FALSE]
 skeleton[death_external_causes==T, death_who_neoplasms := FALSE]
 skeleton[death_external_causes==T, death_who_diabetes_endocrine_disorders := FALSE]
 skeleton[death_external_causes==T, death_who_neuro_psychiatric_conditions := FALSE]
 skeleton[death_external_causes==T, death_who_sense_organ_diseases := FALSE]
 skeleton[death_external_causes==T, death_who_cardiovascular_diseases := FALSE]
 skeleton[death_external_causes==T, death_who_respiratory_diseases := FALSE]
 skeleton[death_external_causes==T, death_who_digestive_diseases := FALSE]
 skeleton[death_external_causes==T, death_who_genitourinary_diseases := FALSE]
 skeleton[death_external_causes==T, death_who_skin_diseases := FALSE]
 skeleton[death_external_causes==T, death_who_musculoskeletal_diseases := FALSE]
 skeleton[death_external_causes==T, death_who_congenital_anomalies := FALSE]
 skeleton[death_external_causes==T, death_who_oral_conditions := FALSE]
 skeleton[death_external_causes==T, death_who_unintentional_injuries := FALSE]
 skeleton[death_external_causes==T, death_who_intentional_injuries := FALSE]
 skeleton[death_external_causes==T, death_who_ill_defined_injuries_accidents := FALSE]
 skeleton[death_external_causes==T, death_who_ill_defined_diseases := FALSE]
 skeleton[death_external_causes==T, death_any_disease := FALSE]

  #adding saab to variables to make sure the names are accurate===========
  names(skeleton)
  skeleton[is_amab==TRUE, rx_hormones_testosterone := FALSE]
  skeleton[is_amab==TRUE, op_afab_mastectomy := FALSE]
  skeleton[is_amab==TRUE, op_afab_breast_reconst_and_other_breast_ops := FALSE]
  skeleton[is_amab==TRUE, op_afab_penis_test_prosth := FALSE]
  skeleton[is_amab==TRUE, op_afab_internal_genital := FALSE]
  skeleton[is_amab==TRUE, op_afab_colpectomy := FALSE]

  skeleton[is_amab==FALSE, rx_hormones_estandro := FALSE]
  skeleton[is_amab==FALSE, op_amab_breast_reconst_and_other_breast_ops := FALSE]
  skeleton[is_amab==FALSE, op_amab_reconst_vag := FALSE]
  skeleton[is_amab==FALSE, op_amab_penis_amp := FALSE]
  skeleton[is_amab==FALSE, op_amab_larynx := FALSE]
  # convert to data.table
  # RICHARD DO SOMETHING IN SWEREG R PACKAGE
  # TO GIVE KRISTEN HORMONE STUFF
  # swereg::add_rx(
  #   skeleton,
  #   rx,
  #   id_name = "LopNr",
  #   ops = list(
  #     "rx_afab_all_hormones"= c(
  #       "HAC10",
  #       "HAC20",
  #       "HAC99",
  #       "HAC15"
  #     ),
  #     "rx_amab_all_hormones"= c(
  #       "HAC10",
  #       "HAC20",
  #       "HAC99",
  #       "HAC15"
  #     )
  #   )
  # )

  ### DROPPING ROWS#########################################
  skeleton[, dob := lubridate::ymd(paste0(fodelseman ,"15"))]
  #skeleton_gd[, age := floor(as.numeric(difftime(isoyearweeksun, dob, "days"))/365.25)]
  skeleton[, age := floor(lubridate::interval(dob, isoyearweeksun)/lubridate::years(1))]
  skeleton <- skeleton[isoyearweek >= 1960]
  skeleton <- skeleton[age >= 0]

  # drop rows after they die
  skeleton[, rowind_isoyearweek_death := cstime::date_to_isoyearweek_c(lubridate::ymd(doddatum))]
  unique(skeleton$rowind_isoyearweek_death)

  skeleton[, is_dead := isoyearweek >= rowind_isoyearweek_death]
  skeleton[is.na(rowind_isoyearweek_death), is_dead := FALSE]
  skeleton[, .(.N), keyby=.(is_dead)]

  skeleton[, remove_isoyearweeks_because_death := FALSE]
  skeleton[isoyearweek > rowind_isoyearweek_death, remove_isoyearweeks_because_death := TRUE]
  skeleton[,.(.N), keyby=.(remove_isoyearweeks_because_death)]
  skeleton[,.(length(unique(id))), keyby=.(is_dead)]

  skeleton[, rowind_final_status_dead := swereg::as_logical_max_with_infinite_as_na(is_dead), by=.(id)]

  nrow(skeleton)
  skeleton <- skeleton[remove_isoyearweeks_because_death==FALSE]
  nrow(skeleton)
  skeleton[, remove_isoyearweeks_because_death := NULL]


  ## drop before immigration and after emmigration#################################
  skeleton[, keep := TRUE]

  skeleton[!is.na(rowind_isoyearweek_immigration) & isoyearweek < rowind_isoyearweek_immigration, keep := FALSE]
  skeleton[!is.na(rowind_isoyearweek_emmigration) & rowind_isoyearweek_emmigration < isoyearweek, keep := FALSE]

  skeleton <- skeleton[keep == TRUE]
  skeleton[, keep := NULL]

  # remove all things that occur after last observation
  skeleton[
    ,
    rowind_isoyearweek_last_observation := max(isoyearweek),
    by = .(
      id
    )]
  skeleton[
    ,
    rowind_isoyearweeksun_last_observation := max(isoyearweeksun),
    by = .(
      id
    )]

  skeleton[rowind_isoyearweek_legal_sex_change_first > rowind_isoyearweek_last_observation, rowind_isoyearweek_legal_sex_change_first := NA]
  skeleton[rowind_isoyearweek_legal_sex_change_reversal > rowind_isoyearweek_last_observation, rowind_isoyearweek_legal_sex_change_reversal := NA]
  skeleton[rowind_isoyearweeksun_legal_sex_change_first > rowind_isoyearweeksun_last_observation, rowind_isoyearweeksun_legal_sex_change_first := NA]
  skeleton[rowind_isoyearweeksun_legal_sex_change_reversal > rowind_isoyearweeksun_last_observation, rowind_isoyearweeksun_legal_sex_change_reversal := NA]

  # id[,row_id := 1:.N, by=.(lopnr_kontroll)]
  # id[,num_obs := .N, by=.(lopnr_kontroll)]
  # id[, lopnr_kontroll_with_duplicates := lopnr_kontroll ]
  # id[num_obs>=2, lopnr_kontroll_with_duplicates := lopnr_kontroll_with_duplicates+100000000*row_id]
  # id[num_obs>=2]
  #
  # ids_to_be_removed <- id[num_obs>=2]$lopnr_kontroll
  # skeleton_people_removed <- skeleton[id %in% ids_to_be_removed]
  # skeleton <- skeleton[!id %in% ids_to_be_removed]
  #
  # for(i in unique(id$row_id)){
  #   id_to_add_in <- id[num_obs>=2 & row_id==(i)]
  #   skeleton_people_to_be_added_in <- copy(skeleton_people_removed)[id %in% id_to_add_in$lopnr_kontroll]
  #   skeleton_people_to_be_added_in[
  #     id_to_add_in,
  #     on = "id==lopnr_kontroll",
  #     id := lopnr_kontroll_with_duplicates
  #   ]
  #   skeleton <- rbindlist(list(skeleton, skeleton_people_to_be_added_in))
  # }
  #
  # ids_to_add_back_in <- id[num_obs>=2,.(lopnr_fall, lopnr_kontroll_with_duplicates)]
  # skeleton[
  #   ids_to_add_back_in,
  #   on = "id==lopnr_kontroll_with_duplicates",
  #   id_of_case := lopnr_fall
  # ]

  skeleton[, uniqueN(id), by=rowind_register_tag]

  # duplicate controls as necessary
  we_have_this_many_controls <- length(unique(skeleton[
    rowind_register_tag %in% c(
      "control_same_sex",
      "control_opposite_sex",
      "control_sibling"
    )
  ]$id))
  we_want_this_many_controls <- length(unique(ids_batch$master_list_of_what_i_expect$unique_id_for_control))

  # if we_want_this_many_people > we_have_this_many_people then duplicate rows
  if(we_want_this_many_controls > we_have_this_many_controls){
    who_to_duplicate <- copy(ids_batch$master_list_of_what_i_expect)
    who_to_duplicate[, N:= .N, by = .(lopnr_kontroll)]
    who_to_duplicate <- who_to_duplicate[N>1]

    duplicated_people <- vector("list", length = nrow(who_to_duplicate))
    for(i in seq_along(duplicated_people)){
      duplicated_people[[i]] <- skeleton[id == who_to_duplicate[i]$lopnr_kontroll]
      duplicated_people[[i]][, id := who_to_duplicate[i]$unique_id_for_control]
    }
    # remember to delete the controls from skeleton who have been duplicated!
    skeleton <- rbindlist(list(
      skeleton[!id %in% who_to_duplicate$lopnr_kontroll],
      rbindlist(duplicated_people)
    ))
  }

  # matching cases to controls ----
  #  make sure that all controls have unique ids (because they could be in other batches)

  skeleton[
    ids_batch$master_list_of_what_i_expect,
    on = "id==lopnr_kontroll",
    unique_id_for_control := unique_id_for_control
  ]
  skeleton[!is.na(unique_id_for_control), id := unique_id_for_control]
  skeleton[, unique_id_for_control := NULL]

  # skeleton[rowind_register_tag %in% c(
  #   "control_same_sex",
  #   "control_opposite_sex"
  # ),.(as.character(id), rowind_register_tag, as.character(unique_id_for_control))] %>%
  #   unique()

  # matching cases to cases (simple) ----
  skeleton[rowind_register_tag=="case", id_of_case:=id]

  # FIGURE OUT CASE FOR THE CONTROL
  skeleton[
    ids_batch$master_list_of_what_i_expect,
    on = "id==unique_id_for_control",
    id_of_case := lopnr_fall
  ]

  # matching children to mothers ----
  skeleton[
    ids_batch$mother_matched_to_child,
    on = "id==lopnr_mor",
    id_of_child := id_child
  ]

  # matching children to fathers ----
  skeleton[
    ids_batch$father_matched_to_child,
    on = "id==lopnr_far",
    id_of_child := id_child
  ]

  # id_of_child for the controls and cases
  skeleton[!rowind_register_tag %in% c(
    "father",
    "mother"
    ), id_of_child:=id]

  # give parents an id_of_case
  id_of_case_for_not_parents <- unique(
    skeleton[!rowind_register_tag%in%c("mother","father"),.(id_of_child=id,id_of_case_of_child=id_of_case)]
  )
  skeleton[
    id_of_case_for_not_parents,
    on = "id_of_child",
    id_of_case_of_child := id_of_case_of_child
  ]
  skeleton[rowind_register_tag %in% c("mother", "father"), id_of_case := id_of_case_of_child]
  skeleton[, id_of_case_of_child := NULL]
  skeleton[id==69][1]
  skeleton[rowind_register_tag=="case"]$id %>% unique()
  skeleton[id_of_child==69]$id %>% unique()

  skeleton[id==37880][1]
  skeleton[id==344296][1]

  skeleton[rowind_register_tag=="control_opposite_sex"]$id %>% unique() %>% as.character()
  skeleton[id==457413][1]
  skeleton[id_of_child==457413]$id %>% unique()
  skeleton[id==424136 ][1]
  skeleton[id==475084][1]

  skeleton[rowind_register_tag=="control_sibling"]$id %>% unique() %>% as.character()
  skeleton[id_of_case==1421][1]
  skeleton[id==2198680001421][1]
  skeleton[id_of_case==1421,.(as.character(id), rowind_register_tag)] %>% unique()

  # save
  # saveRDS(skeleton, fs::path(org::project$data_processed, "skeleton.RDS"))
  qs2::qs_save(skeleton, fs::path(data_generic_skeleton, paste0("skeleton1_create_",file_number,".qs2")))

}

