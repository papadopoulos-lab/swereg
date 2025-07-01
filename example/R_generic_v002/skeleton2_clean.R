skeleton2_clean <- function(file_number = 1){
  # this will only run if you directly highlight it from inside here
  if(plnr::is_run_directly()){
    file_number <- 1
  }
  skeleton <- qs::qread(fs::path(data_generic_skeleton, paste0("skeleton1_create_",file_number,".qs")))

  skeleton[, uniqueN(id), by=rowind_register_tag]
  skeleton[rowind_register_tag=="mother"][1,]
  # skeleton[rowind_register_tag=="case", .(length(unique(id))), keyby=.(fodelselandgrupp)]

  # process variables that are relevant to both parents and children ----
  skeleton[, rowdep_edu_cat := fcase(
    edu_utbniva %in% c(1, 2), "3_elementary_school_and_lower",
    edu_utbniva %in% c(3, 4), "2_started_upper_secondary",
    edu_utbniva %in% 5:7, "1_started_university"
  )]
  skeleton[, edu_utbniva := NULL]

  skeleton[, rowind_birthcountry := fcase(
    fodelselandgrupp %in% c("Sverige"), "1_sweden",
    fodelselandgrupp %in% c("Övriga Norden"), "2_nordics",
    fodelselandgrupp %in% c("Utanför Norden"), "3_outside_nordics"
  )]
  skeleton[, fodelselandgrupp := NULL]

  skeleton[, rowdep_income_inflation_adjusted := cdisp]
  skeleton[, cdisp := NULL]

  # processing parents ----
  # find out when a kid was born and spread it to the parents
  skeleton[!rowind_register_tag %in% c("mother", "father"), temp:=min(isoyear), by=.(id)]
  skeleton[, rowind_child_birth_isoyear := mean(temp, na.rm=T), by=.(id_of_child)]
  skeleton[, temp := NULL]


  # PROCESS PARENTS HERE, AND TAKE THEM OUT TO WIDE FORMAT
  parents <- skeleton[rowind_register_tag %in% c("mother", "father") & isoyear==rowind_child_birth_isoyear]
  # make sure we only have one row per parent
  parents <- parents[, .SD[1], by = id]
  parents[, .(N=.N), keyby=.(id)]
  parents[id==599224]

  # select only the columns you want for parents

  parents <- parents[,.(
    id_of_child,
    rowind_register_tag,
    rowind_birthcountry,
    rowdep_edu_cat,
    rowdep_income_inflation_adjusted
  )]

  # remember to specify all the variables you want to go wide (in value.var)!
  parents_wide <- dcast.data.table(
    parents,
    id_of_child ~ rowind_register_tag,
    value.var = c("rowind_birthcountry", "rowdep_edu_cat", "rowdep_income_inflation_adjusted")
  )

  # WE REMOVE PARENTS HERE and merge in the parents as wide
  skeleton_gd <-
    merge(
      skeleton[rowind_register_tag %in% c("case", "control_same_sex", "control_opposite_sex", "control_sibling")],
      parents_wide,
      by.x = "id",
      by.y = "id_of_child",
      all.x = T
    )
  skeleton_gd[id==69][1]

  # pull out the cases, for the ICD10 GD diagnoses ONLY, so that we can copy them to controls
  skeleton_cases <- skeleton_gd[
    rowind_register_tag=="case",
    .(
      id,
      isoyearweek,
      diag_gd_icd10_F64_0,
      diag_gd_icd10_F64_89,
      diag_gd_icd10_F64_089,
      diag_gd_icd89_transsexual
    )
  ]
  skeleton_gd[,diag_gd_icd10_F64_0 := NULL]
  skeleton_gd[,diag_gd_icd10_F64_89 := NULL]
  skeleton_gd[,diag_gd_icd10_F64_089 := NULL]
  skeleton_gd[,diag_gd_icd89_transsexual := NULL]
  skeleton_gd[
    skeleton_cases,
    on = c("id_of_case==id", "isoyearweek"),
    c("diag_gd_icd10_F64_0",
      "diag_gd_icd10_F64_89",
      "diag_gd_icd10_F64_089",
      "diag_gd_icd89_transsexual") := list(
        diag_gd_icd10_F64_0,
        diag_gd_icd10_F64_89,
        diag_gd_icd10_F64_089,
        diag_gd_icd89_transsexual
      )
  ]
  # if the timeseries don't overlap (e.g. due to death), then set exposure to false for those weeks
  skeleton_gd[is.na(diag_gd_icd10_F64_0), diag_gd_icd10_F64_0 := FALSE]
  skeleton_gd[is.na(diag_gd_icd10_F64_89), diag_gd_icd10_F64_89 := FALSE]
  skeleton_gd[is.na(diag_gd_icd10_F64_089), diag_gd_icd10_F64_089 := FALSE]
  skeleton_gd[is.na(diag_gd_icd89_transsexual), diag_gd_icd89_transsexual := FALSE]

  # Organizing sample with row independent variables ==============================
  ## Refining sample and parents==============================
  ### mean(skeleton_gd$doddatum=="")
  names(skeleton_gd)

  # GD diagnosis information
  # (TOTAL) age at first GD dx
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE, temp := age]
  skeleton_gd[id==69 & !is.na(temp)]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_gd := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (TOTAL) year of first GD diagnosis
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE, temp := isoyear]
  skeleton_gd[id==69 & !is.na(temp)]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_gd := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (TOTAL) date of first GD diagnosis
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE, temp := isoyearweek_sunday]
  skeleton_gd[id==69 & !is.na(temp)]
  skeleton_gd[, rowind_isoyearweeksun_first_gd := swereg::min_with_infinite_as_na(temp, na.rm = TRUE), by = .(id)]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (TOTAL) cumulative count of gd dx per TRUE week
  # Richard code to start: skeleton[, temp := cumsum(diag_gd_icd10_F64_089), by = .(id)]
  skeleton_gd[, diag_gd_icd10_F64_089 := ifelse(is.na(diag_gd_icd10_F64_089), FALSE, diag_gd_icd10_F64_089)]
  skeleton_gd[, gd_dx_cumulative_count := cumsum(diag_gd_icd10_F64_089==T), by = id]
  # print(skeleton)

  #--this reduces data down to just a range of time. Ending at 2022 so that there will be minimum 1 year of follow up-------
  skeleton_gd[, keep := FALSE]
  #  skeleton[rowind_isoyear_first_gd %in% c(2001:2022) & !is.na(rowind_isoyear_first_gd), keep := TRUE]

  # THIS PART MIGHT BE CHANGED DEPENDING IF YOU WANT PARENTS!!!!==========================
  ## This study does not want parents
  #ids_gd <- unique(skeleton[keep==TRUE]$id)
  #skeleton[, keep := NULL]
  #skeleton_gd <- skeleton[id %in% ids_gd]



  #skeleton_gd <- copy(skeleton)
  #--tells us who exits the study because of death
  skeleton_gd[!is.na(rowind_isoyearweek_death),.(N=length(unique(id))), keyby=.(rowind_register_tag)]
  skeleton_gd[rowind_final_status_dead==T,.(N=length(unique(id))), keyby=.(rowind_register_tag)]
  skeleton_gd[is_dead==T,.(N=length(unique(id))), keyby=.(rowind_register_tag)]
  skeleton_gd[rowind_register_tag=="mother",.(id,id_of_case)]
  # assigning a first gd date for controls so that we can test analysis code
  skeleton_gd[, rowind_isoyearweeksun_first_gd := swereg::first_non_na(rowind_isoyearweeksun_first_gd), by = id_of_case]


  #completes education when there are missing. Last known education carries down
  skeleton_gd[, rowdep_edu_cat := zoo::na.locf(rowdep_edu_cat, na.rm = F), by = .(id)]

  # Organizing sample with row independent variables ==============================
  ## Refining sample and parents==============================
  # mean(skeleton_gd$doddatum=="")
  ### GD diagnosis information
  #### (TOTAL) age at first GD dx
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE, temp := age]
  skeleton_gd[id==69 & !is.na(temp)]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_gd := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  #### (TOTAL) isoyear of first GD diagnosis
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE, temp := isoyear]
  skeleton_gd[id==69 & !is.na(temp)]
  # spread the MINIMUM ISOYEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_gd := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  #### (TOTAL) date of first GD diagnosis
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE, temp := isoyearweek_sunday]
  skeleton_gd[id==69 & !is.na(temp)]
  skeleton_gd[, rowind_isoyearweeksun_first_gd := swereg::min_with_infinite_as_na(temp, na.rm = TRUE), by = .(id)]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  #### (TOTAL) isoyear death
  skeleton_gd[is_dead == TRUE, temp := isoyear]
  skeleton_gd[id==69 & !is.na(temp)]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_death := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  #### (TOTAL) age at time of death
  skeleton_gd[is_dead == TRUE, temp := age]
  skeleton_gd[id==69 & !is.na(temp)]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_death := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  #--------------------------------------------------------------
  # make date of birth row independent
  skeleton_gd[, rowind_isoyearweek_dob := min(dob)]
  skeleton_gd[id==69]

  #country of birth
  skeleton_gd[isoyear==rowind_isoyear_first_gd, temp := rowind_birthcountry]
  skeleton_gd[, rowind_country_first_dx := first_non_na(temp), by = id]
  skeleton_gd[, temp := NULL]
  skeleton_gd[id==69]

  #verifying it worked correctly by looking to see if there are dates attached to first dx variable
  summary(skeleton_gd$age)
  head(skeleton_gd[, c("diag_gd_icd89_transsexual", "age", "id")], n = 5)
  # GD diagnosis information by saab
  # (AFAB) age at first GD diagnosis
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE & is_amab == FALSE, temp := age]
  skeleton_gd[id==69 & !is.na(temp)]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_gd_afab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (AFAB) year at first GD diagnosis
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE & is_amab == FALSE, temp := isoyear]
  skeleton_gd[id==69 & !is.na(temp)]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_gd_afab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (AFAB) date of first GD diagnosis
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE & is_amab == FALSE, temp := isoyearweek_sunday]
  skeleton_gd[id==69 & !is.na(temp)]
  skeleton_gd[, rowind_isoyearweeksun_first_gd_afab := swereg::min_with_infinite_as_na(temp, na.rm = TRUE), by = .(id)]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (AMAB) age at first GD diagnosis
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE & is_amab == TRUE, temp := age]
  skeleton_gd[id==69 & !is.na(temp)]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_gd_amab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (AMAB) year at first GD diagnosis
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE & is_amab == TRUE, temp := isoyear]
  skeleton_gd[id==69 & !is.na(temp)]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_isoyear_first_gd_amab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (AMAB) date of first GD diagnosis
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE & is_amab == TRUE, temp := isoyearweek_sunday]
  skeleton_gd[id==69 & !is.na(temp)]
  skeleton_gd[, rowind_isoyearweeksun_first_gd_amab := swereg::min_with_infinite_as_na(temp, na.rm = TRUE), by = .(id)]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # Verification, checking to make sure it looks right
  skeleton_gd[, comparison_result := rowind_isoyear_first_gd_afab == rowind_isoyear_first_gd & is_amab == FALSE]
  skeleton_gd[, .(rowind_isoyear_first_gd_afab, rowind_isoyear_first_gd, is_amab, comparison_result)]
  false_cases <- skeleton_gd[comparison_result == FALSE]
  false_cases[, .(rowind_isoyear_first_gd_afab, rowind_isoyear_first_gd, is_amab, comparison_result)]

  #----Creating Gender Affirming Treatment (GAT) variables------------------
  #GAT regardless of SAAB
  #puberty blockers, by age
  skeleton_gd[rx_hormones_pubblock == TRUE, temp := age]
  # making row independent variables
  skeleton_gd[, rowind_age_first_rx_hormones_pubblock := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  #puberty blockers, by year
  skeleton_gd[rx_hormones_pubblock == TRUE, temp := isoyear]
  # making row independent variables
  skeleton_gd[, rowind_isoyear_first_rx_hormones_pubblock := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  #puberty blockers, by date
  skeleton_gd[diag_gd_icd10_F64_089 == TRUE, temp := isoyearweek_sunday]
  skeleton_gd[id==69 & !is.na(temp)]
  skeleton_gd[, rowind_isoyearweeksun_first_rx_hormones_pubblock := swereg::min_with_infinite_as_na(temp, na.rm = TRUE), by = .(id)]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS


  # (AFAB) GAT, all by age
  #puberty blockers
  skeleton_gd[rx_hormones_pubblock == TRUE & is_amab == FALSE, temp := age]
  # making row independent variables
  skeleton_gd[, rowind_age_first_rx_hormones_afab_pubblock := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # hormones
  skeleton_gd[rx_hormones_testosterone == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_rx_hormones_testosterone := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # mastectomy
  skeleton_gd[op_afab_mastectomy == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_op_afab_mastectomy := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # breast reconstruction/other
  skeleton_gd[op_afab_breast_reconst_and_other_breast_ops == TRUE, temp := age]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_age_first_op_afab_breast_reconst_and_other_breast_ops := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # penis prosthesis
  skeleton_gd[op_afab_penis_test_prosth == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_op_afab_penis_test_prosth := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # hysterectomy
  skeleton_gd[op_afab_internal_genital == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_op_afab_internal_genital := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # colpectomy
  skeleton_gd[op_afab_colpectomy == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_op_afab_colpectomy := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any top surgery
  skeleton_gd[, first_top_afab := pmin(op_afab_mastectomy, op_afab_breast_reconst_and_other_breast_ops, na.rm=T), by = .(id)]
  skeleton_gd[first_top_afab == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_top_afab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any bottom surgery
  skeleton_gd[, first_bottom_afab := pmin(op_afab_penis_test_prosth, op_afab_internal_genital, op_afab_colpectomy, na.rm=T), by = .(id)]
  skeleton_gd[first_bottom_afab == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_bottom_afab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any surgery
  skeleton_gd[, first_op_afab_all := pmin(op_afab_mastectomy, op_afab_breast_reconst_and_other_breast_ops, op_afab_penis_test_prosth, op_afab_internal_genital, op_afab_colpectomy, na.rm=T), by = .(id)]
  skeleton_gd[first_op_afab_all == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_op_afab_all := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (AFAB) GAT by isoyear ----
  #puberty blockers
  skeleton_gd[rx_hormones_pubblock == TRUE & is_amab == FALSE, temp := isoyear]
  # making row independent variables
  skeleton_gd[, rowind_isoyear_first_rx_hormones_afab_pubblock := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # hormones
  skeleton_gd[rx_hormones_testosterone == TRUE, temp := isoyear]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_rx_hormones_testosterone := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # mastectomy
  skeleton_gd[op_afab_mastectomy == TRUE, temp := isoyear]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_afab_mastectomy := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # breast reconstruction/other
  skeleton_gd[op_afab_breast_reconst_and_other_breast_ops == TRUE, temp := isoyear]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_afab_breast_reconst_and_other_breast_ops := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # penis prosthesis
  skeleton_gd[op_afab_penis_test_prosth == TRUE, temp := isoyear]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_afab_penis_test_prosth := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # hysterectomy
  skeleton_gd[op_afab_internal_genital == TRUE, temp := isoyear]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_afab_internal_genital := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # colpectomy
  skeleton_gd[op_afab_colpectomy == TRUE, temp := isoyear]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_afab_colpectomy := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any top surgery
  skeleton_gd[first_top_afab == TRUE, temp := isoyear]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_top_afab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any bottom surgery
  skeleton_gd[first_bottom_afab == TRUE, temp := isoyear]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_bottom_afab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any surgery
  skeleton_gd[first_op_afab_all == TRUE, temp := isoyear]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_afab_all := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (AFAB) GAT by date, i.e, isoyearweek_sunday ----
  #puberty blockers
  skeleton_gd[rx_hormones_pubblock == TRUE & is_amab == FALSE, temp := isoyearweek_sunday]
  # making row independent variables
  skeleton_gd[, rowind_isoyearweeksun_first_rx_hormones_afab_pubblock := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # hormones
  skeleton_gd[rx_hormones_testosterone == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_rx_hormones_testosterone := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # mastectomy
  skeleton_gd[op_afab_mastectomy == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_afab_mastectomy := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # breast reconstruction/other
  skeleton_gd[op_afab_breast_reconst_and_other_breast_ops == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_afab_breast_reconst_and_other_breast_ops := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # penis prosthesis
  skeleton_gd[op_afab_penis_test_prosth == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_afab_penis_test_prosth := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # hysterectomy
  skeleton_gd[op_afab_internal_genital == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_afab_internal_genital := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # colpectomy
  skeleton_gd[op_afab_colpectomy == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_afab_colpectomy := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any top surgery
  skeleton_gd[first_top_afab == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_top_afab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any bottom surgery
  skeleton_gd[first_bottom_afab == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_bottom_afab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any surgery
  skeleton_gd[first_op_afab_all == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_afab_all := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # -----------------------------------------------------------------------------------------
  # (AMAB) GAT by age
  #puberty blockers
  skeleton_gd[rx_hormones_pubblock == TRUE & is_amab == TRUE, temp := age]
  # making row independent variables
  skeleton_gd[, rowind_age_first_rx_hormones_amab_pubblock := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # hormones
  skeleton_gd[rx_hormones_estandro == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_rx_hormones_estandro := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  #larnyx shave
  skeleton_gd[op_amab_larynx == TRUE, temp := age]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_age_first_op_amab_larynx := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # breast reconstruction/other
  skeleton_gd[op_amab_breast_reconst_and_other_breast_ops == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_op_amab_breast_reconst_and_other_breast_ops := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # vaginal construction
  skeleton_gd[op_amab_reconst_vag == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_op_amab_reconst_vag := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # penilectomy
  skeleton_gd[op_amab_penis_amp == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_op_amab_penis_amp := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  #     orchidectomy
  skeleton_gd[op_amab_orchidectomy == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_op_amab_orchidectomy := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any top surgery
  skeleton_gd[, first_top_amab := pmin(rowind_age_first_op_amab_breast_reconst_and_other_breast_ops, na.rm=T), by = .(id)]
  skeleton_gd[first_top_amab == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_top_amab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any bottom surgery
  skeleton_gd[, first_bottom_amab := pmin(op_amab_penis_amp, op_amab_reconst_vag, op_amab_orchidectomy, na.rm=T), by = .(id)]
  skeleton_gd[first_bottom_amab == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_bottom_amab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any surgery
  skeleton_gd[, first_op_amab_all := pmin(op_amab_breast_reconst_and_other_breast_ops, op_amab_penis_amp, op_amab_reconst_vag, op_amab_orchidectomy, na.rm=T), by = .(id)]
  skeleton_gd[first_op_amab_all == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_first_op_amab_all := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (AMAB) GAT by isoyear
  #puberty blockers
  skeleton_gd[rx_hormones_pubblock == TRUE & is_amab == TRUE, temp := isoyear]
  # making row independent variables
  skeleton_gd[, rowind_isoyear_first_rx_hormones_amab_pubblock := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # hormones
  skeleton_gd[rx_hormones_estandro == TRUE, temp := isoyear]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_rx_hormones_estandro := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  #larynx shave
  skeleton_gd[op_amab_larynx == TRUE, temp := isoyear]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_amab_larynx := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # breast reconstruction/other
  skeleton_gd[op_amab_breast_reconst_and_other_breast_ops == TRUE, temp := isoyear]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_amab_breast_reconst_and_other_breast_ops := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # vaginal construction
  skeleton_gd[op_amab_reconst_vag == TRUE, temp := isoyear]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_amab_reconst_vag := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # penilectomy
  skeleton_gd[op_amab_penis_amp == TRUE, temp := isoyear]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_amab_penis_amp := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # orchidectomy
  skeleton_gd[op_amab_orchidectomy == TRUE, temp := isoyear]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_amab_orchidectomy := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any top surgery
  skeleton_gd[first_top_amab == TRUE, temp := isoyear]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_top_amab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any bottom surgery
  skeleton_gd[first_bottom_amab == TRUE, temp := isoyear]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_bottom_amab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any surgery
  skeleton_gd[first_op_amab_all == TRUE, temp := isoyear]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyear_first_op_amab_all := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # (AMAB) GAT by date, i.e. isoyearweek_sunday
  #puberty blockers
  skeleton_gd[rx_hormones_pubblock == TRUE & is_amab == TRUE, temp := isoyearweek_sunday]
  # making row independent variables
  skeleton_gd[, rowind_isoyearweeksun_first_rx_hormones_amab_pubblock := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # hormones
  skeleton_gd[rx_hormones_estandro == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_rx_hormones_estandro := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  #larynx shave
  skeleton_gd[op_amab_larynx == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_amab_larynx := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # breast reconstruction/other
  skeleton_gd[op_amab_breast_reconst_and_other_breast_ops == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_amab_breast_reconst_and_other_breast_ops := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # vaginal construction
  skeleton_gd[op_amab_reconst_vag == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM year over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_amab_reconst_vag := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # penilectomy
  skeleton_gd[op_amab_penis_amp == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_amab_penis_amp := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # orchidectomy
  skeleton_gd[op_amab_orchidectomy == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_amab_orchidectomy := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any top surgery
  skeleton_gd[first_top_amab == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_top_amab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any bottom surgery
  skeleton_gd[first_bottom_amab == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_bottom_amab := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  # first of any surgery
  skeleton_gd[first_op_amab_all == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_first_op_amab_all := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  # ----row independent cause of death---------------------------------------------------------------------------
  #---isoyearweek_sun of death by cause

  skeleton_gd[death_who_certain_infectious_parasitic_diseases == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_certain_infectious_parasitic_diseases := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_maternal_conditions == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_maternal_conditions := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_perinatal_conditions == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_perinatal_conditions := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_nutritional_deficiencies == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_nutritional_deficiencies := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_neoplasms == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_neoplasms := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_diabetes_endocrine_disorders == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_diabetes_endocrine_disorders := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_neuro_psychiatric_conditions == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_neuro_psychiatric_conditions := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_sense_organ_diseases == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_sense_organ_diseases := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_cardiovascular_diseases == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_cardiovascular_diseases := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_respiratory_diseases == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_respiratory_diseases := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_digestive_diseases == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_digestive_diseases := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_genitourinary_diseases == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_genitourinary_diseases := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_skin_diseases == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_skin_diseases := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_musculoskeletal_diseases == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_musculoskeletal_diseases := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_congenital_anomalies == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_congenital_anomalies := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_oral_conditions == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_oral_conditions := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

    skeleton_gd[death_who_ill_defined_diseases == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_ill_defined_diseases := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_ill_defined_injuries_accidents == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_ill_defined_injuries_accidents := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_intentional_injuries == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_intentional_injuries := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_who_unintentional_injuries == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_who_unintentional_injuries := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_external_causes == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_external_causes := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS

  skeleton_gd[death_any_disease == TRUE, temp := isoyearweek_sunday]
  # spread the MINIMUM YEAR over all rows of the person
  skeleton_gd[, rowind_isoyearweeksun_death_any_disease := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS


  # creating cause of death, categorical variable here
  # checking if any participants have more than 1 cause (should only be 1)
  death_causes_columns <- c(
    "death_external_causes",
    "death_who_certain_infectious_parasitic_diseases", "death_who_maternal_conditions", "death_who_perinatal_conditions", "death_who_nutritional_deficiencies",
    "death_who_neoplasms", "death_who_diabetes_endocrine_disorders", "death_who_neuro_psychiatric_conditions", "death_who_sense_organ_diseases", "death_who_cardiovascular_diseases",
    "death_who_respiratory_diseases", "death_who_digestive_diseases", "death_who_genitourinary_diseases", "death_who_skin_diseases", "death_who_musculoskeletal_diseases",
    "death_who_congenital_anomalies", "death_who_oral_conditions", "death_who_ill_defined_diseases", "death_who_ill_defined_injuries_accidents",
    "death_who_intentional_injuries", "death_who_unintentional_injuries")

  skeleton_gd[, death_causes_multiple := rowSums(.SD, na.rm = TRUE) > 1, by = id, .SDcols = death_causes_columns]
  participants_multiple_causes <- skeleton_gd[death_causes_multiple == TRUE, .N, by = id]
  # print(participants_multiple_causes)

  # creating categorical variable
  skeleton_gd[, death_categorical := fcase(
    death_external_causes == TRUE, "external_causes",
    death_who_certain_infectious_parasitic_diseases == TRUE, "infectious_parasitic_diseases",
    death_who_maternal_conditions == TRUE, "maternal_conditions",
    death_who_perinatal_conditions == TRUE, "perinatal_conditions",
    death_who_nutritional_deficiencies == TRUE, "nutritional_deficiencies",
    death_who_neoplasms == TRUE, "malignant_neoplasms",
    death_who_diabetes_endocrine_disorders == TRUE, "diabetes_and_endocrine disorders",
    death_who_neuro_psychiatric_conditions == TRUE, "neuropsych_conditions",
    death_who_sense_organ_diseases == TRUE, "sense_organ_diseases",
    death_who_cardiovascular_diseases == TRUE, "cardiovascular_diseases",
    death_who_respiratory_diseases == TRUE, "respiratory_diseases",
    death_who_digestive_diseases == TRUE, "digestive_diseases",
    death_who_genitourinary_diseases == TRUE, "genitourinary_diseases",
    death_who_skin_diseases == TRUE, "skin_diseases",
    death_who_musculoskeletal_diseases == TRUE, "musculoskeletal_diseases",
    death_who_congenital_anomalies  == TRUE, "congenital_anomalies",
    death_who_oral_conditions == TRUE, "oral_conditions",
    death_who_ill_defined_diseases == TRUE, "ill_defined_diseases",
    death_who_ill_defined_injuries_accidents == TRUE, "ill_defined injuries_accidents",
    death_who_intentional_injuries == TRUE, "intentional_injuries",
    death_who_unintentional_injuries == TRUE, "unintentional_injuries",
    default = NA_character_  # Default to NA if no cause is TRUE
  )]
  # print(skeleton_gd$death_categorical[!is.na(skeleton_gd$death_categorical)& skeleton_gd$rowind_register_tag == "case"])
  # print(skeleton_gd$death_categorical[!is.na(skeleton_gd$death_categorical)& skeleton_gd$rowind_register_tag == "control"])

  # creating collapsed, 2 level version
  skeleton_gd[, death_binary := fcase(
    death_external_causes == TRUE, "external_causes",
    death_any_disease == TRUE, "any_disease",
    default = NA_character_  # Default to NA if no cause is TRUE
  )]
  # this can compare categories to see if I made any errors in recoding/grouping
  skeleton_gd[!is.na(death_binary), .(.N), keyby=.(death_binary, rowind_register_tag)]
  skeleton_gd[!is.na(death_binary), .(.N), keyby=.(rowind_register_tag, death_binary, death_categorical)]

  skeleton_gd[death_binary=="any_disease" & is.na(death_categorical)]$id
  apply(skeleton_gd[id==50847   , death_causes_columns ,with=F],2,sum)

  #--------------------------------------------------------------------------------------------------------
  #demographics based on time of first gd dx, row independent
  #education, first line should create an index of when each person received their first gd dx.
  #this is because I need the other variables to be reflective of when the participant entered the study
  #This index tells us the row number where first dx was identified, by id number
  skeleton_gd[isoyear==rowind_isoyear_first_gd, temp := rowdep_edu_cat]
  skeleton_gd[, rowind_education_first_dx := first_non_na(temp), by = id]
  skeleton_gd[, temp := NULL]
  skeleton_gd[id==69]

  mean(!is.na(skeleton_gd$rowind_education_first_dx))
  mean(!is.na(skeleton_gd[rowind_register_tag=="father"]$rowind_education_first_dx))
  mean(!is.na(skeleton_gd[rowind_register_tag=="mother"]$rowind_education_first_dx))

  skeleton_gd[,.(mean(is.na(rowind_education_first_dx))),by=.(id)]

  #income (this will need to change since we need parent income, also needs to be adjusted by year)
  skeleton_gd[isoyear==rowind_isoyear_first_gd, temp := rowdep_income_inflation_adjusted]
  skeleton_gd[, rowind_income_first_dx := first_non_na(temp), by = id]
  skeleton_gd[, temp := NULL]
  skeleton_gd[id==69]
  #country
  skeleton_gd[isoyear==rowind_isoyear_first_gd, temp := rowind_birthcountry]
  skeleton_gd[, rowind_country_first_dx := first_non_na(temp), by = id]
  skeleton_gd[, temp := NULL]
  skeleton_gd[id==69]
  #date of study exit
  skeleton_gd[, rowind_isoyearweeksun_last_observation := max(isoyearweek_sunday), by = id]
  summary(skeleton_gd$rowind_isoyearweeksun_last_observation)
  #age at study exit
  #creating variable where the last lien by id==true
  skeleton_gd[, last_observation := (isoyearweek_sunday == max(isoyearweek_sunday)), by = id]
  #creating row independent variable for age at the time of last row
  skeleton_gd[last_observation == TRUE, temp := age]
  # spread the MINIMUM AGE over all rows of the person
  skeleton_gd[, rowind_age_last_observation := swereg::min_with_infinite_as_na(temp, na.rm=T), by = .(id)]
  skeleton_gd[id==69]
  skeleton_gd[, temp := NULL] # REMEMBER TO DELETE TEMP AFTERWARDS
  summary(skeleton_gd$rowind_age_last_observation)

  # copy information from cases to controls
  skeleton_gd[rowind_register_tag=="case", rowind_shared_saab_of_case := saab]
  skeleton_gd[, rowind_shared_saab_of_case := swereg::first_non_na(rowind_shared_saab_of_case), by =.(id_of_case)]

  skeleton_gd[, uniqueN(id), by=rowind_register_tag]

  skeleton_gd[, uniqueN(id), keyby=.(saab, rowind_register_tag)]
  skeleton_gd[rowind_register_tag=="control_sibling" & rowind_shared_saab_of_case=="Male" & saab=="Male", rowind_register_tag := "control_sibling_same_sex"]
  skeleton_gd[rowind_register_tag=="control_sibling" & rowind_shared_saab_of_case=="Female" & saab=="Female", rowind_register_tag := "control_sibling_same_sex"]
  skeleton_gd[rowind_register_tag=="control_sibling" & rowind_shared_saab_of_case=="Male" & saab=="Female", rowind_register_tag := "control_sibling_opposite_sex"]
  skeleton_gd[rowind_register_tag=="control_sibling" & rowind_shared_saab_of_case=="Female" & saab=="Male", rowind_register_tag := "control_sibling_opposite_sex"]
  skeleton_gd[, uniqueN(id), keyby=.(saab, rowind_register_tag)]

  setnames(skeleton_gd, "rowind_register_tag", "rowind_register_tag_control4")
  skeleton_gd[, rowind_register_tag_control2 := rowind_register_tag_control4]
  skeleton_gd[rowind_register_tag_control2 %in% c("control_same_sex", "control_opposite_sex"), rowind_register_tag_control2 := "control"]
  skeleton_gd[rowind_register_tag_control2 %in% c("control_sibling_same_sex", "control_sibling_opposite_sex"), rowind_register_tag_control2 := "control_sibling"]

  skeleton_gd[, uniqueN(id), by=rowind_register_tag_control4]
  skeleton_gd[, uniqueN(id), by=rowind_register_tag_control2]

  qs::qsave(skeleton_gd, fs::path(data_generic_skeleton, paste0("skeleton2_clean_",file_number,".qs")))
}
