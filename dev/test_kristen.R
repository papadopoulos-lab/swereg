org::initialize_project(
  env = .GlobalEnv,
  home = c(
    "~/Project Clark 2023 GAT Youth Descriptive/2023-gd-register-clark-gat-descriptives",
    "~/papadopoulos-lab/2023-gd-register-clark-gat-descriptives"
  ),
  results = c(
    "C:/Users/kricl384/Box/2023-gd-register-clark-gat-descriptives/results/",
    "~/papadopoulos-lab/2023-gd-register-clark-gat-descriptives/results"
  ),
  data_raw = c(
    "//argos.rudbeck.uu.se/MyGroups$/Bronze/Postdoc_Kristen/2023-gd-register-clark-gat-descriptives/data_raw",
    "/data/argos/Bronze/Postdoc_Kristen/2023-gd-register-clark-gat-descriptives/data_raw"
  ),
  data_processed = c(
    "//argos.rudbeck.uu.se/MyGroups$/Bronze/Postdoc_Kristen/2023-gd-register-clark-gat-descriptives/data_processed/",
    "/data/argos/Bronze/Postdoc_Kristen/2023-gd-register-clark-gat-descriptives/data_processed/"
  )
)

org::project$home
org::project$results_today # save results here



# load every single function (commands) into what is currently available in these named libraries
library(data.table)
library(ggplot2)
library(magrittr)

# get the correct ids/lopnr
id <- haven::read_sas(
  fs::path(org::project$data_raw, "SCB/fp_lev_fall_och_kontroller_1.sas7bdat")
) %>%
  setDT()
id

ids_gd <- unique(id$lopnr_fall)

# get parent data
d_parent <- haven::read_sas(
  fs::path(org::project$data_raw, "SCB/fp_lev_bioforaldrar.sas7bdat")
) %>%
  dplyr::filter(LopNr %in% ids_gd) %>%
  setDT()

ids_father <- d_parent$lopnrfar
ids_mother <- d_parent$lopnrmor

ids <- c(ids_gd, ids_father, ids_mother) %>%
  unique() %>%
  na.omit()

# Initial setup----
# create the initial skeleton
skeleton <- swereg::create_skeleton(
  ids = ids,
  date_min = "2006-01-01",
  date_max = "2016-12-31"
)

ov <- haven::read_sas(fs::path(org::project$data_raw, "Sos", "ov.sas7bdat")) %>%
  dplyr::filter(LopNr %in% ids) %>%
  setDT() # convert to data.table
# inpatient
sv <- haven::read_sas(fs::path(org::project$data_raw, "Sos", "sv.sas7bdat")) %>%
  dplyr::filter(LopNr %in% ids) %>%
  setDT()
diagnoses_and_operations <- rbindlist(list(ov, sv), use.names=T, fill=T) # put ov and sv on top of each other
rm("ov", "sv") # remove ov and sv from the RAM of the computer

swereg::add_diagnoses(
  skeleton,
  diagnoses_and_operations,
  id_name = "LopNr",
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

sum(skeleton$diag_gd_icd89_transsexual)


diagnoses_and_operations[, gd_hdia := HDIA %in% c("F640", "F648", "F649")]
diagnoses_and_operations[, gd_not_hdia := !HDIA %in% c("F640", "F648", "F649") & DIA1 %in% c("F640", "F648", "F649")]

setorder(diagnoses_and_operations, LopNr, INDATUMA)

diagnoses_and_operations <- diagnoses_and_operations[gd_hdia==TRUE | gd_not_hdia == T]

diagnoses_and_operations[, row := 1:.N, by = .(LopNr)]
diagnoses_and_operations[row>1, row := 2]

diagnoses_and_operations[row==1, .(mean(gd_hdia), mean(gd_not_hdia))]
