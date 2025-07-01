library(data.table)
library(magrittr)

devtools::load_all(".")

# mht
folder <- "/data/argos/Bronze/Embla_data/_MHT/mht_raw/scb/"
idx <- read.csv(
  fs::path(folder, "Individ_2007.csv"),
  fileEncoding = "UTF-16"
) %>%
  setDT()
idx

# gd
folder <- "/data/argos/Bronze/Postdoc_Kristen/2023-gd-register-clark-gat-descriptives/data_raw/"
id <- haven::read_sas(
  fs::path(folder, "SCB/fp_lev_fall_och_kontroller_1.sas7bdat")
) %>%
  setDT()
id

ids <- unique(id$lopnr_fall)

skeleton <- swereg::create_skeleton(
  ids = ids,
  date_min = "2001-01-01",
  date_max = "2015-12-31"
)

# add one-time information
d <- haven::read_sas(
  fs::path(folder, "SCB/demografi.sas7bdat")
) %>%
  dplyr::filter(lopnr %in% ids) %>%
  setDT()

swereg::add_onetime(
  skeleton,
  d,
  id_name = "lopnr"
)

# add annual information
isoyear <- 2001
id_name <- "LopNr"

d <- haven::read_sas(
  fs::path(folder, "SCB/fp_lev_famtyp2001.sas7bdat")
) %>%
  dplyr::filter(LopNr %in% ids) %>%
  setDT()

swereg::add_annual(
  skeleton,
  d,
  id_name = "LopNr",
  isoyear = 2001
)

# add ICD-10 information
# diagnoses and surgeries
# outpatient
ov <- haven::read_sas(file.path(folder, "Sos", "ov.sas7bdat")) %>%
  dplyr::filter(LopNr %in% ids) %>%
  setDT() # convert to data.table
# inpatient
sv <- haven::read_sas(file.path(folder, "Sos", "sv.sas7bdat")) %>%
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
      "^296,", "!296,00" # ICD8
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
  id_name = "LopNr",
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
)

rm("diagnoses_and_operations")

skeleton

xtabs(~skeleton$diag_gd_icd10_F64_089)
for(i in stringr::str_subset(names(skeleton), "^op")) print(xtabs(~skeleton[[i]]))

xtabs(~skeleton[is_isoyear==T]$icd10_F64_089)

skeleton[is_isoyear==T & icd10_F64_089==T]


