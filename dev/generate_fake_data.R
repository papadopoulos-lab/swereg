# Generate fake Swedish registry data based on actual data structures
# This creates realistic synthetic data matching real Swedish healthcare registries

library(data.table)
library(magrittr)

# Generate fake personal identifiers (lopnr) - based on actual format
generate_fake_lopnr <- function(n = 1000) {
  # Real lopnr range from inspection: 1 to ~240,000
  # lopnr is just a numeric identifier, no prefix
  1:n
}

# Generate fake demographics data - matches actual demografi.sas7bdat structure
generate_fake_demographics <- function(ids) {
  n <- length(ids)
  data.table(
    lopnr = ids,  # Numeric as in real data
    fodelseman = sprintf("%6d", sample(1950:2005, n, replace = TRUE)), # Birth year-month (YYYY)
    DodDatum = ifelse(runif(n) < 0.05,
                     sprintf("%8d", sample(1980:2020, n, replace = TRUE)),
                     "") # Death date YYYYMMDD or empty (5% have death dates)
  )
}

# Generate fake annual family data - matches actual fp_lev_famtyp2001.sas7bdat
generate_fake_annual_family <- function(ids, year = 2001) {
  n <- length(ids)
  data.table(
    LopNr = ids,  # Numeric mixed case as in real data
    FamTyp = sprintf("%02d", sample(11:89, n, replace = TRUE)) # Family type as character with leading zero
  )
}

# Generate fake combined diagnoses data with SOURCE column
# SOURCE can be: "inpatient", "outpatient", or "cancer"
generate_fake_diagnoses <- function(ids, n_inpatient = 2000, n_outpatient = 2000, n_cancer = 1000) {

  # ICD-O-3 morphology codes (cancer registry)
  icdo3_codes <- c(
    "8140/3", "8500/3", "8070/3", "8010/3", "8480/3",
    "8720/3", "8050/3", "8260/3", "8310/3", "8070/2"
  )

  # SNOMED-CT v3 codes
 snomed3_codes <- c(
    "80146002", "44054006", "13645005", "73211009",
    "22298006", "38341003", "84114007", "59621000"
  )

  # SNOMED-CT v10 codes
  snomedo10_codes <- c(
    "387713003", "22298006", "414545008", "267036007",
    "128462008", "116154003", "39065001", "84114007"
  )

  # ICD-10 codes
  icd10_codes <- c(
    "J069", "M255", "M244", "M544", "M809", "M796", "N950B", "N951", "M796G",
    "K802", "464,01", "009,20", "724,10", "591,99", "453,09", "753,29",
    "F640", "F648", "F649", "F6489",
    "F200", "F201", "F209", "F320", "F321", "F329", "F300", "F301", "F319",
    "F412", "F413", "F419", "F500", "F501", "F509", "F840", "F841", "F849",
    "F900", "F901", "F909", "F600", "F601", "F609",
    "I10", "I21", "I22", "I25", "E10", "E11", "E14", "J44", "J45", "J46",
    "K25", "K26", "K27", "N39", "M79", "R06"
  )

  # Cancer-specific ICD-10 codes (for HDIA in cancer records)
  cancer_icd10 <- c("C50", "C61", "C34", "C18", "C20", "C43", "C67", "C64", "C25", "C16")

  # Helper function to create base record structure
  create_records <- function(n, source_type) {
    dates <- sample(seq(as.Date("1978-01-01"), as.Date("2020-12-31"), by = "day"), n, replace = TRUE)

    dt <- data.table(
      SOURCE = source_type,
      LopNr = sample(ids, n, replace = TRUE),
      AR = as.integer(format(dates, "%Y")),
      INDATUMA = format(dates, "%Y%m%d"),
      INDATUM = dates
    )

    # Set diagnosis codes based on source
    if (source_type == "cancer") {
      # Cancer records: ICD-O-3 always filled, cancer-specific ICD-10
      dt[, HDIA := sample(cancer_icd10, n, replace = TRUE)]
      dt[, ICDO3 := sample(icdo3_codes, n, replace = TRUE)]
      dt[, SNOMED3 := ""]
      dt[, SNOMEDO10 := ""]
    } else {
      # Inpatient/outpatient: regular ICD-10, rarely ICD-O-3/SNOMED
      dt[, HDIA := sample(icd10_codes, n, replace = TRUE)]
      dt[, ICDO3 := sample(c(icdo3_codes, ""), n, replace = TRUE, prob = c(rep(0.01, 10), 0.9))]
      dt[, SNOMED3 := sample(c(snomed3_codes, ""), n, replace = TRUE, prob = c(rep(0.01, 8), 0.92))]
      dt[, SNOMEDO10 := sample(c(snomedo10_codes, ""), n, replace = TRUE, prob = c(rep(0.01, 8), 0.92))]
    }

    # External cause codes (mostly empty)
    dt[, EKOD1 := sample(c("", "X60", "X61", "X70", "X71", "X80", "X81"), n,
                         replace = TRUE, prob = c(0.95, rep(0.01, 6)))]
    dt[, `:=`(EKOD2 = "", EKOD3 = "", EKOD4 = "", EKOD5 = "")]

    # Operation codes
    dt[, OP := sample(c("", "HAC10", "AAF00", "JDF00"), n, replace = TRUE, prob = c(0.8, 0.05, 0.05, 0.1))]

    # Secondary diagnoses (DIA1-DIA30)
    dt[, DIA1 := sample(c("", icd10_codes), n, replace = TRUE, prob = c(0.7, rep(0.3/length(icd10_codes), length(icd10_codes))))]
    dt[, DIA2 := sample(c("", icd10_codes), n, replace = TRUE, prob = c(0.85, rep(0.15/length(icd10_codes), length(icd10_codes))))]
    dt[, DIA3 := sample(c("", icd10_codes), n, replace = TRUE, prob = c(0.93, rep(0.07/length(icd10_codes), length(icd10_codes))))]
    for (i in 4:30) {
      dt[[paste0("DIA", i)]] <- ""
    }

    # Discharge dates (inpatient only)
    if (source_type == "inpatient") {
      dt[, UTDATUMA := format(INDATUM + sample(0:30, .N, replace = TRUE), "%Y%m%d")]
      dt[, UTDATUM := INDATUM + sample(0:30, .N, replace = TRUE)]
    } else {
      dt[, UTDATUMA := NA_character_]
      dt[, UTDATUM := as.Date(NA)]
    }

    # Extra EKOD columns for inpatient
    dt[, `:=`(EKOD6 = "", EKOD7 = "")]

    return(dt)
  }

  # Generate records for each source
  inpatient <- create_records(n_inpatient, "inpatient")
  outpatient <- create_records(n_outpatient, "outpatient")
  cancer <- create_records(n_cancer, "cancer")

  # Combine all records
  dt <- rbindlist(list(inpatient, outpatient, cancer), use.names = TRUE, fill = TRUE)

  # Shuffle rows
  dt <- dt[sample(.N)]

  return(dt)
}

# Generate fake prescription data - matches actual LMED structure
generate_fake_prescriptions <- function(ids, n_records = 10000) {
  # Real ATC codes from inspection plus hormone therapy codes
  real_atc <- c(
    # From actual data inspection
    "C09", "N05C", "G03FA01", "N06A", "G03CA03", "G03FA12", "N02A", "C03",
    "C07", "C08", "G03FA17", "N05B", "C10AA", "N05A",
    # Full hormone therapy codes (research focus)
    "G03CA01", "G03CA03", "G03CA04", "G03CA57", # Estrogens
    "G03FA01", "G03FA04", "G03FA05", "G03FA10", "G03FA11", "G03FA12", "G03FA17", # Combined
    "G03AA05", "G03AA07", "G03AA12", # Antiandrogens
    "G03BA01", "G03BA03", "G03BA11", # Progestins
    # Mental health
    "N05BA01", "N05BA02", "N05BA04", "N05BA06", # Anxiolytics
    "N06AA02", "N06AA04", "N06AA09", "N06AA10", # Antidepressants
    "N05AH02", "N05AH03", "N05AH04" # Antipsychotics
  )

  dates <- sample(seq(as.Date("2007-01-01"), as.Date("2021-12-31"), by = "day"),
                  n_records, replace = TRUE)

  data.table(
    p444_lopnr_personnr = as.integer(sample(ids, n_records, replace = TRUE)),
    Fall = sample(c(1L, NA_integer_), n_records, replace = TRUE, prob = c(0.6, 0.4)),
    Kontroll = sample(c(1L, NA_integer_), n_records, replace = TRUE, prob = c(0.4, 0.6)),
    VARUNR = sample(c(as.integer(sample(10000:999999, 1000)), NA_integer_), n_records, replace = TRUE),
    ATC = sample(real_atc, n_records, replace = TRUE),
    ALDER = as.integer(sample(18:90, n_records, replace = TRUE)),
    LK = as.integer(sample(c(115, 120, 126, 128, 180, 181, 182, 186, 480, 484, 580, 683, 687,
                            885, 1081, 1264, 1280, 1283, 1284, 1291, 1415, 1435, 1480, 1486,
                            1488, 1980, 2062, 2480, 2582, NA), n_records, replace = TRUE)),
    EDATUM = as.IDate(dates),
    FDATUM = as.IDate(dates - sample(0:30, n_records, replace = TRUE)),
    OTYP = sample(c("R", ""), n_records, replace = TRUE, prob = c(0.95, 0.05)),
    YRKE = sample(c("LK", "", "AL"), n_records, replace = TRUE, prob = c(0.8, 0.15, 0.05)),
    SPKOD1 = as.integer(sample(c(0:50, NA), n_records, replace = TRUE)),
    SPKOD2 = as.integer(sample(c(0:10, NA), n_records, replace = TRUE, prob = c(rep(0.1, 11), 0.9))),
    SPKOD3 = as.integer(sample(c(0:5, NA), n_records, replace = TRUE, prob = c(rep(0.05, 6), 0.7))),
    VERKS = as.logical(sample(c(TRUE, FALSE, NA), n_records, replace = TRUE, prob = c(0.1, 0.1, 0.8))),
    STARTFP = as.integer(sample(0:3, n_records, replace = TRUE)),
    BYTET = as.integer(sample(c(1:5, NA), n_records, replace = TRUE)),
    FNPLPACKID = sample(c(NA_real_, sample(10^12:10^13, 100)), n_records, replace = TRUE),
    FVARUNR = as.integer(sample(c(NA, sample(10000:999999, 100)), n_records, replace = TRUE)),
    NPLPACKID = sample(c(19980306100100, sample(10^12:10^13, 200), NA_real_), n_records, replace = TRUE),
    ANTAL = as.numeric(sample(1:10, n_records, replace = TRUE)),
    LKF = as.logical(sample(c(TRUE, FALSE, NA), n_records, replace = TRUE, prob = c(0.05, 0.05, 0.9))),
    arbetspl_lopnr = as.logical(rep(NA, n_records)),
    produkt = sample(c("", "Activelle", "Product A", "Product B", "Generic Med"), n_records, replace = TRUE),
    lnmn = sample(c("", "Detailed product description", "Medicine details"), n_records, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    ddd = as.numeric(sample(c(1, 8, 10, 30, 84, 100), n_records, replace = TRUE)),
    forpddd = as.numeric(sample(c(10, 50, 84, 100), n_records, replace = TRUE)),
    fddd = as.numeric(sample(c(10, 50, 84, 100), n_records, replace = TRUE)),
    dddenhet = sample(c("mg", "unit dose"), n_records, replace = TRUE, prob = c(0.9, 0.1)),
    forps = sample(c("10 tablett(er)", "100 tablett(er)", "3 x 28 tablett(er)"), n_records, replace = TRUE),
    antnum = as.integer(sample(c(10, 84, 100), n_records, replace = TRUE)),
    styrkalf = sample(c("8 mg", "10 mg", "15 mg", "1 mg/0,5 mg"), n_records, replace = TRUE),
    styrknum = as.numeric(sample(c(8, 10, 15, NA), n_records, replace = TRUE)),
    styrkaenhet = sample(c("mg", ""), n_records, replace = TRUE, prob = c(0.8, 0.2)),
    lform = sample(c("Tablett", "Filmdragerad tablett", "Kapsel"), n_records, replace = TRUE),
    subnamn = sample(c("", "Noretisteron och estrogen"), n_records, replace = TRUE, prob = c(0.9, 0.1)),
    AR = as.integer(sample(2007:2021, n_records, replace = TRUE))
  )
}

# Generate fake cause of death data - Swedish registry structure
generate_fake_cod <- function(ids, n_records = 100) {
  common_cod <- c("I219", "I220", "C780", "C800", "J440", "F030", "X609", "K720", "N179")

  death_dates <- sample(seq(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day"),
                        n_records, replace = TRUE)

  data.table(
    lopnr = sample(ids, n_records, replace = TRUE),
    dodsdat = death_dates,
    ulorsak = sample(common_cod, n_records, replace = TRUE),
    morsak1 = sample(c(common_cod, ""), n_records, replace = TRUE, prob = c(rep(0.4/length(common_cod), length(common_cod)), 0.6)),
    morsak2 = sample(c(common_cod, ""), n_records, replace = TRUE, prob = c(rep(0.2/length(common_cod), length(common_cod)), 0.8))
  )
}

# Main function to generate and save all fake data as package data
generate_all_fake_data <- function(n_individuals = 1000) {
  # Create data directory for package data
  if (!dir.exists("data")) {
    dir.create("data")
  }

  cat("Generating fake Swedish registry data with correct structures...\n")

  # Generate IDs
  ids <- generate_fake_lopnr(n_individuals)
  cat("Generated", length(ids), "fake personal identifiers\n")

  # Generate demographics (SCB format)
  fake_demographics <- generate_fake_demographics(ids)
  usethis::use_data(fake_demographics, overwrite = TRUE)
  cat("Generated demographics data:", nrow(fake_demographics), "records\n")

  # Generate annual family data (SCB format)
  fake_annual_family <- generate_fake_annual_family(ids, 2001)
  usethis::use_data(fake_annual_family, overwrite = TRUE)
  cat("Generated annual family data:", nrow(fake_annual_family), "records\n")

  # Generate combined diagnoses data (inpatient, outpatient, cancer)
  fake_diagnoses <- generate_fake_diagnoses(ids, n_inpatient = 2000, n_outpatient = 2000, n_cancer = 1000)
  usethis::use_data(fake_diagnoses, overwrite = TRUE)
  cat("Generated diagnosis data:", nrow(fake_diagnoses), "records\n")
  cat("  - inpatient:", sum(fake_diagnoses$SOURCE == "inpatient"), "\n")
  cat("  - outpatient:", sum(fake_diagnoses$SOURCE == "outpatient"), "\n")
  cat("  - cancer:", sum(fake_diagnoses$SOURCE == "cancer"), "\n")

  # Generate prescriptions (LMED format)
  fake_prescriptions <- generate_fake_prescriptions(ids, n_records = 8000)
  usethis::use_data(fake_prescriptions, overwrite = TRUE)
  cat("Generated prescriptions data:", nrow(fake_prescriptions), "records\n")

  # Generate cause of death
  fake_cod <- generate_fake_cod(ids, n_records = 50)
  usethis::use_data(fake_cod, overwrite = TRUE)
  cat("Generated cause of death data:", nrow(fake_cod), "records\n")

  # Save ID list for reference
  fake_person_ids <- ids
  usethis::use_data(fake_person_ids, overwrite = TRUE)

  cat("\nFake data generation complete!\n")
  cat("Package datasets created:\n")
  cat("- fake_demographics (SCB demographics with lopnr, fodelseman, DodDatum)\n")
  cat("- fake_annual_family (SCB annual family data with LopNr, FamTyp)\n")
  cat("- fake_diagnoses (Combined diagnoses with SOURCE: inpatient/outpatient/cancer)\n")
  cat("- fake_prescriptions (LMED prescription data with 37 columns)\n")
  cat("- fake_cod (Death registry data)\n")
  cat("- fake_person_ids (Reference list of all person IDs)\n")
  cat("\nData available to users via data(fake_demographics) etc.\n")
  cat("Use 'source(\"dev/test_with_fake_data.R\")' to test with this data\n")

  return(list(
    ids = ids,
    demographics = fake_demographics,
    annual_family = fake_annual_family,
    diagnoses = fake_diagnoses,
    prescriptions = fake_prescriptions,
    cod = fake_cod
  ))
}

# Generate the fake data if this script is run directly
if (interactive() || !exists("skip_generation")) {
  fake_data <- generate_all_fake_data()
}
