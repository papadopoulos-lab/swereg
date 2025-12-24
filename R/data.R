#' Fake Swedish Registry Datasets
#'
#' These datasets contain synthetic Swedish healthcare registry data for development,
#' testing, and vignettes. They mimic the structure and format of real Swedish
#' healthcare registries but contain completely fabricated data.
#'
#' @format Various data structures matching real Swedish registries:
#'
#' @section fake_person_ids:
#' A numeric vector of 1000 fake personal identifiers (lopnr).
#' Used as reference IDs across all other datasets.
#'
#' @section fake_demographics:
#' Demographics data (SCB format) with 1000 records:
#' \describe{
#'   \item{lopnr}{Personal identifier matching fake_person_ids}
#'   \item{fodelseman}{Birth year-month (YYYYMM format)}
#'   \item{DodDatum}{Death date (YYYYMMDD format) or empty string}
#' }
#'
#' @section fake_annual_family:
#' Annual family status data (SCB format) with 1000 records:
#' \describe{
#'   \item{LopNr}{Personal identifier (mixed case as in real data)}
#'   \item{FamTyp}{Family type code (2-digit character)}
#' }
#'
#' @section fake_diagnoses:
#' Combined diagnosis data with ~5000 records from three sources:
#' \describe{
#'   \item{SOURCE}{Data source: "inpatient", "outpatient", or "cancer"}
#'   \item{LopNr}{Personal identifier}
#'   \item{AR}{Year of care}
#'   \item{INDATUMA}{Admission date (YYYYMMDD character)}
#'   \item{INDATUM}{Admission date (Date class)}
#'   \item{UTDATUMA}{Discharge date (YYYYMMDD character, inpatient only)}
#'   \item{UTDATUM}{Discharge date (Date class, inpatient only)}
#'   \item{HDIA}{Main diagnosis (ICD-10 code)}
#'   \item{DIA1-DIA30}{Additional diagnoses}
#'   \item{EKOD1-EKOD7}{External cause codes}
#'   \item{OP}{Operation codes}
#'   \item{ICDO3}{ICD-O-3 morphology codes (populated for cancer source)}
#'   \item{SNOMED3}{SNOMED-CT version 3 codes}
#'   \item{SNOMEDO10}{SNOMED-CT version 10 codes}
#' }
#'
#' The SOURCE column identifies the registry origin:
#' \itemize{
#'   \item "inpatient": NPR inpatient data (~2000 records)
#'   \item "outpatient": NPR outpatient data (~2000 records)
#'   \item "cancer": Cancer registry data (~1000 records, always has ICDO3)
#' }
#'
#' @section fake_prescriptions:
#' Prescription drug dispensing data (LMED format) with ~8000 records:
#' \describe{
#'   \item{p444_lopnr_personnr}{Personal identifier with p444 prefix}
#'   \item{Fall}{Case indicator}
#'   \item{Kontroll}{Control indicator}
#'   \item{VARUNR}{Product number}
#'   \item{ATC}{ATC classification code}
#'   \item{ALDER}{Age at prescription}
#'   \item{LK}{Healthcare county code}
#'   \item{EDATUM}{End date}
#'   \item{FDATUM}{Start date}
#'   \item{OTYP}{Origin type}
#'   \item{...}{Additional 27 columns matching real LMED structure}
#' }
#'
#' @section fake_cod:
#' Cause of death data with ~50 records (Swedish registry format):
#' \describe{
#'   \item{lopnr}{Personal identifier}
#'   \item{dodsdat}{Date of death}
#'   \item{ulorsak}{Underlying cause of death (ICD-10) - Swedish variable name}
#'   \item{morsak1}{First multiple/contributory cause of death}
#'   \item{morsak2}{Second multiple/contributory cause of death}
#' }
#'
#' @details
#' These datasets are created by \code{dev/generate_fake_data.R} and contain:
#'
#' \strong{Key features:}
#' \itemize{
#'   \item Personal identifiers are numeric (e.g., 623334, 753064)
#'   \item Prescription data uses column name "p444_lopnr_personnr"
#'   \item ICD-10 codes include gender dysphoria (F64*), mental health (F20*, F32*, F40*), and physical health codes
#'   \item ATC codes include hormone therapy (G03*), mental health medications (N05*, N06*)
#'   \item Date ranges span 1978-2021 depending on registry
#'   \item Realistic missing data patterns
#'   \item SOURCE column in fake_diagnoses tracks data origin
#' }
#'
#' \strong{Usage requirements:}
#' \itemize{
#'   \item Always apply \code{swereg::make_lowercase_names()} after loading data
#'   \item Use appropriate identifier column names (lopnr vs p444_lopnr_personnr)
#'   \item Follow Swedish registry conventions for date formats
#'   \item Filter by SOURCE column when needed (e.g., SOURCE == "cancer" for ICD-O-3)
#' }
#'
#' @source Generated synthetic data based on real Swedish healthcare registry structures
#'
#' @examples
#' \dontrun{
#' # Load fake data
#' data("fake_person_ids")
#' data("fake_demographics")
#' data("fake_diagnoses")
#'
#' # CRITICAL: Apply lowercase names
#' swereg::make_lowercase_names(fake_demographics)
#' swereg::make_lowercase_names(fake_diagnoses, date_columns = "indatum")
#'
#' # Check source distribution
#' table(fake_diagnoses$source)
#'
#' # Filter by source
#' inpatient_only <- fake_diagnoses[source == "inpatient"]
#' cancer_only <- fake_diagnoses[source == "cancer"]
#'
#' # Create skeleton with fake data
#' skeleton <- create_skeleton(
#'   ids = fake_person_ids[1:100],
#'   date_min = "2015-01-01",
#'   date_max = "2020-12-31"
#' )
#' }
#'
#' @name fake_data
#' @aliases fake_person_ids fake_demographics fake_annual_family
#'   fake_diagnoses fake_prescriptions fake_cod
#' @docType data
#' @keywords datasets
NULL

#' @rdname fake_data
"fake_person_ids"

#' @rdname fake_data
"fake_demographics"

#' @rdname fake_data
"fake_annual_family"

#' @rdname fake_data
"fake_diagnoses"

#' @rdname fake_data
"fake_prescriptions"

#' @rdname fake_data
"fake_cod"
