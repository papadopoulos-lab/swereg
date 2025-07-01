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
#' @section fake_inpatient_diagnoses:
#' Inpatient hospital diagnoses (NPR format) with ~3000 records:
#' \describe{
#'   \item{LopNr}{Personal identifier}
#'   \item{AR}{Year of care}
#'   \item{INDATUMA}{Admission date (YYYYMMDD character)}
#'   \item{INDATUM}{Admission date (Date class)}
#'   \item{UTDATUMA}{Discharge date (YYYYMMDD character)}
#'   \item{UTDATUM}{Discharge date (Date class)}
#'   \item{HDIA}{Main diagnosis (ICD-10 code)}
#'   \item{DIA1-DIA30}{Additional diagnoses}
#'   \item{EKOD1-EKOD7}{External cause codes}
#'   \item{OP}{Operation codes}
#' }
#' 
#' @section fake_outpatient_diagnoses:
#' Outpatient specialist diagnoses (NPR format) with ~2000 records.
#' Same structure as inpatient data but without discharge dates.
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
#' Cause of death data with ~50 records:
#' \describe{
#'   \item{lopnr}{Personal identifier}
#'   \item{dodsdat}{Date of death}
#'   \item{underlying_cod}{Underlying cause of death (ICD-10)}
#'   \item{contributory_cod1}{First contributory cause}
#'   \item{contributory_cod2}{Second contributory cause}
#' }
#' 
#' @details 
#' These datasets are created by \code{dev/generate_fake_data.R} and contain:
#' 
#' \strong{Key Features:}
#' \itemize{
#'   \item Personal identifiers are numeric (e.g., 623334, 753064)
#'   \item Prescription data uses column name "p444_lopnr_personnr" 
#'   \item ICD-10 codes include gender dysphoria (F64*), mental health (F20*, F32*, F40*), and physical health codes
#'   \item ATC codes include hormone therapy (G03*), mental health medications (N05*, N06*)
#'   \item Date ranges span 1978-2021 depending on registry
#'   \item Realistic missing data patterns
#' }
#' 
#' \strong{Usage Requirements:}
#' \itemize{
#'   \item Always apply \code{swereg::make_lowercase_names()} after loading data
#'   \item Use appropriate identifier column names (lopnr vs p444_lopnr_personnr)
#'   \item Follow Swedish registry conventions for date formats
#' }
#' 
#' @source Generated synthetic data based on real Swedish healthcare registry structures
#' 
#' @examples
#' \dontrun{
#' # Load fake data
#' data("fake_person_ids")
#' data("fake_demographics")
#' data("fake_prescriptions")
#' 
#' # CRITICAL: Apply lowercase names
#' swereg::make_lowercase_names(fake_demographics)
#' swereg::make_lowercase_names(fake_prescriptions)
#' 
#' # Create skeleton with fake data
#' skeleton <- create_skeleton(
#'   ids = fake_person_ids[1:100],
#'   date_min = "2015-01-01",
#'   date_max = "2020-12-31"
#' )
#' 
#' # Add demographics
#' add_onetime(skeleton, fake_demographics[lopnr %in% fake_person_ids[1:100]], 
#'             id_name = "lopnr")
#' }
#' 
#' @name fake_data
#' @aliases fake_person_ids fake_demographics fake_annual_family 
#'   fake_inpatient_diagnoses fake_outpatient_diagnoses fake_prescriptions fake_cod
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
"fake_inpatient_diagnoses"

#' @rdname fake_data
"fake_outpatient_diagnoses"

#' @rdname fake_data
"fake_prescriptions"

#' @rdname fake_data
"fake_cod"