## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(data.table)
library(csutil)
library(qs)

## -----------------------------------------------------------------------------
# Setup for batched processing
BATCH_SIZE <- 50  # Small for demonstration - use 1000-5000 for real studies
OUTPUT_DIR <- tempdir()  # Use temporary directory for vignette

# Setup for demonstration (see skeleton1_create vignette for detailed data integration)
# For batching, we organize data into large_data_files list for memory management
large_data_files <- list(
  "fake_demographics" = swereg::fake_demographics |>
    data.table::copy() |>
    swereg::make_lowercase_names(date_columns = "fodelseman"),
  "fake_annual_family" = swereg::fake_annual_family |>
    swereg::make_lowercase_names(),
  "fake_diagnoses" = swereg::fake_diagnoses |>
    data.table::copy() |>
    swereg::make_lowercase_names(date_columns = "indatum"),
  "fake_diagnoses" = swereg::fake_diagnoses |>
    data.table::copy() |>
    swereg::make_lowercase_names(date_columns = "indatum"),
  "fake_prescriptions" = swereg::fake_prescriptions |>
    data.table::copy() |>
    swereg::make_lowercase_names(date_columns = "edatum"),
  "fake_cod" = swereg::fake_cod |>
    data.table::copy() |>
    swereg::make_lowercase_names(date_columns = "dodsdat")
)

cat("Data loaded and preprocessed - ready for batched processing\n")

## -----------------------------------------------------------------------------
skeleton1_create_batch <- function(batch_ids, batch_number, large_data_files) {
  # Declare variables for data.table non-standard evaluation
  lopnr <- p444_lopnr_personnr <- NULL
  
  cat("Processing batch", batch_number, "with", length(batch_ids), "individuals\n")
  
  # Create skeleton for this batch
  skeleton <- swereg::create_skeleton(
    ids = batch_ids,
    date_min = "2015-01-01",
    date_max = "2018-12-31"  # Shorter period for demonstration
  )
  
  # Add demographics
  demographics_subset <- large_data_files[["fake_demographics"]][lopnr %in% batch_ids]
  if (nrow(demographics_subset) > 0) {
    swereg::add_onetime(skeleton, demographics_subset, id_name = "lopnr")
  }
  
  # Add annual data
  annual_subset <- large_data_files[["fake_annual_family"]][lopnr %in% batch_ids]
  if (nrow(annual_subset) > 0) {
    swereg::add_annual(skeleton, annual_subset, id_name = "lopnr", isoyear = 2015)
  }
  
  # Add diagnoses
  diagnoses_subset <- rbindlist(list(
    large_data_files[["fake_diagnoses"]][lopnr %in% batch_ids],
    large_data_files[["fake_diagnoses"]][lopnr %in% batch_ids]
  ), use.names = TRUE, fill = TRUE)
  
  if (nrow(diagnoses_subset) > 0) {
    swereg::add_diagnoses(
      skeleton,
      diagnoses_subset,
      id_name = "lopnr",
      diags = list(
        "depression" = c("F32", "F33"),
        "anxiety" = c("F40", "F41"),
        "gender_dysphoria" = c("F64"),
        "psychosis" = c("F20", "F25")
      )
    )
  }
  
  # Add prescriptions
  prescriptions_subset <- large_data_files[["fake_prescriptions"]][p444_lopnr_personnr %in% batch_ids]
  if (nrow(prescriptions_subset) > 0) {
    swereg::add_rx(
      skeleton,
      prescriptions_subset,
      id_name = "p444_lopnr_personnr",
      rxs = list(
        "antidepressants" = c("N06A"),
        "antipsychotics" = c("N05A"),
        "hormones" = c("G03")
      )
    )
  }
  
  # Add cause of death
  cod_subset <- large_data_files[["fake_cod"]][lopnr %in% batch_ids]
  if (nrow(cod_subset) > 0) {
    swereg::add_cods(
      skeleton,
      cod_subset,
      id_name = "lopnr",
      cods = list(
        "external_death" = c("X60", "X70"),
        "cardiovascular_death" = c("I21", "I22")
      )
    )
  }
  
  # Save batch using qs (much faster than RDS)
  output_file <- file.path(OUTPUT_DIR, paste0("skeleton1_create_", batch_number, ".qs"))
  qs::qsave(skeleton, output_file)
  
  cat("Saved skeleton1_create", batch_number, ":", nrow(skeleton), "rows\n")
  return(output_file)
}

## -----------------------------------------------------------------------------
# Process first 100 individuals in 2 batches
data("fake_person_ids", package = "swereg")
ids_subset <- fake_person_ids[1:100]
id_batches <- csutil::easy_split(ids_subset, BATCH_SIZE)

skeleton1_files <- vector("character", length(id_batches))
for (i in seq_along(id_batches)) {
  skeleton1_files[i] <- skeleton1_create_batch(id_batches[[i]], i, large_data_files)
}

cat("skeleton1_create phase completed for", length(id_batches), "batches\n")

# CRITICAL: Remove large datasets from memory
# This is the key benefit of organizing data into large_data_files - 
# easy cleanup of "the big lump of data" after skeleton1_create is complete
rm(large_data_files)
gc()  # Force garbage collection

cat("Large datasets removed from memory\n")

## -----------------------------------------------------------------------------
skeleton2_clean_batch <- function(batch_number) {
  cat("Cleaning batch", batch_number, "\n")
  
  # Load skeleton1 for this batch
  input_file <- file.path(OUTPUT_DIR, paste0("skeleton1_create_", batch_number, ".qs"))
  skeleton <- qs::qread(input_file)
  
  # CLEANING OPERATIONS (using only data within skeleton)
  
  # 1. Create age variable
  skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
  skeleton[, age := isoyear - birth_year]
  
  # 2. Create mental health composite variables
  skeleton[, any_mental_health := depression | anxiety | psychosis]
  skeleton[, severe_mental_illness := psychosis | gender_dysphoria]
  
  # 3. Create medication concordance variables
  skeleton[, depression_treated := depression & antidepressants]
  skeleton[, psychosis_treated := psychosis & antipsychotics]
  
  # 4. Create life stage variables
  skeleton[, life_stage := fcase(
    age < 18, "child",
    age >= 18 & age < 65, "adult", 
    age >= 65, "elderly",
    default = "unknown"
  )]
  
  # 5. Create outcome variables
  skeleton[, death_any := external_death | cardiovascular_death]
  
  # 6. Filter to valid ages and reasonable time periods
  skeleton <- skeleton[age >= 0 & age <= 100]
  skeleton <- skeleton[isoyear >= 2015]  # Remove historical rows
  
  # 7. Create person-level summaries for annual data
  if (skeleton[, any(is_isoyear == TRUE)]) {
    skeleton[is_isoyear == TRUE, n_mental_health_year := sum(c(depression, anxiety, psychosis), na.rm = TRUE), by = .(id, isoyear)]
    skeleton[is_isoyear == TRUE, treatment_adherence := mean(c(depression_treated, psychosis_treated), na.rm = TRUE), by = .(id, isoyear)]
  }
  
  # 8. Create registry tag variables (simulate case-control study)
  skeleton[, register_tag := fcase(
    gender_dysphoria == TRUE, "case",
    id %% 3 == 0, "control_matched",
    default = "control_population"
  )]
  
  # 9. Create shared case variables (for matched studies)
  # Find first gender dysphoria diagnosis for cases
  gd_first <- skeleton[gender_dysphoria == TRUE & register_tag == "case", 
                       .(first_gd_year = min(isoyear, na.rm = TRUE)), 
                       by = .(id)]
  
  # Add to skeleton
  skeleton[gd_first, on = "id", first_gd_year := first_gd_year]
  
  # For controls, assign their matched case's first GD year (simplified)
  skeleton[register_tag != "case", first_gd_year := 2016]  # Simplified for demo
  
  # 10. Remove temporary variables
  skeleton[, c("fodelseman", "birth_year") := NULL]
  
  # Save cleaned skeleton using qs
  output_file <- file.path(OUTPUT_DIR, paste0("skeleton2_clean_", batch_number, ".qs"))
  qs::qsave(skeleton, output_file)
  
  cat("Cleaned skeleton2_clean", batch_number, ":", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")
  return(output_file)
}

## -----------------------------------------------------------------------------
# Process all batches for skeleton2_clean
skeleton2_files <- vector("character", length(id_batches))
for (i in seq_along(id_batches)) {
  skeleton2_files[i] <- skeleton2_clean_batch(i)
}

cat("skeleton2_clean phase completed\n")

## -----------------------------------------------------------------------------
skeleton3_analyze <- function(skeleton2_files) {
  cat("Creating analysis dataset from", length(skeleton2_files), "batches\n")
  
  # Load all cleaned batches
  all_batches <- vector("list", length(skeleton2_files))
  for (i in seq_along(skeleton2_files)) {
    skeleton <- qs::qread(skeleton2_files[i])
    
    # Extract analysis variables (collapse weekly data to yearly)
    # Use max_with_infinite_as_na because we're aggregating weekly data to yearly:
    # "did anything happen this year?"
    analysis_data <- skeleton[
      .(
        # Demographic variables
        age = swereg::first_non_na(age),
        life_stage = swereg::first_non_na(life_stage),
        
        # Outcome variables - use max to detect "did anything happen this year?"
        any_mental_health = swereg::max_with_infinite_as_na(any_mental_health),
        severe_mental_illness = swereg::max_with_infinite_as_na(severe_mental_illness),
        depression = swereg::max_with_infinite_as_na(depression),
        anxiety = swereg::max_with_infinite_as_na(anxiety),
        psychosis = swereg::max_with_infinite_as_na(psychosis),
        gender_dysphoria = swereg::max_with_infinite_as_na(gender_dysphoria),
        
        # Treatment variables
        antidepressants = swereg::max_with_infinite_as_na(antidepressants),
        antipsychotics = swereg::max_with_infinite_as_na(antipsychotics),
        hormones = swereg::max_with_infinite_as_na(hormones),
        
        # Derived variables
        depression_treated = swereg::max_with_infinite_as_na(depression_treated),
        psychosis_treated = swereg::max_with_infinite_as_na(psychosis_treated),
        
        # Mortality
        death_any = swereg::max_with_infinite_as_na(death_any),
        
        # Study design variables
        first_gd_year = swereg::first_non_na(first_gd_year),
        
        # Summary variables
        n_mental_health_year = swereg::first_non_na(n_mental_health_year),
        treatment_adherence = swereg::first_non_na(treatment_adherence)
      ),
      by = .(id, study_year = isoyear, register_tag)
    ]
    
    all_batches[[i]] <- analysis_data
  }
  
  # Combine all batches
  final_analysis <- rbindlist(all_batches)
  
  # Save final analysis dataset
  output_file <- file.path(OUTPUT_DIR, "skeleton3_analyze.qs")
  qs::qsave(final_analysis, output_file)
  
  cat("Saved skeleton3_analyze:", nrow(final_analysis), "person-years\n")
  
  return(final_analysis)
}

# Create final analysis dataset
analysis_data <- skeleton3_analyze(skeleton2_files)

cat("Analysis dataset created:", nrow(analysis_data), "person-years\n")
cat("Variables:", ncol(analysis_data), "\n")
cat("Study population breakdown:\n")
print(table(analysis_data$register_tag))

## -----------------------------------------------------------------------------
# Show structure
str(analysis_data)

# Example analysis: Depression prevalence by register tag
depression_summary <- analysis_data[, .(
  n_person_years = .N,
  depression_prev = mean(depression, na.rm = TRUE),
  # Fix treatment rate calculation to avoid NaN
  treatment_rate = ifelse(sum(depression, na.rm = TRUE) > 0, 
                         mean(depression_treated[depression == TRUE], na.rm = TRUE), 
                         NA_real_)
), by = .(register_tag)]

print(depression_summary)

## -----------------------------------------------------------------------------
# Example: Mental health treatment patterns
treatment_summary <- analysis_data[any_mental_health == TRUE, .(
  antidepressant_use = mean(antidepressants, na.rm = TRUE),
  antipsychotic_use = mean(antipsychotics, na.rm = TRUE),
  hormone_use = mean(hormones, na.rm = TRUE),
  mean_age = mean(age, na.rm = TRUE)
), by = register_tag]

print(treatment_summary)

## -----------------------------------------------------------------------------
# For production studies, batch size depends on:
# - Available RAM: Larger batches use more memory but fewer file operations
# - Processing time: Very large batches can hit memory limits
# - File system: Too many small files can slow down I/O

# Recommended batch sizes:
# - 1,000-2,000 individuals for modest hardware (8-16GB RAM)
# - 5,000-10,000 individuals for high-memory systems (32-64GB RAM)
# - Each batch uses ~200-500MB RAM during processing

## -----------------------------------------------------------------------------
# For production studies, organize files systematically:
# OUTPUT_DIR/
#   skeleton1/
#     skeleton1_create_1.qs, skeleton1_create_2.qs, ...
#   skeleton2/ 
#     skeleton2_clean_1.qs, skeleton2_clean_2.qs, ...
#   skeleton3/
#     skeleton3_analyze.qs

# Clean up strategy:
# 1. Keep skeleton2 files for quality checks
# 2. Remove skeleton1 files after skeleton2_clean succeeds
# 3. Archive skeleton2 files after skeleton3_analyze succeeds

## -----------------------------------------------------------------------------
# Production workflows should include error handling
skeleton1_create_batch_safe <- function(batch_ids, batch_number, large_data_files) {
  tryCatch({
    return(skeleton1_create_batch(batch_ids, batch_number, large_data_files))
  }, error = function(e) {
    cat("ERROR in batch", batch_number, ":", e$message, "\n")
    return(NULL)
  })
}

# Check for failed batches before proceeding
# failed_batches <- which(sapply(skeleton1_files, is.null))
# This allows resuming failed batch processing

