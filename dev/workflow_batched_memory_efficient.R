# Memory-efficient batched workflow for swereg
# Based on example/Run_generic_v002.R pattern:
# 1. Read large datasets
# 2. Create skeleton1_create (compilation to skeleton format) 
# 3. Create skeleton2_clean (clean variables after removing large datasets from RAM)
# 4. Batch processing to keep memory costs down

library(data.table)
devtools::load_all(".")

# Configuration
BATCH_SIZE <- 100  # Number of individuals per batch
OUTPUT_DIR <- "output_batched"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# STEP 1: READ LARGE DATASETS
cat("=== STEP 1: READING LARGE DATASETS ===\n")

read_large_files_fake <- function() {
  large_files <- list()
  
  cat("Loading fake registry data...\n")
  
  # Load all fake data with make_lowercase_names
  data("fake_person_ids")
  data("fake_demographics") 
  data("fake_annual_family")
  data("fake_inpatient_diagnoses")
  data("fake_outpatient_diagnoses") 
  data("fake_prescriptions")
  data("fake_cod")
  
  # Apply make_lowercase_names to all datasets
  swereg::make_lowercase_names(fake_demographics)
  swereg::make_lowercase_names(fake_annual_family)
  swereg::make_lowercase_names(fake_inpatient_diagnoses)
  swereg::make_lowercase_names(fake_outpatient_diagnoses)
  swereg::make_lowercase_names(fake_prescriptions)
  swereg::make_lowercase_names(fake_cod)
  
  # Store in large_files list
  large_files[["demographics"]] <- fake_demographics
  large_files[["annual_family"]] <- fake_annual_family
  
  # Combine diagnoses as in real workflow
  large_files[["diagnoses_and_operations"]] <- rbindlist(list(
    fake_inpatient_diagnoses,
    fake_outpatient_diagnoses
  ), use.names = TRUE, fill = TRUE)
  
  large_files[["prescriptions"]] <- fake_prescriptions
  large_files[["cod"]] <- fake_cod
  large_files[["person_ids"]] <- fake_person_ids
  
  cat("Loaded", length(large_files), "large datasets\n")
  return(large_files)
}

large_files <- read_large_files_fake()

# Get IDs and split into batches for memory management
all_ids <- large_files[["person_ids"]]
n_batches <- ceiling(length(all_ids) / BATCH_SIZE)

cat("Processing", length(all_ids), "individuals in", n_batches, "batches\n")

# Split IDs into batches
id_batches <- split(all_ids, ceiling(seq_along(all_ids) / BATCH_SIZE))

# STEP 2: CREATE SKELETON1_CREATE (batched)
cat("\n=== STEP 2: CREATING SKELETON1_CREATE (BATCHED) ===\n")

skeleton1_create_batch <- function(batch_number, ids_batch, large_files) {
  cat("Processing batch", batch_number, "with", length(ids_batch), "individuals\n")
  
  # Create skeleton for this batch
  skeleton <- swereg::create_skeleton(
    ids = ids_batch,
    date_min = "2010-01-01",
    date_max = "2020-12-31"
  )
  
  # Add one-time demographics
  demographics_subset <- large_files[["demographics"]][lopnr %in% ids_batch]
  if (nrow(demographics_subset) > 0) {
    swereg::add_onetime(skeleton, demographics_subset, id_name = "lopnr")
  }
  
  # Add annual family data (example for 2015)
  annual_subset <- large_files[["annual_family"]][lopnr %in% ids_batch]
  if (nrow(annual_subset) > 0) {
    swereg::add_annual(skeleton, annual_subset, id_name = "lopnr", isoyear = 2015)
  }
  
  # Add diagnoses
  diagnoses_subset <- large_files[["diagnoses_and_operations"]][lopnr %in% ids_batch]
  if (nrow(diagnoses_subset) > 0) {
    swereg::add_diagnoses(
      skeleton,
      diagnoses_subset,
      id_name = "lopnr",
      diags = list(
        "gd_any" = c("^F64"),
        "depression" = c("^F32", "^F33"),
        "anxiety" = c("^F40", "^F41")
      )
    )
  }
  
  # Add prescriptions
  prescriptions_subset <- large_files[["prescriptions"]][p444_lopnr_personnr %in% ids_batch]
  if (nrow(prescriptions_subset) > 0) {
    swereg::add_rx(
      skeleton,
      prescriptions_subset,
      id_name = "p444_lopnr_personnr",
      rxs = list(
        "antidepressants" = c("^N06A"),
        "hormones" = c("^G03")
      )
    )
  }
  
  # Add cause of death
  cod_subset <- large_files[["cod"]][lopnr %in% ids_batch]
  if (nrow(cod_subset) > 0) {
    swereg::add_cods(
      skeleton,
      cod_subset,
      id_name = "lopnr",
      cods = list(
        "external_causes" = c("^X60", "^X70")
      )
    )
  }
  
  # Save skeleton1 for this batch
  output_file <- file.path(OUTPUT_DIR, paste0("skeleton1_batch_", batch_number, ".rds"))
  saveRDS(skeleton, output_file)
  
  cat("Saved skeleton1 for batch", batch_number, ":", nrow(skeleton), "rows\n")
  return(output_file)
}

# Process all batches for skeleton1_create
skeleton1_files <- vector("character", n_batches)
for (i in seq_len(n_batches)) {
  skeleton1_files[i] <- skeleton1_create_batch(i, id_batches[[i]], large_files)
}

# Remove large_files from memory after skeleton1_create phase
rm(large_files)
gc()

# STEP 3: CREATE SKELETON2_CLEAN (batched)  
cat("\n=== STEP 3: CREATING SKELETON2_CLEAN (BATCHED) ===\n")

skeleton2_clean_batch <- function(batch_number) {
  cat("Cleaning batch", batch_number, "\n")
  
  # Load skeleton1 for this batch
  input_file <- file.path(OUTPUT_DIR, paste0("skeleton1_batch_", batch_number, ".rds"))
  skeleton <- readRDS(input_file)
  
  # CLEANING OPERATIONS (using only data within skeleton)
  
  # Create derived variables
  skeleton[, birth_year := as.numeric(substr(fodelseman, 1, 4))]
  skeleton[, birth_month := as.numeric(substr(fodelseman, 5, 6))]
  
  # Create age variable
  skeleton[, age := isoyear - birth_year]
  
  # Create any diagnosis flag
  skeleton[, any_mental_health := gd_any | depression | anxiety]
  
  # Create any medication flag  
  skeleton[, any_medication := antidepressants | hormones]
  
  # Filter to reasonable age range
  skeleton <- skeleton[age >= 0 & age <= 100]
  
  # Create summary variables for annual data
  if (skeleton[, any(is_isoyear == TRUE)]) {
    skeleton[is_isoyear == TRUE, n_diagnoses_year := sum(c(gd_any, depression, anxiety), na.rm = TRUE), by = .(id, isoyear)]
  }
  
  # Remove temporary variables we don't need
  skeleton[, c("fodelseman") := NULL]
  
  # Save cleaned skeleton
  output_file <- file.path(OUTPUT_DIR, paste0("skeleton2_batch_", batch_number, ".rds"))
  saveRDS(skeleton, output_file)
  
  cat("Cleaned batch", batch_number, ":", nrow(skeleton), "rows,", ncol(skeleton), "columns\n")
  
  # Clean up intermediate file
  file.remove(input_file)
  
  return(output_file)
}

# Process all batches for skeleton2_clean
skeleton2_files <- vector("character", n_batches)
for (i in seq_len(n_batches)) {
  skeleton2_files[i] <- skeleton2_clean_batch(i)
}

# STEP 4: COMBINE RESULTS (optional)
cat("\n=== STEP 4: COMBINING RESULTS ===\n")

combine_batches <- function(skeleton2_files) {
  cat("Combining", length(skeleton2_files), "cleaned batches\n")
  
  skeletons <- vector("list", length(skeleton2_files))
  for (i in seq_along(skeleton2_files)) {
    skeletons[[i]] <- readRDS(skeleton2_files[i])
    cat("Loaded batch", i, ":", nrow(skeletons[[i]]), "rows\n")
  }
  
  # Combine all batches
  final_skeleton <- rbindlist(skeletons)
  
  # Save final combined skeleton
  final_file <- file.path(OUTPUT_DIR, "final_skeleton_combined.rds")
  saveRDS(final_skeleton, final_file)
  
  cat("Final combined skeleton:", nrow(final_skeleton), "rows,", ncol(final_skeleton), "columns\n")
  cat("Saved to:", final_file, "\n")
  
  # Summary statistics
  cat("\nSummary:\n")
  cat("- Total individuals:", length(unique(final_skeleton$id)), "\n")
  cat("- Date range:", min(final_skeleton$isoyearweek), "to", max(final_skeleton$isoyearweek), "\n")
  cat("- GD cases:", sum(final_skeleton$gd_any, na.rm = TRUE), "\n")
  cat("- Depression cases:", sum(final_skeleton$depression, na.rm = TRUE), "\n")
  cat("- Antidepressant use:", sum(final_skeleton$antidepressants, na.rm = TRUE), "\n")
  
  return(final_file)
}

final_file <- combine_batches(skeleton2_files)

cat("\nMemory-efficient batched workflow completed!\n")
cat("Individual batch files available in:", OUTPUT_DIR, "\n")
cat("Combined file:", final_file, "\n")