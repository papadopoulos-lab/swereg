# Script to inspect actual data file structures used in dev scripts
# This examines the real data files to understand exact column names and structures

library(data.table)
library(magrittr)

# Function to safely inspect data structure
inspect_data_structure <- function(file_path, description, sample_rows = 5) {
  cat("\n=== ", description, " ===\n")
  cat("File:", file_path, "\n")
  
  if (!file.exists(file_path)) {
    cat("FILE NOT FOUND\n")
    return(NULL)
  }
  
  tryCatch({
    # Try to read file based on extension
    if (grepl("\\.sas7bdat$", file_path)) {
      data <- haven::read_sas(file_path)
    } else if (grepl("\\.csv$", file_path)) {
      data <- fread(file_path, nrows = 100) # Just read first 100 rows for inspection
    } else if (grepl("\\.txt$", file_path)) {
      data <- fread(file_path, nrows = 100)
    } else {
      cat("Unknown file format\n")
      return(NULL)
    }
    
    setDT(data)
    
    cat("Dimensions:", nrow(data), "rows,", ncol(data), "columns\n")
    cat("Column names:\n")
    print(names(data))
    cat("\nColumn types:\n")
    print(sapply(data, class))
    
    if (nrow(data) > 0 && sample_rows > 0) {
      cat("\nFirst", min(sample_rows, nrow(data)), "rows:\n")
      print(head(data, sample_rows))
    }
    
    return(data)
    
  }, error = function(e) {
    cat("ERROR reading file:", e$message, "\n")
    return(NULL)
  })
}

# Inspect all data files mentioned in dev scripts
cat("INSPECTING DATA STRUCTURES FROM DEV SCRIPTS\n")
cat("==========================================\n")

# 1. SCB Individual registry (from test_ge.R)
inspect_data_structure(
  "/data/argos/Bronze/Embla_data/_MHT/mht_raw/scb/Individ_2007.csv",
  "SCB Individual Registry 2007"
)

# 2. SCB Demographics (from test_package.R)
inspect_data_structure(
  "/data/argos/Bronze/Postdoc_Kristen/2023-gd-register-clark-gat-descriptives/data_raw/SCB/demografi.sas7bdat",
  "SCB Demographics"
)

# 3. SCB Annual Family data (from test_package.R)
inspect_data_structure(
  "/data/argos/Bronze/Postdoc_Kristen/2023-gd-register-clark-gat-descriptives/data_raw/SCB/fp_lev_famtyp2001.sas7bdat",
  "SCB Annual Family 2001"
)

# 4. NPR Outpatient data (ov.sas7bdat)
inspect_data_structure(
  "/data/argos/Bronze/Postdoc_Kristen/2023-gd-register-clark-gat-descriptives/data_raw/Sos/ov.sas7bdat",
  "NPR Outpatient Diagnoses and Operations"
)

# 5. NPR Inpatient data (sv.sas7bdat)
inspect_data_structure(
  "/data/argos/Bronze/Postdoc_Kristen/2023-gd-register-clark-gat-descriptives/data_raw/Sos/sv.sas7bdat", 
  "NPR Inpatient Diagnoses and Operations"
)

# 6. LMED Prescription data (from test_ge.R)
inspect_data_structure(
  "/data/argos/Bronze/Embla_data/_MHT/mht_raw/sos/T_T_R_LMED__12831_2021.txt",
  "LMED Prescription Data"
)

# Alternative demographics file (from test_ge.R MHT study)
inspect_data_structure(
  "/data/argos/Bronze/Embla_data/_MHT/mht_raw/SCB/demografi.sas7bdat",
  "SCB Demographics (MHT Study)"
)

cat("\n\nINSPECTION COMPLETE\n")
cat("Use this information to create accurate fake data structures\n")