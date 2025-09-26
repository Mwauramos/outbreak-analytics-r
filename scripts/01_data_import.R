# ==============================================================================
# OUTBREAK ANALYTICS IN R - DATA IMPORT (CORRECTED VERSION)
# ==============================================================================
# Project: Outbreak Data Analysis Pipeline
# Author: Amos Mwaura
# Date: 2025
# Description: Import outbreak data from various sources including files, 
#              databases, and health information systems
# ==============================================================================

# Load required packages
library(tidyverse)  # For data manipulation and visualization
library(rio)        # For importing data from various formats
library(here)       # For reproducible file paths

# Load optional packages with error handling
load_optional_package <- function(pkg_name) {
  if (requireNamespace(pkg_name, quietly = TRUE)) {
    library(pkg_name, character.only = TRUE)
    return(TRUE)
  } else {
    cat("Package", pkg_name, "not available. Some functions may be limited.\n")
    return(FALSE)
  }
}

readepi_available <- load_optional_package("readepi")
dbplyr_available <- load_optional_package("dbplyr") 
dbi_available <- load_optional_package("DBI")

# ==============================================================================
# SECTION 1: SETUP AND CONFIGURATION
# ==============================================================================

# Set up project structure
cat("Setting up project structure...\n")

# Create directories if they don't exist
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/raw")) dir.create("data/raw")
if (!dir.exists("data/processed")) dir.create("data/processed")
if (!dir.exists("outputs")) dir.create("outputs")
if (!dir.exists("outputs/plots")) dir.create("outputs/plots")

# Define file paths
raw_data_path <- here("data", "raw")
processed_data_path <- here("data", "processed")

cat("Project directories created successfully!\n")

# ==============================================================================
# SECTION 2: IMPORT FROM LOCAL FILES
# ==============================================================================

#' Import outbreak data from CSV files
#' 
#' This function imports CSV data and performs basic checks
#' @param file_name Name of the CSV file to import
#' @param data_path Path to the data directory
#' @return A tibble containing the imported data
import_csv_data <- function(file_name, data_path = raw_data_path) {
  file_path <- file.path(data_path, file_name)
  
  if (!file.exists(file_path)) {
    cat("Warning: File", file_name, "not found at", file_path, "\n")
    return(NULL)
  }
  
  cat("Importing", file_name, "...\n")
  
  # Import data using rio for robust file handling
  data <- rio::import(file_path) %>%
    dplyr::as_tibble()
  
  # Basic data summary
  cat("Data imported successfully!\n")
  cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
  cat("Column names:", paste(names(data), collapse = ", "), "\n\n")
  
  return(data)
}

# ==============================================================================
# SECTION 3: IMPORT FROM COMPRESSED FILES
# ==============================================================================

#' Import data from compressed files (ZIP, etc.)
#' 
#' @param zip_file_name Name of the compressed file
#' @param data_path Path to the data directory
#' @return A tibble containing the imported data
import_compressed_data <- function(zip_file_name, data_path = raw_data_path) {
  file_path <- file.path(data_path, zip_file_name)
  
  if (!file.exists(file_path)) {
    cat("Warning: Compressed file", zip_file_name, "not found\n")
    return(NULL)
  }
  
  cat("Importing compressed file:", zip_file_name, "...\n")
  
  # Import compressed data using rio
  data <- rio::import(file_path) %>%
    dplyr::as_tibble()
  
  cat("Compressed data imported successfully!\n")
  cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
  
  return(data)
}

# ==============================================================================
# SECTION 4: SIMULATE OUTBREAK DATA FOR DEMONSTRATION (CORRECTED)
# ==============================================================================

# If actual data files are not available, create simulated data
create_simulated_outbreak_data <- function() {
  cat("Creating simulated outbreak data for demonstration...\n")
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Create realistic outbreak data
  n_cases <- 500
  
  outbreak_data <- tibble(
    case_id = 1:n_cases,
    age = sample(0:90, n_cases, replace = TRUE,
                 prob = c(rep(0.3, 18), rep(1.5, 62), rep(0.8, 11))), # Age distribution
    gender = sample(c("male", "female", "m", "f", "M", "F", "1", "2"), 
                   n_cases, replace = TRUE,
                   prob = c(0.2, 0.2, 0.15, 0.15, 0.1, 0.1, 0.05, 0.05)),
    status = sample(c("confirmed", "probable", "suspected", "", NA), 
                   n_cases, replace = TRUE,
                   prob = c(0.4, 0.3, 0.2, 0.05, 0.05)),
    date_onset = sample(seq.Date(from = as.Date("2023-01-01"), 
                                to = as.Date("2023-06-30"), 
                                by = "day"), 
                       n_cases, replace = TRUE),
    date_sample = sample(seq.Date(from = as.Date("2023-01-01"), 
                                 to = as.Date("2023-07-15"), 
                                 by = "day"), 
                        n_cases, replace = TRUE),
    region = sample(c("North", "South", "East", "West", "Central"), 
                   n_cases, replace = TRUE),
    constant_col = "constant_value" # Constant column to be removed
  )
  
  # Add some problematic entries for cleaning demonstration
  outbreak_data$age[sample(1:n_cases, 20)] <- c("twenty", "thirty-five", "forty", 
                                               "fifty-two", rep("unknown", 16))
  outbreak_data$gender[sample(1:n_cases, 30)] <- NA
  outbreak_data$status[sample(1:n_cases, 25)] <- ""
  
  # Create some duplicate rows by repeating random rows
  duplicate_indices <- sample(1:n_cases, 50) # 50 rows to duplicate
  duplicated_rows <- outbreak_data[duplicate_indices, ]
  
  # Bind original data with duplicated rows
  outbreak_data <- bind_rows(outbreak_data, duplicated_rows)
  
  cat("Simulated outbreak data created successfully!\n")
  cat("Data includes common data quality issues for cleaning demonstration\n")
  cat("Total records (including duplicates):", nrow(outbreak_data), "\n")
  
  return(outbreak_data)
}

# ==============================================================================
# SECTION 5: DATABASE CONNECTION FUNCTIONS
# ==============================================================================

#' Connect to a database (example function)
#' 
#' @param db_type Type of database ("mysql", "postgresql", "sqlite")
#' @param host Database host
#' @param db_name Database name
#' @param username Username
#' @param password Password
#' @param port Port number
#' @return Database connection object
connect_to_database <- function(db_type = "mysql", 
                               host = NULL, 
                               db_name = NULL,
                               username = NULL, 
                               password = NULL, 
                               port = NULL) {
  
  cat("Attempting to connect to", db_type, "database...\n")
  
  if (!readepi_available) {
    cat("readepi package not available. Database connection not possible.\n")
    cat("To enable database connections, ensure readepi is properly installed.\n")
    return(NULL)
  }
  
  tryCatch({
    # Example connection using readepi
    if (!is.null(host) && !is.null(db_name)) {
      connection <- readepi::login(
        from = host,
        type = toupper(db_type),
        user_name = username,
        password = password,
        db_name = db_name,
        port = port
      )
      cat("Database connection established successfully!\n")
      return(connection)
    } else {
      cat("Database connection parameters missing. Using simulated data instead.\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Database connection failed:", e$message, "\n")
    cat("Proceeding with simulated data...\n")
    return(NULL)
  })
}

# ==============================================================================
# SECTION 6: MAIN DATA IMPORT WORKFLOW
# ==============================================================================

#' Main function to import outbreak data
#' 
#' This function attempts to import real data, falls back to simulation
import_outbreak_data <- function() {
  cat("=== OUTBREAK DATA IMPORT WORKFLOW ===\n\n")
  
  # Try to import real data first
  outbreak_data <- NULL
  
  # Check for common outbreak data files
  potential_files <- c("simulated_ebola_2.csv", "outbreak_data.csv", 
                      "case_data.csv", "linelist.csv")
  
  for (file in potential_files) {
    outbreak_data <- import_csv_data(file)
    if (!is.null(outbreak_data)) {
      cat("Successfully imported:", file, "\n")
      break
    }
  }
  
  # If no real data found, create simulated data
  if (is.null(outbreak_data)) {
    cat("No real outbreak data found. Creating simulated data...\n")
    outbreak_data <- create_simulated_outbreak_data()
    
    # Save simulated data for use in other scripts
    rio::export(outbreak_data, file.path(raw_data_path, "simulated_outbreak_data.csv"))
    cat("Simulated data saved to:", file.path(raw_data_path, "simulated_outbreak_data.csv"), "\n")
  }
  
  # Basic data exploration
  cat("\n=== DATA OVERVIEW ===\n")
  cat("Dataset dimensions:", nrow(outbreak_data), "rows x", ncol(outbreak_data), "columns\n")
  cat("Column names:\n")
  print(names(outbreak_data))
  
  cat("\nData types:\n")
  print(sapply(outbreak_data, class))
  
  cat("\nFirst few rows:\n")
  print(head(outbreak_data))
  
  cat("\nMissing values per column:\n")
  missing_summary <- outbreak_data %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    mutate(missing_percent = round(missing_count / nrow(outbreak_data) * 100, 2)) %>%
    arrange(desc(missing_count))
  
  print(missing_summary)
  
  return(outbreak_data)
}

# ==============================================================================
# SECTION 7: EXECUTE IMPORT
# ==============================================================================

# Run the main import workflow
cat("Starting outbreak data import process...\n\n")

# Execute the import function with error handling
tryCatch({
  raw_outbreak_data <- import_outbreak_data()
  
  # Save the raw data for use in subsequent scripts
  cat("\nSaving raw data for downstream processing...\n")
  saveRDS(raw_outbreak_data, file = file.path(processed_data_path, "raw_outbreak_data.rds"))
  
  cat("\n=== DATA IMPORT COMPLETED SUCCESSFULLY ===\n")
  cat("Raw data saved to:", file.path(processed_data_path, "raw_outbreak_data.rds"), "\n")
  cat("Ready for data cleaning step!\n")
  
  # Show summary of what was accomplished
  cat("\nSummary:\n")
  cat("- Created project structure\n")
  cat("- Generated", nrow(raw_outbreak_data), "outbreak records\n")
  cat("- Saved data in CSV and RDS formats\n")
  cat("- Ready to run: 02_data_cleaning.R\n")
  
}, error = function(e) {
  cat("ERROR in data import:", e$message, "\n")
  cat("Please check your R environment and try again.\n")
})

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
