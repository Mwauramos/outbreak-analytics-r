# ==============================================================================
# OUTBREAK ANALYTICS IN R - DATA CLEANING
# ==============================================================================
# Project: Outbreak Data Analysis Pipeline
# Author: Amos Mwaura
# Date: 2025
# Description: Clean and standardize outbreak case data using cleanepi package
# ==============================================================================

# Load required packages
library(tidyverse)  # For data manipulation
library(cleanepi)   # For specialized epidemic data cleaning
library(here)       # For reproducible file paths
library(janitor)    # For additional cleaning functions
library(lubridate)  # For date handling

# ==============================================================================
# SECTION 1: SETUP AND DATA LOADING
# ==============================================================================

cat("=== OUTBREAK DATA CLEANING WORKFLOW ===\n\n")

# Define paths
processed_data_path <- here("data", "processed")
raw_data_file <- file.path(processed_data_path, "raw_outbreak_data.rds")

# Load raw data from previous step
if (file.exists(raw_data_file)) {
  cat("Loading raw outbreak data...\n")
  raw_data <- readRDS(raw_data_file)
  cat("Raw data loaded successfully!\n")
  cat("Dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n\n")
} else {
  stop("Raw data file not found. Please run 01_data_import.R first.")
}

# ==============================================================================
# SECTION 2: INITIAL DATA ASSESSMENT
# ==============================================================================

#' Perform comprehensive data assessment
#' 
#' @param data The dataset to assess
#' @return A summary of data quality issues
assess_data_quality <- function(data) {
  cat("=== INITIAL DATA ASSESSMENT ===\n")
  
  # Use cleanepi's scan_data function for quick overview
  cat("Data structure overview:\n")
  scan_results <- cleanepi::scan_data(data)
  print(scan_results)
  
  # Additional assessments
  cat("\nColumn names (checking for standardization needs):\n")
  print(names(data))
  
  cat("\nData types:\n")
  print(sapply(data, class))
  
  # Check for duplicate rows
  duplicate_count <- nrow(data) - nrow(distinct(data))
  cat("\nDuplicate rows:", duplicate_count, "\n")
  
  # Check for completely empty rows/columns
  empty_rows <- sum(apply(is.na(data) | data == "" | data == " ", 1, all))
  empty_cols <- sum(apply(is.na(data) | data == "" | data == " ", 2, all))
  cat("Empty rows:", empty_rows, "\n")
  cat("Empty columns:", empty_cols, "\n")
  
  cat("\n" + "="*50 + "\n\n")
  
  return(scan_results)
}

# Perform initial assessment
initial_assessment <- assess_data_quality(raw_data)

# ==============================================================================
# SECTION 3: COLUMN NAME STANDARDIZATION
# ==============================================================================

#' Standardize column names for consistency
#' 
#' @param data The dataset to standardize
#' @return Dataset with standardized column names
standardize_column_names <- function(data) {
  cat("=== STANDARDIZING COLUMN NAMES ===\n")
  
  cat("Original column names:\n")
  print(names(data))
  
  # Use cleanepi's standardize_column_names function
  standardized_data <- cleanepi::standardize_column_names(
    data = data,
    keep = NULL  # Don't keep any original names
  )
  
  cat("\nStandardized column names:\n")
  print(names(standardized_data))
  
  cat("Column name standardization completed!\n\n")
  
  return(standardized_data)
}

# Apply column name standardization
cleaned_data <- standardize_column_names(raw_data)

# ==============================================================================
# SECTION 4: REMOVE DATA IRREGULARITIES
# ==============================================================================

#' Remove various data irregularities
#' 
#' @param data The dataset to clean
#' @return Dataset with irregularities removed
remove_irregularities <- function(data) {
  cat("=== REMOVING DATA IRREGULARITIES ===\n")
  
  original_rows <- nrow(data)
  original_cols <- ncol(data)
  
  # Remove constant columns (where all values are the same)
  cat("Removing constant columns...\n")
  data <- cleanepi::remove_constants(data)
  
  # Remove duplicate rows
  cat("Removing duplicate rows...\n")
  data <- cleanepi::remove_duplicates(data)
  duplicate_info <- attr(data, "report")
  if (!is.null(duplicate_info$duplicated_rows)) {
    cat("Removed", length(duplicate_info$duplicated_rows), "duplicate rows\n")
  }
  
  # Remove completely empty rows and columns using janitor
  cat("Removing empty rows and columns...\n")
  data <- data %>%
    janitor::remove_empty(which = c("rows", "cols"))
  
  final_rows <- nrow(data)
  final_cols <- ncol(data)
  
  cat("Irregularity removal completed!\n")
  cat("Rows: ", original_rows, "->", final_rows, 
      "(", original_rows - final_rows, "removed)\n")
  cat("Columns: ", original_cols, "->", final_cols, 
      "(", original_cols - final_cols, "removed)\n\n")
  
  return(data)
}

# Remove irregularities
cleaned_data <- remove_irregularities(cleaned_data)

# ==============================================================================
# SECTION 5: HANDLE MISSING VALUES
# ==============================================================================

#' Replace various representations of missing values with NA
#' 
#' @param data The dataset to process
#' @return Dataset with standardized missing values
standardize_missing_values <- function(data) {
  cat("=== STANDARDIZING MISSING VALUES ===\n")
  
  # Count missing values before
  missing_before <- data %>%
    summarise(across(everything(), ~sum(is.na(.) | . == "" | . == " "))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    summarise(total_missing = sum(missing_count)) %>%
    pull(total_missing)
  
  # Use cleanepi to replace missing values
  data <- cleanepi::replace_missing_values(
    data = data,
    na_strings = c("", " ", "unknown", "Unknown", "UNKNOWN", "N/A", "n/a", "NULL", "null")
  )
  
  # Count missing values after
  missing_after <- data %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    summarise(total_missing = sum(missing_count)) %>%
    pull(total_missing)
  
  cat("Missing value standardization completed!\n")
  cat("Total missing values: ", missing_before, "->", missing_after, "\n\n")
  
  return(data)
}

# Standardize missing values
cleaned_data <- standardize_missing_values(cleaned_data)

# ==============================================================================
# SECTION 6: VALIDATE AND CLEAN SUBJECT IDs
# ==============================================================================

#' Validate subject IDs
#' 
#' @param data The dataset to validate
#' @param id_column Name of the ID column
#' @param id_range Valid range for IDs
#' @return Dataset with validated IDs
validate_subject_ids <- function(data, id_column = "case_id", id_range = c(1, 10000)) {
  cat("=== VALIDATING SUBJECT IDS ===\n")
  
  if (id_column %in% names(data)) {
    # Check subject IDs using cleanepi
    data <- cleanepi::check_subject_ids(
      data = data,
      target_columns = id_column,
      range = id_range
    )
    
    # Get validation report
    id_report <- attr(data, "report")
    if (!is.null(id_report$duplicated_rows)) {
      cat("Found", length(id_report$duplicated_rows), "duplicate subject IDs\n")
    }
    
    cat("Subject ID validation completed!\n\n")
  } else {
    cat("ID column '", id_column, "' not found. Skipping ID validation.\n\n")
  }
  
  return(data)
}

# Validate subject IDs
cleaned_data <- validate_subject_ids(cleaned_data)

# ==============================================================================
# SECTION 7: STANDARDIZE DATES
# ==============================================================================

#' Standardize date columns to consistent format
#' 
#' @param data The dataset to process
#' @param date_columns Vector of date column names
#' @return Dataset with standardized dates
standardize_date_columns <- function(data, date_columns = NULL) {
  cat("=== STANDARDIZING DATES ===\n")
  
  # If no date columns specified, auto-detect
  if (is.null(date_columns)) {
    # Look for columns with "date" in the name
    date_columns <- names(data)[grepl("date", names(data), ignore.case = TRUE)]
  }
  
  if (length(date_columns) > 0) {
    cat("Date columns found:", paste(date_columns, collapse = ", "), "\n")
    
    # Use cleanepi to standardize dates
    data <- cleanepi::standardize_dates(
      data = data,
      target_columns = date_columns
    )
    
    cat("Date standardization completed!\n")
    
    # Show date ranges for each date column
    for (col in date_columns) {
      if (col %in% names(data) && any(!is.na(data[[col]]))) {
        date_range <- range(data[[col]], na.rm = TRUE)
        cat(col, "range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")
      }
    }
  } else {
    cat("No date columns found for standardization.\n")
  }
  
  cat("\n")
  return(data)
}

# Standardize dates
cleaned_data <- standardize_date_columns(cleaned_data)

# ==============================================================================
# SECTION 8: CONVERT NUMERIC VALUES
# ==============================================================================

#' Convert character numbers to numeric values
#' 
#' @param data The dataset to process
#' @param numeric_columns Vector of columns to convert
#' @return Dataset with converted numeric columns
convert_numeric_columns <- function(data, numeric_columns = NULL) {
  cat("=== CONVERTING NUMERIC VALUES ===\n")
  
  # If no numeric columns specified, auto-detect
  if (is.null(numeric_columns)) {
    # Look for age column or other likely numeric columns
    numeric_columns <- names(data)[grepl("age|count|number|value", names(data), ignore.case = TRUE)]
  }
  
  if (length(numeric_columns) > 0) {
    cat("Numeric columns found:", paste(numeric_columns, collapse = ", "), "\n")
    
    for (col in numeric_columns) {
      if (col %in% names(data)) {
        # Use cleanepi to convert to numeric (handles text numbers)
        data <- cleanepi::convert_to_numeric(
          data = data,
          target_columns = col
        )
        
        # Show conversion summary
        numeric_count <- sum(!is.na(data[[col]]))
        cat(col, ": ", numeric_count, "numeric values after conversion\n")
      }
    }
  } else {
    cat("No numeric columns found for conversion.\n")
  }
  
  cat("\n")
  return(data)
}

# Convert numeric columns
cleaned_data <- convert_numeric_columns(cleaned_data)

# ==============================================================================
# SECTION 9: STANDARDIZE CATEGORICAL VARIABLES
# ==============================================================================

#' Create and apply data dictionary for categorical variables
#' 
#' @param data The dataset to process
#' @return Dataset with standardized categorical variables
standardize_categorical_variables <- function(data) {
  cat("=== STANDARDIZING CATEGORICAL VARIABLES ===\n")
  
  # Create a comprehensive data dictionary
  data_dictionary <- tibble(
    options = c(
      # Gender variations
      "1", "2", "M", "m", "F", "f", "male", "Male", "MALE", 
      "female", "Female", "FEMALE",
      # Status variations  
      "confirmed", "Confirmed", "CONFIRMED", "conf", "positive",
      "probable", "Probable", "PROBABLE", "prob",
      "suspected", "Suspected", "SUSPECTED", "susp", "possible"
    ),
    values = c(
      # Gender standardization
      rep("male", 6), rep("female", 6),
      # Status standardization
      rep("confirmed", 5), rep("probable", 3), rep("suspected", 5)
    ),
    grp = c(
      rep("gender", 12), rep("status", 13)
    ),
    orders = c(1:12, 1:13)
  )
  
  cat("Applying data dictionary for standardization...\n")
  
  # Apply dictionary using cleanepi
  data <- cleanepi::clean_using_dictionary(
    data = data,
    dictionary = data_dictionary
  )
  
  # Show unique values for categorical columns after cleaning
  categorical_cols <- c("gender", "status", "region")
  for (col in categorical_cols) {
    if (col %in% names(data)) {
      unique_vals <- unique(data[[col]])
      unique_vals <- unique_vals[!is.na(unique_vals)]
      cat(col, "unique values:", paste(unique_vals, collapse = ", "), "\n")
    }
  }
  
  cat("Categorical variable standardization completed!\n\n")
  
  return(data)
}

# Standardize categorical variables
cleaned_data <- standardize_categorical_variables(cleaned_data)

# ==============================================================================
# SECTION 10: CHECK DATE SEQUENCES
# ==============================================================================

#' Validate chronological order of date events
#' 
#' @param data The dataset to check
#' @return Dataset with date sequence validation
check_date_sequences <- function(data) {
  cat("=== CHECKING DATE SEQUENCES ===\n")
  
  # Find date columns
  date_cols <- names(data)[sapply(data, function(x) "Date" %in% class(x) || "IDate" %in% class(x))]
  
  if (length(date_cols) >= 2) {
    cat("Checking date sequence for:", paste(date_cols, collapse = " -> "), "\n")
    
    # Check date sequence using cleanepi
    data <- cleanepi::check_date_sequence(
      data = data,
      target_columns = date_cols
    )
    
    # Get sequence check report
    sequence_report <- attr(data, "report")
    if (!is.null(sequence_report$incorrect_date_sequence)) {
      incorrect_count <- length(sequence_report$incorrect_date_sequence)
      cat("Found", incorrect_count, "records with incorrect date sequences\n")
    }
    
  } else {
    cat("Insufficient date columns for sequence checking.\n")
  }
  
  cat("Date sequence checking completed!\n\n")
  
  return(data)
}

# Check date sequences
cleaned_data <- check_date_sequences(cleaned_data)

# ==============================================================================
# SECTION 11: CALCULATE TIME SPANS
# ==============================================================================

#' Calculate time spans between events
#' 
#' @param data The dataset to process
#' @return Dataset with calculated time spans
calculate_time_spans <- function(data) {
  cat("=== CALCULATING TIME SPANS ===\n")
  
  # Check if we have necessary date columns
  has_onset <- "date_onset" %in% names(data)
  has_sample <- "date_sample" %in% names(data)
  
  if (has_sample) {
    # Calculate time since sample collection
    data <- cleanepi::timespan(
      data = data,
      target_column = "date_sample",
      end_date = Sys.Date(),
      span_unit = "months",
      span_column_name = "months_since_sample",
      span_remainder_unit = "days"
    )
    cat("Added months_since_sample column\n")
  }
  
  if (has_onset && has_sample) {
    # Calculate time from onset to sample
    data <- data %>%
      mutate(
        days_onset_to_sample = as.numeric(date_sample - date_onset)
      )
    cat("Added days_onset_to_sample column\n")
  }
  
  cat("Time span calculations completed!\n\n")
  
  return(data)
}

# Calculate time spans
cleaned_data <- calculate_time_spans(cleaned_data)

# ==============================================================================
# SECTION 12: FINAL DATA QUALITY ASSESSMENT
# ==============================================================================

#' Final assessment of cleaned data
#' 
#' @param data The cleaned dataset
final_data_assessment <- function(data) {
  cat("=== FINAL DATA QUALITY ASSESSMENT ===\n")
  
  cat("Final dataset dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
  
  # Missing values summary
  missing_summary <- data %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    mutate(missing_percent = round(missing_count / nrow(data) * 100, 2)) %>%
    arrange(desc(missing_count))
  
  cat("\nMissing values summary:\n")
  print(missing_summary)
  
  # Data types summary
  cat("\nFinal data types:\n")
  print(sapply(data, class))
  
  # Basic statistics for numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) > 0) {
    cat("\nNumeric columns summary:\n")
    print(data %>% select(all_of(numeric_cols)) %>% summary())
  }
  
  # Categorical variables summary
  categorical_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  if (length(categorical_cols) > 0) {
    cat("\nCategorical variables unique values:\n")
    for (col in categorical_cols) {
      unique_count <- length(unique(data[[col]][!is.na(data[[col]])]))
      cat(col, ":", unique_count, "unique values\n")
    }
  }
  
  cat("\n" + "="*50 + "\n")
}

# Final assessment
final_data_assessment(cleaned_data)

# ==============================================================================
# SECTION 13: SAVE CLEANED DATA
# ==============================================================================

cat("=== SAVING CLEANED DATA ===\n")

# Save cleaned data in multiple formats
cleaned_data_rds <- file.path(processed_data_path, "cleaned_outbreak_data.rds")
cleaned_data_csv <- file.path(processed_data_path, "cleaned_outbreak_data.csv")

# Save as RDS (preserves data types)
saveRDS(cleaned_data, file = cleaned_data_rds)
cat("Cleaned data saved as RDS:", cleaned_data_rds, "\n")

# Save as CSV (for broader compatibility)
rio::export(cleaned_data, file = cleaned_data_csv)
cat("Cleaned data saved as CSV:", cleaned_data_csv, "\n")

# Create a cleaning report
cat("\n=== GENERATING CLEANING REPORT ===\n")

# Generate comprehensive cleaning report using cleanepi
cleaning_report_path <- file.path(here("outputs"), "data_cleaning_report.html")

# Print report (this will open in browser)
tryCatch({
  cleanepi::print_report(cleaned_data)
  cat("Interactive cleaning report generated and opened in browser\n")
}, error = function(e) {
  cat("Could not generate interactive report:", e$message, "\n")
})

cat("\n=== DATA CLEANING COMPLETED SUCCESSFULLY ===\n")
cat("Cleaned data files:\n")
cat("  - RDS format:", cleaned_data_rds, "\n")
cat("  - CSV format:", cleaned_data_csv, "\n")
cat("Ready for data validation step!\n")

# Clean up environment (optional)
# rm(list = setdiff(ls(), "cleaned_data"))

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
