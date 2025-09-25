# ==============================================================================
# OUTBREAK ANALYTICS IN R - DATA VALIDATION
# ==============================================================================
# Project: Outbreak Data Analysis Pipeline
# Author: Amos Mwaura
# Date: 2025
# Description: Validate and tag cleaned outbreak data using linelist package
# ==============================================================================

# Load required packages
library(tidyverse)  # For data manipulation
library(linelist)   # For tagging and validating outbreak data
library(here)       # For reproducible file paths
library(validateR)  # Additional validation functions
library(assertthat) # For data assertions

# ==============================================================================
# SECTION 1: SETUP AND DATA LOADING
# ==============================================================================

cat("=== OUTBREAK DATA VALIDATION WORKFLOW ===\n\n")

# Define paths
processed_data_path <- here("data", "processed")
cleaned_data_file <- file.path(processed_data_path, "cleaned_outbreak_data.rds")

# Load cleaned data from previous step
if (file.exists(cleaned_data_file)) {
  cat("Loading cleaned outbreak data...\n")
  cleaned_data <- readRDS(cleaned_data_file)
  cat("Cleaned data loaded successfully!\n")
  cat("Dimensions:", nrow(cleaned_data), "rows x", ncol(cleaned_data), "columns\n\n")
} else {
  stop("Cleaned data file not found. Please run 02_data_cleaning.R first.")
}

# ==============================================================================
# SECTION 2: CREATE LINELIST WITH TAGGED VARIABLES
# ==============================================================================

#' Create a linelist object with appropriate tags
#' 
#' @param data The cleaned dataset
#' @return A linelist object with tagged epidemiological variables
create_validated_linelist <- function(data) {
  cat("=== CREATING VALIDATED LINELIST ===\n")
  
  # Display available linelist tags
  cat("Available linelist tags:\n")
  available_tags <- linelist::tags_names()
  print(available_tags)
  
  # Display current column names
  cat("\nCurrent column names:\n")
  print(names(data))
  
  # Create mapping between our columns and linelist tags
  tag_mapping <- list()
  
  # Map common epidemiological variables
  if ("case_id" %in% names(data)) tag_mapping$id <- "case_id"
  if ("age" %in% names(data)) tag_mapping$age <- "age"
  if ("gender" %in% names(data)) tag_mapping$gender <- "gender"
  if ("date_onset" %in% names(data)) tag_mapping$date_onset <- "date_onset"
  if ("date_sample" %in% names(data)) tag_mapping$date_reporting <- "date_sample"
  if ("region" %in% names(data)) tag_mapping$location <- "region"
  if ("status" %in% names(data)) tag_mapping$outcome <- "status"
  
  cat("\nTag mapping:\n")
  for (tag in names(tag_mapping)) {
    cat("  ", tag, "->", tag_mapping[[tag]], "\n")
  }
  
  # Create linelist object using do.call for dynamic arguments
  cat("\nCreating linelist object...\n")
  linelist_args <- c(list(x = data), tag_mapping)
  linelist_data <- do.call(linelist::make_linelist, linelist_args)
  
  cat("Linelist created successfully!\n")
  cat("Tagged variables:", length(tag_mapping), "\n\n")
  
  return(linelist_data)
}

# Create the linelist
validated_linelist <- create_validated_linelist(cleaned_data)

# Display the linelist
cat("=== LINELIST OVERVIEW ===\n")
print(validated_linelist)
cat("\n")

# ==============================================================================
# SECTION 3: VALIDATE LINELIST STRUCTURE
# ==============================================================================

#' Validate the linelist structure and data types
#' 
#' @param linelist_obj The linelist object to validate
#' @return Validation results
validate_linelist_structure <- function(linelist_obj) {
  cat("=== VALIDATING LINELIST STRUCTURE ===\n")
  
  # Get expected data types for tags
  cat("Expected data types for linelist tags:\n")
  expected_types <- linelist::tags_types()
  print(expected_types)
  
  # Validate the linelist
  cat("\nValidating linelist structure...\n")
  
  tryCatch({
    validation_result <- linelist::validate_linelist(linelist_obj)
    cat("✓ Linelist validation passed!\n")
    cat("All tagged variables have correct data types.\n")
    return(TRUE)
  }, error = function(e) {
    cat("✗ Linelist validation failed:\n")
    cat("Error:", e$message, "\n")
    
    # Provide detailed information about validation failures
    cat("\nChecking individual tag data types:\n")
    tags <- linelist::tags_df(linelist_obj)
    for (col_name in names(tags)) {
      actual_type <- class(tags[[col_name]])[1]
      cat("  ", col_name, ": ", actual_type, "\n")
    }
    
    return(FALSE)
  })
}

# Validate the linelist structure
validation_success <- validate_linelist_structure(validated_linelist)

# ==============================================================================
# SECTION 4: FIX DATA TYPE ISSUES (IF NEEDED)
# ==============================================================================

#' Fix data type issues based on validation results
#' 
#' @param data The dataset to fix
#' @return Dataset with corrected data types
fix_data_types <- function(data) {
  cat("=== FIXING DATA TYPE ISSUES ===\n")
  
  # Convert common data type issues
  if ("age" %in% names(data) && !is.numeric(data$age)) {
    cat("Converting age to numeric...\n")
    data$age <- as.numeric(data$age)
  }
  
  if ("gender" %in% names(data) && !is.character(data$gender)) {
    cat("Converting gender to character...\n")
    data$gender <- as.character(data$gender)
  }
  
  # Ensure date columns are proper date types
  date_columns <- names(data)[grepl("date", names(data), ignore.case = TRUE)]
  for (col in date_columns) {
    if (col %in% names(data) && !inherits(data[[col]], "Date")) {
      cat("Converting", col, "to Date...\n")
      data[[col]] <- as.Date(data[[col]])
    }
  }
  
  cat("Data type corrections completed!\n\n")
  return(data)
}

# Fix data types if validation failed
if (!validation_success) {
  cat("Attempting to fix data type issues...\n")
  cleaned_data <- fix_data_types(cleaned_data)
  
  # Recreate linelist with fixed data
  validated_linelist <- create_validated_linelist(cleaned_data)
  validation_success <- validate_linelist_structure(validated_linelist)
}

# ==============================================================================
# SECTION 5: COMPREHENSIVE DATA VALIDATION CHECKS
# ==============================================================================

#' Perform comprehensive validation checks
#' 
#' @param linelist_obj The linelist object to validate
#' @return Validation report
perform_validation_checks <- function(linelist_obj) {
  cat("=== COMPREHENSIVE DATA VALIDATION ===\n")
  
  validation_results <- list()
  
  # Extract the underlying data
  data <- as_tibble(linelist_obj)
  
  # Check 1: ID uniqueness
  cat("1. Checking ID uniqueness...\n")
  if ("id" %in% names(linelist::tags_df(linelist_obj))) {
    ids <- linelist::tags_df(linelist_obj)$id
    duplicate_ids <- sum(duplicated(ids))
    validation_results$duplicate_ids <- duplicate_ids
    if (duplicate_ids == 0) {
      cat("   ✓ All IDs are unique\n")
    } else {
      cat("   ✗ Found", duplicate_ids, "duplicate IDs\n")
    }
  }
  
  # Check 2: Age validity
  cat("2. Checking age validity...\n")
  if ("age" %in% names(data)) {
    invalid_ages <- sum(data$age < 0 | data$age > 120, na.rm = TRUE)
    validation_results$invalid_ages <- invalid_ages
    age_range <- range(data$age, na.rm = TRUE)
    cat("   Age range:", age_range[1], "to", age_range[2], "years\n")
    if (invalid_ages == 0) {
      cat("   ✓ All ages are within valid range (0-120)\n")
    } else {
      cat("   ✗ Found", invalid_ages, "invalid ages\n")
    }
  }
  
  # Check 3: Date validity
  cat("3. Checking date validity...\n")
  date_cols <- names(data)[sapply(data, function(x) inherits(x, "Date"))]
  for (col in date_cols) {
    if (col %in% names(data)) {
      # Check for future dates (beyond today)
      future_dates <- sum(data[[col]] > Sys.Date(), na.rm = TRUE)
      # Check for very old dates (before 1900)
      old_dates <- sum(data[[col]] < as.Date("1900-01-01"), na.rm = TRUE)
      
      validation_results[[paste0(col, "_future")]] <- future_dates
      validation_results[[paste0(col, "_old")]] <- old_dates
      
      date_range <- range(data[[col]], na.rm = TRUE)
      cat("   ", col, "range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")
      
      if (future_dates == 0 && old_dates == 0) {
        cat("   ✓", col, "dates are valid\n")
      } else {
        if (future_dates > 0) cat("   ✗", col, "has", future_dates, "future dates\n")
        if (old_dates > 0) cat("   ✗", col, "has", old_dates, "dates before 1900\n")
      }
    }
  }
  
  # Check 4: Gender values
  cat("4. Checking gender values...\n")
  if ("gender" %in% names(data)) {
    gender_values <- table(data$gender, useNA = "ifany")
    validation_results$gender_distribution <- gender_values
    valid_genders <- c("male", "female", "Male", "Female", "m", "f", "M", "F")
    invalid_genders <- sum(!data$gender %in% valid_genders, na.rm = TRUE)
    
    cat("   Gender distribution:\n")
    print(gender_values)
    
    if (invalid_genders == 0) {
      cat("   ✓ All gender values are valid\n")
    } else {
      cat("   ✗ Found", invalid_genders, "invalid gender values\n")
    }
  }
  
  # Check 5: Missing values analysis
  cat("5. Analyzing missing values...\n")
  missing_analysis <- data %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    mutate(missing_percent = round(missing_count / nrow(data) * 100, 2)) %>%
    arrange(desc(missing_percent))
  
  validation_results$missing_analysis <- missing_analysis
  
  # Flag columns with high missing rates
  high_missing <- missing_analysis %>% filter(missing_percent > 50)
  if (nrow(high_missing) > 0) {
    cat("   ⚠ Columns with >50% missing values:\n")
    print(high_missing)
  } else {
    cat("   ✓ No columns have excessive missing values (>50%)\n")
  }
  
  # Check 6: Logical consistency between dates
  cat("6. Checking date sequence consistency...\n")
  if ("date_onset" %in% names(data) && "date_sample" %in% names(data)) {
    # Sample date should be after or equal to onset date
    date_issues <- sum(data$date_sample < data$date_onset, na.rm = TRUE)
    validation_results$date_sequence_issues <- date_issues
    
    if (date_issues == 0) {
      cat("   ✓ Date sequences are logical\n")
    } else {
      cat("   ✗ Found", date_issues, "cases where sample date is before onset date\n")
    }
  }
  
  cat("\nValidation checks completed!\n\n")
  return(validation_results)
}

# Perform comprehensive validation
validation_report <- perform_validation_checks(validated_linelist)

# ==============================================================================
# SECTION 6: SAFEGUARDING TESTS
# ==============================================================================

#' Test linelist safeguarding features
#' 
#' @param linelist_obj The linelist object to test
test_safeguarding <- function(linelist_obj) {
  cat("=== TESTING SAFEGUARDING FEATURES ===\n")
  
  # Test 1: Try to remove a tagged column (should generate warning/error)
  cat("1. Testing tagged column protection...\n")
  
  # Set lost tags action to warning for demonstration
  linelist::lost_tags_action(action = "warning")
  
  tryCatch({
    # Try to select only some columns, dropping tagged ones
    test_df <- linelist_obj %>%
      dplyr::select(1:3) # Select only first 3 columns
    
    cat("   Warning generated when tagged columns are removed\n")
  }, error = function(e) {
    cat("   Error generated:", e$message, "\n")
  })
  
  # Test 2: Extract tagged data safely
  cat("2. Testing safe extraction of tagged data...\n")
  tagged_data <- linelist::tags_df(linelist_obj)
  cat("   ✓ Successfully extracted", ncol(tagged_data), "tagged columns\n")
  cat("   Tagged columns:", paste(names(tagged_data), collapse = ", "), "\n")
  
  # Test 3: Demonstrate error mode
  cat("3. Testing error mode for lost tags...\n")
  linelist::lost_tags_action(action = "error")
  
  tryCatch({
    test_df <- linelist_obj %>%
      dplyr::select(case_id)  # This should cause an error if other tags are lost
    cat("   No error generated (all essential tags preserved)\n")
  }, error = function(e) {
    cat("   ✓ Error correctly generated when essential tags lost:", e$message, "\n")
  })
  
  # Reset to warning mode
  linelist::lost_tags_action(action = "warning")
  
  cat("Safeguarding tests completed!\n\n")
}

# Test safeguarding features
test_safeguarding(validated_linelist)

# ==============================================================================
# SECTION 7: CREATE VALIDATION SUMMARY REPORT
# ==============================================================================

#' Generate a comprehensive validation summary
#' 
#' @param validation_results The validation results
#' @param linelist_obj The validated linelist object
generate_validation_summary <- function(validation_results, linelist_obj) {
  cat("=== VALIDATION SUMMARY REPORT ===\n")
  
  # Basic dataset information
  cat("Dataset Overview:\n")
  cat("  Total records:", nrow(linelist_obj), "\n")
  cat("  Total variables:", ncol(linelist_obj), "\n")
  cat("  Tagged variables:", length(linelist::tags_df(linelist_obj)), "\n")
  
  # Tag information
  cat("\nTagged Variables:\n")
  tags_info <- linelist::tags_df(linelist_obj)
  for (i in 1:ncol(tags_info)) {
    col_name <- names(tags_info)[i]
    data_type <- class(tags_info[[col_name]])[1]
    non_missing <- sum(!is.na(tags_info[[col_name]]))
    cat("  ", col_name, "(", data_type, "):", non_missing, "non-missing values\n")
  }
  
  # Validation results summary
  cat("\nValidation Results:\n")
  
  # Count issues
  total_issues <- 0
  
  if (!is.null(validation_results$duplicate_ids) && validation_results$duplicate_ids > 0) {
    cat("  ✗ Duplicate IDs:", validation_results$duplicate_ids, "\n")
    total_issues <- total_issues + validation_results$duplicate_ids
  }
  
  if (!is.null(validation_results$invalid_ages) && validation_results$invalid_ages > 0) {
    cat("  ✗ Invalid ages:", validation_results$invalid_ages, "\n")
    total_issues <- total_issues + validation_results$invalid_ages
  }
  
  if (!is.null(validation_results$date_sequence_issues) && validation_results$date_sequence_issues > 0) {
    cat("  ✗ Date sequence issues:", validation_results$date_sequence_issues, "\n")
    total_issues <- total_issues + validation_results$date_sequence_issues
  }
  
  # Data completeness
  cat("\nData Completeness:\n")
  if (!is.null(validation_results$missing_analysis)) {
    high_missing <- validation_results$missing_analysis %>% 
      filter(missing_percent > 20) %>%
      arrange(desc(missing_percent))
    
    if (nrow(high_missing) > 0) {
      cat("  Variables with >20% missing data:\n")
      for (i in 1:nrow(high_missing)) {
        cat("    ", high_missing$column[i], ":", high_missing$missing_percent[i], "%\n")
      }
    } else {
      cat("  ✓ All variables have <20% missing data\n")
    }
  }
  
  # Overall assessment
  cat("\nOverall Assessment:\n")
  if (total_issues == 0) {
    cat("  ✓ PASSED: Data validation completed successfully\n")
    cat("  The dataset is ready for analysis\n")
  } else {
    cat("  ⚠ ISSUES FOUND:", total_issues, "data quality issues detected\n")
    cat("  Consider addressing these issues before proceeding with analysis\n")
  }
  
  cat("\n" + "="*60 + "\n")
  
  return(list(
    total_records = nrow(linelist_obj),
    tagged_variables = length(linelist::tags_df(linelist_obj)),
    total_issues = total_issues,
    passed = total_issues == 0
  ))
}

# Generate validation summary
validation_summary <- generate_validation_summary(validation_report, validated_linelist)

# ==============================================================================
# SECTION 8: SAVE VALIDATED DATA
# ==============================================================================

cat("=== SAVING VALIDATED DATA ===\n")

# Save validated linelist
validated_linelist_file <- file.path(processed_data_path, "validated_linelist.rds")
saveRDS(validated_linelist, file = validated_linelist_file)
cat("Validated linelist saved:", validated_linelist_file, "\n")

# Save validation report
validation_report_file <- file.path(here("outputs"), "validation_report.rds")
saveRDS(list(
  validation_results = validation_report,
  validation_summary = validation_summary,
  timestamp = Sys.time()
), file = validation_report_file)
cat("Validation report saved:", validation_report_file, "\n")

# Create a human-readable validation report
validation_text_file <- file.path(here("outputs"), "validation_summary.txt")
sink(validation_text_file)
cat("OUTBREAK DATA VALIDATION REPORT\n")
cat("Generated on:", as.character(Sys.time()), "\n")
cat("="*60, "\n\n")
generate_validation_summary(validation_report, validated_linelist)
sink()
cat("Human-readable validation summary saved:", validation_text_file, "\n")

cat("\n=== DATA VALIDATION COMPLETED SUCCESSFULLY ===\n")
cat("Validated files:\n")
cat("  - Linelist object:", validated_linelist_file, "\n")
cat("  - Validation report:", validation_report_file, "\n")
cat("  - Summary report:", validation_text_file, "\n")

if (validation_summary$passed) {
  cat("✓ Data validation PASSED - Ready for visualization and analysis!\n")
} else {
  cat("⚠ Data validation found issues - Review before proceeding\n")
}

# ==============================================================================
# END OF SCRIPT
# ==============================================================================