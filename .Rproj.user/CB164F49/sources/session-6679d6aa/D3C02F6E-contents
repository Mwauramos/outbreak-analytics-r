# ==============================================================================
# COMPLETE OUTBREAK ANALYTICS PIPELINE IN R
# ==============================================================================
# Project: Comprehensive Outbreak Data Analysis Pipeline
# Author: Amos Mwaura
# Date: 2025
# Description: Complete pipeline from data import through visualization
# GitHub: https://github.com/Mwauramos/outbreak-analytics-r
# ==============================================================================

cat("Starting Complete Outbreak Analytics Pipeline...\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# ==============================================================================
# SECTION 1: PACKAGE LOADING AND SETUP
# ==============================================================================

# Function to safely load packages
load_package_safely <- function(pkg_name, required = TRUE) {
  if (requireNamespace(pkg_name, quietly = TRUE)) {
    library(pkg_name, character.only = TRUE)
    return(TRUE)
  } else {
    if (required) {
      stop("Required package ", pkg_name, " not found. Please install it first.")
    } else {
      cat("Optional package", pkg_name, "not available. Some functions may be limited.\n")
      return(FALSE)
    }
  }
}

# Load required packages
cat("Loading required packages...\n")
load_package_safely("tidyverse")
load_package_safely("rio")
load_package_safely("here")

# Load optional packages with graceful handling
janitor_available <- load_package_safely("janitor", FALSE)
cleanepi_available <- load_package_safely("cleanepi", FALSE)
linelist_available <- load_package_safely("linelist", FALSE)
incidence2_available <- load_package_safely("incidence2", FALSE)
scales_available <- load_package_safely("scales", FALSE)
patchwork_available <- load_package_safely("patchwork", FALSE)
plotly_available <- load_package_safely("plotly", FALSE)
DT_available <- load_package_safely("DT", FALSE)

# Set here path
here_path <- here::here()
cat("Working directory:", here_path, "\n")

# ==============================================================================
# SECTION 2: PROJECT SETUP
# ==============================================================================

setup_project_structure <- function() {
  cat("Setting up project structure...\n")
  
  # Create directories
  dirs_to_create <- c(
    "data", "data/raw", "data/processed", 
    "outputs", "outputs/plots", "outputs/reports"
  )
  
  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  }
  
  cat("Project directories created successfully!\n")
  
  return(list(
    raw_data_path = here("data", "raw"),
    processed_data_path = here("data", "processed"),
    outputs_path = here("outputs"),
    plots_path = here("outputs", "plots")
  ))
}

# Setup project
paths <- setup_project_structure()

# ==============================================================================
# SECTION 3: DATA SIMULATION (CORRECTED VERSION)
# ==============================================================================

create_realistic_outbreak_data <- function(n_cases = 500) {
  cat("Creating realistic outbreak data...\n")
  
  set.seed(123)
  
  # Create base data
  outbreak_data <- tibble(
    case_id = 1:n_cases,
    age = sample(0:90, n_cases, replace = TRUE),
    gender = sample(c("male", "female", "m", "f", "M", "F", "1", "2"), 
                    n_cases, replace = TRUE),
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
    constant_col = "constant_value"
  )
  
  # Add data quality issues
  age_indices <- sample(1:n_cases, 20)
  outbreak_data$age[age_indices] <- c("twenty", "thirty-five", "forty", "fifty-two", 
                                      rep("unknown", 16))[1:length(age_indices)]
  
  gender_indices <- sample(1:n_cases, 30)
  outbreak_data$gender[gender_indices] <- NA
  
  status_indices <- sample(1:n_cases, 25)
  outbreak_data$status[status_indices] <- ""
  
  # Create duplicates properly
  duplicate_indices <- sample(1:n_cases, 50)
  duplicated_rows <- outbreak_data[duplicate_indices, ]
  outbreak_data <- bind_rows(outbreak_data, duplicated_rows)
  
  cat("Created", nrow(outbreak_data), "records with intentional data quality issues\n")
  return(outbreak_data)
}

# ==============================================================================
# SECTION 4: DATA IMPORT
# ==============================================================================

import_or_simulate_data <- function() {
  cat(paste(rep("=", 50), collapse=""), "\n")
  cat("DATA IMPORT PHASE\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  # Try to find existing data files
  potential_files <- c("outbreak_data.csv", "case_data.csv", "linelist.csv")
  outbreak_data <- NULL
  
  for (file in potential_files) {
    file_path <- file.path(paths$raw_data_path, file)
    if (file.exists(file_path)) {
      cat("Found data file:", file, "\n")
      outbreak_data <- rio::import(file_path) %>% as_tibble()
      break
    }
  }
  
  # If no data found, create simulated data
  if (is.null(outbreak_data)) {
    cat("No existing data found. Creating simulated data...\n")
    outbreak_data <- create_realistic_outbreak_data()
    
    # Save for future use
    rio::export(outbreak_data, file.path(paths$raw_data_path, "simulated_outbreak_data.csv"))
  }
  
  # Data overview
  cat("\nData Overview:\n")
  cat("Dimensions:", nrow(outbreak_data), "rows x", ncol(outbreak_data), "columns\n")
  cat("Columns:", paste(names(outbreak_data), collapse = ", "), "\n")
  
  return(outbreak_data)
}

# ==============================================================================
# SECTION 5: DATA CLEANING (ERROR-CORRECTED)
# ==============================================================================

clean_outbreak_data <- function(raw_data) {
  cat(paste(rep("=", 50), collapse=""), "\n")
  cat("DATA CLEANING PHASE\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  cleaned_data <- raw_data
  
  # 1. Standardize column names
  cat("1. Standardizing column names...\n")
  if (cleanepi_available) {
    cleaned_data <- cleanepi::standardize_column_names(cleaned_data)
  } else {
    # Fallback: use janitor or manual cleaning
    if (janitor_available) {
      cleaned_data <- janitor::clean_names(cleaned_data)
    }
  }
  
  # 2. Remove irregularities
  cat("2. Removing irregularities...\n")
  original_rows <- nrow(cleaned_data)
  
  # Remove duplicates
  cleaned_data <- distinct(cleaned_data)
  duplicates_removed <- original_rows - nrow(cleaned_data)
  cat("   Removed", duplicates_removed, "duplicate rows\n")
  
  # Remove constant columns
  constant_cols <- names(cleaned_data)[sapply(cleaned_data, function(x) length(unique(x[!is.na(x)])) <= 1)]
  if (length(constant_cols) > 0) {
    cleaned_data <- cleaned_data %>% select(-all_of(constant_cols))
    cat("   Removed", length(constant_cols), "constant columns:", paste(constant_cols, collapse = ", "), "\n")
  }
  
  # 3. Handle missing values
  cat("3. Standardizing missing values...\n")
  if (cleanepi_available) {
    cleaned_data <- cleanepi::replace_missing_values(
      cleaned_data, 
      na_strings = c("", " ", "unknown", "Unknown", "UNKNOWN")
    )
  } else {
    # Fallback manual approach
    cleaned_data <- cleaned_data %>%
      mutate(across(everything(), ~ifelse(. %in% c("", " ", "unknown", "Unknown", "UNKNOWN"), NA, .)))
  }
  
  # 4. Convert age to numeric
  cat("4. Converting age to numeric...\n")
  if ("age" %in% names(cleaned_data)) {
    if (cleanepi_available) {
      cleaned_data <- cleanepi::convert_to_numeric(cleaned_data, target_columns = "age")
    } else {
      # Manual conversion with text number handling
      text_to_num <- c("twenty" = 20, "thirty-five" = 35, "forty" = 40, "fifty-two" = 52)
      cleaned_data <- cleaned_data %>%
        mutate(
          age = case_when(
            age %in% names(text_to_num) ~ text_to_num[age],
            !is.na(suppressWarnings(as.numeric(age))) ~ as.numeric(age),
            TRUE ~ NA_real_
          )
        )
    }
    cat("   Converted age column to numeric\n")
  }
  
  # 5. Standardize dates
  cat("5. Standardizing dates...\n")
  date_cols <- names(cleaned_data)[grepl("date", names(cleaned_data), ignore.case = TRUE)]
  for (col in date_cols) {
    if (col %in% names(cleaned_data)) {
      tryCatch({
        cleaned_data[[col]] <- as.Date(cleaned_data[[col]])
        cat("   Converted", col, "to Date format\n")
      }, error = function(e) {
        cat("   Warning: Could not convert", col, "to Date format\n")
      })
    }
  }
  
  # 6. Clean categorical variables
  cat("6. Cleaning categorical variables...\n")
  if ("gender" %in% names(cleaned_data)) {
    # Create corrected data dictionary
    gender_mapping <- c("1" = "male", "2" = "female", "M" = "male", "m" = "male", 
                        "F" = "female", "f" = "female", "male" = "male", "female" = "female")
    
    cleaned_data <- cleaned_data %>%
      mutate(gender = ifelse(gender %in% names(gender_mapping), gender_mapping[gender], gender))
  }
  
  # 7. Add derived columns
  cat("7. Adding derived columns...\n")
  if ("date_sample" %in% names(cleaned_data)) {
    cleaned_data <- cleaned_data %>%
      mutate(
        months_since_sample = as.numeric(difftime(Sys.Date(), date_sample, units = "days")) %/% 30,
        remainder_days = as.numeric(difftime(Sys.Date(), date_sample, units = "days")) %% 30
      )
  }
  
  if ("date_onset" %in% names(cleaned_data) && "date_sample" %in% names(cleaned_data)) {
    cleaned_data <- cleaned_data %>%
      mutate(days_onset_to_sample = as.numeric(date_sample - date_onset))
  }
  
  cat("Data cleaning completed!\n")
  cat("Final dimensions:", nrow(cleaned_data), "rows x", ncol(cleaned_data), "columns\n")
  
  return(cleaned_data)
}

# ==============================================================================
# SECTION 6: DATA VALIDATION
# ==============================================================================

validate_outbreak_data <- function(cleaned_data) {
  cat(paste(rep("=", 50), collapse=""), "\n")
  cat("DATA VALIDATION PHASE\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  validation_results <- list()
  
  # Basic validation checks
  cat("1. Checking data dimensions...\n")
  validation_results$total_records <- nrow(cleaned_data)
  validation_results$total_columns <- ncol(cleaned_data)
  cat("   Records:", validation_results$total_records, "\n")
  cat("   Columns:", validation_results$total_columns, "\n")
  
  # ID uniqueness check
  if ("case_id" %in% names(cleaned_data)) {
    cat("2. Checking ID uniqueness...\n")
    duplicate_ids <- sum(duplicated(cleaned_data$case_id))
    validation_results$duplicate_ids <- duplicate_ids
    if (duplicate_ids == 0) {
      cat("   All IDs are unique\n")
    } else {
      cat("   Found", duplicate_ids, "duplicate IDs\n")
    }
  }
  
  # Age validation
  if ("age" %in% names(cleaned_data)) {
    cat("3. Checking age validity...\n")
    invalid_ages <- sum(cleaned_data$age < 0 | cleaned_data$age > 120, na.rm = TRUE)
    validation_results$invalid_ages <- invalid_ages
    age_range <- range(cleaned_data$age, na.rm = TRUE)
    cat("   Age range:", age_range[1], "to", age_range[2], "years\n")
    if (invalid_ages == 0) {
      cat("   All ages are valid\n")
    } else {
      cat("   Found", invalid_ages, "invalid ages\n")
    }
  }
  
  # Date validation
  cat("4. Checking date validity...\n")
  date_cols <- names(cleaned_data)[sapply(cleaned_data, function(x) inherits(x, "Date"))]
  for (col in date_cols) {
    if (col %in% names(cleaned_data)) {
      future_dates <- sum(cleaned_data[[col]] > Sys.Date(), na.rm = TRUE)
      old_dates <- sum(cleaned_data[[col]] < as.Date("1900-01-01"), na.rm = TRUE)
      
      date_range <- range(cleaned_data[[col]], na.rm = TRUE)
      cat("   ", col, "range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")
      
      if (future_dates == 0 && old_dates == 0) {
        cat("   ", col, "dates are valid\n")
      } else {
        if (future_dates > 0) cat("   Warning:", col, "has", future_dates, "future dates\n")
        if (old_dates > 0) cat("   Warning:", col, "has", old_dates, "very old dates\n")
      }
    }
  }
  
  # Missing values analysis
  cat("5. Analyzing missing values...\n")
  missing_analysis <- cleaned_data %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    mutate(missing_percent = round(missing_count / nrow(cleaned_data) * 100, 2)) %>%
    arrange(desc(missing_percent))
  
  validation_results$missing_analysis <- missing_analysis
  
  high_missing <- missing_analysis %>% filter(missing_percent > 50)
  if (nrow(high_missing) > 0) {
    cat("   Columns with >50% missing values:\n")
    print(high_missing)
  } else {
    cat("   No columns have excessive missing values\n")
  }
  
  # Create linelist if possible
  validated_linelist <- NULL
  if (linelist_available) {
    cat("6. Creating validated linelist...\n")
    tryCatch({
      tag_mapping <- list()
      if ("case_id" %in% names(cleaned_data)) tag_mapping$id <- "case_id"
      if ("age" %in% names(cleaned_data)) tag_mapping$age <- "age"
      if ("gender" %in% names(cleaned_data)) tag_mapping$gender <- "gender"
      if ("date_onset" %in% names(cleaned_data)) tag_mapping$date_onset <- "date_onset"
      if ("date_sample" %in% names(cleaned_data)) tag_mapping$date_reporting <- "date_sample"
      if ("region" %in% names(cleaned_data)) tag_mapping$location <- "region"
      if ("status" %in% names(cleaned_data)) tag_mapping$outcome <- "status"
      
      linelist_args <- c(list(x = cleaned_data), tag_mapping)
      validated_linelist <- do.call(linelist::make_linelist, linelist_args)
      
      # Validate structure
      linelist::validate_linelist(validated_linelist)
      cat("   Linelist created and validated successfully\n")
      cat("   Tagged variables:", length(tag_mapping), "\n")
      
    }, error = function(e) {
      cat("   Could not create linelist:", e$message, "\n")
      validated_linelist <- cleaned_data
    })
  } else {
    validated_linelist <- cleaned_data
  }
  
  cat("Data validation completed!\n")
  
  return(list(
    data = validated_linelist,
    validation_results = validation_results
  ))
}

# ==============================================================================
# SECTION 7: VISUALIZATION (ERROR-CORRECTED)
# ==============================================================================

create_outbreak_visualizations <- function(data) {
  cat(paste(rep("=", 50), collapse=""), "\n")
  cat("DATA VISUALIZATION PHASE\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  # Convert linelist to tibble if needed
  if (inherits(data, "linelist")) {
    viz_data <- as_tibble(data)
  } else {
    viz_data <- data
  }
  
  plots <- list()
  
  # Prepare data for visualization
  cat("Preparing data for visualization...\n")
  
  # Create age groups
  if ("age" %in% names(viz_data)) {
    viz_data <- viz_data %>%
      mutate(
        age_group = case_when(
          age < 5 ~ "0-4",
          age < 15 ~ "5-14", 
          age < 25 ~ "15-24",
          age < 45 ~ "25-44",
          age < 65 ~ "45-64",
          age >= 65 ~ "65+",
          TRUE ~ "Unknown"
        ),
        age_group = factor(age_group, levels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+", "Unknown"))
      )
  }
  
  # 1. Create epidemic curves
  cat("1. Creating epidemic curves...\n")
  if (incidence2_available && "date_onset" %in% names(viz_data)) {
    tryCatch({
      # Daily incidence
      daily_inc <- incidence2::incidence(viz_data, date_index = "date_onset", interval = "day")
      plots$daily_epicurve <- plot(daily_inc) +
        labs(title = "Daily Epidemic Curve", x = "Date", y = "Cases") +
        theme_minimal()
      
      # Weekly incidence
      weekly_inc <- incidence2::incidence(viz_data, date_index = "date_onset", interval = "week")
      plots$weekly_epicurve <- plot(weekly_inc) +
        labs(title = "Weekly Epidemic Curve", x = "Week", y = "Cases") +
        theme_minimal()
      
      # Cumulative
      cum_inc <- incidence2::cumulate(daily_inc)
      plots$cumulative_epicurve <- plot(cum_inc) +
        labs(title = "Cumulative Cases", x = "Date", y = "Cumulative Cases") +
        theme_minimal()
      
      cat("   Created epidemic curves\n")
    }, error = function(e) {
      cat("   Could not create epidemic curves:", e$message, "\n")
    })
  } else {
    cat("   Skipping epidemic curves (missing requirements)\n")
  }
  
  # 2. Age distribution
  cat("2. Creating age distribution...\n")
  if ("age" %in% names(viz_data)) {
    plots$age_histogram <- ggplot(viz_data, aes(x = age)) +
      geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
      labs(title = "Age Distribution of Cases", x = "Age (years)", y = "Number of Cases") +
      theme_minimal()
    
    if ("age_group" %in% names(viz_data)) {
      plots$age_group_bar <- ggplot(viz_data, aes(x = age_group)) +
        geom_bar(fill = "darkgreen", alpha = 0.7) +
        labs(title = "Cases by Age Group", x = "Age Group", y = "Number of Cases") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    cat("   Created age distribution plots\n")
  }
  
  # 3. Gender distribution
  cat("3. Creating gender distribution...\n")
  if ("gender" %in% names(viz_data)) {
    plots$gender_bar <- ggplot(viz_data, aes(x = gender)) +
      geom_bar(fill = "purple", alpha = 0.7) +
      labs(title = "Cases by Gender", x = "Gender", y = "Number of Cases") +
      theme_minimal()
    
    # Pie chart
    gender_counts <- viz_data %>%
      filter(!is.na(gender)) %>%
      count(gender, name = "count") %>%
      mutate(percentage = round(count / sum(count) * 100, 1))
    
    plots$gender_pie <- ggplot(gender_counts, aes(x = "", y = count, fill = gender)) +
      geom_col() +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +
      labs(title = "Gender Distribution") +
      theme_void()
    
    cat("   Created gender distribution plots\n")
  }
  
  # 4. Geographic distribution
  cat("4. Creating geographic distribution...\n")
  if ("region" %in% names(viz_data)) {
    plots$region_bar <- ggplot(viz_data, aes(x = region)) +
      geom_bar(fill = "orange", alpha = 0.7) +
      labs(title = "Cases by Region", x = "Region", y = "Number of Cases") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    cat("   Created geographic distribution plot\n")
  }
  
  # 5. Status distribution
  cat("5. Creating case status distribution...\n")
  if ("status" %in% names(viz_data)) {
    plots$status_bar <- ggplot(viz_data, aes(x = status)) +
      geom_bar(fill = "red", alpha = 0.7) +
      labs(title = "Cases by Status", x = "Case Status", y = "Number of Cases") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    cat("   Created case status distribution plot\n")
  }
  
  # 6. Summary dashboard
  cat("6. Creating summary dashboard...\n")
  summary_stats <- paste0(
    "OUTBREAK SUMMARY\n\n",
    "Total Cases: ", nrow(viz_data), "\n",
    if ("age" %in% names(viz_data)) paste0("Age Range: ", min(viz_data$age, na.rm = TRUE), "-", max(viz_data$age, na.rm = TRUE), " years\n") else "",
    if ("date_onset" %in% names(viz_data)) {
      date_range <- range(viz_data$date_onset, na.rm = TRUE)
      paste0("Date Range: ", date_range[1], " to ", date_range[2], "\n")
    } else "",
    if ("region" %in% names(viz_data)) paste0("Regions Affected: ", length(unique(viz_data$region[!is.na(viz_data$region)])), "\n") else ""
  )
  
  summary_df <- data.frame(x = 0.5, y = 0.5, label = summary_stats)
  
  plots$summary_dashboard <- ggplot(summary_df, aes(x, y, label = label)) +
    geom_text(size = 4, fontface = "bold") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.background = element_rect(fill = "lightgray", color = "black"))
  
  cat("Visualization creation completed!\n")
  cat("Created", length(plots), "visualizations\n")
  
  return(plots)
}

# ==============================================================================
# SECTION 8: SAVE OUTPUTS
# ==============================================================================

save_outputs <- function(cleaned_data, validation_results, plots, paths) {
  cat(paste(rep("=", 50), collapse=""), "\n")
  cat("SAVING OUTPUTS\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  # Save cleaned data
  cat("Saving cleaned data...\n")
  saveRDS(cleaned_data, file.path(paths$processed_data_path, "cleaned_outbreak_data.rds"))
  rio::export(cleaned_data, file.path(paths$processed_data_path, "cleaned_outbreak_data.csv"))
  
  # Save validation results
  cat("Saving validation results...\n")
  saveRDS(validation_results, file.path(paths$outputs_path, "validation_results.rds"))
  
  # Save plots
  cat("Saving visualizations...\n")
  plot_count <- 0
  for (plot_name in names(plots)) {
    if (!is.null(plots[[plot_name]])) {
      tryCatch({
        ggsave(
          filename = file.path(paths$plots_path, paste0(sprintf("%02d", plot_count + 1), "_", plot_name, ".png")),
          plot = plots[[plot_name]],
          width = 12, height = 8, dpi = 300, bg = "white"
        )
        plot_count <- plot_count + 1
        cat("   Saved:", plot_name, "\n")
      }, error = function(e) {
        cat("   Failed to save:", plot_name, "-", e$message, "\n")
      })
    }
  }
  
  # Generate summary report
  cat("Generating summary report...\n")
  report_file <- file.path(paths$outputs_path, "pipeline_summary.txt")
  
  sink(report_file)
  cat("OUTBREAK ANALYTICS PIPELINE SUMMARY REPORT\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat("Generated:", as.character(Sys.time()), "\n\n")
  
  cat("DATA SUMMARY:\n")
  if (inherits(cleaned_data, "linelist")) {
    data_for_summary <- as_tibble(cleaned_data)
  } else {
    data_for_summary <- cleaned_data
  }
  
  cat("- Total records:", nrow(data_for_summary), "\n")
  cat("- Total variables:", ncol(data_for_summary), "\n")
  cat("- Date range:", 
      if ("date_onset" %in% names(data_for_summary)) {
        paste(range(data_for_summary$date_onset, na.rm = TRUE), collapse = " to ")
      } else "Not available", "\n")
  
  if (!is.null(validation_results$missing_analysis)) {
    cat("\nMISSING DATA:\n")
    high_missing <- validation_results$missing_analysis %>%
      filter(missing_percent > 0) %>%
      arrange(desc(missing_percent))
    for (i in 1:min(5, nrow(high_missing))) {
      cat("-", high_missing$column[i], ":", high_missing$missing_percent[i], "%\n")
    }
  }
  
  cat("\nVISUALIZATIONS CREATED:\n")
  cat("- Total plots:", plot_count, "\n")
  cat("- Saved to:", paths$plots_path, "\n")
  
  cat("\nFILES CREATED:\n")
  cat("- Cleaned data (RDS):", file.path(paths$processed_data_path, "cleaned_outbreak_data.rds"), "\n")
  cat("- Cleaned data (CSV):", file.path(paths$processed_data_path, "cleaned_outbreak_data.csv"), "\n")
  cat("- Validation results:", file.path(paths$outputs_path, "validation_results.rds"), "\n")
  cat("- Plots directory:", paths$plots_path, "\n")
  
  sink()
  
  cat("Summary report saved to:", report_file, "\n")
  cat("All outputs saved successfully!\n")
}

# ==============================================================================
# MAIN PIPELINE EXECUTION
# ==============================================================================

run_complete_pipeline <- function() {
  cat("STARTING COMPLETE OUTBREAK ANALYTICS PIPELINE\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  start_time <- Sys.time()
  
  tryCatch({
    # Phase 1: Import/Simulate Data
    raw_data <- import_or_simulate_data()
    
    # Phase 2: Clean Data
    cleaned_data <- clean_outbreak_data(raw_data)
    
    # Phase 3: Validate Data
    validation_output <- validate_outbreak_data(cleaned_data)
    validated_data <- validation_output$data
    validation_results <- validation_output$validation_results
    
    # Phase 4: Create Visualizations
    plots <- create_outbreak_visualizations(validated_data)
    
    # Phase 5: Save All Outputs
    save_outputs(validated_data, validation_results, plots, paths)
    
    # Final summary
    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    cat("\n")
    cat(paste(rep("=", 60), collapse=""), "\n")
    cat("PIPELINE EXECUTION COMPLETED SUCCESSFULLY!\n")
    cat(paste(rep("=", 60), collapse=""), "\n")
    cat("Execution time:", round(execution_time, 2), "seconds\n")
    cat("Total records processed:", nrow(validated_data), "\n")
    cat("Visualizations created:", length(plots), "\n")
    cat("\nOutput locations:\n")
    cat("- Data files:", paths$processed_data_path, "\n")
    cat("- Plots:", paths$plots_path, "\n")
    cat("- Reports:", paths$outputs_path, "\n")
    cat("\nYour outbreak analytics pipeline is ready!\n")
    cat("Check the outputs directory for all generated files.\n")
    
    return(list(
      data = validated_data,
      validation_results = validation_results,
      plots = plots,
      execution_time = execution_time
    ))
    
  }, error = function(e) {
    cat("ERROR in pipeline execution:\n")
    cat("Message:", e$message, "\n")
    cat("Please check your R environment and try again.\n")
    return(NULL)
  })
}

# ==============================================================================
# EXECUTE THE COMPLETE PIPELINE
# ==============================================================================

# Run the complete pipeline
cat("Ready to execute complete outbreak analytics pipeline...\n")
cat("This will create a full outbreak analysis from start to finish.\n\n")

# Execute
results <- run_complete_pipeline()

# ==============================================================================
# INTERACTIVE FEATURES (OPTIONAL)
# ==============================================================================

create_interactive_elements <- function(data, plots) {
  if (!plotly_available || !DT_available) {
    cat("Interactive features require plotly and DT packages.\n")
    return(NULL)
  }
  
  cat("Creating interactive elements...\n")
  
  interactive_elements <- list()
  
  # Interactive table
  tryCatch({
    if (inherits(data, "linelist")) {
      table_data <- as_tibble(data)
    } else {
      table_data <- data
    }
    
    # Limit to first 1000 rows for performance
    if (nrow(table_data) > 1000) {
      table_data <- table_data[1:1000, ]
    }
    
    interactive_table <- DT::datatable(
      table_data,
      options = list(pageLength = 25, scrollX = TRUE),
      caption = "Outbreak Case Data (Interactive Table)"
    )
    
    # Save interactive table
    DT::saveWidget(
      interactive_table,
      file = file.path(paths$outputs_path, "interactive_data_table.html"),
      selfcontained = TRUE
    )
    
    interactive_elements$table <- "interactive_data_table.html"
    cat("   Created interactive data table\n")
    
  }, error = function(e) {
    cat("   Could not create interactive table:", e$message, "\n")
  })
  
  # Interactive plot
  if ("daily_epicurve" %in% names(plots) && !is.null(plots$daily_epicurve)) {
    tryCatch({
      interactive_plot <- plotly::ggplotly(plots$daily_epicurve)
      
      htmlwidgets::saveWidget(
        interactive_plot,
        file = file.path(paths$outputs_path, "interactive_epicurve.html"),
        selfcontained = TRUE
      )
      
      interactive_elements$plot <- "interactive_epicurve.html"
      cat("   Created interactive epidemic curve\n")
      
    }, error = function(e) {
      cat("   Could not create interactive plot:", e$message, "\n")
    })
  }
  
  return(interactive_elements)
}

# Create interactive elements if possible
if (!is.null(results)) {
  interactive_elements <- create_interactive_elements(results$data, results$plots)
  
  if (!is.null(interactive_elements)) {
    cat("\nInteractive elements created:\n")
    for (element in names(interactive_elements)) {
      cat("-", element, ":", interactive_elements[[element]], "\n")
    }
  }
}

# ==============================================================================
# FINAL INSTRUCTIONS
# ==============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("OUTBREAK ANALYTICS PIPELINE - SETUP COMPLETE\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("\nYour complete outbreak analytics pipeline includes:\n")
cat("\n1. DATA IMPORT & SIMULATION\n")
cat("   - Robust data import from multiple sources\n")
cat("   - Realistic outbreak data simulation\n")
cat("   - Comprehensive error handling\n")

cat("\n2. DATA CLEANING\n")
cat("   - Column name standardization\n")
cat("   - Duplicate and constant removal\n")
cat("   - Missing value standardization\n")
cat("   - Data type conversions\n")
cat("   - Text-to-number conversion\n")

cat("\n3. DATA VALIDATION\n")
cat("   - Comprehensive quality checks\n")
cat("   - Linelist creation and validation\n")
cat("   - Missing data analysis\n")
cat("   - Date and age range validation\n")

cat("\n4. DATA VISUALIZATION\n")
cat("   - Professional epidemic curves\n")
cat("   - Demographic analysis plots\n")
cat("   - Geographic distribution maps\n")
cat("   - Summary dashboards\n")

cat("\n5. OUTPUT GENERATION\n")
cat("   - High-quality PNG visualizations\n")
cat("   - Cleaned data in multiple formats\n")
cat("   - Comprehensive validation reports\n")
cat("   - Interactive HTML elements\n")

cat("\nTO USE THIS PIPELINE:\n")
cat("1. Save this script as 'complete_outbreak_pipeline.R'\n")
cat("2. Set your working directory to your project folder\n")
cat("3. Run: source('complete_outbreak_pipeline.R')\n")
cat("4. Check the outputs/ directory for all generated files\n")

cat("\nFEATURES:\n")
cat("- Handles missing packages gracefully\n")
cat("- Works with real data or generates realistic simulations\n")
cat("- Comprehensive error handling and validation\n")
cat("- Professional publication-ready visualizations\n")
cat("- Suitable for GitHub portfolio demonstration\n")

cat("\nPACKAGE REQUIREMENTS:\n")
cat("- Essential: tidyverse, rio, here\n") 
cat("- Recommended: cleanepi, linelist, incidence2, janitor\n")
cat("- Optional: plotly, DT, patchwork, scales\n")

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("Pipeline ready for execution! Run the script to start your analysis.\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# ==============================================================================
# END OF COMPLETE PIPELINE
# ==============================================================================