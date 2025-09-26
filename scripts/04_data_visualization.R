# ==============================================================================
# SECTION 7: VISUALIZATION
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

