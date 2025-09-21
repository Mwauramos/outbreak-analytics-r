# Outbreak Analytics with R

[![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![GitHub Issues](https://img.shields.io/github/issues/Mwauramos/outbreak-analytics-r)](https://github.com/Mwauramos/outbreak-analytics-r/issues)

> A comprehensive R toolkit for epidemiological outbreak data analysis, from data ingestion to visualization.

## Overview

This project provides a complete pipeline for analyzing outbreak and epidemic data using modern R packages from the [Epiverse-TRACE](https://epiverse-trace.github.io/) ecosystem. It demonstrates best practices for reading, cleaning, validating, and visualizing epidemiological data in emergency response situations.

## Features

- **Multi-source Data Import**: Read from files, databases, and Health Information Systems (HIS)
- **Automated Data Cleaning**: Standardize dates, handle missing values, validate IDs
- **Data Validation Framework**: Tag and safeguard critical epidemiological variables
- **Advanced Visualization**: Create epidemic curves and time-series analysis
- **Reproducible Workflows**: Complete pipeline from raw data to publication-ready figures

## Technologies Used

### Core R Packages
- **Data Import**: `readepi`, `rio`, `DBI`, `dbplyr`
- **Data Cleaning**: `cleanepi`, `tidyverse`
- **Data Validation**: `linelist`
- **Visualization**: `incidence2`, `ggplot2`, `tracetheme`
- **Simulation**: `simulist`

### External Systems
- **DHIS2** integration for health information systems
- **SORMAS** compatibility for surveillance data
- **MySQL/PostgreSQL** database connectivity
- **REDCap** API support

## Project Structure

```
outbreak-analytics-r/
├── data/                           # Sample datasets
│   ├── raw/                       # Raw outbreak data files
│   ├── cleaned/                   # Processed data outputs
│   └── simulated/                 # Generated test datasets
├── R/                             # Core analysis functions
│   ├── 01_data_import.R          # Multi-source data reading
│   ├── 02_data_cleaning.R        # Cleaning and standardization
│   ├── 03_data_validation.R      # Linelist creation and validation
│   ├── 04_visualization.R        # Epidemic curve generation
│   └── utils.R                   # Helper functions
├── scripts/                       # Analysis workflows
│   ├── complete_pipeline.R       # End-to-end analysis
│   ├── ebola_analysis.R          # Ebola case study
│   └── marburg_analysis.R        # Marburg outbreak example
├── outputs/                       # Generated reports and figures
│   ├── figures/                  # Epidemic curves and plots
│   ├── reports/                  # HTML cleaning reports
│   └── tables/                   # Summary statistics
├── tutorials/                     # Step-by-step guides
│   ├── 01_setup.md              # Environment setup
│   ├── 02_reading_data.md       # Data import tutorial
│   ├── 03_cleaning_data.md      # Data cleaning guide
│   ├── 04_validation.md         # Validation framework
│   └── 05_visualization.md      # Visualization techniques
├── tests/                        # Unit tests and validation
├── renv.lock                     # Package dependency management
├── DESCRIPTION                   # R package metadata
├── NAMESPACE                     # Package exports
└── README.md                     # This file
```

## 🚀 Quick Start

### Prerequisites

- R (≥ 4.0.0)
- RStudio (recommended)
- Git

### Installation

1. **Clone the repository**
```bash
git clone https://github.com/Mwauramos/outbreak-analytics-r.git
cd outbreak-analytics-r
```

2. **Install dependencies**
```r
# Install renv for dependency management
install.packages("renv")
renv::restore()

# Or install packages manually
source("R/install_packages.R")
```

3. **Set up project structure**
```r
# Create necessary directories
source("R/setup_project.R")
```

### Basic Usage

```r
# Load the project functions
source("R/utils.R")

# Complete analysis pipeline
source("scripts/complete_pipeline.R")

# Or run individual components
source("R/01_data_import.R")    # Import data
source("R/02_data_cleaning.R")  # Clean and standardize
source("R/03_data_validation.R") # Validate and tag
source("R/04_visualization.R")   # Generate visualizations
```

## 📊 Example Workflows

### 1. Ebola Outbreak Analysis

```r
# Read Ebola case data
ebola_raw <- read_outbreak_data(
  file = "data/raw/ebola_cases.csv",
  type = "csv"
)

# Clean and standardize
ebola_clean <- clean_outbreak_data(
  data = ebola_raw,
  date_columns = c("date_onset", "date_sample"),
  id_column = "case_id"
)

# Create linelist object
ebola_linelist <- create_linelist(
  data = ebola_clean,
  id = "case_id",
  date_onset = "date_onset",
  gender = "gender"
)

# Generate epidemic curve
plot_epidemic_curve(
  data = ebola_linelist,
  date_column = "date_onset",
  interval = "week"
)
```

### 2. Multi-source Data Integration

```r
# Read from different sources
dhis2_data <- read_dhis2_data(
  url = "https://example.dhis2.org",
  program = "outbreak_surveillance"
)

database_data <- read_database_data(
  connection = db_connection,
  table = "case_reports"
)

file_data <- read_file_data("data/raw/cases.xlsx")

# Combine and harmonize
combined_data <- harmonize_data_sources(
  dhis2_data, database_data, file_data
)
```

## Key Features Demonstrated

### Data Import Capabilities
- **File formats**: CSV, Excel, TSV, compressed files
- **Databases**: MySQL, PostgreSQL, SQLite, SQL Server  
- **APIs**: DHIS2, SORMAS, REDCap integration
- **Cross-platform compatibility** with `here` package

### Data Cleaning Pipeline
- **Column standardization**: Automated naming conventions
- **Missing value handling**: Multiple encoding detection
- **Date standardization**: ISO format conversion
- **Duplicate detection**: Advanced deduplication algorithms
- **Data type conversion**: Text-to-numeric with multi-language support

### Validation Framework
- **Linelist objects**: Tagged epidemiological variables
- **Data integrity**: Safeguarding against accidental deletion
- **Type validation**: Automatic data type checking
- **Sequence validation**: Date logic verification

### Visualization Tools
- **Epidemic curves**: Daily, weekly, monthly aggregation
- **Stratified analysis**: By demographics and case characteristics
- **Peak estimation**: Bootstrap confidence intervals
- **Custom themes**: Publication-ready formatting

## Testing

Run the test suite to validate functionality:

```r
# Run all tests
testthat::test_dir("tests/")

# Test specific components
testthat::test_file("tests/test_data_import.R")
testthat::test_file("tests/test_data_cleaning.R")
```

## Use Cases

This toolkit is designed for:

- **Emergency response teams** conducting outbreak investigations
- **Epidemiologists** analyzing surveillance data
- **Public health researchers** studying disease patterns
- **Data scientists** working with health data
- **Students** learning epidemiological data analysis

## Contributing

Contributions are welcome! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Documentation

Detailed documentation is available in the `tutorials/` directory:

- [Environment Setup](tutorials/01_setup.md)
- [Data Import Guide](tutorials/02_reading_data.md)  
- [Data Cleaning Workflow](tutorials/03_cleaning_data.md)
- [Validation Framework](tutorials/04_validation.md)
- [Visualization Techniques](tutorials/05_visualization.md)

## Related Projects

- [Epiverse-TRACE](https://epiverse-trace.github.io/) - R packages for outbreak analytics
- [R4Epis](https://r4epis.netlify.app/) - R for applied epidemiology
- [The Epidemiologist R Handbook](https://epirhandbook.com/) - Comprehensive R guide

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Epiverse-TRACE](https://epiverse-trace.github.io/) for the foundational packages
- [The Carpentries](https://carpentries.org/) for the tutorial framework
- Contributors to the R epidemiology ecosystem

## Contact

**Amos Mwaura**  
📧 Email: mwauramos.n@gmail.com  


---

⭐ **Star this repository if you find it useful!**

*Built with ❤️ for the global health community*
