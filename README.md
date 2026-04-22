# vivaglint <img src="man\figures\vivaglint-badge.png" align="right" height="138" alt="vivaglint" />

R package for analyzing Viva Glint survey data with comprehensive statistical tools and organizational insights.

## Overview

**'vivaglint'** simplifies the analysis of 'Viva Glint' survey exports by providing a complete toolkit for data import, validation, statistical analysis, and reporting. The package handles the complexities of the 'Viva Glint' data structure and offers intuitive functions for common analysis tasks.

## Installation

Install the package from CRAN (pending package approval; if package install does not succeed via CRAN, you can install via GitHub below):

```r
# Install from CRAN
install.packages("vivaglint")
```

Install the development version from GitHub:

```r
# Install from GitHub
install.packages("devtools")
devtools::install_github("microsoft/vivaglint")
```

## Core Capabilities

### 1. **Data Import & Validation**
- Automatic validation of Glint export structure (CSV or API)
- Survey metadata extraction


### 2. **Survey Analysis**
- **Summary statistics**: Mean, SD, response rates, favorability percentages
- **Response distributions**: Detailed breakdown by response value
- **Cycle comparisons**: Track changes over time with delta calculations
- **Manager aggregations**: Roll up results by organizational hierarchy

### 3. **Advanced Analytics**
- **Correlation analysis**: Spearman, Pearson, and Kendall methods with significance testing
- **Factor analysis**: Identify latent constructs with multiple rotation options
- **Attrition prediction**: Link survey responses to employee turnover

## Quick Start

```r
library(vivaglint)

# 1. Load survey data
survey <- read_glint_survey("survey_export.csv")

# Or pull directly from the Viva Glint API
glint_setup(
  tenant_id = "your-tenant-id",
  client_id = "your-client-id",
  client_secret = "your-client-secret",
  experience_name = "your-experience-name"
)
survey <- read_glint_survey_api(
  survey_uuid = "your-survey-uuid",
  cycle_id = "your-cycle-id",
  emp_id_col = "EMP ID"
)

# 2. Get summary statistics (5-point scale)
summary <- summarize_survey(survey, scale_points = 5)
print(summary)

# 3. Analyze response distributions
distributions <- get_response_dist(survey)

# 4. Calculate question correlations
correlations <- get_correlations(survey, method = "spearman")

# 5. Aggregate by manager
manager_results <- aggregate_by_manager(survey, scale_points = 5)

# 6. Compare across cycles
comparison <- compare_cycles(survey1, survey2, survey3,
                            scale_points = 5,
                            cycle_names = c("Q1", "Q2", "Q3"))

# 7. Analyze employee attrition
attrition <- analyze_attrition(
  survey,
  attrition_file = "employee_attributes.csv",
  emp_id_col = "EMP ID",
  term_date_col = "Termination Date",
  scale_points = 5,
  time_periods = c(30, 90, 180)
)

# 8. Analyze by attributes
demo_results <- analyze_by_attributes(
  survey,
  attribute_file = "employee_attributes.csv",
  scale_points = 5,
  attribute_cols = c("Department", "Gender", "Tenure Group"),
  min_group_size = 10
)
```

## Key Functions

| Function | Purpose |
|----------|---------|
| `read_glint_survey()` | Import and validate Glint CSV exports |
| `read_glint_survey_api()` | Export and import survey data via the Glint API |
| `glint_setup()` | Configure API credentials for `read_glint_survey_api()` |
| `summarize_survey()` | Calculate comprehensive question metrics |
| `get_response_dist()` | Get response value distributions |
| `compare_cycles()` | Compare metrics across survey cycles |
| `get_correlations()` | Calculate inter-question correlations |
| `extract_survey_factors()` | Perform factor analysis |
| `analyze_attrition()` | Link survey responses to turnover |
| `analyze_by_attributes()` | Aggregate results by attribute groups |
| `pivot_long()` | Reshape data to long format |
| `aggregate_by_manager()` | Roll up results by manager |


## Dependencies

**Required:**
- dplyr (>= 1.0.0)
- tidyr (>= 1.0.0)
- readr (>= 2.0.0)
- stringr (>= 1.4.0)
- lubridate (>= 1.7.0)
- purrr (>= 0.3.0)
- httr (>= 1.4.0)

**Optional:**
- psych (>= 2.0.0) - for factor analysis

## Documentation

Access function documentation within R or on MSLearn:

```r
?read_glint_survey
?summarize_survey
?analyze_attrition
# etc.
```

Additional documentation files:
- `FUNCTION_OUTPUTS.md` - Detailed output structure documentation
- `USER_GUIDE.md` - Comprehensive user guide with examples



## Data Privacy & Security

By default, this package processes survey data locally and does not transmit data to external services. If you use the API import functions (e.g., `read_glint_survey_api()`), the package connects to Microsoft Graph to export and download survey data. All analysis is still performed locally. Ensure compliance with your organization's data handling policies when working with employee survey data.

## Code of Conduct

Please read the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/) prior to engaging with this package.

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft trademarks or logos is subject to and must follow Microsoft's Trademark & Brand Guidelines. Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship. Any use of third-party trademarks or logos are subject to those third-party's policies.

## Support  

This project uses GitHub Issues to track bugs and feature requests. Please search the existing 
issues before filing new issues to avoid duplicates.  For new issues, file your bug or 
feature request as a new Issue.

## Trademarks

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft trademarks or logos is subject to and must follow Microsoft’s Trademark & Brand Guidelines. Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship. Any use of third-party trademarks or logos are subject to those third-party’s policies.

---

**Version:** 0.1.0
**Last Updated:** March 2026
