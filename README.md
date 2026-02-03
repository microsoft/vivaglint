# vivaglint

R package for analyzing Viva Glint survey data with comprehensive statistical tools and organizational insights.

## Overview

**vivaglint** simplifies the analysis of Microsoft Viva Glint survey exports by providing a complete toolkit for data import, validation, statistical analysis, and reporting. The package handles the complexities of Glint's data structure and offers intuitive functions for common analysis tasks.

## Installation

Install the package directly from a local source using `devtools` or `remotes`:

```r
# Install from local directory
install.packages("devtools")
devtools::install_local("path/to/vivaglint")

# Or using remotes
install.packages("remotes")
remotes::install_local("path/to/vivaglint")
```

If installing from GitHub (when available):

```r
# Install from GitHub
devtools::install_github("username/vivaglint")
```

## Core Capabilities

### 1. **Data Import & Validation**
- Automatic validation of Glint export structure
- Date parsing with format detection
- Question metadata extraction
- Comprehensive error messages for troubleshooting

### 2. **Survey Analysis**
- **Summary statistics**: Mean, SD, response rates, favorability percentages
- **Response distributions**: Detailed breakdown by response value
- **Cycle comparisons**: Track changes over time with delta calculations
- **Manager aggregations**: Roll up results by organizational hierarchy

### 3. **Advanced Analytics**
- **Correlation analysis**: Spearman, Pearson, and Kendall methods
- **Factor analysis**: Identify latent constructs with multiple rotation options
- **Attrition prediction**: Link survey responses to employee turnover

### 4. **Data Reshaping**
- Convert wide survey data to long format
- Extract comments for qualitative analysis
- Flexible output formats for different analysis needs

### 5. **Organizational Hierarchy**
- Manager-level aggregations (direct or full tree)
- Recursive report traversal
- Team-level metrics and comparisons

## Quick Start

```r
library(vivaglint)

# 1. Load survey data
survey <- read_glint_survey("survey_export.csv")

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
```

## Key Functions

| Function | Purpose |
|----------|---------|
| `read_glint_survey()` | Import and validate Glint CSV exports |
| `summarize_survey()` | Calculate comprehensive question metrics |
| `get_response_dist()` | Get response value distributions |
| `compare_cycles()` | Compare metrics across survey cycles |
| `get_correlations()` | Calculate inter-question correlations |
| `extract_survey_factors()` | Perform factor analysis |
| `analyze_attrition()` | Link survey responses to turnover |
| `pivot_long()` | Reshape data to long format |
| `aggregate_by_manager()` | Roll up results by manager |

## Favorability Classifications

The package uses Glint's standard favorability classifications based on scale points:

| Scale | Favorable | Neutral | Unfavorable |
|-------|-----------|---------|-------------|
| 2-pt  | 2         | -       | 1           |
| 3-pt  | 3         | 2       | 1           |
| 4-pt  | 4         | 2-3     | 1           |
| 5-pt  | 4-5       | 3       | 1-2         |
| 6-pt  | 4-6       | -       | 1-3         |
| 7-pt  | 6-7       | 4-5     | 1-3         |
| 8-pt  | 6-8       | 4-5     | 1-3         |
| 9-pt  | 7-9       | 4-6     | 1-3         |
| 10-pt | 8-10      | 4-7     | 1-3         |
| 11-pt | 10-11     | 8-9     | 1-7         |

## Output Examples

### Survey Summary
```r
summary <- summarize_survey(survey, scale_points = 5)
```

| question | mean | sd | n_responses | response_rate | pct_favorable | pct_neutral | pct_unfavorable |
|----------|------|----|-----------|--------------|--------------| ------------|-----------------|
| My work is meaningful | 3.6 | 1.14 | 5 | 1.00 | 60.0 | 20.0 | 20.0 |
| I feel valued | 4.2 | 0.84 | 5 | 1.00 | 80.0 | 20.0 | 0.0 |

### Attrition Analysis
```r
attrition <- analyze_attrition(survey, attrition_file,
                               emp_id_col = "EMP ID",
                               term_date_col = "Termination Date",
                               scale_points = 5)
```

Shows risk ratios indicating how much more likely employees with unfavorable responses are to leave compared to those with favorable responses.

## Dependencies

**Required:**
- dplyr (>= 1.0.0)
- tidyr (>= 1.0.0)
- readr (>= 2.0.0)
- stringr (>= 1.4.0)
- lubridate (>= 1.7.0)
- purrr (>= 0.3.0)

**Optional:**
- psych (>= 2.0.0) - for factor analysis

## Documentation

Access function documentation within R:

```r
?read_glint_survey
?summarize_survey
?analyze_attrition
# etc.
```

Additional documentation files:
- `QUICK_REFERENCE.md` - Quick reference guide with examples
- `FUNCTION_OUTPUTS.md` - Detailed output structure documentation
- `PACKAGE_USAGE.md` - Comprehensive usage guide

## Package Structure

```
vivaglint/
├── R/
│   ├── import.R       # Data import and validation
│   ├── analyze.R      # Statistical analysis functions
│   ├── reshape.R      # Data transformation
│   ├── hierarchy.R    # Organizational analysis
│   └── utils.R        # Helper functions
├── tests/
│   └── testthat/      # Test suite (124 tests)
├── man/               # Function documentation
└── vignettes/         # Usage examples
```

## Data Privacy & Security

This package processes survey data locally and does not transmit data to external services. All analysis is performed within your R environment. Ensure compliance with your organization's data handling policies when working with employee survey data.

## License

MIT License

## Support

For issues, questions, or contributions, please refer to the package maintainer.

---

**Version:** 0.1.0
**Last Updated:** January 2025
