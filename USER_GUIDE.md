# vivaglint User Guide

## Overview

The vivaglint package helps you import, validate, and analyze Viva Glint survey exports. It provides question-level summaries, response distributions, trend analysis across cycles, manager rollups, attribute segmentation, attrition analysis, and comment search.

## Installation

From CRAN:

```r
install.packages("vivaglint")
```

Development setup:

```r
install.packages(c("devtools", "dplyr", "tidyr", "readr", "stringr", "lubridate", "purrr", "httr"))

devtools::load_all()
# Regenerate documentation when needed
# devtools::document()
```

## Quick Start (CSV Export)

```r
library(vivaglint)

survey_path <- system.file("extdata", "survey_export.csv", package = "vivaglint")
survey <- read_glint_survey(survey_path, emp_id_col = "EMP ID")

summary <- summarize_survey(survey, scale_points = 5)
response_dist <- get_response_dist(survey)
correlations <- get_correlations(survey, method = "spearman")
```

## API Import (Optional)

```r
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
```

## Core Workflows

### Summarize Questions

```r
summary <- summarize_survey(survey, scale_points = 5)
summary_subset <- summarize_survey(
  survey,
  scale_points = 5,
  questions = c("My work is meaningful", "I feel valued")
)
```

### Response Distributions

```r
dist <- get_response_dist(survey)
```

### Compare Survey Cycles

```r
survey_path <- system.file("extdata", "survey_export.csv", package = "vivaglint")
survey_q1 <- read_glint_survey(survey_path, emp_id_col = "EMP ID")
survey_q2 <- read_glint_survey(survey_path, emp_id_col = "EMP ID")
survey_q3 <- read_glint_survey(survey_path, emp_id_col = "EMP ID")

comparison <- compare_cycles(
  survey_q1, survey_q2, survey_q3,
  scale_points = 5,
  cycle_names = c("Q1 2024", "Q2 2024", "Q3 2024")
)
```

### Manager Rollups

```r
manager_summary <- aggregate_by_manager(
  survey,
  scale_points = 5,
  emp_id_col = "EMP ID",
  manager_id_col = "Manager ID"
)
```

### Attribute Segmentation

```r
survey_path <- system.file("extdata", "survey_export.csv", package = "vivaglint")
attr_path <- system.file("extdata", "employee_attributes.csv", package = "vivaglint")

survey <- read_glint_survey(survey_path, emp_id_col = "EMP ID")
survey_enriched <- join_attributes(survey, attr_path, emp_id_col = "EMP ID")
attr_results <- analyze_by_attributes(
  survey_enriched,
  scale_points = 5,
  attribute_cols = c("Department", "Gender"),
  emp_id_col = "EMP ID",
  min_group_size = 10
)
```

### Attrition Analysis

```r
attrition_path <- system.file("extdata", "attrition.csv", package = "vivaglint")

attrition <- analyze_attrition(
  survey,
  attrition_file = attrition_path,
  emp_id_col = "EMP ID",
  term_date_col = "Termination Date",
  scale_points = 5,
  time_periods = c(30, 90, 180)
)
```

### Correlations

```r
cor_long <- get_correlations(survey)
cor_matrix <- get_correlations(survey, format = "matrix")
```

### Factor Analysis (Requires psych)

```r
factors <- extract_survey_factors(survey, n_factors = 2, rotation = "oblimin")
```

### Comments and Reshaping

```r
comments <- pivot_long(survey, data_type = "comments")
all_responses <- pivot_long(survey, data_type = "all")

comment_hits <- search_comments(survey, "manager")
```

### Split Quantitative vs Qualitative Data

```r
parts <- split_survey_data(survey)
quant_summary <- summarize_survey(parts$quantitative, scale_points = 5,
                                  emp_id_col = "EMP ID")
```

## Function Summary

| Function | Purpose |
|----------|---------|
| read_glint_survey() | Import and validate Glint CSV exports |
| read_glint_survey_api() | Export and import survey data via the Glint API |
| glint_setup() | Configure API credentials for read_glint_survey_api() |
| extract_questions() | Identify question columns and their related fields |
| summarize_survey() | Question-level summary metrics |
| get_response_dist() | Response distributions by question |
| compare_cycles() | Track changes across survey cycles |
| aggregate_by_manager() | Roll up results by manager |
| join_attributes() | Add employee attributes to a survey object |
| analyze_by_attributes() | Segment results by attributes |
| analyze_attrition() | Attrition risk by survey responses |
| get_correlations() | Correlation analysis across questions |
| extract_survey_factors() | Factor analysis over responses |
| pivot_long() | Long-format responses/comments |
| search_comments() | Find comments by keyword |
| split_survey_data() | Separate quantitative vs qualitative data |

## Common Validation Errors

**Missing Standard Columns**
```
Missing required standard column(s): 'EMP ID', 'Manager ID'

Your CSV file must contain all of the following standard Viva Glint columns:
  - First Name
  - Last Name
  - Email
  - Status
  - EMP ID
  - Manager ID
  - Survey Cycle Completion Date
  - Survey Cycle Sent Date
```

**Incomplete Question Sets**
```
Incomplete question column sets found:
  - Question 'My work is meaningful' is missing: '_COMMENT_TOPICS'

Each question must have all four columns:
  - [Question Text]
  - [Question Text]_COMMENT
  - [Question Text]_COMMENT_TOPICS
  - [Question Text]_SENSITIVE_COMMENT_FLAG
```

## Additional Resources

- FUNCTION_OUTPUTS.md - Detailed output structure examples
