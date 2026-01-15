# vivaglint Package Usage Guide

## Installation and Setup

To use this package, you'll need to install the required dependencies and generate documentation:

```r
# Install dependencies
install.packages(c("dplyr", "tidyr", "readr", "stringr", "lubridate", "data.tree", "purrr"))

# Install devtools if not already installed
install.packages("devtools")

# Load the package for development
devtools::load_all()

# Generate documentation from roxygen comments
devtools::document()
```

## Core Functions

### 1. Import and Validate Data

```r
# Read a Viva Glint survey export
survey <- read_glint_survey("path/to/your/survey_export.csv")

# The function automatically validates the structure and parses dates
# Returns a list with:
#   - $data: the survey data as a tibble
#   - $metadata: information about questions and structure

# Extract question information
questions <- extract_questions(survey)
print(questions)
```

### 2. Reshape Data

```r
# Convert to long format (one row per respondent-question)
long_responses <- pivot_responses_long(survey)
head(long_responses)

# Extract just the comments in long format
comments <- pivot_comments_long(survey)
head(comments)

# Include empty comments
all_comments <- pivot_comments_long(survey, include_empty = TRUE)
```

### 3. Analyze Questions

```r
# Analyze a single question
result <- analyze_question(survey, "My work gives me a sense of personal accomplishment.")
print(result)

# The result includes:
# - mean: average response
# - sd: standard deviation
# - n_responses: count of non-blank responses
# - n_skips: count of blank/null responses
# - n_total: total respondents
# - response_rate: (responses + skips) / total
# - value_counts: count for each response value
# - value_percents: percentage for each response value

# Analyze all questions in the survey
full_analysis <- analyze_survey(survey)
print(full_analysis)

# Expand value distributions into separate columns
full_analysis_expanded <- analyze_survey(survey, expand_values = TRUE)
```

### 4. Compare Across Survey Cycles

```r
# Load multiple survey cycles
survey_q1 <- read_glint_survey("survey_2024_q1.csv")
survey_q2 <- read_glint_survey("survey_2024_q2.csv")
survey_q3 <- read_glint_survey("survey_2024_q3.csv")

# Compare trends across cycles
comparison <- compare_cycles(
  survey_q1, survey_q2, survey_q3,
  cycle_names = c("Q1 2024", "Q2 2024", "Q3 2024")
)

# View questions with biggest improvements
library(dplyr)
comparison %>%
  filter(!is.na(change_from_previous)) %>%
  arrange(desc(change_from_previous)) %>%
  select(cycle, question, mean, change_from_previous)
```

### 5. Organizational Hierarchy Analysis

```r
# Build organizational tree
org_tree <- build_org_tree(survey)
print(org_tree)

# Aggregate responses by manager (direct reports only)
manager_summary <- aggregate_by_manager(survey)
head(manager_summary)

# Aggregate with full organizational tree (all indirect reports)
manager_summary_full <- aggregate_by_manager(survey, full_tree = TRUE)

# Compare managers (with minimum team size for anonymity)
manager_comparison <- compare_managers(survey, min_team_size = 5)

# View top and bottom performing teams
manager_comparison %>%
  filter(question == "My work gives me a sense of personal accomplishment.") %>%
  arrange(rank) %>%
  select(manager_name, team_size, mean, rank, diff_from_overall)
```

## Testing the Package

Run the test suite:

```r
# Run all tests
devtools::test()

# Check package for issues
devtools::check()
```

## Example Workflow

```r
library(vivaglint)
library(dplyr)
library(ggplot2)

# 1. Load survey data
survey <- read_glint_survey("my_survey_export.csv")

# 2. Get overview of all questions
overview <- analyze_survey(survey, expand_values = TRUE)
print(overview)

# 3. Identify questions with low scores
low_scoring <- overview %>%
  filter(mean < 3) %>%
  arrange(mean) %>%
  select(question, mean, n_responses, response_rate)

# 4. Analyze comments for low-scoring questions
comments <- pivot_comments_long(survey) %>%
  filter(question %in% low_scoring$question)

# 5. Compare manager performance
manager_analysis <- compare_managers(survey, min_team_size = 5) %>%
  filter(question == low_scoring$question[1])

# 6. Visualize results
ggplot(manager_analysis, aes(x = reorder(manager_name, mean), y = mean)) +
  geom_col() +
  geom_hline(yintercept = manager_analysis$overall_mean[1],
             linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Manager Comparison",
       subtitle = low_scoring$question[1],
       x = "Manager", y = "Mean Score")
```

## Error Handling

The package provides detailed, human-readable error messages:

### Missing Standard Columns
```
Error: Missing required standard column(s): 'EMP ID', 'Manager ID'

Your CSV file must contain all of the following standard Viva Glint columns:
  - First Name
  - Last Name
  - Email
  - Status
  - EMP ID
  - Manager ID
  - Survey Cycle Completion Date
  - Survey Cycle Sent Date

Please ensure you are using a complete Viva Glint survey export.
```

### Incomplete Question Sets
```
Error: Incomplete question column sets found:
  - Question 'My work is meaningful' is missing: '_COMMENT_TOPICS', '_SENSITIVE_COMMENT_FLAG'

Each question must have all four columns:
  - [Question Text]
  - [Question Text]_COMMENT
  - [Question Text]_COMMENT_TOPICS
  - [Question Text]_SENSITIVE_COMMENT_FLAG
```

### Invalid File Format
```
Error: Error reading CSV file: [details]
Please ensure the file is a valid CSV format.
```

## Notes

- Response values can be any integer (not limited to 1-5 scale)
- Response rate is calculated as: (responses + skips) / total respondents
- Dates are parsed from format: "DD-MM-YYYY HH:MM"
- Comments are preserved with their associated topics and sensitivity flags
- Manager comparisons exclude teams below minimum size for anonymity protection (default: 5)
- The organizational tree handles cycles, orphaned nodes, and multiple root nodes
