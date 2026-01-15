# vivaglint Quick Reference

## Function Summary

### 📥 Import Functions

```r
# Load survey data
survey <- read_glint_survey("file.csv")
# Returns: list(data = tibble, metadata = list)
# - data: 5 rows × 16 columns (8 standard + 4 per question)
# - metadata: questions, n_respondents, n_questions, file_path

# Extract question information
questions <- extract_questions(survey)
# Returns: tibble with 2 rows × 5 columns
# Columns: question, response_col, comment_col, topics_col, flag_col
```

### 📊 Analysis Functions

```r
# Analyze single question
result <- analyze_question(survey, "My work is meaningful")
# Returns: 1 row × 9 columns
# Columns: question, mean, sd, n_responses, n_skips, n_total,
#          response_rate, value_counts (list), value_percents (list)

# Analyze all questions
analysis <- analyze_survey(survey, expand_values = FALSE)
# Returns: 2 rows × 9 columns (one row per question)
# Same columns as analyze_question()

# Analyze all questions (expanded)
analysis <- analyze_survey(survey, expand_values = TRUE)
# Returns: 2 rows × 15 columns
# Additional columns: count_2, pct_2, count_3, pct_3, count_4, pct_4, count_5, pct_5

# Compare survey cycles
comparison <- compare_cycles(survey1, survey2, survey3,
                            cycle_names = c("Q1", "Q2", "Q3"))
# Returns: 6 rows × 11 columns (2 questions × 3 cycles)
# Additional columns: cycle, change_from_previous, pct_change_from_previous

# Calculate question correlations
correlations <- get_correlations(survey)
# Returns: 4 rows × 5 columns (2 questions × 2 questions, long format)
# Columns: question1, question2, correlation, p_value, n

# Get correlation matrix
cor_matrix <- get_correlations(survey, format = "matrix")
# Returns: 2x2 matrix with questions as rows/columns

# Factor analysis (requires psych package)
factors <- extract_survey_factors(survey, n_factors = 2, rotation = "oblimin")
# Returns: list with loadings, variance_explained, communalities, etc.
```

### 🔄 Reshape Functions

```r
# Convert to long format
long_data <- pivot_responses_long(survey)
# Returns: 10 rows × 13 columns (5 respondents × 2 questions)
# Columns: [8 standard cols], question, response, comment,
#          comment_topics, sensitive_flag

# Extract comments only
comments <- pivot_comments_long(survey)
# Returns: 8 rows × 13 columns (only non-empty comments)
# Same structure as pivot_responses_long()
```

### 👥 Hierarchy Functions

```r
# Build organizational tree
org_tree <- build_org_tree(survey)
# Returns: data.tree Node object
# Hierarchical structure showing reporting relationships

# Aggregate by manager
manager_agg <- aggregate_by_manager(survey, full_tree = FALSE)
# Returns: 6 rows × 11 columns (3 managers × 2 questions)
# Columns: manager_id, manager_name, question, team_size, mean, sd,
#          n_responses, n_skips, n_total, response_rate
```

## Example Workflow

```r
library(vivaglint)
# or: devtools::load_all()

# 1. Load data
survey <- read_glint_survey("my_survey.csv")

# 2. Quick overview
analyze_survey(survey, expand_values = TRUE)

# 3. Detailed analysis of specific question
analyze_question(survey, "My work is meaningful")

# 4. Get all comments
comments <- pivot_comments_long(survey)
View(comments)

# 5. Manager analysis
manager_summary <- aggregate_by_manager(survey)

# 6. Multi-cycle trends
comparison <- compare_cycles(
  survey_q1, survey_q2, survey_q3,
  cycle_names = c("Q1 2024", "Q2 2024", "Q3 2024")
)

# 7. Question correlations
correlations <- get_correlations(survey)
cor_matrix <- get_correlations(survey, format = "matrix")

# 8. Factor analysis
factors <- extract_survey_factors(survey, rotation = "oblimin")
```

## Key Metrics Explained

- **mean**: Average numeric response
- **sd**: Standard deviation
- **n_responses**: Count of non-blank responses
- **n_skips**: Count of blank/null responses
- **n_total**: Total respondents (n_responses + n_skips)
- **response_rate**: (n_responses + n_skips) / n_total
- **count_X**: Number of responses with value X
- **pct_X**: Percentage of responses with value X
- **change_from_previous**: Difference in mean from prior cycle
- **pct_change_from_previous**: Percent change from prior cycle
- **diff_from_overall**: Manager mean - overall mean
- **rank**: Manager ranking (1 = highest mean)
- **percentile**: Manager's position (0-100)

## Common Patterns

### Filter low-scoring questions
```r
analysis <- analyze_survey(survey, expand_values = TRUE)
low_scores <- analysis %>% filter(mean < 3) %>% arrange(mean)
```

### Get comments for specific question
```r
comments <- pivot_comments_long(survey) %>%
  filter(question == "My work is meaningful")
```


### Track trends over time
```r
trends <- compare_cycles(s1, s2, s3, cycle_names = c("Q1", "Q2", "Q3")) %>%
  filter(question == "My work is meaningful") %>%
  select(cycle, mean, change_from_previous)
```

### Find highly correlated questions
```r
correlations <- get_correlations(survey) %>%
  filter(question1 != question2) %>%  # Remove self-correlations
  filter(p_value < 0.05) %>%  # Only statistically significant
  arrange(desc(abs(correlation)))
```

### Identify underlying question factors
```r
factors <- extract_survey_factors(survey, n_factors = 3, rotation = "varimax")
# View loadings above threshold
print(factors$loadings_clean)
# View variance explained by each factor
print(factors$variance_explained)
```

### Identify top concerns by sentiment
```r
comments <- pivot_comments_long(survey) %>%
  filter(sensitive_flag >= 3) %>%
  arrange(desc(sensitive_flag))
```

## Data Dimensions Reference

| Function | Input | Output Rows | Output Cols |
|----------|-------|-------------|-------------|
| `read_glint_survey()` | CSV | n | 8 + 4q |
| `extract_questions()` | survey | q | 5 |
| `summarize_survey()` | survey | q | 7 |
| `get_response_dist()` | survey | q | 1 + 2v |
| `compare_cycles()` | multiple surveys | q×c | 11 |
| `pivot_long()` | survey | n×q or varies | 13 |
| `aggregate_by_manager()` | survey | m×q | 11 |
| `build_org_tree()` | survey | hierarchical | N/A |
| `get_correlations()` | survey | q×q | 5 or matrix |
| `extract_survey_factors()` | survey | list | N/A |

**Legend:**
- n = respondents
- q = questions
- m = managers (with teams ≥ min_size)
- c = cycles
- v = unique response values

## Error Messages

The package provides helpful validation errors:

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

```
Incomplete question column sets found:
  - Question 'My work is meaningful' is missing: '_COMMENT_TOPICS'

Each question must have all four columns:
  - [Question Text]
  - [Question Text]_COMMENT
  - [Question Text]_COMMENT_TOPICS
  - [Question Text]_SENSITIVE_COMMENT_FLAG
```

## See Also

- [PACKAGE_USAGE.md](PACKAGE_USAGE.md) - Comprehensive usage guide
- [FUNCTION_OUTPUTS.md](FUNCTION_OUTPUTS.md) - Detailed output examples
- [demo.R](demo.R) - Working demo script
