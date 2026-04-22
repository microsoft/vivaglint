# vivaglint Function Output Examples

This document shows the structure of objects returned by each function in the vivaglint package.

## 1. read_glint_survey()

**Returns:** A list with class `glint_survey` containing `data` and `metadata`

```r
survey <- read_glint_survey("tests/testthat/fixtures/sample_survey.csv")
str(survey)
```

**Output Structure:**
```
List of 2
 $ data    : tibble [5 × 16]
   ..$ First Name                                        : chr [1:5] "Alice" "Bob" "Carol" "David" ...
   ..$ Last Name                                         : chr [1:5] "Smith" "Jones" "White" "Brown" ...
   ..$ Email                                             : chr [1:5] "alice@example.com" "bob@example.com" ...
   ..$ Status                                            : chr [1:5] "ACTIVE" "ACTIVE" "ACTIVE" "ACTIVE" ...
   ..$ EMP ID                                            : chr [1:5] "e001" "e002" "e003" "e004" ...
   ..$ Manager ID                                        : chr [1:5] "m001" "m001" "m002" "m002" ...
   ..$ Survey Cycle Completion Date                      : POSIXct [1:5]
   ..$ Survey Cycle Sent Date                            : POSIXct [1:5]
   ..$ My work is meaningful                             : num [1:5] 4 3 5 2 4
   ..$ My work is meaningful_COMMENT                     : chr [1:5] "Great work environment" "Could be better" ...
   ..$ My work is meaningful_COMMENT_TOPICS              : chr [1:5] "Accomplishment" "Improvement" ...
   ..$ My work is meaningful_SENSITIVE_COMMENT_FLAG      : num [1:5] 1 2 1 3 NA
   ..$ I feel valued                                     : num [1:5] 5 4 5 3 4
   ..$ I feel valued_COMMENT                             : chr [1:5] "Appreciate the recognition" "" ...
   ..$ I feel valued_COMMENT_TOPICS                      : chr [1:5] "Recognition" "" ...
   ..$ I feel valued_SENSITIVE_COMMENT_FLAG              : num [1:5] 1 NA 1 2 1

 $ metadata: List of 5
   ..$ standard_columns: chr [1:8] "First Name" "Last Name" "Email" "Status" ...
   ..$ questions       : tibble [2 × 5]
   ..$ n_respondents   : int 5
   ..$ n_questions     : int 2
   ..$ file_path       : chr "tests/testthat/fixtures/sample_survey.csv"
```

---

## 1b. read_glint_survey_api()

**Returns:** Same structure as `read_glint_survey()`

```r
survey <- read_glint_survey_api(
  survey_uuid = "your-survey-uuid",
  cycle_id = "your-cycle-id",
  emp_id_col = "EMP ID"
)
str(survey)
```

**Notes:**
- `metadata$file_path` is set to `NA` because the data is pulled from the API.

---

## 2. extract_questions()

**Returns:** A tibble with one row per question

```r
questions <- extract_questions(survey)
print(questions)
```

**Output:**
```
# A tibble: 2 × 5
  question              response_col          comment_col                topics_col                        flag_col
  <chr>                 <chr>                 <chr>                      <chr>                             <chr>
1 My work is meaningful My work is meaningful My work is meaningful_COM… My work is meaningful_COMMENT_T… My work…
2 I feel valued         I feel valued         I feel valued_COMMENT      I feel valued_COMMENT_TOPICS      I feel …
```

---

## 3. summarize_survey()

**Returns:** A tibble with one row per question containing summary metrics

```r
result <- summarize_survey(survey, questions = "My work is meaningful")
print(result)
```

**Output:**
```
# A tibble: 1 × 7
  question               mean    sd n_responses n_skips n_total response_rate
  <chr>                 <dbl> <dbl>       <int>   <int>   <int>         <dbl>
1 My work is meaningful   3.6  1.14           5       0       5             1
```

**All questions:**
```r
analysis <- summarize_survey(survey)
print(analysis)

# A tibble: 2 × 7
  question               mean    sd n_responses n_skips n_total response_rate
  <chr>                 <dbl> <dbl>       <int>   <int>   <int>         <dbl>
1 My work is meaningful   3.6 1.14            5       0       5             1
2 I feel valued           4.2 0.837           5       0       5             1
```

---

## 4. get_response_dist()

**Returns:** A tibble with response distribution (counts and percentages)

```r
distributions <- get_response_dist(survey)
print(distributions)
```

**Output:**
```
# A tibble: 2 × 9
  question              count_2 pct_2 count_3 pct_3 count_4 pct_4 count_5 pct_5
  <chr>                   <dbl> <dbl>   <int> <dbl>   <int> <dbl>   <int> <dbl>
1 My work is meaningful       1    20       1    20       2    40       1    20
2 I feel valued               0     0       1    20       2    40       2    40
```

**Single question:**
```r
dist <- get_response_dist(survey, questions = "My work is meaningful")
print(dist)

# A tibble: 1 × 9
  question              count_2 pct_2 count_3 pct_3 count_4 pct_4 count_5 pct_5
  <chr>                   <dbl> <dbl>   <int> <dbl>   <int> <dbl>   <int> <dbl>
1 My work is meaningful       1    20       1    20       2    40       1    20
```

---

## 5. compare_cycles()

**Returns:** A tibble with one row per question per cycle

```r
# Example with 3 survey cycles
comparison <- compare_cycles(survey1, survey2, survey3,
                            cycle_names = c("Q1 2024", "Q2 2024", "Q3 2024"))
print(comparison)
```

**Output:**
```
# A tibble: 6 × 11
  cycle    question               mean    sd n_responses n_skips n_total response_rate change_from_prev… pct_change_from_…
  <chr>    <chr>                 <dbl> <dbl>       <int>   <int>   <int>         <dbl>             <dbl>             <dbl>
1 Q1 2024  My work is meaningful   3.6  1.14           5       0       5             1             NA               NA
2 Q1 2024  I feel valued           4.2  0.837          5       0       5             1             NA               NA
3 Q2 2024  My work is meaningful   3.8  1.09           5       0       5             1              0.2              5.56
4 Q2 2024  I feel valued           4.4  0.894          5       0       5             1              0.2              4.76
5 Q3 2024  My work is meaningful   4.0  0.707          5       0       5             1              0.2              5.26
6 Q3 2024  I feel valued           4.6  0.548          5       0       5             1              0.2              4.55
```

**Simplified view showing change tracking:**
```r
comparison %>%
  select(cycle, question, mean, change_from_previous, pct_change_from_previous)

# A tibble: 6 × 5
  cycle    question                   mean change_from_previous pct_change_from_previous
  <chr>    <chr>                     <dbl>                <dbl>                    <dbl>
1 Q1 2024  My work is meaningful      3.6                   NA                      NA
2 Q1 2024  I feel valued              4.2                   NA                      NA
3 Q2 2024  My work is meaningful      3.8                  0.2                     5.56
4 Q2 2024  I feel valued              4.4                  0.2                     4.76
5 Q3 2024  My work is meaningful      4.0                  0.2                     5.26
6 Q3 2024  I feel valued              4.6                  0.2                     4.55
```

---

## 6. get_correlations()

**Returns:** A tibble in long format (default) or correlation matrix

```r
# Long format (default)
correlations <- get_correlations(survey)
print(correlations)
```

**Output (long format):**
```
# A tibble: 4 × 5
  question1             question2             correlation p_value     n
  <chr>                 <chr>                       <dbl>   <dbl> <int>
1 My work is meaningful My work is meaningful       1      0          5
2 I feel valued         My work is meaningful       0.865  0.0582     5
3 My work is meaningful I feel valued               0.865  0.0582     5
4 I feel valued         I feel valued               1      0          5
```

**Output (matrix format):**
```r
cor_matrix <- get_correlations(survey, format = "matrix")
print(cor_matrix)

                      My work is meaningful I feel valued
My work is meaningful             1.0000000     0.8651809
I feel valued                     0.8651809     1.0000000
```

**Different correlation methods:**
```r
# Pearson correlation
pearson <- get_correlations(survey, method = "pearson")

# Kendall's tau
kendall <- get_correlations(survey, method = "kendall")
```

---

## 7. extract_survey_factors()

**Returns:** A list with class `survey_factors` containing factor analysis results

```r
# With specified number of factors
factors <- extract_survey_factors(survey, n_factors = 1, rotation = "oblimin")
print(factors)
```

**Output structure:**
```
List of 6
 $ loadings            : num [1:2, 1] 0.944 0.944
   ..- attr(*, "dimnames")=List of 2
   .. ..$ : chr [1:2] "My work is meaningful" "I feel valued"
   .. ..$ : chr "MR1"
 $ loadings_clean      : tibble [2 × 3]
   ..$ question: chr [1:2] "My work is meaningful" "I feel valued"
   ..$ factor  : chr [1:2] "MR1" "MR1"
   ..$ loading : num [1:2] 0.944 0.944
 $ factor_correlations : NULL
 $ variance_explained  : tibble [1 × 4]
   ..$ factor     : chr "MR1"
   ..$ ss_loadings: num 1.78
   ..$ prop_var   : num 0.891
   ..$ cum_var    : num 0.891
 $ communalities       : tibble [2 × 3]
   ..$ question  : chr [1:2] "My work is meaningful" "I feel valued"
   ..$ communality: num [1:2] 0.891 0.891
   ..$ uniqueness: num [1:2] 0.109 0.109
 $ fa_object           : psych::fa object
 - attr(*, "class")= chr [1:2] "survey_factors" "list"
```

**Accessing components:**
```r
# View clean loadings
print(factors$loadings_clean)
# A tibble: 2 × 3
  question              factor loading
  <chr>                 <chr>    <dbl>
1 My work is meaningful MR1      0.944
2 I feel valued         MR1      0.944

# View variance explained
print(factors$variance_explained)
# A tibble: 1 × 4
  factor ss_loadings prop_var cum_var
  <chr>        <dbl>    <dbl>   <dbl>
1 MR1           1.78    0.891   0.891

# View communalities
print(factors$communalities)
# A tibble: 2 × 3
  question              communality uniqueness
  <chr>                       <dbl>      <dbl>
1 My work is meaningful       0.891      0.109
2 I feel valued               0.891      0.109
```

**Multiple factors with varimax rotation:**
```r
factors_3 <- extract_survey_factors(survey, n_factors = 3, rotation = "varimax")
print(factors_3$variance_explained)

# A tibble: 3 × 4
  factor ss_loadings prop_var cum_var
  <chr>        <dbl>    <dbl>   <dbl>
1 MR1          0.997   0.0831  0.0831
2 MR2          0.970   0.0808  0.164
3 MR3          0.138   0.0115  0.175
```

**Automatic factor determination:**
```r
# Uses parallel analysis to determine optimal number of factors
factors_auto <- extract_survey_factors(survey)
# Determining optimal number of factors using parallel analysis...
# Parallel analysis suggests 2 factor(s)
```

---

## 8. pivot_long()

**Returns:** A tibble in long format (one row per respondent-question)

```r
# All responses
long_data <- pivot_long(survey, data_type = "all")
print(long_data)
```

**Output (data_type = "all"):**
```
# A tibble: 10 × 13
   `First Name` `Last Name` Email              Status `EMP ID` `Manager ID` `Survey Cycle Compl…¹` `Survey Cycle Se…²`
   <chr>        <chr>       <chr>              <chr>  <chr>    <chr>        <dttm>                 <dttm>
 1 Alice        Smith       alice@example.com  ACTIVE e001     m001         2024-01-15 10:30:00    2024-01-10 08:00:00
 2 Bob          Jones       bob@example.com    ACTIVE e002     m001         2024-01-16 09:15:00    2024-01-10 08:00:00
 3 Carol        White       carol@example.com  ACTIVE e003     m002         2024-01-15 14:20:00    2024-01-10 08:00:00
 4 David        Brown       david@example.com  ACTIVE e004     m002         2024-01-17 11:45:00    2024-01-10 08:00:00
 5 Eve          Davis       eve@example.com    ACTIVE e005     m003         2024-01-15 16:00:00    2024-01-10 08:00:00
 6 Alice        Smith       alice@example.com  ACTIVE e001     m001         2024-01-15 10:30:00    2024-01-10 08:00:00
 7 Bob          Jones       bob@example.com    ACTIVE e002     m001         2024-01-16 09:15:00    2024-01-10 08:00:00
 8 Carol        White       carol@example.com  ACTIVE e003     m002         2024-01-15 14:20:00    2024-01-10 08:00:00
 9 David        Brown       david@example.com  ACTIVE e004     m002         2024-01-17 11:45:00    2024-01-10 08:00:00
10 Eve          Davis       eve@example.com    ACTIVE e005     m003         2024-01-15 16:00:00    2024-01-10 08:00:00
# ℹ abbreviated names: ¹​`Survey Cycle Completion Date`, ²​`Survey Cycle Sent Date`
# ℹ 5 more variables: question <chr>, response <dbl>, comment <chr>, comment_topics <chr>, sensitive_flag <dbl>
```

**Simplified view (key columns only):**
```r
long_data %>% select(`First Name`, question, response, comment)

# A tibble: 10 × 4
   `First Name` question              response comment
   <chr>        <chr>                    <dbl> <chr>
 1 Alice        My work is meaningful        4 Great work environment
 2 Bob          My work is meaningful        3 Could be better
 3 Carol        My work is meaningful        5 Love my job
 4 David        My work is meaningful        2 Not challenging enough
 5 Eve          My work is meaningful        4 ""
 6 Alice        I feel valued                5 Appreciate the recognition
 7 Bob          I feel valued                4 ""
 8 Carol        I feel valued                5 Team is supportive
 9 David        I feel valued                3 Need more growth opportunities
10 Eve          I feel valued                4 Good benefits
```

**Output (data_type = "comments"):**
```r
comments <- pivot_long(survey, data_type = "comments")
print(comments)

# A tibble: 8 × 13
  `First Name` `Last Name` Email              Status `EMP ID` `Manager ID` question              response comment
  <chr>        <chr>       <chr>              <chr>  <chr>    <chr>        <chr>                    <dbl> <chr>
1 Alice        Smith       alice@example.com  ACTIVE e001     m001         My work is meaningful        4 Great work environment
2 Bob          Jones       bob@example.com    ACTIVE e002     m001         My work is meaningful        3 Could be better
3 Carol        White       carol@example.com  ACTIVE e003     m002         My work is meaningful        5 Love my job
4 David        Brown       david@example.com  ACTIVE e004     m002         My work is meaningful        2 Not challenging enough
5 Alice        Smith       alice@example.com  ACTIVE e001     m001         I feel valued                5 Appreciate the recognition
6 Carol        White       carol@example.com  ACTIVE e003     m002         I feel valued                5 Team is supportive
7 David        Brown       david@example.com  ACTIVE e004     m002         I feel valued                3 Need more growth opportunities
8 Eve          Davis       eve@example.com    ACTIVE e005     m003         I feel valued                4 Good benefits
# ℹ 4 more variables: comment_topics <chr>, sensitive_flag <dbl>, ...
```

**Output (data_type = "both"):**
```r
both <- pivot_long(survey, data_type = "both")
str(both)

List of 2
 $ all     : tibble [10 × 13]
 $ comments: tibble [8 × 13]

# Access each component
all_responses <- both$all
comments_only <- both$comments
```

**Without standard columns:**
```r
minimal <- pivot_long(survey, include_standard_cols = FALSE)
print(minimal)

# A tibble: 10 × 5
   question              response comment                        comment_topics       sensitive_flag
   <chr>                    <dbl> <chr>                          <chr>                         <dbl>
 1 My work is meaningful        4 Great work environment         Accomplishment                    1
 2 My work is meaningful        3 Could be better                Improvement                       2
 ...
```

---

## 9. aggregate_by_manager()

**Returns:** A tibble with one row per manager per question

```r
manager_summary <- aggregate_by_manager(survey)
print(manager_summary)
```

**Output:**
```
# A tibble: 6 × 11
  manager_id manager_name question               team_size  mean    sd n_responses n_skips n_total response_rate
  <chr>      <chr>        <chr>                      <int> <dbl> <dbl>       <int>   <int>   <int>         <dbl>
1 m001       Alice Smith  My work is meaningful          2   3.5 0.707           2       0       2             1
2 m001       Alice Smith  I feel valued                  2   4.5 0.707           2       0       2             1
3 m002       Bob Jones    My work is meaningful          2   3.5 2.12            2       0       2             1
4 m002       Bob Jones    I feel valued                  2   4   1.41            2       0       2             1
5 m003       Carol White  My work is meaningful          1   4   NA              1       0       1             1
6 m003       Carol White  I feel valued                  1   4   NA              1       0       1             1
```

**With full organizational tree:**
```r
# Include all indirect reports in team aggregation
manager_full <- aggregate_by_manager(survey, full_tree = TRUE)
```

**With minimum team size filter:**
```r
# Only include managers with teams of 3 or more
manager_filtered <- aggregate_by_manager(survey, min_team_size = 3)
```

---

## 11. analyze_by_attributes()

**Returns:** A tibble with one row per demographic-group-question combination

```r
attr_analysis <- analyze_by_attributes(
  survey,
  attribute_file = "tests/testthat/fixtures/employee_attributes.csv",
  scale_points = 5,
  attribute_cols = c("Department", "Gender"),
  min_group_size = 10
)
```

**Output Structure:**
```r
# A tibble: 180 × 13
   Department  Gender group_size question           mean    sd n_responses n_skips
   <chr>       <chr>       <int> <chr>             <dbl> <dbl>       <int>   <int>
 1 Engineering Female        937 My work is mean…   3.52  1.10         894      43
 2 Engineering Female        937 I feel valued      3.59  1.09         893      44
 3 Engineering Female        937 I have opportun…   3.59  1.07         906      31
 4 Engineering Male          974 My work is mean…   3.59  1.07         919      55
 5 Engineering Male          974 I feel valued      3.57  1.11         920      54
# … with 175 more rows, and 5 more variables: n_total <int>,
#   response_rate <dbl>, pct_favorable <dbl>, pct_neutral <dbl>,
#   pct_unfavorable <dbl>
```

**Column Descriptions:**
- **Demographic columns** (e.g., Department, Gender): The grouping variables specified in `attribute_cols`
- **group_size**: Number of employees in this demographic combination
- **question**: The question text
- **mean, sd, n_responses, n_skips, n_total, response_rate**: Standard survey metrics from `summarize_survey()`
- **pct_favorable, pct_neutral, pct_unfavorable**: Favorability percentages based on scale

**Usage Notes:**
- Only attribute groups with at least `min_group_size` employees are included
- The attribute file must contain the `emp_id_col` and all `attribute_cols`
- Returns empty tibble with warning if no groups meet the minimum size threshold
- Can analyze any number of demographic variables simultaneously

**Example: Find lowest scoring attribute groups**
```r
attr_analysis %>%
  filter(question == "My work is meaningful") %>%
  arrange(mean) %>%
  select(Department, Gender, group_size, mean, pct_favorable)
```

---

## 12. analyze_attrition()

**Returns:** A tibble with one row per question-time period combination

```r
attrition <- analyze_attrition(
  survey,
  attrition_file = "tests/testthat/fixtures/large_attrition.csv",
  emp_id_col = "EMP ID",
  term_date_col = "Termination Date",
  scale_points = 5,
  time_periods = c(30, 90, 180)
)
```

**Output Structure:**
```r
# A tibble: 36 × 7
   question                  days favorable_n favorable_attrition unfavorable_n
   <chr>                    <dbl>       <int>               <dbl>         <int>
 1 My work is meaningful       30        2251              0.0262          1498
 2 My work is meaningful       90        2251              0.0697          1498
 3 My work is meaningful      180        2251              0.1113          1498
 4 I feel valued               30        2274              0.0281          1506
 5 I feel valued               90        2274              0.0665          1506
# … with 31 more rows, and 2 more variables: unfavorable_attrition <dbl>,
#   attrition_ratio <dbl>
```

**Column Descriptions:**
- **question**: The survey question text
- **days**: Time period (days after survey completion) for attrition calculation
- **favorable_n**: Number of employees who gave favorable responses
- **favorable_attrition**: Proportion of favorable responders who left within the time period
- **unfavorable_n**: Number of employees who gave unfavorable responses
- **unfavorable_attrition**: Proportion of unfavorable responders who left within the time period
- **attrition_ratio**: Risk ratio (unfavorable_attrition / favorable_attrition)
  - Values > 1 indicate higher attrition among unfavorable responders
  - Values < 1 indicate higher attrition among favorable responders
  - Inf indicates no attrition in favorable group but attrition in unfavorable group
  - NA indicates insufficient data for comparison

**Usage Notes:**
- Responses are classified as favorable/unfavorable based on the scale's favorability map
- Neutral responses are excluded from the analysis
- The attribute file must contain `emp_id_col` and `term_date_col`
- Termination dates are automatically parsed from common formats
- Attrition is calculated from survey completion date to termination date

**Example: Find questions with highest 90-day attrition risk**
```r
attrition %>%
  filter(days == 90) %>%
  arrange(desc(attrition_ratio)) %>%
  select(question, favorable_attrition, unfavorable_attrition, attrition_ratio)
```

---

## Summary of Return Types

| Function | Return Type | Dimensions (typical) |
|----------|-------------|----------------------|
| `read_glint_survey()` | List (glint_survey) | data: n×(8+4q), metadata: list |
| `extract_questions()` | Tibble | q×5 |
| `summarize_survey()` | Tibble | q×10 |
| `get_response_dist()` | Tibble | q×(1+2v) |
| `compare_cycles()` | Tibble | (q×c)×13 |
| `get_correlations()` | Tibble or Matrix | q²×5 or q×q |
| `extract_survey_factors()` | List (survey_factors) | list with 6 components |
| `pivot_long()` | Tibble or List | (n×q)×13 or list of 2 tibbles |
| `aggregate_by_manager()` | Tibble | (m×q)×14 |
| `analyze_by_attributes()` | Tibble | (d×q)×(13+k) |
| `analyze_attrition()` | Tibble | (q×t)×7 |

**Legend:**
- n = number of respondents
- q = number of questions
- m = number of managers
- c = number of cycles
- v = number of unique response values
- d = number of demographic group combinations
- k = number of demographic columns
- t = number of time periods

---

## Function Name Changes

**Previous function names (deprecated):**
- `analyze_question()` → Use `summarize_survey()` with specific question
- `analyze_survey()` → Use `summarize_survey()` or `get_response_dist()`
- `pivot_responses_long()` → Use `pivot_long(data_type = "all")`
- `pivot_comments_long()` → Use `pivot_long(data_type = "comments")`

**New unified function:**
- `pivot_long()` - Replaces both pivot_responses_long() and pivot_comments_long()
  - Use `data_type = "all"` for all responses
  - Use `data_type = "comments"` for comments only
  - Use `data_type = "both"` to get both as separate tibbles

**Removed functions:**
- `compare_managers()` - Removed from package
