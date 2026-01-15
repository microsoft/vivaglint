# vivaglint Package Demo
# Run this script to see the package in action

library(vivaglint)

# 1. Load survey data
cat("=== Loading Survey Data ===\n")
survey <- read_glint_survey("tests/testthat/fixtures/sample_survey.csv")
cat("Loaded", survey$metadata$n_questions, "questions from", survey$metadata$n_respondents, "respondents\n\n")

# 2. List all questions
cat("=== Questions in Survey ===\n")
questions <- extract_questions(survey)
print(questions$question)
cat("\n")

# 3. Analyze all questions
cat("=== Question Analysis ===\n")
analysis <- summarize_survey(survey)
print(analysis)
cat("\n")

# 3b. Get response distributions
cat("=== Response Distributions ===\n")
distributions <- get_response_dist(survey)
print(distributions)
cat("\n")

# 4. Analyze a specific question
cat("=== Detailed Analysis: My work is meaningful ===\n")
detailed <- summarize_survey(survey, questions = "My work is meaningful")
cat("Mean:", detailed$mean, "\n")
cat("SD:", detailed$sd, "\n")
cat("Response Rate:", detailed$response_rate * 100, "%\n")
cat("Responses:", detailed$n_responses, "| Skips:", detailed$n_skips, "\n")
cat("\n")

# 5. Extract comments
cat("=== Comments ===\n")
comments <- pivot_long(survey, data_type = "comments")
cat("Total non-empty comments:", nrow(comments), "\n")
cat("Sample comments:\n")
print(head(comments[, c("First Name", "question", "comment", "comment_topics")], 3))
cat("\n")

# 6. Reshape to long format
cat("=== Long Format Data ===\n")
long_data <- pivot_long(survey, data_type = "all")
cat("Dimensions:", nrow(long_data), "rows x", ncol(long_data), "columns\n")
cat("Sample:\n")
print(head(long_data[, c("First Name", "question", "response")], 5))
cat("\n")

# 7. Get both formats at once
cat("=== Using data_type='both' ===\n")
both <- pivot_long(survey, data_type = "both")
cat("All responses:", nrow(both$all), "rows\n")
cat("Comments only:", nrow(both$comments), "rows\n")
cat("\n")

# 8. Calculate correlations
cat("=== Question Correlations ===\n")
correlations_long <- get_correlations(survey)
cat("Long format:\n")
print(correlations_long)
cat("\n")

cat("Matrix format:\n")
correlations_matrix <- get_correlations(survey, format = "matrix")
print(correlations_matrix)
cat("\n")

# 9. Factor analysis (if psych package is available)
if (requireNamespace("psych", quietly = TRUE)) {
  cat("=== Factor Analysis ===\n")
  factors <- extract_survey_factors(survey, n_factors = 1, rotation = "oblimin")
  cat("Clean loadings (|loading| >= 0.3):\n")
  print(factors$loadings_clean)
  cat("\n")
  cat("Variance explained:\n")
  print(factors$variance_explained)
  cat("\n")
}

cat("=== Demo Complete! ===\n")
cat("All functions working correctly.\n")
