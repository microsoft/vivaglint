test_that("summarize_survey calculates metrics for single question", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- summarize_survey(survey, questions = "My work is meaningful")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$question, "My work is meaningful")
  expect_equal(result$n_responses, 5)
  expect_equal(result$n_skips, 0)
  expect_equal(result$n_total, 5)
  expect_equal(result$response_rate, 1.0)
  expect_true(!is.na(result$mean))
  expect_true(!is.na(result$sd))
})

test_that("summarize_survey calculates metrics for multiple specific questions", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- summarize_survey(survey, questions = c("My work is meaningful", "I feel valued"))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$n_responses, c(5, 5))
  expect_equal(result$n_total, c(5, 5))
})

test_that("summarize_survey does not include value distributions", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- summarize_survey(survey, questions = "My work is meaningful")

  expect_false("value_counts" %in% names(result))
  expect_false("value_percents" %in% names(result))
  expect_true("mean" %in% names(result))
  expect_true("sd" %in% names(result))
})

test_that("summarize_survey analyzes all questions", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- summarize_survey(survey)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(c("My work is meaningful", "I feel valued") %in% result$question))
})

test_that("get_response_dist returns distribution with expanded columns", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- get_response_dist(survey)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_false("value_counts" %in% names(result))
  expect_false("value_percents" %in% names(result))
  expect_true(any(grepl("^count_", names(result))))
  expect_true(any(grepl("^pct_", names(result))))
})

test_that("get_response_dist works for specific questions", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- get_response_dist(survey, questions = "My work is meaningful")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$question, "My work is meaningful")
  expect_true(any(grepl("^count_", names(result))))
  expect_true(any(grepl("^pct_", names(result))))
})

test_that("get_response_dist throws error for non-existent question", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  expect_error(
    get_response_dist(survey, questions = "Non-existent question"),
    "Question\\(s\\) not found"
  )
})

test_that("summarize_survey throws error for non-existent question", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  expect_error(
    summarize_survey(survey, questions = "Non-existent question"),
    "Question\\(s\\) not found"
  )
})

test_that("compare_cycles requires at least two surveys", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  expect_error(
    compare_cycles(survey),
    "At least two surveys are required"
  )
})

test_that("get_correlations returns long format by default", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- get_correlations(survey)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)  # 2 questions x 2 questions
  expect_true(all(c("question1", "question2", "correlation", "p_value", "n") %in% names(result)))

  # Check that self-correlations have p-value of 0
  self_corr <- result[result$question1 == result$question2, ]
  expect_equal(self_corr$p_value, c(0, 0))

  # Check that non-self correlations have valid p-values
  non_self_corr <- result[result$question1 != result$question2, ]
  expect_true(all(!is.na(non_self_corr$p_value)))
  expect_true(all(non_self_corr$p_value >= 0 & non_self_corr$p_value <= 1))
})

test_that("get_correlations returns matrix format", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- get_correlations(survey, format = "matrix")

  expect_true(is.matrix(result))
  expect_equal(nrow(result), 2)  # 2 questions
  expect_equal(ncol(result), 2)  # 2 questions
  expect_equal(rownames(result), c("My work is meaningful", "I feel valued"))
})

test_that("get_correlations supports different correlation methods", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  spearman <- get_correlations(survey, method = "spearman")
  pearson <- get_correlations(survey, method = "pearson")
  kendall <- get_correlations(survey, method = "kendall")

  expect_s3_class(spearman, "tbl_df")
  expect_s3_class(pearson, "tbl_df")
  expect_s3_class(kendall, "tbl_df")
})

test_that("get_correlations validates method parameter", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  expect_error(
    get_correlations(survey, method = "invalid"),
    "method must be one of: 'spearman', 'pearson', or 'kendall'"
  )
})

test_that("get_correlations validates format parameter", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  expect_error(
    get_correlations(survey, format = "invalid"),
    "format must be one of: 'long' or 'matrix'"
  )
})

test_that("get_correlations diagonal is 1.0 in matrix format", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- get_correlations(survey, format = "matrix")

  expect_equal(unname(diag(result)), c(1.0, 1.0))
})

test_that("extract_survey_factors requires psych package", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  # Only test if psych is not installed
  if (!requireNamespace("psych", quietly = TRUE)) {
    expect_error(
      extract_survey_factors(survey, n_factors = 1),
      "Package 'psych' is required"
    )
  } else {
    # If psych is installed, test basic functionality
    result <- extract_survey_factors(survey, n_factors = 1)

    expect_s3_class(result, "survey_factors")
    expect_true("loadings" %in% names(result))
    expect_true("loadings_clean" %in% names(result))
    expect_true("variance_explained" %in% names(result))
    expect_true("communalities" %in% names(result))
    expect_true("fa_object" %in% names(result))
  }
})

test_that("extract_survey_factors validates rotation parameter", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  skip_if_not_installed("psych")

  expect_error(
    extract_survey_factors(survey, n_factors = 1, rotation = "invalid"),
    "rotation must be one of"
  )
})

test_that("extract_survey_factors validates fm parameter", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  skip_if_not_installed("psych")

  expect_error(
    extract_survey_factors(survey, n_factors = 1, fm = "invalid"),
    "fm must be one of"
  )
})

test_that("extract_survey_factors works with different rotations", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  skip_if_not_installed("psych")

  oblimin <- extract_survey_factors(survey, n_factors = 1, rotation = "oblimin")
  varimax <- extract_survey_factors(survey, n_factors = 1, rotation = "varimax")

  expect_s3_class(oblimin, "survey_factors")
  expect_s3_class(varimax, "survey_factors")
  expect_s3_class(oblimin$loadings_clean, "tbl_df")
  expect_s3_class(varimax$loadings_clean, "tbl_df")
})
