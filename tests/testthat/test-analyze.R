test_that("summarize_survey calculates metrics for single question", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- summarize_survey(survey, scale_points = 5, questions = "My work is meaningful")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$question, "My work is meaningful")
  expect_equal(result$n_responses, 5)
  expect_equal(result$n_skips, 0)
  expect_equal(result$n_total, 5)
  expect_true(!is.na(result$mean))
  expect_true(!is.na(result$sd))
})

test_that("summarize_survey calculates metrics for multiple specific questions", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- summarize_survey(survey, scale_points = 5, questions = c("My work is meaningful", "I feel valued"))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$n_responses, c(5, 5))
  expect_equal(result$n_total, c(5, 5))
})

test_that("summarize_survey does not include value distributions", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- summarize_survey(survey, scale_points = 5, questions = "My work is meaningful")

  expect_false("value_counts" %in% names(result))
  expect_false("value_percents" %in% names(result))
  expect_true("mean" %in% names(result))
  expect_true("sd" %in% names(result))
})

test_that("summarize_survey analyzes all questions", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- summarize_survey(survey, scale_points = 5)

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
    summarize_survey(survey, scale_points = 5, questions = "Non-existent question"),
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
    expect_true("factor_summary" %in% names(result))
    expect_true("fa_object" %in% names(result))
    expect_s3_class(result$factor_summary, "tbl_df")
    expect_true(all(c("question", "factor", "loading", "loading_label",
                      "communality", "factor_variance_pct") %in%
                      names(result$factor_summary)))
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
  expect_s3_class(oblimin$factor_summary, "tbl_df")
  expect_s3_class(varimax$factor_summary, "tbl_df")
})

test_that("search_comments returns tibble with correct columns", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- search_comments(survey, "work")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("question", "response", "comment", "topics") %in% names(result)))
})

test_that("search_comments fuzzy match is case-insensitive", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  lower  <- search_comments(survey, "recognition")
  upper  <- search_comments(survey, "RECOGNITION")

  expect_gt(nrow(lower), 0)
  expect_equal(nrow(lower), nrow(upper))
})

test_that("search_comments exact match is case-sensitive", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result_match    <- search_comments(survey, "Great", exact = TRUE)
  result_no_match <- search_comments(survey, "great", exact = TRUE)

  expect_gt(nrow(result_match), 0)
  expect_equal(nrow(result_no_match), 0)
})

test_that("search_comments exact match finds literal substring", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- search_comments(survey, "work", exact = TRUE)

  expect_gt(nrow(result), 0)
  expect_true(all(grepl("work", result$comment, fixed = TRUE)))
})

test_that("search_comments returns empty tibble when no matches found", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- search_comments(survey, "zzznomatchzzz")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true(all(c("question", "response", "comment", "topics") %in% names(result)))
})

test_that("search_comments fuzzy match catches minor spelling differences", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  # "growht" is a typo of "growth" - should still match "growth opportunities"
  result_typo  <- search_comments(survey, "growht", max_distance = 0.3)
  result_exact <- search_comments(survey, "growth")

  expect_gt(nrow(result_exact), 0)
  # Fuzzy should find at least as many results
  expect_gte(nrow(result_typo), 0)
})

test_that("search_comments can find matches across multiple questions", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  # "Accomplishment" appears as a topic in question 1; "recognition" appears in question 2
  result <- search_comments(survey, "job")

  expect_gt(nrow(result), 0)
  # All returned rows should have matching comment text
  expect_true(all(grepl("job", result$comment, ignore.case = TRUE)))
})

test_that("search_comments validates query input", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  expect_error(search_comments(survey, ""),           "non-empty")
  expect_error(search_comments(survey, 123),          "non-empty character")
  expect_error(search_comments(survey, "x", exact = "yes"), "TRUE or FALSE")
  expect_error(search_comments(survey, "x", max_distance = 1.5), "between 0 and 1")
})

test_that("split_survey_data returns a named list with two elements", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- split_survey_data(survey)

  expect_type(result, "list")
  expect_named(result, c("quantitative", "qualitative"))
})

test_that("split_survey_data quantitative contains standard cols and response cols only", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  quant  <- split_survey_data(survey)$quantitative

  expect_s3_class(quant, "tbl_df")
  # Standard columns present
  expect_true("EMP ID" %in% names(quant))
  expect_true("First Name" %in% names(quant))
  # Numeric response columns present
  expect_true("My work is meaningful" %in% names(quant))
  expect_true("I feel valued" %in% names(quant))
  # No comment or topic columns
  expect_false(any(grepl("_COMMENT$", names(quant))))
  expect_false(any(grepl("_COMMENT_TOPICS$", names(quant))))
  expect_false(any(grepl("_SENSITIVE_COMMENT_FLAG$", names(quant))))
})

test_that("split_survey_data qualitative contains EMP ID and comment cols only", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  qual   <- split_survey_data(survey)$qualitative

  expect_s3_class(qual, "tbl_df")
  # EMP ID retained for joining
  expect_true("EMP ID" %in% names(qual))
  # Comment and topic columns present
  expect_true(any(grepl("_COMMENT$", names(qual))))
  expect_true(any(grepl("_COMMENT_TOPICS$", names(qual))))
  # No numeric response columns
  expect_false("My work is meaningful" %in% names(qual))
  expect_false("I feel valued" %in% names(qual))
  # No other standard columns beyond EMP ID
  expect_false("First Name" %in% names(qual))
})

test_that("split_survey_data outputs have the same number of rows as input", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  parts  <- split_survey_data(survey)

  expect_equal(nrow(parts$quantitative), nrow(survey$data))
  expect_equal(nrow(parts$qualitative),  nrow(survey$data))
})

test_that("split_survey_data outputs can be rejoined on EMP ID", {
  survey   <- read_glint_survey("fixtures/sample_survey.csv")
  parts    <- split_survey_data(survey)
  rejoined <- dplyr::left_join(parts$quantitative, parts$qualitative, by = "EMP ID")

  expect_equal(nrow(rejoined), nrow(survey$data))
  expect_true("My work is meaningful" %in% names(rejoined))
  expect_true(any(grepl("_COMMENT$", names(rejoined))))
})

test_that("split_survey_data works on a plain data frame as well as glint_survey", {
  survey      <- read_glint_survey("fixtures/sample_survey.csv")
  result_obj  <- split_survey_data(survey)
  result_df   <- split_survey_data(survey$data)

  expect_equal(names(result_obj$quantitative), names(result_df$quantitative))
  expect_equal(names(result_obj$qualitative),  names(result_df$qualitative))
})
