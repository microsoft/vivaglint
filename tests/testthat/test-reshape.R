test_that("pivot_long with data_type='all' creates correct format", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  long_data <- pivot_long(survey, data_type = "all")

  expect_s3_class(long_data, "tbl_df")
  expect_true("question" %in% names(long_data))
  expect_true("response" %in% names(long_data))
  expect_true("comment" %in% names(long_data))
  expect_equal(nrow(long_data), 10)  # 5 respondents x 2 questions
})

test_that("pivot_long includes standard columns by default", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  long_data <- pivot_long(survey)

  expect_true("EMP ID" %in% names(long_data))
  expect_true("Manager ID" %in% names(long_data))
  expect_true("First Name" %in% names(long_data))
})

test_that("pivot_long can exclude standard columns", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  long_data <- pivot_long(survey, include_standard_cols = FALSE)

  expect_false("EMP ID" %in% names(long_data))
  expect_false("Manager ID" %in% names(long_data))
  expect_true("question" %in% names(long_data))
})

test_that("pivot_long with data_type='comments' filters empty comments by default", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  comments <- pivot_long(survey, data_type = "comments")

  expect_s3_class(comments, "tbl_df")
  expect_true(all(!is.na(comments$comment) & comments$comment != ""))
})

test_that("pivot_long with data_type='comments' can include empty comments", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  all_comments <- pivot_long(survey, data_type = "comments", include_empty = TRUE)

  expect_s3_class(all_comments, "tbl_df")
  expect_equal(nrow(all_comments), 10)  # 5 respondents x 2 questions
})

test_that("pivot_long includes respondent metadata", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  comments <- pivot_long(survey, data_type = "comments")

  expect_true("First Name" %in% names(comments))
  expect_true("Last Name" %in% names(comments))
  expect_true("Email" %in% names(comments))
  expect_true("EMP ID" %in% names(comments))
})

test_that("pivot_long with data_type='both' returns list with two tibbles", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")
  result <- pivot_long(survey, data_type = "both")

  expect_type(result, "list")
  expect_true("all" %in% names(result))
  expect_true("comments" %in% names(result))
  expect_s3_class(result$all, "tbl_df")
  expect_s3_class(result$comments, "tbl_df")
  expect_equal(nrow(result$all), 10)  # All responses
  expect_true(nrow(result$comments) <= 10)  # Filtered comments
})

test_that("pivot_long validates data_type parameter", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  expect_error(
    pivot_long(survey, data_type = "invalid"),
    "data_type must be one of: 'all', 'comments', or 'both'"
  )
})
