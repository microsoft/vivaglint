test_that("read_glint_survey reads valid CSV correctly", {
  survey <- read_glint_survey("fixtures/sample_survey.csv", emp_id_col = "EXID")

  expect_s3_class(survey, "glint_survey")
  expect_type(survey, "list")
  expect_named(survey, c("data", "metadata"))
  expect_s3_class(survey$data, "tbl_df")
  expect_equal(nrow(survey$data), 5)
})

test_that("read_glint_survey validates standard columns", {
  # Create a temporary CSV with missing columns
  temp_file <- tempfile(fileext = ".csv")
  writeLines(
    "Question1,Question1_COMMENT,Question1_COMMENT_TOPICS,Question1_SENSITIVE_COMMENT_FLAG,First Name,Last Name\n1,comment,topic,1,John,Doe",
    temp_file
  )

  expect_error(
    read_glint_survey(temp_file, emp_id_col = "EXID"),
    "Missing required standard column"
  )

  unlink(temp_file)
})

test_that("read_glint_survey parses dates correctly", {
  survey <- read_glint_survey("fixtures/sample_survey.csv", emp_id_col = "EXID")

  expect_s3_class(survey$data$`Survey Cycle Completion Date`, "POSIXct")
  expect_s3_class(survey$data$`Survey Cycle Sent Date`, "POSIXct")
})

test_that("extract_questions returns correct structure", {
  survey <- read_glint_survey("fixtures/sample_survey.csv", emp_id_col = "EXID")
  questions <- extract_questions(survey)

  expect_s3_class(questions, "tbl_df")
  expect_named(questions, c("question", "response_col", "comment_col", "topics_col", "flag_col"))
  expect_equal(nrow(questions), 2)
  expect_true("My work is meaningful" %in% questions$question)
  expect_true("I feel valued" %in% questions$question)
})

test_that("read_glint_survey throws error for missing file", {
  expect_error(
    read_glint_survey("nonexistent_file.csv", emp_id_col = "EXID"),
    "File not found"
  )
})

test_that("validate_glint_structure detects incomplete question sets", {
  # Create a temporary CSV with incomplete question columns
  temp_file <- tempfile(fileext = ".csv")
  writeLines(
    paste(
      "Question1,Question1_COMMENT,First Name,Last Name,Email,Status,EXID,Manager ID,",
      "Survey Cycle Completion Date,Survey Cycle Sent Date\n",
      "1,comment,John,Doe,john@example.com,ACTIVE,e001,m001,15-01-2024 10:30,10-01-2024 08:00",
      sep = ""
    ),
    temp_file
  )

  expect_error(
    read_glint_survey(temp_file, emp_id_col = "EXID"),
    "Incomplete question column"
  )

  unlink(temp_file)
})
