test_that("get_question_stem removes suffixes correctly", {
  expect_equal(get_question_stem("Question text"), "Question text")
  expect_equal(get_question_stem("Question text_COMMENT"), "Question text")
  expect_equal(get_question_stem("Question text_COMMENT_TOPICS"), "Question text")
  expect_equal(get_question_stem("Question text_SENSITIVE_COMMENT_FLAG"), "Question text")
})

test_that("parse_comment_topics splits comma-separated topics", {
  topics <- c("Topic1, Topic2, Topic3", "Topic4", NA, "")
  result <- parse_comment_topics(topics, return_format = "list")

  expect_type(result, "list")
  expect_length(result, 4)
  expect_equal(result[[1]], c("Topic1", "Topic2", "Topic3"))
  expect_equal(result[[2]], "Topic4")
  expect_length(result[[3]], 0)
  expect_length(result[[4]], 0)
})

test_that("parse_comment_topics returns tidy format", {
  topics <- c("Topic1, Topic2", "Topic3")
  result <- parse_comment_topics(topics, return_format = "tidy")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("index", "topic"))
  expect_equal(nrow(result), 3)
})

test_that("parse_comment_topics throws error for invalid format", {
  expect_error(
    parse_comment_topics(c("Topic1"), return_format = "invalid"),
    "return_format must be either"
  )
})

test_that("get_standard_columns returns expected columns", {
  cols <- get_standard_columns(emp_id_col = "EXID")

  expect_type(cols, "character")
  expect_length(cols, 8)
  expect_true("First Name" %in% cols)
  expect_true("EXID" %in% cols)
  expect_true("Manager ID" %in% cols)
  expect_true("Survey Cycle Completion Date" %in% cols)
})
