test_that("analyze_by_attributes requires attribute file", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  expect_error(
    analyze_by_attributes(
      survey,
      attribute_file = "nonexistent_file.csv",
      scale_points = 5,
      attribute_cols = "Department"
    ),
    "Attribute file not found"
  )
})

test_that("analyze_by_attributes validates emp_id_col", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  # Create test attribute file
  attr_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    dplyr::tibble(
      `Wrong ID Column` = c("e001", "e002", "e003", "e004", "e005"),
      Department = c("Sales", "Sales", "Engineering", "Engineering", "HR")
    ),
    attr_file
  )

  expect_error(
    analyze_by_attributes(
      survey,
      attribute_file = attr_file,
      scale_points = 5,
      attribute_cols = "Department",
      emp_id_col = "EMP ID"
    ),
    "Column 'EMP ID' not found in attribute file"
  )

  unlink(attr_file)
})

test_that("analyze_by_attributes validates attribute_cols", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  # Create test attribute file
  attr_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    dplyr::tibble(
      `EMP ID` = c("e001", "e002", "e003", "e004", "e005"),
      Department = c("Sales", "Sales", "Engineering", "Engineering", "HR")
    ),
    attr_file
  )

  expect_error(
    analyze_by_attributes(
      survey,
      attribute_file = attr_file,
      scale_points = 5,
      attribute_cols = c("Department", "Gender")
    ),
    "Attribute column\\(s\\) not found in attribute file: Gender"
  )

  unlink(attr_file)
})

test_that("analyze_by_attributes validates scale_points", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  # Create test attribute file
  attr_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    dplyr::tibble(
      `EMP ID` = c("e001", "e002", "e003", "e004", "e005"),
      Department = c("Sales", "Sales", "Engineering", "Engineering", "HR")
    ),
    attr_file
  )

  expect_error(
    analyze_by_attributes(
      survey,
      attribute_file = attr_file,
      scale_points = 12,
      attribute_cols = "Department"
    ),
    "scale_points must be an integer between 2 and 11"
  )

  unlink(attr_file)
})

test_that("analyze_by_attributes works with single attribute", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  # Create test attribute file
  attr_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    dplyr::tibble(
      `EMP ID` = c("e001", "e002", "e003", "e004", "e005"),
      Department = c("Sales", "Sales", "Engineering", "Engineering", "Engineering")
    ),
    attr_file
  )

  result <- analyze_by_attributes(
    survey,
    attribute_file = attr_file,
    scale_points = 5,
    attribute_cols = "Department",
    min_group_size = 2
  )

  expect_s3_class(result, "tbl_df")
  expect_true("Department" %in% names(result))
  expect_true("group_size" %in% names(result))
  expect_true("question" %in% names(result))
  expect_true("mean" %in% names(result))
  expect_true("pct_favorable" %in% names(result))

  # Should have 2 departments × 2 questions = 4 rows
  expect_equal(nrow(result), 4)

  # Check group sizes
  expect_true(all(result$group_size >= 2))

  unlink(attr_file)
})

test_that("analyze_by_attributes works with multiple attributes", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  # Create test attribute file
  attr_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    dplyr::tibble(
      `EMP ID` = c("e001", "e002", "e003", "e004", "e005"),
      Department = c("Sales", "Sales", "Engineering", "Engineering", "HR"),
      Gender = c("Male", "Female", "Male", "Female", "Male")
    ),
    attr_file
  )

  result <- analyze_by_attributes(
    survey,
    attribute_file = attr_file,
    scale_points = 5,
    attribute_cols = c("Department", "Gender"),
    min_group_size = 1
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("Department", "Gender") %in% names(result)))
  expect_true("group_size" %in% names(result))

  # Should have multiple attribute combinations
  expect_gt(nrow(result), 4)

  unlink(attr_file)
})

test_that("analyze_by_attributes respects min_group_size", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  # Create test attribute file with one small group
  attr_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    dplyr::tibble(
      `EMP ID` = c("e001", "e002", "e003", "e004", "e005"),
      Department = c("Sales", "Sales", "Sales", "Sales", "HR")
    ),
    attr_file
  )

  result <- analyze_by_attributes(
    survey,
    attribute_file = attr_file,
    scale_points = 5,
    attribute_cols = "Department",
    min_group_size = 2
  )

  # Should only include Sales (4 employees), not HR (1 employee)
  expect_equal(unique(result$Department), "Sales")
  expect_true(all(result$group_size >= 2))

  unlink(attr_file)
})

test_that("analyze_by_attributes warns when no groups meet threshold", {
  survey <- read_glint_survey("fixtures/sample_survey.csv")

  # Create test attribute file with all small groups
  attr_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    dplyr::tibble(
      `EMP ID` = c("e001", "e002", "e003", "e004", "e005"),
      Department = c("Sales", "Engineering", "HR", "Marketing", "Finance")
    ),
    attr_file
  )

  expect_warning(
    result <- analyze_by_attributes(
      survey,
      attribute_file = attr_file,
      scale_points = 5,
      attribute_cols = "Department",
      min_group_size = 5
    ),
    "No attribute groups meet the minimum size threshold"
  )

  expect_equal(nrow(result), 0)

  unlink(attr_file)
})
