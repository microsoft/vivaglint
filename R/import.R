#' Read Viva Glint Survey Data
#'
#' Reads a Viva Glint survey export CSV file with automatic validation and parsing.
#' This function validates the file structure, parses dates, and organizes the data
#' into a structured format ready for analysis.
#'
#' @param file_path Character string specifying the path to the CSV file
#' @param encoding Character string specifying file encoding (default: "UTF-8")
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{data}{A tibble containing the survey data}
#'     \item{metadata}{A list with survey metadata including standard columns and question list}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#' head(survey$data)
#' survey$metadata$questions
#' }
read_glint_survey <- function(file_path, encoding = "UTF-8") {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(sprintf(
      "File not found: '%s'\nPlease check that the file path is correct.",
      file_path
    ))
  }

  # Read the CSV file
  data <- tryCatch(
    {
      readr::read_csv(
        file_path,
        locale = readr::locale(encoding = encoding),
        show_col_types = FALSE
      )
    },
    error = function(e) {
      stop(sprintf(
        "Error reading CSV file: %s\nPlease ensure the file is a valid CSV format.",
        e$message
      ))
    }
  )

  # Validate the data structure
  validate_glint_structure(data)

  # Get standard column names
  standard_cols <- get_standard_columns()

  # Parse date columns
  date_cols <- c("Survey Cycle Completion Date", "Survey Cycle Sent Date")
  for (col in date_cols) {
    if (col %in% names(data)) {
      data[[col]] <- tryCatch(
        {
          lubridate::dmy_hm(data[[col]])
        },
        error = function(e) {
          stop(sprintf(
            "Error parsing date column '%s': %s\nExpected format: DD-MM-YYYY HH:MM (e.g., '26-03-2024 09:34')",
            col, e$message
          ))
        }
      )
    }
  }

  # Extract question information
  questions_df <- extract_questions(data)

  # Create metadata
  metadata <- list(
    standard_columns = standard_cols,
    questions = questions_df,
    n_respondents = nrow(data),
    n_questions = nrow(questions_df),
    file_path = file_path
  )

  # Return structured list
  result <- list(
    data = data,
    metadata = metadata
  )

  class(result) <- c("glint_survey", "list")
  return(result)
}


#' Validate Glint Data Structure
#'
#' Internal function to validate that a data frame conforms to the expected
#' Viva Glint export structure. Throws detailed, human-readable errors if validation fails.
#'
#' @param data A data frame to validate
#'
#' @return NULL (invisibly) if validation passes, otherwise throws an error
#'
#' @keywords internal
validate_glint_structure <- function(data) {
  # Check for required standard columns
  standard_cols <- get_standard_columns()
  missing_cols <- setdiff(standard_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Missing required standard column(s): %s\n\nYour CSV file must contain all of the following standard Viva Glint columns:\n  %s\n\nPlease ensure you are using a complete Viva Glint survey export.",
      paste0("'", missing_cols, "'", collapse = ", "),
      paste0("  - ", standard_cols, collapse = "\n")
    ))
  }

  # Identify question columns (non-standard columns)
  question_cols <- setdiff(names(data), standard_cols)

  if (length(question_cols) == 0) {
    stop(
      "No question columns found in the data.\n\nA Viva Glint export should contain at least one question with its associated columns:\n  - [Question Text]\n  - [Question Text]_COMMENT\n  - [Question Text]_COMMENT_TOPICS\n  - [Question Text]_SENSITIVE_COMMENT_FLAG\n\nPlease check that you are using a complete survey export file."
    )
  }

  # Get unique question stems
  question_stems <- unique(sapply(question_cols, get_question_stem))

  # Validate that each question has all four required columns
  expected_suffixes <- c("", "_COMMENT", "_COMMENT_TOPICS", "_SENSITIVE_COMMENT_FLAG")
  orphaned_cols <- character(0)
  incomplete_questions <- character(0)

  for (stem in question_stems) {
    expected_cols <- paste0(stem, expected_suffixes)
    found_cols <- expected_cols %in% names(data)

    # If we have some but not all columns for this question, it's incomplete
    if (any(found_cols) && !all(found_cols)) {
      missing_for_q <- expected_cols[!found_cols]
      incomplete_questions <- c(incomplete_questions, sprintf(
        "  - Question '%s' is missing: %s",
        stem,
        paste0("'", basename(missing_for_q), "'", collapse = ", ")
      ))
    }
  }

  # Check for orphaned columns (columns that don't fit the pattern)
  for (col in question_cols) {
    stem <- get_question_stem(col)
    expected_cols <- paste0(stem, expected_suffixes)

    # If this column's stem would create columns that don't all exist, it might be orphaned
    # But only flag it if it's truly orphaned (not part of a valid set)
    if (!all(expected_cols %in% names(data))) {
      # Check if this is actually an orphaned column
      if (!col %in% paste0(stem, expected_suffixes[1])) {
        # This is a suffix column without its base
        if (!paste0(stem, expected_suffixes[1]) %in% names(data)) {
          orphaned_cols <- c(orphaned_cols, col)
        }
      }
    }
  }

  # Report errors if found
  error_messages <- character(0)

  if (length(incomplete_questions) > 0) {
    error_messages <- c(
      error_messages,
      sprintf(
        "Incomplete question column sets found:\n%s\n\nEach question must have all four columns:\n  - [Question Text]\n  - [Question Text]_COMMENT\n  - [Question Text]_COMMENT_TOPICS\n  - [Question Text]_SENSITIVE_COMMENT_FLAG",
        paste(incomplete_questions, collapse = "\n")
      )
    )
  }

  if (length(orphaned_cols) > 0) {
    error_messages <- c(
      error_messages,
      sprintf(
        "Orphaned columns found that don't belong to a complete question set:\n  %s\n\nPlease check your CSV export for incomplete or corrupted question columns.",
        paste0("  - '", orphaned_cols, "'", collapse = "\n")
      )
    )
  }

  if (length(error_messages) > 0) {
    stop(paste(error_messages, collapse = "\n\n"))
  }

  invisible(NULL)
}


#' Extract Questions from Glint Survey
#'
#' Parses column names to extract unique questions and their associated column names.
#'
#' @param data A data frame or glint_survey object containing survey data
#'
#' @return A tibble with one row per question containing:
#'   \describe{
#'     \item{question}{The question text}
#'     \item{response_col}{Column name for numeric responses}
#'     \item{comment_col}{Column name for comments}
#'     \item{topics_col}{Column name for comment topics}
#'     \item{flag_col}{Column name for sensitive comment flags}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#' questions <- extract_questions(survey)
#' print(questions)
#' }
extract_questions <- function(data) {
  # Handle glint_survey objects
  if (inherits(data, "glint_survey")) {
    data <- data$data
  }

  # Get standard columns
  standard_cols <- get_standard_columns()

  # Identify question columns
  question_cols <- setdiff(names(data), standard_cols)

  # Get unique question stems
  question_stems <- unique(sapply(question_cols, get_question_stem))

  # Build dataframe of questions
  questions_df <- purrr::map_dfr(question_stems, function(stem) {
    dplyr::tibble(
      question = stem,
      response_col = stem,
      comment_col = paste0(stem, "_COMMENT"),
      topics_col = paste0(stem, "_COMMENT_TOPICS"),
      flag_col = paste0(stem, "_SENSITIVE_COMMENT_FLAG")
    )
  })

  return(questions_df)
}
