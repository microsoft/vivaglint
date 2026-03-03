#' Read Viva Glint Survey Data
#'
#' Reads a Viva Glint survey export CSV file with automatic validation and parsing.
#' This function validates the file structure, parses dates, and organizes the data
#' into a structured format ready for analysis.
#'
#' @param file_path Character string specifying the path to the CSV file
#' @param encoding Character string specifying file encoding (default: "UTF-8")
#'
#' @return A \code{glint_survey} object (an S3 class extending \code{list}) with
#'   two elements:
#'   \describe{
#'     \item{data}{A tibble containing all survey responses. Date columns are
#'       parsed automatically from DD-MM-YYYY HH:MM format to POSIXct.}
#'     \item{metadata}{A list with five elements: \code{standard_columns} (the
#'       eight standard Glint column names), \code{questions} (a tibble from
#'       \code{extract_questions()} with one row per question), \code{n_respondents}
#'       (total row count), \code{n_questions} (number of questions), and
#'       \code{file_path} (the path originally supplied).}
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
  if (!file.exists(file_path)) {
    stop(sprintf(
      "File not found: '%s'\nPlease check that the file path is correct.",
      file_path
    ))
  }

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

  validate_glint_structure(data)

  standard_cols <- get_standard_columns()

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

  questions_df <- extract_questions(data)

  metadata <- list(
    standard_columns = standard_cols,
    questions = questions_df,
    n_respondents = nrow(data),
    n_questions = nrow(questions_df),
    file_path = file_path
  )

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
  standard_cols <- get_standard_columns()
  missing_cols <- setdiff(standard_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Missing required standard column(s): %s\n\nYour CSV file must contain all of the following standard Viva Glint columns:\n  %s\n\nPlease ensure you are using a complete Viva Glint survey export.",
      paste0("'", missing_cols, "'", collapse = ", "),
      paste0("  - ", standard_cols, collapse = "\n")
    ))
  }

  question_cols <- setdiff(names(data), standard_cols)

  if (length(question_cols) == 0) {
    stop(
      "No question columns found in the data.\n\nA Viva Glint export should contain at least one question with its associated columns:\n  - [Question Text]\n  - [Question Text]_COMMENT\n  - [Question Text]_COMMENT_TOPICS\n  - [Question Text]_SENSITIVE_COMMENT_FLAG\n\nPlease check that you are using a complete survey export file."
    )
  }

  question_stems <- unique(sapply(question_cols, get_question_stem))

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
  if (inherits(data, "glint_survey")) {
    data <- data$data
  }

  standard_cols <- get_standard_columns()
  question_cols <- setdiff(names(data), standard_cols)
  question_stems <- unique(sapply(question_cols, get_question_stem))

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


#' Join Employee Attributes to Survey Data
#'
#' Reads employee attribute data from a CSV file or data frame and joins it to
#' a survey object by employee ID. Returns an enriched \code{glint_survey}
#' object that can be passed directly to \code{analyze_by_attributes()} or any
#' other package function, eliminating the need to re-read and re-join the
#' attribute file on every call.
#'
#' @param survey A \code{glint_survey} object or data frame containing survey data
#' @param attribute_source Either a character string file path to a CSV file, or
#'   a data frame containing employee attributes. All columns are coerced to
#'   character to avoid type conflicts during joining.
#' @param emp_id_col Character string specifying the employee ID column name in
#'   the attribute data (default: \code{"EMP ID"}). The survey data must contain
#'   a column named \code{"EMP ID"}.
#'
#' @return If \code{survey} is a \code{glint_survey} object, returns an enriched
#'   \code{glint_survey} with the attribute columns appended to \code{$data} and
#'   the names of all joined attribute columns stored in
#'   \code{$metadata$attribute_cols}. If \code{survey} is a plain data frame,
#'   returns the joined data frame.
#'
#'   Respondents whose \code{EMP ID} has no match in the attribute data will
#'   have \code{NA} for all attribute columns; a message is emitted indicating
#'   how many were affected.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Join from a file path
#' survey_enriched <- join_attributes(survey, "employee_attributes.csv")
#'
#' # Join from a data frame already in memory
#' attrs <- readr::read_csv("employee_attributes.csv")
#' survey_enriched <- join_attributes(survey, attrs)
#'
#' # Use the enriched survey directly — no attribute_file needed
#' results <- analyze_by_attributes(survey_enriched, scale_points = 5,
#'                                  attribute_cols = "Department")
#'
#' # Filter before analysis
#' na_only <- survey_enriched
#' na_only$data <- dplyr::filter(survey_enriched$data, Division == "North America")
#' results_na <- analyze_by_attributes(na_only, scale_points = 5,
#'                                     attribute_cols = "Department")
#' }
join_attributes <- function(survey, attribute_source, emp_id_col = "EMP ID") {
  if (inherits(survey, "glint_survey")) {
    data <- survey$data
  } else if (is.data.frame(survey)) {
    data <- survey
  } else {
    stop("survey must be a glint_survey object or a data frame")
  }

  if (!"EMP ID" %in% names(data)) {
    stop("Column 'EMP ID' not found in survey data")
  }

  if (is.character(attribute_source) && length(attribute_source) == 1) {
    if (!file.exists(attribute_source)) {
      stop(sprintf("Attribute file not found: '%s'", attribute_source))
    }
    attributes <- readr::read_csv(
      attribute_source,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    )
  } else if (is.data.frame(attribute_source)) {
    attributes <- dplyr::mutate(
      attribute_source,
      dplyr::across(dplyr::everything(), as.character)
    )
  } else {
    stop("attribute_source must be a file path (character string) or a data frame")
  }

  if (!emp_id_col %in% names(attributes)) {
    stop(sprintf("Column '%s' not found in attribute data", emp_id_col))
  }

  new_attr_cols <- setdiff(names(attributes), emp_id_col)

  overlap <- intersect(new_attr_cols, names(data))
  if (length(overlap) > 0) {
    warning(sprintf(
      "The following columns already exist in the survey data and will be overwritten: %s",
      paste0("'", overlap, "'", collapse = ", ")
    ))
    data <- dplyr::select(data, -dplyr::all_of(overlap))
  }

  joined_data <- dplyr::left_join(
    data,
    attributes,
    by = c("EMP ID" = emp_id_col)
  )

  n_unmatched <- sum(!data[["EMP ID"]] %in% attributes[[emp_id_col]])
  if (n_unmatched > 0) {
    message(sprintf(
      "%d respondent(s) had no match in the attribute data and will have NA for all attribute columns.",
      n_unmatched
    ))
  }

  if (inherits(survey, "glint_survey")) {
    existing_attr_cols <- survey$metadata$attribute_cols %||% character(0)
    survey$data <- joined_data
    survey$metadata$attribute_cols <- unique(c(existing_attr_cols, new_attr_cols))
    return(survey)
  } else {
    return(joined_data)
  }
}
