#' Read Viva Glint Survey Data
#'
#' Reads a Viva Glint survey export CSV file with automatic validation and parsing.
#' This function validates the file structure, parses dates, and organizes the data
#' into a structured format ready for analysis.
#' To import directly from the Viva Glint API, use
#' \code{read_glint_survey_api()}.
#'
#' @param file_path Character string specifying the path to the CSV file
#' @param emp_id_col Character string specifying the name of the employee ID
#'   column in the survey export (e.g., \code{"Employee ID"},
#'   \code{"EmployeeID"}, \code{"emp_id"}). The column name varies by
#'   organization. The value is stored in \code{survey$metadata$emp_id_col}
#'   and used automatically by downstream functions so you do not need to
#'   repeat it on every call.
#' @param first_name_col Column name for first name (default: "First Name")
#' @param last_name_col Column name for last name (default: "Last Name")
#' @param email_col Column name for email (default: "Email")
#' @param status_col Column name for status (default: "Status")
#' @param completion_date_col Column name for survey completion date
#'   (default: "Survey Cycle Completion Date")
#' @param sent_date_col Column name for survey sent date
#'   (default: "Survey Cycle Sent Date")
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
#' survey_path <- system.file("extdata", "survey_export.csv", package = "vivaglint")
#' survey <- read_glint_survey(survey_path, emp_id_col = "EMP ID")
#' head(survey$data)
#' survey$metadata$questions
read_glint_survey <- function(file_path,
                              emp_id_col = NULL,
                              first_name_col = "First Name",
                              last_name_col = "Last Name",
                              email_col = "Email",
                              status_col = "Status",
                              completion_date_col = "Survey Cycle Completion Date",
                              sent_date_col = "Survey Cycle Sent Date",
                              encoding = "UTF-8") {
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

  build_glint_survey(
    data,
    emp_id_col,
    first_name_col = first_name_col,
    last_name_col = last_name_col,
    email_col = email_col,
    status_col = status_col,
    completion_date_col = completion_date_col,
    sent_date_col = sent_date_col,
    file_path = file_path
  )
}


#' Build a glint_survey Object
#'
#' Internal helper to validate, parse, and attach metadata.
#'
#' @param data A data frame containing Viva Glint survey data
#' @param emp_id_col Character string specifying the employee ID column name
#' @param first_name_col Column name for first name
#' @param last_name_col Column name for last name
#' @param email_col Column name for email
#' @param status_col Column name for status
#' @param completion_date_col Column name for survey completion date
#' @param sent_date_col Column name for survey sent date
#' @param file_path Character string for the source file path (or NA when not
#'   loaded from disk)
#'
#' @return A \code{glint_survey} object
#'
#' @keywords internal
build_glint_survey <- function(data,
                               emp_id_col,
                               first_name_col = "First Name",
                               last_name_col = "Last Name",
                               email_col = "Email",
                               status_col = "Status",
                               completion_date_col = "Survey Cycle Completion Date",
                               sent_date_col = "Survey Cycle Sent Date",
                               file_path = NA_character_) {
  validate_glint_structure(
    data,
    emp_id_col,
    first_name_col = first_name_col,
    last_name_col = last_name_col,
    email_col = email_col,
    status_col = status_col,
    completion_date_col = completion_date_col,
    sent_date_col = sent_date_col
  )

  standard_cols <- get_standard_columns(
    emp_id_col,
    first_name_col = first_name_col,
    last_name_col = last_name_col,
    email_col = email_col,
    status_col = status_col,
    completion_date_col = completion_date_col,
    sent_date_col = sent_date_col
  )

  date_cols <- c(completion_date_col, sent_date_col)
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

  questions_df <- extract_questions(data, emp_id_col)

  standard_column_map <- list(
    first_name = first_name_col,
    last_name = last_name_col,
    email = email_col,
    status = status_col,
    emp_id = emp_id_col,
    manager_id = "Manager ID",
    completion_date = completion_date_col,
    sent_date = sent_date_col
  )

  metadata <- list(
    standard_columns = standard_cols,
    standard_column_map = standard_column_map,
    emp_id_col = emp_id_col,
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
#' @param emp_id_col Character string specifying the employee ID column name
#' @param first_name_col Column name for first name
#' @param last_name_col Column name for last name
#' @param email_col Column name for email
#' @param status_col Column name for status
#' @param completion_date_col Column name for survey completion date
#' @param sent_date_col Column name for survey sent date
#'
#' @return NULL (invisibly) if validation passes, otherwise throws an error
#'
#' @keywords internal
validate_glint_structure <- function(data,
                                     emp_id_col,
                                     first_name_col = "First Name",
                                     last_name_col = "Last Name",
                                     email_col = "Email",
                                     status_col = "Status",
                                     completion_date_col = "Survey Cycle Completion Date",
                                     sent_date_col = "Survey Cycle Sent Date") {
  question_suffixes <- c("_COMMENT", "_COMMENT_TOPICS", "_SENSITIVE_COMMENT_FLAG")
  question_suffix_pattern <- "(_COMMENT|_COMMENT_TOPICS|_SENSITIVE_COMMENT_FLAG)$"
  normalize_name <- function(value) trimws(value, which = "right")
  resolve_base_col <- function(stem_trim, column_names) {
    exact_match <- column_names[column_names == stem_trim]
    if (length(exact_match) > 0) {
      return(exact_match[1])
    }
    normalized_names <- normalize_name(column_names)
    matches <- column_names[normalized_names == stem_trim]
    if (length(matches) > 0) {
      return(matches[1])
    }
    ""
  }
  find_suffix_cols <- function(stem_trim, column_names) {
    candidates <- column_names[grepl(question_suffix_pattern, column_names)]
    if (length(candidates) == 0) {
      return(character(0))
    }
    stems <- normalize_name(vapply(candidates, get_question_stem, character(1)))
    candidates[stems == stem_trim]
  }
  detect_question_columns <- function(column_names) {
    suffix_cols <- column_names[grepl(question_suffix_pattern, column_names)]
    if (length(suffix_cols) == 0) {
      return(character(0))
    }
    stems_trim <- unique(normalize_name(vapply(suffix_cols, get_question_stem, character(1))))
    stems_trim <- stems_trim[nzchar(stems_trim)]
    base_cols <- vapply(stems_trim, resolve_base_col, character(1), column_names = column_names)
    base_cols <- base_cols[nzchar(base_cols)]
    unique(c(base_cols, suffix_cols))
  }
  detected_columns <- setdiff(names(data), detect_question_columns(names(data)))
  detected_columns_output <- if (length(detected_columns) == 0) {
    "  - (none)"
  } else {
    paste0("  - ", detected_columns, collapse = "\n")
  }

  required_name_map <- list(
    "Employee ID" = emp_id_col
  )
  optional_name_map <- list(
    "First Name" = first_name_col,
    "Last Name" = last_name_col,
    "Email" = email_col,
    "Status" = status_col,
    "Survey Cycle Completion Date" = completion_date_col,
    "Survey Cycle Sent Date" = sent_date_col
  )
  missing_required_names <- names(required_name_map)[vapply(
    required_name_map,
    function(x) is.null(x) || is.na(x) || !nzchar(x),
    logical(1)
  )]
  if (length(missing_required_names) > 0) {
    stop(
      "Column name(s) must be provided for: ",
      paste(missing_required_names, collapse = ", "),
      ".\n\nColumns detected in the data frame:\n",
      detected_columns_output,
      "\n\nPass the correct column name arguments or rename your columns.",
      call. = FALSE
    )
  }

  required_cols <- unname(unlist(required_name_map))
  required_cols <- required_cols[!is.na(required_cols) & nzchar(required_cols)]
  missing_required_cols <- setdiff(required_cols, names(data))
  if (length(missing_required_cols) > 0) {
    stop(sprintf(
      "Missing required standard column(s): %s\n\nYour CSV file must contain all of the following required Viva Glint columns:\n%s\n\nColumns detected in the data frame:\n%s\n\nPlease ensure you are using a complete Viva Glint survey export, or pass the correct column name arguments.",
      paste0("'", missing_required_cols, "'", collapse = ", "),
      paste0("  - ", required_cols, collapse = "\n"),
      detected_columns_output
    ))
  }

  missing_optional_names <- names(optional_name_map)[vapply(
    optional_name_map,
    function(x) is.null(x) || is.na(x) || !nzchar(x),
    logical(1)
  )]
  optional_name_map_present <- optional_name_map[setdiff(names(optional_name_map), missing_optional_names)]
  optional_cols <- unname(unlist(optional_name_map_present))
  optional_cols <- optional_cols[!is.na(optional_cols) & nzchar(optional_cols)]
  missing_optional_cols <- setdiff(optional_cols, names(data))
  missing_optional_labels <- unique(c(
    missing_optional_names,
    names(optional_name_map_present)[optional_cols %in% missing_optional_cols]
  ))

  if (length(missing_optional_labels) > 0) {
    impact_map <- list(
      "First Name" = "aggregate_by_manager() manager_name output",
      "Last Name" = "aggregate_by_manager() manager_name output",
      "Survey Cycle Completion Date" = "analyze_attrition() (survey date calculations)",
      "Survey Cycle Sent Date" = "no current functions depend on it",
      "Email" = "no current functions depend on it",
      "Status" = "no current functions depend on it"
    )
    warning_lines <- c(
      "Optional standard columns are missing or unspecified. Related functionality may be limited:",
      vapply(missing_optional_labels, function(label) {
        expected <- optional_name_map[[label]]
        expected_note <- if (is.null(expected) || !nzchar(expected)) {
          "no column name provided"
        } else {
          paste0("expected '", expected, "'")
        }
        impact <- impact_map[[label]]
        impact_note <- if (is.null(impact)) "some functionality may be limited" else impact
        paste0("  - ", label, " (", expected_note, "). Affects: ", impact_note, ".")
      }, character(1)),
      "",
      "Columns detected in the data frame:",
      detected_columns_output
    )
    warning(paste(warning_lines, collapse = "\n"), call. = FALSE)
  }

  all_standard_cols <- get_standard_columns(
    emp_id_col,
    first_name_col = first_name_col,
    last_name_col = last_name_col,
    email_col = email_col,
    status_col = status_col,
    completion_date_col = completion_date_col,
    sent_date_col = sent_date_col
  )
  question_cols <- setdiff(names(data), all_standard_cols)
  suffix_cols <- question_cols[grepl(question_suffix_pattern, question_cols)]
  question_stems <- unique(normalize_name(vapply(suffix_cols, get_question_stem, character(1))))
  question_stems <- question_stems[nzchar(question_stems)]

  if (length(question_stems) == 0) {
    stop(
      "No question columns found in the data.\n\nA Viva Glint export should contain at least one question with its associated columns:\n  - [Question Text]\n  - [Question Text]_COMMENT\n  - [Question Text]_COMMENT_TOPICS\n  - [Question Text]_SENSITIVE_COMMENT_FLAG\n\nPlease check that you are using a complete survey export file."
    )
  }

  expected_suffixes <- c("", question_suffixes)
  orphaned_cols <- character(0)
  incomplete_questions <- character(0)

  for (stem in question_stems) {
    base_col <- resolve_base_col(stem, names(data))
    expected_cols <- c(
      if (nzchar(base_col)) base_col else stem,
      paste0(stem, question_suffixes)
    )
    existing_suffix_cols <- find_suffix_cols(stem, names(data))
    existing_suffix_types <- vapply(question_suffixes, function(suffix) {
      any(endsWith(existing_suffix_cols, suffix))
    }, logical(1))
    found_cols <- c(nzchar(base_col), existing_suffix_types)

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

  if (length(suffix_cols) > 0) {
    suffix_stems <- normalize_name(vapply(suffix_cols, get_question_stem, character(1)))
    base_matches <- vapply(suffix_stems, resolve_base_col, character(1), column_names = names(data))
    orphaned_cols <- unique(suffix_cols[!nzchar(base_matches)])
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
#' @param emp_id_col Character string specifying the employee ID column name.
#'   When \code{data} is a \code{glint_survey} object this is resolved
#'   automatically from \code{data$metadata$emp_id_col}. Required when
#'   \code{data} is a plain data frame.
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
#' survey_path <- system.file("extdata", "survey_export.csv", package = "vivaglint")
#' survey <- read_glint_survey(survey_path, emp_id_col = "EMP ID")
#' questions <- extract_questions(survey)
#' print(questions)
extract_questions <- function(data, emp_id_col = NULL) {
  if (inherits(data, "glint_survey")) {
    emp_id_col <- data$metadata$emp_id_col %||% emp_id_col
    standard_cols <- data$metadata$standard_columns
    data <- data$data
  } else {
    standard_cols <- NULL
  }
  if (is.null(emp_id_col)) {
    stop("emp_id_col must be specified. When loading with read_glint_survey(), pass emp_id_col to store it automatically.")
  }

  standard_cols <- standard_cols %||% get_standard_columns(emp_id_col)
  question_cols <- setdiff(names(data), standard_cols)
  question_suffixes <- c("_COMMENT", "_COMMENT_TOPICS", "_SENSITIVE_COMMENT_FLAG")
  question_suffix_pattern <- "(_COMMENT|_COMMENT_TOPICS|_SENSITIVE_COMMENT_FLAG)$"
  normalize_name <- function(value) trimws(value, which = "right")
  resolve_base_col <- function(stem_trim, column_names) {
    exact_match <- column_names[column_names == stem_trim]
    if (length(exact_match) > 0) {
      return(exact_match[1])
    }
    normalized_names <- normalize_name(column_names)
    matches <- column_names[normalized_names == stem_trim]
    if (length(matches) > 0) {
      return(matches[1])
    }
    ""
  }
  find_suffix_col <- function(stem_trim, suffix, column_names) {
    candidates <- column_names[grepl(paste0(suffix, "$"), column_names)]
    if (length(candidates) == 0) {
      return(paste0(stem_trim, suffix))
    }
    stems <- normalize_name(vapply(candidates, get_question_stem, character(1)))
    matches <- candidates[stems == stem_trim]
    if (length(matches) > 0) {
      return(matches[1])
    }
    paste0(stem_trim, suffix)
  }
  suffix_cols <- question_cols[grepl(question_suffix_pattern, question_cols)]
  question_stems <- unique(normalize_name(vapply(suffix_cols, get_question_stem, character(1))))
  question_stems <- question_stems[nzchar(question_stems)]

  questions_df <- purrr::map_dfr(question_stems, function(stem) {
    response_col <- resolve_base_col(stem, names(data))
    if (!nzchar(response_col)) {
      response_col <- stem
    }
    dplyr::tibble(
      question = stem,
      response_col = response_col,
      comment_col = find_suffix_col(stem, "_COMMENT", names(data)),
      topics_col = find_suffix_col(stem, "_COMMENT_TOPICS", names(data)),
      flag_col = find_suffix_col(stem, "_SENSITIVE_COMMENT_FLAG", names(data))
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
#' @param emp_id_col Character string specifying the employee ID column name.
#'   Must match the column name in both the survey data and the attribute data.
#'   When \code{survey} is a \code{glint_survey} object this defaults to
#'   \code{survey$metadata$emp_id_col} (set at import via
#'   \code{read_glint_survey()}), so you can omit this argument if you loaded
#'   the survey with the correct \code{emp_id_col}.
#'
#' @return If \code{survey} is a \code{glint_survey} object, returns an enriched
#'   \code{glint_survey} with the attribute columns appended to \code{$data} and
#'   the names of all joined attribute columns stored in
#'   \code{$metadata$attribute_cols}. If \code{survey} is a plain data frame,
#'   returns the joined data frame.
#'
#'   Respondents with no match in the attribute data will have \code{NA} for all
#'   attribute columns; a message is emitted indicating how many were affected.
#'
#' @export
#'
#' @examples
#' survey_path <- system.file("extdata", "survey_export.csv", package = "vivaglint")
#' attr_path <- system.file("extdata", "employee_attributes.csv", package = "vivaglint")
#' survey <- read_glint_survey(survey_path, emp_id_col = "EMP ID")
#'
#' survey_enriched <- join_attributes(survey, attr_path, emp_id_col = "EMP ID")
#' results <- analyze_by_attributes(survey_enriched, scale_points = 5,
#'                                  attribute_cols = "Department")
join_attributes <- function(survey, attribute_source, emp_id_col = NULL) {
  if (inherits(survey, "glint_survey")) {
    emp_id_col <- emp_id_col %||% survey$metadata$emp_id_col
    data <- survey$data
  } else if (is.data.frame(survey)) {
    data <- survey
  } else {
    stop("survey must be a glint_survey object or a data frame")
  }

  if (is.null(emp_id_col)) {
    stop("emp_id_col must be specified when survey is a plain data frame (it cannot be resolved from metadata)")
  }

  if (!emp_id_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in survey data", emp_id_col))
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
    by = emp_id_col
  )

  n_unmatched <- sum(!data[[emp_id_col]] %in% attributes[[emp_id_col]])
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
