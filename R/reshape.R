#' Pivot Survey Data to Long Format
#'
#' Transforms survey data from wide format (one row per respondent) to long format
#' (one row per respondent-question combination). Can return all responses, only
#' comments, or both depending on the data_type parameter.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param data_type Character string indicating what data to return: "all" for all
#'   responses including those without comments, "comments" for only responses with
#'   comments, or "both" for separate tibbles (default: "all")
#' @param include_empty Logical indicating whether to include empty comments when
#'   data_type = "comments" (default: FALSE)
#' @param include_standard_cols Logical indicating whether to include standard
#'   columns (EMP ID, Manager ID, etc.) in the output (default: TRUE)
#'
#' @return When \code{data_type = "all"} or \code{"comments"}, a single tibble
#'   in long format with columns:
#'   \describe{
#'     \item{Standard columns}{EMP ID, First Name, Last Name, etc. (if
#'       \code{include_standard_cols = TRUE})}
#'     \item{question}{The question text}
#'     \item{response}{Numeric response value}
#'     \item{comment}{Comment text (NA if not provided)}
#'     \item{comment_topics}{Comma-separated topic tags (NA if not provided)}
#'     \item{sensitive_flag}{Sensitivity flag value}
#'   }
#'   When \code{data_type = "both"}, a named list with two elements:
#'   \describe{
#'     \item{all}{Long-format tibble of all responses (same structure as above)}
#'     \item{comments}{Long-format tibble filtered to rows with non-empty comments}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Get all responses (with and without comments)
#' all_data <- pivot_long(survey)
#'
#' # Get only responses with comments
#' comments_only <- pivot_long(survey, data_type = "comments")
#'
#' # Get both separately
#' both <- pivot_long(survey, data_type = "both")
#' all_responses <- both$all
#' comments <- both$comments
#' }
pivot_long <- function(survey, data_type = "all", include_empty = FALSE, include_standard_cols = TRUE) {
  if (!data_type %in% c("all", "comments", "both")) {
    stop("data_type must be one of: 'all', 'comments', or 'both'")
  }

  if (inherits(survey, "glint_survey")) {
    emp_id_col <- survey$metadata$emp_id_col
    data <- survey$data
    questions <- survey$metadata$questions
  } else {
    emp_id_col <- NULL
    data <- survey
    questions <- extract_questions(data)
  }
  if (is.null(emp_id_col)) {
    stop("emp_id_col could not be determined. Load your survey with read_glint_survey() and specify emp_id_col.")
  }

  standard_cols <- get_standard_columns(emp_id_col)
  present_standard_cols <- intersect(standard_cols, names(data))
  standard_data <- data[, present_standard_cols, drop = FALSE]

  long_data <- purrr::map_dfr(seq_len(nrow(questions)), function(i) {
    q <- questions[i, ]

    question_data <- dplyr::tibble(
      question = q$question,
      response = data[[q$response_col]],
      comment = data[[q$comment_col]],
      comment_topics = data[[q$topics_col]],
      sensitive_flag = data[[q$flag_col]]
    )

    if (include_standard_cols) {
      question_data <- dplyr::bind_cols(standard_data, question_data)
    }

    return(question_data)
  })

  if (data_type == "all") {
    return(long_data)
  } else if (data_type == "comments") {
    if (!include_empty) {
      long_data <- long_data %>%
        dplyr::filter(!is.na(comment) & comment != "")
    }
    return(long_data)
  } else {  # data_type == "both"
    comments_data <- long_data
    if (!include_empty) {
      comments_data <- comments_data %>%
        dplyr::filter(!is.na(comment) & comment != "")
    }

    return(list(
      all = long_data,
      comments = comments_data
    ))
  }
}
