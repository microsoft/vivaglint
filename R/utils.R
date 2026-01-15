#' Get Question Stem from Column Name
#'
#' Extracts the base question text from a column name by removing
#' standard suffixes (_COMMENT, _COMMENT_TOPICS, _SENSITIVE_COMMENT_FLAG).
#'
#' @param col_name Character string representing a column name
#'
#' @return Character string with the base question text
#'
#' @keywords internal
get_question_stem <- function(col_name) {
  # Remove the standard suffixes
  col_name <- stringr::str_remove(col_name, "_COMMENT_TOPICS$")
  col_name <- stringr::str_remove(col_name, "_SENSITIVE_COMMENT_FLAG$")
  col_name <- stringr::str_remove(col_name, "_COMMENT$")

  return(col_name)
}


#' Parse Comment Topics
#'
#' Splits comma-separated topic strings into vectors, handling empty and NA values.
#'
#' @param topics Character vector of comma-separated topic strings
#' @param return_format Character string specifying output format: "list" (default) or "tidy"
#'
#' @return If return_format is "list", returns a list of character vectors.
#'   If return_format is "tidy", returns a tibble with columns for original index and individual topics.
#'
#' @keywords internal
parse_comment_topics <- function(topics, return_format = "list") {
  if (!return_format %in% c("list", "tidy")) {
    stop("return_format must be either 'list' or 'tidy'")
  }

  # Handle NA and empty strings
  topics <- ifelse(is.na(topics) | topics == "", NA_character_, topics)

  # Split by comma and trim whitespace
  topic_list <- lapply(topics, function(x) {
    if (is.na(x)) {
      return(character(0))
    }
    stringr::str_trim(stringr::str_split(x, ",")[[1]])
  })

  if (return_format == "list") {
    return(topic_list)
  } else {
    # Return tidy format: one row per topic
    result <- purrr::map2_dfr(
      seq_along(topic_list),
      topic_list,
      function(idx, topics) {
        if (length(topics) == 0) {
          return(NULL)
        }
        dplyr::tibble(
          index = idx,
          topic = topics
        )
      }
    )
    return(result)
  }
}


#' Get Standard Column Names
#'
#' Returns the vector of standard column names expected in Viva Glint exports.
#'
#' @return Character vector of standard column names
#'
#' @keywords internal
get_standard_columns <- function() {
  c("First Name", "Last Name", "Email", "Status", "EMP ID", "Manager ID",
    "Survey Cycle Completion Date", "Survey Cycle Sent Date")
}
