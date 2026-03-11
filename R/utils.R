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


#' Convert Raw Mean to Glint Score
#'
#' Transforms a raw mean response on a rating scale to the 100-point Glint Score
#' as displayed in the Viva Glint UI. Rounded to the nearest whole number, consistent
#' with Glint's rounding convention.
#'
#' @param mean_val Numeric raw mean value
#' @param scale_points Integer number of scale points (2-11)
#'
#' @return Numeric Glint Score on a 0-100 scale
#'
#' @keywords internal
mean_to_glint_score <- function(mean_val, scale_points) {
  round(((mean_val - 1) / (scale_points - 1)) * 100)
}


#' Get Standard Column Names
#'
#' Returns the vector of standard column names expected in Viva Glint exports.
#' The employee ID column name is configurable because customers use different
#' names. "Manager ID" is included for question-detection exclusion but is not
#' required to be present.
#'
#' @param emp_id_col Character string specifying the employee ID column name
#'
#' @return Character vector of standard column names
#'
#' @keywords internal
get_standard_columns <- function(emp_id_col) {
  c("First Name", "Last Name", "Email", "Status", emp_id_col, "Manager ID",
    "Survey Cycle Completion Date", "Survey Cycle Sent Date")
}


#' Get Favorability Map for Scale Points
#'
#' Returns the favorability classification for a given scale point count,
#' based on Glint standards.
#'
#' @param scale_points Integer specifying the number of scale points (2-11)
#'
#' @return A list with three elements: favorable, neutral, and unfavorable,
#'   each containing a vector of scale points classified into that category
#'
#' @keywords internal
get_favorability_map <- function(scale_points) {
  favorability_map <- list(
    "2" = list(favorable = c(2), neutral = c(), unfavorable = c(1)),
    "3" = list(favorable = c(3), neutral = c(2), unfavorable = c(1)),
    "4" = list(favorable = c(4), neutral = c(2, 3), unfavorable = c(1)),
    "5" = list(favorable = c(4, 5), neutral = c(3), unfavorable = c(1, 2)),
    "6" = list(favorable = c(4, 5, 6), neutral = c(), unfavorable = c(1, 2, 3)),
    "7" = list(favorable = c(6, 7), neutral = c(4, 5), unfavorable = c(1, 2, 3)),
    "8" = list(favorable = c(6, 7, 8), neutral = c(4, 5), unfavorable = c(1, 2, 3)),
    "9" = list(favorable = c(7, 8, 9), neutral = c(4, 5, 6), unfavorable = c(1, 2, 3)),
    "10" = list(favorable = c(8, 9, 10), neutral = c(4, 5, 6, 7), unfavorable = c(1, 2, 3)),
    "11" = list(favorable = c(10, 11), neutral = c(8, 9), unfavorable = c(1, 2, 3, 4, 5, 6, 7))
  )

  return(favorability_map[[as.character(scale_points)]])
}
