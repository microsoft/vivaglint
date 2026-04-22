#' Aggregate Responses by Manager
#'
#' Rolls up survey responses to the manager level, calculating the same
#' metrics as summarize_survey() for each manager's team.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param scale_points Integer specifying the number of scale points (2-11)
#' @param emp_id_col Character string specifying the employee ID column name
#' @param manager_id_col Character string specifying the manager ID column name
#' @param full_tree Logical indicating whether to include full subtree
#'   (all indirect reports) or only direct reports (default: FALSE)
#' @param plot Logical. If \code{TRUE}, prints a ranked lollipop chart of team
#'   Glint Scores by manager and returns the data invisibly. When multiple
#'   questions are present the chart is faceted by question. Requires
#'   \pkg{ggplot2}. Default: \code{FALSE}.
#'
#' @return A tibble with one row per manager-question combination containing:
#'   \describe{
#'     \item{manager_id}{The manager's employee ID}
#'     \item{manager_name}{The manager's full name (First Name + Last Name)}
#'     \item{question}{The question text}
#'     \item{team_size}{Number of employees in the team (direct reports only, or
#'       full subtree when \code{full_tree = TRUE})}
#'     \item{mean, sd, glint_score, n_responses, n_skips, n_total}{Descriptive
#'       statistics for this manager's team on this question}
#'     \item{pct_favorable, pct_neutral, pct_unfavorable}{Favorability
#'       percentages for this manager's team on this question}
#'   }
#'   When \code{plot = TRUE}, the same tibble is returned invisibly after
#'   printing the plot.
#'
#' @export
#'
#' @examples
#' survey_path <- system.file("extdata", "survey_export.csv", package = "vivaglint")
#' survey <- read_glint_survey(survey_path, emp_id_col = "EMP ID")
#'
#' # Direct reports only
#' manager_summary <- aggregate_by_manager(survey, scale_points = 5,
#'                                         emp_id_col = "EMP ID",
#'                                         manager_id_col = "Manager ID")
#'
#' # Full organizational tree
#' manager_summary_full <- aggregate_by_manager(survey, scale_points = 5,
#'                                              emp_id_col = "EMP ID",
#'                                              manager_id_col = "Manager ID",
#'                                              full_tree = TRUE)
#'
#' # With ranked dot plot
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   aggregate_by_manager(survey, scale_points = 5, emp_id_col = "EMP ID",
#'                        manager_id_col = "Manager ID", plot = TRUE)
#' }
aggregate_by_manager <- function(survey, scale_points, emp_id_col = NULL,
                                 manager_id_col, full_tree = FALSE,
                                 plot = FALSE) {
  if (plot) .check_ggplot2()
  if (inherits(survey, "glint_survey")) {
    emp_id_col <- emp_id_col %||% survey$metadata$emp_id_col
    data <- survey$data
    questions <- survey$metadata$questions
    col_map <- survey$metadata$standard_column_map %||% list()
    first_name_col <- col_map$first_name %||% "First Name"
    last_name_col <- col_map$last_name %||% "Last Name"
  } else {
    data <- survey
    questions <- extract_questions(data)
    first_name_col <- "First Name"
    last_name_col <- "Last Name"
  }

  if (is.null(emp_id_col)) {
    stop("emp_id_col must be specified when survey is a plain data frame")
  }

  missing_name_labels <- character(0)
  if (is.null(first_name_col) || !nzchar(first_name_col)) {
    missing_name_labels <- c(missing_name_labels, "First Name")
  }
  if (is.null(last_name_col) || !nzchar(last_name_col)) {
    missing_name_labels <- c(missing_name_labels, "Last Name")
  }
  if (length(missing_name_labels) > 0) {
    stop(
      "aggregate_by_manager() requires column name(s) for: ",
      paste(missing_name_labels, collapse = ", "),
      ". Provide first_name_col/last_name_col when reading the survey.",
      call. = FALSE
    )
  }
  missing_name_cols <- setdiff(c(first_name_col, last_name_col), names(data))
  if (length(missing_name_cols) > 0) {
    stop(
      "aggregate_by_manager() requires the following column(s) to build manager names: ",
      paste0("'", missing_name_cols, "'", collapse = ", "),
      "\n\nColumns detected in the data frame:\n",
      paste0("  - ", names(data), collapse = "\n"),
      "\n\nTo resolve, pass first_name_col/last_name_col when reading with read_glint_survey() or read_glint_survey_api(), or rename your columns.",
      call. = FALSE
    )
  }

  managers <- data %>%
    dplyr::filter(!is.na(.data[[manager_id_col]])) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(manager_id_col))) %>%
    dplyr::pull(manager_id_col)

  manager_names <- data %>%
    dplyr::filter(.data[[emp_id_col]] %in% managers) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(emp_id_col, first_name_col, last_name_col)))) %>%
    dplyr::mutate(manager_name = paste(.data[[first_name_col]], .data[[last_name_col]]))

  results <- purrr::map_dfr(managers, function(mgr_id) {
    if (full_tree) {
      team_members <- get_all_reports(mgr_id, data, emp_id_col, manager_id_col)
    } else {
      team_members <- data %>%
        dplyr::filter(.data[[manager_id_col]] == mgr_id) %>%
        dplyr::pull(emp_id_col)
    }

    if (length(team_members) == 0) {
      return(NULL)
    }

    team_data <- data %>%
      dplyr::filter(.data[[emp_id_col]] %in% team_members)

    question_results <- summarize_survey(team_data, scale_points = scale_points, questions = "all", emp_id_col = emp_id_col)
    question_results$manager_id <- mgr_id
    question_results$team_size <- length(team_members)

    return(question_results)
  })

  results <- results %>%
    dplyr::left_join(manager_names, by = setNames(emp_id_col, "manager_id")) %>%
    dplyr::select(manager_id, manager_name, question, team_size, dplyr::everything())

  if (plot) {
    print(.plot_manager(results))
    return(invisible(results))
  }

  return(results)
}


#' Get All Reports for a Manager
#'
#' Internal function to get all direct and indirect reports for a manager.
#'
#' @param manager_id Character string of manager's employee ID
#' @param data Data frame with survey data
#' @param emp_id_col Character string specifying the employee ID column name
#' @param manager_id_col Character string specifying the manager ID column name
#'
#' @return Character vector of employee IDs for all reports
#'
#' @keywords internal
get_all_reports <- function(manager_id, data, emp_id_col, manager_id_col) {
  direct_reports <- data %>%
    dplyr::filter(.data[[manager_id_col]] == manager_id) %>%
    dplyr::pull(emp_id_col) %>%
    unique()

  if (length(direct_reports) == 0) {
    return(character(0))
  }

  all_reports <- direct_reports

  # Recursively get reports of reports
  for (report in direct_reports) {
    indirect_reports <- get_all_reports(report, data, emp_id_col, manager_id_col)
    all_reports <- c(all_reports, indirect_reports)
  }

  return(unique(all_reports))
}
