#' Aggregate Responses by Manager
#'
#' Rolls up survey responses to the manager level, calculating the same
#' metrics as summarize_survey() for each manager's team.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param scale_points Integer specifying the number of scale points (2-11)
#' @param full_tree Logical indicating whether to include full subtree
#'   (all indirect reports) or only direct reports (default: FALSE)
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
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Direct reports only
#' manager_summary <- aggregate_by_manager(survey, scale_points = 5)
#'
#' # Full organizational tree
#' manager_summary_full <- aggregate_by_manager(survey, scale_points = 5, full_tree = TRUE)
#' }
aggregate_by_manager <- function(survey, scale_points, full_tree = FALSE) {
  if (inherits(survey, "glint_survey")) {
    data <- survey$data
    questions <- survey$metadata$questions
  } else {
    data <- survey
    questions <- extract_questions(data)
  }

  managers <- data %>%
    dplyr::filter(!is.na(`Manager ID`)) %>%
    dplyr::distinct(`Manager ID`) %>%
    dplyr::pull(`Manager ID`)

  manager_names <- data %>%
    dplyr::filter(`EMP ID` %in% managers) %>%
    dplyr::distinct(`EMP ID`, `First Name`, `Last Name`) %>%
    dplyr::mutate(manager_name = paste(`First Name`, `Last Name`))

  results <- purrr::map_dfr(managers, function(mgr_id) {
    if (full_tree) {
      team_members <- get_all_reports(mgr_id, data)
    } else {
      team_members <- data %>%
        dplyr::filter(`Manager ID` == mgr_id) %>%
        dplyr::pull(`EMP ID`)
    }

    if (length(team_members) == 0) {
      return(NULL)
    }

    team_data <- data %>%
      dplyr::filter(`EMP ID` %in% team_members)

    question_results <- summarize_survey(team_data, scale_points = scale_points, questions = "all")
    question_results$manager_id <- mgr_id
    question_results$team_size <- length(team_members)

    return(question_results)
  })

  results <- results %>%
    dplyr::left_join(manager_names, by = c("manager_id" = "EMP ID")) %>%
    dplyr::select(manager_id, manager_name, question, team_size, dplyr::everything())

  return(results)
}


#' Get All Reports for a Manager
#'
#' Internal function to get all direct and indirect reports for a manager.
#'
#' @param manager_id Character string of manager's employee ID
#' @param data Data frame with survey data including EMP ID and Manager ID
#'
#' @return Character vector of employee IDs for all reports
#'
#' @keywords internal
get_all_reports <- function(manager_id, data) {
  direct_reports <- data %>%
    dplyr::filter(`Manager ID` == manager_id) %>%
    dplyr::pull(`EMP ID`) %>%
    unique()

  if (length(direct_reports) == 0) {
    return(character(0))
  }

  all_reports <- direct_reports

  # Recursively get reports of reports
  for (report in direct_reports) {
    indirect_reports <- get_all_reports(report, data)
    all_reports <- c(all_reports, indirect_reports)
  }

  return(unique(all_reports))
}
