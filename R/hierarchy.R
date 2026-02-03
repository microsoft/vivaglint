#' Aggregate Responses by Manager
#'
#' Rolls up survey responses to the manager level, calculating the same
#' metrics as summarize_survey() for each manager's team.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param full_tree Logical indicating whether to include full subtree
#'   (all indirect reports) or only direct reports (default: FALSE)
#'
#' @return A tibble with one row per manager-question combination containing:
#'   \describe{
#'     \item{manager_id}{The manager's employee ID}
#'     \item{manager_name}{The manager's full name}
#'     \item{question}{The question text}
#'     \item{team_size}{Number of employees in the team (direct or full tree)}
#'     \item{All metrics from summarize_survey()}{}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Direct reports only
#' manager_summary <- aggregate_by_manager(survey)
#'
#' # Full organizational tree
#' manager_summary_full <- aggregate_by_manager(survey, full_tree = TRUE)
#' }
aggregate_by_manager <- function(survey, full_tree = FALSE) {
  # Handle glint_survey objects
  if (inherits(survey, "glint_survey")) {
    data <- survey$data
    questions <- survey$metadata$questions
  } else {
    data <- survey
    questions <- extract_questions(data)
  }

  # Get unique managers
  managers <- data %>%
    dplyr::filter(!is.na(`Manager ID`)) %>%
    dplyr::distinct(`Manager ID`) %>%
    dplyr::pull(`Manager ID`)

  # Get manager names
  manager_names <- data %>%
    dplyr::filter(`EMP ID` %in% managers) %>%
    dplyr::distinct(`EMP ID`, `First Name`, `Last Name`) %>%
    dplyr::mutate(manager_name = paste(`First Name`, `Last Name`))

  # Aggregate for each manager
  results <- purrr::map_dfr(managers, function(mgr_id) {
    # Get team members
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

    # Filter data to team members
    team_data <- data %>%
      dplyr::filter(`EMP ID` %in% team_members)

    # Analyze each question for this team
    question_results <- summarize_survey(team_data, questions = "all")
    question_results$manager_id <- mgr_id
    question_results$team_size <- length(team_members)

    return(question_results)
  })

  # Add manager names
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
