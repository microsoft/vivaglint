#' Build Organizational Tree
#'
#' Creates an organizational hierarchy from EMP ID and Manager ID columns,
#' detecting and handling cycles, orphaned nodes, and top-level employees.
#'
#' @param survey A glint_survey object or data frame containing survey data
#'
#' @return A data.tree Node object representing the organizational hierarchy
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#' org_tree <- build_org_tree(survey)
#' print(org_tree)
#' }
build_org_tree <- function(survey) {
  # Handle glint_survey objects
  if (inherits(survey, "glint_survey")) {
    data <- survey$data
  } else {
    data <- survey
  }

  # Extract employee and manager IDs
  emp_data <- data %>%
    dplyr::select(`EMP ID`, `Manager ID`, `First Name`, `Last Name`) %>%
    dplyr::distinct()

  # Identify top-level employees (no manager or self-referential)
  top_level <- emp_data %>%
    dplyr::filter(is.na(`Manager ID`) | `Manager ID` == `EMP ID` | !(`Manager ID` %in% emp_data$`EMP ID`))

  # Create root node
  root <- data.tree::Node$new("Organization")

  # Detect cycles
  cycles <- detect_hierarchy_cycles(emp_data)
  if (length(cycles) > 0) {
    warning(sprintf(
      "Detected %d cycle(s) in the organizational hierarchy. These employees will be treated as top-level:\n  %s",
      length(cycles),
      paste(cycles, collapse = ", ")
    ))
  }

  # Build tree recursively
  added_employees <- character(0)

  # Add top-level employees first
  for (i in seq_len(nrow(top_level))) {
    emp <- top_level[i, ]
    emp_id <- emp$`EMP ID`

    if (!emp_id %in% added_employees) {
      node_name <- sprintf("%s %s (%s)", emp$`First Name`, emp$`Last Name`, emp_id)
      emp_node <- root$AddChild(node_name)
      emp_node$emp_id <- emp_id
      added_employees <- c(added_employees, emp_id)

      # Add their reports recursively
      added_employees <- add_reports_recursive(emp_node, emp_id, emp_data, added_employees, cycles)
    }
  }

  return(root)
}


#' Add Reports Recursively
#'
#' Internal function to recursively add direct reports to an employee node.
#'
#' @param parent_node data.tree Node for the parent employee
#' @param parent_emp_id Character string of parent's employee ID
#' @param emp_data Data frame with employee and manager relationships
#' @param added_employees Character vector of already-added employee IDs
#' @param cycles Character vector of employee IDs involved in cycles
#'
#' @return Updated vector of added_employees
#'
#' @keywords internal
add_reports_recursive <- function(parent_node, parent_emp_id, emp_data, added_employees, cycles) {
  # Find direct reports
  reports <- emp_data %>%
    dplyr::filter(`Manager ID` == parent_emp_id, `EMP ID` != parent_emp_id, !(`EMP ID` %in% cycles))

  # Add each report
  for (i in seq_len(nrow(reports))) {
    emp <- reports[i, ]
    emp_id <- emp$`EMP ID`

    # Skip if already added (prevents infinite loops)
    if (emp_id %in% added_employees) {
      next
    }

    node_name <- sprintf("%s %s (%s)", emp$`First Name`, emp$`Last Name`, emp_id)
    emp_node <- parent_node$AddChild(node_name)
    emp_node$emp_id <- emp_id
    added_employees <- c(added_employees, emp_id)

    # Recursively add their reports
    added_employees <- add_reports_recursive(emp_node, emp_id, emp_data, added_employees, cycles)
  }

  return(added_employees)
}


#' Detect Hierarchy Cycles
#'
#' Internal function to detect circular reporting relationships in the organizational hierarchy.
#'
#' @param emp_data Data frame with employee and manager relationships
#'
#' @return Character vector of employee IDs involved in cycles
#'
#' @keywords internal
detect_hierarchy_cycles <- function(emp_data) {
  cycles <- character(0)

  for (i in seq_len(nrow(emp_data))) {
    emp_id <- emp_data$`EMP ID`[i]
    manager_id <- emp_data$`Manager ID`[i]

    if (is.na(manager_id) || manager_id == emp_id) {
      next
    }

    # Follow the chain up to detect cycles
    visited <- character(0)
    current <- manager_id

    while (!is.na(current) && !(current %in% visited)) {
      visited <- c(visited, current)

      # Check if we've cycled back to the starting employee
      if (current == emp_id) {
        cycles <- c(cycles, emp_id)
        break
      }

      # Find the next manager
      next_manager <- emp_data %>%
        dplyr::filter(`EMP ID` == current) %>%
        dplyr::pull(`Manager ID`)

      if (length(next_manager) == 0 || is.na(next_manager[1])) {
        break
      }

      current <- next_manager[1]
    }
  }

  return(unique(cycles))
}


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

  # Build org tree if needed for full tree aggregation
  if (full_tree) {
    org_tree <- build_org_tree(survey)
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


