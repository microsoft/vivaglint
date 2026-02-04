#' Summarize Survey Questions
#'
#' Calculates comprehensive metrics for survey questions, returning a summary
#' analysis in tidy format. This includes mean, standard deviation, response counts,
#' skip counts, response rates, and favorability percentages.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param scale_points Integer specifying the number of scale points (2-11)
#' @param questions Character vector of question text(s) to analyze, or "all" to
#'   analyze all questions (default: "all")
#'
#' @return A tibble with one row per question containing:
#'   \describe{
#'     \item{question}{The question text}
#'     \item{mean}{Mean of numeric responses}
#'     \item{sd}{Standard deviation of numeric responses}
#'     \item{n_responses}{Count of non-blank, non-null responses}
#'     \item{n_skips}{Count of blank or null responses}
#'     \item{n_total}{Total number of respondents}
#'     \item{response_rate}{Proportion of respondents who saw question (rounded to 2 decimal places)}
#'     \item{pct_favorable}{Percentage of responses classified as favorable}
#'     \item{pct_neutral}{Percentage of responses classified as neutral}
#'     \item{pct_unfavorable}{Percentage of responses classified as unfavorable}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Summarize all questions (5-point scale)
#' summary <- summarize_survey(survey, scale_points = 5)
#'
#' # Summarize specific questions
#' summary_subset <- summarize_survey(survey, scale_points = 5,
#'   questions = c("My work is meaningful", "I feel valued"))
#' }
summarize_survey <- function(survey, scale_points, questions = "all") {
  # Validate scale_points
  if (!scale_points %in% 2:11) {
    stop("scale_points must be an integer between 2 and 11")
  }

  # Get favorability classification
  favorability <- get_favorability_map(scale_points)
  # Handle glint_survey objects
  if (inherits(survey, "glint_survey")) {
    data <- survey$data
    all_questions <- survey$metadata$questions$question
  } else {
    data <- survey
    all_questions <- extract_questions(survey)$question
  }

  # Determine which questions to analyze
  if (length(questions) == 1 && questions == "all") {
    questions_to_analyze <- all_questions
  } else {
    # Validate that requested questions exist
    missing_questions <- setdiff(questions, all_questions)
    if (length(missing_questions) > 0) {
      stop(sprintf(
        "Question(s) not found: %s\n\nAvailable questions:\n  %s",
        paste0("'", missing_questions, "'", collapse = ", "),
        paste0("  - ", all_questions, collapse = "\n")
      ))
    }
    questions_to_analyze <- questions
  }

  # Analyze each question
  results <- purrr::map_dfr(questions_to_analyze, function(question_text) {
    # Extract response column
    responses <- data[[question_text]]

    # Calculate total respondents
    n_total <- length(responses)

    # Separate responses into valid responses and skips
    valid_responses <- responses[!is.na(responses)]
    n_responses <- length(valid_responses)
    n_skips <- n_total - n_responses

    # Calculate mean and SD (only for valid responses)
    mean_response <- if (n_responses > 0) mean(valid_responses, na.rm = TRUE) else NA_real_
    sd_response <- if (n_responses > 1) sd(valid_responses, na.rm = TRUE) else NA_real_

    # Calculate response rate
    response_rate <- round((n_responses + n_skips) / n_total, 2)

    # Calculate favorability percentages
    if (n_responses > 0) {
      n_favorable <- sum(valid_responses %in% favorability$favorable)
      n_neutral <- sum(valid_responses %in% favorability$neutral)
      n_unfavorable <- sum(valid_responses %in% favorability$unfavorable)

      pct_favorable <- round((n_favorable / n_responses) * 100, 1)
      pct_neutral <- round((n_neutral / n_responses) * 100, 1)
      pct_unfavorable <- round((n_unfavorable / n_responses) * 100, 1)
    } else {
      pct_favorable <- NA_real_
      pct_neutral <- NA_real_
      pct_unfavorable <- NA_real_
    }

    # Create result tibble
    result <- dplyr::tibble(
      question = question_text,
      mean = mean_response,
      sd = sd_response,
      n_responses = n_responses,
      n_skips = n_skips,
      n_total = n_total,
      response_rate = response_rate,
      pct_favorable = pct_favorable,
      pct_neutral = pct_neutral,
      pct_unfavorable = pct_unfavorable
    )

    return(result)
  })

  return(results)
}


#' Get Response Distribution
#'
#' Calculates the distribution of response values for survey questions, returning
#' counts and percentages for each response value in tidy format.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param questions Character vector of question text(s) to analyze, or "all" to
#'   analyze all questions (default: "all")
#'
#' @return A tibble with one row per question containing:
#'   \describe{
#'     \item{question}{The question text}
#'     \item{count_X}{Count of responses with value X (for each unique response value)}
#'     \item{pct_X}{Percentage of responses with value X (for each unique response value)}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Get response distribution for all questions
#' dist <- get_response_dist(survey)
#'
#' # Get response distribution for specific questions
#' dist_subset <- get_response_dist(survey,
#'   questions = c("My work is meaningful", "I feel valued"))
#' }
get_response_dist <- function(survey, questions = "all") {
  # Handle glint_survey objects
  if (inherits(survey, "glint_survey")) {
    data <- survey$data
    all_questions <- survey$metadata$questions$question
  } else {
    data <- survey
    all_questions <- extract_questions(survey)$question
  }

  # Determine which questions to analyze
  if (length(questions) == 1 && questions == "all") {
    questions_to_analyze <- all_questions
  } else {
    # Validate that requested questions exist
    missing_questions <- setdiff(questions, all_questions)
    if (length(missing_questions) > 0) {
      stop(sprintf(
        "Question(s) not found: %s\n\nAvailable questions:\n  %s",
        paste0("'", missing_questions, "'", collapse = ", "),
        paste0("  - ", all_questions, collapse = "\n")
      ))
    }
    questions_to_analyze <- questions
  }

  # Analyze each question
  results <- purrr::map_dfr(questions_to_analyze, function(question_text) {
    # Extract response column
    responses <- data[[question_text]]

    # Get valid responses (non-NA)
    valid_responses <- responses[!is.na(responses)]

    # Calculate distribution of response values
    if (length(valid_responses) > 0) {
      value_table <- table(valid_responses)
      value_counts <- as.list(value_table)
      names(value_counts) <- paste0("count_", names(value_counts))

      value_percents <- as.list(prop.table(value_table) * 100)
      names(value_percents) <- paste0("pct_", names(value_percents))
    } else {
      value_counts <- list()
      value_percents <- list()
    }

    # Create result tibble with question
    result <- dplyr::tibble(question = question_text)

    # Add value counts and percents as list columns
    result$value_counts <- list(value_counts)
    result$value_percents <- list(value_percents)

    return(result)
  })

  # Expand value distributions into separate columns
  results <- expand_value_distributions(results)

  return(results)
}


#' Expand Value Distributions
#'
#' Internal function to expand value_counts and value_percents list columns
#' into separate columns for each response value.
#'
#' @param analysis_df A tibble from get_response_dist() with value_counts and value_percents list columns
#'
#' @return A tibble with expanded columns for each response value
#'
#' @keywords internal
expand_value_distributions <- function(analysis_df) {
  # Extract all unique response values across all questions
  all_values <- unique(unlist(lapply(analysis_df$value_counts, function(vc) {
    as.numeric(stringr::str_remove(names(vc), "count_"))
  })))
  all_values <- sort(all_values)

  # Create columns for each value
  for (val in all_values) {
    count_name <- paste0("count_", val)
    pct_name <- paste0("pct_", val)

    # Extract count for this value from each row
    analysis_df[[count_name]] <- sapply(analysis_df$value_counts, function(vc) {
      vc[[count_name]] %||% 0
    })

    # Extract percent for this value from each row
    analysis_df[[pct_name]] <- sapply(analysis_df$value_percents, function(vp) {
      vp[[pct_name]] %||% 0
    })
  }

  # Remove the list columns
  analysis_df$value_counts <- NULL
  analysis_df$value_percents <- NULL

  return(analysis_df)
}


#' Compare Survey Cycles
#'
#' Compares question-level metrics across multiple survey cycles, calculating
#' change scores and trends over time.
#'
#' @param ... Two or more glint_survey objects or data frames to compare
#' @param scale_points Integer specifying the number of scale points (2-11)
#' @param cycle_names Optional character vector of names for each cycle.
#'   If not provided, will use "Cycle 1", "Cycle 2", etc.
#'
#' @return A tibble with one row per question-cycle combination containing:
#'   \describe{
#'     \item{cycle}{The cycle name or number}
#'     \item{question}{The question text}
#'     \item{All metrics from summarize_survey()}{}
#'     \item{change_from_previous}{Change in mean from previous cycle (NA for first cycle)}
#'     \item{pct_change_from_previous}{Percent change from previous cycle}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey1 <- read_glint_survey("survey_2023_q1.csv")
#' survey2 <- read_glint_survey("survey_2023_q2.csv")
#' survey3 <- read_glint_survey("survey_2023_q3.csv")
#'
#' comparison <- compare_cycles(survey1, survey2, survey3,
#'                               scale_points = 5,
#'                               cycle_names = c("Q1 2023", "Q2 2023", "Q3 2023"))
#' print(comparison)
#' }
compare_cycles <- function(..., scale_points, cycle_names = NULL) {
  surveys <- list(...)

  if (length(surveys) < 2) {
    stop("At least two surveys are required for comparison")
  }

  # Generate cycle names if not provided
  if (is.null(cycle_names)) {
    cycle_names <- paste("Cycle", seq_along(surveys))
  } else if (length(cycle_names) != length(surveys)) {
    stop("Number of cycle_names must match number of surveys")
  }

  # Analyze each survey
  analyses <- purrr::map2_dfr(surveys, cycle_names, function(survey, cycle_name) {
    result <- summarize_survey(survey, scale_points = scale_points)
    result$cycle <- cycle_name
    result
  })

  # Reorder columns to put cycle first
  analyses <- analyses %>%
    dplyr::select(cycle, question, dplyr::everything())

  # Calculate changes from previous cycle
  analyses <- analyses %>%
    dplyr::arrange(question, cycle) %>%
    dplyr::group_by(question) %>%
    dplyr::mutate(
      change_from_previous = mean - dplyr::lag(mean),
      pct_change_from_previous = (mean - dplyr::lag(mean)) / dplyr::lag(mean) * 100
    ) %>%
    dplyr::ungroup()

  return(analyses)
}


#' Calculate Question Correlations
#'
#' Calculates correlations between all question response columns in the survey.
#' Supports multiple correlation methods and output formats.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param method Character string indicating the correlation method: "spearman"
#'   (default), "pearson", or "kendall"
#' @param format Character string indicating output format: "long" for long format
#'   with one row per question pair (default), or "matrix" for traditional
#'   correlation matrix
#' @param use Character string indicating how to handle missing values, passed to
#'   cor() function (default: "pairwise.complete.obs")
#'
#' @return If format = "long", a tibble with columns:
#'   \describe{
#'     \item{question1}{First question text}
#'     \item{question2}{Second question text}
#'     \item{correlation}{Correlation coefficient}
#'     \item{p_value}{P-value for test of correlation significance}
#'     \item{n}{Number of complete pairs used in calculation}
#'   }
#'   If format = "matrix", a matrix with questions as rows and columns
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Get correlations in long format (default)
#' correlations <- get_correlations(survey)
#'
#' # Get Pearson correlations
#' correlations_pearson <- get_correlations(survey, method = "pearson")
#'
#' # Get correlation matrix
#' cor_matrix <- get_correlations(survey, format = "matrix")
#' }
get_correlations <- function(survey, method = "spearman", format = "long", use = "pairwise.complete.obs") {
  # Validate method parameter
  if (!method %in% c("spearman", "pearson", "kendall")) {
    stop("method must be one of: 'spearman', 'pearson', or 'kendall'")
  }

  # Validate format parameter
  if (!format %in% c("long", "matrix")) {
    stop("format must be one of: 'long' or 'matrix'")
  }

  # Handle glint_survey objects
  if (inherits(survey, "glint_survey")) {
    data <- survey$data
    questions <- survey$metadata$questions$question
  } else {
    data <- survey
    questions <- extract_questions(data)$question
  }

  # Extract response columns
  response_data <- data[, questions, drop = FALSE]

  # Calculate correlation matrix
  cor_matrix <- cor(response_data, method = method, use = use)

  # Return based on format
  if (format == "matrix") {
    return(cor_matrix)
  } else {
    # Convert to long format
    long_data <- expand.grid(
      question1 = questions,
      question2 = questions,
      stringsAsFactors = FALSE
    )

    # Add correlation values
    long_data$correlation <- as.vector(cor_matrix)

    # Calculate n and p-values for each pair
    long_data$p_value <- NA_real_
    long_data$n <- NA_integer_

    for (i in seq_len(nrow(long_data))) {
      q1 <- long_data$question1[i]
      q2 <- long_data$question2[i]

      # Get complete pairs
      x <- response_data[[q1]]
      y <- response_data[[q2]]
      complete_pairs <- !is.na(x) & !is.na(y)
      n_complete <- sum(complete_pairs)

      long_data$n[i] <- n_complete

      # Calculate p-value using cor.test
      if (n_complete > 2 && q1 != q2) {
        test_result <- cor.test(
          x[complete_pairs],
          y[complete_pairs],
          method = method,
          exact = FALSE  # Use asymptotic p-value for consistency
        )
        long_data$p_value[i] <- test_result$p.value
      } else if (q1 == q2) {
        # Self-correlation always has p-value of 0
        long_data$p_value[i] <- 0
      }
    }

    # Convert to tibble
    long_data <- dplyr::as_tibble(long_data)

    # Reorder columns
    long_data <- long_data %>%
      dplyr::select(question1, question2, correlation, p_value, n)

    return(long_data)
  }
}


#' Extract Survey Factors
#'
#' Performs factor analysis on survey question responses to identify underlying
#' latent factors. Supports multiple rotation methods with oblique rotation as default.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param n_factors Integer indicating the number of factors to extract. If NULL
#'   (default), will use parallel analysis to determine optimal number of factors
#' @param rotation Character string indicating rotation method: "oblimin" (default),
#'   "varimax", "promax", "quartimax", "equamax", or "none"
#' @param min_loading Minimum factor loading to display in results (default: 0.3)
#' @param fm Character string indicating factoring method: "minres" (default),
#'   "ml" (maximum likelihood), "pa" (principal axis), "wls" (weighted least squares)
#'
#' @return A list containing:
#'   \describe{
#'     \item{loadings}{Matrix of factor loadings}
#'     \item{loadings_clean}{Tibble of loadings above min_loading threshold}
#'     \item{factor_correlations}{Correlation matrix between factors (for oblique rotations)}
#'     \item{variance_explained}{Tibble showing variance explained by each factor}
#'     \item{communalities}{Tibble showing communality (h2) for each question}
#'     \item{fa_object}{Original fa object from psych package for further analysis}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Extract factors with default settings (oblique rotation)
#' factors <- extract_survey_factors(survey)
#'
#' # Extract 3 factors with varimax rotation
#' factors <- extract_survey_factors(survey, n_factors = 3, rotation = "varimax")
#'
#' # View clean loadings
#' print(factors$loadings_clean)
#'
#' # View variance explained
#' print(factors$variance_explained)
#' }
extract_survey_factors <- function(survey, n_factors = NULL, rotation = "oblimin",
                                   min_loading = 0.3, fm = "minres") {
  # Check if psych package is available
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package 'psych' is required for factor analysis. Install it with: install.packages('psych')")
  }

  # Validate rotation parameter
  valid_rotations <- c("oblimin", "varimax", "promax", "quartimax", "equamax", "none")
  if (!rotation %in% valid_rotations) {
    stop(sprintf(
      "rotation must be one of: %s",
      paste0("'", valid_rotations, "'", collapse = ", ")
    ))
  }

  # Validate fm parameter
  valid_fm <- c("minres", "ml", "pa", "wls", "gls", "uls")
  if (!fm %in% valid_fm) {
    stop(sprintf(
      "fm must be one of: %s",
      paste0("'", valid_fm, "'", collapse = ", ")
    ))
  }

  # Handle glint_survey objects
  if (inherits(survey, "glint_survey")) {
    data <- survey$data
    questions <- survey$metadata$questions$question
  } else {
    data <- survey
    questions <- extract_questions(data)$question
  }

  # Extract response columns
  response_data <- data[, questions, drop = FALSE]

  # Remove cases with all missing values
  response_data <- response_data[rowSums(!is.na(response_data)) > 0, ]

  # Determine number of factors if not specified
  if (is.null(n_factors)) {
    message("Determining optimal number of factors using parallel analysis...")
    pa_result <- psych::fa.parallel(
      response_data,
      fa = "fa",
      fm = fm,
      plot = FALSE,
      n.iter = 20
    )
    n_factors <- pa_result$nfact
    message(sprintf("Parallel analysis suggests %d factor(s)", n_factors))
  }

  # Validate n_factors
  if (n_factors < 1 || n_factors > ncol(response_data)) {
    stop(sprintf(
      "n_factors must be between 1 and %d (number of questions)",
      ncol(response_data)
    ))
  }

  # Perform factor analysis
  fa_result <- psych::fa(
    response_data,
    nfactors = n_factors,
    rotate = rotation,
    fm = fm
  )

  # Extract loadings matrix
  loadings_matrix <- fa_result$loadings
  class(loadings_matrix) <- "matrix"

  # Create clean loadings tibble
  loadings_long <- expand.grid(
    question = rownames(loadings_matrix),
    factor = colnames(loadings_matrix),
    stringsAsFactors = FALSE
  )

  loadings_long$loading <- as.vector(loadings_matrix)

  # Filter by minimum loading
  loadings_clean <- loadings_long %>%
    dplyr::as_tibble() %>%
    dplyr::filter(abs(loading) >= min_loading) %>%
    dplyr::arrange(factor, dplyr::desc(abs(loading)))

  # Extract variance explained
  # Handle both single and multiple factor cases
  if (n_factors == 1) {
    variance_exp <- dplyr::tibble(
      factor = colnames(loadings_matrix),
      ss_loadings = fa_result$values[1],
      prop_var = fa_result$values[1] / ncol(response_data),
      cum_var = fa_result$values[1] / ncol(response_data)
    )
  } else {
    variance_exp <- dplyr::tibble(
      factor = colnames(loadings_matrix),
      ss_loadings = fa_result$Vaccount["SS loadings", ],
      prop_var = fa_result$Vaccount["Proportion Var", ],
      cum_var = fa_result$Vaccount["Cumulative Var", ]
    )
  }

  # Extract communalities
  communalities <- dplyr::tibble(
    question = names(fa_result$communality),
    communality = fa_result$communality,
    uniqueness = fa_result$uniquenesses
  ) %>%
    dplyr::arrange(dplyr::desc(communality))

  # Extract factor correlations (for oblique rotations)
  factor_cors <- NULL
  if (rotation %in% c("oblimin", "promax") && n_factors > 1) {
    if (!is.null(fa_result$Phi)) {
      factor_cors <- fa_result$Phi
    }
  }

  # Return results
  result <- list(
    loadings = loadings_matrix,
    loadings_clean = loadings_clean,
    factor_correlations = factor_cors,
    variance_explained = variance_exp,
    communalities = communalities,
    fa_object = fa_result
  )

  class(result) <- c("survey_factors", "list")
  return(result)
}


#' Analyze Employee Attrition
#'
#' Analyzes the relationship between survey responses and employee attrition,
#' calculating how much more (or less) likely employees are to leave within
#' specified time periods if they respond unfavorably vs favorably to survey questions.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param attrition_file Path to CSV file containing employee attrition data
#' @param emp_id_col Character string specifying the column name for employee ID
#' @param term_date_col Character string specifying the column name for termination date
#' @param scale_points Integer specifying the number of scale points (2-11)
#' @param time_periods Integer vector specifying time periods in days to analyze
#'   (default: c(30, 90, 180))
#'
#' @return A tibble with one row per question-time period combination containing:
#'   \describe{
#'     \item{question}{The question text}
#'     \item{days}{The time period in days}
#'     \item{favorable_n}{Number of employees who responded favorably}
#'     \item{favorable_attrition}{Proportion who left within time period (favorable)}
#'     \item{unfavorable_n}{Number of employees who responded unfavorably}
#'     \item{unfavorable_attrition}{Proportion who left within time period (unfavorable)}
#'     \item{attrition_ratio}{Ratio of unfavorable to favorable attrition rates}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Analyze attrition for 5-point scale survey
#' attrition <- analyze_attrition(
#'   survey,
#'   attrition_file = "employee_attributes.csv",
#'   emp_id_col = "EMP ID",
#'   term_date_col = "Termination Date",
#'   scale_points = 5,
#'   time_periods = c(30, 90, 180)
#' )
#' print(attrition)
#' }
analyze_attrition <- function(survey, attrition_file, emp_id_col, term_date_col,
                              scale_points, time_periods = c(30, 90, 180)) {
  # Validate scale_points
  if (!scale_points %in% 2:11) {
    stop("scale_points must be an integer between 2 and 11")
  }

  # Get favorability classification
  favorability <- get_favorability_map(scale_points)

  # Handle glint_survey objects
  if (inherits(survey, "glint_survey")) {
    survey_data <- survey$data
    questions <- survey$metadata$questions$question
  } else {
    survey_data <- survey
    questions <- extract_questions(survey)$question
  }

  # Read attrition file
  if (!file.exists(attrition_file)) {
    stop(sprintf("Attrition file not found: '%s'", attrition_file))
  }

  attrition_data <- readr::read_csv(attrition_file, show_col_types = FALSE)

  # Validate columns exist
  if (!emp_id_col %in% names(attrition_data)) {
    stop(sprintf("Column '%s' not found in attrition file", emp_id_col))
  }
  if (!term_date_col %in% names(attrition_data)) {
    stop(sprintf("Column '%s' not found in attrition file", term_date_col))
  }
  if (!emp_id_col %in% names(survey_data)) {
    stop(sprintf("Column '%s' not found in survey data", emp_id_col))
  }

  # Parse termination dates (try multiple formats)
  attrition_data[[term_date_col]] <- tryCatch(
    {
      # Try different date parsing functions
      parsed <- lubridate::parse_date_time(
        attrition_data[[term_date_col]],
        orders = c("ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS")
      )
      as.Date(parsed)
    },
    error = function(e) {
      stop(sprintf(
        "Error parsing termination dates in column '%s': %s",
        term_date_col, e$message
      ))
    }
  )

  # Get survey completion date
  if (!"Survey Cycle Completion Date" %in% names(survey_data)) {
    stop("Survey data must contain 'Survey Cycle Completion Date' column")
  }

  # Join survey data with attrition data
  combined_data <- survey_data %>%
    dplyr::left_join(
      attrition_data %>% dplyr::select(dplyr::all_of(c(emp_id_col, term_date_col))),
      by = emp_id_col
    )

  # Convert survey completion date to Date
  combined_data$survey_date <- as.Date(combined_data$`Survey Cycle Completion Date`)

  # Calculate days to termination
  combined_data$days_to_term <- as.numeric(
    combined_data[[term_date_col]] - combined_data$survey_date
  )

  # Analyze each question for each time period
  results <- purrr::map_dfr(questions, function(question_text) {
    # Get responses for this question
    responses <- combined_data[[question_text]]

    # Classify responses as favorable/unfavorable
    response_class <- dplyr::case_when(
      responses %in% favorability$favorable ~ "favorable",
      responses %in% favorability$unfavorable ~ "unfavorable",
      TRUE ~ "neutral"
    )

    # Analyze for each time period
    purrr::map_dfr(time_periods, function(days) {
      # Determine if employee left within time period
      left_within_period <- !is.na(combined_data$days_to_term) &
        combined_data$days_to_term > 0 &
        combined_data$days_to_term <= days

      # Calculate attrition for favorable responses
      favorable_mask <- response_class == "favorable" & !is.na(responses)
      favorable_n <- sum(favorable_mask)
      favorable_attrition <- if (favorable_n > 0) {
        sum(left_within_period[favorable_mask]) / favorable_n
      } else {
        NA_real_
      }

      # Calculate attrition for unfavorable responses
      unfavorable_mask <- response_class == "unfavorable" & !is.na(responses)
      unfavorable_n <- sum(unfavorable_mask)
      unfavorable_attrition <- if (unfavorable_n > 0) {
        sum(left_within_period[unfavorable_mask]) / unfavorable_n
      } else {
        NA_real_
      }

      # Calculate ratio
      attrition_ratio <- if (!is.na(favorable_attrition) && !is.na(unfavorable_attrition)) {
        if (favorable_attrition == 0 && unfavorable_attrition == 0) {
          NA_real_  # No attrition in either group
        } else if (favorable_attrition == 0 && unfavorable_attrition > 0) {
          Inf  # Infinite risk ratio when favorable has no attrition
        } else {
          unfavorable_attrition / favorable_attrition
        }
      } else {
        NA_real_
      }

      dplyr::tibble(
        question = question_text,
        days = days,
        favorable_n = favorable_n,
        favorable_attrition = round(favorable_attrition, 4),
        unfavorable_n = unfavorable_n,
        unfavorable_attrition = round(unfavorable_attrition, 4),
        attrition_ratio = ifelse(is.finite(attrition_ratio), round(attrition_ratio, 2), attrition_ratio)
      )
    })
  })

  return(results)
}


#' Analyze Survey Responses by Attributes
#'
#' Aggregates survey responses by employee attributes (e.g., Department,
#' Gender, Tenure) and calculates the same metrics as summarize_survey() for
#' each attribute group combination. Only groups meeting the minimum size
#' threshold are included in the results.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param attribute_file Character string path to CSV file containing employee
#'   attributes
#' @param scale_points Integer specifying the number of scale points (2-11)
#' @param attribute_cols Character vector of column names from the attribute
#'   file to group by (e.g., c("Department", "Gender", "Tenure Group"))
#' @param emp_id_col Character string specifying the employee ID column name
#'   in the attribute file (default: "EMP ID")
#' @param min_group_size Integer specifying the minimum number of employees
#'   required for an attribute group to be included in results (default: 5)
#'
#' @return A tibble with one row per attribute-group-question combination
#'   containing:
#'   \describe{
#'     \item{attribute columns}{Values for each attribute grouping variable}
#'     \item{group_size}{Number of employees in this attribute group}
#'     \item{All columns from summarize_survey()}{question, n_responses, n_skips,
#'       n_total, response_rate, mean, sd, pct_favorable, pct_neutral,
#'       pct_unfavorable}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Analyze by single attribute
#' dept_analysis <- analyze_by_attributes(
#'   survey,
#'   attribute_file = "employee_attributes.csv",
#'   scale_points = 5,
#'   attribute_cols = "Department"
#' )
#'
#' # Analyze by multiple attributes
#' multi_analysis <- analyze_by_attributes(
#'   survey,
#'   attribute_file = "employee_attributes.csv",
#'   scale_points = 5,
#'   attribute_cols = c("Department", "Gender", "Tenure Group"),
#'   min_group_size = 10
#' )
#' }
analyze_by_attributes <- function(survey, attribute_file, scale_points,
                                 attribute_cols, emp_id_col = "EMP ID",
                                 min_group_size = 5) {
  # Validate scale_points
  if (!scale_points %in% 2:11) {
    stop("scale_points must be an integer between 2 and 11")
  }

  # Handle glint_survey objects
  if (inherits(survey, "glint_survey")) {
    survey_data <- survey$data
  } else {
    survey_data <- survey
  }

  # Read attribute file
  if (!file.exists(attribute_file)) {
    stop(paste0("Attribute file not found: ", attribute_file))
  }

  attributes <- readr::read_csv(
    attribute_file,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  # Validate emp_id_col exists in attribute file
  if (!emp_id_col %in% names(attributes)) {
    stop(paste0("Column '", emp_id_col, "' not found in attribute file"))
  }

  # Validate attribute_cols exist in attribute file
  missing_cols <- setdiff(attribute_cols, names(attributes))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Attribute column(s) not found in attribute file: ",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Join survey data with attributes
  combined_data <- survey_data %>%
    dplyr::left_join(
      attributes %>% dplyr::select(dplyr::all_of(c(emp_id_col, attribute_cols))),
      by = c("EMP ID" = emp_id_col)
    )

  # Get all unique attribute combinations
  attribute_groups <- combined_data %>%
    dplyr::select(dplyr::all_of(attribute_cols), `EMP ID`) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(attribute_cols))) %>%
    dplyr::summarise(
      group_size = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(group_size >= min_group_size)

  if (nrow(attribute_groups) == 0) {
    warning("No attribute groups meet the minimum size threshold")
    return(dplyr::tibble())
  }

  # Get question columns (excluding attributes and standard columns)
  standard_cols <- get_standard_columns()
  all_cols <- names(survey_data)
  question_cols <- setdiff(all_cols, standard_cols)

  # Analyze each attribute group
  results <- purrr::map_dfr(seq_len(nrow(attribute_groups)), function(i) {
    group_values <- attribute_groups[i, attribute_cols, drop = FALSE]
    group_size <- attribute_groups$group_size[i]

    # Filter survey data to this attribute group
    group_data <- combined_data
    for (col in attribute_cols) {
      group_data <- group_data %>%
        dplyr::filter(!!rlang::sym(col) == group_values[[col]])
    }

    # Select only the original survey columns (remove attribute columns)
    group_data_survey <- group_data %>%
      dplyr::select(dplyr::all_of(c(standard_cols, question_cols)))

    # Run summarize_survey for this group
    group_summary <- summarize_survey(group_data_survey, scale_points = scale_points, questions = "all")

    # Add attribute columns and group size
    group_summary <- dplyr::bind_cols(
      group_values,
      dplyr::tibble(group_size = group_size),
      group_summary
    )

    return(group_summary)
  })

  return(results)
}


# Define the %||% operator for use in expand_value_distributions
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
