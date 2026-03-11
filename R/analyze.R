#' Summarize Survey Questions
#'
#' Calculates comprehensive metrics for survey questions, returning a summary
#' analysis in tidy format. This includes mean, standard deviation, Glint Score,
#' response counts, skip counts, and favorability percentages.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param scale_points Integer specifying the number of scale points (2-11)
#' @param questions Character vector of question text(s) to analyze, or "all" to
#'   analyze all questions (default: "all")
#' @param plot Logical. If \code{TRUE}, prints a favorability stacked bar chart
#'   sorted by Glint Score and returns the data invisibly. Requires
#'   \pkg{ggplot2}. Default: \code{FALSE}.
#'
#' @return A tibble with one row per question containing:
#'   \describe{
#'     \item{question}{The question text}
#'     \item{mean}{Mean of numeric responses (raw scale)}
#'     \item{sd}{Standard deviation of numeric responses}
#'     \item{glint_score}{Mean transformed to a 0-100 scale, matching the score
#'       displayed in the Viva Glint UI: \code{round(((mean - 1) / (scale_points - 1)) * 100)}}
#'     \item{n_responses}{Count of non-blank, non-null responses}
#'     \item{n_skips}{Count of blank or null responses}
#'     \item{n_total}{Total number of respondents}
#'     \item{pct_favorable}{Percentage of responses classified as favorable}
#'     \item{pct_neutral}{Percentage of responses classified as neutral}
#'     \item{pct_unfavorable}{Percentage of responses classified as unfavorable}
#'   }
#'   When \code{plot = TRUE}, the same tibble is returned invisibly after
#'   printing the plot.
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
#'
#' # With favorability chart
#' summarize_survey(survey, scale_points = 5, plot = TRUE)
#' }
summarize_survey <- function(survey, scale_points, questions = "all",
                             emp_id_col = NULL, plot = FALSE) {
  if (plot) .check_ggplot2()
  if (!scale_points %in% 2:11) {
    stop("scale_points must be an integer between 2 and 11")
  }

  favorability <- get_favorability_map(scale_points)

  if (inherits(survey, "glint_survey")) {
    emp_id_col <- emp_id_col %||% survey$metadata$emp_id_col
    data <- survey$data
    all_questions <- survey$metadata$questions$question
  } else {
    data <- survey
    all_questions <- extract_questions(survey, emp_id_col)$question
  }

  if (length(questions) == 1 && questions == "all") {
    questions_to_analyze <- all_questions
  } else {
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

  results <- purrr::map_dfr(questions_to_analyze, function(question_text) {
    responses <- data[[question_text]]
    n_total <- length(responses)

    valid_responses <- responses[!is.na(responses)]
    n_responses <- length(valid_responses)
    n_skips <- n_total - n_responses

    mean_response <- if (n_responses > 0) mean(valid_responses, na.rm = TRUE) else NA_real_
    sd_response <- if (n_responses > 1) sd(valid_responses, na.rm = TRUE) else NA_real_

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

    result <- dplyr::tibble(
      question = question_text,
      mean = mean_response,
      sd = sd_response,
      glint_score = mean_to_glint_score(mean_response, scale_points),
      n_responses = n_responses,
      n_skips = n_skips,
      n_total = n_total,
      pct_favorable = pct_favorable,
      pct_neutral = pct_neutral,
      pct_unfavorable = pct_unfavorable
    )

    return(result)
  })

  if (plot) {
    print(.plot_survey_summary(results))
    return(invisible(results))
  }

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
#' @param plot Logical. If \code{TRUE}, prints a stacked bar chart of response
#'   value distributions (red \eqn{\to} blue gradient) and returns the data
#'   invisibly. Requires \pkg{ggplot2}. Default: \code{FALSE}.
#'
#' @return A tibble with one row per question containing:
#'   \describe{
#'     \item{question}{The question text}
#'     \item{count_X}{Count of responses with value X (for each unique response value)}
#'     \item{pct_X}{Percentage of responses with value X (for each unique response value)}
#'   }
#'   When \code{plot = TRUE}, the same tibble is returned invisibly after
#'   printing the plot.
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
#'
#' # With distribution chart
#' get_response_dist(survey, plot = TRUE)
#' }
get_response_dist <- function(survey, questions = "all", plot = FALSE) {
  if (plot) .check_ggplot2()
  if (inherits(survey, "glint_survey")) {
    data <- survey$data
    all_questions <- survey$metadata$questions$question
  } else {
    data <- survey
    all_questions <- extract_questions(survey)$question
  }

  if (length(questions) == 1 && questions == "all") {
    questions_to_analyze <- all_questions
  } else {
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

  results <- purrr::map_dfr(questions_to_analyze, function(question_text) {
    responses <- data[[question_text]]
    valid_responses <- responses[!is.na(responses)]

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

    result <- dplyr::tibble(question = question_text)
    result$value_counts <- list(value_counts)
    result$value_percents <- list(value_percents)

    return(result)
  })

  results <- expand_value_distributions(results)

  if (plot) {
    print(.plot_response_dist(results))
    return(invisible(results))
  }

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
  all_values <- unique(unlist(lapply(analysis_df$value_counts, function(vc) {
    as.numeric(stringr::str_remove(names(vc), "count_"))
  })))
  all_values <- sort(all_values)

  for (val in all_values) {
    count_name <- paste0("count_", val)
    pct_name <- paste0("pct_", val)

    analysis_df[[count_name]] <- sapply(analysis_df$value_counts, function(vc) {
      vc[[count_name]] %||% 0
    })

    analysis_df[[pct_name]] <- sapply(analysis_df$value_percents, function(vp) {
      vp[[pct_name]] %||% 0
    })
  }

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
#' @param plot Logical. If \code{TRUE}, prints a line chart of Glint Score over
#'   cycles (one line per question) and returns the data invisibly. Requires
#'   \pkg{ggplot2}. Default: \code{FALSE}.
#'
#' @return A tibble with one row per question-cycle combination containing:
#'   \describe{
#'     \item{cycle}{The cycle name or number}
#'     \item{question}{The question text}
#'     \item{mean, sd, n_responses, n_skips, n_total}{Descriptive
#'       statistics for this question in this cycle (see \code{summarize_survey()})}
#'     \item{pct_favorable, pct_neutral, pct_unfavorable}{Favorability
#'       percentages for this question in this cycle}
#'     \item{change_from_previous}{Change in raw mean score from the previous
#'       cycle (NA for the first cycle)}
#'     \item{pct_change_from_previous}{Percentage change in raw mean score from
#'       the previous cycle (NA for the first cycle)}
#'     \item{glint_score_change_from_previous}{Change in Glint Score (0-100
#'       scale) from the previous cycle (NA for the first cycle)}
#'   }
#'   When \code{plot = TRUE}, the same tibble is returned invisibly after
#'   printing the plot.
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
#'
#' # With trend chart
#' compare_cycles(survey1, survey2, survey3, scale_points = 5,
#'                cycle_names = c("Q1 2023", "Q2 2023", "Q3 2023"),
#'                plot = TRUE)
#' }
compare_cycles <- function(..., scale_points, cycle_names = NULL,
                           plot = FALSE) {
  if (plot) .check_ggplot2()
  surveys <- list(...)

  if (length(surveys) < 2) {
    stop("At least two surveys are required for comparison")
  }

  if (is.null(cycle_names)) {
    cycle_names <- paste("Cycle", seq_along(surveys))
  } else if (length(cycle_names) != length(surveys)) {
    stop("Number of cycle_names must match number of surveys")
  }

  analyses <- purrr::map2_dfr(surveys, cycle_names, function(survey, cycle_name) {
    result <- summarize_survey(survey, scale_points = scale_points)
    result$cycle <- cycle_name
    result
  })

  # Reorder columns to put cycle first
  analyses <- analyses %>%
    dplyr::select(cycle, question, dplyr::everything())

  analyses <- analyses %>%
    dplyr::arrange(question, cycle) %>%
    dplyr::group_by(question) %>%
    dplyr::mutate(
      change_from_previous = mean - dplyr::lag(mean),
      pct_change_from_previous = (mean - dplyr::lag(mean)) / dplyr::lag(mean) * 100,
      glint_score_change_from_previous = glint_score - dplyr::lag(glint_score)
    ) %>%
    dplyr::ungroup()

  if (plot) {
    print(.plot_compare_cycles(analyses, cycle_names))
    return(invisible(analyses))
  }

  return(analyses)
}


#' Calculate Question Correlations
#'
#' Calculates correlations between all question response columns in the survey.
#' Supports multiple correlation methods and output formats.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param method Character string indicating the correlation method: "pearson"
#'   (default), "spearman", or "kendall"
#' @param format Character string indicating output format: "long" for long format
#'   with one row per question pair (default), or "matrix" for traditional
#'   correlation matrix
#' @param use Character string indicating how to handle missing values, passed to
#'   cor() function (default: "pairwise.complete.obs")
#' @param plot Logical. If \code{TRUE}, prints a correlation heatmap and returns
#'   the data invisibly. Only supported when \code{format = "long"} (the
#'   default); ignored with a warning when \code{format = "matrix"}. Requires
#'   \pkg{ggplot2}. Default: \code{FALSE}.
#'
#' @return If format = "long", a tibble with columns:
#'   \describe{
#'     \item{question1}{First question text}
#'     \item{question2}{Second question text}
#'     \item{correlation}{Correlation coefficient}
#'     \item{p_value}{P-value for test of correlation significance}
#'     \item{n}{Number of complete pairs used in calculation}
#'   }
#'   If format = "matrix", a matrix with questions as rows and columns.
#'   When \code{plot = TRUE} and \code{format = "long"}, the tibble is returned
#'   invisibly after printing the plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Get Pearson correlations in long format (default)
#' correlations <- get_correlations(survey)
#'
#' # Get Spearman correlations
#' correlations_spearman <- get_correlations(survey, method = "spearman")
#'
#' # Get correlation matrix
#' cor_matrix <- get_correlations(survey, format = "matrix")
#'
#' # With correlation heatmap
#' get_correlations(survey, plot = TRUE)
#' }
get_correlations <- function(survey, method = "pearson", format = "long",
                             use = "pairwise.complete.obs", plot = FALSE) {
  if (plot) .check_ggplot2()
  if (!method %in% c("spearman", "pearson", "kendall")) {
    stop("method must be one of: 'spearman', 'pearson', or 'kendall'")
  }

  if (!format %in% c("long", "matrix")) {
    stop("format must be one of: 'long' or 'matrix'")
  }

  if (inherits(survey, "glint_survey")) {
    data <- survey$data
    questions <- survey$metadata$questions$question
  } else {
    data <- survey
    questions <- extract_questions(data)$question
  }

  response_data <- data[, questions, drop = FALSE]
  cor_matrix <- cor(response_data, method = method, use = use)

  if (format == "matrix") {
    if (plot) {
      warning("plot = TRUE is not supported when format = 'matrix'. Returning matrix without plot.")
    }
    return(cor_matrix)
  } else {
    long_data <- expand.grid(
      question1 = questions,
      question2 = questions,
      stringsAsFactors = FALSE
    )

    long_data$correlation <- as.vector(cor_matrix)
    long_data$p_value <- NA_real_
    long_data$n <- NA_integer_

    for (i in seq_len(nrow(long_data))) {
      q1 <- long_data$question1[i]
      q2 <- long_data$question2[i]

      x <- response_data[[q1]]
      y <- response_data[[q2]]
      complete_pairs <- !is.na(x) & !is.na(y)
      n_complete <- sum(complete_pairs)

      long_data$n[i] <- n_complete

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

    long_data <- dplyr::as_tibble(long_data) %>%
      dplyr::select(question1, question2, correlation, p_value, n)

    if (plot) {
      print(.plot_correlations(long_data))
      return(invisible(long_data))
    }

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
#' @param fm Character string indicating factoring method: "minres" (minimum
#'   residuals, default), "ml" (maximum likelihood), "pa" (principal axis),
#'   "wls" (weighted least squares), "gls" (generalized least squares), or
#'   "uls" (unweighted least squares)
#' @param plot Logical. If \code{TRUE}, prints a factor loading heatmap and
#'   returns the result list invisibly. Requires \pkg{ggplot2}. Default:
#'   \code{FALSE}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{factor_summary}{A tibble with one row per question-factor combination
#'       (filtered to \code{abs(loading) >= min_loading}), containing:
#'       \code{question} (item text), \code{factor} (factor name),
#'       \code{loading} (loading coefficient), \code{loading_label}
#'       ("Strong" >= 0.75 / "Medium" 0.60-0.74 / "Weak" < 0.60),
#'       \code{communality} (proportion of item variance explained by all factors),
#'       and \code{factor_variance_pct} (percentage of total variance explained
#'       by this factor). Sorted by factor then descending loading strength.}
#'     \item{fa_object}{Original fa object from psych package for further analysis}
#'   }
#'   When \code{plot = TRUE}, the same list is returned invisibly after printing
#'   the plot.
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
#' # View consolidated factor summary
#' print(factors$factor_summary)
#'
#' # Filter to strong loaders only
#' strong <- dplyr::filter(factors$factor_summary, loading_label == "Strong")
#'
#' # With factor loading heatmap
#' extract_survey_factors(survey, plot = TRUE)
#' }
extract_survey_factors <- function(survey, n_factors = NULL, rotation = "oblimin",
                                   min_loading = 0.3, fm = "minres",
                                   plot = FALSE) {
  if (plot) .check_ggplot2()
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package 'psych' is required for factor analysis. Install it with: install.packages('psych')")
  }

  valid_rotations <- c("oblimin", "varimax", "promax", "quartimax", "equamax", "none")
  if (!rotation %in% valid_rotations) {
    stop(sprintf(
      "rotation must be one of: %s",
      paste0("'", valid_rotations, "'", collapse = ", ")
    ))
  }

  valid_fm <- c("minres", "ml", "pa", "wls", "gls", "uls")
  if (!fm %in% valid_fm) {
    stop(sprintf(
      "fm must be one of: %s",
      paste0("'", valid_fm, "'", collapse = ", ")
    ))
  }

  if (inherits(survey, "glint_survey")) {
    data <- survey$data
    questions <- survey$metadata$questions$question
  } else {
    data <- survey
    questions <- extract_questions(data)$question
  }

  response_data <- data[, questions, drop = FALSE]

  # Remove cases with all missing values
  response_data <- response_data[rowSums(!is.na(response_data)) > 0, ]

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

  if (n_factors < 1 || n_factors > ncol(response_data)) {
    stop(sprintf(
      "n_factors must be between 1 and %d (number of questions)",
      ncol(response_data)
    ))
  }

  fa_result <- psych::fa(
    response_data,
    nfactors = n_factors,
    rotate = rotation,
    fm = fm
  )

  loadings_matrix <- fa_result$loadings
  class(loadings_matrix) <- "matrix"

  # Build long-format loadings (all question × factor combinations)
  loadings_long <- expand.grid(
    question = rownames(loadings_matrix),
    factor = colnames(loadings_matrix),
    stringsAsFactors = FALSE
  )
  loadings_long$loading <- as.vector(loadings_matrix)

  # Build communality lookup (per question)
  communality_lookup <- dplyr::tibble(
    question = names(fa_result$communality),
    communality = fa_result$communality
  )

  # Build factor variance lookup (per factor)
  if (n_factors == 1) {
    factor_variance_pct <- fa_result$values[1] / ncol(response_data) * 100
  } else {
    factor_variance_pct <- fa_result$Vaccount["Proportion Var", ] * 100
  }
  variance_lookup <- dplyr::tibble(
    factor = colnames(loadings_matrix),
    factor_variance_pct = factor_variance_pct
  )

  # Assemble consolidated factor summary
  factor_summary <- dplyr::as_tibble(loadings_long) %>%
    dplyr::filter(abs(loading) >= min_loading) %>%
    dplyr::mutate(
      loading_label = dplyr::case_when(
        abs(loading) >= 0.75 ~ "Strong",
        abs(loading) <  0.60 ~ "Weak",
        TRUE                 ~ "Medium"
      )
    ) %>%
    dplyr::left_join(communality_lookup, by = "question") %>%
    dplyr::left_join(variance_lookup, by = "factor") %>%
    dplyr::arrange(factor, dplyr::desc(abs(loading))) %>%
    dplyr::select(question, factor, loading, loading_label, communality,
                  factor_variance_pct)

  result <- list(
    factor_summary = factor_summary,
    fa_object = fa_result
  )

  class(result) <- c("survey_factors", "list")

  if (plot) {
    print(.plot_survey_factors(result$factor_summary))
    return(invisible(result))
  }

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
#'   (default: c(90, 180, 365))
#' @param attribute_cols Optional character vector of attribute column names to
#'   segment results by (e.g., \code{c("Department", "Gender")}). Attributes must
#'   already be joined to the survey via \code{join_attributes()}. When \code{NULL}
#'   (default), results are returned for the overall population.
#' @param min_group_size Minimum number of employees required in an attribute group
#'   for it to be included in results (default: 5). Ignored when
#'   \code{attribute_cols = NULL}.
#' @param plot Logical. If \code{TRUE}, prints a grouped bar chart of favorable
#'   vs. unfavorable attrition rates faceted by time period and returns the data
#'   invisibly. When \code{attribute_cols} is supplied, the first attribute
#'   column is used as an additional facet dimension. Requires \pkg{ggplot2}.
#'   Default: \code{FALSE}.
#'
#' @return A tibble with one row per (attribute group)-question-time period
#'   combination containing:
#'   \describe{
#'     \item{attribute columns}{One column per entry in \code{attribute_cols}
#'       (only present when \code{attribute_cols} is supplied)}
#'     \item{group_size}{Number of employees in the attribute group (only present
#'       when \code{attribute_cols} is supplied)}
#'     \item{question}{The question text}
#'     \item{days}{The time period in days}
#'     \item{favorable_n}{Number of employees who responded favorably}
#'     \item{favorable_attrition}{Proportion who left within time period (favorable)}
#'     \item{unfavorable_n}{Number of employees who responded unfavorably}
#'     \item{unfavorable_attrition}{Proportion who left within time period (unfavorable)}
#'     \item{attrition_ratio}{Ratio of unfavorable to favorable attrition rates}
#'   }
#'   When \code{plot = TRUE}, the same tibble is returned invisibly after
#'   printing the plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Overall attrition analysis
#' attrition <- analyze_attrition(
#'   survey,
#'   attrition_file = "attrition.csv",
#'   emp_id_col = "EMP ID",
#'   term_date_col = "Termination Date",
#'   scale_points = 5
#' )
#'
#' # Attrition segmented by Department and Gender
#' survey_enriched <- join_attributes(survey, "employee_attributes.csv")
#' attrition_by_dept <- analyze_attrition(
#'   survey_enriched,
#'   attrition_file = "attrition.csv",
#'   emp_id_col = "EMP ID",
#'   term_date_col = "Termination Date",
#'   scale_points = 5,
#'   attribute_cols = c("Department", "Gender"),
#'   min_group_size = 10
#' )
#'
#' # With attrition chart
#' analyze_attrition(survey, attrition_file = "attrition.csv",
#'                   emp_id_col = "EMP ID", term_date_col = "Termination Date",
#'                   scale_points = 5, plot = TRUE)
#' }
analyze_attrition <- function(survey, attrition_file, emp_id_col = NULL, term_date_col,
                              scale_points, time_periods = c(90, 180, 365),
                              attribute_cols = NULL, min_group_size = 5,
                              plot = FALSE) {
  if (plot) .check_ggplot2()
  if (!scale_points %in% 2:11) {
    stop("scale_points must be an integer between 2 and 11")
  }

  favorability <- get_favorability_map(scale_points)

  if (inherits(survey, "glint_survey")) {
    emp_id_col <- emp_id_col %||% survey$metadata$emp_id_col
    survey_data <- survey$data
    questions <- survey$metadata$questions$question
  } else {
    survey_data <- survey
    questions <- extract_questions(survey)$question
  }

  if (is.null(emp_id_col)) {
    stop("emp_id_col must be specified when survey is a plain data frame")
  }

  if (!file.exists(attrition_file)) {
    stop(sprintf("Attrition file not found: '%s'", attrition_file))
  }

  attrition_data <- readr::read_csv(attrition_file, show_col_types = FALSE)

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

  if (!"Survey Cycle Completion Date" %in% names(survey_data)) {
    stop("Survey data must contain 'Survey Cycle Completion Date' column")
  }

  combined_data <- survey_data %>%
    dplyr::left_join(
      attrition_data %>% dplyr::select(dplyr::all_of(c(emp_id_col, term_date_col))),
      by = emp_id_col
    )

  combined_data$survey_date <- as.Date(combined_data$`Survey Cycle Completion Date`)
  combined_data$days_to_term <- as.numeric(
    combined_data[[term_date_col]] - combined_data$survey_date
  )

  if (!is.null(attribute_cols)) {
    missing_attr <- setdiff(attribute_cols, names(combined_data))
    if (length(missing_attr) > 0) {
      stop(paste0(
        "Attribute column(s) not found in survey data: ",
        paste(missing_attr, collapse = ", "),
        "\nDid you call join_attributes() first?"
      ))
    }
  }

  # Core analysis: one row per question × time period for a given data slice
  run_attrition_core <- function(data_slice) {
    purrr::map_dfr(questions, function(question_text) {
      responses <- data_slice[[question_text]]

      response_class <- dplyr::case_when(
        responses %in% favorability$favorable   ~ "favorable",
        responses %in% favorability$unfavorable ~ "unfavorable",
        TRUE                                    ~ "neutral"
      )

      purrr::map_dfr(time_periods, function(days) {
        left_within_period <- !is.na(data_slice$days_to_term) &
          data_slice$days_to_term > 0 &
          data_slice$days_to_term <= days

        favorable_mask <- response_class == "favorable" & !is.na(responses)
        favorable_n <- sum(favorable_mask)
        favorable_attrition <- if (favorable_n > 0) {
          sum(left_within_period[favorable_mask]) / favorable_n
        } else {
          NA_real_
        }

        unfavorable_mask <- response_class == "unfavorable" & !is.na(responses)
        unfavorable_n <- sum(unfavorable_mask)
        unfavorable_attrition <- if (unfavorable_n > 0) {
          sum(left_within_period[unfavorable_mask]) / unfavorable_n
        } else {
          NA_real_
        }

        attrition_ratio <- if (!is.na(favorable_attrition) && !is.na(unfavorable_attrition)) {
          if (favorable_attrition == 0 && unfavorable_attrition == 0) {
            NA_real_
          } else if (favorable_attrition == 0 && unfavorable_attrition > 0) {
            Inf
          } else {
            unfavorable_attrition / favorable_attrition
          }
        } else {
          NA_real_
        }

        dplyr::tibble(
          question             = question_text,
          days                 = days,
          favorable_n          = favorable_n,
          favorable_attrition  = round(favorable_attrition, 4),
          unfavorable_n        = unfavorable_n,
          unfavorable_attrition = round(unfavorable_attrition, 4),
          attrition_ratio      = ifelse(is.finite(attrition_ratio),
                                        round(attrition_ratio, 2),
                                        attrition_ratio)
        )
      })
    })
  }

  # Overall analysis (no attributes)
  if (is.null(attribute_cols)) {
    results <- run_attrition_core(combined_data)
    if (plot) {
      print(.plot_attrition(results, NULL))
      return(invisible(results))
    }
    return(results)
  }

  # Attribute-segmented analysis
  attribute_groups <- combined_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(attribute_cols))) %>%
    dplyr::summarise(group_size = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(group_size >= min_group_size)

  if (nrow(attribute_groups) == 0) {
    warning("No attribute groups meet the minimum size threshold")
    return(dplyr::tibble())
  }

  results <- purrr::map_dfr(seq_len(nrow(attribute_groups)), function(i) {
    group_values <- attribute_groups[i, attribute_cols, drop = FALSE]
    group_size   <- attribute_groups$group_size[i]

    group_data <- combined_data
    for (col in attribute_cols) {
      group_data <- group_data %>%
        dplyr::filter(!!rlang::sym(col) == group_values[[col]])
    }

    dplyr::bind_cols(
      group_values,
      dplyr::tibble(group_size = group_size),
      run_attrition_core(group_data)
    )
  })

  if (plot) {
    print(.plot_attrition(results, attribute_cols))
    return(invisible(results))
  }

  results
}


#' Analyze Survey Responses by Attributes
#'
#' Aggregates survey responses by employee attributes (e.g., Department,
#' Gender, Tenure) and calculates the same metrics as \code{summarize_survey()}
#' for each attribute group combination. Only groups meeting the minimum size
#' threshold are included in the results.
#'
#' Attributes can be supplied in two ways:
#' \enumerate{
#'   \item Pass \code{attribute_file} (a file path or data frame) and the join
#'     is performed internally on each call.
#'   \item Pre-join attributes once with \code{join_attributes()} and pass the
#'     enriched survey directly — omit \code{attribute_file} entirely. This is
#'     more efficient when calling \code{analyze_by_attributes()} multiple times
#'     or when you want to filter the data before analysis.
#' }
#'
#' @param survey A \code{glint_survey} object or data frame. If attributes have
#'   already been joined via \code{join_attributes()}, \code{attribute_file} can
#'   be omitted.
#' @param attribute_file Optional. A file path (character string) or data frame
#'   containing employee attributes to join. If \code{NULL} (default), the
#'   survey must already contain the columns named in \code{attribute_cols}.
#' @param scale_points Integer specifying the number of scale points (2-11)
#' @param attribute_cols Character vector of column names to group by
#'   (e.g., \code{c("Department", "Gender", "Tenure Group")})
#' @param emp_id_col Character string specifying the employee ID column name
#'   in both the survey data and the attribute data
#' @param min_group_size Integer specifying the minimum number of employees
#'   required for a group to be included in results (default: 5)
#' @param plot Logical. If \code{TRUE}, prints a faceted dot plot of Glint
#'   Scores by attribute group (one facet per question) and returns the data
#'   invisibly. When multiple \code{attribute_cols} are supplied, only the first
#'   is plotted. Requires \pkg{ggplot2}. Default: \code{FALSE}.
#'
#' @return A tibble with one row per attribute-group-question combination
#'   containing:
#'   \describe{
#'     \item{attribute columns}{Values for each attribute grouping variable}
#'     \item{group_size}{Number of employees in this attribute group}
#'     \item{question, mean, sd, n_responses, n_skips, n_total}{
#'       Descriptive statistics for this group on this question}
#'     \item{pct_favorable, pct_neutral, pct_unfavorable}{Favorability
#'       percentages for this group on this question}
#'   }
#'   When \code{plot = TRUE}, the same tibble is returned invisibly after
#'   printing the plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Option 1: provide attribute_file directly
#' results <- analyze_by_attributes(
#'   survey,
#'   attribute_file = "employee_attributes.csv",
#'   scale_points = 5,
#'   attribute_cols = "Department",
#'   emp_id_col = "EMP ID"
#' )
#'
#' # Option 2: pre-join with join_attributes(), then omit attribute_file
#' survey_enriched <- join_attributes(survey, "employee_attributes.csv",
#'                                    emp_id_col = "EMP ID")
#' results <- analyze_by_attributes(survey_enriched, scale_points = 5,
#'                                  attribute_cols = "Department",
#'                                  emp_id_col = "EMP ID")
#'
#' # With dot plot
#' analyze_by_attributes(survey_enriched, scale_points = 5,
#'                       attribute_cols = "Department",
#'                       emp_id_col = "EMP ID", plot = TRUE)
#' }
analyze_by_attributes <- function(survey, attribute_file = NULL, scale_points,
                                 attribute_cols, emp_id_col = NULL,
                                 min_group_size = 5, plot = FALSE) {
  if (plot) .check_ggplot2()
  if (!scale_points %in% 2:11) {
    stop("scale_points must be an integer between 2 and 11")
  }

  if (inherits(survey, "glint_survey")) {
    emp_id_col <- emp_id_col %||% survey$metadata$emp_id_col
  }

  if (is.null(emp_id_col)) {
    stop("emp_id_col must be specified when survey is a plain data frame")
  }

  if (!is.null(attribute_file)) {
    survey <- join_attributes(survey, attribute_file, emp_id_col = emp_id_col)
  }

  if (inherits(survey, "glint_survey")) {
    survey_data <- survey$data
  } else {
    survey_data <- survey
  }

  missing_cols <- setdiff(attribute_cols, names(survey_data))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Attribute column(s) not found in survey data: ",
      paste(missing_cols, collapse = ", "),
      "\nDid you forget to supply attribute_file or call join_attributes() first?"
    ))
  }

  # Use survey_data directly as combined_data — attributes already joined
  combined_data <- survey_data

  attribute_groups <- combined_data %>%
    dplyr::select(dplyr::all_of(c(attribute_cols, emp_id_col))) %>%
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

  # Get question columns (excluding standard columns AND ALL joined attribute columns,
  # not just the ones used for grouping in this call)
  standard_cols <- get_standard_columns(emp_id_col)
  all_cols <- names(survey_data)
  all_attr_cols <- if (inherits(survey, "glint_survey")) {
    survey$metadata$attribute_cols %||% attribute_cols
  } else {
    attribute_cols
  }
  question_cols <- setdiff(all_cols, c(standard_cols, all_attr_cols))

  results <- purrr::map_dfr(seq_len(nrow(attribute_groups)), function(i) {
    group_values <- attribute_groups[i, attribute_cols, drop = FALSE]
    group_size <- attribute_groups$group_size[i]

    group_data <- combined_data
    for (col in attribute_cols) {
      group_data <- group_data %>%
        dplyr::filter(!!rlang::sym(col) == group_values[[col]])
    }

    group_data_survey <- group_data %>%
      dplyr::select(dplyr::all_of(c(standard_cols, question_cols)))

    group_summary <- summarize_survey(group_data_survey, scale_points = scale_points, questions = "all", emp_id_col = emp_id_col)

    group_summary <- dplyr::bind_cols(
      group_values,
      dplyr::tibble(group_size = group_size),
      group_summary
    )

    return(group_summary)
  })

  if (plot) {
    print(.plot_by_attributes(results, attribute_cols))
    return(invisible(results))
  }

  return(results)
}


#' Search Survey Comments
#'
#' Searches through all survey comment text and returns matching responses
#' with their associated question text, numeric response values, comments, and
#' topics. Supports both exact substring matching and fuzzy (approximate)
#' matching that tolerates minor spelling differences.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param query Character string to search for within comments
#' @param exact Logical. If \code{TRUE}, performs case-sensitive exact substring
#'   matching. If \code{FALSE} (default), performs case-insensitive approximate
#'   matching that also tolerates minor spelling differences.
#' @param max_distance Numeric between 0 and 1 controlling fuzzy match tolerance
#'   when \code{exact = FALSE}. Higher values allow more differences (e.g. more
#'   typos). Default is 0.2.
#'
#' @return A tibble with one row per matching comment containing:
#'   \describe{
#'     \item{question}{The survey question text}
#'     \item{response}{The numeric survey response value}
#'     \item{comment}{The comment text that matched the query}
#'     \item{topics}{The comment topic(s) associated with the comment}
#'   }
#'   Returns an empty tibble with the same columns if no matches are found.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#'
#' # Fuzzy search (default) - finds "manager", "Manager", "managers", etc.
#' results <- search_comments(survey, "manager")
#'
#' # Exact search - finds only the literal string "Manager" (case-sensitive)
#' results_exact <- search_comments(survey, "Manager", exact = TRUE)
#'
#' # Wider fuzzy tolerance to catch more spelling variations
#' results_wide <- search_comments(survey, "management", max_distance = 0.3)
#' }
search_comments <- function(survey, query, exact = FALSE, max_distance = 0.2) {
  if (!is.character(query) || length(query) != 1 || nchar(trimws(query)) == 0) {
    stop("query must be a non-empty character string")
  }
  if (!is.logical(exact) || length(exact) != 1) {
    stop("exact must be TRUE or FALSE")
  }
  if (!is.numeric(max_distance) || length(max_distance) != 1 ||
      max_distance < 0 || max_distance > 1) {
    stop("max_distance must be a number between 0 and 1")
  }

  if (inherits(survey, "glint_survey")) {
    data <- survey$data
    all_questions <- survey$metadata$questions$question
  } else {
    data <- survey
    all_questions <- extract_questions(survey)$question
  }

  results <- purrr::map_dfr(all_questions, function(question_text) {
    comment_col <- paste0(question_text, "_COMMENT")
    topics_col  <- paste0(question_text, "_COMMENT_TOPICS")

    if (!comment_col %in% names(data)) return(NULL)

    comments  <- as.character(data[[comment_col]])
    responses <- data[[question_text]]
    topics    <- if (topics_col %in% names(data)) {
      as.character(data[[topics_col]])
    } else {
      rep(NA_character_, nrow(data))
    }

    # Only consider rows with non-empty comments
    has_comment <- !is.na(comments) & nchar(trimws(comments)) > 0
    if (!any(has_comment)) return(NULL)

    if (exact) {
      # Case-sensitive literal substring match
      matched <- has_comment & grepl(query, comments, fixed = TRUE)
    } else {
      # Case-insensitive partial match
      partial_match <- has_comment & grepl(query, comments, ignore.case = TRUE)

      # Approximate match via agrep on non-empty comments only
      fuzzy_match <- logical(length(comments))
      candidate_idx <- which(has_comment)
      if (length(candidate_idx) > 0) {
        hits <- agrep(query, comments[candidate_idx],
                      max.distance = max_distance, ignore.case = TRUE)
        if (length(hits) > 0) {
          fuzzy_match[candidate_idx[hits]] <- TRUE
        }
      }

      matched <- partial_match | fuzzy_match
    }

    if (!any(matched)) return(NULL)

    dplyr::tibble(
      question = question_text,
      response = responses[matched],
      comment  = comments[matched],
      topics   = topics[matched]
    )
  })

  if (is.null(results) || nrow(results) == 0) {
    return(dplyr::tibble(
      question = character(),
      response = numeric(),
      comment  = character(),
      topics   = character()
    ))
  }

  return(results)
}


#' Split Survey Data into Quantitative and Qualitative Components
#'
#' Separates a survey dataset into two data frames: one containing only numeric
#' response data (for statistical analysis) and one containing only comment
#' and topic data (for qualitative analysis). Both outputs retain the employee
#' ID column so they can be rejoined at any time.
#'
#' @param survey A glint_survey object or data frame containing survey data
#' @param emp_id_col Character string specifying the employee ID column name
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{quantitative}{A tibble with all standard respondent columns plus
#'       one numeric response column per question. Comment, topic, and sensitive
#'       flag columns are excluded.}
#'     \item{qualitative}{A tibble with the employee ID column plus the comment,
#'       topic, and sensitive flag columns for every question. Numeric response
#'       columns are excluded.}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' survey <- read_glint_survey("survey_export.csv")
#' parts  <- split_survey_data(survey)
#'
#' # Analyze numeric responses
#' summary <- summarize_survey(parts$quantitative, scale_points = 5)
#'
#' # Work with comments separately
#' comments <- parts$qualitative
#' }
split_survey_data <- function(survey, emp_id_col = NULL) {
  if (inherits(survey, "glint_survey")) {
    emp_id_col <- emp_id_col %||% survey$metadata$emp_id_col
    data <- survey$data
  } else {
    data <- survey
  }

  if (is.null(emp_id_col)) {
    stop("emp_id_col could not be determined. Load your survey with read_glint_survey() and specify emp_id_col.")
  }

  standard_cols <- get_standard_columns(emp_id_col)
  questions     <- extract_questions(data, emp_id_col)

  response_cols <- questions$response_col
  comment_cols  <- questions$comment_col[questions$comment_col %in% names(data)]
  topics_cols   <- questions$topics_col[questions$topics_col   %in% names(data)]
  flag_cols     <- questions$flag_col[questions$flag_col       %in% names(data)]

  # Quantitative: standard columns + one numeric response column per question
  quant_cols <- c(
    standard_cols[standard_cols %in% names(data)],
    response_cols[response_cols %in% names(data)]
  )

  # Qualitative: employee ID (for joining) + all comment/topic/flag columns
  qual_cols <- c(emp_id_col, comment_cols, topics_cols, flag_cols)

  list(
    quantitative = dplyr::select(data, dplyr::all_of(quant_cols)),
    qualitative  = dplyr::select(data, dplyr::all_of(qual_cols))
  )
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
