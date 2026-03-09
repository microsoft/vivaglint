# Internal ggplot2 plot helpers for plot = TRUE on analysis functions.
# None of these functions are exported; all names are prefixed with .plot_.

.check_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plotting. ",
      "Install it with: install.packages('ggplot2')"
    )
  }
}


# ── summarize_survey ──────────────────────────────────────────────────────────

.plot_survey_summary <- function(results) {
  long <- tidyr::pivot_longer(
    results,
    cols      = c("pct_unfavorable", "pct_neutral", "pct_favorable"),
    names_to  = "sentiment",
    values_to = "pct"
  )
  long$sentiment <- factor(
    long$sentiment,
    levels = c("pct_unfavorable", "pct_neutral", "pct_favorable"),
    labels = c("Unfavorable", "Neutral", "Favorable")
  )
  gs            <- stats::setNames(results$glint_score, results$question)
  long$question <- factor(long$question,
    levels = results$question[order(results$glint_score)])

  ggplot2::ggplot(long,
    ggplot2::aes(.data$question, .data$pct, fill = .data$sentiment)) +
    ggplot2::geom_col(width = 0.65,
      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      data = long[long$sentiment == "Favorable", ],
      ggplot2::aes(y = 103,
        label = gs[as.character(.data$question)]),
      hjust = 0, size = 3.2, fontface = "bold", color = "#333333"
    ) +
    ggplot2::scale_fill_manual(
      values = c(Favorable = "#0078D4", Neutral = "#B0BEC5",
                 Unfavorable = "#C42B1C"),
      breaks = c("Favorable", "Neutral", "Unfavorable")
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 115), breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = "Favorability Breakdown",
      subtitle = "Questions sorted by Glint Score (0-100); score shown at right",
      x = NULL, y = "% of Respondents", fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title           = ggplot2::element_text(face = "bold"),
      legend.position      = "bottom",
      panel.grid.major.y   = ggplot2::element_blank()
    )
}


# ── get_response_dist ─────────────────────────────────────────────────────────

.plot_response_dist <- function(results) {
  pct_cols <- grep("^pct_", names(results), value = TRUE)
  values   <- sort(as.numeric(sub("^pct_", "", pct_cols)))
  n_vals   <- length(values)

  # Gradient: dark red → pink → grey → light blue → dark blue
  pal        <- grDevices::colorRampPalette(
    c("#C42B1C", "#F4ACAC", "#B0BEC5", "#6FB3E0", "#0078D4")
  )(n_vals)
  names(pal) <- as.character(values)

  long <- tidyr::pivot_longer(
    results,
    cols      = dplyr::all_of(pct_cols),
    names_to  = "value",
    values_to = "pct"
  )
  long$value <- factor(sub("^pct_", "", long$value),
    levels = as.character(values))

  # Sort questions by weighted mean response (lowest score at bottom)
  wmeans    <- tapply(
    as.numeric(as.character(long$value)) * long$pct,
    long$question, sum, na.rm = TRUE
  ) / tapply(long$pct, long$question, sum, na.rm = TRUE)
  long$question <- factor(long$question, levels = names(sort(wmeans)))

  ggplot2::ggplot(long,
    ggplot2::aes(.data$question, .data$pct, fill = .data$value)) +
    ggplot2::geom_col(width = 0.65,
      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = "Response Value Distribution",
      subtitle = "Each segment = % of respondents selecting that scale value",
      x = NULL, y = "% of Respondents", fill = "Value"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title           = ggplot2::element_text(face = "bold"),
      legend.position      = "bottom",
      panel.grid.major.y   = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
}


# ── compare_cycles ────────────────────────────────────────────────────────────

.plot_compare_cycles <- function(results, cycle_names) {
  results$cycle <- factor(results$cycle, levels = cycle_names)
  last_cycle    <- cycle_names[length(cycle_names)]
  labels_df     <- results[results$cycle == last_cycle, ]

  score_min <- max(0,   min(results$glint_score, na.rm = TRUE) - 10)
  score_max <- min(100, max(results$glint_score, na.rm = TRUE) + 15)

  ggplot2::ggplot(results,
    ggplot2::aes(.data$cycle, .data$glint_score,
      color = .data$question, group = .data$question)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_text(
      data = labels_df,
      ggplot2::aes(label = .data$glint_score),
      hjust = -0.7, size = 3, show.legend = FALSE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(score_min, score_max),
      breaks = seq(0, 100, 10)
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::labs(
      title    = "Glint Score Trends",
      subtitle = "Score on 0-100 scale; score value shown at final cycle",
      x = NULL, y = "Glint Score", color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title        = ggplot2::element_text(face = "bold"),
      legend.position   = "right",
      panel.grid.minor  = ggplot2::element_blank()
    )
}


# ── get_correlations ──────────────────────────────────────────────────────────

.plot_correlations <- function(results) {
  questions      <- unique(results$question1)
  results$question1 <- factor(results$question1, levels = questions)
  results$question2 <- factor(results$question2, levels = rev(questions))

  ggplot2::ggplot(results,
    ggplot2::aes(.data$question1, .data$question2,
      fill = .data$correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", .data$correlation)),
      size  = 3.2,
      color = ifelse(abs(results$correlation) > 0.5, "white", "#333333")
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#C42B1C", mid = "#FFFFFF", high = "#0078D4",
      midpoint = 0, limits = c(-1, 1), name = "r"
    ) +
    ggplot2::labs(
      title    = "Question Correlation Heatmap",
      subtitle = "Pairwise complete observations",
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      panel.grid  = ggplot2::element_blank()
    )
}


# ── extract_survey_factors ────────────────────────────────────────────────────

.plot_survey_factors <- function(factor_summary) {
  factor_summary$question <- factor(
    factor_summary$question,
    levels = rev(unique(factor_summary$question))
  )

  ggplot2::ggplot(factor_summary,
    ggplot2::aes(.data$factor, .data$question,
      fill = .data$loading)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", .data$loading)),
      size  = 3.5,
      color = ifelse(factor_summary$loading > 0.55, "white", "#333333")
    ) +
    ggplot2::scale_fill_gradient(
      low = "#D6ECFA", high = "#0078D4", limits = c(0, 1),
      name = "Loading"
    ) +
    ggplot2::labs(
      title    = "Factor Loading Heatmap",
      subtitle = "Cells shown where |loading| meets minimum threshold",
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(face = "bold"),
      panel.grid  = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 11)
    )
}


# ── analyze_attrition ─────────────────────────────────────────────────────────

.plot_attrition <- function(results, attribute_cols) {
  id_cols   <- c(attribute_cols, "question", "days")
  id_cols   <- id_cols[id_cols %in% names(results)]
  plot_data <- tidyr::pivot_longer(
    results[, c(id_cols, "favorable_attrition", "unfavorable_attrition"),
            drop = FALSE],
    cols      = c("favorable_attrition", "unfavorable_attrition"),
    names_to  = "group",
    values_to = "attrition"
  )
  plot_data$group <- factor(
    plot_data$group,
    levels = c("favorable_attrition", "unfavorable_attrition"),
    labels = c("Favorable", "Unfavorable")
  )
  plot_data$days <- factor(
    paste0(plot_data$days, " days"),
    levels = paste0(sort(unique(results$days)), " days")
  )

  p <- ggplot2::ggplot(plot_data,
      ggplot2::aes(.data$question, .data$attrition, fill = .data$group)) +
    ggplot2::geom_col(position = "dodge", width = 0.65) +
    ggplot2::scale_fill_manual(
      values = c(Favorable = "#0078D4", Unfavorable = "#C42B1C")
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(x * 100, 1), "%")
    ) +
    ggplot2::labs(
      title    = "Attrition Rate by Favorability",
      subtitle = paste0("Proportion who left within each window; ",
        "favorable vs. unfavorable respondents"),
      x = NULL, y = "Attrition Rate", fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title          = ggplot2::element_text(face = "bold"),
      legend.position     = "bottom",
      axis.text.x         = ggplot2::element_text(angle = 35, hjust = 1),
      panel.grid.major.x  = ggplot2::element_blank(),
      strip.text          = ggplot2::element_text(face = "bold")
    )

  attr_col <- if (!is.null(attribute_cols)) attribute_cols[1] else NULL

  if (!is.null(attr_col) && attr_col %in% names(plot_data)) {
    p <- p + ggplot2::facet_grid(
      stats::reformulate("days", attr_col)
    )
  } else {
    p <- p + ggplot2::facet_wrap(~days, nrow = 1)
  }

  p
}


# ── analyze_by_attributes ─────────────────────────────────────────────────────

.plot_by_attributes <- function(results, attribute_cols) {
  attr_col <- attribute_cols[1]

  if (length(attribute_cols) > 1) {
    message(
      "plot = TRUE shows results for the first attribute column ('",
      attr_col, "') only."
    )
    results <- results %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(c(attr_col, "question")))
      ) %>%
      dplyr::summarise(
        glint_score = stats::weighted.mean(
          .data$glint_score, .data$group_size, na.rm = TRUE
        ),
        group_size = sum(.data$group_size, na.rm = TRUE),
        .groups = "drop"
      )
  }

  score_min <- max(0,   min(results$glint_score, na.rm = TRUE) - 10)
  score_max <- min(100, max(results$glint_score, na.rm = TRUE) + 15)

  ggplot2::ggplot(results,
    ggplot2::aes(.data$glint_score, .data[[attr_col]])) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$group_size, color = .data$glint_score)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$glint_score)),
      hjust = -1.0, size = 2.8
    ) +
    ggplot2::scale_color_gradient(
      low = "#C42B1C", high = "#0078D4",
      limits = c(score_min, score_max), name = "Score"
    ) +
    ggplot2::scale_size_continuous(range = c(2, 6), name = "Group n") +
    ggplot2::scale_x_continuous(limits = c(score_min, score_max + 5)) +
    ggplot2::facet_wrap(~question, nrow = 2) +
    ggplot2::labs(
      title    = paste0("Glint Score by ", attr_col),
      subtitle = "Dot size = group respondent count",
      x = "Glint Score", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title          = ggplot2::element_text(face = "bold"),
      panel.grid.major.y  = ggplot2::element_blank(),
      strip.text          = ggplot2::element_text(face = "bold", size = 9),
      legend.position     = "bottom"
    )
}


# ── aggregate_by_manager ─────────────────────────────────────────────────────

.plot_manager <- function(results) {
  n_questions <- length(unique(results$question))

  # Sort managers by mean Glint Score across all questions
  mgr_order         <- tapply(results$glint_score, results$manager_name,
    mean, na.rm = TRUE)
  results$manager_name <- factor(results$manager_name,
    levels = names(sort(mgr_order)))

  score_min  <- max(0,   min(results$glint_score, na.rm = TRUE) - 10)
  score_max  <- min(100, max(results$glint_score, na.rm = TRUE) + 15)

  # Per-question average for the reference line
  pop_avg_df <- results %>%
    dplyr::group_by(.data$question) %>%
    dplyr::summarise(
      pop_avg = mean(.data$glint_score, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(results,
      ggplot2::aes(.data$glint_score, .data$manager_name)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x    = score_min,
        xend = .data$glint_score,
        y    = .data$manager_name,
        yend = .data$manager_name
      ),
      color = "#B0BEC5", linewidth = 0.8
    ) +
    ggplot2::geom_vline(
      data     = pop_avg_df,
      ggplot2::aes(xintercept = .data$pop_avg),
      linetype = "dashed", color = "#555555", linewidth = 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$team_size, color = .data$glint_score)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$glint_score),
      hjust = -1.1, size = 3.2, fontface = "bold"
    ) +
    ggplot2::scale_color_gradient(
      low = "#C42B1C", high = "#0078D4",
      limits = c(score_min, score_max), name = "Score"
    ) +
    ggplot2::scale_size_continuous(range = c(3, 9), name = "Team n") +
    ggplot2::scale_x_continuous(limits = c(score_min, score_max + 5)) +
    ggplot2::labs(
      title    = "Team Glint Scores by Manager",
      subtitle = paste0("Sorted by score; dashed line = group mean; ",
        "dot size = team respondent count"),
      x = "Glint Score", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title          = ggplot2::element_text(face = "bold"),
      panel.grid.major.y  = ggplot2::element_blank(),
      legend.position     = "bottom"
    )

  if (n_questions > 1) {
    p <- p + ggplot2::facet_wrap(~question)
  }

  p
}
