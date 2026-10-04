# ggplot2 chart builders used by the dashboard.

theme_dashboard <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.title         = ggplot2::element_text(colour = "grey35"),
      axis.text          = ggplot2::element_text(colour = "grey25"),
      legend.position    = "top",
      legend.title       = ggplot2::element_blank(),
      plot.margin        = ggplot2::margin(8, 12, 8, 8)
    )
}

#' Heat map of risk-factor prevalence (% of respondents) by diabetes status.
plot_risk_heatmap <- function(summary) {
  ggplot2::ggplot(summary, ggplot2::aes(diabetic_status, factor_label, fill = share)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1),
                   colour = share > 0.6),
      size = 4, show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = "#F1F7F6", high = COLOR_PRIMARY,
      labels = scales::label_percent(), limits = c(0, 1)
    ) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    ggplot2::labs(x = NULL, y = NULL, fill = "Prevalence") +
    theme_dashboard() +
    ggplot2::theme(
      panel.grid      = ggplot2::element_blank(),
      legend.position = "right",
      legend.title    = ggplot2::element_text(size = 11),
      legend.key.height = grid::unit(1.2, "cm")
    )
}

#' Grouped bar chart comparing risk-factor prevalence across statuses.
plot_risk_bars <- function(summary) {
  ggplot2::ggplot(summary, ggplot2::aes(share, factor_label, fill = diabetic_status)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(), expand = c(0, 0.01)) +
    ggplot2::scale_fill_manual(values = STATUS_COLORS, breaks = STATUS_LEVELS) +
    ggplot2::labs(x = "Share of respondents with factor", y = NULL) +
    theme_dashboard()
}

#' For one risk factor: how respondents who have it split across statuses.
plot_factor_split <- function(summary, factor_key) {
  data <- summary |>
    dplyr::filter(factor == factor_key) |>
    dplyr::mutate(split = count / sum(count))

  ggplot2::ggplot(data, ggplot2::aes(diabetic_status, split, fill = diabetic_status)) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(split, accuracy = 0.1)),
      vjust = -0.5, size = 4.5, fontface = "bold", colour = "grey20"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(), limits = c(0, 1), expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(values = STATUS_COLORS) +
    ggplot2::labs(x = NULL, y = "Share of respondents with this factor") +
    theme_dashboard() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey90")
    )
}

#' Population pyramid of respondents by age group and sex.
plot_age_pyramid <- function(counts) {
  counts <- dplyr::mutate(counts, signed = ifelse(sex_label == "Female", -n, n))
  limit  <- max(counts$n) * 1.05

  ggplot2::ggplot(counts, ggplot2::aes(signed, age_group, fill = sex_label)) +
    ggplot2::geom_col(width = 0.85) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40") +
    ggplot2::scale_x_continuous(
      labels = function(x) scales::comma(abs(x)), limits = c(-limit, limit)
    ) +
    ggplot2::scale_fill_manual(values = SEX_COLORS) +
    ggplot2::labs(x = "Respondents", y = "Age group") +
    theme_dashboard()
}

#' Horizontal bar chart of predicted class probabilities.
plot_prediction <- function(probabilities) {
  ggplot2::ggplot(probabilities, ggplot2::aes(probability, status, fill = status)) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(probability, accuracy = 1)),
      hjust = -0.15, size = 4.5, fontface = "bold", colour = "grey20"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(), limits = c(0, 1.08), expand = c(0, 0)
    ) +
    ggplot2::scale_y_discrete(limits = rev(STATUS_LEVELS)) +
    ggplot2::scale_fill_manual(values = STATUS_COLORS) +
    ggplot2::labs(x = "Predicted probability", y = NULL) +
    theme_dashboard()
}
