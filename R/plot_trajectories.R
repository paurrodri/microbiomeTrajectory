#' Plot microbiota trajectory curves
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import ggplot2
#'
#' @return Documentation will follow
#' @export
#'


plot_trajectories <- function(
  microbiota_age_data,
  filter_expression = TRUE,
  ci                = TRUE,
  alpha             = 0.15,
  breaks            = c(0, 3, 6, 9, 12, 15),
  color_values      = list("CG" = "#02AE71",
                           "TG1" = "#06AEFA",
                           "TG2" = "#3B6BC0",
                           "HMG" = "#FFB829"),
  xlabel            = "Chronological age (months)",
  ylabel            = "Microbiota age (months)",
  color_label       = "Feeding group",
  caption           = '',
  save_figure       = FALSE,
  figure_name       = "microbiota_trajectories.pdf"){

  plot_data <- microbiota_age_data %>%
    # Filter by e.g. delivery mode
    dplyr::filter(eval(filter_expression)) %>%
    # Convert age from days to months
    dplyr::mutate(
      Chronological_age = Chronological_age / 30,
      Microbiota_age    = Microbiota_age    / 30
    )

  xmin <- min(plot_data$Chronological_age)
  xmax <- max(plot_data$Chronological_age)
  ymin <- min(plot_data$Microbiota_age)
  ymax <- max(plot_data$Microbiota_age)

  plot <-
    plot_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = Chronological_age,
      y = Microbiota_age,
      fill = Feeding_group,
      color = Feeding_group)
    ) +
    ggplot2::geom_smooth(
      method = 'loess',
      se = ci,
      alpha = alpha,
      level = 0.95,
      linewidth = 0.5) +
    ggplot2::scale_x_continuous(breaks = breaks,
                                limits = c(xmin, xmax)) +
    ggplot2::scale_y_continuous(breaks = breaks,
                                limits = c(ymin, ymax)) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::labs(
      x = xlabel,
      y = ylabel,
      fill  = color_label,
      color = color_label,
      caption = caption) +
    custom_theme()

  if(save_figure){
    plot
    ggplot2::ggsave(
      filename = figure_name,
      units = "cm",
      height = 8,
      width = 10,
      scale = 1
    )
  }

  return(plot)
}
