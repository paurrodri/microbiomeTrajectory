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


  custom_theme <- ggplot2::theme(
    axis.line = ggplot2::element_line(
      color = "black",
      linewidth = 0.5,
      lineend = "square"),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 2),
      vjust = 1),
    axis.text.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = 2),
      vjust = 0),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 2),
      hjust = 1),
    axis.text.y.right = ggplot2::element_text(
      margin = ggplot2::margin(l = 2),
      hjust = 0),
    axis.ticks.length = ggplot2::unit(5/2, "pt"),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 5/2),
      vjust = 1),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = 5/2),
      vjust = 0),
    axis.title.y = ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = 5/2),
      vjust = 1),
    axis.title.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = 5/2),
      vjust = 0),
    legend.spacing = ggplot2::unit(10, "pt"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.key.size = ggplot2::unit(10, "pt"),
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.title = ggplot2::element_text(hjust = 0),
    legend.justification = c("left", "center"),
    legend.box.margin = ggplot2::margin(0, 0, 0, 0),
    legend.box.spacing = ggplot2::unit(10, "pt"),
    strip.background = ggplot2::element_rect(fill = "grey80"),
    strip.text = ggplot2::element_text(
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(5/2, 5/2, 5/2, 5/2)),
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.placement = "outside",
    strip.switch.pad.grid = ggplot2::unit(5/2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(5/2, "pt"),
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(
      face = "bold",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = 5)),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.8),
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = 5)),
    plot.tag = ggplot2::element_text(
      face = "bold",
      hjust = 0,
      vjust = 0.7),
    plot.tag.position = c(0, 1),
    plot.margin = ggplot2::margin(5/2, 5/2, 5/2, 5/2)
  )

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
    custom_theme

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
