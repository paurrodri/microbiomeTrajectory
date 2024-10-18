#' Custom theme for plots
#'
#' Documentation will follow
#'
#' @import ggplot2
#'
#' @return Documentation will follow
#' @export
#'

custom_theme <- function(
  font_size = 10,
  line_size = 0.5,
  smaller_fraction = 0.8,
  axis_margin_fraction = 0.25){

  smaller_size <- font_size * smaller_fraction
  half_size    <- font_size * 0.5
  quarter_size <- font_size * 0.25
  axis_margin_size  <- smaller_size * axis_margin_fraction


  theme <- ggplot2::theme(
    line = ggplot2::element_line(
      color = "black",
      linewidth = line_size,
      linetype = 1,
      lineend = "butt"),
    rect = ggplot2::element_rect(
      fill = NA,
      color = NA,
      linewidth = line_size,
      linetype = 1),
    text = ggplot2::element_text(
      family = "",
      face = "plain",
      color = "black",
      size = font_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 0.9,
      margin = ggplot2::margin(),
      debug = FALSE),
    axis.line = ggplot2::element_line(
      color = "black",
      linewidth = line_size,
      lineend = "square"),
    axis.text.x = ggplot2::element_text(
      color = "black",
      size = smaller_size,
      margin = ggplot2::margin(t = axis_margin_size),
      vjust = 1),
    axis.text.x.top = ggplot2::element_text(
      color = "black",
      size = smaller_size,
      margin = ggplot2::margin(b = axis_margin_size),
      vjust = 0),
    axis.text.y = ggplot2::element_text(
      color = "black",
      size = smaller_size,
      margin = ggplot2::margin(r = axis_margin_size),
      hjust = 1),
    axis.text.y.right = ggplot2::element_text(
      color = "black",
      size = smaller_size,
      margin = ggplot2::margin(l = axis_margin_size),
      hjust = 0),
    axis.ticks.length = ggplot2::unit(quarter_size, "pt"),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = quarter_size),
      vjust = 1),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = quarter_size),
      vjust = 0),
    axis.title.y = ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = quarter_size),
      vjust = 1),
    axis.title.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = quarter_size),
      vjust = 0),
    legend.spacing = ggplot2::unit(font_size, "pt"),
    legend.margin  = ggplot2::margin(0, 0, 0, 0),
    legend.key.size = ggplot2::unit(font_size, "pt"),
    legend.text = ggplot2::element_text(size = ggplot2::rel(smaller_fraction)),
    legend.title = ggplot2::element_text(hjust = 0),
    legend.justification = c("left", "center"),
    legend.box.margin = ggplot2::margin(0, 0, 0, 0),
    legend.box.spacing = ggplot2::unit(font_size, "pt"),
    strip.background = ggplot2::element_rect(fill = "grey80"),
    strip.text = ggplot2::element_text(
      size = ggplot2::rel(smaller_fraction),
      margin = ggplot2::margin(quarter_size, quarter_size, quarter_size, quarter_size)),
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.placement = "outside",
    strip.switch.pad.grid = ggplot2::unit(quarter_size, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(quarter_size, "pt"),
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(
      face = "bold",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_size)),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(smaller_fraction),
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = half_size)),
    plot.tag = ggplot2::element_text(
      face = "bold",
      hjust = 0,
      vjust = 0.7),
    plot.tag.position = c(0, 1),
    plot.margin = ggplot2::margin(quarter_size, quarter_size, quarter_size, quarter_size)
  )
  return(theme)
}
