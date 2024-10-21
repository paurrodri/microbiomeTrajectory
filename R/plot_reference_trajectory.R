#' Plot microbiota age trajectory from reference/training set
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


plot_reference_trajectory <- function(
  microbiota_age_data,
  color             = "#FFB829",
  xlabel            = "Chronological age (months)",
  ylabel            = "Microbiota age (months)",
  title             = '',
  font_size         = 12,
  save_figure       = FALSE,
  figure_name       = "microbiota_age_trajectory_reference.pdf"){

  plot_data <- microbiota_age_data %>%
    # Filter by e.g. delivery mode
    dplyr::filter(Feeding_group == 'HMG' & Delivery_mode == "Vaginal") %>%
    # Convert age from days to months
    dplyr::mutate(Microbiota_age = Microbiota_age / 30) %>%
    dplyr::select(Sample_ID, Microbiota_age, Visit) %>%
    dplyr::mutate(Chronological_age = dplyr::case_when(
      Visit == 'Baseline' ~ 1/2,
      Visit == '3 months' ~ 3,
      Visit == '6 months' ~ 6,
      Visit == '12 months' ~ 12,
      Visit == '15 months' ~ 15
    ))

  plot_data_summary <- plot_data %>%
    dplyr::group_by(Visit) %>%
    dplyr::summarise(median_microbiota_age = median(Microbiota_age),
                   sd_microbiota_age = sd(Microbiota_age)) %>%
    dplyr::mutate(Chronological_age = dplyr::case_when(
      Visit == 'Baseline' ~ 1/2,
      Visit == '3 months' ~ 3,
      Visit == '6 months' ~ 6,
      Visit == '12 months' ~ 12,
      Visit == '15 months' ~ 15
    ))

  # Aesthetics
  yscale <- ggplot2::scale_y_continuous(
    breaks = c(0, 3, 6, 9, 12, 15),
    limits = c(0, 15.5))
  xscale <- ggplot2::scale_x_continuous(
    breaks = c(0, 3, 6, 9, 12, 15),
    limits = c(0, 15.5))

  down_limit <- plot_data_summary %>%
    dplyr::filter(Visit == 'Baseline') %>%
    dplyr::mutate(limit = median_microbiota_age - sd_microbiota_age) %>%
    dplyr::select(limit) %>% dplyr::pull()

  if (down_limit < 0){
    yscale <- ggplot2::scale_y_continuous(
      breaks = c(0, 3, 6, 9, 12, 15),
      limits = c(1.2*down_limit, 15.5))
  }

  # Plot
  plot <- plot_data_summary %>%
    ggplot2::ggplot(ggplot2::aes(
      x = Chronological_age,
      y = median_microbiota_age)) +
    ggplot2::geom_point(color = color) +
    ggplot2::geom_line(color = color) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = median_microbiota_age - sd_microbiota_age,
      ymax = median_microbiota_age + sd_microbiota_age),
      width = .2,
      color = color,
      position = ggplot2::position_dodge(0.05)) +
    # For dots as predictions
    # ggplot2::geom_point(data = plot_data,
    #            ggplot2::aes(x = Chronological_age,
    #                y = Microbiota_age),
    #            fill = color,
    #            color = color,
    #            alpha = 0.25) +
    # For the diagonal
    ggplot2::geom_abline(linetype = "dashed",
                         color = "#989994") +
    custom_theme(font_size = font_size) +
    yscale +
    xscale +
    ggplot2::labs(
      title = '',
      x = xlabel,
      y = ylabel)

  if(save_figure){
    plot
    ggplot2::ggsave(
      filename = figure_name,
      units = "cm",
      height = 7.5,
      width = 7.5,
      scale = 1
    )
  }

  return(plot)
}
