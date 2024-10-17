#' Plot Microbiota-for-age Z-scores (MAZ) for each feeding group and visit
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

plot_maz <- function(
  maz_data,
  filter_expression = TRUE,
  outlier_threshold = 3,
  color_values      = list("CG" = "#02AE71",
                           "TG1" = "#06AEFA",
                           "TG2" = "#3B6BC0",
                           "HMG" = "#FFB829"),
  visit_levels      = c("Baseline", "3 months", "6 months", "12 months", "15 months"),
  xlabel            = "Feeding group",
  ylabel            = "Microbiota-for-age Z-score",
  plot_title        = NULL,
  save_figure       = FALSE,
  figure_name       = "maz_scores.pdf"
){


  .ceiling_signif <- function(x, digits){
    if (x >= 0) {
      y <- ceiling(x/10^digits) * 10^digits
    }
    else {
      y <- -ceiling(-x/10^digits) * 10^digits
    }
    return(y)
  }

  plot_data <-
    maz_data %>%
    # Filter by e.g. delivery mode
    dplyr::filter(eval(filter_expression)) %>%
    dplyr::mutate(Visit = factor(Visit, levels = visit_levels))

  axis_limit <- .ceiling_signif(max(-min(plot_data$MAZ), max(plot_data$MAZ)), 0)

  set.seed(631)

  plot <-
    plot_data %>%
      ggplot2::ggplot(ggplot2::aes(
        x = Feeding_group,
        y = MAZ,
        fill = Feeding_group
      )) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_jitter(
        shape = 21,
        alpha = 1,
        position = ggplot2::position_jitterdodge(0.5)
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(-12, 12, 3),
        limits = c(-axis_limit, axis_limit)
      ) +
      ggplot2::scale_fill_manual(
        values = color_values
        ) +
      ggplot2::geom_hline(
        yintercept = -outlier_threshold,
        linetype = "dashed",
        color = "#989994") +
      ggplot2::geom_hline(
        yintercept =  outlier_threshold,
        linetype = "dashed",
        color = "#989994") +
      ggplot2::labs(
        x = xlabel,
        y = ylabel,
        title = plot_title
      ) +
      ggplot2::facet_wrap('Visit', nrow = 1) +
      custom_theme() +
      ggplot2::theme(
        legend.position = "none"
      )

  if(save_figure){
    plot
    ggplot2::ggsave(
      filename = figure_name,
      units = "cm",
      height = 10,
      width = 24.7,
      scale = 1
    )
   }

  return(plot)

}
