#' Calculate distance between trajectories
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import splinectomeR
#' @import patchwork
#' @import ggplot2
#' @importFrom utils head
#'
#' @return Documentation will follow
#' @export
#'
calculate_distance_between_trajectories <- function(
  microbiota_age_data,
  filter_expression = TRUE,
  min_threshold = 0.1,
  groups_to_compare = list("CG-HMG" = c("CG", "HMG"),
                           "TG1-HMG" = c("TG1", "HMG"),
                           "TG2-HMG" = c("TG2", "HMG")),
  xlab              = 'Chronological age (days)',
  ylab              = 'Distance between trajectories',
  save_figure       = FALSE,
  figure_name       = "trajectory_distance.pdf"){

  .ceiling_signif <- function(x, digits){
    if (x >= 0) {
      y <- ceiling(x/10^digits) * 10^digits
    }
    else {
      y <- -ceiling(-x/10^digits) * 10^digits
    }
    return(y)
  }


  # 1. Compute distance
  distance <- purrr::map(groups_to_compare, function(groups){
    microbiota_age_data %>%
      dplyr::filter(eval(filter_expression)) %>%
      splinectomeR::permuspliner(
        data = .,
        xvar = 'Chronological_age',
        yvar = 'Microbiota_age',
        category = 'Feeding_group',
        cases = 'Subject_ID',
        groups = c(groups[1], groups[2]),
        set_spar = 0.75,
        quiet = TRUE
      )
    })
  pvals <- purrr::map(names(groups_to_compare), function(name){
    distance[[name]][['pval']]
  })
  names(pvals) <- names(groups_to_compare)
  pvals %<>%
    tibble::enframe(name = 'groups', value = 'P') %>%
    dplyr::mutate(
      P = as.numeric(P),
      significance = dplyr::case_when(
        P < 0.05   ~ '*',
        P < 0.01   ~ '**',
        P < 0.001  ~ '***',
        P < 0.0001 ~ '****',
        TRUE ~ 'ns'
    ))

  # 2. Check when distance becomes smaller than min_threshold
  timepoint_distance_minimizes <-
    purrr::map(names(groups_to_compare), function(name){
    distance[[name]]$true_distance %>%
      dplyr::filter(abs.distance < min_threshold) %>%
      dplyr::arrange(x) %>%
      head(1) %>%
      dplyr::select(x, abs.distance)
  })
  names(timepoint_distance_minimizes) <- names(groups_to_compare)

  #  3. Plot distance between splines
  upper_limit <- purrr::map(names(groups_to_compare), function(name){
    max(distance[[name]]$true_distance$abs.distance)
  }) %>% unlist() %>% max()

  axis_limits <- c(0, .ceiling_signif(upper_limit, digits = 2))

  plot <- purrr::map(names(groups_to_compare), function(name){

    minimum <- timepoint_distance_minimizes[[name]]$x %>% round(0)
    maximum <- distance[[name]]$true_distance$abs.distance %>% max() %>% ceiling()

    splinectomeR::permuspliner.plot.permdistance(
      distance[[name]],
      xlabel = xlab) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = minimum),
        linetype = "dashed",
        color = 'red') +
      ggplot2::annotate(
        "text",
        x = minimum - 5,
        y = maximum,
        hjust = 1,
        label = paste(' ', minimum),
        color = 'red',
      ) +
      ggplot2::ylim(axis_limits) +
      ggplot2::labs(
        y = ylab,
        subtitle = name) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 10)
      )
  }) %>% patchwork::wrap_plots(
    ncol = length(groups_to_compare)
  )

  if (save_figure){
    plot
    ggplot2::ggsave(
      filename = figure_name,
      units = "cm",
      height = 10,
      width = 24,
      scale = 1,
      landscape = T
    )
  }
  return(plot)
}
