#' Compare trajectories between feeding groups
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import splinectomeR
#' @import patchwork
#' @import ggplot2
#'
#' @return Documentation will follow
#' @export
#'

compare_trajectories_between_groups <- function(
  microbiota_age_data,
  filter_expression = TRUE,
  interval          = 60,
  groups_to_compare = list("CG-HMG" = c("CG", "HMG"),
                           "TG1-HMG" = c("TG1", "HMG"),
                           "TG2-HMG" = c("TG2", "HMG")),
  xlab              = 'Chronological age (days)',
  ylab              = 'P-value',
  threshold_point_color = 'red',
  save_figure       = FALSE,
  figure_name       = "trajectory_comparison_pvalues.pdf"){

  # 1. Compute comparison
  comparison <- purrr::map(groups_to_compare, function(groups){
     suppressWarnings(splinectomeR::sliding_spliner(
        data = microbiota_age_data %>%
          dplyr::filter(eval(filter_expression)),
        xvar = 'Chronological_age',
        yvar = 'Microbiota_age',
        category = 'Feeding_group',
        cases = 'Subject_ID',
        groups = c(groups[1], groups[2]),
        ints = interval,
        quiet = TRUE
      ))
  })
  pvals <- purrr::map(names(groups_to_compare), function(name){
    comparison[[name]][['pval_table']] %>%
      dplyr::rename('P' = 'p_value') %>%
      dplyr::select(-number_of_observations) %>%
      dplyr::mutate(significance = dplyr::case_when(
        P < 0.05   ~ '*',
        P < 0.01   ~ '**',
        P < 0.001  ~ '***',
        P < 0.0001 ~ '****',
        TRUE ~ 'ns'
      ))
  })
  names(pvals) <- names(groups_to_compare)

  # 2. Check when (timepoint) comparison stops being significant
  timepoint_comparison_no_longer_significant <-
    purrr::map(names(groups_to_compare), function(name){
    pvals[[name]] %>%
        dplyr::filter(Chronological_age > 30) %>%
        dplyr::filter(grepl("ns", significance)) %>%
        head(1) %>%
        dplyr::pull(Chronological_age)
  })
  names(timepoint_comparison_no_longer_significant) <- names(groups_to_compare)


  # 3. Plot p-values
  axis_limits <- c(
    purrr::map(names(groups_to_compare), function(name){min(pvals[[name]]$P)}) %>% unlist() %>% min() %>% log10() %>% floor() %>% `^`(10, .),
    purrr::map(names(groups_to_compare), function(name){max(pvals[[name]]$P)}) %>% unlist() %>% max() %>% round()
  )

  .plot_pvals_trajectory_comparison <- function(
    data,
    xvar,
    axis_limits,
    threshold_point,
    threshold_point_color = 'red',
    xlab = '',
    ylab = 'P-value',
    subtitle = ''){

    # Inspired on splinectomeR package
    # https://github.com/RRShieldsCutler/splinectomeR

    .norm_range <- function(x) {
      (x - min(x))/(max(x) - min(x))
    }

    pval.p <- data["pval_table"][[1]] %>%
      dplyr::bind_rows(
        dplyr::slice(., which(!!dplyr::sym(xvar)!= threshold_point)),
        dplyr::slice(., which(!!dplyr::sym(xvar) == threshold_point))
      )
    xvar <- names(pval.p)[1]

    pval.p$N.norm <- .norm_range(x = pval.p[, 3])

    manual_breaks <- c(1e-7, 1e-6, 1e-4, 0.01, 0.05, 0.1, 1)
    manual_labels <- c("0.0000001", "0.000001", "0.0001", "0.01", "0.05", "0.1", "1")

    if (length(unique(pval.p[, 3])) <= 1) {
      cat("Number of observations per interval is uniform; points will not be plotted.")
    }

    plot <- ggplot2::ggplot(
      pval.p,
      ggplot2::aes(x = pval.p[, 1], y = p_value)
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point(
        shape = 20,
        size = (pval.p$N.norm * 2),
        color = ifelse(
          pval.p[, 1] == threshold_point,
          threshold_point_color,
          "black")
      ) +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = 0.05),
        linetype = "dashed") +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = threshold_point),
        linetype = "dashed",
        color = threshold_point_color) +
      ggplot2::labs(x = xlab,
                    y = ylab,
                    subtitle = subtitle) +
      ggplot2::scale_y_continuous(
        limits = axis_limits,
        trans  = scales::log10_trans(),
        breaks = manual_breaks,
        labels = manual_labels
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        plot.background = ggplot2::element_rect(color = "white"),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "black"),
        axis.title = ggplot2::element_text(size = 10)
      ) +
      ggplot2::annotate(
        "text",
        x = threshold_point + 5,
        y = 0.1,
        hjust = 1,
        label = paste(round(threshold_point, 0), ''),
        color = threshold_point_color,
      )

    return(plot)
  }

  plot <- purrr::map(names(groups_to_compare), function(name){
    threshold_point <- timepoint_comparison_no_longer_significant[[name]]
    plot <- .plot_pvals_trajectory_comparison(
      data = comparison[[name]],
      xvar = 'Chronological_age',
      axis_limits = axis_limits,
      threshold_point = threshold_point,
      threshold_point_color = threshold_point_color,
      xlab = xlab,
      ylab = ylab,
      subtitle = name)
    return(plot)
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
