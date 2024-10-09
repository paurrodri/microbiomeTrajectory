#' Plot Kaplan-Meier curves - all combinations used in the study
#'
#' Output does not include table of cumulative events, only the KM curve.
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import glue
#' @import survival
#' @import survminer
#' @import tibble
#' @import ggplot2
#' @import patchwork
#' @importFrom stats as.formula median p.adjust pchisq sd
#'
#' @return Documentation will follow
#' @export
#'

plot_kaplan_meier_all_combinations <- function(
  event_data,
  event_name,
  on_off_trajectory_data,
  filter_expression = expression(Feeding_group != "HMG")){

  if (event_name == "antibiotics"){
    plot_title <- "Antibiotics"
    ylabel <- "Children using antibiotics"
  } else if (event_name == "rti_mild"){
    plot_title <- "Mild RTIs"
    ylabel <- "Children developing mild RTIs"
  }

  combinations <- tibble::tibble(
    trajectory_status_variable = list(
      'Trajectory_status_per_subject',
      'Trajectory_status_3_months',
      'Trajectory_status_12_months'),
    time_limits = list(
      list(1*30, 15*30),
      list(3*30, 12*30),
      list(12*30, 15*30))
  )

  figure_list <- purrr::pmap(
    combinations,
    function(trajectory_status_variable, time_limits){

      if (trajectory_status_variable == "Trajectory_status_per_subject"){
        legend_title <- "Trajectory overall"
      } else if (trajectory_status_variable == "Trajectory_status_3_months"){
        legend_title <- "Trajectory at 3 months"
      } else if (trajectory_status_variable == "Trajectory_status_12_months"){
        legend_title <- "Trajectory at 12 months"
      }

      time_limits_unlisted <- time_limits %>% unlist()
      plot_subtitle <- paste("Followed",
                             paste(time_limits_unlisted/30, collapse = '-'),
                             "months")

      figure <- plot_kaplan_meier(
        event_data, on_off_trajectory_data, filter_expression,
        trajectory_status_variable = trajectory_status_variable,
        time_limits = time_limits_unlisted,
        plot_title = plot_title,
        plot_subtitle = plot_subtitle,
        legend_title = legend_title,
        ylabel = ylabel
      )
      return(figure)
    })

  plot_list    <- lapply(figure_list, function(x) x$plot)
  table_list   <- lapply(figure_list, function(x) x$cumevents)
  figure_setup <- (plot_list[[1]]/table_list[[1]]) |
    (plot_list[[2]]/table_list[[2]]) |
    (plot_list[[3]]/table_list[[3]])

  all_figures <- patchwork::wrap_plots(plot_list, ncol = 3)
  # all_figures <- patchwork::wrap_plots(figure_setup, ncol = 3)

  return(all_figures)
}
