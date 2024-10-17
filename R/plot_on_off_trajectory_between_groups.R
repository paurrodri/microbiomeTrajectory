#' Plot on-/off-trajectory per feeding group
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import forcats
#' @import glue
#' @import scales
#' @import stringr
#'
#' @return Documentation will follow
#' @export
#'

plot_on_off_trajectory_between_groups <- function(
  on_off_trajectory_data,
  filter_expression = TRUE,
  TG_group, # TGs # TG1 # TG2
  trajectory_status_variable, # Trajectory_status_early_visits # late_visits # per_subject
  trend = "CG-TG-HMG", # "CG-TG-HMG" or "TG-CG-HMG"
  show_n_barplots = FALSE,
  save_figure = FALSE,
  figure_name = "on_off_trajectory_groups_barplot.pdf",
  color_on_trajectory = "#30A190",
  color_off_trajectory = "#E09B65",
  label_on_trajectory = "On-trajectory",
  label_off_trajectory = "Off-trajectory",
  xlabel = "Feeding group",
  ylabel = "Proportion of subjects",
  y_axis_ticks_interval = 0.1,
  plot_title = "", # if NULL, uses trajectory_status_variable. If FALSE, no title
  plot_caption = NULL, # if NULL, uses P-value. If FALSE, no caption
  plot_font_size = 14,
  legend_title = "",
  legend_title_size = 14,
  legend_text_size  = 14,
  legend_key_size   = 14
){

  # Prepare data
  data_catt <- on_off_trajectory_data %>%
    # Filter by e.g. delivery mode
    dplyr::filter(eval(filter_expression)) %>%
    dplyr::distinct(Subject_ID, .keep_all = T)

  trend_levels <- stringr::str_split(trend, "-")[[1]]
  trend_levels[trend_levels == "TG"] <- TG_group

  if (TG_group == 'TGs'){
    data_catt %<>%
      dplyr::mutate(
        FG = ifelse(Feeding_group %in% c('TG1', 'TG2'),
                    'TGs',
                    as.character(Feeding_group)),
        FG = factor(FG, levels = trend_levels))
    table_catt <- data_catt %>%
      dplyr::select(eval(trajectory_status_variable), FG) %>%
      table()
  } else {
    data_catt %<>%
      dplyr::filter(Feeding_group %in% trend_levels) %>%
      droplevels() %>%
      dplyr::mutate(
        FG = factor(Feeding_group, levels = trend_levels)
      )
    table_catt <- data_catt %>%
      dplyr::select(eval(trajectory_status_variable), FG) %>%
      table()
  }
  # Compute Cochran-Armitage trend test
  catt <- CATT::CATT(table = table_catt)
  p_string <- if (catt$p < 0.0001) "P < 0.0001" else glue::glue("P = {format(round(catt$p, 4), scientific = F)}")

  # Plot aesthetics
  custom_plot_title <- ifelse(
    is.null(plot_title),
    stringr::str_replace_all(trajectory_status_variable, "_", " "),
    ifelse(plot_title == FALSE, "", plot_title)
    )
  custom_plot_caption <- ifelse(
    is.null(plot_caption),
    p_string,
    ifelse(plot_caption == FALSE, "", plot_caption)
  )
  if (show_n_barplots){
    xlabels <- data_catt %>%
      dplyr::group_by(FG) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(label = stringr::str_c(
        `FG`, n, sep = '\n(N = ')) %>%
      dplyr::mutate(label = stringr::str_c(
        label, ")", sep = '')) %>%
      dplyr::pull(label)
    xscale <- ggplot2::scale_x_discrete(labels = xlabels)
  } else {
    xscale <- ggplot2::scale_x_discrete()
  }

  # Plot
  plot <-
    data_catt %>%
    dplyr::group_by(FG, !!dplyr::sym(trajectory_status_variable)) %>%
    dplyr::count() %>%
    tidyr::drop_na() %>%
    ggplot2::ggplot(ggplot2::aes(
      x = FG,
      y = n,
      fill = forcats::fct_rev(!!dplyr::sym(trajectory_status_variable))))  +
    ggplot2::geom_bar(
      position = "fill",
      stat = "identity") +
    xscale +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      breaks = seq(0, 1, by = y_axis_ticks_interval)) +
    ggplot2::scale_fill_manual(
      values = c(color_off_trajectory,
                 color_on_trajectory),
      labels = c(label_off_trajectory,
                 label_on_trajectory),
      guide = ggplot2::guide_legend(reverse = FALSE)) +
    ggplot2::labs(
      fill = legend_title,
      x = xlabel,
      y = ylabel,
      title = custom_plot_title,
      caption = custom_plot_caption,
    ) +
    custom_theme(font_size = plot_font_size) +
    ggplot2::theme(
      legend.position = 'top',
      legend.direction = "vertical",
      legend.title = ggplot2::element_text(size = legend_title_size),
      legend.text  = ggplot2::element_text(size = legend_text_size),
      legend.key.height = ggplot2::unit(legend_key_size, 'pt'),
      legend.key.width  = ggplot2::unit(legend_key_size, 'pt'),
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      reverse = TRUE))

  if(save_figure){
    plot
    ggplot2::ggsave(
      filename = figure_name,
      units = "cm",
      height = 12,
      width = 12,
      scale = 1
    )
  }

  return(plot)

}
