#' Plot biomarker measurements for on-/off-trajectory groups and each follow-up visit
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import ggplot2
#' @import glue
#' @import forcats
#' @import patchwork
#' @import exactRankTests
#' @import tibble
#' @import ggsignif
#'
#' @return Documentation will follow
#' @export
#'

plot_biomarker_on_off_trajectory <- function(
  biomarker,
  biomarker_data,
  on_off_trajectory_data,
  filter_expression    = expression(Feeding_group != "HMG"),
  add_units_string     = TRUE, # "umol/g wet feces" for OrgAcids, "mg/g dry feces" for A1AT, "mg/kg dry feces" for IgA and CP
  color_on_trajectory  = "#30A190",
  color_off_trajectory = "#E09B65",
  label_on_trajectory  = "On-trajectory",
  label_off_trajectory = "Off-trajectory",
  visit_levels         = c("Baseline", "3 months", "6 months", "12 months", "15 months"),
  plot_title           = "",
  plot_caption         = "",
  title_fontsize       = 12,
  subtitle_fontsize    = 12,
  text_fontsize        = 10,
  save_figure          = FALSE,
  figure_name          = glue::glue("{biomarker}_on_off_trajectory_boxplot.pdf")
){

  # Units and scale
  if (biomarker == "pH"){
    scale_string <- ""
    units_string <- ""
  } else if (biomarker == "alpha-1-Antitrypsin"){
    scale_string <- " (log10)"
    units_string <- "mg/g dry feces"
  } else if (biomarker %in% c("IgA", "Calprotectin")) {
    scale_string <- " (log10)"
    units_string <- "mg/kg dry feces"
  } else if (biomarker %in% c(
    "Acetic acid", "Propionic acid",
    "Butyric acid", "Valeric acid",
    "Lactic acid", "Isobutyric acid",
    "Isovaleric acid")) {
    scale_string <- " (log10)"
    units_string <- "umol/g wet feces" # using "u" instead of "mu" as default due to difficulties in processing "mu" symbol as an R package
  } else {
    usethis::ui_stop(glue::glue('Biomarker "{biomarker}" not recognised. Stopping function.'))
  }

  # Prepare data
  data <- biomarker_data %>%
    dplyr::select(Sample_ID, !!dplyr::sym(biomarker)) %>%
    dplyr::left_join(on_off_trajectory_data, by = "Sample_ID") %>%
    dplyr::filter(eval(filter_expression)) %>%
    dplyr::filter(!is.na(!!dplyr::sym(biomarker))) %>%
    dplyr::mutate(
      Visit = factor(Visit, levels = visit_levels),
      Visit = forcats::fct_drop(Visit)
    )

  # Y scale
  options(scipen = 10L)
  min_y  <- data %>% dplyr::ungroup() %>% dplyr::select(!!dplyr::sym(biomarker)) %>% min(na.rm = T)
  max_y  <- data %>% dplyr::ungroup() %>% dplyr::select(!!dplyr::sym(biomarker)) %>% max(na.rm = T)

  min_ylimit_log <- ifelse(ceiling(-log10(min_y)) >= 0, 10^(-ceiling(-log10(min_y))), 1)
  max_ylimit_log <- ceiling(max_y/10^floor(log10(max_y))) * 10^floor(log10(max_y)) + 50

  if(scale_string == " (log10)"){
    yscale <- ggplot2::scale_y_log10(limits = c(min_ylimit_log, max_ylimit_log))
  } else {
    # only possibility is "pH"
    yscale <- ggplot2::scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14), labels = seq(0, 14))
  }

  ylabel <- ifelse(
    add_units_string,
    glue::glue('{biomarker}{scale_string}\n({units_string})'),
    glue::glue('{biomarker}{scale_string}')
  )

  ### Map over visits
  plot_visits <- purrr::map(levels(data$Visit), function(visit){

    plot_data <- data %>%
      dplyr::filter(Visit == visit) %>%
      dplyr::select(Trajectory_status_per_sample, !!dplyr::sym(biomarker))

    ## Plot
    set.seed(631) # useful for consistent jittering

    plot <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(
        x = Trajectory_status_per_sample,
        y = !!dplyr::sym(biomarker),
        fill = Trajectory_status_per_sample
      )) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_jitter(
        shape = 21,
        alpha = 1,
        position = ggplot2::position_jitterdodge(0.5)
      ) +
      # cm.analysis::cm.geom_pvalues(
      #   plot_data = plot_data,
      #   y = biomarker,
      #   group = "Trajectory_status_per_sample",
      #   asterisks = TRUE,
      #   hide_ns = TRUE,
      #   asterisk_breaks = c(-Inf, 0.001, 0.01, 0.05, 1),
      #   asterisk_labels = c("***", "**", "*", "")
      # )
      yscale +
      ggplot2::scale_fill_manual(
        values = c(color_on_trajectory, color_off_trajectory),
        labels = c(label_on_trajectory, label_off_trajectory)
      ) +
      ggplot2::labs(
        x = "",
        y = ylabel,
        subtitle = visit
      ) +
      custom_theme(font_size = text_fontsize) +
      ggplot2::theme(
        legend.position = "none",
        plot.subtitle = ggplot2::element_text(size = subtitle_fontsize),
        axis.title.x = ggplot2::element_text(size = title_fontsize),
        axis.text.x = ggplot2::element_text(
          angle = 30,
          hjust = 1,
          size = text_fontsize)
      )

    # Add p-value to plot
    test <- exactRankTests::wilcox.exact(
      plot_data[[biomarker]] ~ plot_data[['Trajectory_status_per_sample']],
      paired = FALSE
    )
    p <- test$p.value
    if (p < 0.05){
      asterisk_string <- dplyr::case_when(
          p < 0.001 ~ "***",
          p < 0.01  ~ "**",
          p < 0.05  ~ "*"
      )
      plot <- plot +
        ggsignif::geom_signif(
          comparisons = list(c(label_on_trajectory, label_off_trajectory)),
          annotation = asterisk_string,
          tip_length = 0.01,
          margin_top = 0.05,
          step_increase = 0.06,
          vjust = 0.5)
    }
    plot
    return(plot)

  }) %>% patchwork::wrap_plots(nrow = 1)

  plot_visits[[1]] <- plot_visits[[1]] +
    ggplot2::labs(title = plot_title)

  if(save_figure){
    plot_visits
    ggplot2::ggsave(
      filename = figure_name,
      units = "cm",
      height = 8,
      width = length(levels(data$Visit)) * 24.7 / 5,
      scale = 1
    )
  }
  return(plot_visits)
}
