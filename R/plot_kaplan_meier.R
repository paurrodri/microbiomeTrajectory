#' Plot Kaplan-Meier curves
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import glue
#' @import survival
#' @import survminer
#' @import ggplot2
#' @importFrom grDevices dev.off pdf
#' @importFrom stats as.formula median p.adjust pchisq sd
#'
#' @return Documentation will follow
#' @export
#'


plot_kaplan_meier <- function(
  event_data, # antibiotics # rti_mild
  on_off_trajectory_data,
  filter_expression = expression(Feeding_group != "HMG"),
  trajectory_status_variable = "Trajectory_status_per_subject_survival", # "Trajectory_status_per_subject_survival" # "Trajectory_status_3_months" # "Trajectory_status_12_months",,
  time_limits = c(1*30, 15*30), # provided in days # c(1*30, 15*30) # c(3*30, 12*30) # c(12*30, 15*30)
  breaks = 30, # provided in days

  caption_log_rank_test_results = TRUE,
  xlabel = "Months of age",
  ylabel = "Event", # tip: edit manually
  plot_title = "", # tip: edit manually
  plot_subtitle = "", # tip: edit manually
  legend_title = "",
  label_on_trajectory = "On-trajectory",
  label_off_trajectory = "Off-trajectory",
  color_on_trajectory = "#30A190",
  color_off_trajectory = "#E09B65",

  plot_title_fontsize    = 16,
  plot_subtitle_fontsize = 14,
  axis_title_fontsize    = 14,
  axis_text_fontsize     = 14,
  legend_title_fontsize  = 14,
  legend_text_fontsize   = 14,
  cumevents_fontsize     = 5,
  plot_caption_fontsize  = 12,

  save_figure = FALSE,
  figure_name = "kaplan_meier_curve.pdf"
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

  # Prepare survival analysis data
  survival_data <- event_data %>%
    dplyr::filter(eval(filter_expression)) %>%
    dplyr::filter(Days_to_event >= time_limits[1] &
                    Days_to_event <= time_limits[2]) %>%
    dplyr::group_by(Subject_ID) %>%
    dplyr::summarise(Days_to_first_event = min(Days_to_event)) %>%
    dplyr::right_join(event_data %>%
                        dplyr::filter(eval(filter_expression)) %>%
                        dplyr::select(Subject_ID,
                                      Days_to_latest_visit,
                                      Feeding_group, Delivery_mode, Sex) %>%
                        dplyr::distinct(Subject_ID, .keep_all = TRUE),
                      by = "Subject_ID") %>%
    dplyr::right_join(
      on_off_trajectory_data %>%
        dplyr::filter(eval(filter_expression)) %>%
        dplyr::select(Subject_ID, !!dplyr::sym(trajectory_status_variable)) %>%
        dplyr::distinct(Subject_ID, .keep_all = TRUE),
      by = "Subject_ID"
    ) %>%
    dplyr::mutate(Timepoint = dplyr::case_when(
      !is.na(Days_to_first_event) ~ Days_to_first_event,
      TRUE ~ Days_to_latest_visit
    ),
    Event_type = dplyr::case_when(
      !is.na(Days_to_first_event) ~ "event",
      TRUE ~ "censored"
    ),
    Timepoint = dplyr::case_when(
      Event_type == "censored" & Timepoint > time_limits[2] ~ time_limits[2],
      TRUE ~ Timepoint
    ),
    Event_code = dplyr::case_when(
      Event_type == "censored" ~ 0,
      TRUE ~ 1
    ), .before = Days_to_first_event
    ) %>%
    dplyr::select(Subject_ID,
                  Timepoint, Event_code,
                  Trajectory_status = !!dplyr::sym(trajectory_status_variable)) %>%
    dplyr::filter(!is.na(Event_code) & ! is.na(Trajectory_status)) %>%
    dplyr::mutate(Timepoint = as.integer(Timepoint),
                  Event_code = as.integer(Event_code),
                  Trajectory_status = factor(Trajectory_status,
                                             levels = c("On-trajectory", "Off-trajectory"))
    ) %>%
    as.data.frame()


  # Perform survival test
  survival_fit <- survival::survfit(
    survival::Surv(time = Timepoint, event = Event_code) ~ Trajectory_status,
    data = survival_data
  )

  # Log-rank test
  if (caption_log_rank_test_results){
    survival_diff <- survival::survdiff(
      survival::Surv(time = Timepoint, event = Event_code) ~ Trajectory_status,
      data = survival_data
    )
    p_log_rank <- pchisq(survival_diff$chisq,
                         length(survival_diff$n) - 1,
                         lower.tail = FALSE)
    p_string <- if (p_log_rank < 0.0001) "p < 0.0001" else glue::glue("p = {round(p_log_rank, 4)}")

    log_rank_test_string <- glue::glue("Log-rank test: {p_string}")

  } else {
    log_rank_test_string <- ""
  }


  # Plot aesthetics
  color_labels <- c('On-trajectory' = glue::glue('{label_on_trajectory}'),
                    'Off-trajectory' = glue::glue('{label_off_trajectory}'))
  color_values <- c('On-trajectory' = color_on_trajectory,
                    'Off-trajectory' = color_off_trajectory)

  # Plot scales
  y_max <- 1 - min(survival_fit$surv)
  y_lim <- c(0, .ceiling_signif(y_max, -1))
  x_lim <- time_limits
  x_breaks <- breaks

  # Plot theme
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
    panel.grid.minor = NULL,
    panel.grid.major.x = NULL,
    panel.grid.major.y = ggplot2::element_line(
      color = "grey90",
      linewidth = 1/2,
      linetype = 2),
    panel.grid.minor.x = NULL,
    panel.grid.minor.y = NULL,
    panel.spacing = ggplot2::unit(5/2, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
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


  # Plot
  plot <- survminer::ggsurvplot(
    survival_fit,
    data = survival_data,
    fun = "event",
    cumevents = TRUE,
    surv.median.line = "hv",
    surv.scale = "percent",
    censor = TRUE,
    censor.size = 2,
    censor.shape = "I",
    ggtheme = custom_theme,
    tables.theme = custom_theme,
    xlim = x_lim,
    xscale = x_breaks,
    ylim = y_lim,
    break.x.by = x_breaks,
    break.y.by = 0.1,
    palette = unname(color_values),
    title = plot_title,
    subtitle = plot_subtitle,
    legend.labs = unname(color_labels),
    legend.title = '',
    xlab = xlabel,
    ylab = ylabel,
    pval.size = 5,
    tables.col = "strata",
    tables.y.text.col = TRUE,
    fontsize = cumevents_fontsize, # size of cum events table
    tables.y.text = FALSE
    )

  plot$plot <- plot$plot +
    ggplot2::labs(
      color = legend_title,
      caption = log_rank_test_string
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(title.position = "top")) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = plot_title_fontsize),
      plot.subtitle = ggplot2::element_text(size = plot_subtitle_fontsize),
      plot.caption = ggplot2::element_text(size = plot_caption_fontsize),
      axis.title = ggplot2::element_text(size = axis_title_fontsize),
      legend.title = ggplot2::element_text(size = legend_title_fontsize),
      legend.text = ggplot2::element_text(size = legend_text_fontsize),
      axis.text = ggplot2::element_text(size = axis_text_fontsize))

  plot$cumevents <- plot$cumevents +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = plot_title_fontsize,
                                         face = "plain"),
      axis.title = ggplot2::element_text(size = axis_title_fontsize),
      axis.text = ggplot2::element_text(size = axis_text_fontsize),
      panel.grid.major.y = NULL)

  if(save_figure){
    pdf(figure_name,
        width = 7,
        height = 7,
        onefile = FALSE)
    plot
    dev.off()
  }

  return(plot)
}
