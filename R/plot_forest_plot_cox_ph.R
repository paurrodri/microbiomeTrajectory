#' Plot forest plot showing Cox PH results
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import forestploter
#'
#' @return Documentation will follow
#' @export
#'

plot_forest_plot_cox_ph <- function(
  results_data,                   # either antibiotics or rti coxph results
  plot_title              = NULL, # if NULL, uses event name, If FALSE, no title
  adjusted_model_string   = "Adjusted for Feeding_group, Delivery_mode, Sex",
  unadjusted_model_string = "Unadjusted",
  save_figure             = FALSE,
  figure_name             = "forest_plot_cox_ph.pdf"
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

  # Prepare data
  data <- results_data %>%
    # Filter relevant tests
    dplyr::filter(Adjustment %in% c(adjusted_model_string, unadjusted_model_string)) %>%
    # Modify columns
    dplyr::mutate(
      trajectory_definition = dplyr::case_when(
        Variable == "Trajectory_status_per_subject" ~ "On-trajectory overall",
        Variable == "Trajectory_status_3_months"    ~ "On-trajectory 3mo",
        Variable == "Trajectory_status_12_months"   ~ "On-trajectory 12mo",
        TRUE ~ NA),
      model_label = stringr::str_c(
        trajectory_definition,
        ", followed ",
        stringr::str_replace(`Followed between`, " months", "mo"),
        sep = ""),
      adjustment = dplyr::case_when(
        stringr::str_detect(Adjustment, "djusted for ") ~ "adjusted",
        stringr::str_detect(Adjustment, "nadjusted")    ~ "unadjusted"),
      blank = " ",
      n_string = stringr::str_c(n_on, "vs", n_off, sep = " "),
      hr_string = dplyr::case_when(
        is.na(HR) ~ "",
        TRUE ~ sprintf("%.2f (%.2f to %.2f)", HR, ci_low, ci_high)),
      p_string = dplyr::case_when(
        is.na(p) ~ "",
        p < 0.001 ~ "<0.001",
        TRUE ~ format(round(p, 3), nsmall = 3))
    ) %>%
    # Sort
    dplyr::arrange(adjustment) %>%
    # Select relevant columns
    dplyr::select(
      model_label,
      adjustment,
      n_on, n_off, n_string,
      blank,
      HR, ci_low, ci_high,
      hr_string, p_string)

  # Ensure correct ordering of labels
  model_label_levels <- data$model_label %>% unique()

  ### Separate titles and content
  plot_titles <- data %>%
    dplyr::select(model_label, n_on, n_off, n_string) %>%
    dplyr::mutate(model_label = factor(model_label, levels = model_label_levels)) %>%
    dplyr::arrange(model_label) %>%
    dplyr::distinct(model_label, .keep_all = T) %>%
    dplyr::mutate(model_label_show = model_label, .after = model_label) %>%
    dplyr::mutate(
      blank = "",
      hr_string = "",
      p_string = ""
    ) %>%
    dplyr::mutate(dplyr::across(everything(), ~ as.character(.))) %>%
    dplyr::mutate(Index = 1 + (dplyr::row_number() - 1) * 3, .before = model_label)

  plot_content <- data %>%
    dplyr::mutate(model_label = factor(model_label, levels = model_label_levels)) %>%
    dplyr::arrange(model_label, adjustment) %>%
    dplyr::mutate(model_label_show = stringr::str_c("                          ",
                                                    adjustment),
           .after = model_label) %>%
    dplyr::mutate(
      n_on = "",
      n_off = "",
      n_string = "") %>%
    dplyr::mutate(
      Index = cumsum(rep(c(2, 1), length.out = dplyr::n())),
      .before = model_label) %>%
    dplyr::select(-adjustment)

  plot_data <- dplyr::bind_rows(
    plot_titles,
    plot_content
  ) %>%
    dplyr::arrange(Index)


  # Aesthetics
  event_title <- unique(results_data$Event_data) %>%
    stringr::str_replace("_data", "") %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_replace("antibiotics", "Antibiotics") %>%
    stringr::str_replace("rti", "RTI")
  custom_plot_title <- ifelse(
    is.null(plot_title),
    event_title,
    ifelse(plot_title == FALSE, "", plot_title)
  )
  xmax  <- .ceiling_signif(max(plot_data$ci_high, na.rm = T), 0)
  custom_theme <- forestploter::forest_theme(
    core = list(fg_params = list(hjust = 0, x = 0),
                bg_params = list(fill = c("#E1E2DC", "#FFFFFF", "#FFFFFF"))
    ),
    colhead = list(fg_params = list(hjust = 0, x = 0))
  )

  # Plot
  plot <- plot_data %>%
    dplyr::select(
      'Model' = model_label_show,
      'On' = n_on,
      'Off' = n_off,
      # the bigger this blank name, the more space to plot the CIs
      `                                                   ` = blank,
      'HR (95% CI)' = hr_string,
      'P' = p_string) %>%
    forestploter::forest(
      .,
      est   = plot_data$HR,
      lower = plot_data$ci_low,
      upper = plot_data$ci_high,
      ci_column = 4, # the index of the blank column
      ref_line = 1,
      arrow_lab = c("On-trajectory lower risk",
                    "On-trajectory higher risk"),
      xlim = c(0, xmax),
      xtrans = c("log"),
      ticks_at = c(0.1, 0.5, 1, 2),
      title = custom_plot_title,
      theme = custom_theme
    )

  if(save_figure){
    plot
    ggplot2::ggsave(
      filename = figure_name,
      units = "cm",
      height = 8,
      width = 24.7,
      scale = 1
    )
  }

  return(plot)

}
