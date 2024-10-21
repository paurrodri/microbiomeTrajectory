#' Plot SHAP values (feature importance) from age predictor model
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import ggplot2
#' @import usethis
#'
#' @return Documentation will follow
#' @export
#'


plot_shap_values <- function(
  shap_data,
  model_performance_data,
  model,
  color             = "#30A190",
  xlabel            = "SHAP feature importance",
  limits            = c(0, 70),
  title             = 'Model feature importance',
  feature_fontsize  = 12,
  xlabel_size       = 9,
  xtext_size        = 8,
  save_figure       = FALSE,
  figure_name       = "shap_feature_importance.pdf"){

  r_squared <- model_performance_data %>%
    dplyr::filter(`Model ID` == model) %>%
    dplyr::pull("R2") %>%
    round(3)

  plot <-
    shap_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = reorder(Feature_description, SHAP),
      y = SHAP)) +
    ggplot2::geom_bar(
      stat = 'identity',
      fill = color) +
    ggplot2::scale_y_continuous(
      limits = limits,
      breaks = seq(limits[1], limits[2], 10)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      y = xlabel,
      title = title,
      subtitle = glue::glue("{model} model, R2 = {r_squared}")
      ) +
    custom_theme(font_size = feature_fontsize) +
    ggplot2::theme(
      legend.position = 'None',
      axis.title.y    = ggplot2::element_blank(),
      axis.title.x    = ggplot2::element_text(size = xlabel_size),
      axis.text.x     = ggplot2::element_text(
        size = xtext_size,
        angle = 0,
        hjust = 0.5)
    )

  if(save_figure){
    plot
    if(model == "genus10"){
      ggplot2::ggsave(
        filename = figure_name,
        units = "cm",
        height = 7.5,
        width = 9,
        scale = 1
      )
    } else if (model == "mgs20"){
      ggplot2::ggsave(
        filename = figure_name,
        units = "cm",
        height = 7.5*1.25,
        width = 9*1.5,
        scale = 1
      )
    } else {
      usethis::ui_stop("This function only supports genus10 or mgs20 models")
    }
  }

  return(plot)
}
