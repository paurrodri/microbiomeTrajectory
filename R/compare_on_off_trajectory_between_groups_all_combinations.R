#' Compare feeding groups between on- and off-trajectory subjects - all combinations used in the study
#'
#' Using Cochran-Armitage trend test
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import CATT
#' @import tidyr
#' @import tibble
#' @import glue
#' @importFrom stats as.formula median p.adjust pchisq sd
#'
#' @return Documentation will follow
#' @export
#'
compare_on_off_trajectory_between_groups_all_combinations <- function(
  on_off_trajectory_data,
  filter_expression = TRUE
){

  combinations_catt <- tidyr::expand_grid(
    TG_group = c('TGs', 'TG1', 'TG2'),
    trajectory_status_variable = c(
      'Trajectory_status_early_visits',
      'Trajectory_status_late_visits',
      'Trajectory_status_per_subject'), # _survival
    trend = c('CG-TG-HMG', 'TG-CG-HMG')
  )

  data_catt <- on_off_trajectory_data %>%
    # Filter by e.g. delivery mode
    dplyr::filter(eval(filter_expression)) %>%
    dplyr::distinct(Subject_ID, .keep_all = T)

  results_catt <- purrr::pmap(combinations_catt,
                              function(TG_group, trajectory_status_variable, trend){

                                trend_levels <- stringr::str_split(trend, "-")[[1]]
                                trend_levels[trend_levels == "TG"] <- TG_group
                                trend_string <-paste(trend_levels, collapse = '-')

                                if (TG_group == 'TGs'){
                                  table_catt <- data_catt %>%
                                    dplyr::mutate(
                                      FG = ifelse(Feeding_group %in% c('TG1', 'TG2'),
                                                  'TGs',
                                                  as.character(Feeding_group)),
                                      FG = factor(FG, levels = trend_levels)) %>%
                                    dplyr::select(eval(trajectory_status_variable), FG) %>%
                                    table()
                                } else {
                                  table_catt <- data_catt %>%
                                    dplyr::filter(Feeding_group %in% trend_levels) %>%
                                    droplevels() %>%
                                    dplyr::mutate(
                                      FG = factor(Feeding_group, levels = trend_levels)
                                    ) %>%
                                    dplyr::select(eval(trajectory_status_variable), FG) %>%
                                    table()
                                }
                                catt <- CATT::CATT(table = table_catt)
                                z_catt <- catt$statistic
                                attr(z_catt, "names") <- NULL
                                p_catt <- if (catt$p < 0.0001) "< 0.0001" else glue::glue("{round(catt$p, 4)}")

                                return(tibble::tibble(
                                  trajectory_status_variable = trajectory_status_variable,
                                  TG_group = TG_group,
                                  trend = trend_string,
                                  Z = z_catt,
                                  P = p_catt
                                ))
                              }) %>% dplyr::bind_rows() %>%
    dplyr::arrange(trend, TG_group)
  return(results_catt)
}
