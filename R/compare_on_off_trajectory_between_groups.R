#' Compare feeding groups between on- and off-trajectory subjects
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
compare_on_off_trajectory_between_groups <- function(
  on_off_trajectory_data,
  filter_expression = TRUE,
  TG_group, # TGs # TG1 # TG2
  trajectory_status_variable, # Trajectory_status_early_visits # late_visits # per_subject
  trend = "CG-TG-HMG" # "CG-TG-HMG" or "TG-CG-HMG"
  ){

  data_catt <- on_off_trajectory_data %>%
    # Filter by e.g. delivery mode
    dplyr::filter(eval(filter_expression)) %>%
    dplyr::distinct(Subject_ID, .keep_all = T)

  trend_levels <- stringr::str_split(trend, "-")[[1]]
  trend_levels[trend_levels == "TG"] <- TG_group

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
   return(list(
     z = z_catt,
     p = p_catt))
}
