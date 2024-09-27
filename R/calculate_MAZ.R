#' Calculate Microbiota-for-age Z-scores (MAZ)
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#'
#' @return Documentation will follow
#' @export
#'
calculate_MAZ <- function(
  microbiota_age_data,
  reference_expression = expression(Feeding_group == "HMG")
  ){

  microbiota_age_reference <- microbiota_age_data %>%
    dplyr::filter(eval(reference_expression))

  stats_reference <- microbiota_age_reference %>%
    dplyr::group_by(Visit) %>%
    dplyr::summarize(
      median_reference_bin = median(Microbiota_age),
      sd_reference_bin     = sd(Microbiota_age)
    )

  maz_data <- microbiota_age_data %>%
    dplyr::left_join(stats_reference, by = 'Visit') %>%
    dplyr::mutate(
      MAZ = (Microbiota_age - median_reference_bin) / sd_reference_bin
    ) %>%
    dplyr::select(-median_reference_bin, -sd_reference_bin)

  return(maz_data)
}
