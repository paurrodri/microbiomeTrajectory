#' Compare microbiome or biomarkers between on- and off-trajectory subjects
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import exactRankTests
#' @import effsize
#' @import tibble
#' @importFrom stats as.formula median p.adjust pchisq sd
#'
#' @return Documentation will follow
#' @export
#'

compare_biome_between_on_off_trajectory <- function(
  biome_data, # biomarker, genus10, mgs20
  on_off_trajectory_data,
  filter_expression = expression(Feeding_group != "HMG"),
  loop_over_visits = c("3 months", "6 months", "12 months", "15 months"),
  FDR_threshold = 0.05
  ){

  # Prepare data
  items_to_compare <- biome_data %>% dplyr::select(-Sample_ID) %>% colnames()
  data <- dplyr::left_join(biome_data,
                           on_off_trajectory_data,
                           by = "Sample_ID") %>%
    dplyr::filter(eval(filter_expression)) %>%
    dplyr::select(everything(),
                  -MAZ,
                  -dplyr::contains("Trajectory_status_"),
                  -dplyr::contains("_age"),
                  Trajectory_status = Trajectory_status_per_sample)

  # Loop over time points
  all_results <- purrr::map(loop_over_visits, function(visit){
    purrr::map(items_to_compare, function(item){

      data_visit <- data %>%
        dplyr::filter(Visit == visit)

      # Units item
      if (stringr::str_detect(item, "alpha-1-Antitrypsin|Calprotectin|IgA")){
        units <- "mg/g dry feces"
      } else if (stringr::str_detect(item, " acid")){
        units <- "umol/g wet feces"
      } else if (item == "pH") {
        units <- ""
      } else {
        units <- "Relative abundance (%)"
      }

      # Perform test
      test <- exactRankTests::wilcox.exact(
        data_visit[[item]] ~ data_visit[['Trajectory_status']],
        paired = FALSE
      )

      # Extract results
      p <- test$p.value

      cliff_delta <- effsize::cliff.delta(
        data_visit[[item]],
        data_visit[['Trajectory_status']]
      )
      if (cliff_delta$estimate > 0){
        higher_in <- levels(data_visit[['Trajectory_status']])[1]
      } else if (cliff_delta$estimate < 0){
        higher_in <- levels(data_visit[['Trajectory_status']])[2]
      } else {
        higher_in <- "none"
      }

      results <- tibble::tibble(
        'Visit' = visit,
        'Item' = item,
        'Units' = units,
        'Effect_size' = round(cliff_delta$estimate, 4),
        'P' = p,
        'Item_higher_in' = higher_in
      )
      return(results)

    }) %>% dplyr::bind_rows()
  }) %>% dplyr::bind_rows() %>%
    # FDR correction
    dplyr::mutate(FDR = p.adjust(P, method = "BH"),
                  .after = "P") %>%
    dplyr::mutate(
      P = round(P, 8),
      FDR = round(FDR, 8),
      Item_higher_in = dplyr::case_when(
        FDR < FDR_threshold  ~ Item_higher_in,
        FDR < 1 ~ "Non-significant",
      ),
      Item_higher_in = factor(Item_higher_in,
                              levels = c(levels(data[['Trajectory_status']]),
                                         'Non-significant'))
    )

  return(all_results)

}
