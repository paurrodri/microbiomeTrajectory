#' Correlate microbiome data and biomarkers data
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import broom
#' @import Kendall
#' @importFrom stats as.formula median p.adjust pchisq sd
#'
#' @return Documentation will follow
#' @export
#'

correlate_microbiome_biomarkers <- function(
  microbiome_data,
  biomarker_data,
  metadata,
  filter_expression = expression(Feeding_group != "HMG"),
  filter_visit_expression = expression(Visit %in% c("3 months", "6 months", "12 months", "15 months")),
  FDR_threshold = 0.05){

  items_x <- microbiome_data %>% dplyr::select(-Sample_ID) %>% colnames()
  items_y <- biomarker_data  %>% dplyr::select(-Sample_ID) %>% colnames()

  data <- metadata %>%
    dplyr::filter(eval(filter_expression)) %>%
    dplyr::filter(eval(filter_visit_expression)) %>%
    dplyr::left_join(microbiome_data, by = "Sample_ID") %>%
    dplyr::left_join(biomarker_data,  by = "Sample_ID")

  visits <- eval(filter_visit_expression[[1]][[3]])
  visits_string <- gsub(" months,", ",", paste(visits, collapse = ", "))

  all_results <- purrr::map(items_x, function(x){
    purrr::map(items_y, function(y){
      results <- Kendall::Kendall(
        x = data[[x]],
        y = data[[y]]) %>%
        broom::tidy() %>%
        dplyr::select(Kendall_Tau = statistic,
                      P = p.value) %>%
        dplyr::mutate(
          Item_microbiome = x,
          Item_biomarker  = y,
          .before = "Kendall_Tau"
        )
    }) %>% dplyr::bind_rows()
    }) %>% dplyr::bind_rows() %>%
    dplyr::mutate(Visits = visits_string, .before = Item_microbiome) %>%
    # FDR correction
    dplyr::mutate(
      FDR = p.adjust(P, method = "BH"),
      Significant = dplyr::case_when(
        FDR < FDR_threshold ~ "*",
        FDR < 1 ~ "ns"
      ),
      .after = "P")

  return(all_results)
}


