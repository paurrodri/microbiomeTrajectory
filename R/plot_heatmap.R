#' Plot heatmap of correlations microbiome-biomarkers incl. comparisons between on- & off-trajectory
#'
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import glue
#' @import circlize
#' @import RColorBrewer
#' @import ComplexHeatmap
#'
#'
#'
#' @return Documentation will follow
#' @export
#' @author Paula Rodríguez-García (paurrodri) and Maria Novosolov (marianovosolov)
#'

plot_heatmap <- function(
  microbiome_biomarker_correlation_results,
  microbiome_on_off_comparison_results,
  biomarker_on_off_comparison_results,
  correlation_colors     = c("#093769", "#0E539E", "#FFFFFF", "#BF1919", "#911313"),
  correlation_breaks     = c(-1, -0.25, 0, 0.25, 1),
  color_on_trajectory    = "#30A190",
  color_off_trajectory   = "#E09B65",
  color_non_significant = "#EEEEEE",
  label_on_trajectory   = "On-trajectory",
  label_off_trajectory  = "Off-trajectory",
  label_non_significant = "Non-significant",
  save_figure = FALSE,
  figure_name = "heatmap.pdf"
){


  # Prepare data - central heatmap -----
  central_data <- list()

  central_data[["correlation"]] <-
    microbiome_biomarker_correlation_results %>%
    dplyr::select(Item_microbiome, Item_biomarker, Kendall_Tau) %>%
    tidyr::pivot_wider(
      id_cols = Item_microbiome,
      names_from = Item_biomarker,
      values_from = Kendall_Tau) %>%
    tibble::column_to_rownames("Item_microbiome") %>%
    as.matrix()

  central_data[["FDR"]] <-
    microbiome_biomarker_correlation_results %>%
    dplyr::select(Item_microbiome, Item_biomarker, FDR) %>%
    tidyr::pivot_wider(
      id_cols = Item_microbiome,
      names_from = Item_biomarker,
      values_from = FDR) %>%
    tibble::column_to_rownames("Item_microbiome") %>%
    as.matrix()

  central_data[["p_value"]] <-
    microbiome_biomarker_correlation_results %>%
    dplyr::select(Item_microbiome, Item_biomarker, P) %>%
    tidyr::pivot_wider(
      id_cols = Item_microbiome,
      names_from = Item_biomarker,
      values_from = P) %>%
    tibble::column_to_rownames("Item_microbiome") %>%
    as.matrix()

  # Prepare data - sidebars -----
  visits <- unique(microbiome_on_off_comparison_results$Visit)
  visits_recoded <- stringr::str_replace(visits, " ", "_")
  sidebar_data <- list()

  anno <- purrr:::imap(visits, function(visit, idx){
    visit_recoded <- visits_recoded[idx]
    sidebar_data  <- list(
      microbiome_on_off_comparison_results %>%
        dplyr::filter(Visit == visit) %>%
        dplyr::select(Item, Item_higher_in) %>%
        dplyr::mutate(Item_higher_in = as.character(Item_higher_in)) %>%
        tibble::column_to_rownames("Item")
    )
    names(sidebar_data) <- visit_recoded
    sidebar_data_name <- glue::glue('sidebar_data_{visit_recoded}')
    assign(sidebar_data_name, sidebar_data)




    output <- .make_heatmap_annotation(
      get(sidebar_data_name),
      annotation_type = "column",
      col = sidebar_colors,
      pt_size = 2.5
    )

    return(output)

  }) %>% purrr::list_c()

  attempt_sidebar_data # get(sidebar_data_name)
  attempt_sidebar # output
  attempt_purrr_list # anno



  vertical_sidebar_annotation_list <- purrr:::imap(visits,
      function(visit, idx){



      }) %>% purrr::list_c()




  .make_heatmap_annotation(
    vertical_sidebar_data_list[[1]],
    annotation_type = "column",
    col = sidebar_colors,
    pt_size = 2.5
  )




  horizontal_sidebar_data_list <- purrr::map(
    unique(biomarker_on_off_comparison_results$Visit),
    function(visit){

      output <- list(
        biomarker_on_off_comparison_results %>%
          dplyr::filter(Visit == visit) %>%
          dplyr::select(Item, Item_higher_in) %>%
          dplyr::mutate(Item_higher_in = as.character(Item_higher_in)) %>%
          tibble::column_to_rownames("Item")
      )
      names(output) <- visit

      return(output)

    })


  # Colors -----
  central_colors <- circlize::colorRamp2(
    correlation_breaks,
    correlation_colors,
    transparency = 0)

  sidebar_colors        <- c(color_on_trajectory, color_off_trajectory, color_non_significant)
  names(sidebar_colors) <- c(label_on_trajectory, label_off_trajectory, label_non_significant)

  # Sidebars -----


  vertical_sidebar_annotation_list <-
    purrr:::imap(
    unique(biomarker_on_off_comparison_results$Visit),
    function(visit, idx){


      output <- .make_heatmap_annotation(
        vertical_sidebar_data_list[[idx]][[visit]],
        annotation_type = "column",
        col = sidebar_colors,
        pt_size = 2.5
      )[[1]]

      return(output)
    }) %>% purrr::list_c()

  vertical_sidebar_annotation_list
  attempt_sidebar
  attempt_sidebar_data
  purrr::list_c(c(attempt_sidebar, attempt_sidebar))


  right_annotation <- eval(str2expression(
    glue::glue(
      "ComplexHeatmap::rowAnnotation({glue::glue_collapse(glue::glue('{vertical_sidebar_annotation_list}'), sep = ', ')})"
    )
  ))

  #
  # attempt_sidebar_data <- list(microbiome_on_off_comparison_results %>%
  #                                dplyr::filter(Visit == '3 months') %>%
  #                                dplyr::select(Item, Item_higher_in) %>%
  #                                dplyr::mutate(Item_higher_in = as.character(Item_higher_in)) %>%
  #                                tibble::column_to_rownames("Item"))
  # names(attempt_sidebar_data) <- '3 months'
  # attempt_sidebar_ <-
    .make_heatmap_annotation(
    attempt_sidebar_data,
    annotation_type = "column",
    col = sidebar_colors,
    pt_size = 2.5
  )
  .make_heatmap_annotation(
    sidebar_data_3_months,
    annotation_type = "column",
    col = sidebar_colors,
    pt_size = 2.5
  )
  .make_heatmap_annotation(
    get(sidebar_data_name),
    annotation_type = "column",
    col = sidebar_colors,
    pt_size = 2.5
  )
  #
  # attempt_anno <- eval(str2expression(
  #   glue::glue(
  #     "ComplexHeatmap::rowAnnotation({glue::glue_collapse(glue::glue('{purrr::list_c(c(attempt_sidebar, attempt_sidebar))}'), sep = ', ')})"
  #   )
  # ))


  # vertical_sidebar_list[[1]]
  # attempt_sidebar
  # purrr::list_c(c(vertical_sidebar_list))
  # purrr::list_c(c(vertical_sidebar_annotation_list))
  # purrr::list_c(c(attempt_sidebar, attempt_sidebar))
  #
  # purrr::map(vertical_sidebar_list, )



  right_annotation <- eval(str2expression(
    glue::glue(
      "ComplexHeatmap::rowAnnotation({glue::glue_collapse(glue::glue('{purrr::list_c(c(vertical_sidebar_list[[1]], vertical_sidebar_list[[2]]}'), sep = ', ')})" # purrr::list_c(vertical_sidebar_list)
    )
  ))

  horizontal_sidebar_list <- purrr:::map(
    names(horizontal_sidebar_data_list),
    function(name){
      .make_heatmap_annotation(
        horizontal_sidebar_data_list[name],
        annotation_type = "row",
        col = sidebar_colors,
        pt_size = 2.5
      )
  })








  if(save_figure){
    heatmap
    # ggplot2::ggsave(
    #   filename = figure_name,
    #   units = "cm",
    #   height = 8,
    #   width = 10,
    #   scale = 1
    # )
  }

  return(plot)


}



.make_heatmap_annotation <- function(
  sidebar_data,
  sidebar_names = "all",
  pt_size = 1.25,
  na_col = 'white',
  annotation_type = "column",
  col = NULL, ...) {

  # sidebar_data <- get(sidebar_data_name)
    data_type <- unlist(lapply(sidebar_data[[1]], class))
    if(any(data_type == "factor")){
      usethis::ui_stop("Please convert all the factor variables to character for the plotting")
    }
    if("all"  %in% sidebar_names){
      extract_data <- names(sidebar_data)
    } else {
      extract_data <- sidebar_names
    }
    Annot_list <- list()

    # i <- extract_data
    for (i in extract_data){
      if(!is.null(names(extract_data))){
        bar_name <- names(extract_data)[stringr::str_which(extract_data, i)]
      } else{
        bar_name <- i
      }
      annot_name = i
      if(nrow(sidebar_data[[i]]) == 0){
        # if(length(sidebar_data[[i]]) == 0){ #if(nrow(sidebar_data[[i]]) == 0){
        next
      }
      if (all(data_type == "character")) {
        if(is.null(col) | is.null(names(col))){
          usethis::ui_stop("please provide a named vector to use as a color palette")
        }
        annot_data = glue::glue("{substitute(sidebar_data)}[['{i}']][[1]]")
        annot_pch = "NULL"
        # make the color palette
        col_categories <- unique(sidebar_data[[i]][[1]]) %>% na.omit()
        col_names <- col_categories %>%
          paste(collapse = "', '")  %>%
          paste0("c(", "'", ., "'", ")")
        col_palette <- col[col_categories] %>%
          paste(collapse = "', '")  %>%
          paste0("c(","'", ., "'", ")")
        if(length(col[col_categories]) == 0){
          Annot_list[[annot_name]] <-
            glue::glue(
              "'{bar_name}' = ComplexHeatmap::anno_simple(x = {annot_data}, pch = {annot_pch}, pt_size = unit({pt_size}, 'mm'), na_col = '{na_col}')"
            )
        } else {
          print("here once")
          Annot_list[[annot_name]] <-
            glue::glue(
              "'{bar_name}' = ComplexHeatmap::anno_simple(x = {annot_data}, pch = {annot_pch}, pt_size = unit({pt_size}, 'mm'), na_col = '{na_col}', col = setNames({col_palette}, {col_names}))"
            )
          print(Annot_list[[annot_name]])
        }

      } else {
        print("here is not supposed to pass")
        if(is.null(col) | length(col) < length(sidebar_data)){
          usethis::ui_stop("please provide a function to use as the color palette - one for each of the continious unique side bars you want")
        }
        annot_data = glue::glue("{substitute(sidebar_data)}[['{i}']][['correlation']]")
        annot_pch = glue::glue("{substitute(sidebar_data)}[['{i}']][['significant']]")
        annot_pch_col = glue::glue("{substitute(sidebar_data)}[['{i}']][['sig_col']]")
        col_palette <- col[[i]]

        Annot_list[[annot_name]] <-
          glue::glue(
            "'{bar_name}' = ComplexHeatmap::anno_simple(x = {annot_data}, pch = {annot_pch}, pt_gp = gpar(col = {annot_pch_col}), pt_size = unit({pt_size}, 'mm'), na_col = '{na_col}', col = {substitute(col)}[['{i}']])"
          )
      }

    }
    return(Annot_list)
}


