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
#' @import grid
#' @import ggplot2
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
  FDR_threshold            = 0.05,
  correlation_colors       = c("#093769", "#0E539E", "#FFFFFF", "#BF1919", "#911313"),
  correlation_breaks       = c(-1, -0.25, 0, 0.25, 1),
  correlation_legend_title = "Microbiome - Biomarker correlation",
  color_on_trajectory      = "#30A190",
  color_off_trajectory     = "#E09B65",
  color_non_significant    = "#EEEEEE",
  label_on_trajectory      = "On-trajectory",
  label_off_trajectory     = "Off-trajectory",
  label_non_significant    = "Non-significant",
  sidebar_legend_title     = "Group with the highest value",
  row_title                = "Microbiome",
  col_title                = "Biomarkers",
  save_figure = FALSE,
  figure_name = "heatmap.pdf"
){

  # Initial checks ------
  if (!all.equal(
    unique(microbiome_on_off_comparison_results$Visit),
    unique(biomarker_on_off_comparison_results$Visit)
  )){
    usethis::ui_stop("Visits in microbiome and biomarker data should be identical")
  }
  if (!all.equal(
    unique(biomarker_on_off_comparison_results$Visit),
    c("3 months", "6 months", "12 months", "15 months")
  )){
    usethis::ui_stop("Current version of function does not support other visits than 3, 6, 12, and 15 months.")
  }

  # Prepare data for central heatmap -----
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

  column_order <- colnames(central_data[['correlation']])


  # Prepare data for sidebars -----
  visits <- unique(microbiome_on_off_comparison_results$Visit)
  visits_recoded <- stringr::str_replace(visits, " ", "_")

  for (idx in seq(1, length(visits))){
    visit         <- visits[idx]
    visit_recoded <- visits_recoded[idx]
    vertical_sidebar_data  <- list(
      microbiome_on_off_comparison_results %>%
        dplyr::filter(Visit == visit) %>%
        dplyr::select(Item, Item_higher_in) %>%
        dplyr::mutate(Item_higher_in = as.character(Item_higher_in)) %>%
        tibble::column_to_rownames("Item")
    )
    names(vertical_sidebar_data) <- visit
    vertical_sidebar_data_name <- glue::glue('vertical_sidebar_data_{visit_recoded}')
    assign(vertical_sidebar_data_name, vertical_sidebar_data)

    horizontal_sidebar_data  <- list(
      biomarker_on_off_comparison_results %>%
        dplyr::filter(Visit == visit) %>%
        dplyr::select(Item, Item_higher_in) %>%
        dplyr::arrange(match(Item, column_order)) %>%
        dplyr::mutate(Item_higher_in = as.character(Item_higher_in)) %>%
        tibble::column_to_rownames("Item")
    )
    names(horizontal_sidebar_data) <- visit
    horizontal_sidebar_data_name <- glue::glue('horizontal_sidebar_data_{visit_recoded}')
    assign(horizontal_sidebar_data_name, horizontal_sidebar_data)
  }

  # Set colors -----
  central_colors <- circlize::colorRamp2(
    correlation_breaks,
    correlation_colors,
    transparency = 0)

  sidebar_colors        <- c(color_on_trajectory, color_off_trajectory, color_non_significant)
  names(sidebar_colors) <- c(label_on_trajectory, label_off_trajectory, label_non_significant)

  # Make sidebars -----
  vertical_sidebar_annotation <- purrr::list_c(c(
    .make_heatmap_annotation(
      vertical_sidebar_data_3_months,
      annotation_type = "row",
      col = sidebar_colors,
      pt_size = 2.5
    ),
    .make_heatmap_annotation(
      vertical_sidebar_data_6_months,
      annotation_type = "row",
      col = sidebar_colors,
      pt_size = 2.5
    ),
    .make_heatmap_annotation(
      vertical_sidebar_data_12_months,
      annotation_type = "row",
      col = sidebar_colors,
      pt_size = 2.5
    ),
    .make_heatmap_annotation(
      vertical_sidebar_data_15_months,
      annotation_type = "row",
      col = sidebar_colors,
      pt_size = 2.5
    )
  ))

  vertical_sidebar <- eval(str2expression(
    glue::glue(
      "ComplexHeatmap::rowAnnotation({glue::glue_collapse(glue::glue('{vertical_sidebar_annotation}'), sep = ', ')})"
    )
  ))

  horizontal_sidebar_annotation <- purrr::list_c(c(
    .make_heatmap_annotation(
      horizontal_sidebar_data_3_months,
      annotation_type = "column",
      col = sidebar_colors,
      pt_size = 2.5
    ),
    .make_heatmap_annotation(
      horizontal_sidebar_data_6_months,
      annotation_type = "column",
      col = sidebar_colors,
      pt_size = 2.5
    ),
    .make_heatmap_annotation(
      horizontal_sidebar_data_12_months,
      annotation_type = "column",
      col = sidebar_colors,
      pt_size = 2.5
    ),
    .make_heatmap_annotation(
      horizontal_sidebar_data_15_months,
      annotation_type = "column",
      col = sidebar_colors,
      pt_size = 2.5
    )
  ))

  horizontal_sidebar <- eval(str2expression(
      glue::glue(
        "ComplexHeatmap::HeatmapAnnotation(
      {glue::glue_collapse(
      glue::glue(
      '{horizontal_sidebar_annotation}'),
      sep = ', ')},
      annotation_name_side = 'left')"
      )
    ))

  # Make legends ------
  central_legend <- ComplexHeatmap::Legend(
    col_fun   = central_colors,
    title     = correlation_legend_title,
    direction = "horizontal",
    at        = correlation_breaks
    )

  sidebar_legend <- ComplexHeatmap::Legend(
    labels    = names(sidebar_colors),
    title     = sidebar_legend_title,
    legend_gp = grid::gpar(fill = sidebar_colors),
    nrow      = length(sidebar_colors),
    by_row = F
    )

  all_legends <- ComplexHeatmap::packLegend(
    central_legend,
    sidebar_legend,
    column_gap = ggplot2::unit(2, "mm"),
    row_gap    = ggplot2::unit(0.5, "cm")
  )


  # Set aesthetics -----
  row_title_gp    <- grid::gpar(fontsize = 15, fontface = "bold")
  col_title_gp    <- grid::gpar(fontsize = 15, fontface = "bold")
  row_title_rot    <- 90
  col_title_rot    <- 0

  row_names_gp    <- grid::gpar(fontsize = 13)
  col_names_gp    <- grid::gpar(fontsize = 13)
  row_names_rot   <- 0
  col_names_rot   <- 45

  row_labels <- rownames(central_data[['correlation']])
  col_labels <- colnames(central_data[['correlation']])

  circle_size <- 2


  # Plot heatmap -----
  heatmap <- ComplexHeatmap::Heatmap(
    matrix = central_data[["correlation"]],

    ## Color and layout
    col    = central_colors,
    na_col = "white",
    border = TRUE,
    width  = ggplot2::unit(6, "mm") * ncol(central_data[["correlation"]]),
    height = ggplot2::unit(6, "mm") * nrow(central_data[["correlation"]]),

    ## Labels
    show_row_names   = TRUE,
    row_names_gp     = row_names_gp,
    row_labels       = row_labels,
    row_names_rot    = row_names_rot,
    column_names_gp  = col_names_gp,
    column_names_rot = col_names_rot,
    column_labels    = col_labels,

    ## Titles
    row_title        = row_title,
    column_title     = col_title,
    row_title_gp     = row_title_gp,
    row_title_rot    = row_title_rot,
    column_title_gp  = col_title_gp,
    column_title_rot = col_title_rot,

    ## Clustering
    cluster_rows                = TRUE,
    cluster_columns             = TRUE,
    column_dend_height          = ggplot2::unit(2, "cm"),
    clustering_distance_columns = "euclidean",
    clustering_method_columns   = "ward.D2",
    clustering_distance_rows    = "euclidean",
    clustering_method_rows      = "ward.D2",

    ## Circle on significant cells
    cell_fun = function(j, i, x, y, width, height, fill) {
      if(as.matrix(central_data[["p_value"]])[i, j] < 0.05) {
        if(as.matrix(central_data[["FDR"]])[i, j] < FDR_threshold) {
        ## If significant, add filled circle, black
        grid::grid.points(x, y,
                    pch  = 16,
                    size = ggplot2::unit(circle_size, "mm"),
                    gp   = grid::gpar(col = "black"))
        }  else {
        ## If nominally-significant, add filled circle, grey
          grid::grid.points(x, y,
                      pch  = 16,
                      size = ggplot2::unit(circle_size, "mm"),
                      gp   = grid::gpar(col = "#989994"))
            }
      }
    },
    # Sidebars
    right_annotation    = vertical_sidebar,
    bottom_annotation   = horizontal_sidebar,
    show_heatmap_legend = FALSE
  )

  # Add the legends
  heatmap <- ComplexHeatmap::draw(
    heatmap,
    annotation_legend_side = "left",
    heatmap_legend_side    = "left",
    merge_legend           = FALSE,
    annotation_legend_list = all_legends
  )

  if(save_figure){
    width  <- 15
    height <- 0.3 * max(nrow(central_data[['correlation']]), 40)
    heatmap
    pdf(figure_name,
        width = width,
        height = height)
    ComplexHeatmap::draw(ht, background = "transparent")
    dev.off()
  }

  return(heatmap)
}



.make_heatmap_annotation <- function(
  sidebar_data,
  sidebar_names = "all",
  pt_size = 1.25,
  na_col = 'white',
  annotation_type = "column",
  col = NULL, ...) {

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
              "'{bar_name}' = ComplexHeatmap::anno_simple(x = {annot_data}, pch = {annot_pch}, pt_size = ggplot2::unit({pt_size}, 'mm'), na_col = '{na_col}')"
            )
        } else {
          Annot_list[[annot_name]] <-
            glue::glue(
              "'{bar_name}' = ComplexHeatmap::anno_simple(x = {annot_data}, pch = {annot_pch}, pt_size = ggplot2::unit({pt_size}, 'mm'), na_col = '{na_col}', col = setNames({col_palette}, {col_names}))"
            )
        }

      } else {
        if(is.null(col) | length(col) < length(sidebar_data)){
          usethis::ui_stop("please provide a function to use as the color palette - one for each of the continious unique side bars you want")
        }
        annot_data = glue::glue("{substitute(sidebar_data)}[['{i}']][['correlation']]")
        annot_pch = glue::glue("{substitute(sidebar_data)}[['{i}']][['significant']]")
        annot_pch_col = glue::glue("{substitute(sidebar_data)}[['{i}']][['sig_col']]")
        col_palette <- col[[i]]

        Annot_list[[annot_name]] <-
          glue::glue(
            "'{bar_name}' = ComplexHeatmap::anno_simple(x = {annot_data}, pch = {annot_pch}, pt_gp = gpar(col = {annot_pch_col}), pt_size = ggplot2::unit({pt_size}, 'mm'), na_col = '{na_col}', col = {substitute(col)}[['{i}']])"
          )
      }

    }
    return(Annot_list)
}


