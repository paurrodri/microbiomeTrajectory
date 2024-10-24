% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_heatmap.R
\name{plot_heatmap}
\alias{plot_heatmap}
\title{Plot heatmap of correlations microbiome-biomarkers incl. comparisons between on- & off-trajectory}
\usage{
plot_heatmap(
  microbiome_biomarker_correlation_results,
  microbiome_on_off_comparison_results,
  biomarker_on_off_comparison_results,
  FDR_threshold = 0.05,
  correlation_colors = c("#093769", "#0E539E", "#FFFFFF", "#BF1919", "#911313"),
  correlation_breaks = c(-1, -0.25, 0, 0.25, 1),
  correlation_legend_title = "Microbiome - Biomarker correlation",
  color_on_trajectory = "#30A190",
  color_off_trajectory = "#E09B65",
  color_non_significant = "#EEEEEE",
  label_on_trajectory = "On-trajectory",
  label_off_trajectory = "Off-trajectory",
  label_non_significant = "Non-significant",
  sidebar_legend_title = "Group with the highest value",
  row_title = "Microbiome",
  col_title = "Biomarkers",
  save_figure = FALSE,
  figure_name = "heatmap.pdf"
)
}
\value{
Documentation will follow
}
\description{
Documentation will follow
}
\author{
Paula Rodríguez-García (paurrodri) and Maria Novosolov (marianovosolov)
}
