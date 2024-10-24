########################################
###  Microbiome trajectory workflow  ###
########################################

# Load libraries ---------------------------------------------------------------
library(magrittr)

# Set parameters ---------------------------------------------------------------
model <- "genus10"
# model <- "mgs20"

subset <- "all" # all infants (HMG-CS already excluded)
# subset <- "C-section" # formula-fed CS infants, AND reference HMG-VD infants

# Process parameters -----------------------------------------------------------
if (subset == "all"){
  filter_expression             <- TRUE # for all infants (HMG-CS already excluded)
  filter_expression_ff          <- expression(Feeding_group != "HMG") # for formula-fed infants
  } else if (subset == "C-section"){
  filter_expression             <- expression(Feeding_group == "HMG" | Delivery_mode == "Caesarean") # for formula-fed CS infants, AND reference HMG-VD infants
  filter_expression_ff          <- expression(Feeding_group != "HMG" & Delivery_mode == "Caesarean") # for formula-fed CS infants
  }
if (model == "genus10"){
  heatmap_row_title <- "Microbiome genera"
} else if (model == "mgs20"){
  heatmap_row_title <- "Microbiome species"
}

# Read input data --------------------------------------------------------------
microbiota_age_data    <- readRDS(glue::glue("../data/microbiota_age_{model}.rds"))
shap_data              <- readRDS(glue::glue("../data/shap_{model}.rds"))
model_performance_data <- readRDS(glue::glue("../data/age_predictor_performance_metrics.rds"))
microbiome_data        <- readRDS(glue::glue("../data/{model}.rds"))
biomarker_data         <- readRDS(glue::glue("../data/biomarker.rds"))
antibiotics_data       <- readRDS(glue::glue("../data/antibiotics.rds"))
rti_mild_data          <- readRDS(glue::glue("../data/rti_mild.rds"))
metadata               <- readRDS(glue::glue("../data/metadata.rds"))


# Calculate MAZ and trajectory status ------------------------------------------
maz_data               <- calculate_MAZ(microbiota_age_data)
on_off_trajectory_data <- define_on_off_trajectory(maz_data)
figure_maz             <- plot_maz(maz_data, filter_expression)


# Microbiota trajectories  -----------------------------------------------------
figure_reference_trajectory <- plot_reference_trajectory(microbiota_age_data)
figure_trajectories         <- plot_trajectories(microbiota_age_data, filter_expression)
figure_pvals                <- compare_trajectories_between_groups(microbiota_age_data, filter_expression)


# Model performance and SHAP ----------------------------------------------
figure_shap <- plot_shap_values(shap_data, model_performance_data, model)

# On/off-trajectory vs feeding group comparison  -------------------------------
on_off_trend_test_results  <- compare_on_off_trajectory_between_groups_all_combinations(on_off_trajectory_data, filter_expression)
figure_on_off_groups_early <- plot_on_off_trajectory_between_groups(on_off_trajectory_data, filter_expression, TG_group = "TGs", trajectory_status_variable = "Trajectory_status_early_visits", label_on_trajectory = "On-trajectory 3-6 months", label_off_trajectory = "Off-trajectory 3-6 months")
figure_on_off_groups_late  <- plot_on_off_trajectory_between_groups(on_off_trajectory_data, filter_expression, TG_group = "TGs", trajectory_status_variable = "Trajectory_status_late_visits", label_on_trajectory = "On-trajectory 12-15 months", label_off_trajectory = "Off-trajectory 12-15 months")


# Microbiome taxa, metabolic biomarkers ----------------------------------------
microbiome_trajectory_status_comparison <- compare_biome_between_on_off_trajectory(microbiome_data, on_off_trajectory_data, filter_expression_ff, FDR_threshold = 0.05)
biomarker_trajectory_status_comparison  <- compare_biome_between_on_off_trajectory(biomarker_data,  on_off_trajectory_data, filter_expression_ff, FDR_threshold = 0.05)
microbiome_biomarker_correlation        <- correlate_microbiome_biomarkers(microbiome_data, biomarker_data, metadata, filter_expression_ff, FDR_threshold = 0.05)
figure_heatmap_correlation_comparison   <- plot_heatmap(microbiome_biomarker_correlation, microbiome_trajectory_status_comparison, biomarker_trajectory_status_comparison, row_title = heatmap_row_title, FDR_threshold = 0.05)
figure_butyrate_on_off_trajectory       <- plot_biomarker_on_off_trajectory("Butyric acid",   biomarker_data, on_off_trajectory_data, filter_expression_ff)
figure_propionate_on_off_trajectory     <- plot_biomarker_on_off_trajectory("Propionic acid", biomarker_data, on_off_trajectory_data, filter_expression_ff)


# Clinical outcomes ------------------------------------------------------------
figure_kaplan_meier_antibiotics   <- plot_kaplan_meier(antibiotics_data, on_off_trajectory_data, filter_expression_ff) # Figure from Main
figure_kaplan_meier_rti_mild      <- plot_kaplan_meier(rti_mild_data,    on_off_trajectory_data, filter_expression_ff) # Figure from Main
figures_kaplan_meier_antibiotics  <- plot_kaplan_meier_all_combinations(antibiotics_data, "antibiotics", on_off_trajectory_data, filter_expression_ff) # All 3 figures (Main and Extended)
figures_kaplan_meier_rti_mild     <- plot_kaplan_meier_all_combinations(rti_mild_data,    "rti_mild", on_off_trajectory_data, filter_expression_ff) # All 3 figures (Main and Extended)
cox_ph_results_antibiotics        <- perform_cox_ph_test_all_combinations(antibiotics_data, on_off_trajectory_data, filter_expression_ff)
cox_ph_results_rti_mild           <- perform_cox_ph_test_all_combinations(rti_mild_data,    on_off_trajectory_data, filter_expression_ff)
figure_cox_ph_antibiotics         <- plot_forest_plot_cox_ph(cox_ph_results_antibiotics)
figure_cox_ph_rti_mild            <- plot_forest_plot_cox_ph(cox_ph_results_rti_mild)

