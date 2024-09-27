########################################
###  Microbiome trajectory workflow  ###
########################################

# Load libraries ----------------------------------------------------------
library(magrittr)

# Set parameters ----------------------------------------------------------
model <- "genus10"
# model <- "mgs20"

# event <- "antibiotics"
event <- "rti_mild"

filter_expression <- TRUE # for all infants (HMG-CS already excluded)
# filter_expression <- expression(Feeding_group == "HMG" | Delivery_mode == "Caesarean") # for formula-fed CS infants, AND reference HMG-VD infants

filter_expression_ff <- expression(Feeding_group != "HMG") # for formula-fed infants
# filter_expression_ff <- expression(Feeding_group != "HMG" & Delivery_mode == "Caesarean") # for formula-fed CS infants


# Read input data --------------------------------------------------------------
microbiota_age_data <- readRDS(glue::glue("../data/microbiota_age_{model}.rds"))
microbiome_data     <- readRDS(glue::glue("../data/{model}.rds"))
biomarker_data      <- readRDS(glue::glue("../data/biomarker.rds"))
event_data          <- readRDS(glue::glue("../data/{event}.rds"))
metadata            <- readRDS(glue::glue("../data/metadata.rds"))


# Calculate MAZ and trajectory status -------------------------------------
maz_data               <- calculate_MAZ(microbiota_age_data)
on_off_trajectory_data <- define_on_off_trajectory(maz_data)


# Trajectory and trajectory comparison figures ---------------------------------
figure_trajectories <- plot_trajectories(microbiota_age_data, filter_expression)
figure_pvals        <- compare_trajectories_sliding_window(microbiota_age_data, filter_expression)
figure_distance     <- calculate_distance_between_trajectories(microbiota_age_data, filter_expression)


# On/off-trajectory vs feeding group comparison  -------------------------------
on_off_trend_test_results <- compare_on_off_trajectory_between_groups_all_combinations(on_off_trajectory_data, filter_expression)


# Microbiome taxa, metabolic biomarkers ----------------------------------------
microbiome_trajectory_status_comparison <- compare_biome_between_on_off_trajectory(microbiome_data, on_off_trajectory_data, filter_expression_ff)
biomarker_trajectory_status_comparison  <- compare_biome_between_on_off_trajectory(biomarker_data,  on_off_trajectory_data, filter_expression_ff)
microbiome_biomarker_correlation        <- correlate_microbiome_biomarkers(microbiome_data, biomarker_data, metadata, filter_expression_ff)


# Clinical outcomes ------------------------------------------------------------
cox_ph_results       <- perform_cox_ph_test_all_combinations(event_data, on_off_trajectory_data, filter_expression_ff)
kaplan_meier_figure  <- plot_kaplan_meier(event_data, on_off_trajectory_data, filter_expression_ff) # # Figure from Main (either antibiotics or mild RTI)
kaplan_meier_figures <- plot_kaplan_meier_all_combinations(event_data, event, on_off_trajectory_data, filter_expression_ff) # All 3 figures (Main and ED, either antibiotics or mild RTI)



