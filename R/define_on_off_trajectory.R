#' Define on- and off-trajectory subjects
#'
#' Based on Microbiota-for-age Z-scores
#' Documentation will follow
#'
#' @import magrittr
#' @import dplyr
#'
#' @return Documentation will follow
#' @export
#'

define_on_off_trajectory <- function(
  maz_data,
  threshold = 3,
  baseline_visit = c("Baseline"),
  early_visits = c("3 months", "6 months"),
  late_visits  = c("12 months", "15 months")
){

  on_off_trajectory_data_per_sample <- maz_data %>%
      dplyr::mutate(
        Trajectory_status_per_sample = dplyr::case_when(
          abs(MAZ) <= threshold ~ "On-trajectory",
          abs(MAZ) > threshold  ~ "Off-trajectory"),
        Trajectory_status_per_sample = factor(
          Trajectory_status_per_sample,
          levels = c('On-trajectory', 'Off-trajectory'))
      )

  on_off_trajectory_3_months <-
    on_off_trajectory_data_per_sample %>%
    dplyr::filter(Visit %in% "3 months") %>%
    dplyr::group_by(Subject_ID) %>%
    dplyr::summarise(
      Trajectory_status_3_months = dplyr::if_else(
        any(Trajectory_status_per_sample == "Off-trajectory"),
        "Off-trajectory",
        "On-trajectory")
    ) %>%
    dplyr::mutate(Trajectory_status_3_months = factor(
      Trajectory_status_3_months,
      levels = c('On-trajectory', 'Off-trajectory'))
    ) %>%
    dplyr::select(Subject_ID, Trajectory_status_3_months)

  on_off_trajectory_12_months <-
    on_off_trajectory_data_per_sample %>%
    dplyr::filter(Visit %in% "12 months") %>%
    dplyr::group_by(Subject_ID) %>%
    dplyr::summarise(
      Trajectory_status_12_months = dplyr::if_else(
        any(Trajectory_status_per_sample == "Off-trajectory"),
        "Off-trajectory",
        "On-trajectory")
    ) %>%
    dplyr::mutate(Trajectory_status_12_months = factor(
      Trajectory_status_12_months,
      levels = c('On-trajectory', 'Off-trajectory'))
    ) %>%
    dplyr::select(Subject_ID, Trajectory_status_12_months)


  on_off_trajectory_early_visits <-
    on_off_trajectory_data_per_sample %>%
    dplyr::filter(Visit %in% early_visits) %>%
    dplyr::group_by(Subject_ID) %>%
    dplyr::summarise(
      Trajectory_status_early_visits = dplyr::if_else(
        any(Trajectory_status_per_sample == "Off-trajectory"),
        "Off-trajectory",
        "On-trajectory")
      ) %>%
    dplyr::mutate(Trajectory_status_early_visits = factor(
      Trajectory_status_early_visits,
      levels = c('On-trajectory', 'Off-trajectory'))
    ) %>%
    dplyr::select(Subject_ID, Trajectory_status_early_visits)

  on_off_trajectory_late_visits <-
    on_off_trajectory_data_per_sample %>%
    dplyr::filter(Visit %in% late_visits) %>%
    dplyr::group_by(Subject_ID) %>%
    dplyr::summarise(
      Trajectory_status_late_visits = dplyr::if_else(
        any(Trajectory_status_per_sample == "Off-trajectory"),
        "Off-trajectory",
        "On-trajectory")
    ) %>%
    dplyr::mutate(Trajectory_status_late_visits = factor(
      Trajectory_status_late_visits,
      levels = c('On-trajectory', 'Off-trajectory'))
    ) %>%
    dplyr::select(Subject_ID, Trajectory_status_late_visits)

  # Based on the four follow-up visits
  on_off_trajectory_per_subject <-
    on_off_trajectory_data_per_sample %>%
    dplyr::filter(Visit %in% c(early_visits, late_visits)) %>%
    dplyr::group_by(Subject_ID) %>%
    dplyr::summarise(
      num_samples = dplyr::n(),
      Trajectory_status_per_subject = dplyr::if_else(
        any(Trajectory_status_per_sample == "Off-trajectory"),
        "Off-trajectory",
        "On-trajectory")
    ) %>%
    # Modification of overall trajectory status
    # If any sample is off-trajectory, then subject is off-trajectory
    # If all samples are on-trajectory AND zero or one sample is missing, then subject is on-trajectory
    # If all samples are on-trajectory AND more than one sample is missing, then subject is excluded (NA)
    dplyr::mutate(
      Trajectory_status_per_subject = dplyr::case_when(
        Trajectory_status_per_subject == "Off-trajectory" ~ "Off-trajectory",
        num_samples >= length(c(early_visits, late_visits)) - 1 ~ "On-trajectory",
        TRUE ~ NA
    )) %>%
    dplyr::mutate(
      Trajectory_status_per_subject = factor(
        Trajectory_status_per_subject,
        levels = c('On-trajectory', 'Off-trajectory'))
    ) %>%
    dplyr::select(
      Subject_ID,
      Trajectory_status_per_subject)

  on_off_trajectory_data <-
    on_off_trajectory_data_per_sample %>%
    dplyr::left_join(
      on_off_trajectory_3_months,
      by = "Subject_ID"
    ) %>%
    dplyr::left_join(
      on_off_trajectory_12_months,
      by = "Subject_ID"
    ) %>%
    dplyr::left_join(
      on_off_trajectory_early_visits,
      by = "Subject_ID"
    ) %>%
    dplyr::left_join(
      on_off_trajectory_late_visits,
      by = "Subject_ID"
    ) %>%
    dplyr::left_join(
      on_off_trajectory_per_subject,
      by = "Subject_ID"
    )

  return(on_off_trajectory_data)

}
