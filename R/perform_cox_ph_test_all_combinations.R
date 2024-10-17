#' Perform Cox Proportional-Hazards test - all combinations used in the study
#'
#' Documentation will follow
#'
#'
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import stringi
#' @import glue
#' @import survival
#' @import tidyr
#' @import tibble
#' @importFrom stats as.formula median p.adjust pchisq sd
#'
#' @return Documentation will follow
#' @export

perform_cox_ph_test_all_combinations <- function(
  event_data, # antibiotics # rti_mild
  on_off_trajectory_data,
  filter_expression = expression(Feeding_group != "HMG")
){

  event_data_name <- deparse(substitute(event_data))

  combinations_cox_ph <- tibble::tibble(
    trajectory_status_variable = list(
      'Trajectory_status_per_subject',
      'Trajectory_status_per_subject',
      'Trajectory_status_3_months',
      'Trajectory_status_3_months',
      'Trajectory_status_12_months',
      'Trajectory_status_12_months'),
    time_limits = list(
      list(1*30, 15*30),
      list(1*30, 15*30),
      list(3*30, 12*30),
      list(3*30, 12*30),
      list(12*30, 15*30),
      list(12*30, 15*30))
  ) %>%
    tidyr::crossing(
      covariates = list(
        list("Feeding_group", "Delivery_mode", "Sex"),
        list("Feeding_group", "Delivery_mode"),
        list("Feeding_group"),
        list("")
      )) %>%
    dplyr::mutate(trajectory_status_variable = unlist(trajectory_status_variable))

  all_results <- purrr::pmap(combinations_cox_ph,
                             function(trajectory_status_variable, time_limits, covariates){

                               time_limits_unlisted <- time_limits %>% unlist()
                               covariates_unlisted <- covariates %>% unlist()

                               time_limits_string <- paste(paste(time_limits_unlisted/30, collapse = '-'), "months")
                               covariates_string <- paste(covariates_unlisted, collapse = ", ")
                               adjustment_string <- ifelse(covariates_string == "",
                                                           "Unadjusted",
                                                           stringr::str_c("Adjusted for ", covariates_string))

                               # Prepare data for analysis
                               data_test <- event_data %>%
                                 dplyr::filter(eval(filter_expression)) %>%
                                 dplyr::filter(Days_to_event >= time_limits_unlisted[1] &
                                                 Days_to_event <= time_limits_unlisted[2]) %>%
                                 dplyr::group_by(Subject_ID) %>%
                                 dplyr::summarise(Days_to_first_event = min(Days_to_event)) %>%
                                 dplyr::right_join(event_data %>%
                                                     dplyr::filter(eval(filter_expression)) %>%
                                                     dplyr::select(Subject_ID,
                                                                   Days_to_latest_visit,
                                                                   Feeding_group, Delivery_mode, Sex) %>%
                                                     dplyr::distinct(Subject_ID, .keep_all = TRUE),
                                                   by = "Subject_ID") %>%
                                 dplyr::right_join(
                                   on_off_trajectory_data %>%
                                     dplyr::filter(eval(filter_expression)) %>%
                                     dplyr::select(Subject_ID, !!dplyr::sym(trajectory_status_variable)) %>%
                                     dplyr::distinct(Subject_ID, .keep_all = TRUE),
                                   by = "Subject_ID"
                                 ) %>%
                                 dplyr::mutate(Timepoint = dplyr::case_when(
                                   !is.na(Days_to_first_event) ~ Days_to_first_event,
                                   TRUE ~ Days_to_latest_visit
                                 ),
                                 Event_type = dplyr::case_when(
                                   !is.na(Days_to_first_event) ~ "event",
                                   TRUE ~ "censored"
                                 ),
                                 Timepoint = dplyr::case_when(
                                   Event_type == "censored" & Timepoint > time_limits_unlisted[2] ~ time_limits_unlisted[2],
                                   TRUE ~ Timepoint
                                 ),
                                 Event_code = dplyr::case_when(
                                   Event_type == "censored" ~ 0,
                                   TRUE ~ 1
                                 ), .before = Days_to_first_event
                                 ) %>%
                                 dplyr::select(Subject_ID,
                                               Timepoint, Event_code,
                                               !!dplyr::sym(trajectory_status_variable),
                                               any_of(covariates_unlisted)) %>%
                                 dplyr::filter(!is.na(Event_code) & ! is.na(!!dplyr::sym(trajectory_status_variable))) %>%
                                 dplyr::mutate(Timepoint = as.integer(Timepoint),
                                               Event_code = as.integer(Event_code),
                                               !!dplyr::sym(trajectory_status_variable) := factor(!!dplyr::sym(trajectory_status_variable),
                                                                                                  levels = c("Off-trajectory", "On-trajectory")),
                                               !!dplyr::sym(trajectory_status_variable) := as.integer(!!dplyr::sym(trajectory_status_variable))
                                 ) %>%
                                 dplyr::mutate_if(is.factor, factor) %>%
                                 as.data.frame()

                               n <- data_test[[trajectory_status_variable]] %>% table()
                               n_off <- n[names(n) == 1] %>% as.integer()
                               n_on  <- n[names(n) == 2] %>% as.integer()


                               ### Custom code for CS infants only - remove DM from covariates
                               if ("Delivery_mode" %in% covariates_unlisted){
                                 if (length(unique(data_test[['Delivery_mode']])) == 1){
                                   covariates_unlisted <- covariates_unlisted[covariates_unlisted != "Delivery_mode"]
                                   covariates_string <- paste(covariates_unlisted, collapse = ", ")
                                   adjustment_string <- ifelse(covariates_string == "",
                                                               "Unadjusted",
                                                               stringr::str_c("Adjusted for ", covariates_string))
                                 }}

                               ### Perform CoxPH test
                               formula_test <- paste("survival::Surv(",
                                                     as.name('Timepoint'), ", ",
                                                     as.name('Event_code'), ") ~ ",
                                                     as.name(trajectory_status_variable))
                               if (any(!stringi::stri_isempty(covariates_unlisted))){
                                 formula_test <- paste(formula_test, "+",
                                                       paste(covariates_unlisted, collapse = " + "))
                               }
                               test_fit <- survival::coxph(as.formula(formula_test), data = data_test)

                               ### Extract test results
                               delta_cox <- summary(test_fit)$coefficients[trajectory_status_variable , 2]
                               ci_cox <- c(summary(test_fit)$conf.int[trajectory_status_variable , 3],
                                           summary(test_fit)$conf.int[trajectory_status_variable , 4])
                               p_cox <- summary(test_fit)$coefficients[trajectory_status_variable , 5]
                               p_cox_string <- if (p_cox < 0.0001) "p < 0.0001" else glue::glue("p: {round(p_cox, 4)}")


                               test_results <- tibble::tibble(
                                 "Event_data" = event_data_name,
                                 "Variable" = trajectory_status_variable,
                                 "Followed between" = time_limits_string,
                                 "n_on" = n_on,
                                 "n_off" = n_off,
                                 "Adjustment" = adjustment_string,
                                 "HR" = delta_cox,
                                 "p" = p_cox,
                                 "ci_low" = ci_cox[1],
                                 "ci_high" = ci_cox[2]
                               )
                               return(test_results)
                             }) %>% dplyr::bind_rows()

  return(all_results)
}
