#' module_gcam.korea_LB1232.Elec_subregions
#'
#' Aggregates subregion's electricity generation to electricity subregions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1232.out_EJ_sR_elec_korea}. The corresponding file in the
#' original data system was \code{LB1232.Elec_subregions.R} (gcam-korea level1).
#' @details Aggregates subregion's electricity generation to electricity subregions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author S. Jeon
module_gcam.korea_LB1232.Elec_subregions <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             "L1231.out_EJ_korea_elec_F_tech"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1232.out_EJ_sR_elec_korea"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    state <- grid_region <- year <- value <- sector <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions") %>%
      select(state, grid_region)
    L1231.out_EJ_korea_elec_F_tech <- get_data(all_data, "L1231.out_EJ_korea_elec_F_tech")

    # ===================================================
    # Aggregating states to electricity subregions
    L1232.out_EJ_sR_elec_korea <- L1231.out_EJ_korea_elec_F_tech %>%
      left_join_error_no_match(states_subregions, by = "state") %>%
      group_by(grid_region, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      # ===================================================
    # Produce outputs
    add_title("Electricity generation by FERC region/fuel/technology") %>%
      add_units("EJ") %>%
      add_comments("L1231.out_EJ_korea_elec_F_tech aggregated to FERC region") %>%
      add_legacy_name("L1232.out_EJ_sR_elec_korea") %>%
      add_precursors("gcam-korea/states_subregions",
                     "L1231.out_EJ_korea_elec_F_tech")

    return_data(L1232.out_EJ_sR_elec_korea)
  } else {
    stop("Unknown command")
  }
}
