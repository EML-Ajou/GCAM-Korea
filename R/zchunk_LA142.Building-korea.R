#' module_gcam.korea_LA142.Building
#'
#' Downscaling each state and sector's shares of Korea building energy use by fuel
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.in_EJ_korea_bld_F}. The corresponding file in the
#' original data system was \code{LA142.Building.R} (gcam-korea level1).
#' @details Scaled national-level building energy consumption by portion of total building energy use by fuel for each state and sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author S. Jeon
module_gcam.korea_LA142.Building <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L142.in_EJ_R_bld_F_Yh",
              "L101.inKEEI_EJ_state_S_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.in_EJ_korea_bld_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L142.in_EJ_R_bld_F_Yh <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh")
    L101.inKEEI_EJ_state_S_F <- get_data(all_data, "L101.inKEEI_EJ_state_S_F")

    # Silence package checks
    sector <- fuel <- year <- value <- state_val <- kor_val <-
      state <- . <- GCAM_region_ID <- LEVEL2_DATA_NAMES <-
      curr_table <- value.x <- value.y <- NULL

    # ===================================================
    # Scale national-level building energy consumption
    # by the portion of total US building energy use by fuel
    # for each state and sector.

    # First, subset the SEDS table so that is contains only the fuels that are part of the GCAM buildings for the
    # residential and commercial sectors.
    L101.inKEEI_EJ_state_S_F %>%
      filter(sector %in% c("comm", "resid"), fuel %in% L142.in_EJ_R_bld_F_Yh$fuel) ->
      L142.in_EJ_korea_bld_F_unscaled

    # Aggregate the SEDS table by fuel to find the annual national building fuel use.
    # The national values will be used to calculate each state's portion of fuel allocation.
    L142.in_EJ_korea_bld_F_unscaled %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(year, fuel) %>%
      summarise(value = sum(value)) ->
      L142.in_EJ_korea_bld_F_unscaled_grouped

    # Calculate the portion of total US building energy use by fuel for each state and sector.
    L142.in_EJ_korea_bld_F_unscaled %>%
      rename(state_val = value) %>%
      left_join_error_no_match(L142.in_EJ_korea_bld_F_unscaled_grouped %>% rename(kor_val = value),
                               by = c("fuel", "year")) %>%
      mutate(value = state_val / kor_val) %>%
      select(-state_val, -kor_val) ->
      L142.in_pct_state_bld_F

    # Now aggregate the building sector energy consumption to entire GCAM regions.
    L142.in_EJ_R_bld_F_Yh %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) ->
      L142.in_EJ_R_bldtot_F_Yh

    # Assume the GCAM US region Id is 1 and select the aggregated
    # building sector energy consumption for the US.
    L142.in_EJ_R_bldtot_F_Yh %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum) %>%
      ungroup %>%
      select(-GCAM_region_ID) ->
      L142.in_EJ_R_bldtot_F_Yh_korea

    # Apportion nation-level energy by fuel to states and sectors by
    # scaling by the portion of total US building energy use by fuel
    # for each state and sector from the SEDS table.
    L142.in_pct_state_bld_F %>%
      left_join_error_no_match(L142.in_EJ_R_bldtot_F_Yh_korea, by = c("year", "fuel")) %>%
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value) ->
      L142.in_EJ_korea_bld_F

    # ===================================================

    L142.in_EJ_korea_bld_F %>%
      add_title("Buildings energy consumption by state, sector (res/comm) and fuel") %>%
      add_units("value = EJ") %>%
      add_comments("Scaled national-level building energy consumption by portion of total US building energy use by fuel for each state and sector from the SEDS table.") %>%
      add_legacy_name("L142.in_EJ_korea_bld_F") %>%
      add_precursors("L142.in_EJ_R_bld_F_Yh", "L101.inKEEI_EJ_state_S_F") ->
      L142.in_EJ_korea_bld_F

    return_data(L142.in_EJ_korea_bld_F)
  } else {
    stop("Unknown command")
  }
}
