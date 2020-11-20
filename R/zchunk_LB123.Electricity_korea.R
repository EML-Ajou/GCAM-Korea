#' module_gcam.korea_LB123.Electricity
#'
#' Calculate electricity fuel consumption, electricity generation, and inputs and outputs of net ownuse
#' (the electricity used by production/transformation facilities) by state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.in_EJ_korea_elec_F}, \code{L123.out_EJ_korea_elec_F}, \code{L123.in_EJ_korea_ownuse_elec}, \code{L123.out_EJ_korea_ownuse_elec}.
#' The corresponding file in the original data system was \code{LB123.Electricity.R} (gcam-korea level1).
#' @details By state, calculates electricity fuel consumption, electricity generation, and inputs and outputs of net ownuse.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author S. Jeon
module_gcam.korea_LB123.Electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             FILE ="gcam-korea/auxiliary_use_value",
             "L126.in_EJ_R_elecownuse_F_Yh",
             "L126.out_EJ_R_elecownuse_F_Yh",
             "L101.inKEEI_EJ_state_S_F",
             "L132.out_EJ_korea_indchp_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.in_EJ_korea_elec_F",
             "L123.out_EJ_korea_elec_F",
             "L123.in_EJ_korea_ownuse_elec",
             "L123.out_EJ_korea_ownuse_elec"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    State <- state <- state_name <- GCAM_region_ID <- year <- value <- sector <-
      fuel <- CSP_GWh <- value.x <- value.y <- net_EJ_korea <- DirectUse_MWh <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions")
    auxiliary_use_value <- get_data(all_data, "gcam-korea/auxiliary_use_value")

    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh") %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum)
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh") %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum)
    L126.in_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.in_EJ_R_elecownuse_F_Yh") %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum)
    L126.out_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.out_EJ_R_elecownuse_F_Yh") %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum)
    L101.inKEEI_EJ_state_S_F <- get_data(all_data, "L101.inKEEI_EJ_state_S_F")
    L132.out_EJ_korea_indchp_F <- get_data(all_data, "L132.out_EJ_korea_indchp_F")

    # ===================================================
    L123.pct_state_elec_F <- L101.inKEEI_EJ_state_S_F %>%
      filter(sector %in% c("electricity_input", "electricity_output")) %>%
      # Compute each state's percentage, by fuel
      group_by(sector, fuel, year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      # This is just PV solar, we will add in CSP next
      mutate(fuel = replace(fuel, fuel == "solar", "solar PV")) %>%
      replace_na(list(value = 0))


    #### Written by Jeon(b) #### Start
    # Add CSP: There is no CSP consumption in Korea case. But to make format appropriate, we need to add CSP table.
    # The reason why it is filtered with geothermal is just in that geothermal is also not used.; value: 0
    L123.pct_state_elec_F %>% filter(fuel == "geothermal") %>% mutate(fuel = "solar CSP") -> solar_CSP
    L123.pct_state_elec_F %>% bind_rows( solar_CSP ) -> L123.pct_state_elec_F


    # Electricity generation inputs by fuel and state
    # Allocating total energy input values to states using shares
    L123.in_EJ_korea_elec_F <- L123.pct_state_elec_F %>%
      # L123.in_EJ_R_elec_F_Yh only has certain fuels
      filter(fuel %in% unique(L123.in_EJ_R_elec_F_Yh$fuel)) %>%
      left_join_error_no_match(L123.in_EJ_R_elec_F_Yh %>%
                                 select(fuel, year, value),
                               by = c("fuel", "year")) %>%
      # Multiplying state share by total value
      mutate(value = value.x * value.y,
             sector = "electricity generation") %>%
      select(-value.x, -value.y)

    # Electricity generation outputs by fuel and state
    # Allocating total electricity generation values to states using shares
    L123.out_EJ_korea_elec_F <- L123.pct_state_elec_F %>%
      left_join_error_no_match(L123.out_EJ_R_elec_F_Yh %>%
                                 select(fuel, year, value),
                               by = c("fuel", "year")) %>%
      # Multiplying state share by total value
      mutate(value = value.x * value.y,
             sector = "electricity generation") %>%
      select(-value.x, -value.y)

    # ELECTRICITY - OWNUSE
    # NOTE: Electricity net own use energy is apportioned to states on the basis of KEEI's direct use by state
    # First calculate the national own use quantity
    L123.net_EJ_korea_ownuse <- L126.in_EJ_R_elecownuse_F_Yh %>%
      left_join_error_no_match(L126.out_EJ_R_elecownuse_F_Yh, by = c("sector", "fuel", "year")) %>%
      # Net value = input value - output value
      mutate(net_EJ_korea = value.x - value.y) %>%
      select(sector, year, net_EJ_korea)

    # Then build table with each state's share of the national ownuse. Note that this is assumed invariant over time.
    L123.net_pct_korea_ownuse_elec <- tidyr::crossing(state = gcamkorea.STATES,
                                                 sector = "electricity ownuse",
                                                 fuel = "electricity",
                                                 year = HISTORICAL_YEARS) %>%
      left_join_error_no_match(auxiliary_use_value, by = c("state", "year")) %>%
      group_by(sector, fuel, year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup()

    # Net own use = national total multiplied by each state's share
    L123.net_EJ_state_ownuse_elec <- L123.net_pct_korea_ownuse_elec %>%
      left_join_error_no_match(L123.net_EJ_korea_ownuse, by = c("sector", "year")) %>%
      # Multiply state share by USA total
      mutate(value = value * net_EJ_korea)

    # The input of the electricity_net_ownuse sector is equal to sum of all generation (industrial CHP + electric sector)
    L123.in_EJ_korea_ownuse_elec <- bind_rows(L123.out_EJ_korea_elec_F, L132.out_EJ_korea_indchp_F) %>%
      group_by(state, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(sector = "electricity ownuse",
             fuel = "electricity") %>%
      select(state, sector, fuel, year, value)

    # Output of electricity_net_ownuse sector is equal to input minus ownuse "net" energy
    L123.out_EJ_korea_ownuse_elec <- L123.in_EJ_korea_ownuse_elec %>%
      left_join_error_no_match(L123.net_EJ_state_ownuse_elec, by = c("state", "sector", "fuel", "year")) %>%
      # Input value - net value
      mutate(value = value.x - value.y) %>%
      select(state, sector, fuel, year, value)
    # ===================================================

    # Produce outputs
    L123.in_EJ_korea_elec_F %>%
      add_title("Electricity sector energy consumption by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("State fuel shares created from L101.inKEEI_EJ_state_S_F multiplied by USA totals from L123.in_EJ_R_elec_F_Yh") %>%
      add_legacy_name("L123.in_EJ_korea_elec_F") %>%
      add_precursors("L101.inKEEI_EJ_state_S_F",
                     #"gcam-korea/korea_re_technical_potential",
                     "gcam-korea/states_subregions", "L123.in_EJ_R_elec_F_Yh") ->
      L123.in_EJ_korea_elec_F

    L123.out_EJ_korea_elec_F %>%
      add_title("Electricity generation by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("State fuel shares created from L101.inKEEI_EJ_state_S_F multiplied by USA totals from L123.out_EJ_R_elec_F_Yh") %>%
      add_legacy_name("L123.out_EJ_korea_elec_F") %>%
      add_precursors("L101.inKEEI_EJ_state_S_F",
                     #"gcam-korea/korea_re_technical_potential",
                     "gcam-korea/states_subregions", "L123.out_EJ_R_elec_F_Yh") ->
      L123.out_EJ_korea_elec_F

    L123.in_EJ_korea_ownuse_elec %>%
      add_title("Input to electricity net ownuse by state") %>%
      add_units("EJ") %>%
      add_comments("Sum of all generation from L123.out_EJ_korea_elec_F and L132.out_EJ_korea_indchp_F") %>%
      add_legacy_name("L123.in_EJ_korea_ownuse_elec") %>%
      add_precursors("L101.inKEEI_EJ_state_S_F",
                     #"gcam-korea/korea_re_technical_potential",
                     "gcam-korea/states_subregions", "L123.out_EJ_R_elec_F_Yh", "L132.out_EJ_korea_indchp_F") ->
      L123.in_EJ_korea_ownuse_elec

    L123.out_EJ_korea_ownuse_elec %>%
      add_title("Output of electricity net ownuse by state") %>%
      add_units("EJ") %>%
      add_comments("Input values from L123.in_EJ_korea_ownuse_elec subtracted by net values") %>%
      add_comments("Net values created with states shares from korea_elect_td_ownuse_prices and USA total net from L126 files") %>%
      add_legacy_name("L123.out_EJ_korea_ownuse_elec") %>%
      add_precursors("L101.inKEEI_EJ_state_S_F",
                     #"gcam-korea/korea_re_technical_potential",
                     "gcam-korea/states_subregions", "L123.out_EJ_R_elec_F_Yh", "L132.out_EJ_korea_indchp_F",
                     "L126.in_EJ_R_elecownuse_F_Yh", "L126.out_EJ_R_elecownuse_F_Yh", "gcam-korea/auxiliary_use_value")  ->
      L123.out_EJ_korea_ownuse_elec

    return_data(L123.in_EJ_korea_elec_F, L123.out_EJ_korea_elec_F, L123.in_EJ_korea_ownuse_elec, L123.out_EJ_korea_ownuse_elec)
  } else {
    stop("Unknown command")
  }
}
