#' module_gcam.korea_LA122.Refining
#'
#' Downscales crude oil, corn ethanol, and biodiesel refining inputs and outputs to state-level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.in_EJ_korea_refining_F}, \code{L122.out_EJ_korea_refining_F}. The corresponding file in the
#' original data system was \code{LA122.Refining.R} (gcam-korea level1).
#' @details Downscales crude oil, corn ethanol, and biodiesel refining inputs and outputs to state-level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author S. Jeon
module_gcam.korea_LA122.Refining <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L122.in_EJ_R_refining_F_Yh",
             "L122.out_EJ_R_refining_F_Yh",
             "L101.inKEEI_EJ_state_S_F",
             FILE = "gcam-korea/kemco_biodiesel_kL.yr"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L122.in_EJ_korea_refining_F",
             "L122.out_EJ_korea_refining_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    GCAM_region_ID <- sector <- fuel <- year <- value <- value.x <- value.y <- state <-
      fuel.x <- Mgal.yr <- pct <- NULL

    # Load required inputs
    L122.in_EJ_R_refining_F_Yh <- get_data(all_data, "L122.in_EJ_R_refining_F_Yh") %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum)
    L122.out_EJ_R_refining_F_Yh <- get_data(all_data, "L122.out_EJ_R_refining_F_Yh") %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum)
    L101.inKEEI_EJ_state_S_F <- get_data(all_data, "L101.inKEEI_EJ_state_S_F")
    kemco_biodiesel_kL.yr <- get_data(all_data, "gcam-korea/kemco_biodiesel_kL.yr")

    # ===================================================
    # CRUDE OIL REFINING
    # NOTE: using SEDS crude oil input to industry as basis for allocation of crude oil refining to states
    # Crude oil consumption by industry is the energy used at refineries (input - output)

    # Calculate the percentages of oil consumption in each state
    L122.pct_state_cor <- L101.inKEEI_EJ_state_S_F %>%
      filter(sector == "industry",
             fuel == "crude oil") %>%
      mutate(sector = "oil refining") %>%
      group_by(year) %>%
      # State percentage of total in each year
      mutate(value = value / sum(value)) %>%
      ungroup()

    # Crude oil refining output by state
    # Apportion the national total to the states
    L122.out_EJ_state_cor <- L122.pct_state_cor %>%
      left_join_error_no_match(L122.out_EJ_R_refining_F_Yh, by = c("sector", "year")) %>%
      # State output value = state proportion * national output value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel = fuel.x, year, value)

    # Inputs to crude oil refining - same method of portional allocations, but with multiple fuels
    # Oil refining input fuels
    oil_input_fuels <- L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "oil refining") %>%
      select(fuel) %>%
      distinct()

    # Repeat state proportions for all fuels in oil refining sector
    L122.pct_state_cor_repF <- L122.pct_state_cor %>%
      select(-fuel) %>%
      repeat_add_columns(oil_input_fuels)

    # Calculate state oil input values
    L122.in_EJ_state_cor_F <- L122.pct_state_cor_repF %>%
      left_join_error_no_match(L122.in_EJ_R_refining_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State input value = state proportion * national input value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value)

    # BIOMASS LIQUIDS
    # NOTE: using SEDS biofuel transformation-related losses to disaggregate ethanol production to states

    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # The sectors, corn ethanol and 'biodiesel, use GCAM default value.
    # Even, Korea has no corn ethanol data in GCAM. So Korea ignores 'corn ethanol'.
    # Biodiesel is divided by the supply capacity of each state.

    # Build table of percentages by historical year
    kemco_biodiesel_kL.yr <- kemco_biodiesel_kL.yr %>%
      transmute(pct = `kl/yr` / sum(`kl/yr`), state)

    # Joining kemco_biodiesel_kL.yr to all states and years
    L122.pct_state_btlbd <- tidyr::crossing(state = gcamkorea.STATES,
                                            year = HISTORICAL_YEARS) %>%
      mutate(sector = "biodiesel",
             fuel = "biomass oil") %>%
      # Using left_join because not all states in kemco_biodiesel_kL.yr
      left_join(kemco_biodiesel_kL.yr, by = "state") %>%
      replace_na(list(pct = 0))

    # Apportion to the states
    L122.out_EJ_R_refining_F_Yh_bds <- L122.out_EJ_R_refining_F_Yh %>%
      filter(sector == "biodiesel",
             year %in% HISTORICAL_YEARS)

    L122.out_EJ_state_btlbd <- L122.pct_state_btlbd %>%
      left_join_error_no_match(L122.out_EJ_R_refining_F_Yh_bds, by = c("sector", "year")) %>%
      # State output value = state proportion * national output value
      mutate(value = pct * value) %>%
      select(state, sector, fuel = fuel.x, year, value)

    # Biodiesel inputs by state and fuel
    # Biodiesel input fuels
    biodiesel_input_fuels <- L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "biodiesel") %>%
      select(fuel) %>%
      distinct()

    # Repeat state proportions for all fuels used in biodiesel sector
    L122.pct_state_btlbd_repF <- L122.pct_state_btlbd %>%
      select(-fuel) %>%
      repeat_add_columns(biodiesel_input_fuels)

    # Biodiesel inputs by state
    L122.in_EJ_state_btlbd_F <- L122.pct_state_btlbd_repF %>%
      left_join_error_no_match(L122.in_EJ_R_refining_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State input value = state proportion * national input value
      mutate(value = pct * value) %>%
      select(state, sector, fuel, year, value)

    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

    # Bind the tables of inputs and outputs of all refineries by state in the base years
    L122.in_EJ_korea_refining_F <- bind_rows(L122.in_EJ_state_cor_F, L122.in_EJ_state_btlbd_F)
    L122.out_EJ_korea_refining_F <- bind_rows( L122.out_EJ_state_cor, L122.out_EJ_state_btlbd)

    # ===================================================

    # Produce outputs
    L122.in_EJ_korea_refining_F %>%
      add_title("Refinery energy inputs by state, sector, and fuel") %>%
      add_units("EJ") %>%
      add_comments("Crude oil and biodiesel input values apportioned to states") %>%
      add_legacy_name("L122.in_EJ_korea_refining_F") %>%
      add_precursors("L101.inKEEI_EJ_state_S_F",
                     "L122.in_EJ_R_refining_F_Yh",
                     "gcam-korea/kemco_biodiesel_kL.yr") ->
      L122.in_EJ_korea_refining_F

    L122.out_EJ_korea_refining_F %>%
      add_title("Refinery output by state and sector") %>%
      add_units("EJ") %>%
      add_comments("Crude oil and biodiesel output values apportioned to states") %>%
      add_legacy_name("L122.out_EJ_korea_refining_F") %>%
      add_precursors("L101.inKEEI_EJ_state_S_F",
                     "L122.out_EJ_R_refining_F_Yh",
                     "gcam-korea/kemco_biodiesel_kL.yr") ->
      L122.out_EJ_korea_refining_F

    return_data(L122.in_EJ_korea_refining_F, L122.out_EJ_korea_refining_F)
  } else {
    stop("Unknown command")
  }
}
