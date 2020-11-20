#' module_gcam.korea_LA101.EIA_KoreaRegionalEnergy
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.KEEI_use_all_KTOE}, \code{L101.inKEEI_EJ_state_S_F}.
#' The corresponding file was KEEI energy balance. (gcam-korea level0).
#' @details See above
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread fill
#' @author S. Jeon
module_gcam.korea_LA101.EIA_KoreaRegionalEnergy <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/KEEI_to_GCAM_fuel",
             FILE = "gcam-korea/KEEI_to_GCAM_sector",
             FILE = "gcam-korea/KEEI_use_all_KTOE",
             FILE = "gcam-korea/A_fuel_conv"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.KEEI_use_all_KTOE",
             "L101.inKEEI_EJ_state_S_F"))
  } else if(command == driver.MAKE) {

    year <- value <- Data_Status <- State <- MSN <- GCAM_fuel <- GCAM_sector <-
      state <- sector <- fuel <- KEEI_fuel <- KEEI_sector <-
      description.x <- description.y <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    KEEI_fuels <- get_data(all_data, "gcam-korea/KEEI_to_GCAM_fuel")
    KEEI_sectors <- get_data(all_data, "gcam-korea/KEEI_to_GCAM_sector")
    KEEI_use_all_Bbtu <- get_data(all_data, "gcam-korea/KEEI_use_all_KTOE")
    A_fuel_conv <- get_data(all_data, "gcam-korea/A_fuel_conv")

    # ===================================================

    # Prep for output tables - add columns for GCAM sector and fuel names, using the substrings of the Mnemonic Series Name (MSN) code, and filter out U.S.
    KEEI_use_all_Bbtu %>%
      gather_years %>%
      mutate(KEEI_fuel = substr(MSN, 1, 2),  # First and second digits of MSN is energy code
             KEEI_sector = substr(MSN, 3, 4)) %>% # Third and fourth digits of MSN is sector code
      left_join(KEEI_fuels, by = "KEEI_fuel") %>%
      left_join(KEEI_sectors, by = "KEEI_sector") %>%
      mutate(state = State, fuel = GCAM_fuel, sector = GCAM_sector) ->
      KTOE_with_GCAM_names

    # Create 1 of the 2 output tables: narrow years from 1971-2010, convert kilo TOE to EJ (fuel specific), remove rows that have no defined sector or fuel name
    KTOE_with_GCAM_names %>%
      select(state, sector, fuel, year, value) %>%
      filter(year %in% HISTORICAL_YEARS, !is.na(fuel), !is.na(sector)) %>%
      left_join(A_fuel_conv, by = "fuel") %>%
      mutate(value = value * CONV_KTOE_EJ) %>%
      group_by(state, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      arrange(fuel, sector) %>%
      ungroup() ->
      L101.inKEEI_EJ_state_S_F

    # Create other output table: leave units as billion BTU, getting rid of missing values: prior to 1980, lots are missing. These data are only used for state-wise allocations
    KTOE_with_GCAM_names %>%
      select(Data_Status, state, MSN, year, value, KEEI_fuel, KEEI_sector, sector, fuel, -State, -description.x, -description.y) %>%
      arrange(Data_Status, state, MSN, KEEI_fuel, KEEI_sector, sector, fuel, -year) -> # Year needs to be in descending order to use fill function
      KTOE_with_GCAM_names_intermediate

    # To create this second output table, I need to split the dataframe and recombine
    KTOE_with_GCAM_names_intermediate %>%
      filter(year %in% 1971:2011) %>% # Custom year range, want to keep NAs in 1960-1970
      fill(value) %>% # Replace NAs in 1971-1979 with values from one year more recent
      bind_rows(filter(KTOE_with_GCAM_names_intermediate, year %in% 1960:1970)) %>% # Reattaching 1960-1970 rows
      arrange(Data_Status, state, MSN, KEEI_fuel, KEEI_sector, sector, fuel, -year) ->
      L101.KEEI_use_all_KTOE

    # ===================================================

    L101.KEEI_use_all_KTOE %>%
      add_title("State Energy Data in KTOE by Year, GCAM-Sector, and GCAM-Fuel") %>%
      add_units("Kilo TOE") %>%
      add_comments("GCAM sector and fuel names were added, NAs for years 1971-1979 were replaced with most recent year's data available") %>%
      add_legacy_name("L101.KEEI_use_all_KTOE") %>%
      add_precursors("gcam-korea/KEEI_use_all_KTOE", "gcam-korea/KEEI_to_GCAM_fuel",
                     "gcam-korea/KEEI_to_GCAM_sector") ->
      L101.KEEI_use_all_KTOE

    L101.inKEEI_EJ_state_S_F %>%
      add_title("State Energy Data in EJ by Year, GCAM-Sector, and GCAM-Fuel") %>%
      add_units("EJ") %>%
      add_comments("GCAM sector and fuel names were added, units converted to EJ, data with no GCAM fuel or sector name removed") %>%
      add_legacy_name("L101.inKEEI_EJ_state_S_F") %>%
      add_precursors("gcam-korea/KEEI_use_all_KTOE", "gcam-korea/KEEI_to_GCAM_fuel",
                     "gcam-korea/KEEI_to_GCAM_sector", "gcam-korea/A_fuel_conv") ->
      L101.inKEEI_EJ_state_S_F

    return_data(L101.KEEI_use_all_KTOE, L101.inKEEI_EJ_state_S_F)
  }
  else {
    stop("Unknown command")
  }
}
