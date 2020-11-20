#' module_gcam.korea_LA114.Wind
#'
#' Compute capacity factors for wind.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L114.CapacityFactor_wind_korea}. The corresponding file in the
#' original data system was \code{LA114.Wind.R} (gcam-korea level1).
#' @details Computes capacity factors for wind.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author M. Roh
module_gcam.korea_LA114.Wind <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "gcam-korea/korea_state_wind",
              FILE = "energy/A23.globaltech_capital",
              FILE = "energy/A23.globaltech_OMfixed",
              FILE = "energy/A23.globaltech_OMvar"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L114.CapacityFactor_wind_korea"))
  } else if(command == driver.MAKE) {

    technology <- year <- state <- sector <- capacity.factor <- fuel <- value <- base_cost <-
      region <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    korea_state_wind <- get_data(all_data, "gcam-korea/korea_state_wind")
    A23.globaltech_capital <- get_data(all_data, "energy/A23.globaltech_capital")
    A23.globaltech_OMfixed <- get_data(all_data, "energy/A23.globaltech_OMfixed")
    A23.globaltech_OMvar <- get_data(all_data, "energy/A23.globaltech_OMvar")

    # ===================================================

    # This function filters A23 tables for wind, then gathers and interpolates...
    # ... to get a single value for wind base cost year. Note that the interpolation is...
    # ... redundant whilst gcamusa.WIND_BASE_COST_YEAR = 2005, ...
    # ... since 2005 is an existing data point in all A23 tables.
    filter_gather_interp_get_cost <- function(x) {
      . <- NULL  # silence package check notes
      x %>% filter(technology == "wind") %>%
        gather_years %>%
        select(year, value) %>%
        complete(year = c(year, gcamkorea.WIND_BASE_COST_YEAR)) %>%
        mutate(value = approx_fun(year, value, rule = 2)) %>%
        filter(year == gcamkorea.WIND_BASE_COST_YEAR) %>%
        pull(value)
    }

    A23.globaltech_capital %>% filter_gather_interp_get_cost -> L114.CapCost
    A23.globaltech_OMfixed %>% filter_gather_interp_get_cost -> L114.OMFixedCost
    A23.globaltech_OMvar %>% filter_gather_interp_get_cost -> L114.OMVarCost
    # ^^ all above rates are in $1975

    # Get fixed charge rate of capital for wind
    filter(A23.globaltech_capital, technology == "wind")$fixed.charge.rate -> L114.FixedChargeRate

    # Convert state level wind base cost data to 1975$/GJ, ...
    # ... then compute capacity factor for the base wind turbine in each state
    korea_state_wind %>%
      mutate(sector = "electricity generation",
             fuel = "wind",
             base_cost = base_cost * gdp_korea_deflator(1975, 2007) / CONV_KWH_GJ,
             capacity.factor = (L114.CapCost * L114.FixedChargeRate + L114.OMFixedCost) /
               (CONV_KWH_GJ * CONV_YEAR_HOURS) / (base_cost - (L114.OMVarCost / CONV_MWH_GJ))) %>%
      # ^^ capacity factor computed dividing sum of capital and fixed costs (converted to $/GJ) by ...
      # ... base cost minus variable cost (converted to $/GJ)
      rename(state = region) %>%
      select(state, sector, fuel, capacity.factor) %>%
      # add attributes for output...
      add_title("Capacity factor for wind by state") %>%
      add_units("Unitless") %>%
      add_comments("Computed from A23 tables for capital, variable and fixed wind costs") %>%
      add_legacy_name("L114.CapacityFactor_wind_korea") %>%
      add_precursors("gcam-korea/korea_state_wind",
                     "energy/A23.globaltech_capital",
                     "energy/A23.globaltech_OMfixed",
                     "energy/A23.globaltech_OMvar") ->
      L114.CapacityFactor_wind_korea

    return_data(L114.CapacityFactor_wind_korea)
  } else {
    stop("Unknown command")
  }
}
