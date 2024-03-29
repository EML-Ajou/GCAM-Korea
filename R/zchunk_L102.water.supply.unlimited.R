#' module_water_L102.water.supply.unlimited
#'
#' Summarize the water price values by water type and GCAM region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.unlimited_water_price_R_W_Y_75USDm3}. The corresponding file in the
#' original data system was \code{L102.water.supply.unlimited.R} (water level1).
#' @details  We generate all the possible combinations between the 32 GCAM regions, 4 water types, and all the defined years.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MH May 2017
module_water_L102.water.supply.unlimited <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/A_unlimited_water_price"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.unlimited_water_price_R_W_Y_75USDm3"))
  } else if(command == driver.MAKE) {

    region <- NULL                      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_unlimited_water_price <- get_data(all_data, "water/A_unlimited_water_price")

    # all 4 water types
    water_type <- c("water consumption", "water withdrawals", "seawater", "biophysical water consumption")

    # copy for ordering later
    water_type_order <- water_type

    # all historical and future years
    year <- c(HISTORICAL_YEARS, FUTURE_YEARS)

    # generate a long table format with the all the possible combinations between the 32 GCAM regions, 4 water types, and all the defined years
    GCAM_region_names %>%
      repeat_add_columns(tibble(water_type)) %>%
      select(-region) %>%
      repeat_add_columns(tibble(year)) %>%
      mutate(value = if_else(water_type != "water withdrawals",
                             water.DEFAULT_UNLIMITED_WATER_PRICE,
                             water.DEFAULT_UNLIMITED_WITHD_WATER_PRICE),
             year = as.integer(year)) ->
      L102.all_region_water_type

    # ===================================================

    # Produce outputs

    L102.all_region_water_type %>%
      add_title("Water price data for different water types") %>%
      add_units("$/m3") %>%
      add_comments("Nominal default water prices") %>%
      add_legacy_name("L102.unlimited_water_price_R_W_Y_75USDm3") %>%
      add_precursors("common/GCAM_region_names", "water/A_unlimited_water_price") ->
      L102.unlimited_water_price_R_W_Y_75USDm3

    return_data(L102.unlimited_water_price_R_W_Y_75USDm3)
  } else {
    stop("Unknown command")
  }
}
