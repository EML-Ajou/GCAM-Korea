#' module_gcam.korea_LA161.Cstorage
#'
#' Calculates onshore CO2 storage by grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.Cstorage_korea}. The corresponding file in the
#' original data system was \code{LA161.Cstorage.R} (gcam-korea level1).
#' @details Calculates onshore CO2 storage by subregion using GCAM's Cstorage curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author M. Roh

module_gcam.korea_LA161.Cstorage <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             FILE = "energy/A61.Cstorage_curves"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.Cstorage_korea"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    state <- grid_region <- Cost_2005USDtCO2 <- CO2_Mt <- MtC <- Cumul_MtC <- quantile <-
      grade <- Cost_1990USDtC <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions") %>%
      select(state, grid_region)
    A61.Cstorage_curves <- get_data(all_data, "energy/A61.Cstorage_curves")

    # ===================================================


    # NOTE: there may be grid regions (e.g. AK, HI) that do not have CO2 storage points. These are not assigned onshore CO2 storage curves
    grid_region <- sort(unique(states_subregions$grid_region))

    L161.Csupply_state <- A61.Cstorage_curves %>%
      # Convert prices to 1990 USD per ton C and amounts to C
      # Carbon storage capacity is not available in Korea. In other worlds, MtC is zero
      # Capture cost uses gcam defualt data
      mutate(Cost_1990USDtC = cost_2005USDtCO2 * emissions.CONV_C_CO2 / gdp_deflator(2005, 1990),
            MtC = 0) %>%
      select(MtC, Cost_1990USDtC) %>%
      # Add grid_region
      repeat_add_columns(tibble(grid_region))

    # The construction of supply curves takes place within each grid region.
    # In each region, there are four cost points assigned, for each quartile of the supply curve
    region_quartiles <- L161.Csupply_state %>%
      group_by(grid_region) %>%
      # Calculate cumulative sum, then filter to the quantiles
      mutate(Cumul_MtC = cumsum(MtC)) %>%
      # use "type=1" to indicate that we want to return only exact matches to the data in the table
      # only take the 2nd to 5th points in the quartiles (25%, 50%, 75%, 100%)
      filter(Cost_1990USDtC %in% quantile(Cost_1990USDtC, type = 1)[2:5]) %>%
      # From the cumulative totals, return to the marginal quantities associated with each grade
      mutate(MtC = Cumul_MtC - lag(Cumul_MtC),
             # this will create grade equal to grade 1 for 25%, grade 2 for 50%, etc.
             grade = paste("grade",seq_len(n()), sep = " ")) %>%
      ungroup() %>%
      # For the first grade, set the marginal quantity with the cumulative total
      mutate(MtC = replace(MtC, is.na(MtC), Cumul_MtC[is.na(MtC)]))

    L161.Cstorage_korea <- region_quartiles %>%
      select(grid_region, grade, Cost_1990USDtC, MtC) %>%
      # Setting a minimum cost of 0 on CO2 storage and transport projects
      mutate(Cost_1990USDtC = pmax(Cost_1990USDtC, 0)) %>%
    # ===================================================
    # Produce outputs
      add_title("CO2 storage curves by regions and grades") %>%
      add_units("MtC; 1990 USD per tC") %>%
      add_comments("Cumulative MtC calculated by grid region, then filtered to quartiles") %>%
      add_comments("Final MtC value is marginal increase") %>%
      add_legacy_name("L161.Cstorage_korea") %>%
      add_precursors("gcam-korea/states_subregions", "energy/A61.Cstorage_curves") ->
      L161.Cstorage_korea

    return_data(L161.Cstorage_korea)
  } else {
    stop("Unknown command")
  }
}
