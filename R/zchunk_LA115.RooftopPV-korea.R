#' module_gcam.korea_LA115.RooftopPV
#'
#' Prepare resource curves for rooftop PV (commercial and residential combined).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L115.rsrc_korea_rooftopPV}. The corresponding file in the
#' original data system was \code{LA115.RooftopPV.R} (gcam-korea level1).
#' @details Use the GCAM default resource curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author M. Roh
module_gcam.korea_LA115.RooftopPV <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             FILE = "energy/A15.roofPV_curves"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L115.rsrc_korea_rooftopPV"))
  } else if(command == driver.MAKE) {

    Relative_Cost <- Rel_Cost <- MWh <- State <- state_name <- p <- generation <-
      GWH <- state <- cumul <- percent_cumul <- minimum <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions")
    A15.roofPV_curves <- get_data(all_data, "energy/A15.roofPV_curves")

    # ===================================================

    A15.roofPV_curves %>%
      filter(region_GCAM3 == "Korea") %>%
      select(maxSubResource, `mid-price`, `curve-exponent`) -> gcamdata_rsrc_rooftopPV

    count <- length(gcamkorea.STATES)

    gcamdata_rsrc_rooftopPV %>%
      repeat_add_columns(tibble(gcamkorea.STATES)) %>%
      mutate(generation=maxSubResource/rep(count, count)) %>%
      rename(state=gcamkorea.STATES, mid_p= `mid-price`, b_exp=`curve-exponent`) %>%
      select(state, generation, mid_p, b_exp) -> L115.rsrc_korea_rooftopPV

    L115.rsrc_korea_rooftopPV %>%
      ## output attributes...
      add_title("Resource curves for rooftop PV") %>%
      add_units("1975$/GJ (mid-price) and EJ/yr (maxsubresource)") %>%
      add_comments("Fitted supply curve parameters are sensitive to ordering generation values with equal price") %>%
      add_legacy_name("L115.rsrc_korea_rooftopPV") %>%
      add_precursors("gcam-korea/states_subregions",
                     "energy/A15.roofPV_curves") ->
      L115.rsrc_korea_rooftopPV

    return_data(L115.rsrc_korea_rooftopPV)
  } else {
    stop("Unknown command")
  }
}
