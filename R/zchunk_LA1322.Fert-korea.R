#' module_gcam.korea_LA1322.Fert
#'
#' Calculate input-output intensity coefficients and input energy for state fertilizer production
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1322.out_Mt_korea_Fert_Yh}, \code{L1322.IO_GJkg_korea_Fert_F_Yh}, \code{L1322.in_EJ_korea_Fert_Yh}. The corresponding file in the
#' original data system was \code{LA1322.Fert.R} (gcam-korea level1).
#' @details Calculate input-output intensity coefficients and input energy for state fertilizer production from state shares of national values.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author M. Roh
module_gcam.korea_LA1322.Fert <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/Mining_Manufacturing_Survey",
             "L1322.Fert_Prod_MtN_R_F_Y",
             "L1322.IO_R_Fert_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1322.out_Mt_korea_Fert_Yh",
             "L1322.IO_GJkg_korea_Fert_F_Yh",
             "L1322.in_EJ_korea_Fert_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    Mining_Manufacturing_Survey <- get_data(all_data, "gcam-korea/Mining_Manufacturing_Survey")
    L1322.Fert_Prod_MtN_R_F_Y <- get_data(all_data, "L1322.Fert_Prod_MtN_R_F_Y")
    L1322.IO_R_Fert_F_Yh <- get_data(all_data, "L1322.IO_R_Fert_F_Yh")

    # Silence package check
    IO_coeff <- code <- VoS <- year <- GCAM_region_ID <-
      value <- national_val <- state_share <- state <- sector <- fuel <- NULL

    # ===================================================
    # Get state fertilizer production, input-output intensity coefficients, and
    # input energy for fertilizer production from national values.

    # c20 is Manufacture of chemicals and chemical products except pharmaceuticals and medicinal chemicals.
    # Fertilzer is the subset of c20.
    L1322.VoS_share_state_Fert <- filter(Mining_Manufacturing_Survey, code == "c20")

    # Determine each state's share of total value of shipments of NAICS 3273 and assign the shares to all
    # historical years.
    L1322.VoS_share_state_Fert %>%
      mutate(state_share = VoS / sum(VoS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) ->
      L1322.VoS_share_state_Fert

    # Add GCAM fertilizer name and fuel type to the data frame assume
    # that the only relevant fuel in the US is gas.
    L1322.VoS_share_state_Fert %>%
      mutate(sector = aglu.FERT_NAME, fuel = "gas") ->
      L1322.VoS_share_state_Fert

    # Select generic fertilizer production data, these
    # values will be scaled by the state NAICS 3273 share to calculate
    # state generic fertilizer production.
    L1322.Fert_Prod_MtN_R_F_Y %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum) %>%
      rename(national_val = value) ->
      L1322.Fert_Prod_MtN_R_F_Y_korea

    # Scale national generic fertilizer production by the state share of
    # national NAICS 3273 values.
    L1322.VoS_share_state_Fert %>%
      left_join(L1322.Fert_Prod_MtN_R_F_Y_korea, by = c("year", "sector", "fuel")) %>%
      mutate(value = national_val * state_share) %>%
      select(state, sector, fuel, year, value) ->
      L1322.out_Mt_korea_Fert_Yh

    # Save the names of the sates from the generic fertilizer production by the state
    # data frame to add to the generic fertilizer production energy use.
    L1322.out_Mt_korea_Fert_Yh %>%
      select(state) %>%
      distinct ->
      state_list

    # Assume that all states have the same input-output intensity coefficients
    # (IO coeff).

    # Select the US national energy use for generic fertilizer production and
    # assign to the value to the states in the state list.
    L1322.IO_R_Fert_F_Yh %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum, fuel == "gas") %>%
      repeat_add_columns(state_list) %>%
      select(state, sector, fuel, year, value) ->
      L1322.IO_GJkg_korea_Fert_F_Yh

    # Multiply state generic fertilizer production by the IO coefficient to calculate
    # state IO for fertilizer production.
    L1322.IO_GJkg_korea_Fert_F_Yh %>%
      rename(IO_coeff = value) %>%
      left_join_error_no_match(L1322.out_Mt_korea_Fert_Yh, by = c("state", "sector", "fuel", "year")) %>%
      mutate(value = IO_coeff * value) %>%
      select(state, sector, fuel, year, value) ->
      L1322.in_EJ_korea_Fert_Yh

    # ===================================================

    # Produce outputs
    L1322.out_Mt_korea_Fert_Yh %>%
      add_title("State fertilizer production") %>%
      add_units("value = Mt (megatonnes = teragrams)") %>%
      add_comments("Scaled the US national fertilizer production by the state's share of fertilizer production based on NAICS shipping information") %>%
      add_legacy_name("L1322.out_Mt_korea_Fert_Yh") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y", "gcam-korea/Mining_Manufacturing_Survey", "L1322.IO_R_Fert_F_Yh") ->
      L1322.out_Mt_korea_Fert_Yh

    L1322.IO_GJkg_korea_Fert_F_Yh %>%
      add_title("State fertilizer input-output coefficient") %>%
      add_units("value = GJkg (gigajoules used/kg fertilizer produced)") %>%
      add_comments("All states are assumed to have the same fertilizer IO coefficient as the national value") %>%
      add_legacy_name("L1322.IO_GJkg_korea_Fert_F_Yh") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y", "gcam-korea/Mining_Manufacturing_Survey", "L1322.IO_R_Fert_F_Yh") ->
      L1322.IO_GJkg_korea_Fert_F_Yh

    L1322.in_EJ_korea_Fert_Yh %>%
      add_title("State fertilizer production input energy use") %>%
      add_units("value = EJ (exajoules)") %>%
      add_comments("State fertilizer production multiplied by state fertilizer input-output coefficient") %>%
      add_legacy_name("L1322.in_EJ_korea_Fert_Yh") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y", "gcam-korea/Mining_Manufacturing_Survey", "L1322.IO_R_Fert_F_Yh") ->
      L1322.in_EJ_korea_Fert_Yh

    return_data(L1322.out_Mt_korea_Fert_Yh, L1322.IO_GJkg_korea_Fert_F_Yh, L1322.in_EJ_korea_Fert_Yh)
  } else {
    stop("Unknown command")
  }
}
