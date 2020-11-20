#' module_gcam.korea_LA144.Residential
#'
#' Calculate residential floorspace by state and residential energy consumption by state/fuel/end use.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_korea_resid}, \code{L144.in_EJ_korea_resid}. The corresponding file in the
#' original data system was \code{LA144.Residential.R} (gcam-korea level1).
#' @details Calculate residential floorspace by state and residential energy consumption by state/fuel/end use.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author M. Roh
module_gcam.korea_LA144.Residential <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             FILE = "gcam-korea/Census_pop_hist",
             FILE = "gcam-korea/A44.flsp_bm2_korea_res",
             "L142.in_EJ_korea_bld_F",
             "L144.in_EJ_R_bld_serv_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_korea_resid",
             "L144.in_EJ_korea_resid"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- value <- subregion9 <- DIVISION2009 <- DIVISION <- subregion13 <- LRGSTATE <- subregion13 <- REPORTABLE_DOMAIN <-
      state <- subregion9 <- year <- variable <- HOUSEHOLDS <- NWEIGHT <- . <- value.x <- value.y <- variable <- pcflsp_m2 <-
      pcflsp_m2.x <- pcflsp_m2.y <- conv_9_13 <- sector <- fuel <- service <- DIVISION <- val_1993 <- conv <- val_1990 <-
      Fuel <- Service <- tv_1995 <- fuel_sum <- share <- service.x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions")
    Census_pop_hist <- get_data(all_data, "gcam-korea/Census_pop_hist") %>% gather_years
    A44.flsp_bm2_korea_res <- get_data(all_data, "gcam-korea/A44.flsp_bm2_korea_res")

    L142.in_EJ_korea_bld_F <- get_data(all_data, "L142.in_EJ_korea_bld_F")
    #L143.share_korea_Pop_CDD <- get_data(all_data, "L143.share_korea_Pop_CDD")
    #L143.share_korea_Pop_HDD <- get_data(all_data, "L143.share_korea_Pop_HDD")

    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh") %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum)

    # ===================================================

    # Aggregate population to the subregion9 and subregion13 levels for calculation of per-capita values
    L144.Census_pop_hist <- Census_pop_hist %>%
      left_join_error_no_match(states_subregions, by = "state") %>%
      select(state, year, value, state_name) %>%
      filter(year %in% HISTORICAL_YEARS)

    L144.pop_korR <- L144.Census_pop_hist %>%
      group_by(state, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()


    L144.pcflsp_bm2_kor_resid <- A44.flsp_bm2_korea_res %>%
      left_join_error_no_match(L144.pop_korR, by = c("year", "state")) %>%
      mutate(pcflsp_bm2 = flsp_bm2 / value) %>%
      select(state, year, pcflsp_bm2)


    # Expand to states: multiply per-capita floorspace in each subregion9 times the population of each state
    L144.flsp_bm2_korea_resid <- L144.Census_pop_hist %>%
      left_join_error_no_match(L144.pcflsp_bm2_kor_resid, by = c("state", "year")) %>%
      transmute(state,
                sector = "resid",
                year,
                # Floorspace = population * per-capita floorspace
                value = value * pcflsp_bm2)


    L144.in_EJ_R_bld_serv_F_Yh<- L144.in_EJ_R_bld_serv_F_Yh %>%
      filter(sector=="bld_resid")

    bld.services <- unique(L144.in_EJ_R_bld_serv_F_Yh$service)

    L144.in_EJ_R_bld_serv_F_Yh.share <- L144.in_EJ_R_bld_serv_F_Yh %>%
      group_by(year, fuel) %>%
      mutate(total=sum(value), share=value/total) %>%
      ungroup() %>%
      repeat_add_columns(tibble(state = gcamkorea.STATES)) %>%
      select(state, fuel, service, year, share)

    L144.in_EJ_korea_resid <- L142.in_EJ_korea_bld_F %>%
      filter(sector == "resid") %>%
      repeat_add_columns(tibble(service=bld.services)) %>%
      left_join(L144.in_EJ_R_bld_serv_F_Yh.share, by=c(state, fuel, service, year)) %>%
      replace_na(list(share = 0)) %>%
      transmute(state,
                sector = "resid",
                fuel,
                service,
                year,
                value = value*share)

    # ===================================================

    # Produce outputs
    L144.flsp_bm2_korea_resid %>%
      add_title("Residential floorspace by state") %>%
      add_units("billion m2") %>%
      add_comments("Floorspace by state calculated by multiplying state population by per-capita census division floorspace") %>%
      add_legacy_name("L144.flsp_bm2_korea_resid") %>%
      add_precursors("gcam-korea/states_subregions",
                     "gcam-korea/Census_pop_hist",
                     "gcam-korea/A44.flsp_bm2_korea_res") ->
      L144.flsp_bm2_korea_resid

    L144.in_EJ_korea_resid %>%
      add_title("Residential energy consumption by state/fuel/end use") %>%
      add_units("EJ/yr") %>%
      add_comments("Service energy is divided by GCAM default share") %>%
      add_legacy_name("L144.in_EJ_korea_resid") %>%
      add_precursors("gcam-korea/states_subregions",
                     "L144.in_EJ_R_bld_serv_F_Yh",
                     "L142.in_EJ_korea_bld_F") ->
      L144.in_EJ_korea_resid

    return_data(L144.flsp_bm2_korea_resid, L144.in_EJ_korea_resid)
  } else {
    stop("Unknown command")
  }
}
