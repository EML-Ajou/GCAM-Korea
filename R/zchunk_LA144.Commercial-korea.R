#' module_gcam.korea_LA144.Commercial
#'
#' Calculates commercial floorspace by state and energy consumption by state/fuel/end use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_korea_comm}, \code{L144.in_EJ_korea_comm}. The corresponding file in the
#' original data system was \code{LA144.Commercial.R} (gcam-korea level1).
#' @details Calculates commercial floorspace by state and energy consumption by state/fuel/end use
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author M. Roh
module_gcam.korea_LA144.Commercial <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             FILE = "gcam-korea/Census_pop_hist",
             FILE = "gcam-korea/A44.flsp_bm2_korea_comm",
             "L144.in_EJ_R_bld_serv_F_Yh",
             "L142.in_EJ_korea_bld_F",
             "L143.share_korea_Pop_CDD",
             "L143.share_korea_Pop_HDD"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_korea_comm",
             "L144.in_EJ_korea_comm"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    subregion4 <- subregion9 <- REGION <- DIVISION <- state <- year <- value <- setNames <-
      SQFT1 <- SQFT2 <- SQFT <- ADJWT <- subregion4 <- . <- pcflsp_m2 <- pcflsp_m2.x <- pcflsp_m2.y <-
      conv_4_9 <- variable <- scaler <- sector <- value <- fuel <- share <- efficiency <- service <-
      value.x <- value.y <- Year <- unit <- value_EJ <- pre <- post <- state_EJ <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions") %>%
      select(state_name, REGION, DIVISION, state) %>%
      distinct()

    Census_pop_hist <- get_data(all_data, "gcam-korea/Census_pop_hist") %>% gather_years
    A44.flsp_bm2_korea_comm <- get_data(all_data, "gcam-korea/A44.flsp_bm2_korea_comm")

    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh") %>%
      filter(GCAM_region_ID == gcamkorea.RegionNum)

    L142.in_EJ_korea_bld_F <- get_data(all_data, "L142.in_EJ_korea_bld_F")

    L143.share_korea_Pop_CDD <- get_data(all_data, "L143.share_korea_Pop_CDD")
    L143.share_korea_Pop_HDD <- get_data(all_data, "L143.share_korea_Pop_HDD")

    # ===================================================

    # Add subregions to census population for aggregating
    L144.Census_pop_hist <- Census_pop_hist %>%
      left_join_error_no_match(states_subregions, by = "state") %>%
      filter(year %in% HISTORICAL_YEARS)

    L144.pop_korR <- L144.Census_pop_hist %>%
      group_by(state, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Calculate per capita floorspace
    L144.flsp_bm2_korR <- A44.flsp_bm2_korea_comm %>%
      left_join_error_no_match(L144.pop_korR, by = c("year", "state")) %>%
      mutate(pcflsp_bm2 = flsp_bm2 / value) %>%
      select(state, year, pcflsp_bm2)

    # Interpolate floorspace values to all historical years
    L144.pcflsp_bm2_kor_comm <- L144.flsp_bm2_korR %>%
      select(state) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      # Using left_join because not all years included
      left_join(L144.flsp_bm2_korR, by = c("state", "year")) %>%
      group_by(state) %>%
      mutate(pcflsp_bm2 = approx_fun(year, pcflsp_bm2, rule = 2)) %>%
      ungroup()

    # Expand to states: multiply per-capita floorspace in each subregion9 times the population of each state
    L144.flsp_bm2_korea_comm <- L144.Census_pop_hist %>%
      left_join_error_no_match(L144.pcflsp_bm2_kor_comm, by = c("state", "year")) %>%
      transmute(state,
                sector = "comm",
                year,
                # Floorspace = population * per-capita floorspace
                value = value * pcflsp_bm2)

    L144.in_EJ_R_bld_serv_F_Yh<- L144.in_EJ_R_bld_serv_F_Yh %>%
      filter(sector=="bld_comm")

    bld.services <- unique(L144.in_EJ_R_bld_serv_F_Yh$service)

    L144.in_EJ_R_bld_serv_F_Yh.share <- L144.in_EJ_R_bld_serv_F_Yh %>%
      group_by(year, fuel) %>%
      mutate(total=sum(value), share=value/total) %>%
      ungroup() %>%
      repeat_add_columns(tibble(state = gcamkorea.STATES)) %>%
      select(state, fuel, service, year, share)

    L144.in_EJ_korea_comm <- L142.in_EJ_korea_bld_F %>%
      filter(sector == "comm") %>%
      repeat_add_columns(tibble(service=bld.services)) %>%
      left_join(L144.in_EJ_R_bld_serv_F_Yh.share, by=c(state, fuel, service, year)) %>%
      replace_na(list(share = 0)) %>%
      transmute(state,
                sector = "comm",
                fuel,
                service,
                year,
                value = value*share)

    # ===================================================

    # Produce outputs
    L144.flsp_bm2_korea_comm %>%
      add_title("Commercial floorspace by state") %>%
      add_units("billion m2") %>%
      add_comments("Floorspace by state calculated by multiplying state population by per-capita census division floorspace") %>%
      add_legacy_name("L144.flsp_bm2_korea_comm") %>%
      add_precursors("gcam-korea/states_subregions",
                     "gcam-korea/Census_pop_hist",
                     "gcam-korea/A44.flsp_bm2_korea_comm") ->
      L144.flsp_bm2_korea_comm

    L144.in_EJ_korea_comm %>%
      add_title("Commercial energy consumption by state/fuel/end use") %>%
      add_units("EJ/yr") %>%
      add_comments("Service energy is divided by GCAM default share") %>%
      add_legacy_name("L144.in_EJ_korea_comm") %>%
      add_precursors("gcam-korea/states_subregions",
                     "L144.in_EJ_R_bld_serv_F_Yh",
                     "L142.in_EJ_korea_bld_F",
                     "L143.share_korea_Pop_CDD",
                     "L143.share_korea_Pop_HDD") ->
      L144.in_EJ_korea_comm

    return_data(L144.flsp_bm2_korea_comm, L144.in_EJ_korea_comm)
  } else {
    stop("Unknown command")
  }
}
