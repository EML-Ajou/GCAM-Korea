#' module_gcam.korea_LA100.Socioeconomics
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.pcGDP_thous90usd_korea}, \code{L100.GDP_mil90usd_korea}, \code{L100.Pop_thous_korea}. The corresponding file in the
#' original data system was \code{LA100.Socioeconomics.R} (gcam-korea level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author M. Roh
module_gcam.korea_LA100.Socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             FILE = "gcam-korea/GRDP_09WON_state",
             FILE = "gcam-korea/GRDP_97WON_state",
             FILE = "gcam-korea/Census_pop_hist",
             FILE = "gcam-korea/PRIMA_pop",
             "L100.gdp_mil90usd_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.pcGDP_thous90usd_korea",
             "L100.GDP_mil90usd_korea",
             "L100.Pop_thous_korea"))
  } else if(command == driver.MAKE) {

    state <- state_name <- year <- value <- Fips <- Area <- population <- iso <-
        share <- pop_ratio <- NULL      # silence package check.

    all_data <- list(...)[[1]]

    states_subregions         <- get_data(all_data, "gcam-korea/states_subregions")
    GRDP_09WON_state     <- get_data(all_data, "gcam-korea/GRDP_09WON_state")
    GRDP_97WON_state     <- get_data(all_data, "gcam-korea/GRDP_97WON_state")
    Census_pop_hist           <- get_data(all_data, "gcam-korea/Census_pop_hist")
    PRIMA_pop                 <- get_data(all_data, "gcam-korea/PRIMA_pop")
    L100.gdp_mil90usd_ctry_Yh <- get_data(all_data, "L100.gdp_mil90usd_ctry_Yh")

    states_subregions <- select(states_subregions, state, state_name)

    GRDP_97WON_state %>%
      gather_years %>%
      PH_year_value_historical %>%
      group_by(Area) %>%
      mutate(value = approx_fun(year, value, rule = 2))%>%
      replace_na(list(value = 0)) ->
      GRDP_97WON_state

    GRDP_09WON_state %>%
      gather_years %>%
      PH_year_value_historical%>%
      replace_na(list(value = 0))->
      GRDP_09WON_state

    Census_pop_hist %>%
      gather_years %>%
      PH_year_value_historical%>%
      replace_na(list(value = 0)) %>%
      rename(population = value) ->
      Census_pop_hist

    # Bind the '97 and '09 GDP datasets to get a continuous time series
    GRDP_97WON_state %>%
      ungroup %>%
      filter(!year %in% unique(GRDP_09WON_state$year)) %>%
      bind_rows(GRDP_09WON_state) %>%
      mutate(state=Area)%>%
      # merge with state name/codes
      left_join_error_no_match(states_subregions, by = c("state")) %>%
      select(state, year, value) %>%
      # merge with census data, and compute total GDP (population * per capita GDP)
      #gdp unit: thou.won, population: person
      left_join(Census_pop_hist, by = c("state", "year")) %>%
      mutate(value = value * 1000 * population) %>%
      arrange(state, year) %>%
      select(-population) %>%
      # compute by-state shares by year
      group_by(year) %>%
      mutate(share = value / sum(value)) %>%
      select(-value) ->
      L100.GDPshare_state

    # Multiply the country-level GDP by the state shares
    L100.gdp_mil90usd_ctry_Yh %>%
      filter(iso == "kor") %>%
      right_join(L100.GDPshare_state, by = c("year")) %>%
      mutate(value = value * share) %>%
      select(-share, -iso) %>%
      add_title("GDP by state") %>%
      add_units("million 1990 USD") %>%
      add_comments("") %>%
      add_precursors("L100.gdp_mil90usd_ctry_Yh",
                     "gcam-korea/GRDP_97WON_state",
                     "gcam-korea/GRDP_09WON_state",
                     "gcam-korea/Census_pop_hist") %>%
      add_legacy_name("L100.GDP_mil90usd_korea") ->
      L100.GDP_mil90usd_korea

    # Compute per capita GDP by state
    L100.GDP_mil90usd_korea %>%
      left_join(Census_pop_hist, by = c("state", "year")) %>%
      mutate(value = value * CONV_MIL_THOUS / population) %>%
      select(-population) %>%
      add_title("Per-capita GDP by state") %>%
      add_units("thousand 1990 USD per capita") %>%
      add_comments("") %>%
      add_precursors("L100.GDP_mil90usd_korea") %>%
      add_legacy_name("L100.pcGDP_thous90usd_korea") ->
      L100.pcGDP_thous90usd_korea

    # Future population by scenario. Right now just one scenario.
    PRIMA_pop %>%
      # reshape
      gather_years(value_col = "population") %>%
      mutate(population = as.numeric(population)) %>%
      # interpolate any missing data from end of history into future
      complete(nesting(state), year = c(socioeconomics.FINAL_HIST_YEAR, FUTURE_YEARS)) %>%
      group_by(state) %>%
      mutate(population = approx_fun(year, population)) %>%
      arrange(state, year) %>%
      # compute ratios (change from end of history)
      mutate(pop_ratio = population / first(population)) %>%
      arrange(state, year) %>%
      #rename(state_name = state) %>%
      #left_join_error_no_match(states_subregions, by = "state") %>%
      ungroup %>%
      select(-population) ->
      L100.Pop_ratio_state

    # Starting from end of history, project state populations into future
    Census_pop_hist %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      right_join(L100.Pop_ratio_state, by = c("state")) %>%
      filter(year > max(HISTORICAL_YEARS)) %>%
      mutate(population = population * pop_ratio) %>%
      bind_rows(Census_pop_hist) %>%
      mutate(value = population * CONV_ONES_THOUS) %>%
      select(-population, -pop_ratio) %>%
      arrange(state, year) %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("State populations from end of history projected into future") %>%
      add_precursors("L100.gdp_mil90usd_ctry_Yh",
                     "gcam-korea/GRDP_97WON_state",
                     "gcam-korea/GRDP_09WON_state",
                     "gcam-korea/PRIMA_pop",
                     "gcam-korea/states_subregions") %>%
      add_legacy_name("L100.Pop_thous_korea") ->
      L100.Pop_thous_korea

    return_data(L100.pcGDP_thous90usd_korea, L100.GDP_mil90usd_korea, L100.Pop_thous_korea)
  } else {
    stop("Unknown command")
  }
}
