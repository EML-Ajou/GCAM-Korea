#' module_gcam.korea_L201.socioeconomics
#'
#' Interest rate, population, and GDP for gcam-korea.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate_GCAM_korea}, \code{L201.Pop_GCAM_korea}, \code{L201.BaseGDP_GCAM_korea}, \code{L201.LaborForceFillout_GCAM_korea}, \code{L201.LaborProductivity_GCAM_korea}. The corresponding file in the
#' original data system was \code{L201.socioeconomics_USA.R} (gcam-usa level2).
#' @details Interest rate, population, and GDP for gcam-korea.
#' Note that Ulsan was promoted to metropolitan city in 1997. so, we use very samll value instead of base year value to prevent model errors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author M. Roh
module_gcam.korea_L201.socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             FILE = "gcam-korea/GDP_Growth",
             "L100.Pop_thous_korea",
             "L100.GDP_mil90usd_korea",
             #"L100.pcGDP_thous90usd_korea",
             "L102.pcgdp_thous90USD_GCAM3_ctry_Y"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.InterestRate_GCAM_korea",
             "L201.Pop_GCAM_korea",
             "L201.BaseGDP_GCAM_korea",
             "L201.LaborForceFillout_GCAM_korea",
             "L201.LaborProductivity_GCAM_korea"))
  } else if(command == driver.MAKE) {

    # silence package checks
    year <- value <- state <- totalPop <- baseGDP <- iso <- growth <- timestep <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions")
    L100.Pop_thous_korea <- get_data(all_data, "L100.Pop_thous_korea")
    L100.GDP_mil90usd_korea <- get_data(all_data, "L100.GDP_mil90usd_korea")
    # L100.pcGDP_thous90usd_korea <- get_data(all_data, "L100.pcGDP_thous90usd_korea")
    L102.pcgdp_thous90USD_GCAM3_ctry_Y<- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_ctry_Y")
    GDP_Growth <- get_data(all_data, "gcam-korea/GDP_Growth")
    # ===================================================
    # NOTE: Socioeconomics for grid regions are dealt with in module_gcam.usa_L223.electricity_USA

    # L201.InterestRate: Interest rates by region
    L201.InterestRate <- tibble(region = states_subregions$state, interest.rate = socioeconomics.DEFAULT_INTEREST_RATE)

    L201.Pop_GCAM_korea <- L100.Pop_thous_korea %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(totalPop = value,
             region = state) %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS))

    L201.Pop_GCAM_korea[which(L201.Pop_GCAM_korea$region=="US" &
                                L201.Pop_GCAM_korea$year %in% c(1975,1990)),]$totalPop <- L201.Pop_GCAM_korea[which(L201.Pop_GCAM_korea$region=="US" &
                                                                                                                            L201.Pop_GCAM_korea$year==2005),]$totalPop

    L201.BaseGDP_GCAM_korea <- L100.GDP_mil90usd_korea %>%
      filter(year == min(MODEL_YEARS)) %>%
      rename(baseGDP = value, region = state) %>%
      # To prevent model error, we use gdp1998 instead of gdp1975
      mutate(baseGDP=if_else(region=="US",
                             L100.GDP_mil90usd_korea[which(L100.GDP_mil90usd_korea$year==1998 &L100.GDP_mil90usd_korea$state=="US"),]$value,
                             baseGDP)) %>%
      mutate(baseGDP = round(baseGDP, socioeconomics.GDP_DIGITS)) %>%
      select(-year)

    # L201.LaborForceFillout: Labor force participation and productivity for all scenarios
    # NOTE: No model of labor force used; labor force participation set to a constant
    L201.LaborForceFillout <- tibble(region = states_subregions$state,
                                     year.fillout = min(MODEL_YEARS),
                                     laborforce = socioeconomics.DEFAULT_LABORFORCE)



    L100.GDP_mil90usd_korea <- arrange(L100.GDP_mil90usd_korea, state, year)
    L100.GDP_mil90usd_korea %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(prevValue = lag(value)) %>%
      mutate(growth.rate = value/prevValue) %>%
      ungroup() %>%
      select(state, year, growth.rate)-> hitorical_gdp_growth

    model_years_gdp_growth <- tidyr::crossing(region = gcamkorea.STATES, year = MODEL_YEARS)
    model_years_gdp_growth %>%
      left_join(hitorical_gdp_growth, by=c("region"="state", "year")) %>%
      left_join(GDP_Growth, by="year") %>%
      mutate(gdp.growth=if_else(year<=CALIBRATION_YEAR, growth.rate, value),
             prevYear=lag(year)) %>%
      mutate(timestep=year-prevYear) %>%
      select(region, year, gdp.growth, timestep)-> model_years_gdp_growth


    L100.Pop_thous_korea <- arrange(L100.Pop_thous_korea, state, year)
    L100.Pop_thous_korea %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(prevValue = lag(value)) %>%
      mutate(pop.growth = value/prevValue) %>%
      ungroup() %>%
      select(state, year, pop.growth)-> pop_growth

    model_years_gdp_growth %>%
      left_join(pop_growth, by=c("region"="state", "year")) %>%
      # Remove the first model year, since it has no previous period to calculate growth rate
      filter(year != min(MODEL_YEARS)) %>%
      mutate(laborproductivity = round((gdp.growth/pop.growth) ^ (1 / timestep) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) -> L201.LaborProductivity_GCAM_korea

    # Usan!!!!!
    L201.LaborProductivity_GCAM_korea[which(L201.LaborProductivity_GCAM_korea$region=="US" &
                                             L201.LaborProductivity_GCAM_korea$year%in%c(1990,2005)),]$laborproductivity <- 0


    # ===================================================

    # Produce outputs
    L201.InterestRate %>%
      add_title("Interest rates by state") %>%
      add_units("Unitless") %>%
      add_comments("Constant assumed for all states") %>%
      add_legacy_name("L201.InterestRate_GCAM_korea") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L201.InterestRate_GCAM_korea

    L201.Pop_GCAM_korea %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("Data from L100.Pop_thous_korea") %>%
      add_legacy_name("L201.Pop_GCAM_korea") %>%
      add_precursors("L100.Pop_thous_korea") ->
      L201.Pop_GCAM_korea

    L201.BaseGDP_GCAM_korea %>%
      add_title("Base year GDP by state") %>%
      add_units("million 1990 USD") %>%
      add_comments("Data from L100.GDP_mil90usd_korea") %>%
      add_legacy_name("L201.BaseGDP_GCAM_korea") %>%
      add_precursors("L100.GDP_mil90usd_korea") ->
      L201.BaseGDP_GCAM_korea

    L201.LaborForceFillout %>%
      add_title("Labor force participation and productivity for all scenarios") %>%
      add_units("Unitless") %>%
      add_comments("Constant value assumed") %>%
      add_legacy_name("L201.LaborForceFillout") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L201.LaborForceFillout_GCAM_korea

    L201.LaborProductivity_GCAM_korea %>%
      add_title("Labor force productivity growth rate for GCAM-Korea") %>%
      add_units("Unitless (annual rate of growth)") %>%
      add_comments("Values from L100.pcGDP_thous90usd_korea used to calculate annual growth") %>%
      add_comments("Korea value written to all states") %>%
      add_legacy_name("L201.LaborProductivity_GCAM_korea") %>%
      add_precursors("gcam-korea/GDP_Growth",
                     "L102.pcgdp_thous90USD_GCAM3_ctry_Y",
                     "L100.Pop_thous_korea") ->
      L201.LaborProductivity_GCAM_korea


    return_data(L201.InterestRate_GCAM_korea, L201.Pop_GCAM_korea, L201.BaseGDP_GCAM_korea, L201.LaborForceFillout_GCAM_korea, L201.LaborProductivity_GCAM_korea)
  } else {
    stop("Unknown command")
  }
}
