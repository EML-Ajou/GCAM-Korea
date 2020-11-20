#' module_gcam.korea_L223.electricity_korea
#'
#' Generates gcam-korea model inputs for electrcity sector by grid regions and states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.DeleteSubsector_kor_elec_market},
#' \code{L223.Supplysector_kor_elec_market}, \code{L223.SubsectorShrwtFllt_kor_elec_market}, \code{L223.SubsectorInterp_kor_elec_market},
#' \code{L223.SubsectorLogit_kor_elec_market}, \code{L223.TechShrwt_kor_elec_market}, \code{L223.TechCoef_kor_elec_market},
#' \code{L223.Production_kor_elec_market},\code{L223.PassthroughSector_elec_korea}, \code{L223.PassthroughTech_elec_kor_grid},
#' \code{L223.Supplysector_elec_kor_grid}, \code{L223.SubsectorShrwtFllt_elec_kor_grid}, \code{L223.SubsectorInterp_elec_kor_grid},
#' \code{L223.SubsectorLogit_elec_kor_grid}, \code{L223.TechShrwt_elec_kor_grid}, \code{L223.TechCoef_elec_kor_grid},
#' \code{L223.Production_elec_kor_grid}, \code{L223.InterestRate_kor_grid}, \code{L223.Pop_kor_grid}, \code{L223.BaseGDP_kor_grid},
#' \code{L223.LaborForceFillout_kor_grid},\code{L223.Supplysector_elec_korea}, \code{L223.ElecReserve_korea},
#' \code{L223.SubsectorLogit_elec_korea}, \code{L223.SubsectorShrwtFllt_elec_korea}, \code{L223.SubsectorShrwt_nuc_korea},
#' \code{L223.SubsectorShrwt_renew_korea}, \code{L223.SubsectorInterp_elec_korea}, \code{L223.SubsectorInterpTo_elec_korea},
#' \code{L223.StubTech_elec_korea}, \code{L223.StubTechEff_elec_korea}, \code{L223.StubTechCapFactor_elec_korea},
#' \code{L223.StubTechFixOut_elec_korea}, \code{L223.StubTechFixOut_hydro_korea}, \code{L223.StubTechProd_elec_korea},
#' \code{L223.StubTechMarket_elec_korea}, \code{L223.StubTechMarket_backup_korea}, \code{L223.StubTechElecMarket_backup_korea},
#' \code{L223.StubTechCapFactor_elec_wind_korea}, \code{L223.StubTechCapFactor_elec_solar_korea}. The corresponding file in the
#' original data system was \code{L223.electricity.R} (gcam-korea level2).
#' @details This chunk generates input files to create an annualized electricity generation sector for each state
#' and creates the demand for the state-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author M. Roh
module_gcam.korea_L223.electricity_korea <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             FILE = "gcam-korea/calibrated_techs",
             FILE = "gcam-korea/korea_re_technical_potential",
             FILE = "gcam-korea/nuclear_fixedOutput",
             FILE = "energy/A23.globaltech_eff",
             FILE = "gcam-korea/A23.subsectorFixedInterp",
             FILE = "gcam-korea/A23.subsectorInterpTo",
             FILE = "gcam-korea/A23.TechCoef_elec_grid",
             "L114.CapacityFactor_wind_korea",
             "L119.CapFacScaler_PV_korea",
             "L119.CapFacScaler_CSP_korea",
             "L223.Supplysector_elec",
             "L223.ElecReserve",
             "L223.SubsectorLogit_elec",
             "L223.SubsectorShrwtFllt_elec",
             "L223.SubsectorShrwt_renew",
             "L223.SubsectorInterp_elec",
             "L223.SubsectorInterpTo_elec",
             "L223.StubTech_elec",
             "L223.StubTechEff_elec",
             "L223.StubTechCapFactor_elec",
             "L223.GlobalIntTechBackup_elec",
             "L1231.in_EJ_korea_elec_F_tech",
             "L1231.out_EJ_korea_elec_F_tech",
             "L1232.out_EJ_sR_elec_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.DeleteSubsector_kor_elec_market",
             "L223.Supplysector_kor_elec_market",
             "L223.SubsectorShrwtFllt_kor_elec_market",
             "L223.SubsectorInterp_kor_elec_market",
             "L223.SubsectorLogit_kor_elec_market",
             "L223.TechShrwt_kor_elec_market",
             "L223.TechCoef_kor_elec_market",
             "L223.Production_kor_elec_market",
             "L223.PassthroughSector_elec_korea",
             "L223.PassthroughTech_elec_kor_grid",
             "L223.Supplysector_elec_kor_grid",
             "L223.SubsectorShrwtFllt_elec_kor_grid",
             "L223.SubsectorInterp_elec_kor_grid",
             "L223.SubsectorLogit_elec_kor_grid",
             "L223.TechShrwt_elec_kor_grid",
             "L223.TechCoef_elec_kor_grid",
             "L223.Production_elec_kor_grid",
             "L223.InterestRate_kor_grid",
             "L223.Pop_kor_grid",
             "L223.BaseGDP_kor_grid",
             "L223.LaborForceFillout_kor_grid",
             "L223.Supplysector_elec_korea",
             "L223.ElecReserve_korea",
             "L223.SubsectorLogit_elec_korea",
             "L223.SubsectorShrwtFllt_elec_korea",
             #"L223.SubsectorShrwt_nuc_korea",
             "L223.SubsectorShrwt_renew_korea",
             "L223.SubsectorInterp_elec_korea",
             "L223.SubsectorInterpTo_elec_korea",
             "L223.StubTech_elec_korea",
             "L223.StubTechEff_elec_korea",
             "L223.StubTechCapFactor_elec_korea",
             "L223.StubTechFixOut_elec_korea",
             "L223.StubTechFixOut_hydro_korea",
             "L223.StubTechFixOut_nuclear_korea",
             "L223.StubTechProd_elec_korea",
             "L223.StubTechMarket_elec_korea",
             "L223.StubTechMarket_backup_korea",
             "L223.StubTechElecMarket_backup_korea",
             "L223.StubTechCapFactor_elec_wind_korea",
             "L223.StubTechCapFactor_elec_solar_korea"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    grid_region <- Geothermal_Hydrothermal_GWh <- state <- geo_state_noresource <-
      region <- supplysector <- subsector <- technology <- year <- value <-
      sector <- calOutputValue <- fuel <- elec <- share <- avg.share <- pref <-
      share.weight.mult <- share.weight <- market.name <- sector.name <- subsector.name <-
      minicam.energy.input <- calibration <- secondary.output <- stub.technology <-
      capacity.factor <- scaler <- capacity.factor.capital <- . <- NULL  # silence package check notes

    # Load required inputs
    A23.TechCoef_elec_grid <- get_data(all_data, "gcam-korea/A23.TechCoef_elec_grid")
    A23.subsectorInterpTo <- get_data(all_data, "gcam-korea/A23.subsectorInterpTo")
    A23.subsectorFixedInterp <- get_data(all_data, "gcam-korea/A23.subsectorFixedInterp")
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions")
    calibrated_techs <- get_data(all_data, "gcam-korea/calibrated_techs")
    nuclear_fixedOutput <- get_data(all_data, "gcam-korea/nuclear_fixedOutput")
    #korea_re_technical_potential <- get_data(all_data, "gcam-korea/korea_re_technical_potential")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    L114.CapacityFactor_wind_korea <- get_data(all_data, "L114.CapacityFactor_wind_korea")
    L119.CapFacScaler_PV_korea <- get_data(all_data, "L119.CapFacScaler_PV_korea")
    L119.CapFacScaler_CSP_korea <- get_data(all_data, "L119.CapFacScaler_CSP_korea")
    L223.Supplysector_elec <- get_data(all_data, "L223.Supplysector_elec")
    L223.ElecReserve <- get_data(all_data, "L223.ElecReserve")
    L223.SubsectorLogit_elec <- get_data(all_data, "L223.SubsectorLogit_elec")
    L223.SubsectorShrwtFllt_elec <- get_data(all_data, "L223.SubsectorShrwtFllt_elec")
    #L223.SubsectorShrwt_nuc <- get_data(all_data, "L223.SubsectorShrwt_nuc")
    L223.SubsectorShrwt_renew <- get_data(all_data, "L223.SubsectorShrwt_renew")
    L223.SubsectorInterp_elec <- get_data(all_data, "L223.SubsectorInterp_elec")
    L223.SubsectorInterpTo_elec <- get_data(all_data, "L223.SubsectorInterpTo_elec")
    L223.StubTech_elec <- get_data(all_data, "L223.StubTech_elec")
    L223.StubTechEff_elec <- get_data(all_data, "L223.StubTechEff_elec")
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec")
    L223.GlobalIntTechBackup_elec <- get_data(all_data, "L223.GlobalIntTechBackup_elec")
    L1231.in_EJ_korea_elec_F_tech <- get_data(all_data, "L1231.in_EJ_korea_elec_F_tech")
    L1231.out_EJ_korea_elec_F_tech <- get_data(all_data, "L1231.out_EJ_korea_elec_F_tech")
    L1232.out_EJ_sR_elec_korea <- get_data(all_data, "L1232.out_EJ_sR_elec_korea")


    # A vector of USA grid region names
    states_subregions %>%
      select(grid_region) %>%
      unique %>%
      arrange(grid_region) %>%
      unlist ->
      grid_regions

    elec_gen_names <- "electricity"

    #### Minyoung might have modified this #### Start
    # A vector indicating states where geothermal electric technologies will not be created
    states_subregions %>%
      transmute(geo_state_noresource = paste(state, "geothermal", sep = " ")) %>%
      unlist ->
      geo_states_noresource
    #### Minyoung might have modified this #### End


    # gcamkorea.USE_REGIONAL_ELEC_MARKETS is TRUE, indicating to resolve electricity demands
    # at the level of the grid regions. The entire loop below produces outputs
    # assoicated with resolving demand at the national level, and is currently disabled.
    # Instead, a set of empty tibbles are produced at the end of this chunk.
    if(!gcamkorea.USE_REGIONAL_ELEC_MARKETS) {
      # PART 1: THE USA REGION
      # Define the sector(s) that will be used in this code file. Can be one or multiple sectors
      # The subsectors of the existing USA electricity sector are deleted.
      # Keeping the supplysector info, incl. reserve margin
      # NOTE: This also removes the rooftop PV subsector of the USA elect_td_bld sector
      L223.SubsectorLogit_elec %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        filter(region == gcamkorea.REGION) ->
        L223.DeleteSubsector_kor_elec_market

      # L223.Supplysector_kor_elec_market: supplysector for electricity sector in the USA region,
      # including logit exponent between grid regions
      # All of the supplysector information is the same as before, except the logit exponent
      tibble(region = gcamkorea.REGION,
             supplysector = elec_gen_names,
             output.unit = "EJ",
             input.unit = "EJ",
             price.unit = "1975$/GJ",
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamkorea.GRID_REGION_LOGIT,
             logit.type = gcamkorea.GRID_REGION_LOGIT_TYPE) %>%
        select(LEVEL2_DATA_NAMES[["Supplysector"]]) ->
        L223.Supplysector_kor_elec_market

      # L223.SubsectorShrwtFllt_kor_elec_market: subsector (grid region) share-weights in USA electricity
      # No need to read in subsector logit exponents, which are applied to the technology competition
      tibble(region = gcamkorea.REGION,
             supplysector = elec_gen_names,
             subsector = paste(grid_regions, elec_gen_names, sep = " "),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) ->
        L223.SubsectorShrwtFllt_kor_elec_market

      # L223.SubsectorInterp_kor_elec_market: temporal interpolation of subsector share-weights in USA electricity
      L223.SubsectorShrwtFllt_kor_elec_market %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        mutate(apply.to = "share-weight",
               from.year = max(MODEL_BASE_YEARS),
               to.year = max(MODEL_YEARS),
               interpolation.function = "fixed") ->
        L223.SubsectorInterp_kor_elec_market

      # L223.SubsectorLogit_kor_elec_market: logit exponent of subsector in USA electricity
      # NOTE: There is only one tech per subsector, so the logit choice does not matter
      L223.SubsectorShrwtFllt_kor_elec_market %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
               logit.exponent = gcamkorea.GRID_REGION_LOGIT,
               logit.type = gcamkorea.GRID_REGION_LOGIT_TYPE) %>%
        select(LEVEL2_DATA_NAMES[["SubsectorLogit"]]) ->
        L223.SubsectorLogit_kor_elec_market

      # L223.TechShrwt_kor_elec_market: technology share-weights in the USA region
      L223.SubsectorShrwtFllt_kor_elec_market %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        mutate(technology = subsector) %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        mutate(share.weight = 1) ->
        L223.TechShrwt_kor_elec_market

      # L223.TechCoef_kor_elec_market: technology coefficients and market names in the USA region
      L223.TechShrwt_kor_elec_market %>%
        select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
        mutate(minicam.energy.input = supplysector,
               coefficient = 1,
               market.name = substr(technology, 1, nchar(subsector) - nchar(supplysector) - 1)) ->
        L223.TechCoef_kor_elec_market

      # L223.Production_kor_elec_market: calibrated electricity production in USA (consuming output of grid subregions)
      L1232.out_EJ_sR_elec_korea %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT)) %>%
        left_join_error_no_match(unique(select(calibrated_techs, sector, supplysector)), by = "sector") %>%
        mutate(subsector = paste(grid_region, supplysector, sep = " ")) ->
      L223.out_EJ_sR_elec

      L223.TechCoef_kor_elec_market %>%
        select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        left_join_error_no_match(L223.out_EJ_sR_elec, by = c("supplysector", "subsector", "year")) %>%
        mutate(share.weight.year = year,
               tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
        set_subsector_shrwt() %>%
        select(LEVEL2_DATA_NAMES[["Production"]]) ->
        L223.Production_kor_elec_market
    }

    # PART 2: THE FERC REGIONS
    # NOTE: FERC grid regions function in similar fashion to the USA region:
    # competing electricity from subregions

    # L223.Supplysector_elec_kor_grid: supplysector for electricity sector in the grid regions,
    # including logit exponent between states within grid region
    # NOTE: use the same logit exponent for states within FERC region as for FERC regions within the USA
    tibble(region = grid_regions,
           supplysector = elec_gen_names,
           output.unit = "EJ",
           input.unit = "EJ",
           price.unit = "1975$/GJ",
           logit.year.fillout = min(MODEL_BASE_YEARS),
           logit.exponent = gcamkorea.GRID_REGION_LOGIT,
           logit.type = gcamkorea.GRID_REGION_LOGIT_TYPE) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L223.Supplysector_elec_kor_grid

    # L223.SubsectorShrwtFllt_elec_kor_grid: subsector (state) share-weights in grid regions
    states_subregions %>%
      select(region = grid_region, state) %>%
      mutate(supplysector = elec_gen_names,
             subsector = paste(state, supplysector, sep = " "),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) %>%
      select(-state) %>%
      arrange(region) ->
      L223.SubsectorShrwtFllt_elec_kor_grid

    # L223.SubsectorInterp_elec_kor_grid: temporal interpolation of subsector (state) share-weights in grid regions
    L223.SubsectorShrwtFllt_elec_kor_grid %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L223.SubsectorInterp_elec_kor_grid

    # L223.SubsectorLogit_elec_kor_grid: logit exponent of subsector (states) in grid regions
    # NOTE: There is only one tech per subsector, so the logit choice does not matter
    L223.SubsectorShrwtFllt_elec_kor_grid %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamkorea.GRID_REGION_LOGIT,
             logit.type = gcamkorea.GRID_REGION_LOGIT_TYPE) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L223.SubsectorLogit_elec_kor_grid

    # L223.TechShrwt_elec_kor_grid: technology share-weights in grid regions
    L223.SubsectorShrwtFllt_elec_kor_grid %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L223.TechShrwt_elec_kor_grid

    # L223.TechCoef_elec_kor_grid: technology coefficients and market names in grid regions
    L223.TechShrwt_elec_kor_grid %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      mutate(minicam.energy.input = supplysector,
             #coefficient = 1,
             market.name = substr(technology, 1, nchar(subsector) - nchar(supplysector) - 1)) ->
      L223.TechCoef_elec_kor_grid

    L223.TechCoef_elec_kor_grid %>%
      left_join_error_no_match(A23.TechCoef_elec_grid, by=colnames(L223.TechCoef_elec_kor_grid)) -> L223.TechCoef_elec_kor_grid


    # L223.PassthroughSector_elec_korea: passthrough sector of US states
    # The marginal revenue sector is the region's electricity sector
    # whereas the marginal revenue market is the grid region.
    states_subregions %>%
      select(region = state, grid_region) %>%
      mutate(passthrough.sector = "electricity",
             marginal.revenue.sector = "electricity",
             marginal.revenue.market = grid_region) %>%
      select(-grid_region) ->
      L223.PassthroughSector_elec_korea

    # L223.PassthroughTech_elec_kor_grid: passthrough technology of grid regions
    # This one should contain region, supplysector, subsector, technology for the grid regions
    # to which electricity produced in states is passed through.
    L223.TechShrwt_elec_kor_grid %>%
      select(region, supplysector, subsector, technology) ->
      L223.PassthroughTech_elec_kor_grid

    # minyoung
    # change cal output to the fixed output of nuclear exept HISTORICAL_YEARS
    # change Gen_II_LWR name to Gen_II_LWR_Kor
    L1231.out_EJ_korea_elec_F_tech %>%
      mutate(technology = if_else(technology=="Gen_II_LWR", "Gen_II_LWR_kor", technology)) ->
      L1231.out_EJ_korea_elec_F_tech

    #L1231.out_EJ_korea_elec_F_tech %>%
    #  left_join(nuclear_fixedOutput, by=c("state"="region", "year"))%>%
    #  replace_na(list(value.y = 0)) %>%
    #  mutate(value = if_else(technology=="Gen_II_LWR", value.y, value.x),
    #         technology = if_else(technology=="Gen_II_LWR", "Gen_II_LWR_kor", technology)) %>%
    #  select(-value.x, -value.y) ->
    #  L1231.out_EJ_korea_elec_F_tech


    # L223.Production_elec_kor_grid: calibrated electricity production in grid region (consuming output of grid subregions)
    L1231.out_EJ_korea_elec_F_tech %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(unique(select(calibrated_techs, sector, supplysector)), by = "sector") %>%
      mutate(subsector = paste(state, supplysector, sep = " ")) %>%
      # This needs to be aggregated to the subsector level
      group_by(supplysector, subsector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup ->
      L223.out_EJ_state_elec

    L223.TechCoef_elec_kor_grid %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L223.out_EJ_state_elec, by = c("supplysector", "subsector", "year")) %>%
      mutate(share.weight.year = year,
             # tech.share.weights are set at technology level
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      # sub.share.weights are set the the subsector level in case with multiple technologies
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L223.Production_elec_kor_grid

    # Socioeconomic information in the electricity grid regions (required for GCAM to run with these regions)

    # L223.InterestRate_kor_grid: Interest rates in the FERC grid regions
    tibble(region = grid_regions,
           interest.rate = socioeconomics.DEFAULT_INTEREST_RATE) ->
      L223.InterestRate_kor_grid

    # L223.Pop_kor_grid: Population
    tibble(region = grid_regions,
           totalPop = 1) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L223.Pop_kor_grid

    # L223.BaseGDP_kor_grid: Base GDP in FERC grid regions
    tibble(region = grid_regions,
           baseGDP = 1)  ->
      L223.BaseGDP_kor_grid

    # L223.LaborForceFillout_kor_grid: labor force in the grid regions
    tibble(region = grid_regions,
           year.fillout = min(MODEL_BASE_YEARS),
           laborforce = socioeconomics.DEFAULT_LABORFORCE) ->
      L223.LaborForceFillout_kor_grid


    # PART 3: THE STATES
    # All tables for which processing is identical are done by a function.
    # This applies to the supplysectors, subsectors, and stub tech characteristics of the states.
    process_country_to_states <- function(data) {
      state <- region <- grid_region <- subsector <- market.name <-
        minicam.energy.input <- NULL  # silence package check notes

      data_new <- data %>%
        filter(region == gcamkorea.REGION) %>%
        write_to_all_states(names(data), region_states=gcamkorea.STATES)

      if("subsector" %in% names(data_new)) {
        data_new <- data_new %>%
          filter(!paste(region, subsector) %in% geo_states_noresource)
      }

      # Re-set markets from USA to regional markets, if called for in the gcam-korea assumptions for selected fuels
      if(gcamkorea.USE_REGIONAL_FUEL_MARKETS & "market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          left_join_error_no_match(select(states_subregions,state, grid_region), by = c("region" = "state")) %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamkorea.REGIONAL_FUEL_MARKETS,
                                       grid_region[minicam.energy.input %in% gcamkorea.REGIONAL_FUEL_MARKETS])) %>%
          select(-grid_region)
      }

      data_new
    }

    # minyoung
    # exclude intp rule of nuclear bcz of the fixed output
    L223.SubsectorInterp_elec %>% filter(subsector != "nuclear") -> L223.SubsectorInterp_elec
    L223.StubTech_elec %>% filter(subsector != "nuclear") -> L223.StubTech_elec
    L223.SubsectorInterpTo_elec %>% filter(subsector != "nuclear") -> L223.SubsectorInterpTo_elec

    process_country_to_states(L223.Supplysector_elec) -> L223.Supplysector_elec_korea
    process_country_to_states(L223.ElecReserve) -> L223.ElecReserve_korea
    process_country_to_states(L223.SubsectorLogit_elec) -> L223.SubsectorLogit_elec_korea
    process_country_to_states(L223.SubsectorShrwtFllt_elec) -> L223.SubsectorShrwtFllt_elec_korea
    #process_country_to_states(L223.SubsectorShrwt_nuc) -> L223.SubsectorShrwt_nuc_korea
    process_country_to_states(L223.SubsectorShrwt_renew) -> L223.SubsectorShrwt_renew_korea
    process_country_to_states(L223.SubsectorInterp_elec) -> L223.SubsectorInterp_elec_korea
    process_country_to_states(L223.SubsectorInterpTo_elec) -> L223.SubsectorInterpTo_elec_korea
    process_country_to_states(L223.StubTech_elec) -> L223.StubTech_elec_korea
    process_country_to_states(L223.StubTechEff_elec) -> L223.StubTechEff_elec_korea
    process_country_to_states(L223.StubTechCapFactor_elec) -> L223.StubTechCapFactor_elec_korea

    # change coal shwrt interp rule

    removeIndex <- which(L223.SubsectorInterpTo_elec_korea$subsector %in%
                           cbind(unique(A23.subsectorFixedInterp$subsector),unique(A23.subsectorInterpTo$subsector)))
    L223.SubsectorInterpTo_elec_korea <- L223.SubsectorInterpTo_elec_korea[-removeIndex,]

    L223.SubsectorInterpTo_elec_korea<- rbind (L223.SubsectorInterpTo_elec_korea, A23.subsectorInterpTo)
    L223.SubsectorInterp_elec_korea <- rbind(L223.SubsectorInterp_elec_korea, A23.subsectorFixedInterp)


    # NOTE: Modify the share-weight path for nuclear to include state preferences
    L1231.out_EJ_korea_elec_F_tech %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      group_by(state) %>%
      summarise(elec = sum(value)) %>%
      ungroup ->
      L223.out_EJ_state_elec

    # minyoung
    # nuclear is changed to the fixed output, so no need to use share weight.
    # L1231.out_EJ_korea_elec_F_tech %>%
    #   filter(fuel == "nuclear", year == max(HISTORICAL_YEARS)) %>%
    #   left_join_error_no_match(L223.out_EJ_state_elec, by = "state") %>%
    #   mutate(share = value / elec,
    #          avg.share = sum(value) / sum(elec),
    #          pref = share / avg.share) %>%
    #   select(state, pref) %>%
    #   # Just set some bounds on the share weight multiplier
    #   mutate(share.weight.mult = pref,
    #          share.weight.mult = replace(share.weight.mult, share.weight.mult < 0.1, 0.1),
    #          share.weight.mult = replace(share.weight.mult, share.weight.mult > 2, 2),
    #          # Set the state of VT to zero because VT has already shut down the only nuclear plant
    #          # which used to account for about 70% of generation
    #          share.weight.mult = replace(share.weight.mult, state == "VT", 0)) ->
    #   L223.state_nuc_pref
    #
    # L223.SubsectorShrwt_nuc_korea %>%
    #   left_join_error_no_match(L223.state_nuc_pref, by = c("region" = "state")) %>%
    #   mutate(share.weight = round(share.weight * share.weight.mult, digits = energy.DIGITS_COST)) %>%
    #   select(-pref, -share.weight.mult) ->
    #   L223.SubsectorShrwt_nuc_korea

    # Stub technology information for state electricity generation
    # calibration
    L1231.in_EJ_korea_elec_F_tech %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, digits = energy.DIGITS_CALOUTPUT),
             region = state) %>%
      left_join_error_no_match(select(calibrated_techs, -minicam.energy.input, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      mutate(stub.technology = technology) %>%
      filter(calibration == "input") ->
      L223.in_EJ_state_elec_F_tech

    # NOTE: Fixed output is assumed to apply in all historical years, regardless of final calibration year
    L1231.out_EJ_korea_elec_F_tech %>%
      filter(year %in% MODEL_YEARS, year %in% HISTORICAL_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT),
             region = state) %>%
      left_join_error_no_match(select(calibrated_techs, -minicam.energy.input, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      mutate(stub.technology = technology) ->
      L223.out_EJ_state_elec_F_tech

    L223.out_EJ_state_elec_F_tech %>%
      filter(calibration == "fixed output") ->
      L223.fixout_EJ_state_elec_F_tech

    L223.out_EJ_state_elec_F_tech %>%
      filter(calibration != "fixed output") ->
      L223.calout_EJ_state_elec_F_tech

    # L223.StubTechFixOut_elec_korea: fixed output of electricity generation technologies
    L223.fixout_EJ_state_elec_F_tech %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], calOutputValue) %>%
      mutate(fixedOutput = round(calOutputValue, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = 0,
             tech.share.weight = 0) %>%
      select(-calOutputValue) ->
      L223.StubTechFixOut_elec_korea

    # Add in future hydropower generation here
    # L223.StubTechFixOut_hydro_korea: fixed output of future hydropower
    # NOTE: This just holds it constant for now;
    # at some point, should downscale of the (almost completely flat) nation-level projection
    L223.StubTechFixOut_elec_korea %>%
      filter(grepl("hydro", stub.technology), year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) ->
      L223.StubTechFixOut_hydro_korea

    # minyoung
    # the values was externally calculated if year is in MODEL_FUTURE_YEARS
    # check out raw data/nuclear_gen.R
    L223.StubTechFixOut_elec_korea %>%
      filter( stub.technology=="Gen_II_LWR_kor", year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      left_join(nuclear_fixedOutput, by=c("region", "year")) %>%
      mutate(fixedOutput=value) %>%
      select(colnames(L223.StubTechFixOut_elec_korea))->
      L223.StubTechFixOut_nuclear_korea

    # L223.StubTechProd_elec_korea: calibrated output of electricity generation technologies
    L223.calout_EJ_state_elec_F_tech %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], calOutputValue) %>%
      mutate(share.weight.year = year) %>%
      set_subsector_shrwt %>%
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      filter(!paste(region, subsector) %in% geo_states_noresource) ->
      L223.StubTechProd_elec_korea


    # L223.StubTechMarket_elec_korea: market names of inputs to state electricity sectors
    L223.StubTech_elec_korea %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # For rooftop_pv (technology), match in distributed_solar instead of backup_electricity (minicam.energy.input)
      left_join_keep_first_only(select(A23.globaltech_eff, supplysector, subsector, technology, minicam.energy.input),
                                by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      # Remove NA rows for hydro
      na.omit %>%
      mutate(market.name = gcamkorea.REGION,
             market.name = replace(market.name,
                                   minicam.energy.input %in% c(gcamkorea.STATE_RENEWABLE_RESOURCES, gcamkorea.STATE_UNLIMITED_RESOURCES),
                                   region[minicam.energy.input %in% c(gcamkorea.STATE_RENEWABLE_RESOURCES, gcamkorea.STATE_UNLIMITED_RESOURCES)])) %>%
      filter(!paste(region, subsector) %in% geo_states_noresource) ->
      L223.StubTechMarket_elec_korea

    if(gcamkorea.USE_REGIONAL_ELEC_MARKETS) {
      L223.StubTechMarket_elec_korea %>%
        left_join_error_no_match(select(states_subregions, grid_region, state), by = c("region" = "state")) %>%
        mutate(market.name = replace(market.name, minicam.energy.input %in% gcamkorea.REGIONAL_FUEL_MARKETS,
                                     grid_region[minicam.energy.input %in% gcamkorea.REGIONAL_FUEL_MARKETS])) %>%
        select(-grid_region) ->
        L223.StubTechMarket_elec_korea
    }

    # L223.StubTechMarket_backup_korea: market names of backup inputs to state electricity sectors
    L223.GlobalIntTechBackup_elec %>%
      mutate(supplysector = sector.name, subsector = subsector.name) %>%
      write_to_all_states(names = c(names(.), 'region'), region_states=gcamkorea.STATES) %>%
      mutate(market.name = gcamkorea.REGION, stub.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) ->
      L223.StubTechMarket_backup_korea

    # L223.StubTechElecMarket_backup_korea: market name of electricity sector for backup calculations
    # The backup electric market is only set here if regional electricity markets are not used (i.e. one national grid)
    if(!gcamkorea.USE_REGIONAL_ELEC_MARKETS) {
      L223.StubTechMarket_backup_korea %>%
        select(LEVEL2_DATA_NAMES[["StubTechYr"]]) %>%
        mutate(electric.sector.market = gcamkorea.REGION) ->
        L223.StubTechElecMarket_backup_korea
    }

    # L223.StubTechCapFactor_elec_wind_korea: capacity factors for wind electricity in the states
    # Just use the subsector for matching - technologies include storage technologies as well
    L114.CapacityFactor_wind_korea %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector),
                               by = c("sector", "fuel")) ->
      L223.CapacityFactor_wind_state

    L223.StubTechCapFactor_elec %>%
      filter(region == gcamkorea.REGION) %>%
      semi_join(L223.CapacityFactor_wind_state, by = c("supplysector", "subsector")) %>%
      select(-region, -capacity.factor) %>%
      write_to_all_states(names = c(names(.), "region"), region_states=gcamkorea.STATES) %>%
      left_join_error_no_match(L223.CapacityFactor_wind_state,
                               by = c("region" = "state", "supplysector", "subsector")) %>%
      mutate(capacity.factor = round(capacity.factor, digits = energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]]) ->
      L223.StubTechCapFactor_elec_wind_korea

    # L223.StubTechCapFactor_elec_solar_korea: capacity factors by state and solar electric technology
    L119.CapFacScaler_PV_korea %>%
      bind_rows(L119.CapFacScaler_CSP_korea) %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology),
                               by = c("sector", "fuel")) ->
      L223.CapFacScaler_solar_state

    # Just use the subsector for matching - technologies include storage technologies as well
    L223.StubTechCapFactor_elec %>%
      filter(region == gcamkorea.REGION) %>%
      semi_join(L223.CapFacScaler_solar_state, by = c("supplysector", "subsector")) %>%
      select(-region) %>%
      write_to_all_states(., c(names(.), "region"), region_states=gcamkorea.STATES) %>%
      # For matching capacity factors to technologies, create a variable (tech) that matches what's in the capacity factor table
      mutate(tech = sub("_storage", "", stub.technology)) %>%
      left_join_error_no_match(L223.CapFacScaler_solar_state,
                               by = c("region" = "state", "supplysector", "subsector", "tech" = "technology")) %>%
      mutate(capacity.factor = round(capacity.factor * scaler, digits = energy.DIGITS_COST)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]]) ->
      L223.StubTechCapFactor_elec_solar_korea

    # Produce outputs

    if(exists("L223.DeleteSubsector_kor_elec_market")) {
      L223.DeleteSubsector_kor_elec_market %>%
        add_title("Define the electricity sector(s) in the USA region") %>%
        add_units("NA") %>%
        add_comments("The file is generated if to resolve electricity demands at the US national level") %>%
        add_comments("The subsectors of the existing USA electricity sector are deleted") %>%
        add_legacy_name("L223.DeleteSubsector_kor_elec_market") %>%
        add_precursors("L223.SubsectorLogit_elec") ->
        L223.DeleteSubsector_kor_elec_market
    } else {
      # If gcamkorea.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.DeleteSubsector_kor_elec_market") ->
        L223.DeleteSubsector_kor_elec_market
    }

    if(exists("L223.Supplysector_kor_elec_market")) {
      L223.Supplysector_kor_elec_market %>%
        add_title("Supplysector for electricity sector in the USA region") %>%
        add_units("NA") %>%
        add_comments("The file is generated if resolving electricity demands at the US national level") %>%
        add_comments("All of the supplysector information is the same as before") %>%
        add_comments("except including logit exponent between grid regions")
        add_legacy_name("L223.Supplysector_kor_elec_market") ->
        L223.Supplysector_kor_elec_market
    } else {
      # If gcamkorea.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.Supplysector_kor_elec_market") ->
        L223.Supplysector_kor_elec_market
    }

    if(exists("L223.SubsectorShrwtFllt_kor_elec_market")) {
      L223.SubsectorShrwtFllt_kor_elec_market %>%
        add_title("Subsector (grid region) share-weights in USA electricity") %>%
        add_units("Unitless") %>%
        add_comments("The file is generated if resolving electricity demands at the US national level") %>%
        add_legacy_name("L223.SubsectorShrwtFllt_kor_elec_market") %>%
        add_precursors("gcam-korea/states_subregions") ->
        L223.SubsectorShrwtFllt_kor_elec_market
    } else {
      # If gcamkorea.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.SubsectorShrwtFllt_kor_elec_market") ->
        L223.SubsectorShrwtFllt_kor_elec_market
    }

    if(exists("L223.SubsectorInterp_kor_elec_market")) {
      L223.SubsectorInterp_kor_elec_market %>%
        add_title("Table headers for temporal interpolation of subsector (grid region) share-weights") %>%
        add_units("Unitless") %>%
        add_comments("The file is generated if resolving electricity demands at the US national level") %>%
        add_legacy_name("L223.SubsectorInterp_kor_elec_market") %>%
        same_precursors_as("L223.SubsectorShrwtFllt_kor_elec_market") ->
        L223.SubsectorInterp_kor_elec_market
    } else {
      # If gcamkorea.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.SubsectorInterp_kor_elec_market") ->
        L223.SubsectorInterp_kor_elec_market
    }

    if(exists("L223.SubsectorLogit_kor_elec_market")) {
      L223.SubsectorLogit_kor_elec_market %>%
        add_title("Logit exponent of subsectors (grid regions) in USA electricity") %>%
        add_units("Unitless") %>%
        add_comments("The file is generated if resolving electricity demands at the US national level") %>%
        add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
        add_legacy_name("L223.SubsectorLogit_kor_elec_market") %>%
        same_precursors_as("L223.SubsectorShrwtFllt_kor_elec_market") ->
        L223.SubsectorLogit_kor_elec_market
    } else {
      # If gcamkorea.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.SubsectorLogit_kor_elec_market") ->
        L223.SubsectorLogit_kor_elec_market
    }

    if(exists("L223.TechShrwt_kor_elec_market")) {
      L223.TechShrwt_kor_elec_market %>%
        add_title("Technology share-weights in the USA region") %>%
        add_units("Unitless") %>%
        add_comments("The file is generated if resolving electricity demands at the US national level") %>%
        add_legacy_name("L223.TechShrwt_kor_elec_market") %>%
        same_precursors_as("L223.SubsectorShrwtFllt_kor_elec_market") ->
        L223.TechShrwt_kor_elec_market
    } else {
      # If gcamkorea.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.TechShrwt_kor_elec_market") ->
        L223.TechShrwt_kor_elec_market
    }

    if(exists("L223.TechCoef_kor_elec_market")) {
      L223.TechCoef_kor_elec_market %>%
        add_title("Technology coefficients and market names in the USA region") %>%
        add_units("Unitless") %>%
        add_comments("The file is generated if resolving electricity demands at the US national level") %>%
        add_legacy_name("L223.TechCoef_kor_elec_market") %>%
        same_precursors_as("L223.TechShrwt_kor_elec_market") ->
        L223.TechCoef_kor_elec_market
    } else {
      # If gcamkorea.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.TechCoef_kor_elec_market") ->
        L223.TechCoef_kor_elec_market
    }

    if(exists("L223.Production_kor_elec_market")) {
      L223.Production_kor_elec_market %>%
        add_title("Calibration electricity production in the USA region") %>%
        add_units("EJ") %>%
        add_comments("The file is generated if resolving electricity demands at the US national level") %>%
        add_legacy_name("L223.Production_kor_elec_market") %>%
        add_precursors("L1232.out_EJ_sR_elec_korea",
                       "gcam-korea/calibrated_techs") ->
        L223.Production_kor_elec_market
    } else {
      # If gcamkorea.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.Production_kor_elec_market") %>%
        add_precursors("L1232.out_EJ_sR_elec_korea") ->
        L223.Production_kor_elec_market
    }

    L223.Supplysector_elec_kor_grid %>%
      add_title("Supplysector information for electricity sector in the grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include logit exponent between states") %>%
      add_comments("Use the same logit exponent for states within FERC region as for FERC regions within the USA") %>%
      add_legacy_name("L223.Supplysector_elec_kor_grid") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L223.Supplysector_elec_kor_grid

    L223.SubsectorShrwtFllt_elec_kor_grid %>%
      add_title("Subsector (state) share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Set share-weights for states within grid region") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec_kor_grid") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L223.SubsectorShrwtFllt_elec_kor_grid

    L223.SubsectorInterp_elec_kor_grid %>%
      add_title("Table header for temporal interpolation of subsector (state) share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Set up temporal interterpolation of state share-weights within grid region") %>%
      add_legacy_name("L223.SubsectorInterp_elec_kor_grid") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_kor_grid") ->
      L223.SubsectorInterp_elec_kor_grid

    L223.SubsectorLogit_elec_kor_grid %>%
      add_title("Logit exponent of subsector (states) in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
      add_legacy_name("L223.SubsectorLogit_elec_kor_grid") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_kor_grid") ->
      L223.SubsectorLogit_elec_kor_grid

    L223.TechShrwt_elec_kor_grid %>%
      add_title("Technology share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Technology is the same as subsector within grid region") %>%
      add_legacy_name("L223.TechShrwt_elec_kor_grid") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_kor_grid") ->
      L223.TechShrwt_elec_kor_grid

    L223.TechCoef_elec_kor_grid %>%
      add_title("Technology coefficients and market names in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Technology is the same as subsector within grid region") %>%
      add_comments("Market name is state name") %>%
      add_legacy_name("L223.TechCoef_elec_kor_grid") %>%
      same_precursors_as("L223.TechShrwt_elec_kor_grid") %>%
      add_precursors("gcam-korea/A23.TechCoef_elec_grid")->
      L223.TechCoef_elec_kor_grid

    L223.PassthroughSector_elec_korea %>%
      add_title("Passthrough sector of the states") %>%
      add_units("Unitless") %>%
      add_comments("The marginal revenue sector is the region's electricity sector.") %>%
      add_comments("The marginal revenue market is the grid region.") %>%
      add_legacy_name("L223.PassthroughSector_elec_korea") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L223.PassthroughSector_elec_korea

    L223.PassthroughTech_elec_kor_grid %>%
      add_title("Passthrough technology of the grid regions") %>%
      add_units("Unitless") %>%
      add_comments("This contains region, supplysector, subsector, technology for the grid regions") %>%
      add_comments("to which electricity produced in states is passed through") %>%
      add_legacy_name("L223.PassthroughTech_elec_kor_grid") %>%
      same_precursors_as("L223.TechShrwt_elec_kor_grid") ->
      L223.PassthroughTech_elec_kor_grid

    L223.Production_elec_kor_grid %>%
      add_title("Calibrated electricity production of subsectors in grid regions") %>%
      add_units("EJ") %>%
      add_comments("Subsector share-weight is zero if production of all technologies in the subsector is zero") %>%
      add_comments("Technology share-weight is zero if production of the technology is zero") %>%
      add_legacy_name("L223.Production_elec_kor_grid") %>%
      add_precursors("L1231.out_EJ_korea_elec_F_tech",
                     "gcam-korea/calibrated_techs") ->
      L223.Production_elec_kor_grid

    L223.InterestRate_kor_grid %>%
      add_title("Interest rates in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default interest rate") %>%
      add_legacy_name("L223.InterestRate_kor_grid") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L223.InterestRate_kor_grid

    L223.Pop_kor_grid %>%
      add_title("Population in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("The same value is copied to all model years") %>%
      add_legacy_name("L223.Pop_kor_grid") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L223.Pop_kor_grid

    L223.BaseGDP_kor_grid %>%
      add_title("Base GDP in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_legacy_name("L223.BaseGDP_kor_grid") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L223.BaseGDP_kor_grid

    L223.LaborForceFillout_kor_grid %>%
      add_title("Labor force in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default labor force") %>%
      add_legacy_name("L223.LaborForceFillout_kor_grid") %>%
      add_precursors("gcam-korea/states_subregions") ->
      L223.LaborForceFillout_kor_grid

    L223.Supplysector_elec_korea %>%
      add_title("Supplysector information of electricity sector in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L223.Supplysector_elec_korea") %>%
      add_precursors("L223.Supplysector_elec",
                     "gcam-korea/korea_re_technical_potential") ->
      L223.Supplysector_elec_korea

    L223.ElecReserve_korea %>%
      add_title("Electricity reserve margin and average grid capacity factor in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L223.ElecReserve_korea") %>%
      add_precursors("L223.ElecReserve",
                     "gcam-korea/korea_re_technical_potential") ->
      L223.ElecReserve_korea

    L223.SubsectorLogit_elec_korea %>%
      add_title("Logit exponent of subsectors (fuels) in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorLogit_elec_korea") %>%
      add_precursors("L223.SubsectorLogit_elec",
                     "gcam-korea/korea_re_technical_potential") ->
      L223.SubsectorLogit_elec_korea

    L223.SubsectorShrwtFllt_elec_korea %>%
      add_title("Subsector (fuel) share-weights in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec_korea") %>%
      add_precursors("L223.SubsectorShrwtFllt_elec") ->
      L223.SubsectorShrwtFllt_elec_korea

    # L223.SubsectorShrwt_nuc_korea %>%
    #   add_title("Share-weights for nuclear in the states") %>%
    #   add_units("Unitless") %>%
    #   add_comments("The same USA region values are repeated for each state") %>%
    #   add_comments("Modify the share-weight path for nuclear to include state preferences") %>%
    #   add_legacy_name("L223.SubsectorShrwt_nuc_korea") %>%
    #   add_precursors("L223.SubsectorShrwt_nuc",
    #                  "L1231.out_EJ_korea_elec_F_tech",
    #                  "gcam-korea/korea_re_technical_potential") ->
    #   L223.SubsectorShrwt_nuc_korea

    L223.SubsectorShrwt_renew_korea %>%
      add_title("Share-weights for renewable energy in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorShrwt_renew_korea") %>%
      add_precursors("L223.SubsectorShrwt_renew",
                     "gcam-korea/korea_re_technical_potential") ->
      L223.SubsectorShrwt_renew_korea

    L223.SubsectorInterp_elec_korea %>%
      add_title("Temporal (2100) interpolation of subsectors (fuels) in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorInterp_elec_korea") %>%
      add_precursors("L223.SubsectorInterp_elec",
                     "gcam-korea/korea_re_technical_potential",
                     "gcam-korea/A23.subsectorFixedInterp") ->
      L223.SubsectorInterp_elec_korea

    L223.SubsectorInterpTo_elec_korea %>%
      add_title("Temporal (2300) interpolation of subsectors (fuels) in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorInterpTo_elec_korea") %>%
      add_precursors("L223.SubsectorInterpTo_elec",
                     "gcam-korea/A23.subsectorInterpTo",
                     "gcam-korea/korea_re_technical_potential") ->
      L223.SubsectorInterpTo_elec_korea

    L223.StubTech_elec_korea %>%
      add_title("Stub technology information for electricity generation in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.StubTech_elec_korea") %>%
      add_precursors("L223.StubTech_elec",
                     "gcam-korea/korea_re_technical_potential") ->
      L223.StubTech_elec_korea

    L223.StubTechEff_elec_korea %>%
      add_title("Stub technology efficiency for electricity generation in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("Re-set markets from USA to regional grid markets for selected fuels") %>%
      add_legacy_name("L223.StubTechEff_elec_korea") %>%
      add_precursors("L223.StubTechEff_elec",
                     "gcam-korea/korea_re_technical_potential") ->
      L223.StubTechEff_elec_korea

    L223.StubTechCapFactor_elec_korea %>%
      add_title("Capacity factor of stub technology for electricity generation in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_korea") %>%
      add_precursors("L223.StubTechCapFactor_elec",
                     "gcam-korea/korea_re_technical_potential") ->
      L223.StubTechCapFactor_elec_korea

    L223.StubTechFixOut_elec_korea %>%
      add_title("Fixed outputs of stub technology electricity generation in the states") %>%
      add_units("EJ") %>%
      add_comments("Applied to historical model years") %>%
      add_legacy_name("L223.StubTechFixOut_elec_korea") %>%
      add_precursors("L1231.out_EJ_korea_elec_F_tech",
                     "gcam-korea/calibrated_techs",
                     "gcam-korea/nuclear_fixedOutput") ->
      L223.StubTechFixOut_elec_korea

    L223.StubTechFixOut_hydro_korea %>%
      add_title("Fixed outputs of future hydropower electricity generation in the states") %>%
      add_units("EJ") %>%
      add_comments("This just holds it constant for now.") %>%
      add_legacy_name("L223.StubTechFixOut_hydro_korea") %>%
      same_precursors_as("L223.StubTechFixOut_elec_korea") ->
      L223.StubTechFixOut_hydro_korea

    L223.StubTechFixOut_nuclear_korea %>%
      add_title("Fixed outputs of future hydropower electricity generation in the states") %>%
      add_units("EJ") %>%
      add_comments("This just holds it constant for now.") %>%
      add_legacy_name("L223.StubTechFixOut_nuclear_korea") %>%
      same_precursors_as("L223.StubTechFixOut_elec_korea") ->
      L223.StubTechFixOut_nuclear_korea

    L223.StubTechProd_elec_korea %>%
      add_title("Calibrated outputs of electricity stub technology in the states") %>%
      add_units("EJ") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechProd_elec_korea") %>%
      add_precursors("L1231.in_EJ_korea_elec_F_tech",
                     "L1231.out_EJ_korea_elec_F_tech",
                     "gcam-korea/calibrated_techs",
                     "gcam-korea/korea_re_technical_potential") ->
      L223.StubTechProd_elec_korea

    L223.StubTechMarket_elec_korea %>%
      add_title("Market names of inputs to state electricity sectors") %>%
      add_units("Unitless") %>%
      add_comments("Re-set markets from USA to regional grid markets for selected fuels") %>%
      add_legacy_name("L223.StubTechMarket_elec_korea") %>%
      same_precursors_as("L223.StubTech_elec_korea") %>%
      add_precursors("energy/A23.globaltech_eff",
                     "gcam-korea/korea_re_technical_potential") ->
      L223.StubTechMarket_elec_korea

    L223.StubTechMarket_backup_korea %>%
      add_title("Market names of backup inputs to state electricity sectors") %>%
      add_units("Unitless") %>%
      add_comments("Set market as USA") %>%
      add_legacy_name("L223.StubTechMarket_backup_korea") %>%
      add_precursors("L223.GlobalIntTechBackup_elec",
                     "gcam-korea/states_subregions") ->
      L223.StubTechMarket_backup_korea

    if(exists("L223.StubTechElecMarket_backup_korea")) {
      L223.StubTechElecMarket_backup_korea %>%
        add_title("Market name of electricity sector for backup calculations") %>%
        add_units("Unitless") %>%
        add_comments("The backup electric market is only set here if regional electricity markets are not used") %>%
        add_legacy_name("L223.StubTechElecMarket_backup_korea") %>%
        same_precursors_as("L223.StubTechMarket_backup_korea") ->
        L223.StubTechElecMarket_backup_korea
    } else {
      # If regional electricity markets are not used,
      # then a blank tibble of the backup electric market is produced.
      missing_data() %>%
        add_legacy_name("L223.StubTechElecMarket_backup_korea") ->
        L223.StubTechElecMarket_backup_korea
    }

    L223.StubTechCapFactor_elec_wind_korea %>%
      add_title("Capacity factors for wind electricity in the states") %>%
      add_units("Unitless") %>%
      add_comments("Include storage technologies as well") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_wind_korea") %>%
      add_precursors("L114.CapacityFactor_wind_korea",
                     "gcam-korea/calibrated_techs",
                     "gcam-korea/states_subregions") ->
      L223.StubTechCapFactor_elec_wind_korea

    L223.StubTechCapFactor_elec_solar_korea %>%
      add_title("Capacity factors for solar electricity in the states") %>%
      add_units("Unitless") %>%
      add_comments("Include storage technologies as well") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_solar_korea") %>%
      add_precursors("L119.CapFacScaler_PV_korea",
                     "L119.CapFacScaler_CSP_korea",
                     "gcam-korea/calibrated_techs",
                     "gcam-korea/states_subregions") ->
      L223.StubTechCapFactor_elec_solar_korea

    return_data(L223.DeleteSubsector_kor_elec_market,
                L223.Supplysector_kor_elec_market,
                L223.SubsectorShrwtFllt_kor_elec_market,
                L223.SubsectorInterp_kor_elec_market,
                L223.SubsectorLogit_kor_elec_market,
                L223.TechShrwt_kor_elec_market,
                L223.TechCoef_kor_elec_market,
                L223.Production_kor_elec_market,
                L223.PassthroughSector_elec_korea,
                L223.PassthroughTech_elec_kor_grid,
                L223.Supplysector_elec_kor_grid,
                L223.SubsectorShrwtFllt_elec_kor_grid,
                L223.SubsectorInterp_elec_kor_grid,
                L223.SubsectorLogit_elec_kor_grid,
                L223.TechShrwt_elec_kor_grid,
                L223.TechCoef_elec_kor_grid,
                L223.Production_elec_kor_grid,
                L223.InterestRate_kor_grid,
                L223.Pop_kor_grid,
                L223.BaseGDP_kor_grid,
                L223.LaborForceFillout_kor_grid,
                L223.Supplysector_elec_korea,
                L223.ElecReserve_korea,
                L223.SubsectorLogit_elec_korea,
                L223.SubsectorShrwtFllt_elec_korea,
                #L223.SubsectorShrwt_nuc_korea,
                L223.SubsectorShrwt_renew_korea,
                L223.SubsectorInterp_elec_korea,
                L223.SubsectorInterpTo_elec_korea,
                L223.StubTech_elec_korea,
                L223.StubTechEff_elec_korea,
                L223.StubTechCapFactor_elec_korea,
                L223.StubTechFixOut_elec_korea,
                L223.StubTechFixOut_hydro_korea,
                L223.StubTechFixOut_nuclear_korea,
                L223.StubTechProd_elec_korea,
                L223.StubTechMarket_elec_korea,
                L223.StubTechMarket_backup_korea,
                L223.StubTechElecMarket_backup_korea,
                L223.StubTechCapFactor_elec_wind_korea,
                L223.StubTechCapFactor_elec_solar_korea)
  } else {
    stop("Unknown command")
  }
}
