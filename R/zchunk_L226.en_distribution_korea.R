#' module_gcam.korea_L226.en_distribution_korea
#'
#' Create a variety of energy and electricity outputs at the state and/or grid_region level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' \itemize{
#' \item{\code{L226.DeleteSupplysector_kor_elec_market}: Removing the electricity T&D sectors of the Korea region.}
#' \item{\code{L226.StubTechCoef_electd_korea}: Stub technology coefficients elec T&D when using national elec markets. State elect_td sectors are treated as stub technologies.}
#' \item{\code{L226.TechShrwt_electd_korea}: tech share weights for elec T&D when using regional electricity markets. The elect_td sectors can not use the global tech database as their input is different.}
#' \item{\code{L226.TechCost_electd_korea}: Tech costs for elec T&D when using regional electricity markets.}
#' \item{\code{L226.TechCoef_electd_korea}: Tech coeff for elec T&D when using regional electricity markets.}
#' \item{\code{L226.Supplysector_en_korea}: Supply sector information for energy handling and delivery sectors for Korea grid regions. Currently using FERC regions as a proxy for regional energy markets.}
#' \item{\code{L226.SubsectorShrwtFllt_en_korea}: Subsector shareweights of energy handling and delivery.}
#' \item{\code{L226.SubsectorLogit_en_korea}: Logit info for energy subsectors. There is only one tech per subsector so the logit choice does not matter.}
#' \item{\code{L226.TechShrwt_en_korea}: Technology shareweights of energy handling and delivery. Can't use stub technologies because these would inherit the wrong energy-inputs.}
#' \item{\code{L226.TechCoef_en_korea}: Technology coefficients and market names of energy handling and delivery.}
#' \item{\code{L226.TechCost_en_korea}: Regional price adjustments/cost adders for Korea energy.}
#' \item{\code{L226.Ccoef_korea}: Carbon coef for Korea cost adder sectors.}
#' \item{\code{L226.Supplysector_electd_korea}: Korea supply sector input, output, and logit info for elec T&D by state.}
#' \item{\code{L226.SubsectorLogit_electd_korea}: Korea subsector logit info for elec T&D by grid_region.}
#' \item{\code{L226.SubsectorShrwtFllt_electd_korea}: Korea subsector shareweight fillout for elec T&D by state.}
#' \item{\code{L226.SubsectorInterp_electd_korea}: Korea interpolation info for elec T&D by state.}
#' }
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author S. Jeon
module_gcam.korea_L226.en_distribution_korea <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             FILE = "energy/A21.sector",
             FILE = "energy/A26.sector",
             FILE = "gcam-korea/korea_energy_prices",
             "L202.CarbonCoef",
             "L226.Supplysector_en",
             "L226.SubsectorLogit_en",
             "L226.SubsectorShrwtFllt_en",
             "L226.SubsectorInterp_en",
             "L226.GlobalTechCost_en",
             "L226.GlobalTechShrwt_en",
             "L226.StubTechCoef_electd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L226.DeleteSupplysector_kor_elec_market",
             "L226.StubTechCoef_electd_korea",
             "L226.TechShrwt_electd_korea",
             "L226.TechCost_electd_korea",
             "L226.TechCoef_electd_korea",
             "L226.Supplysector_electd_korea",
             "L226.SubsectorLogit_electd_korea",
             "L226.SubsectorShrwtFllt_electd_korea",
             "L226.SubsectorInterp_electd_korea",
             "L226.Supplysector_en_korea",
             "L226.SubsectorShrwtFllt_en_korea",
             "L226.SubsectorLogit_en_korea",
             "L226.TechShrwt_en_korea",
             "L226.TechCoef_en_korea",
             "L226.TechCost_en_korea",
             "L226.Ccoef_korea"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions")
    A21.sector <- get_data(all_data, "energy/A21.sector")
    A26.sector <- get_data(all_data, "energy/A26.sector")
    korea_energy_prices <- get_data(all_data, "gcam-korea/korea_energy_prices")
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef")
    L226.Supplysector_en <- get_data(all_data, "L226.Supplysector_en")
    L226.SubsectorLogit_en <- get_data(all_data, "L226.SubsectorLogit_en")
    L226.SubsectorShrwtFllt_en <- get_data(all_data, "L226.SubsectorShrwtFllt_en")
    L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en")
    L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en")
    L226.GlobalTechShrwt_en <- get_data(all_data, "L226.GlobalTechShrwt_en")
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")


    # silence check package notes
    region <- supplysector <- from.year <- to.year <- output.unit <- input.unit <- price.unit <- liq_adj <-
      logit.exponent <- logit.type <- . <- subsector <- State <- Coal <- Natural.gas <- Distillate.fuel.oil <-
      grid_region <- state_name <- coal_adj <- gas_adj <- liq_adju <- sector1 <- adjustment <- technology <-
      year <- minicam.non.energy.input <- tmp <- sector2 <- trash1 <- trash2 <- input.cost <- sector.name <-
      subsector.name <- stub.technology <- market.name <- state <- NULL


    # global_energy_to_korea_electd - takes global energy inputs from L226.en_distribution.R
    # and processes for use in USA electricity T&D
    global_energy_to_korea_electd <- function(data) {
      data %>%
        filter(region == gcamkorea.REGION,
               supplysector %in% gcamkorea.ELECT_TD_SECTORS) %>%
        write_to_all_states(names(data), region_states=gcamkorea.STATES)
    } # end global_energy_to_korea_electd


    # Process inputs:
    L226.SubsectorInterp_en %>%
      mutate(from.year = as.integer(from.year),
             to.year = as.integer(to.year)) ->
      L226.SubsectorInterp_en


    # Build tables

    # Supplysector information

    # PART 1: FUEL HANDLING AND DELIVERY SECTORS

    # L226.Supplysector_en_korea: Supply sector information for energy handling and delivery sectors
    # NOTE: Currently using FERC regions as a proxy for regional energy markets
    A21.sector %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.exponent, logit.type) %>%
      filter(supplysector %in% gcamkorea.REGIONAL_FUEL_MARKETS) ->
      A21.tmp

    A26.sector  %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.exponent, logit.type) %>%
      filter(supplysector %in% gcamkorea.REGIONAL_FUEL_MARKETS) %>%
      bind_rows(A21.tmp) %>%
      repeat_add_columns(tibble(region = unique(states_subregions$grid_region))) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) ->
      L226.Supplysector_en_korea


    # L226.SubsectorShrwtFllt_en_korea: subsector shareweights of energy handling and delivery
    L226.Supplysector_en_korea %>%
      mutate(subsector = supplysector,
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = gcamkorea.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L226.SubsectorShrwtFllt_en_korea


    # L226.SubsectorLogit_en_korea
    # NOTE: There is only one tech per subsector so the logit choice does not matter
    L226.SubsectorShrwtFllt_en_korea %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamusa.DEFAULT_LOGITEXP,
             logit.type = NA) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) ->
      L226.SubsectorLogit_en_korea


    # L226.TechShrwt_en_korea: technology shareweights of energy handling and delivery
    # NOTE: can't use stub technologies because these would inherit the wrong energy-inputs
    L226.SubsectorShrwtFllt_en_korea %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subsector,
             share.weight = gcamkorea.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], "share.weight") ->
      L226.TechShrwt_en_korea


    # L226.TechCoef_en_korea: technology coefficients and market names of energy handling and delivery
    L226.TechShrwt_en_korea %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      mutate(minicam.energy.input = supplysector,
             coefficient = gcamkorea.DEFAULT_COEFFICIENT,
             market.name = gcamkorea.DEFAULT_MARKET) ->
      L226.TechCoef_en_korea


    # L226.CostAdj_75USDGJ_FERC_F: grid region specific cost adders
    # NOTE: the average national costs are already accounted in the corresponding sectors of the USA;
    # this table implements a price adjustment factor.
    #
    # Step 1 to calculate the cost adders:
    # Get US prices for coal, natural gas, distillate fuel oil for use in calculating the adjustments:
    # korea_energy_prices %>%
    #   filter(State == gcamkorea.REGION) %>%
    #   select(Coal, Natural.gas, Distillate.fuel.oil) ->
    #   kor_C_NG_DFO_prices

    # Calculate the adjustment factor:
    # Step 2: Use the US prices calculated in step 1 to compute the
    # adjustment factors = (state price - US price) * some unit conversions.
    # Distillate fuel oil is used as proxy for liquid fuels to avoid composition bias in the petroleum total.
    # In other words, states with a lot of residual fuel would end up having lower apparent liquid fuel prices.
    # In states with missing values for coal, assign the maximum price.
    # # For gas, the value in Hawaii is extremely high; just cap it at a max threshold
    # korea_energy_prices %>%
    #   # save NA for processing
    #   left_join(select(states_subregions, grid_region, state_name), by = c("State" = "state_name")) %>%
    #   mutate(coal_adj = (Coal ) * CONV_BTU_KJ * gdp_deflator(1975, 2009),
    #          gas_adj = (Natural.gas ) * CONV_BTU_KJ * gdp_deflator(1975, 2009),
    #          liq_adj = (Distillate.fuel.oil )* CONV_BTU_KJ  * gdp_deflator(1975, 2009)) ->
    #   EIA_tmp

    # # Step 3: get maximum coal adjustment for replacing NA's:
    # EIA_tmp %>%
    #   select(coal_adj) %>%
    #   na.omit %>%
    #   summarize(coal_adj = max(coal_adj)) %>%
    #   as.double ->
    #   maxCoalAdj

    # Step 4 use to replace NAs from step 2 and finish adjustment calculations by taking the median of each grid_region:
    # EIA_tmp%>%
    #   replace_na(list(coal_adj = maxCoalAdj)) %>%
    #   group_by(grid_region) %>%
    #   summarize(coal_adj = median(coal_adj),
    #             gas_adj = median(gas_adj),
    #             liq_adj = median(liq_adj)) %>%
    #   ungroup %>%
    #   na.omit ->
    #   L226.CostAdj_75USDGJ_FERC_F

     korea_energy_prices %>%
       left_join(select(states_subregions, grid_region, state_name), by = c("State" = "state_name")) %>%
       group_by(grid_region) %>%
       summarize(coal_adj = median(coal),
                 gas_adj = median(gas),
                 liq_adj = median(liquids)) %>%
       ungroup -> korea_energy_prices

    # L226.TechCost_en_korea: cost adders
    L226.TechShrwt_en_korea %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      mutate(minicam.non.energy.input = "regional price adjustment") %>%
      left_join_error_no_match(korea_energy_prices, by = c("region" = "grid_region")) %>%
       rename(coal = coal_adj,
              gas = gas_adj,
              liquids = liq_adj) %>%
      gather(sector1, adjustment, -region, -supplysector, -subsector, -technology, -year, -minicam.non.energy.input) %>%
      mutate(tmp = supplysector,
             tmp = if_else(grepl("refined liquids*", tmp), "refined liquids", tmp)) %>%
      separate(tmp, c("trash1", "sector2"), sep = " ") %>%
      filter(sector1 == sector2) %>%
      select(-trash1, -sector1, -sector2) %>%
      rename(input.cost = adjustment) %>%
      mutate(input.cost = round(input.cost, gcamusa.DIGITS_COST)) ->
      L226.TechCost_en_korea


    # L226.Ccoef_korea: carbon coef for cost adder sectors
    L202.CarbonCoef %>%
      filter(region == gcamkorea.REGION) %>%
      select(-region) ->
      L226.Ccoef_korea.usa

    L226.TechCost_en_korea %>%
      select(region, supplysector) %>%
      distinct %>%
      left_join_error_no_match(L226.Ccoef_korea.usa, by = c("supplysector" = "PrimaryFuelCO2Coef.name")) ->
      L226.Ccoef_korea



    # PART 2: ELECTRICITY TRANSMISSION AND DISTRIBUTION

    # L226.DeleteSupplysector_kor_elec_market: Removing the electricity T&D sectors of the USA region
    # This should probably be converted to an assumption and read in at some point.
    L226.DeleteSupplysector_kor_elec_market <- tibble(region = gcamkorea.REGION, supplysector = gcamkorea.ELECT_TD_SECTORS)


    # Replacing for loop starting on line 152 in old DS.
    # There's also two inputs to this chunk that are NULL, and nothing gets done to: L226.SubsectorShrwt_en, L226.SubsectorInterpTo_en
    L226.Supplysector_en %>%
      global_energy_to_korea_electd() ->
      L226.Supplysector_electd_korea

    L226.SubsectorLogit_en %>%
      global_energy_to_korea_electd() ->
      L226.SubsectorLogit_electd_korea

    L226.SubsectorShrwtFllt_en %>%
      global_energy_to_korea_electd() ->
      L226.SubsectorShrwtFllt_electd_korea

    L226.SubsectorInterp_en %>%
      global_energy_to_korea_electd() ->
      L226.SubsectorInterp_electd_korea


    # Using national electric markets
    if(!gcamusa.USE_REGIONAL_ELEC_MARKETS) {

      # L226.StubTechCoef_electd_korea: Using national elec markets. State elect_td sectors are treated as stub technologies
      L226.StubTechCoef_electd %>%
        global_energy_to_korea_electd() ->
        L226.StubTechCoef_electd_korea

    }


    # Using regional electric markets
    if(gcamusa.USE_REGIONAL_ELEC_MARKETS) {

      # The elect_td sectors can not use the global tech database as their input is different.

      # L226.TechShrwt_electd_korea: Tech share weights for electricity T&D
      L226.GlobalTechShrwt_en %>%
        filter(sector.name %in% gcamkorea.ELECT_TD_SECTORS) %>%
        write_to_all_states(c("region", names(L226.GlobalTechShrwt_en)), region_states=gcamkorea.STATES) %>%
        rename(supplysector = sector.name,
               subsector = subsector.name) ->
        L226.TechShrwt_electd_korea

      # L226.TechCost_electd_korea: Tech costs for electricity T&D
      L226.GlobalTechCost_en %>%
        filter(sector.name %in% gcamkorea.ELECT_TD_SECTORS) %>%
        write_to_all_states(c("region", names(L226.GlobalTechCost_en)), region_states=gcamkorea.STATES) %>%
        rename(supplysector = sector.name,
               subsector = subsector.name) ->
        L226.TechCost_electd_korea

      # L226.TechCoef_electd_korea: Tech coefficients for electricity T&D
      L226.StubTechCoef_electd %>%
        global_energy_to_korea_electd() %>%
        rename(technology = stub.technology) %>%
        mutate(minicam.energy.input = "electricity domestic supply") %>%
        select(-market.name) %>%
        left_join_error_no_match(select(states_subregions, grid_region, state), by = c("region" = "state")) %>%
        rename(market.name = grid_region) ->
        L226.TechCoef_electd_korea
    }


    # Produce outputs
    L226.DeleteSupplysector_kor_elec_market %>%
      add_title("Removing the electricity T&D sectors of the USA region") %>%
      add_units("NA") %>%
      add_comments("Removing the electricity T&D sectors of the USA region") %>%
      add_legacy_name("L226.DeleteSupplysector_kor_elec_market") ->
      L226.DeleteSupplysector_kor_elec_market

    if(exists("L226.StubTechCoef_electd_korea")) {
      L226.StubTechCoef_electd_korea %>%
        add_title("Stub technology coefficients elec T&D when using national elec markets") %>%
        add_units("NA") %>%
        add_comments("Stub technology coefficients elec T&D when using national elec markets.") %>%
        add_comments("State elect_td sectors are treated as stub technologies.") %>%
        add_legacy_name("L226.StubTechCoef_electd_korea") %>%
        add_precursors("L226.StubTechCoef_electd") ->
        L226.StubTechCoef_electd_korea
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is TRUE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L226.StubTechCoef_electd_korea") %>%
        add_precursors("L226.StubTechCoef_electd") ->
        L226.StubTechCoef_electd_korea
    }


    if(exists("L226.TechShrwt_electd_korea")) {
      L226.TechShrwt_electd_korea %>%
        add_title("Tech share weights for elec T&D when using regional electricity markets") %>%
        add_units("NA") %>%
        add_comments("Tech share weights for elec T&D when using regional electricity markets") %>%
        add_comments("The elect_td sectors can not use the global tech database as their input is different.") %>%
        add_legacy_name("L226.TechShrwt_electd_korea") %>%
        add_precursors("L226.GlobalTechShrwt_en") ->
        L226.TechShrwt_electd_korea
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is FALSE,
      # indicating to resolve electricity demands at the national level,
      # then blank tibbles of the grid region level data are produced.
      missing_data() %>%
        add_legacy_name("L226.TechShrwt_electd_korea") %>%
        add_precursors("L226.GlobalTechShrwt_en") ->
        L226.TechShrwt_electd_korea
    }


    if(exists("L226.TechCost_electd_korea")) {
      L226.TechCost_electd_korea %>%
        add_title("Tech costs for elec T&D when using regional electricity markets") %>%
        add_units("1975$") %>%
        add_comments("Tech costs for elec T&D when using regional electricity markets") %>%
        add_comments("The elect_td sectors can not use the global tech database as their input is different.") %>%
        add_legacy_name("L226.TechCost_electd_korea") %>%
        add_precursors("L226.GlobalTechCost_en") ->
        L226.TechCost_electd_korea
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is FALSE,
      # indicating to resolve electricity demands at the national level,
      # then blank tibbles of the grid region level data are produced.
      missing_data() %>%
        add_legacy_name("L226.TechCost_electd_korea") %>%
        add_precursors("L226.GlobalTechCost_en") ->
        L226.TechCost_electd_korea
    }


    if(exists("L226.TechCoef_electd_korea")) {
      L226.TechCoef_electd_korea %>%
        add_title("Tech coefficients for elec T&D when using regional electricity markets") %>%
        add_units("NA") %>%
        add_comments("Tech coeff for elec T&D when using regional electricity markets.") %>%
        add_comments("The elect_td sectors can not use the global tech database as their input is different.") %>%
        add_legacy_name("L226.TechCoef_electd_korea") %>%
        add_precursors("gcam-korea/states_subregions",
                       "L226.StubTechCoef_electd") ->
        L226.TechCoef_electd_korea
    } else {
      # If gcamusa.USE_REGIONAL_ELEC_MARKETS is FALSE,
      # indicating to resolve electricity demands at the national level,
      # then blank tibbles of the grid region level data are produced.
      missing_data() %>%
        add_legacy_name("L226.TechCoef_electd_korea") %>%
        add_precursors("gcam-korea/states_subregions",
                       "L226.StubTechCoef_electd") ->
        L226.TechCoef_electd_korea
    }


    L226.Supplysector_en_korea %>%
      add_title("Supply sector information for energy handling and delivery sectors.") %>%
      add_units("varies") %>%
      add_comments("Supply sector information for energy handling and delivery sectors for USA grid regions.") %>%
      add_comments("Currently using FERC regions as a proxy for regional energy markets.") %>%
      add_legacy_name("L226.Supplysector_en_korea") %>%
      add_precursors("energy/A21.sector",
                     "energy/A26.sector") ->
      L226.Supplysector_en_korea

    L226.SubsectorShrwtFllt_en_korea %>%
      add_title("Subsector shareweights of energy handling and delivery") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweights of energy handling and delivery") %>%
      add_legacy_name("L226.SubsectorShrwtFllt_en_korea") %>%
      same_precursors_as(L226.Supplysector_en_korea) ->
      L226.SubsectorShrwtFllt_en_korea

    L226.SubsectorLogit_en_korea %>%
      add_title("Logit info for energy subsectors") %>%
      add_units("NA") %>%
      add_comments("Logit info for energy subsectors.") %>%
      add_comments("There is only one tech per subsector so the logit choice does not matter.") %>%
      add_legacy_name("L226.SubsectorLogit_en_korea") %>%
      same_precursors_as(L226.SubsectorShrwtFllt_en_korea) ->
      L226.SubsectorLogit_en_korea

    L226.TechShrwt_en_korea %>%
      add_title("Technology shareweights of energy handling and delivery") %>%
      add_units("NA") %>%
      add_comments("Technology shareweights of energy handling and delivery.") %>%
      add_comments("Can't use stub technologies because these would inherit the wrong energy-inputs.") %>%
      add_legacy_name("L226.TechShrwt_en_korea") %>%
      same_precursors_as(L226.SubsectorShrwtFllt_en_korea) ->
      L226.TechShrwt_en_korea

    L226.TechCoef_en_korea %>%
      add_title("Technology coefficients and market names of energy handling and delivery") %>%
      add_units("units") %>%
      add_comments("Technology coefficients and market names of energy handling and delivery") %>%
      add_legacy_name("L226.TechCoef_en_korea") %>%
      same_precursors_as(L226.TechShrwt_en_korea) ->
      L226.TechCoef_en_korea

    L226.TechCost_en_korea %>%
      add_title("Regional price adjustments/cost adders for USA energy.") %>%
      add_units("1975$/GJ") %>%
      add_comments("Regional price adjustments/cost adders for USA energy") %>%
      add_legacy_name("L226.TechCost_en_korea") %>%
      add_precursors("gcam-korea/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-korea/korea_energy_prices") ->
      L226.TechCost_en_korea

    L226.Ccoef_korea %>%
      add_title("Carbon coef for cost adder sectors") %>%
      add_units("NA") %>%
      add_comments("Carbon coef for cost adder sectors") %>%
      add_legacy_name("L226.Ccoef_korea") %>%
      add_precursors("gcam-korea/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-korea/korea_energy_prices",
                     "L202.CarbonCoef") ->
      L226.Ccoef_korea

    L226.Supplysector_electd_korea %>%
      add_title("USA supply sector input, output, and logit info for elec T&D") %>%
      add_units("varies") %>%
      add_comments("USA supply sector input, output, and logit info for elec T&D by state") %>%
      add_legacy_name("L226.Supplysector_electd_korea") %>%
      add_precursors("L226.Supplysector_en") ->
      L226.Supplysector_electd_korea

    L226.SubsectorLogit_electd_korea %>%
      add_title("USA subsector logit info for elec T&D") %>%
      add_units("varies") %>%
      add_comments("USA subsector logit info for elec T&D by grid_region") %>%
      add_legacy_name("L226.SubsectorLogit_electd_korea") %>%
      add_precursors("L226.SubsectorLogit_en") ->
      L226.SubsectorLogit_electd_korea

    L226.SubsectorShrwtFllt_electd_korea %>%
      add_title("USA subsector shareweight fillout for elec T&D") %>%
      add_units("varies") %>%
      add_comments("USA subsector shareweight fillout for elec T&D by state") %>%
      add_legacy_name("L226.SubsectorShrwtFllt_electd_korea") %>%
      add_precursors("L226.SubsectorShrwtFllt_en") ->
      L226.SubsectorShrwtFllt_electd_korea

    L226.SubsectorInterp_electd_korea %>%
      add_title("USA interpolation info for elec T&D") %>%
      add_units("varies") %>%
      add_comments("USA interpolation info for elec T&D by state") %>%
      add_legacy_name("L226.SubsectorInterp_electd_korea") %>%
      add_precursors("L226.SubsectorInterp_en") ->
      L226.SubsectorInterp_electd_korea

    return_data(L226.DeleteSupplysector_kor_elec_market,
                L226.StubTechCoef_electd_korea,
                L226.TechShrwt_electd_korea,
                L226.TechCost_electd_korea,
                L226.TechCoef_electd_korea,
                L226.Supplysector_en_korea,
                L226.SubsectorShrwtFllt_en_korea,
                L226.SubsectorLogit_en_korea,
                L226.TechShrwt_en_korea,
                L226.TechCoef_en_korea,
                L226.TechCost_en_korea,
                L226.Ccoef_korea,
                L226.Supplysector_electd_korea,
                L226.SubsectorLogit_electd_korea,
                L226.SubsectorShrwtFllt_electd_korea,
                L226.SubsectorInterp_electd_korea)
  } else {
    stop("Unknown command")
  }
}
