#' module_gcam.korea_L210.resources_korea
#'
#' gcam-korea resource market information, prices, TechChange parameters, and supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.DeleteRenewRsrc_korea_rsrc}, \code{L210.DeleteUnlimitRsrc_korea_rsrc}, \code{L210.RenewRsrc_korea},
#' \code{L210.UnlimitRsrc_korea}, \code{L210.UnlimitRsrcPrice_korea}, \code{L210.SmthRenewRsrcTechChange_korea},
#' \code{L210.SmthRenewRsrcCurves_wind_korea}, \code{L210.GrdRenewRsrcCurves_geo_korea}, \code{L210.GrdRenewRsrcMax_geo_korea},
#' \code{L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea}, \code{L210.DeleteUnlimitRsrc_korea_limestone},
#' \code{L210.UnlimitRsrc_limestone_korea}, \code{L210.UnlimitRsrcPrice_limestone_korea}. The corresponding file in the
#' original data system was \code{L210.resources.R} (gcam-korea level2).
#' @details gcam-korea resource market information, prices, TechChange parameters, and supply curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author M. Roh

module_gcam.korea_L210.resources_korea <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-korea/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "gcam-korea/korea_re_technical_potential",
             FILE = "gcam-korea/korea_state_wind",
             "L115.rsrc_korea_rooftopPV",
             "L1231.out_EJ_korea_elec_F_tech",
             "L1321.out_Mt_korea_cement_Yh",
             "L210.RenewRsrc",
             "L210.UnlimitRsrc",
             "L210.UnlimitRsrcPrice",
             "L210.SmthRenewRsrcTechChange",
             "L210.SmthRenewRsrcCurves_wind",
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV",
             "L210.GrdRenewRsrcCurves_geo",
             "L210.GrdRenewRsrcMax_geo"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.DeleteRenewRsrc_korea_rsrc", #
             "L210.DeleteUnlimitRsrc_korea_rsrc", #
             "L210.RenewRsrc_korea", #
             "L210.UnlimitRsrc_korea", #
             "L210.UnlimitRsrcPrice_korea", #
             "L210.SmthRenewRsrcTechChange_korea", #
             "L210.SmthRenewRsrcCurves_wind_korea", #
             "L210.GrdRenewRsrcCurves_geo_korea", #
             "L210.GrdRenewRsrcMax_geo_korea", #
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea", #
             "L210.DeleteUnlimitRsrc_korea_limestone", #
             "L210.UnlimitRsrc_limestone_korea", #
             "L210.UnlimitRsrcPrice_limestone_korea")) #
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    Geothermal_Hydrothermal_GWh <- State <- available <- b_exp <- cost_modifier <- curve.exponent <- curve_exponent <-
      extractioncost <- generation <- geothermal <- grade <- grade_share <- maxResource <- maxSubResource <- mid.price <-
      mid_p <- mid_price <- object <- offtake <- offtake_share <- region <- renewresource <- smooth.renewable.subresource <-
      state <- unlimited.resource <- value <- year <- year.fillout <- . <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-korea/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    korea_re_technical_potential <- get_data(all_data, "gcam-korea/korea_re_technical_potential")
    korea_state_wind <- get_data(all_data, "gcam-korea/korea_state_wind")
    L115.rsrc_korea_rooftopPV <- get_data(all_data, "L115.rsrc_korea_rooftopPV")
    L1231.out_EJ_korea_elec_F_tech <- get_data(all_data, "L1231.out_EJ_korea_elec_F_tech")
    L1321.out_Mt_korea_cement_Yh <- get_data(all_data, "L1321.out_Mt_korea_cement_Yh")
    L210.RenewRsrc <- get_data(all_data, "L210.RenewRsrc")
    L210.UnlimitRsrc <- get_data(all_data, "L210.UnlimitRsrc")
    L210.UnlimitRsrcPrice <- get_data(all_data, "L210.UnlimitRsrcPrice")
    L210.SmthRenewRsrcTechChange <- get_data(all_data, "L210.SmthRenewRsrcTechChange")
    L210.SmthRenewRsrcCurves_wind <- get_data(all_data, "L210.SmthRenewRsrcCurves_wind")
    L210.SmthRenewRsrcCurvesGdpElast_roofPV <- get_data(all_data, "L210.SmthRenewRsrcCurvesGdpElast_roofPV")
    L210.GrdRenewRsrcCurves_geo <- get_data(all_data, "L210.GrdRenewRsrcCurves_geo")
    L210.GrdRenewRsrcMax_geo <- get_data(all_data, "L210.GrdRenewRsrcMax_geo")
    # ===================================================
    # States that produce cement
    cement_states <- unique(L1321.out_Mt_korea_cement_Yh$state)

    # NOTE: geothermal resource is not created in the states considered to have zero hydrothermal production available
    korea_re_technical_potential <- korea_re_technical_potential %>%
      # remove TOTAL row
      filter(State != "TOTAL") %>%
      # Add state abbreviation
      left_join_error_no_match(states_subregions, by = c("State" = "state_name")) %>%
      # Convert geothermal to EJ
      mutate(geothermal = Geothermal_Hydrothermal_GWh * CONV_GWH_EJ / gcamkorea.GEOTHERMAL_DEFAULT_EFFICIENCY,
             renewresource = "geothermal") %>%
      select(region = state, renewresource, geothermal)

    # States that do not use geothermal
    geo_states_noresource <- korea_re_technical_potential %>%
      filter(geothermal == 0) %>%
      select(region, renewresource)

    # States that use geothermal
    geo_states <- filter(korea_re_technical_potential, geothermal > 0)$region %>% unique()

    # NOTE: keeping limestone resources separate, written out to XML batch file for cement
    # L210.DeleteRenewRsrc_korea_rsrc: remove selected renewable resources from the USA region
    L210.DeleteRenewRsrc_korea_rsrc <- L210.RenewRsrc %>%
      filter(region == gcamkorea.REGION,
             renewresource %in% gcamkorea.STATE_RENEWABLE_RESOURCES) %>%
      select(region, renewresource) %>%
    # filtering and selecting was not removing attributes from L210.RenewRsrc, so we add this meaningless mutate
    mutate(region = region)

    # L210.DeleteUnlimitRsrc_korea_rsrc: remove selected renewable resources from the USA region
    L210.DeleteUnlimitRsrc_korea_rsrc <- L210.UnlimitRsrc %>%
      filter(region == gcamkorea.REGION,
             unlimited.resource %in% gcamkorea.STATE_UNLIMITED_RESOURCES) %>%
      select(region, unlimited.resource) %>%
      # filtering and selecting was not removing attributes from L210.UnlimitRsrc, so we add this meaningless mutate
      mutate(region = region)

    # Separate into limestone and other unlimited resource
    L210.DeleteUnlimitRsrc_korea_limestone <- L210.DeleteUnlimitRsrc_korea_rsrc %>%
      filter(unlimited.resource == "limestone")

    L210.DeleteUnlimitRsrc_korea_rsrc <- L210.DeleteUnlimitRsrc_korea_rsrc %>%
      filter(unlimited.resource != "limestone")

    # L210.RenewRsrc_korea: renewable resource info in the states
    L210.RenewRsrc_korea <- L210.RenewRsrc %>%
      filter(region == gcamkorea.REGION,
             renewresource %in% gcamkorea.STATE_RENEWABLE_RESOURCES) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["RenewRsrc"]], region_states=gcamkorea.STATES) %>%
      # Remove geothermal from states that don't have it
      anti_join(geo_states_noresource, by = c("region", "renewresource")) %>%
      mutate(market = region)

    # L210.UnlimitRsrc_korea: unlimited resource info in the states
    L210.UnlimitRsrc_korea <- L210.UnlimitRsrc %>%
      filter(region == gcamkorea.REGION,
             unlimited.resource %in% gcamkorea.STATE_UNLIMITED_RESOURCES) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["UnlimitRsrc"]], region_states=gcamkorea.STATES)

    L210.UnlimitRsrc_limestone_korea <- L210.UnlimitRsrc_korea %>%
      filter(unlimited.resource == "limestone",
             region %in% cement_states)

    L210.UnlimitRsrc_korea <- L210.UnlimitRsrc_korea %>%
      filter(unlimited.resource != "limestone")

    # L210.UnlimitRsrcPrice_korea: unlimited resource prices in the states
    L210.UnlimitRsrcPrice_korea <- L210.UnlimitRsrcPrice %>%
      filter(region == gcamkorea.REGION,
             unlimited.resource %in% gcamkorea.STATE_UNLIMITED_RESOURCES) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["UnlimitRsrcPrice"]], region_states=gcamkorea.STATES)

    L210.UnlimitRsrcPrice_limestone_korea <- L210.UnlimitRsrcPrice_korea %>%
      filter(unlimited.resource == "limestone",
             region %in% cement_states)

    L210.UnlimitRsrcPrice_korea <- L210.UnlimitRsrcPrice_korea %>%
      filter(unlimited.resource != "limestone")

    # L210.SmthRenewRsrcTechChange_korea: smooth renewable resource tech change
    L210.SmthRenewRsrcTechChange_korea <- L210.SmthRenewRsrcTechChange %>%
      filter(region == gcamkorea.REGION,
             renewresource %in% gcamkorea.STATE_RENEWABLE_RESOURCES) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["SmthRenewRsrcTechChange"]], region_states=gcamkorea.STATES) %>%
      # If geothermal is included in this table, remove states that don't exist
      anti_join(geo_states_noresource, by = c("region", "renewresource"))

    # L210.SmthRenewRsrcCurves_wind_korea: wind resource curves in the states
    L210.SmthRenewRsrcCurves_wind_korea <- L210.SmthRenewRsrcCurves_wind %>%
      filter(region == gcamkorea.REGION) %>%
      repeat_add_columns(tibble(state = gcamkorea.STATES)) %>%
      # Add in new maxSubResource, mid.price, and curve.exponent from korea_state_wind
      left_join_error_no_match(korea_state_wind, by = c("state" = "region")) %>%
      # Convert korea_state_wind units from 2007$/kWh to 1975$/GJ
      mutate(mid_price = mid_price * gdp_deflator(1975, 2007) / CONV_KWH_GJ) %>%
      select(region = state, renewresource, smooth.renewable.subresource, year.fillout,
             maxSubResource = maxResource, mid.price = mid_price, curve.exponent = curve_exponent)

    # Minyoung
    # ~~~~~~~~~~~~~~~~~~~~~~~~ comment out: GrdRenewRsrcCurves_geo ~~~~~~~~~~~~~~~~~~~~~~~~

    # L210.GrdRenewRsrcCurves_geo_korea: geothermal resource curves in the states
    #L210.GrdRenewRsrcCurves_geo_korea <- L210.GrdRenewRsrcCurves_geo %>%
    #  filter(region == gcamkorea.REGION) %>%
    #  mutate(change_cost = extractioncost - lag(extractioncost))

    # Calculate the cost increment between grades 1 and 2
    #L210.GeoGrade2Increment <- L210.GrdRenewRsrcCurves_geo_korea %>%
    #  summarise(value = extractioncost[grade == "grade 2"] - extractioncost[grade == "grade 1"])

    # Calculate the share of the resource to allocate to each grade
    # NOTE: The method here gives precendence to the state-level NREL data in defining the quantities
    # L210.GrdRenewRsrcCurves_geo_korea <- L210.GrdRenewRsrcCurves_geo_korea %>%
    #  mutate(grade_share = available / sum(available)) %>%
    #  select(-region) %>%
    #  repeat_add_columns(tibble(region = geo_states)) %>%
    #  left_join_error_no_match(korea_re_technical_potential, by = c("region", "renewresource")) %>%
    #  mutate(available = round(grade_share * geothermal, energy.DIGITS_CALOUTPUT)) %>%
    #  left_join_error_no_match(L1231.out_EJ_korea_elec_F_tech %>%
    #                             filter(year == max(HISTORICAL_YEARS)), by = c("region" = "state", "renewresource" = "fuel")) %>%
      # Each state is assigned the same cost points, even though costs are obviously different by state
      # We don't have any data indicating the cost of the next geothermal power station by state, so we'll use the historical generation
      # to modify the floor of the cost curve in each state. This is ad-hoc and can be improved at some point.
    #  mutate(offtake = value / gcamkorea.GEOTHERMAL_DEFAULT_EFFICIENCY,
    #         offtake_share = offtake / available) %>%
    #  group_by(region) %>%
    #  mutate(offtake_share = if_else(available == 0, offtake_share[available != 0], offtake_share)) %>%
    #  ungroup() %>%
      # Index everything to the state with the largest share of its resource in use. That will get the floor of the cost curve
    #  mutate(cost_modifier = 1 - (offtake_share / max(offtake_share)),
    #         extractioncost = if_else(grade == "grade 1",
    #                                  round(extractioncost + L210.GeoGrade2Increment$value * 0.5 * cost_modifier, energy.DIGITS_COST),
    #                                  extractioncost)) %>%
    #  select(LEVEL2_DATA_NAMES[["RenewRsrcCurves"]])
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Korea has no geothermal energy
    # This geothermal resource curve is only for a model structure.
    # The 'available' is that 'available' of South Korea is divided by the Korea state count.
    L210.GrdRenewRsrcCurves_geo_korea <- L210.GrdRenewRsrcCurves_geo %>%
      filter(region == gcamkorea.REGION)

    L210.GrdRenewRsrcCurves_geo_korea <- L210.GrdRenewRsrcCurves_geo_korea %>%
      mutate(grade_share = available/length(states_subregions$state)) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = states_subregions$state)) %>%
      mutate(available = round(grade_share * available, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(L1231.out_EJ_korea_elec_F_tech %>% filter(year == max(HISTORICAL_YEARS)),
                               by = c("region" = "state", "renewresource" = "fuel")) %>%
      select(LEVEL2_DATA_NAMES[["RenewRsrcCurves"]])


    # Maximum resources: currently assuming this is just set to 1, and the resource info is stored in the grades
    # L210.GrdRenewRsrcMax_geo_korea: max sub resource for geothermal (placeholder)
    L210.GrdRenewRsrcMax_geo_korea <- L210.GrdRenewRsrcMax_geo %>%
      filter(region == gcamkorea.REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = geo_states))

    # L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea: rooftop PV resource curves in the states
    L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea <- L210.SmthRenewRsrcCurvesGdpElast_roofPV %>%
      filter(region == gcamkorea.REGION) %>%
      select(-region, -maxSubResource, -mid.price, -curve.exponent) %>%
      write_to_all_states(names = c(names(.), "region"), region_states=gcamkorea.STATES) %>%
      left_join_error_no_match(L115.rsrc_korea_rooftopPV, by = c("region" = "state")) %>%
      rename(maxSubResource = generation, mid.price = mid_p, curve.exponent = b_exp)
    # ===================================================

    # Produce outputs
    L210.DeleteRenewRsrc_korea_rsrc %>%
      add_title("Remove selected renewable resources from the USA region") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered by region and resource type") %>%
      add_legacy_name("L210.DeleteRenewRsrc_korea_rsrc") %>%
      add_precursors("L210.RenewRsrc") ->
      L210.DeleteRenewRsrc_korea_rsrc

    L210.DeleteUnlimitRsrc_korea_rsrc %>%
      add_title("Remove selected unlimited resources from the USA region") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered by region and resource type") %>%
      add_legacy_name("L210.DeleteUnlimitRsrc_korea_rsrc") %>%
      add_precursors("L210.UnlimitRsrc") ->
      L210.DeleteUnlimitRsrc_korea_rsrc

    L210.DeleteUnlimitRsrc_korea_limestone %>%
      add_title("Remove limestone from the USA region") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered by region and resource type") %>%
      add_legacy_name("L210.DeleteUnlimitRsrc_korea_limestone") %>%
      add_precursors("L210.UnlimitRsrc") ->
      L210.DeleteUnlimitRsrc_korea_limestone

    L210.RenewRsrc_korea %>%
      add_title("Renewable resource info in the states") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered and written to all states") %>%
      add_legacy_name("L210.RenewRsrc_korea") %>%
      add_precursors("L210.RenewRsrc", "gcam-korea/korea_re_technical_potential", "gcam-korea/states_subregions") ->
      L210.RenewRsrc_korea

    L210.UnlimitRsrc_korea %>%
      add_title("Unlimited resource info in the states") %>%
      add_units("NA") %>%
      add_comments("L210.UnlimitRsrc filtered and written to all states") %>%
      add_legacy_name("L210.UnlimitRsrc_korea") %>%
      add_precursors("L210.UnlimitRsrc") ->
      L210.UnlimitRsrc_korea

    L210.UnlimitRsrc_limestone_korea %>%
      add_title("Limestone info in the states") %>%
      add_units("NA") %>%
      add_comments("L210.UnlimitRsrc filtered and written to all states") %>%
      add_legacy_name("L210.UnlimitRsrc_limestone_korea") %>%
      add_precursors("L210.UnlimitRsrc", "L1321.out_Mt_korea_cement_Yh") ->
      L210.UnlimitRsrc_limestone_korea

    L210.UnlimitRsrcPrice_korea %>%
      add_title("Unlimited resource prices in the states") %>%
      add_units("1975$/GJ") %>%
      add_comments("L210.UnlimitRsrcPrice filtered and written to all states") %>%
      add_legacy_name("L210.UnlimitRsrcPrice_korea") %>%
      add_precursors("L210.UnlimitRsrcPrice") ->
      L210.UnlimitRsrcPrice_korea

    L210.UnlimitRsrcPrice_limestone_korea %>%
      add_title("Limestone prices in the states") %>%
      add_units("1975$/kg") %>%
      add_comments("L210.UnlimitRsrcPrice filtered and written to all states") %>%
      add_legacy_name("L210.UnlimitRsrcPrice_limestone_korea") %>%
      add_precursors("L210.UnlimitRsrcPrice", "L1321.out_Mt_korea_cement_Yh") ->
      L210.UnlimitRsrcPrice_limestone_korea

    L210.SmthRenewRsrcTechChange_korea %>%
      add_title("Smooth renewable resource tech change: USA") %>%
      add_units("Unitless") %>%
      add_comments("L210.SmthRenewRsrcTechChange filtered and written to all states") %>%
      add_legacy_name("L210.SmthRenewRsrcTechChange_korea") %>%
      add_precursors("L210.SmthRenewRsrcTechChange", "energy/calibrated_techs") ->
      L210.SmthRenewRsrcTechChange_korea

    L210.SmthRenewRsrcCurves_wind_korea %>%
      add_title("Wind resource curves in the states") %>%
      add_units("maxSubResource: EJ; mid.price: 1975$/GJ") %>%
      add_comments("L210.SmthRenewRsrcCurves_wind filtered and written to all states") %>%
      add_legacy_name("L210.SmthRenewRsrcCurves_wind_korea") %>%
      add_precursors("L210.SmthRenewRsrcCurves_wind", "gcam-korea/korea_state_wind") ->
      L210.SmthRenewRsrcCurves_wind_korea

    L210.GrdRenewRsrcCurves_geo_korea %>%
      add_title("Geothermal resource curves in the states") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("USA data from L210.GrdRenewRsrcCurves_geo shared out with korea_re_technical_potential") %>%
      add_legacy_name("L210.GrdRenewRsrcCurves_geo_korea") %>%
      add_precursors("L210.GrdRenewRsrcCurves_geo", "gcam-korea/korea_re_technical_potential", "L1231.out_EJ_korea_elec_F_tech") ->
      L210.GrdRenewRsrcCurves_geo_korea

    L210.GrdRenewRsrcMax_geo_korea %>%
      add_title("Max sub resource for geothermal (placeholder)") %>%
      add_units("Unitless") %>%
      add_comments("L210.GrdRenewRsrcMax_geo filtered and written to relevant states, constant value used") %>%
      add_legacy_name("L210.GrdRenewRsrcMax_geo_korea") %>%
      add_precursors("L210.GrdRenewRsrcMax_geo") ->
      L210.GrdRenewRsrcMax_geo_korea

    L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea %>%
      add_title("Rooftop PV resource curves by state") %>%
      add_units("maxSubResource: EJ; mid.price = 1975$/GJ") %>%
      add_comments("Values from L115.rsrc_korea_rooftopPV added to categories from L210.SmthRenewRsrcCurvesGdpElast_roofPV") %>%
      add_legacy_name("L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea") %>%
      add_precursors("L210.SmthRenewRsrcCurvesGdpElast_roofPV", "L115.rsrc_korea_rooftopPV") ->
      L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea

    return_data(L210.DeleteRenewRsrc_korea_rsrc, L210.DeleteUnlimitRsrc_korea_rsrc, L210.RenewRsrc_korea, L210.UnlimitRsrc_korea,
                L210.UnlimitRsrcPrice_korea, L210.SmthRenewRsrcTechChange_korea, L210.SmthRenewRsrcCurves_wind_korea,
                L210.GrdRenewRsrcCurves_geo_korea, L210.GrdRenewRsrcMax_geo_korea, L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea,
                L210.DeleteUnlimitRsrc_korea_limestone, L210.UnlimitRsrc_limestone_korea, L210.UnlimitRsrcPrice_limestone_korea)
  } else {
    stop("Unknown command")
  }
}
