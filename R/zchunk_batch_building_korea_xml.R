#' module_gcam.korea_batch_building_korea_xml
#'
#' Construct XML data structure for \code{building_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_korea.xml}.
module_gcam.korea_batch_building_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(#FILE="gcam-korea/bld_price_elasticity",
             "L244.DeleteConsumer_koreaBld",
             "L244.DeleteSupplysector_koreaBld",
             "L244.SubregionalShares_gcamKorea",
             "L244.PriceExp_IntGains_gcamKorea",
             "L244.Floorspace_gcamKorea",
             "L244.DemandFunction_serv_gcamKorea",
             "L244.DemandFunction_flsp_gcamKorea",
             "L244.Satiation_flsp_gcamKorea",
             "L244.SatiationAdder_gcamKorea",
             "L244.ThermalBaseService_gcamKorea",
             "L244.GenericBaseService_gcamKorea",
             "L244.ThermalServiceSatiation_gcamKorea",
             "L244.GenericServiceSatiation_gcamKorea",
             "L244.Intgains_scalar_gcamKorea",
             "L244.ShellConductance_bld_gcamKorea",
             "L244.Supplysector_bld_gcamKorea",
             "L244.FinalEnergyKeyword_bld_gcamKorea",
             "L244.SubsectorShrwtFllt_bld_gcamKorea",
             "L244.SubsectorInterp_bld_gcamKorea",
             "L244.SubsectorInterpTo_bld_gcamKorea",
             "L244.SubsectorLogit_bld_gcamKorea",
             "L244.StubTech_bld_gcamKorea",
             "L244.StubTechCalInput_bld_gcamKorea",
             "L244.StubTechMarket_bld_korea",
             "L244.GlobalTechIntGainOutputRatio_korea",
             #"L244.GlobalTechInterpTo_bld_korea",
             "L244.GlobalTechEff_bld_korea",
             "L244.GlobalTechShrwt_bld_gcamKorea",
             "L244.GlobalTechCost_bld_gcamKorea",
             "L244.GlobalTechSCurve_bld_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    #bld_price_elasticity <- get_data(all_data, "gcam-korea/bld_price_elasticity")
    L244.DeleteConsumer_koreaBld <- get_data(all_data, "L244.DeleteConsumer_koreaBld")
    L244.DeleteSupplysector_koreaBld <- get_data(all_data, "L244.DeleteSupplysector_koreaBld")
    L244.SubregionalShares <- get_data(all_data, "L244.SubregionalShares_gcamKorea")
    L244.PriceExp_IntGains <- get_data(all_data, "L244.PriceExp_IntGains_gcamKorea")
    L244.Floorspace <- get_data(all_data, "L244.Floorspace_gcamKorea")
    L244.DemandFunction_serv <- get_data(all_data, "L244.DemandFunction_serv_gcamKorea")
    L244.DemandFunction_flsp <- get_data(all_data, "L244.DemandFunction_flsp_gcamKorea")
    L244.Satiation_flsp <- get_data(all_data, "L244.Satiation_flsp_gcamKorea")
    L244.SatiationAdder <- get_data(all_data, "L244.SatiationAdder_gcamKorea")
    L244.ThermalBaseService <- get_data(all_data, "L244.ThermalBaseService_gcamKorea")
    L244.GenericBaseService <- get_data(all_data, "L244.GenericBaseService_gcamKorea")
    L244.ThermalServiceSatiation <- get_data(all_data, "L244.ThermalServiceSatiation_gcamKorea")
    L244.GenericServiceSatiation <- get_data(all_data, "L244.GenericServiceSatiation_gcamKorea")
    L244.Intgains_scalar <- get_data(all_data, "L244.Intgains_scalar_gcamKorea")
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld_gcamKorea")
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld_gcamKorea")
    L244.FinalEnergyKeyword_bld <- get_data(all_data, "L244.FinalEnergyKeyword_bld_gcamKorea")
    L244.SubsectorShrwtFllt_bld <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_gcamKorea")
    L244.SubsectorInterp_bld <- get_data(all_data, "L244.SubsectorInterp_bld_gcamKorea")
    L244.SubsectorInterpTo_bld <- get_data(all_data, "L244.SubsectorInterpTo_bld_gcamKorea")
    L244.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld_gcamKorea")
    L244.StubTech_bld <- get_data(all_data, "L244.StubTech_bld_gcamKorea")
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld_gcamKorea")
    L244.StubTechMarket_bld_korea <- get_data(all_data, "L244.StubTechMarket_bld_korea")
    L244.GlobalTechIntGainOutputRatio_korea <- get_data(all_data, "L244.GlobalTechIntGainOutputRatio_korea")
    #L244.GlobalTechInterpTo_bld_korea <- get_data(all_data, "L244.GlobalTechInterpTo_bld_korea")
    L244.GlobalTechEff_bld_korea <- get_data(all_data, "L244.GlobalTechEff_bld_korea")
    L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld_gcamKorea")
    L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld_gcamKorea")
    L244.GlobalTechSCurve_bld_korea <- get_data(all_data, "L244.GlobalTechSCurve_bld_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/building_korea.xml") %>%
      add_xml_data(L244.DeleteConsumer_koreaBld, "DeleteConsumer") %>%
      add_xml_data(L244.DeleteSupplysector_koreaBld, "DeleteSupplysector") %>%
      add_xml_data(L244.SubregionalShares, "SubregionalShares") %>%
      add_xml_data(L244.PriceExp_IntGains, "PriceExp_IntGains") %>%
      add_xml_data(L244.Floorspace, "Floorspace") %>%
      add_xml_data(L244.DemandFunction_serv, "DemandFunction_serv") %>%
      add_xml_data(L244.DemandFunction_flsp, "DemandFunction_flsp") %>%
      add_xml_data(L244.Satiation_flsp, "Satiation_flsp") %>%
      add_xml_data(L244.SatiationAdder, "SatiationAdder") %>%
      add_xml_data(L244.ThermalBaseService, "ThermalBaseService") %>%
      add_xml_data(L244.GenericBaseService, "GenericBaseService") %>%
      add_xml_data(L244.ThermalServiceSatiation, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericServiceSatiation, "GenericServiceSatiation") %>%
      add_xml_data(L244.Intgains_scalar, "Intgains_scalar") %>%
      add_xml_data(L244.ShellConductance_bld, "ShellConductance") %>%
      add_logit_tables_xml(L244.Supplysector_bld, "Supplysector") %>%
      add_xml_data(L244.FinalEnergyKeyword_bld, "FinalEnergyKeyword") %>%
      add_xml_data(L244.SubsectorShrwtFllt_bld, "SubsectorShrwtFllt") %>%
      add_xml_data(L244.SubsectorInterp_bld, "SubsectorInterp") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld, "SubsectorLogit") %>%
      add_xml_data(L244.StubTech_bld, "StubTech") %>%
      add_xml_data(L244.StubTechCalInput_bld, "StubTechCalInput") %>%
      add_xml_data(L244.StubTechMarket_bld_korea, "StubTechMarket") %>%
      add_xml_data(L244.GlobalTechIntGainOutputRatio_korea, "GlobalTechIntGainOutputRatio") %>%
      #add_xml_data(L244.GlobalTechInterpTo_bld_korea, "GlobalTechInterpTo") %>%
      add_xml_data(L244.GlobalTechEff_bld_korea, "GlobalTechEff") %>%
      add_xml_data(L244.GlobalTechShrwt_bld, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechSCurve_bld_korea, "GlobalTechSCurve")  %>%
      #add_xml_data(bld_price_elasticity, "PriceElasticity") %>%
      add_precursors(#"gcam-korea/bld_price_elasticity",
                     "L244.DeleteConsumer_koreaBld",
                     "L244.DeleteSupplysector_koreaBld",
                     "L244.SubregionalShares_gcamKorea",
                     "L244.PriceExp_IntGains_gcamKorea",
                     "L244.Floorspace_gcamKorea",
                     "L244.DemandFunction_serv_gcamKorea",
                     "L244.DemandFunction_flsp_gcamKorea",
                     "L244.Satiation_flsp_gcamKorea",
                     "L244.SatiationAdder_gcamKorea",
                     "L244.ThermalBaseService_gcamKorea",
                     "L244.GenericBaseService_gcamKorea",
                     "L244.ThermalServiceSatiation_gcamKorea",
                     "L244.GenericServiceSatiation_gcamKorea",
                     "L244.Intgains_scalar_gcamKorea",
                     "L244.ShellConductance_bld_gcamKorea",
                     "L244.Supplysector_bld_gcamKorea",
                     "L244.FinalEnergyKeyword_bld_gcamKorea",
                     "L244.SubsectorShrwtFllt_bld_gcamKorea",
                     "L244.SubsectorInterp_bld_gcamKorea",
                     "L244.SubsectorInterpTo_bld_gcamKorea",
                     "L244.SubsectorLogit_bld_gcamKorea",
                     "L244.StubTech_bld_gcamKorea",
                     "L244.StubTechCalInput_bld_gcamKorea",
                     "L244.StubTechMarket_bld_korea",
                     "L244.GlobalTechIntGainOutputRatio_korea",
                     #"L244.GlobalTechInterpTo_bld_korea",
                     "L244.GlobalTechEff_bld_korea",
                     "L244.GlobalTechShrwt_bld_gcamKorea",
                     "L244.GlobalTechCost_bld_gcamKorea",
                     "L244.GlobalTechSCurve_bld_korea") ->
      building_korea.xml

    # # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L244.SubsectorInterpTo_bld)) {

      building_korea.xml %>%
        add_xml_data(L244.SubsectorInterpTo_bld, "SubsectorInterpTo") ->
        building_korea.xml

      }

    return_data(building_korea.xml)
  } else {
    stop("Unknown command")
  }
}
