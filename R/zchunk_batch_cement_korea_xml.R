#' module_gcam.korea_batch_cement_korea_xml
#'
#' Construct XML data structure for \code{cement_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{cement_korea.xml}.
module_gcam.korea_batch_cement_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.DeleteUnlimitRsrc_korea_limestone",
             "L210.UnlimitRsrc_limestone_korea",
             "L210.UnlimitRsrcPrice_limestone_korea",
             "L2321.DeleteSupplysector_koreaCement",
             "L2321.Supplysector_cement_korea",
             "L2321.FinalEnergyKeyword_cement_korea",
             "L2321.SubsectorLogit_cement_korea",
             "L2321.SubsectorShrwtFllt_cement_korea",
             "L2321.SubsectorInterp_cement_korea",
             "L2321.StubTech_cement_korea",
             "L2321.PerCapitaBased_cement_korea",
             "L2321.PriceElasticity_cement_korea",
             "L2321.IncomeElasticity_cement_gcam3_korea",
             "L2321.DeleteFinalDemand_koreaCement",
             "L2321.StubTechProd_cement_korea",
             "L2321.StubTechCoef_cement_korea",
             "L2321.StubTechCalInput_cement_heat_korea",
             "L2321.StubTechMarket_cement_korea",
             "L2321.BaseService_cement_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cement_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.DeleteUnlimitRsrc_korea_limestone <- get_data(all_data, "L210.DeleteUnlimitRsrc_korea_limestone")
    L210.UnlimitRsrc_limestone_korea <- get_data(all_data, "L210.UnlimitRsrc_limestone_korea")
    L210.UnlimitRsrcPrice_limestone_korea <- get_data(all_data, "L210.UnlimitRsrcPrice_limestone_korea")
    L2321.DeleteSupplysector_koreaCement <- get_data(all_data, "L2321.DeleteSupplysector_koreaCement")
    L2321.FinalEnergyKeyword_cement_korea <- get_data(all_data, "L2321.FinalEnergyKeyword_cement_korea")
    L2321.SubsectorLogit_cement_korea <- get_data(all_data, "L2321.SubsectorLogit_cement_korea")
    L2321.SubsectorShrwtFllt_cement_korea <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement_korea")
    L2321.SubsectorInterp_cement_korea <- get_data(all_data, "L2321.SubsectorInterp_cement_korea")
    L2321.StubTech_cement_korea <- get_data(all_data, "L2321.StubTech_cement_korea")
    L2321.PerCapitaBased_cement_korea <- get_data(all_data, "L2321.PerCapitaBased_cement_korea")
    L2321.PriceElasticity_cement_korea <- get_data(all_data, "L2321.PriceElasticity_cement_korea")
    L2321.IncomeElasticity_cement_gcam3_korea <- get_data(all_data, "L2321.IncomeElasticity_cement_gcam3_korea")
    L2321.DeleteFinalDemand_koreaCement <- get_data(all_data, "L2321.DeleteFinalDemand_koreaCement")
    L2321.Supplysector_cement_korea <- get_data(all_data, "L2321.Supplysector_cement_korea")
    L2321.StubTechProd_cement_korea <- get_data(all_data, "L2321.StubTechProd_cement_korea")
    L2321.StubTechCoef_cement_korea <- get_data(all_data, "L2321.StubTechCoef_cement_korea")
    L2321.StubTechCalInput_cement_heat_korea <- get_data(all_data, "L2321.StubTechCalInput_cement_heat_korea")
    L2321.StubTechMarket_cement_korea <- get_data(all_data, "L2321.StubTechMarket_cement_korea")
    L2321.BaseService_cement_korea <- get_data(all_data, "L2321.BaseService_cement_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/cement_korea.xml") %>%
      add_xml_data(L210.DeleteUnlimitRsrc_korea_limestone, "DeleteUnlimitRsrc") %>%
      add_xml_data(L210.UnlimitRsrc_limestone_korea, "UnlimitRsrc") %>%
      add_xml_data(L210.UnlimitRsrcPrice_limestone_korea, "UnlimitRsrcPrice") %>%
      add_xml_data(L2321.DeleteSupplysector_koreaCement, "DeleteSupplysector") %>%
      add_xml_data(L2321.DeleteFinalDemand_koreaCement, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L2321.Supplysector_cement_korea, "Supplysector") %>%
      add_xml_data(L2321.FinalEnergyKeyword_cement_korea, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2321.SubsectorLogit_cement_korea, "SubsectorLogit") %>%
      add_xml_data(L2321.SubsectorShrwtFllt_cement_korea, "SubsectorShrwtFllt") %>%
      add_xml_data(L2321.SubsectorInterp_cement_korea, "SubsectorInterp") %>%
      add_xml_data(L2321.StubTech_cement_korea, "StubTech") %>%
      add_xml_data(L2321.PerCapitaBased_cement_korea, "PerCapitaBased") %>%
      add_xml_data(L2321.PriceElasticity_cement_korea, "PriceElasticity") %>%
      add_xml_data(L2321.IncomeElasticity_cement_gcam3_korea, "IncomeElasticity") %>%
      add_xml_data(L2321.StubTechProd_cement_korea, "StubTechProd") %>%
      add_xml_data(L2321.StubTechCoef_cement_korea, "StubTechCoef") %>%
      add_xml_data(L2321.StubTechCalInput_cement_heat_korea, "StubTechCalInput") %>%
      add_xml_data(L2321.StubTechMarket_cement_korea, "StubTechMarket") %>%
      add_xml_data(L2321.BaseService_cement_korea, "BaseService") %>%
      add_precursors("L210.DeleteUnlimitRsrc_korea_limestone",
                     "L210.UnlimitRsrc_limestone_korea",
                     "L210.UnlimitRsrcPrice_limestone_korea",
                     "L2321.DeleteSupplysector_koreaCement",
                     "L2321.Supplysector_cement_korea",
                     "L2321.FinalEnergyKeyword_cement_korea",
                     "L2321.SubsectorLogit_cement_korea",
                     "L2321.SubsectorShrwtFllt_cement_korea",
                     "L2321.SubsectorInterp_cement_korea",
                     "L2321.StubTech_cement_korea",
                     "L2321.PerCapitaBased_cement_korea",
                     "L2321.PriceElasticity_cement_korea",
                     "L2321.IncomeElasticity_cement_gcam3_korea",
                     "L2321.DeleteFinalDemand_koreaCement",
                     "L2321.StubTechProd_cement_korea",
                     "L2321.StubTechCoef_cement_korea",
                     "L2321.StubTechCalInput_cement_heat_korea",
                     "L2321.StubTechMarket_cement_korea",
                     "L2321.BaseService_cement_korea") ->
      cement_korea.xml

    return_data(cement_korea.xml)
  } else {
    stop("Unknown command")
  }
}
