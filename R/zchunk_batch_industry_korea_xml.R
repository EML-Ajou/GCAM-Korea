#' module_gcam.korea_batch_industry_korea_xml
#'
#' Construct XML data structure for \code{industry_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_korea.xml}.
module_gcam.korea_batch_industry_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE="gcam-korea/fuelprefElasticity",
             "L232.DeleteSupplysector_koreaInd",
             "L232.DeleteFinalDemand_koreaInd",
             "L232.Supplysector_ind_korea",
             "L232.FinalEnergyKeyword_ind_korea",
             "L232.SubsectorLogit_ind_korea",
             "L232.SubsectorShrwtFllt_ind_korea",
             "L232.SubsectorInterp_ind_korea",
             "L232.StubTech_ind_korea",
             "L232.StubTechInterp_ind_korea",
             "L232.PerCapitaBased_ind_korea",
             "L232.PriceElasticity_ind_korea",
             "L232.IncomeElasticity_ind_gcam3_korea",
             "L232.StubTechCalInput_indenergy_korea",
             "L232.StubTechCalInput_indfeed_korea",
             "L232.StubTechProd_industry_korea",
             "L232.StubTechCoef_industry_korea",
             "L232.StubTechMarket_ind_korea",
             "L232.BaseService_ind_korea",
             "L232.StubTechSecMarket_ind_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    fuelprefElasticity <- get_data(all_data, "gcam-korea/fuelprefElasticity")
    L232.DeleteSupplysector_koreaInd <- get_data(all_data, "L232.DeleteSupplysector_koreaInd")
    L232.DeleteFinalDemand_koreaInd <- get_data(all_data, "L232.DeleteFinalDemand_koreaInd")
    L232.Supplysector_ind_korea <- get_data(all_data, "L232.Supplysector_ind_korea")
    L232.FinalEnergyKeyword_ind_korea <- get_data(all_data, "L232.FinalEnergyKeyword_ind_korea")
    L232.SubsectorLogit_ind_korea <- get_data(all_data, "L232.SubsectorLogit_ind_korea")
    L232.SubsectorShrwtFllt_ind_korea <- get_data(all_data, "L232.SubsectorShrwtFllt_ind_korea")
    L232.SubsectorInterp_ind_korea <- get_data(all_data, "L232.SubsectorInterp_ind_korea")
    L232.StubTech_ind_korea <- get_data(all_data, "L232.StubTech_ind_korea")
    L232.StubTechInterp_ind_korea <- get_data(all_data, "L232.StubTechInterp_ind_korea")
    L232.PerCapitaBased_ind_korea <- get_data(all_data, "L232.PerCapitaBased_ind_korea")
    L232.PriceElasticity_ind_korea <- get_data(all_data, "L232.PriceElasticity_ind_korea")
    L232.IncomeElasticity_ind_gcam3_korea <- get_data(all_data, "L232.IncomeElasticity_ind_gcam3_korea")
    L232.StubTechCalInput_indenergy_korea <- get_data(all_data, "L232.StubTechCalInput_indenergy_korea")
    L232.StubTechCalInput_indfeed_korea <- get_data(all_data, "L232.StubTechCalInput_indfeed_korea")
    L232.StubTechProd_industry_korea <- get_data(all_data, "L232.StubTechProd_industry_korea")
    L232.StubTechCoef_industry_korea <- get_data(all_data, "L232.StubTechCoef_industry_korea")
    L232.StubTechMarket_ind_korea <- get_data(all_data, "L232.StubTechMarket_ind_korea")
    L232.StubTechSecMarket_ind_korea <- get_data(all_data, "L232.StubTechSecMarket_ind_korea")
    L232.BaseService_ind_korea <- get_data(all_data, "L232.BaseService_ind_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/industry_korea.xml") %>%
      add_xml_data(L232.DeleteSupplysector_koreaInd, "DeleteSupplysector") %>%
      add_xml_data(L232.DeleteFinalDemand_koreaInd, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L232.Supplysector_ind_korea, "Supplysector") %>%
      add_xml_data(L232.FinalEnergyKeyword_ind_korea, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L232.SubsectorLogit_ind_korea, "SubsectorLogit") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind_korea, "SubsectorShrwtFllt") %>%
      add_xml_data(L232.SubsectorInterp_ind_korea, "SubsectorInterp") %>%
      add_xml_data(L232.StubTech_ind_korea, "StubTech") %>%
      add_xml_data(L232.StubTechInterp_ind_korea, "StubTechInterp") %>%
      add_xml_data(L232.PerCapitaBased_ind_korea, "PerCapitaBased") %>%
      add_xml_data(L232.PriceElasticity_ind_korea, "PriceElasticity") %>%
      add_xml_data(L232.IncomeElasticity_ind_gcam3_korea, "IncomeElasticity") %>%
      add_xml_data(L232.StubTechCalInput_indenergy_korea, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechCalInput_indfeed_korea, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechProd_industry_korea, "StubTechProd") %>%
      add_xml_data(L232.StubTechCoef_industry_korea, "StubTechCoef") %>%
      add_xml_data(L232.StubTechMarket_ind_korea, "StubTechMarket") %>%
      add_xml_data(L232.StubTechSecMarket_ind_korea, "StubTechSecMarket") %>%
      add_xml_data(L232.BaseService_ind_korea, "BaseService") %>%
      add_xml_data(fuelprefElasticity, "FuelPrefElast") %>%
      add_precursors("gcam-korea/fuelprefElasticity",
                     "L232.DeleteSupplysector_koreaInd",
                     "L232.DeleteFinalDemand_koreaInd",
                     "L232.Supplysector_ind_korea",
                     "L232.FinalEnergyKeyword_ind_korea",
                     "L232.SubsectorLogit_ind_korea",
                     "L232.SubsectorShrwtFllt_ind_korea",
                     "L232.SubsectorInterp_ind_korea",
                     "L232.StubTech_ind_korea",
                     "L232.StubTechInterp_ind_korea",
                     "L232.PerCapitaBased_ind_korea",
                     "L232.PriceElasticity_ind_korea",
                     "L232.IncomeElasticity_ind_gcam3_korea",
                     "L232.StubTechCalInput_indenergy_korea",
                     "L232.StubTechCalInput_indfeed_korea",
                     "L232.StubTechProd_industry_korea",
                     "L232.StubTechCoef_industry_korea",
                     "L232.StubTechMarket_ind_korea",
                     "L232.BaseService_ind_korea",
                     "L232.StubTechSecMarket_ind_korea") ->
      industry_korea.xml

    return_data(industry_korea.xml)
  } else {
    stop("Unknown command")
  }
}
