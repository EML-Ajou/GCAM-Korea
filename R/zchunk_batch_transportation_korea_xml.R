#' module_gcam.korea_batch_transportation_korea_xml
#'
#' Construct XML data structure for \code{transportation_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_korea.xml}.
module_gcam.korea_batch_transportation_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.DeleteSupplysector_koreaTrn",
             "L254.DeleteFinalDemand_koreaTrn",
             "L254.Supplysector_trn_korea",
             "L254.FinalEnergyKeyword_trn_korea",
             "L254.tranSubsectorLogit_trn_korea",
             "L254.tranSubsectorShrwtFllt_trn_korea",
             "L254.tranSubsectorInterp_trn_korea",
             "L254.tranSubsectorSpeed_trn_korea",
             "L254.tranSubsectorSpeed_passthru_trn_korea",
             "L254.tranSubsectorSpeed_noVOTT_trn_korea",
             "L254.tranSubsectorSpeed_nonmotor_trn_korea",
             "L254.tranSubsectorVOTT_trn_korea",
             "L254.tranSubsectorFuelPref_trn_korea",
             "L254.StubTranTech_trn_korea",
             "L254.StubTranTech_passthru_trn_korea",
             "L254.StubTranTech_nonmotor_trn_korea",
             "L254.StubTranTechLoadFactor_trn_korea",
             "L254.StubTranTechCost_trn_korea",
             "L254.StubTranTechCoef_trn_korea",
             "L254.PerCapitaBased_trn_korea",
             "L254.PriceElasticity_trn_korea",
             "L254.IncomeElasticity_trn_korea",
             "L254.StubTranTechCalInput_trn_korea",
             "L254.StubTranTechProd_nonmotor_korea",
             "L254.StubTranTechCalInput_passthru_trn_korea",
             "L254.BaseService_trn_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transportation_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.DeleteSupplysector_koreaTrn <- get_data(all_data, "L254.DeleteSupplysector_koreaTrn")
    L254.DeleteFinalDemand_koreaTrn <- get_data(all_data, "L254.DeleteFinalDemand_koreaTrn")
    L254.Supplysector_trn_korea <- get_data(all_data, "L254.Supplysector_trn_korea")
    L254.FinalEnergyKeyword_trn_korea <- get_data(all_data, "L254.FinalEnergyKeyword_trn_korea")
    L254.tranSubsectorLogit_trn_korea <- get_data(all_data, "L254.tranSubsectorLogit_trn_korea")
    L254.tranSubsectorShrwtFllt_trn_korea <- get_data(all_data, "L254.tranSubsectorShrwtFllt_trn_korea")
    L254.tranSubsectorInterp_trn_korea <- get_data(all_data, "L254.tranSubsectorInterp_trn_korea")
    L254.tranSubsectorSpeed_trn_korea <- get_data(all_data, "L254.tranSubsectorSpeed_trn_korea")
    L254.tranSubsectorSpeed_passthru_trn_korea <- get_data(all_data, "L254.tranSubsectorSpeed_passthru_trn_korea")
    L254.tranSubsectorSpeed_noVOTT_trn_korea <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT_trn_korea")
    L254.tranSubsectorSpeed_nonmotor_trn_korea <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor_trn_korea")
    L254.tranSubsectorVOTT_trn_korea <- get_data(all_data, "L254.tranSubsectorVOTT_trn_korea")
    L254.tranSubsectorFuelPref_trn_korea <- get_data(all_data, "L254.tranSubsectorFuelPref_trn_korea")
    L254.StubTranTech_trn_korea <- get_data(all_data, "L254.StubTranTech_trn_korea")
    L254.StubTranTech_passthru_trn_korea <- get_data(all_data, "L254.StubTranTech_passthru_trn_korea")
    L254.StubTranTech_nonmotor_trn_korea <- get_data(all_data, "L254.StubTranTech_nonmotor_trn_korea")
    L254.StubTranTechLoadFactor_trn_korea <- get_data(all_data, "L254.StubTranTechLoadFactor_trn_korea")
    L254.StubTranTechCost_trn_korea <- get_data(all_data, "L254.StubTranTechCost_trn_korea")
    L254.StubTranTechCoef_trn_korea <- get_data(all_data, "L254.StubTranTechCoef_trn_korea")
    L254.PerCapitaBased_trn_korea <- get_data(all_data, "L254.PerCapitaBased_trn_korea")
    L254.PriceElasticity_trn_korea <- get_data(all_data, "L254.PriceElasticity_trn_korea")
    L254.IncomeElasticity_trn_korea <- get_data(all_data, "L254.IncomeElasticity_trn_korea")
    L254.StubTranTechCalInput_trn_korea <- get_data(all_data, "L254.StubTranTechCalInput_trn_korea")
    L254.StubTranTechProd_nonmotor_korea <- get_data(all_data, "L254.StubTranTechProd_nonmotor_korea")
    L254.StubTranTechCalInput_passthru_trn_korea <- get_data(all_data, "L254.StubTranTechCalInput_passthru_trn_korea")
    L254.BaseService_trn_korea <- get_data(all_data, "L254.BaseService_trn_korea")
    # ===================================================

    # Produce outputs
    create_xml("korea/transportation_korea.xml") %>%
      add_xml_data(L254.DeleteSupplysector_koreaTrn, "DeleteSupplysector") %>%
      add_xml_data(L254.DeleteFinalDemand_koreaTrn, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L254.Supplysector_trn_korea, "Supplysector") %>%
      add_xml_data(L254.FinalEnergyKeyword_trn_korea, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L254.tranSubsectorLogit_trn_korea, "tranSubsectorLogit", "tranSubsector") %>%
      add_xml_data(L254.tranSubsectorShrwtFllt_trn_korea, "tranSubsectorShrwtFllt") %>%
      add_xml_data(L254.tranSubsectorInterp_trn_korea, "tranSubsectorInterp") %>%
      add_xml_data(L254.tranSubsectorSpeed_trn_korea, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_passthru_trn_korea, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_noVOTT_trn_korea, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_nonmotor_trn_korea, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorVOTT_trn_korea, "tranSubsectorVOTT") %>%
      add_xml_data(L254.tranSubsectorFuelPref_trn_korea, "tranSubsectorFuelPref") %>%
      add_xml_data(L254.StubTranTech_trn_korea, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_passthru_trn_korea, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_nonmotor_trn_korea, "StubTranTech") %>%
      add_xml_data(L254.StubTranTechLoadFactor_trn_korea, "StubTranTechLoadFactor") %>%
      add_xml_data(L254.StubTranTechCost_trn_korea, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_trn_korea, "StubTranTechCoef") %>%
      add_xml_data(L254.PerCapitaBased_trn_korea, "PerCapitaBased") %>%
      add_xml_data(L254.PriceElasticity_trn_korea, "PriceElasticity") %>%
      add_xml_data(L254.IncomeElasticity_trn_korea, "IncomeElasticity") %>%
      add_xml_data(L254.StubTranTechCalInput_trn_korea, "StubTranTechCalInput") %>%
      add_xml_data(L254.StubTranTechProd_nonmotor_korea, "StubTranTechProd") %>%
      add_xml_data(L254.StubTranTechCalInput_passthru_trn_korea, "StubTranTechCalInput") %>%
      add_xml_data(L254.BaseService_trn_korea, "BaseService") %>%
      add_precursors("L254.DeleteSupplysector_koreaTrn",
                     "L254.DeleteFinalDemand_koreaTrn",
                     "L254.Supplysector_trn_korea",
                     "L254.FinalEnergyKeyword_trn_korea",
                     "L254.tranSubsectorLogit_trn_korea",
                     "L254.tranSubsectorShrwtFllt_trn_korea",
                     "L254.tranSubsectorInterp_trn_korea",
                     "L254.tranSubsectorSpeed_trn_korea",
                     "L254.tranSubsectorSpeed_passthru_trn_korea",
                     "L254.tranSubsectorSpeed_noVOTT_trn_korea",
                     "L254.tranSubsectorSpeed_nonmotor_trn_korea",
                     "L254.tranSubsectorVOTT_trn_korea",
                     "L254.tranSubsectorFuelPref_trn_korea",
                     "L254.StubTranTech_trn_korea",
                     "L254.StubTranTech_passthru_trn_korea",
                     "L254.StubTranTech_nonmotor_trn_korea",
                     "L254.StubTranTechLoadFactor_trn_korea",
                     "L254.StubTranTechCost_trn_korea",
                     "L254.StubTranTechCoef_trn_korea",
                     "L254.PerCapitaBased_trn_korea",
                     "L254.PriceElasticity_trn_korea",
                     "L254.IncomeElasticity_trn_korea",
                     "L254.StubTranTechCalInput_trn_korea",
                     "L254.StubTranTechProd_nonmotor_korea",
                     "L254.StubTranTechCalInput_passthru_trn_korea",
                     "L254.BaseService_trn_korea") ->
      transportation_korea.xml

    return_data(transportation_korea.xml)
  } else {
    stop("Unknown command")
  }
}
