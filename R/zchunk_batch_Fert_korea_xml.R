#' module_gcam.korea_batch_Fert_korea_xml
#'
#' Construct XML data structure for \code{Fert_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Fert_korea.xml}.
module_gcam.korea_batch_Fert_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2322.DeleteSubsector_koreaFert",
             "L2322.FinalEnergyKeyword_Fert_korea",
             "L2322.Supplysector_Fert_korea",
             "L2322.SubsectorLogit_Fert_korea",
             "L2322.SubsectorShrwtFllt_Fert_korea",
             "L2322.SubsectorInterp_Fert_korea",
             "L2322.StubTech_Fert_korea",
             "L2322.FinalEnergyKeyword_koreaFert",
             "L2322.SubsectorLogit_koreaFert",
             "L2322.SubsectorShrwtFllt_koreaFert",
             "L2322.SubsectorInterp_koreaFert",
             "L2322.TechShrwt_koreaFert",
             "L2322.Production_koreaFert",
             "L2322.TechCoef_koreaFert",
             "L2322.StubTechProd_Fert_korea",
             "L2322.StubTechCoef_Fert_korea",
             "L2322.StubTechMarket_Fert_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Fert_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2322.DeleteSubsector_koreaFert <- get_data(all_data, "L2322.DeleteSubsector_koreaFert")
    L2322.FinalEnergyKeyword_Fert_korea <- get_data(all_data, "L2322.FinalEnergyKeyword_Fert_korea")
    L2322.Supplysector_Fert_korea <- get_data(all_data, "L2322.Supplysector_Fert_korea")
    L2322.SubsectorLogit_Fert_korea <- get_data(all_data, "L2322.SubsectorLogit_Fert_korea")
    L2322.SubsectorShrwtFllt_Fert_korea <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert_korea")
    L2322.SubsectorInterp_Fert_korea <- get_data(all_data, "L2322.SubsectorInterp_Fert_korea")
    L2322.StubTech_Fert_korea <- get_data(all_data, "L2322.StubTech_Fert_korea")
    L2322.FinalEnergyKeyword_koreaFert <- get_data(all_data, "L2322.FinalEnergyKeyword_koreaFert")
    L2322.SubsectorLogit_koreaFert <- get_data(all_data, "L2322.SubsectorLogit_koreaFert")
    L2322.SubsectorShrwtFllt_koreaFert <- get_data(all_data, "L2322.SubsectorShrwtFllt_koreaFert")
    L2322.SubsectorInterp_koreaFert <- get_data(all_data, "L2322.SubsectorInterp_koreaFert")
    L2322.TechShrwt_koreaFert <- get_data(all_data, "L2322.TechShrwt_koreaFert")
    L2322.Production_koreaFert <- get_data(all_data, "L2322.Production_koreaFert")
    L2322.TechCoef_koreaFert <- get_data(all_data, "L2322.TechCoef_koreaFert")
    L2322.StubTechProd_Fert_korea <- get_data(all_data, "L2322.StubTechProd_Fert_korea")
    L2322.StubTechCoef_Fert_korea <- get_data(all_data, "L2322.StubTechCoef_Fert_korea")
    L2322.StubTechMarket_Fert_korea <- get_data(all_data, "L2322.StubTechMarket_Fert_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/Fert_korea.xml") %>%
      add_xml_data(L2322.DeleteSubsector_koreaFert, "DeleteSubsector") %>%
      add_xml_data(L2322.FinalEnergyKeyword_Fert_korea, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2322.Supplysector_Fert_korea, "Supplysector") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_Fert_korea, "SubsectorLogit") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_Fert_korea, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterp_Fert_korea, "SubsectorInterp") %>%
      add_xml_data(L2322.StubTech_Fert_korea, "StubTech") %>%
      add_xml_data(L2322.FinalEnergyKeyword_koreaFert, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_koreaFert, "SubsectorLogit") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_koreaFert, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterp_koreaFert, "SubsectorInterp") %>%
      add_xml_data(L2322.TechShrwt_koreaFert, "TechShrwt") %>%
      add_xml_data(L2322.Production_koreaFert, "Production") %>%
      add_xml_data(L2322.TechCoef_koreaFert, "TechCoef") %>%
      add_xml_data(L2322.StubTechProd_Fert_korea, "StubTechProd") %>%
      add_xml_data(L2322.StubTechCoef_Fert_korea, "StubTechCoef") %>%
      add_xml_data(L2322.StubTechMarket_Fert_korea, "StubTechMarket") %>%
      add_precursors("L2322.DeleteSubsector_koreaFert",
                     "L2322.FinalEnergyKeyword_Fert_korea",
                     "L2322.Supplysector_Fert_korea",
                     "L2322.SubsectorLogit_Fert_korea",
                     "L2322.SubsectorShrwtFllt_Fert_korea",
                     "L2322.SubsectorInterp_Fert_korea",
                     "L2322.StubTech_Fert_korea",
                     "L2322.FinalEnergyKeyword_koreaFert",
                     "L2322.SubsectorLogit_koreaFert",
                     "L2322.SubsectorShrwtFllt_koreaFert",
                     "L2322.SubsectorInterp_koreaFert",
                     "L2322.TechShrwt_koreaFert",
                     "L2322.Production_koreaFert",
                     "L2322.TechCoef_koreaFert",
                     "L2322.StubTechProd_Fert_korea",
                     "L2322.StubTechCoef_Fert_korea",
                     "L2322.StubTechMarket_Fert_korea") ->
      Fert_korea.xml

    return_data(Fert_korea.xml)
  } else {
    stop("Unknown command")
  }
}
