#' module_gcam.korea_batch_en_transformation_korea_xml
#'
#' Construct XML data structure for \code{en_transformation_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation_korea.xml}.
module_gcam.korea_batch_en_transformation_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.DeleteStubTech_koreaEn",
             "L222.PassThroughSector_koreaEn",
             "L222.SubsectorLogit_en_korea",
             "L222.StubTech_en_korea",
             "L222.StubTechCoef_refining_korea",
             "L222.GlobalTechInterp_en_korea",
             "L222.GlobalTechCoef_en_korea",
             "L222.GlobalTechCost_en_korea",
             "L222.GlobalTechShrwt_en_korea",
             "L222.GlobalTechCapture_en_korea",
             "L222.GlobalTechSCurve_en_korea",
             "L222.Tech_koreaEn",
             "L222.TechShrwt_koreaEn",
             "L222.TechInterp_koreaEn",
             "L222.TechShrwt_koreaEn",
             "L222.TechCoef_koreaEn",
             "L222.Production_koreaRefining",
             "L222.Supplysector_en_korea",
             "L222.SubsectorShrwtFllt_en_korea",
             "L222.StubTechProd_refining_korea",
             "L222.StubTechMarket_en_korea",
             "L222.CarbonCoef_en_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_transformation_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.DeleteStubTech_koreaEn <- get_data(all_data, "L222.DeleteStubTech_koreaEn")
    L222.PassThroughSector_koreaEn <- get_data(all_data, "L222.PassThroughSector_koreaEn")
    L222.SubsectorLogit_en_korea <- get_data(all_data, "L222.SubsectorLogit_en_korea")
    L222.StubTech_en_korea <- get_data(all_data, "L222.StubTech_en_korea")
    L222.StubTechCoef_refining_korea <- get_data(all_data, "L222.StubTechCoef_refining_korea")
    L222.GlobalTechInterp_en_korea <- get_data(all_data, "L222.GlobalTechInterp_en_korea")
    L222.GlobalTechCoef_en_korea <- get_data(all_data, "L222.GlobalTechCoef_en_korea")
    L222.GlobalTechCost_en_korea <- get_data(all_data, "L222.GlobalTechCost_en_korea")
    L222.GlobalTechShrwt_en_korea <- get_data(all_data, "L222.GlobalTechShrwt_en_korea")
    L222.GlobalTechCapture_en_korea <- get_data(all_data, "L222.GlobalTechCapture_en_korea")
    L222.GlobalTechSCurve_en_korea <- get_data(all_data, "L222.GlobalTechSCurve_en_korea")
    L222.Tech_koreaEn <- get_data(all_data, "L222.Tech_koreaEn")
    L222.TechShrwt_koreaEn <- get_data(all_data, "L222.TechShrwt_koreaEn")
    L222.TechInterp_koreaEn <- get_data(all_data, "L222.TechInterp_koreaEn")
    L222.TechShrwt_koreaEn <- get_data(all_data, "L222.TechShrwt_koreaEn")
    L222.TechCoef_koreaEn <- get_data(all_data, "L222.TechCoef_koreaEn")
    L222.Production_koreaRefining <- get_data(all_data, "L222.Production_koreaRefining")
    L222.Supplysector_en_korea <- get_data(all_data, "L222.Supplysector_en_korea")
    L222.SubsectorShrwtFllt_en_korea <- get_data(all_data, "L222.SubsectorShrwtFllt_en_korea")
    L222.StubTechProd_refining_korea <- get_data(all_data, "L222.StubTechProd_refining_korea")
    L222.StubTechMarket_en_korea <- get_data(all_data, "L222.StubTechMarket_en_korea")
    L222.CarbonCoef_en_korea <- get_data(all_data, "L222.CarbonCoef_en_korea")

    technology <- year <- NULL # Silence package checks
    # ===================================================
    # Rename tibble columns to match the header information.
    L222.Tech_koreaEn <- rename(L222.Tech_koreaEn, pass.through.technology = technology)
    L222.SubsectorShrwtFllt_en_korea <- rename(L222.SubsectorShrwtFllt_en_korea, year.fillout = year)

    # Produce outputs
    create_xml("korea/en_transformation_korea.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L222.DeleteStubTech_koreaEn, "DeleteStubTech") %>%
      add_xml_data(L222.PassThroughSector_koreaEn, "PassThroughSector") %>%
      add_logit_tables_xml(L222.SubsectorLogit_en_korea, "SubsectorLogit") %>%
      add_xml_data(L222.StubTech_en_korea, "StubTech") %>%
      add_xml_data(L222.StubTechCoef_refining_korea, "StubTechCoef") %>%
      add_xml_data(L222.GlobalTechInterp_en_korea, "GlobalTechInterp") %>%
      add_xml_data(L222.GlobalTechCoef_en_korea, "GlobalTechCoef") %>%
      add_xml_data(L222.GlobalTechCost_en_korea, "GlobalTechCost") %>%
      add_xml_data(L222.GlobalTechShrwt_en_korea, "GlobalTechShrwt") %>%
      add_xml_data(L222.GlobalTechCapture_en_korea, "GlobalTechCapture") %>%
      add_xml_data(L222.GlobalTechSCurve_en_korea, "GlobalTechSCurve") %>%
      add_xml_data(L222.Tech_koreaEn, "PassThroughTech") %>%
      add_xml_data(L222.TechInterp_koreaEn, "TechInterp") %>%
      add_xml_data(L222.TechShrwt_koreaEn, "TechShrwt") %>%
      add_xml_data(L222.TechShrwt_koreaEn, "TechShrwt") %>%
      add_xml_data(L222.TechCoef_koreaEn, "TechCoef") %>%
      add_xml_data(L222.Production_koreaRefining, "Production") %>%
      add_logit_tables_xml(L222.Supplysector_en_korea, "Supplysector") %>%
      add_xml_data(L222.SubsectorShrwtFllt_en_korea, "SubsectorShrwtFllt") %>%
      add_xml_data(L222.StubTechProd_refining_korea, "StubTechProd") %>%
      add_xml_data(L222.StubTechMarket_en_korea, "StubTechMarket") %>%
      add_xml_data(L222.CarbonCoef_en_korea, "CarbonCoef") %>%
      add_precursors("L222.DeleteStubTech_koreaEn",
                     "L222.PassThroughSector_koreaEn",
                     "L222.SubsectorLogit_en_korea",
                     "L222.StubTech_en_korea",
                     "L222.StubTechCoef_refining_korea",
                     "L222.GlobalTechInterp_en_korea",
                     "L222.GlobalTechCoef_en_korea",
                     "L222.GlobalTechCost_en_korea",
                     "L222.GlobalTechShrwt_en_korea",
                     "L222.GlobalTechCapture_en_korea",
                     "L222.GlobalTechSCurve_en_korea",
                     "L222.Tech_koreaEn",
                     "L222.TechShrwt_koreaEn",
                     "L222.TechInterp_koreaEn",
                     "L222.TechShrwt_koreaEn",
                     "L222.TechCoef_koreaEn",
                     "L222.Production_koreaRefining",
                     "L222.Supplysector_en_korea",
                     "L222.SubsectorShrwtFllt_en_korea",
                     "L222.StubTechProd_refining_korea",
                     "L222.StubTechMarket_en_korea",
                     "L222.CarbonCoef_en_korea") ->
      en_transformation_korea.xml

    return_data(en_transformation_korea.xml)
  } else {
    stop("Unknown command")
  }
}
