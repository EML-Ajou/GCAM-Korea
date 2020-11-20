#' module_gcam.korea_batch_Cstorage_korea_xml
#'
#' Construct XML data structure for \code{Cstorage_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Cstorage_korea.xml}.
module_gcam.korea_batch_Cstorage_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L261.DeleteDepRsrc_USAC_korea",
             "L261.DeleteSubsector_USAC_korea",
             "L261.DepRsrc_FERC_korea",
             "L261.DepRsrcCurves_FERC_korea",
             "L261.Supplysector_C_korea",
             "L261.SubsectorLogit_C_korea",
             "L261.SubsectorShrwtFllt_C_korea",
             "L261.StubTech_C_korea",
             "L261.StubTechMarket_C_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Cstorage_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L261.DeleteDepRsrc_USAC_korea <- get_data(all_data, "L261.DeleteDepRsrc_USAC_korea")
    L261.DeleteSubsector_USAC_korea <- get_data(all_data, "L261.DeleteSubsector_USAC_korea")
    L261.DepRsrc_FERC_korea <- get_data(all_data, "L261.DepRsrc_FERC_korea")
    L261.DepRsrcCurves_FERC_korea <- get_data(all_data, "L261.DepRsrcCurves_FERC_korea")
    L261.Supplysector_C_korea <- get_data(all_data, "L261.Supplysector_C_korea")
    L261.SubsectorLogit_C_korea <- get_data(all_data, "L261.SubsectorLogit_C_korea")
    L261.SubsectorShrwtFllt_C_korea <- get_data(all_data, "L261.SubsectorShrwtFllt_C_korea")
    L261.StubTech_C_korea <- get_data(all_data, "L261.StubTech_C_korea")
    L261.StubTechMarket_C_korea <- get_data(all_data, "L261.StubTechMarket_C_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/Cstorage_korea.xml") %>%
      add_xml_data(L261.DeleteDepRsrc_USAC_korea, "DeleteDepRsrc") %>%
      add_xml_data(L261.DeleteSubsector_USAC_korea, "DeleteSubsector") %>%
      add_xml_data(L261.DepRsrc_FERC_korea, "DepRsrc") %>%
      add_xml_data(L261.DepRsrcCurves_FERC_korea, "DepRsrcCurves") %>%
      add_logit_tables_xml(L261.Supplysector_C_korea, "Supplysector") %>%
      add_logit_tables_xml(L261.SubsectorLogit_C_korea, "SubsectorLogit") %>%
      add_xml_data(L261.SubsectorShrwtFllt_C_korea, "SubsectorShrwtFllt") %>%
      add_xml_data(L261.StubTech_C_korea, "StubTech") %>%
      add_xml_data(L261.StubTechMarket_C_korea, "StubTechMarket") %>%
      add_precursors("L261.DeleteDepRsrc_USAC_korea", "L261.DeleteSubsector_USAC_korea", "L261.DepRsrc_FERC_korea", "L261.DepRsrcCurves_FERC_korea", "L261.Supplysector_C_korea", "L261.SubsectorLogit_C_korea", "L261.SubsectorShrwtFllt_C_korea", "L261.StubTech_C_korea", "L261.StubTechMarket_C_korea") ->
      Cstorage_korea.xml

    return_data(Cstorage_korea.xml)
  } else {
    stop("Unknown command")
  }
}
