#' module_gcam.korea_batch_electd_korea_xml
#'
#' Construct XML data structure for \code{electd_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electd_korea.xml}.
module_gcam.korea_batch_electd_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.DeleteSupplysector_kor_elec_market",
             "L226.Supplysector_electd_korea",
             "L226.SubsectorLogit_electd_korea",
             "L226.SubsectorShrwtFllt_electd_korea",
             "L226.SubsectorInterp_electd_korea",
             "L226.Supplysector_electd_korea",
             "L226.TechShrwt_electd_korea",
             "L226.TechCost_electd_korea",
             "L226.TechCoef_electd_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electd_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L226.DeleteSupplysector_kor_elec_market <- get_data(all_data, "L226.DeleteSupplysector_kor_elec_market")
    L226.Supplysector_electd_korea<- get_data(all_data, "L226.Supplysector_electd_korea")
    L226.SubsectorLogit_electd_korea <- get_data(all_data, "L226.SubsectorLogit_electd_korea")
    L226.SubsectorShrwtFllt_electd_korea <- get_data(all_data, "L226.SubsectorShrwtFllt_electd_korea")
    L226.SubsectorInterp_electd_korea <- get_data(all_data, "L226.SubsectorInterp_electd_korea")
    L226.Supplysector_electd_korea <- get_data(all_data, "L226.Supplysector_electd_korea")
    L226.TechShrwt_electd_korea <- get_data(all_data, "L226.TechShrwt_electd_korea")
    L226.TechCost_electd_korea <- get_data(all_data, "L226.TechCost_electd_korea")
    L226.TechCoef_electd_korea <- get_data(all_data, "L226.TechCoef_electd_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/electd_korea.xml") %>%
      add_xml_data(L226.DeleteSupplysector_kor_elec_market, "DeleteSupplysector") %>%
      add_logit_tables_xml(L226.Supplysector_electd_korea, "Supplysector") %>%
      add_logit_tables_xml(L226.SubsectorLogit_electd_korea, "SubsectorLogit") %>%
      add_xml_data(L226.SubsectorShrwtFllt_electd_korea, "SubsectorShrwtFllt") %>%
      add_xml_data(L226.SubsectorInterp_electd_korea, "SubsectorInterp") %>%
      add_xml_data(L226.TechShrwt_electd_korea, "TechShrwt") %>%
      add_xml_data(L226.TechCost_electd_korea, "TechCost") %>%
      add_xml_data(L226.TechCoef_electd_korea, "TechCoef") %>%
      add_precursors("L226.DeleteSupplysector_kor_elec_market",
                     "L226.Supplysector_electd_korea",
                     "L226.SubsectorLogit_electd_korea",
                     "L226.SubsectorShrwtFllt_electd_korea",
                     "L226.SubsectorInterp_electd_korea",
                     "L226.Supplysector_electd_korea",
                     "L226.TechShrwt_electd_korea",
                     "L226.TechCost_electd_korea",
                     "L226.TechCoef_electd_korea") ->
      electd_korea.xml

    return_data(electd_korea.xml)
  } else {
    stop("Unknown command")
  }
}
