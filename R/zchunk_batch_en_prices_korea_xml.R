#' module_gcam.korea_batch_en_prices_korea_xml
#'
#' Construct XML data structure for \code{en_prices_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_prices_korea.xml}.
module_gcam.korea_batch_en_prices_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.Supplysector_en_korea",
             "L226.SubsectorShrwtFllt_en_korea",
             "L226.SubsectorLogit_en_korea",
             "L226.TechShrwt_en_korea",
             "L226.TechCoef_en_korea",
             "L226.TechCost_en_korea",
             "L226.Ccoef_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_prices_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    compVal <- passthrough.sector <- share.weight <-
      supplysector <- technology <- NULL # silence package check notes

    # Load required inputs
    L226.Supplysector_en_korea <- get_data(all_data, "L226.Supplysector_en_korea")
    L226.SubsectorShrwtFllt_en_korea <- get_data(all_data, "L226.SubsectorShrwtFllt_en_korea")
    L226.SubsectorLogit_en_korea <- get_data(all_data, "L226.SubsectorLogit_en_korea")
    L226.TechShrwt_en_korea <- get_data(all_data, "L226.TechShrwt_en_korea")
    L226.TechCoef_en_korea <- get_data(all_data, "L226.TechCoef_en_korea")
    L226.TechCost_en_korea <- get_data(all_data, "L226.TechCost_en_korea")
    L226.Ccoef_korea <- get_data(all_data, "L226.Ccoef_korea")

    # ===================================================
    # Rename tibble columns to match the L2 data header information.
    L226.Ccoef_korea <- rename(L226.Ccoef_korea, PrimaryFuelCO2Coef.name = supplysector)

    # Produce outputs
    create_xml("korea/en_prices_korea.xml") %>%
      add_logit_tables_xml(L226.Supplysector_en_korea, "Supplysector") %>%
      add_xml_data(L226.SubsectorShrwtFllt_en_korea, "SubsectorShrwtFllt") %>%
      add_logit_tables_xml(L226.SubsectorLogit_en_korea, "SubsectorLogit") %>%
      add_xml_data(L226.TechShrwt_en_korea, "TechShrwt") %>%
      add_xml_data(L226.TechCoef_en_korea, "TechCoef") %>%
      add_xml_data(L226.TechCost_en_korea, "TechCost") %>%
      add_xml_data(L226.Ccoef_korea, "CarbonCoef") %>%
      add_precursors("L226.Supplysector_en_korea",
                     "L226.SubsectorShrwtFllt_en_korea",
                     "L226.SubsectorLogit_en_korea",
                     "L226.TechShrwt_en_korea",
                     "L226.TechCoef_en_korea",
                     "L226.TechCost_en_korea",
                     "L226.Ccoef_korea") ->
      en_prices_korea.xml

    return_data(en_prices_korea.xml)
  } else {
    stop("Unknown command")
  }
}
