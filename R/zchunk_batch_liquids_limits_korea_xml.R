#' module_gcam.korea_batch_liquids_limits_korea_xml
#'
#' Construct XML data structure for \code{liquids_limits_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{liquids_limits_korea.xml}.
module_gcam.korea_batch_liquids_limits_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.CreditMkt_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "liquids_limits_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.CreditMkt_korea <- get_data(all_data, "L270.CreditMkt_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/liquids_limits_korea.xml") %>%
      add_xml_data(L270.CreditMkt_korea, "PortfolioStd") %>%
      add_precursors("L270.CreditMkt_korea") ->
      liquids_limits_korea.xml

    return_data(liquids_limits_korea.xml)
  } else {
    stop("Unknown command")
  }
}
