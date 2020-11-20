#' module_gcam.korea_batch_interest_rate_korea_xml
#'
#' Construct XML data structure for \code{interest_rate_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{interest_rate_korea.xml}.
module_gcam.korea_batch_interest_rate_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.InterestRate_GCAM_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "interest_rate_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.InterestRate_GCAM_korea <- get_data(all_data, "L201.InterestRate_GCAM_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/interest_rate_korea.xml") %>%
      add_xml_data(L201.InterestRate_GCAM_korea, "InterestRate") %>%
      add_precursors("L201.InterestRate_GCAM_korea") ->
      interest_rate_korea.xml

    return_data(interest_rate_korea.xml)
  } else {
    stop("Unknown command")
  }
}
