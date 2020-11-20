#' module_gcam.korea_batch_HDDCDD_korea.xml
#'
#' Construct XML data structure for \code{HDDCDD_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{HDDCDD_korea.xml}.
module_gcam.korea_batch_HDDCDD_korea.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.HDDCDD_A2_GFDL_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "HDDCDD_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.HDDCDD_A2_GFDL_korea <- get_data(all_data, "L244.HDDCDD_A2_GFDL_korea")

    # Produce outputs
    create_xml("korea/HDDCDD_korea.xml") %>%
      add_xml_data(L244.HDDCDD_A2_GFDL_korea, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_A2_GFDL_korea") ->
      HDDCDD_korea.xml

    return_data(HDDCDD_korea.xml)
  } else {
    stop("Unknown command")
  }
}
