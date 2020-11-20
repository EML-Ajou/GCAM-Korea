#' module_gcam.korea_batch_hydrogen_korea_xml
#'
#' Construct XML data structure for \code{hydrogen_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen_korea.xml}.
module_gcam.korea_batch_hydrogen_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.DeleteSubsector_h2_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hydrogen_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L225.DeleteSubsector_h2_korea <- get_data(all_data, "L225.DeleteSubsector_h2_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/hydrogen_korea.xml") %>%
      add_xml_data(L225.DeleteSubsector_h2_korea, "DeleteSubsector") %>%
      add_precursors("L225.DeleteSubsector_h2_korea") ->
      hydrogen_korea.xml

    return_data(hydrogen_korea.xml)
  } else {
    stop("Unknown command")
  }
}
