#' module_gcam.korea_batch_socioeconomics_xml
#'
#' Construct XML data structure for \code{socioeconomics_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_korea.xml}.
module_gcam.korea_batch_socioeconomics_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.Pop_GCAM_korea",
             "L201.BaseGDP_GCAM_korea",
             "L201.LaborForceFillout_GCAM_korea",
             "L201.LaborProductivity_GCAM_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.Pop_GCAM_korea <- get_data(all_data, "L201.Pop_GCAM_korea")
    L201.BaseGDP_GCAMU_korea <- get_data(all_data, "L201.BaseGDP_GCAM_korea")
    L201.LaborForceFillout_GCAM_korea <- get_data(all_data, "L201.LaborForceFillout_GCAM_korea")
    L201.LaborProductivity_GCAM_korea <- get_data(all_data, "L201.LaborProductivity_GCAM_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/socioeconomics_korea.xml") %>%
      add_xml_data(L201.Pop_GCAM_korea, "Pop") %>%
      add_xml_data(L201.BaseGDP_GCAMU_korea, "BaseGDP") %>%
      add_xml_data(L201.LaborForceFillout_GCAM_korea, "LaborForceFillout") %>%
      add_xml_data(L201.LaborProductivity_GCAM_korea, "LaborProductivity") %>%
      add_precursors("L201.Pop_GCAM_korea", "L201.BaseGDP_GCAM_korea", "L201.LaborForceFillout_GCAM_korea", "L201.LaborProductivity_GCAM_korea") ->
      socioeconomics_korea.xml

    return_data(socioeconomics_korea.xml)
  } else {
    stop("Unknown command")
  }
}
