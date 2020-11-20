#' module_gcam.korea_batch_resources_korea_xml
#'
#' Construct XML data structure for \code{resources_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_korea.xml}.
module_gcam.korea_batch_resources_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.DeleteRenewRsrc_korea_rsrc",
             "L210.DeleteUnlimitRsrc_korea_rsrc",
             "L210.DeleteUnlimitRsrc_korea_rsrc",
             "L210.RenewRsrc_korea",
             "L210.UnlimitRsrc_korea",
             "L210.UnlimitRsrcPrice_korea",
             "L210.SmthRenewRsrcTechChange_korea",
             "L210.SmthRenewRsrcCurves_wind_korea",
             "L210.GrdRenewRsrcCurves_geo_korea",
             "L210.GrdRenewRsrcMax_geo_korea",
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.DeleteRenewRsrc_korea_rsrc <- get_data(all_data, "L210.DeleteRenewRsrc_korea_rsrc")
    L210.DeleteUnlimitRsrc_korea_rsrc <- get_data(all_data, "L210.DeleteUnlimitRsrc_korea_rsrc")
    L210.DeleteUnlimitRsrc_korea_rsrc <- get_data(all_data, "L210.DeleteUnlimitRsrc_korea_rsrc")
    L210.RenewRsrc_korea <- get_data(all_data, "L210.RenewRsrc_korea")
    L210.UnlimitRsrc_korea <- get_data(all_data, "L210.UnlimitRsrc_korea")
    L210.UnlimitRsrcPrice_korea <- get_data(all_data, "L210.UnlimitRsrcPrice_korea")
    L210.SmthRenewRsrcTechChange_korea <- get_data(all_data, "L210.SmthRenewRsrcTechChange_korea")
    L210.SmthRenewRsrcCurves_wind_korea <- get_data(all_data, "L210.SmthRenewRsrcCurves_wind_korea")
    L210.GrdRenewRsrcCurves_geo_korea <- get_data(all_data, "L210.GrdRenewRsrcCurves_geo_korea")
    L210.GrdRenewRsrcMax_geo_korea <- get_data(all_data, "L210.GrdRenewRsrcMax_geo_korea")
    L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea <- get_data(all_data, "L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea")

    # ===================================================

    # Produce outputs
    create_xml("korea/resources_korea.xml") %>%
      add_xml_data(L210.DeleteRenewRsrc_korea_rsrc, "DeleteRenewRsrc") %>%
      add_xml_data(L210.DeleteUnlimitRsrc_korea_rsrc, "DeleteUnlimitRsrc") %>%
      add_xml_data(L210.RenewRsrc_korea, "RenewRsrc") %>%
      add_xml_data(L210.UnlimitRsrc_korea, "UnlimitRsrc") %>%
      add_xml_data(L210.UnlimitRsrcPrice_korea, "UnlimitRsrcPrice") %>%
      add_xml_data(L210.SmthRenewRsrcTechChange_korea, "SmthRenewRsrcTechChange") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_wind_korea, "SmthRenewRsrcCurves") %>%

      ## Jeon ## Start
      #add_xml_data(L210.GrdRenewRsrcCurves_geo_korea, "GrdRenewRsrcCurves") %>%
      #add_xml_data(L210.GrdRenewRsrcMax_geo_korea, "GrdRenewRsrcMax") %>%
      ## Jeon ## End

      add_xml_data(L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea, "SmthRenewRsrcCurvesGdpElast") %>%
      add_precursors("L210.DeleteRenewRsrc_korea_rsrc",
                     "L210.DeleteUnlimitRsrc_korea_rsrc",
                     "L210.DeleteUnlimitRsrc_korea_rsrc",
                     "L210.RenewRsrc_korea",
                     "L210.UnlimitRsrc_korea",
                     "L210.UnlimitRsrcPrice_korea",
                     "L210.SmthRenewRsrcTechChange_korea",
                     "L210.SmthRenewRsrcCurves_wind_korea",
                     "L210.GrdRenewRsrcCurves_geo_korea",
                     "L210.GrdRenewRsrcMax_geo_korea",
                     "L210.SmthRenewRsrcCurvesGdpElast_roofPV_korea") ->
      resources_korea.xml

    return_data(resources_korea.xml)
  } else {
    stop("Unknown command")
  }
}
