#' module_gcam.korea_batch_electricity_korea_xml
#'
#' Construct XML data structure for \code{electricity_korea.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_korea.xml}.
module_gcam.korea_batch_electricity_korea_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE="gcam-korea/A23.subsectorShrwt",
             "L223.PassthroughSector_elec_korea",
             "L223.PassthroughTech_elec_kor_grid",
             "L223.Supplysector_elec_kor_grid",
             "L223.SubsectorShrwtFllt_elec_kor_grid",
             "L223.SubsectorInterp_elec_kor_grid",
             "L223.SubsectorLogit_elec_kor_grid",
             "L223.TechShrwt_elec_kor_grid",
             "L223.TechCoef_elec_kor_grid",
             "L223.Production_elec_kor_grid",
             "L223.InterestRate_kor_grid",
             "L223.Pop_kor_grid",
             "L223.BaseGDP_kor_grid",
             "L223.LaborForceFillout_kor_grid",
             "L223.Supplysector_elec_korea",
             "L223.ElecReserve_korea",
             "L223.SubsectorLogit_elec_korea",
             "L223.SubsectorShrwtFllt_elec_korea",
             #"L223.SubsectorShrwt_nuc_korea",
             "L223.SubsectorShrwt_renew_korea",
             "L223.SubsectorInterp_elec_korea",
             "L223.SubsectorInterpTo_elec_korea",
             "L223.StubTech_elec_korea",
             "L223.StubTechEff_elec_korea",
             "L223.StubTechCapFactor_elec_korea",
             "L223.StubTechFixOut_elec_korea",
             "L223.StubTechFixOut_hydro_korea",
             "L223.StubTechFixOut_nuclear_korea",
             "L223.StubTechProd_elec_korea",
             "L223.StubTechMarket_elec_korea",
             "L223.StubTechMarket_backup_korea",
             "L223.StubTechElecMarket_backup_korea",
             "L223.StubTechCapFactor_elec_wind_korea",
             "L223.StubTechCapFactor_elec_solar_korea",
             "L2232.DeleteSupplysector_kor_elec_market",
             "L2232.Supplysector_kor_elec_market",
             "L2232.SubsectorShrwtFllt_kor_elec_market",
             "L2232.SubsectorInterp_kor_elec_market",
             "L2232.SubsectorLogit_kor_elec_market",
             "L2232.TechShrwt_kor_elec_market",
             "L2232.TechCoef_kor_elec_market",
             "L2232.Production_exports_kor_elec_market",
             "L2232.Supplysector_elec_kor_grid",
             "L2232.ElecReserve_kor_grid",
             "L2232.SubsectorShrwtFllt_elec_kor_grid",
             "L2232.SubsectorInterp_elec_kor_grid",
             "L2232.SubsectorLogit_elec_kor_grid",
             "L2232.TechShrwt_elec_kor_grid",
             "L2232.TechCoef_elec_kor_grid",
             "L2232.TechCoef_elecownuse_kor_grid",
             "L2232.Production_imports_kor_grid",
             "L2232.Production_elec_gen_kor_grid",
             "L2232.StubTechElecMarket_backup_korea"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_korea.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    passthrough.sector <- technology <- share.weight <- NULL # silence package check notes

    # Load required inputs

    A23.subsectorShrwt <- get_data(all_data, "gcam-korea/A23.subsectorShrwt")
    L223.PassthroughSector_elec_korea <- get_data(all_data, "L223.PassthroughSector_elec_korea")
    L223.PassthroughTech_elec_kor_grid <- get_data(all_data, "L223.PassthroughTech_elec_kor_grid")
    L223.Supplysector_elec_kor_grid <- get_data(all_data, "L223.Supplysector_elec_kor_grid")
    L223.SubsectorShrwtFllt_elec_kor_grid <- get_data(all_data, "L223.SubsectorShrwtFllt_elec_kor_grid")
    L223.SubsectorInterp_elec_kor_grid <- get_data(all_data, "L223.SubsectorInterp_elec_kor_grid")
    L223.SubsectorLogit_elec_kor_grid <- get_data(all_data, "L223.SubsectorLogit_elec_kor_grid")
    L223.TechShrwt_elec_kor_grid <- get_data(all_data, "L223.TechShrwt_elec_kor_grid")
    L223.TechCoef_elec_kor_grid <- get_data(all_data, "L223.TechCoef_elec_kor_grid")
    L223.Production_elec_kor_grid <- get_data(all_data, "L223.Production_elec_kor_grid")
    L223.InterestRate_kor_grid <- get_data(all_data, "L223.InterestRate_kor_grid")
    L223.Pop_kor_grid <- get_data(all_data, "L223.Pop_kor_grid")
    L223.BaseGDP_kor_grid <- get_data(all_data, "L223.BaseGDP_kor_grid")
    L223.LaborForceFillout_kor_grid <- get_data(all_data, "L223.LaborForceFillout_kor_grid")
    L223.Supplysector_elec_korea <- get_data(all_data, "L223.Supplysector_elec_korea")
    L223.ElecReserve_korea <- get_data(all_data, "L223.ElecReserve_korea")
    L223.SubsectorLogit_elec_korea <- get_data(all_data, "L223.SubsectorLogit_elec_korea")
    L223.SubsectorShrwtFllt_elec_korea <- get_data(all_data, "L223.SubsectorShrwtFllt_elec_korea")
    #L223.SubsectorShrwt_nuc_korea <- get_data(all_data, "L223.SubsectorShrwt_nuc_korea")
    L223.SubsectorShrwt_renew_korea <- get_data(all_data, "L223.SubsectorShrwt_renew_korea")
    L223.SubsectorInterp_elec_korea <- get_data(all_data, "L223.SubsectorInterp_elec_korea")
    L223.SubsectorInterpTo_elec_korea <- get_data(all_data, "L223.SubsectorInterpTo_elec_korea")
    L223.StubTech_elec_korea <- get_data(all_data, "L223.StubTech_elec_korea")
    L223.StubTechEff_elec_korea <- get_data(all_data, "L223.StubTechEff_elec_korea")
    L223.StubTechCapFactor_elec_korea <- get_data(all_data, "L223.StubTechCapFactor_elec_korea")
    L223.StubTechFixOut_elec_korea <- get_data(all_data, "L223.StubTechFixOut_elec_korea")
    L223.StubTechFixOut_hydro_korea <- get_data(all_data, "L223.StubTechFixOut_hydro_korea")
    L223.StubTechFixOut_nuclear_korea <- get_data(all_data, "L223.StubTechFixOut_nuclear_korea")
    L223.StubTechProd_elec_korea <- get_data(all_data, "L223.StubTechProd_elec_korea")
    L223.StubTechMarket_elec_korea <- get_data(all_data, "L223.StubTechMarket_elec_korea")
    L223.StubTechMarket_backup_korea <- get_data(all_data, "L223.StubTechMarket_backup_korea")
    L223.StubTechElecMarket_backup_korea <- get_data(all_data, "L223.StubTechElecMarket_backup_korea")
    L223.StubTechCapFactor_elec_wind_korea <- get_data(all_data, "L223.StubTechCapFactor_elec_wind_korea")
    L223.StubTechCapFactor_elec_solar_korea <- get_data(all_data, "L223.StubTechCapFactor_elec_solar_korea")
    L2232.DeleteSupplysector_kor_elec_market <- get_data(all_data, "L2232.DeleteSupplysector_kor_elec_market")
    L2232.Supplysector_kor_elec_market <- get_data(all_data, "L2232.Supplysector_kor_elec_market")
    L2232.SubsectorShrwtFllt_kor_elec_market <- get_data(all_data, "L2232.SubsectorShrwtFllt_kor_elec_market")
    L2232.SubsectorInterp_kor_elec_market <- get_data(all_data, "L2232.SubsectorInterp_kor_elec_market")
    L2232.SubsectorLogit_kor_elec_market <- get_data(all_data, "L2232.SubsectorLogit_kor_elec_market")
    L2232.TechShrwt_kor_elec_market <- get_data(all_data, "L2232.TechShrwt_kor_elec_market")
    L2232.TechCoef_kor_elec_market <- get_data(all_data, "L2232.TechCoef_kor_elec_market")
    L2232.Production_exports_kor_elec_market <- get_data(all_data, "L2232.Production_exports_kor_elec_market")
    L2232.Supplysector_elec_kor_grid <- get_data(all_data, "L2232.Supplysector_elec_kor_grid")
    L2232.ElecReserve_kor_grid <- get_data(all_data, "L2232.ElecReserve_kor_grid")
    L2232.SubsectorShrwtFllt_elec_kor_grid <- get_data(all_data, "L2232.SubsectorShrwtFllt_elec_kor_grid")
    L2232.SubsectorInterp_elec_kor_grid <- get_data(all_data, "L2232.SubsectorInterp_elec_kor_grid")
    L2232.SubsectorLogit_elec_kor_grid <- get_data(all_data, "L2232.SubsectorLogit_elec_kor_grid")
    L2232.TechShrwt_elec_kor_grid <- get_data(all_data, "L2232.TechShrwt_elec_kor_grid")
    L2232.TechCoef_elec_kor_grid <- get_data(all_data, "L2232.TechCoef_elec_kor_grid")
    L2232.TechCoef_elecownuse_kor_grid <- get_data(all_data, "L2232.TechCoef_elecownuse_kor_grid")
    L2232.Production_imports_kor_grid <- get_data(all_data, "L2232.Production_imports_kor_grid")
    L2232.Production_elec_gen_kor_grid <- get_data(all_data, "L2232.Production_elec_gen_kor_grid")
    L2232.StubTechElecMarket_backup_korea <- get_data(all_data, "L2232.StubTechElecMarket_backup_korea")

    # ===================================================
    # Rename tibble columns to match the L2 data names.
    L223.PassthroughSector_elec_korea <- rename(L223.PassthroughSector_elec_korea, pass.through.sector = passthrough.sector)
    L223.PassthroughTech_elec_kor_grid  <- rename(L223.PassthroughTech_elec_kor_grid, pass.through.technology = technology)
    L223.StubTechProd_elec_korea      <- rename(L223.StubTechProd_elec_korea, tech.share.weight = share.weight)

    # Produce outputs
    create_xml("korea/electricity_korea.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L223.PassthroughSector_elec_korea, "PassThroughSector") %>%
      add_xml_data(L223.PassthroughTech_elec_kor_grid, "PassThroughTech") %>%
      add_logit_tables_xml(L223.Supplysector_elec_kor_grid, "Supplysector") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec_kor_grid, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.SubsectorInterp_elec_kor_grid, "SubsectorInterp") %>%
      add_logit_tables_xml(L223.SubsectorLogit_elec_kor_grid, "SubsectorLogit") %>%
      add_xml_data(L223.TechShrwt_elec_kor_grid, "TechShrwt") %>%
      add_xml_data(L223.TechCoef_elec_kor_grid, "TechCoef") %>%
      add_xml_data(L223.Production_elec_kor_grid, "Production") %>%
      add_xml_data(L223.InterestRate_kor_grid, "InterestRate") %>%
      add_xml_data(L223.Pop_kor_grid, "Pop") %>%
      add_xml_data(L223.BaseGDP_kor_grid, "BaseGDP") %>%
      add_xml_data(L223.LaborForceFillout_kor_grid, "LaborForceFillout") %>%
      add_logit_tables_xml(L223.Supplysector_elec_korea, "Supplysector") %>%
      add_xml_data(L223.ElecReserve_korea, "ElecReserve") %>%
      add_logit_tables_xml(L223.SubsectorLogit_elec_korea, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec_korea, "SubsectorShrwtFllt") %>%
      #add_xml_data(L223.SubsectorShrwt_nuc_korea, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_renew_korea, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorInterp_elec_korea, "SubsectorInterp") %>%
      add_xml_data(L223.SubsectorInterpTo_elec_korea, "SubsectorInterpTo") %>%
      add_xml_data(L223.StubTech_elec_korea, "StubTech") %>%
      add_xml_data(L223.StubTechEff_elec_korea, "StubTechEff") %>%
      add_xml_data(L223.StubTechCapFactor_elec_korea, "StubTechCapFactor") %>%
      add_xml_data(L223.StubTechFixOut_elec_korea, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechFixOut_hydro_korea, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechFixOut_nuclear_korea, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechProd_elec_korea, "StubTechProd") %>%
      add_xml_data(L223.StubTechMarket_elec_korea, "StubTechMarket") %>%
      add_xml_data(L223.StubTechMarket_backup_korea, "StubTechMarket") %>%
      add_xml_data(L223.StubTechElecMarket_backup_korea, "StubTechElecMarket") %>%
      add_xml_data(L223.StubTechCapFactor_elec_wind_korea, "StubTechCapFactor") %>%
      add_xml_data(L223.StubTechCapFactor_elec_solar_korea, "StubTechCapFactor") %>%
      add_xml_data(L2232.DeleteSupplysector_kor_elec_market, "DeleteSupplysector") %>%
      add_logit_tables_xml(L2232.Supplysector_kor_elec_market, "Supplysector") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_kor_elec_market, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_kor_elec_market, "SubsectorInterp") %>%
      add_logit_tables_xml(L2232.SubsectorLogit_kor_elec_market, "SubsectorLogit") %>%
      add_xml_data(L2232.TechShrwt_kor_elec_market, "TechShrwt") %>%
      add_xml_data(L2232.TechCoef_kor_elec_market, "TechCoef") %>%
      add_xml_data(L2232.Production_exports_kor_elec_market, "Production") %>%
      add_logit_tables_xml(L2232.Supplysector_elec_kor_grid, "Supplysector") %>%
      add_xml_data(L2232.ElecReserve_kor_grid, "ElecReserve") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_elec_kor_grid, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_elec_kor_grid, "SubsectorInterp") %>%
      add_logit_tables_xml(L2232.SubsectorLogit_elec_kor_grid, "SubsectorLogit") %>%
      add_xml_data(L2232.TechShrwt_elec_kor_grid, "TechShrwt") %>%
      add_xml_data(L2232.TechCoef_elec_kor_grid, "TechCoef") %>%
      add_xml_data(L2232.TechCoef_elecownuse_kor_grid, "TechCoef") %>%
      add_xml_data(L2232.Production_imports_kor_grid, "Production") %>%
      add_xml_data(L2232.Production_elec_gen_kor_grid, "Production") %>%
      add_xml_data(L2232.StubTechElecMarket_backup_korea, "StubTechElecMarket") %>%
      add_xml_data(A23.subsectorShrwt, "SubsectorShrwt") %>%
      add_precursors("gcam-korea/A23.subsectorShrwt",
                     "L223.PassthroughSector_elec_korea",
                     "L223.PassthroughTech_elec_kor_grid",
                     "L223.Supplysector_elec_kor_grid",
                     "L223.SubsectorShrwtFllt_elec_kor_grid",
                     "L223.SubsectorInterp_elec_kor_grid",
                     "L223.SubsectorLogit_elec_kor_grid",
                     "L223.TechShrwt_elec_kor_grid",
                     "L223.TechCoef_elec_kor_grid",
                     "L223.Production_elec_kor_grid",
                     "L223.InterestRate_kor_grid",
                     "L223.Pop_kor_grid",
                     "L223.BaseGDP_kor_grid",
                     "L223.LaborForceFillout_kor_grid",
                     "L223.Supplysector_elec_korea",
                     "L223.ElecReserve_korea",
                     "L223.SubsectorLogit_elec_korea",
                     "L223.SubsectorShrwtFllt_elec_korea",
                     #"L223.SubsectorShrwt_nuc_korea",
                     "L223.SubsectorShrwt_renew_korea",
                     "L223.SubsectorInterp_elec_korea",
                     "L223.SubsectorInterpTo_elec_korea",
                     "L223.StubTech_elec_korea",
                     "L223.StubTechEff_elec_korea",
                     "L223.StubTechCapFactor_elec_korea",
                     "L223.StubTechFixOut_elec_korea",
                     "L223.StubTechFixOut_hydro_korea",
                     "L223.StubTechFixOut_nuclear_korea",
                     "L223.StubTechProd_elec_korea",
                     "L223.StubTechMarket_elec_korea",
                     "L223.StubTechMarket_backup_korea",
                     "L223.StubTechElecMarket_backup_korea",
                     "L223.StubTechCapFactor_elec_wind_korea",
                     "L223.StubTechCapFactor_elec_solar_korea",
                     "L2232.DeleteSupplysector_kor_elec_market",
                     "L2232.Supplysector_kor_elec_market",
                     "L2232.SubsectorShrwtFllt_kor_elec_market",
                     "L2232.SubsectorInterp_kor_elec_market",
                     "L2232.SubsectorLogit_kor_elec_market",
                     "L2232.TechShrwt_kor_elec_market",
                     "L2232.TechCoef_kor_elec_market",
                     "L2232.Production_exports_kor_elec_market",
                     "L2232.Supplysector_elec_kor_grid",
                     "L2232.ElecReserve_kor_grid",
                     "L2232.SubsectorShrwtFllt_elec_kor_grid",
                     "L2232.SubsectorInterp_elec_kor_grid",
                     "L2232.SubsectorLogit_elec_kor_grid",
                     "L2232.TechShrwt_elec_kor_grid",
                     "L2232.TechCoef_elec_kor_grid",
                     "L2232.TechCoef_elecownuse_kor_grid",
                     "L2232.Production_imports_kor_grid",
                     "L2232.Production_elec_gen_kor_grid",
                     "L2232.StubTechElecMarket_backup_korea") ->
      electricity_korea.xml

    return_data(electricity_korea.xml)
  } else {
    stop("Unknown command")
  }
}
