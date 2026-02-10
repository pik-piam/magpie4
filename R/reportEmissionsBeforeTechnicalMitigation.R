#' @title reportEmissionsBeforeTechnicalMitigation
#' @description reports GHG emissions before technical mitigation. Technical abatement includes all abatement done in the MACC curves, but exclude endogenous mitigation. These emissions are NOT the standard reporting emissions, but used for special purposes like remind-magpie coupling.
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @return MAgPIE object (Unit: Mt CO2/yr, Mt N2O/yr and Mt CH4/yr)
#' @author Florian Humpenoeder, Benjamin Leon Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- reportEmissionsBeforeTechnicalMitigation(gdx)
#'   }
#'
#' @section Emissions before technical mitigation variables:
#' Name | Unit | Meta
#' ---|---|---
#' Emissions before technical mitigation\|N2O\|Land\|+\|Agriculture | Mt N2O/yr | N2O emissions before MACC abatement
#' Emissions before technical mitigation\|N2O\|Land\|Agriculture\|+\|Animal Waste Management | Mt N2O/yr | N2O from animal waste before mitigation
#' Emissions before technical mitigation\|N2O\|Land\|Agriculture\|+\|Agricultural Soils | Mt N2O/yr | N2O from agricultural soils before mitigation
#' Emissions before technical mitigation\|CH4\|Land\|+\|Agriculture | Mt CH4/yr | CH4 emissions before MACC abatement
#' Emissions before technical mitigation\|CH4\|Land\|Agriculture\|+\|Enteric fermentation | Mt CH4/yr | CH4 from enteric fermentation before mitigation
#' @md


reportEmissionsBeforeTechnicalMitigation <- function(gdx, level = "regglo") {

  x <- NULL
  # N2O, NOx, NH3
  # n_emissions=c("n2o_n","nh3_n","no2_n","no3_n")
  n_emissions <- "n2o_n"
  total <- EmissionsBeforeTechnicalMitigation(gdx, level = level, type = n_emissions, unit = "gas", subcategories = TRUE)
  for (emi in getNames(total, dim = 2)){
    prefix <- paste0("Emissions before technical mitigation|", reportingnames(emi), "|Land")
    a <- total[, , emi]
    emi2 <- reportingnames(emi)
    x <- mbind(x, setNames(dimSums(a, dim = 3),
                           paste0(prefix, "|+|Agriculture (Mt ", emi2, "/yr)")))
    x <- mbind(x, setNames(dimSums(a[, , "awms"], dim = 3),
                           paste0(prefix, "|Agriculture|+|Animal Waste Management (Mt ", emi2, "/yr)")))
    x <- mbind(x, setNames(dimSums(a[, , c("inorg_fert", "man_crop", "resid", "SOM", "rice", "man_past")], dim = 3),
                           paste0(prefix, "|Agriculture|+|Agricultural Soils (Mt ", emi2, "/yr)")))
    x <- mbind(x, setNames(dimSums(a[, , c("inorg_fert", "rice")], dim = 3),
                           paste0(prefix, "|Agriculture|Agricultural Soils|+|Inorganic Fertilizers (Mt ", emi2, "/yr)")))
    x <- mbind(x, setNames(dimSums(a[, , c("man_crop")], dim = 3),
                           paste0(prefix, "|Agriculture|Agricultural Soils|+|Manure applied to Croplands (Mt ", emi2, "/yr)")))
    x <- mbind(x, setNames(dimSums(a[, , c("resid")], dim = 3),
                           paste0(prefix, "|Agriculture|Agricultural Soils|+|Decay of Crop Residues (Mt ", emi2, "/yr)")))
    x <- mbind(x, setNames(dimSums(a[, , c("SOM")], dim = 3),
                           paste0(prefix, "|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss (Mt ", emi2, "/yr)")))
    #  x <- mbind(x,setNames(dimSums(a[, , c("rice")], dim = 3),
    #                     paste0(prefix, "|Agriculture|Agricultural Soils|+|Lower N2O emissions of rice (Mt ", emi2, "/yr)")))
    x <- mbind(x, setNames(dimSums(a[, , c("man_past")], dim = 3),
                           paste0(prefix, "|Agriculture|Agricultural Soils|+|Pasture (Mt ", emi2, "/yr)")))
  }

  #CH4
  a <- collapseNames(EmissionsBeforeTechnicalMitigation(gdx, level = level, type = "ch4", unit = "gas", subcategories = TRUE),
                     collapsedim = 2)
  x <- mbind(x, setNames(dimSums(a, dim = 3), "Emissions before technical mitigation|CH4|Land|+|Agriculture (Mt CH4/yr)"))
  x <- mbind(x, setNames(dimSums(a[, , c("rice")], dim = 3), "Emissions before technical mitigation|CH4|Land|Agriculture|+|Rice (Mt CH4/yr)"))
  x <- mbind(x, setNames(dimSums(a[, , c("awms")], dim = 3), "Emissions before technical mitigation|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr)"))
  x <- mbind(x, setNames(dimSums(a[, , c("ent_ferm")], dim = 3), "Emissions before technical mitigation|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr)"))

  return(x)
}
