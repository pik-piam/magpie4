#' @title getReportFableScenathon
#' @description Collects outputs from MAgPIE runs for FABLE Scenathon.
#'
#' @export
#'
#' @param gdx a GDX file
#' @param iso country/region selection. Default `NULL`, i.e. all `regglo` reporting
#' @param file a file name the output should be written to using write.report.
#' If `NULL` the report is returned instead as a MAgPIE object. For the easier
#' reporting in Scenathon tabs, a .csv file extension is recommenended.
#' @author Miodrag Stevanovic
#' @importFrom magclass write.magpie setNames getItems mbind add_columns
#' @examples
#'
#'   \dontrun{
#'     x <- getReportFableScenathon(gdx, file = "magpie2scenathon.csv", iso = "IND")
#'   }
#'

getReportFableScenathon <- function(gdx, file = NULL, iso = NULL) {

  # Income ------------------------------------------------------------------
  GDP <- reportIncome(gdx)[iso,,"Total income (million US$2017 PPP/yr)"]
  GDP <- setNames(GDP, "GPD")


  # Population --------------------------------------------------------------
  Poplulation <- reportPopulation(gdx)[iso,,]
  Poplulation <- setNames(Poplulation, "Population")


  # Kcal --------------------------------------------------------------------
  kcal_targ <- reportKcal(gdx)[iso,,"Nutrition|Calorie Supply (kcal/capita/day)"]
  kcal_targ <- setNames(kcal_targ, "kcal_targ")


  # Land Resources ----------------------------------------------------------
  tabs <- c(TotalLand = "Resources|Land Cover (million ha)",
           CalcCropland = "Resources|Land Cover|+|Cropland (million ha)",
           CalcPasture = "Resources|Land Cover|+|Pastures and Rangelands (million ha)",
           CalcUrban = "Resources|Land Cover|+|Urban Area (million ha)",
           CalcOtherLand = "Resources|Land Cover|+|Other Land (million ha)",
           CalcForest = "Resources|Land Cover|+|Forest (million ha)")
  Landuse <- reportLandUse(gdx)[iso,,tabs]
  for(i in 1:length(getItems(Landuse, dim=3))){
    dimnames(Landuse)[[3]][i] <- names(tabs[which(tabs==getItems(Landuse, dim=3)[i])])
  }

  tabs <- c(CalcNewForest = "Resources|Land Cover Change|+|Forest (million ha wrt 1995)",
            NewForestChange = "Resources|Land Cover Change|Forest|+|Managed Forest (million ha wrt 1995)")
  LanduseChange <- reportLandUseChange(gdx, baseyear = 1995)[iso,,tabs]
  for(i in 1:length(getItems(LanduseChange, dim=3))){
    dimnames(LanduseChange)[[3]][i] <- names(tabs[which(tabs==getItems(LanduseChange, dim=3)[i])])
  }
  LanduseChange <- add_columns(LanduseChange, dim = 3, addnm = "NetForestChange")
  LanduseChange[,,"NetForestChange"] <- setNames(LanduseChange[,,"NewForestChange"], NULL)

  tabs <- c("Resources|Land Cover|Forest|Natural Forest|Primary Forest|Protected (million ha)",
            "Resources|Land Cover|Forest|Natural Forest|Secondary Forest|Protected (million ha)",
            "Resources|Land Cover|Other Land|Protected (million ha)")
  tmp <- reportProtectedArea(gdx)[iso,,tabs]
  ProtectedLand <- tmp[,,2:3]
  ProtectedLand[,,1] <- tmp[,,1] + tmp[,,2]
  dimnames(ProtectedLand)[[3]][1] <- "ProtectedAreasForest"
  ProtectedLand[,,2] <- tmp[,,3]
  dimnames(ProtectedLand)[[3]][2] <- "ProtectedAreasOther"


  # Emissions ---------------------------------------------------------------
  Emissions <- reportEmissions(gdx)[iso,,]

  tabs <- c(CalcCropCO2 = "Emissions|CO2|Land (Mt CO2/yr)",
            CalcAllAgriCO2e = "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)",
            CalcDeforCO2 = "Emissions|CO2|Land|Land-use Change|+|Gross LUC (Mt CO2/yr)",
            CalcSequestCO2 = "Emissions|CO2|Land|Land-use Change|+|Regrowth (Mt CO2/yr)",
            CalcAllLandCO2e = "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)")
  EmissionsCO2 <- Emissions[,,tabs]
  for(i in 1:length(getItems(EmissionsCO2, dim=3))){
    dimnames(EmissionsCO2)[[3]][i] <- names(tabs[i])
  }
  EmissionsCO2 <- add_columns(EmissionsCO2, dim = 3, addnm = "CalcOtherLUCCO2")
  EmissionsCO2[,,"CalcOtherLUCCO2"] <- setNames(EmissionsCO2[,,"CalcDeforCO2"], NULL)

  tabs <- c(CalcCropN2O = "Emissions|N2O|Land|Agriculture|+|Agricultural Soils (Mt N2O/yr)",
            CalcLiveN2O = "Emissions|N2O|Land|Agriculture|+|Animal Waste Management (Mt N2O/yr)")
  EmissionsN2O <- Emissions[,,tabs] * 265
  for(i in 1:length(getItems(EmissionsN2O, dim=3))){
    dimnames(EmissionsN2O)[[3]][i] <- names(tabs[i])
  }


  tabs <- c("Emissions|CH4|Land|Agriculture|+|Rice (Mt CH4/yr)",
            "Emissions|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr)",
            "Emissions|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr)")
  tmp <- Emissions[,,tabs] * 28
  EmissionsCH4 <- tmp[,,1:2]
  dimnames(EmissionsCH4)[[3]][1] <- "CalcCropCH4"
  EmissionsCH4[,,2] <- tmp[,,2] + tmp[,,3]
  dimnames(EmissionsCH4)[[3]][2] <- "CalcLiveCH4"

  # SDGs --------------------------------------------------------------------
  tabs <- c(BiodivTarget = "SDG|SDG15|Terrestrial biodiversity (index)",
            ForestShare = "SDG|SDG15|Forest share (share of total land)",
            Non_agricultural_land = "SDG|SDG15|Non-agricultural land share (share of total land)",
            Other_natural_land = "SDG|SDG15|Other natural land share (share of total land)",
            Primary_forest_share = "SDG|SDG15|Primary forest share (share of total land)")
  SDG15 <- reportSDG15(gdx)[iso,,tabs]
  for(i in 1:length(getItems(SDG15, dim=3))){
    dimnames(SDG15)[[3]][i] <- names(tabs[which(tabs==getItems(SDG15, dim=3)[i])])
  }

  # Output ------------------------------------------------------------------


  out <- mbind(GDP, Poplulation, kcal_targ, Landuse, LanduseChange, ProtectedLand,
               EmissionsCO2, EmissionsN2O, EmissionsCH4, SDG15)

  if (!is.null(file)) {
    write.magpie(out, file_name = file)
  } else {
    return(out)
  }
}
