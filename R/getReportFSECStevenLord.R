#' @title getReportFSECStevenLord
#' @description Reports cropland, pasture, and nitrogen pollution indicators for the FSEC project
#' (specifically Steven Lord)
#'
#' @export
#'
#' @param gdx a GDX file
#' @param reportOutputDir a folder name for the output to be written to. If NULL the report is not saved to
#' disk, and only returned to the calling function.
#' @param magpieOutputDir a magpie output directory which contains a mapping file (clustermap*.rds) for the
#' disaggregation of grid output
#' @param scenario the name of the scenario used. If NULL the report is not saved to disk, and only returned to the
#' calling function.
#' @return A list of MAgPIE objects containing the reports
#' @author Michael Crawford
#' @importFrom magclass write.magpie
#' @examples
#'
#'   \dontrun{
#'     x <- getReportFSECStevenLord(gdx, magpieOutputDir)
#'   }
#'

getReportFSECStevenLord <- function(gdx, reportOutputDir = NULL, magpieOutputDir, scenario = NULL) {

  # Functions -------------------------------------------------------------------------------------------------------

  .formatReport <- function(.x, .name = NULL) {
    getSets(.x)[c("d1.1", "d1.2")] <- c("iso", "cell")
    getSets(.x, fulldim = FALSE)[3] <- "variable"
    if (!is.null(.name)) {
      getNames(.x) <- .name
    }
    return(.x)
  }

  .saveNetCDFReport <- function(.x, .file, .comment = NULL) {
    if (!is.null(reportOutputDir) & !is.null(scenario)) {
      write.magpie(.x,
                   file_name = file.path(reportOutputDir, paste0(scenario, "-", .file, ".nc")),
                   comment = .comment)
    }
  }

  # Land use patterns -----------------------------------------------------------------------------------------------

  gridLand <- reportGridLand(gdx, dir = magpieOutputDir)

  # Add multicropping parameter into croplands
  # multicropping_parameter <- readGDX(gdx, "f18_multicropping")
  # multicropping_parameter <- gdxAggregate(gdx,
  #                                         x = multicropping_parameter,
  #                                         to = "grid",
  #                                         absolute = FALSE,
  #                                         dir = magpieOutputDir)
  # multicropping_parameter <- multicropping_parameter[, getItems(gridLand)$year, ]

  # Croplands (by area harvested)
  # areaHarvested <- gridLand[, , "Cropland"] * multicropping_parameter
  # areaHarvested[areaHarvested < 0.0001] <- 0 # Remove minuscule values of Cropland (< 10 ha per grid cell)

  cropland <- gridLand[, , "Cropland"]
  cropland[cropland < 0.0001] <- 0 # Remove minuscule values of cropland (< 10 ha per grid cell)

  # Pastures
  pasture <- gridLand[, , "Pastures and Rangelands"]
  pasture[pasture < 0.0001] <- 0 # Remove minuscule values of Pasture (< 10 ha per grid cell)

  # Non-agricultural land
  nonAgLand <- gridLand[, , c("Managed Forest", "Primary Forest", "Secondary Forest", "Urban Area", "Other Land")]
  nonAgLand <- dimSums(nonAgLand, dim = 3)
  nonAgLand[nonAgLand < 0.0001] <- 0 # Remove minuscule values of non-agricultural land (< 10 ha per grid cell)

  # Total land
  total <- dimSums(gridLand, dim = 3)

  # Combined object
  land <- mbind(cropland, pasture, nonAgLand, total)
  land <- .formatReport(land, c("Cropland", "PasturesAndRangelands", "NonAgriculturalLand", "TotalLand"))
  .saveNetCDFReport(land, .file = "Landuse", .comment = "unit: Mh")

  # Nitrogen Budgets ------------------------------------------------------------------------------------------------

  # Nitrogen budget per unit cropland
  nbCropland <- reportNitrogenBudgetCropland(gdx, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
  nbCropland <- nbCropland[, , c("Fertilizer", "Nutrient Surplus", "N2O-N", "NH3-N", "NO2-N", "NO3-N")]
  nbCropland <- .formatReport(nbCropland, c("Fertilizer", "NutrientSurplus", "N2O_N", "NH3_N", "NO2_N", "NO3_N"))
  .saveNetCDFReport(nbCropland, .file = "NitrogenBudget_Cropland", .comment = "unit: Mt")

  # Nitrogen budget per unit pasture
  nbPasture <- reportNitrogenBudgetPasture(gdx, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
  nbPasture <- nbPasture[, , c("Fertilizer", "Nutrient Surplus", "N2O-N", "NH3-N", "NO2-N", "NO3-N")]
  nbPasture <- .formatReport(nbPasture, c("Fertilizer", "NutrientSurplus", "N2O_N", "NH3_N", "NO2_N", "NO3_N"))
  .saveNetCDFReport(nbPasture, .file = "NitrogenBudget_Pasture", .comment = "unit: Mt")

  # Nitrogen budget per unit Non-agricultural land
  nbNonAgLand <- reportNitrogenBudgetNonagland(gdx, grid = TRUE, dir = magpieOutputDir)
  nbNonAgLand <- nbNonAgLand[, , "Nutrient Surplus"]
  nbNonAgLand <- .formatReport(nbNonAgLand, "NutrientSurplus")
  .saveNetCDFReport(nbNonAgLand, .file = "NitrogenBudget_NonAgLand", .comment = "unit: Mt")

  # Nitrogen budget per unit total land
  nbManureExcretion <- reportGridManureExcretion(gdx, dir = magpieOutputDir)
  nbManureExcretion <- nbManureExcretion[, , c("Manure|Manure In Confinements|+|Losses",
                                               "Manure|Manure In Confinements|Losses|N2O-N|Direct",
                                               "Manure|Manure In Confinements|Losses|NH3-N",
                                               "Manure|Manure In Confinements|Losses|NO2-N",
                                               "Manure|Manure In Confinements|Losses|NO3-N")]
  nbManureExcretion <- .formatReport(nbManureExcretion, c("ManureInConfinements_TotalLosses",
                                                          "ManureInConfinements_Losses_N2O_N_Direct",
                                                          "ManureInConfinements_Losses_NH3_N",
                                                          "ManureInConfinements_Losses_NO2_N",
                                                          "ManureInConfinements_Losses_NO3_N"))
  .saveNetCDFReport(nbManureExcretion, .file = "NitrogenBudget_ManureExcretion", .comment = "unit: Mt")

  # Return list -----------------------------------------------------------------------------------------------------

  return(list("Landuse"                        = land,
              "NitrogenBudget_Cropland"        = nbCropland,
              "NitrogenBudget_Pasture"         = nbPasture,
              "NitrogenBudget_NonAgLand"       = nbNonAgLand,
              "NitrogenBudget_ManureExcretion" = nbManureExcretion))

}
