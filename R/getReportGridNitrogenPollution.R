#' @title getReportGridNitrogenPollution
#' @description Reports nutrient surplus indicators as well as exceedance of the critical nitrogen surplus at the
#' grid level
#' @author Michael Crawford
#'
#' @export
#'
#' @param
#' @param reportOutputDir a folder name for the output to be written to. If NULL the report is not saved to
#' disk, and only returned to the calling function.
#' @param magpieOutputDir a magpie output directory which contains a mapping file (clustermap*.rds) for the
#' disaggregation of grid output
#' @param scenario the name of the scenario used. If NULL the report is not saved to disk, and only returned to the
#' calling function.
#'
#' @return A list of MAgPIE objects containing the reports
#'
#' @importFrom madrat toolConditionalReplace
#'
#' @examples
#'
#'   \dontrun{
#'     x <- getReportGridNitrogenPollution(magpieOutputDir)
#'   }
#'

getReportGridNitrogenPollution <- function(magpieOutputDir, reportOutputDir = NULL, scenario = NULL) {

  # -----------------------------------------------------------------------------------------------------------------
  # Helper functions

  .formatReport <- function(x, name) {
    getSets(x)[c("d1.1", "d1.2", "d1.3")] <- c("x", "y", "iso")
    getSets(x, fulldim = FALSE)[3] <- "variable"
    getNames(x) <- name

    return(x)
  }

  .saveReport <- function(x, file, comment = NULL) {
    if (!is.null(reportOutputDir) && !is.null(scenario)) {
      write.magpie(x,
                   file_name = file.path(reportOutputDir, paste0(scenario, "-", file, ".nc")),
                   comment = comment)
    }
  }

  # -----------------------------------------------------------------------------------------------------------------
  # Nutrient surplus from different land-use types

  gdxPath <- file.path(magpieOutputDir, "fulldata.gdx")

  # Cropland
  croplandBudget  <- reportNitrogenBudgetCropland(gdxPath, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
  croplandSurplus <- croplandBudget[, , "Nutrient Surplus"]
  croplandSurplus <- .formatReport(croplandSurplus, "Nutrient surplus from cropland")

  # Pasture
  pastureBudget  <- reportNitrogenBudgetPasture(gdxPath, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
  pastureSurplus <- pastureBudget[, , "Nutrient Surplus"]
  pastureSurplus <- .formatReport(pastureSurplus, "Nutrient surplus from pasture")

  # Manure excretion
  manureBudget  <- reportGridManureExcretion(gdxPath, dir = magpieOutputDir)
  manureSurplus <- manureBudget[, , "Manure|Manure In Confinements|+|Losses"]
  manureSurplus <- .formatReport(manureSurplus, "Nutrient surplus from manure losses in confinements")

  # Non-agricultural land
  nonAgLandBudget  <- reportNitrogenBudgetNonagland(gdxPath, grid = TRUE, dir = magpieOutputDir)
  nonAgLandSurplus <- nonAgLandBudget[, , "Nutrient Surplus"]
  nonAgLandSurplus <- .formatReport(nonAgLandSurplus, "Nutrient surplus from non-agricultural land")

  # Calculate total nutrient surplus
  total <- mbind(croplandSurplus, pastureSurplus, manureSurplus, nonAgLandSurplus)
  total <- dimSums(total, dim = 3)
  total <- .formatReport(total, "Nutrient surplus from land and manure management")

  # Save raw surpluses to output folder
  surpluses <- mbind(croplandSurplus, pastureSurplus, manureSurplus, nonAgLandSurplus, total)
  .saveReport(surpluses, file = "gridNitrogenSurplus", comment = "Mt N/yr")

  # Total land
  gridLand  <- reportGridLand(gdxPath, dir = magpieOutputDir)
  totalLand <- dimSums(gridLand, dim = 3)

  # Calculate intensity of nutrient surplus
  nutrientSurplus_perTotalArea <- (total / totalLand) * 1e3 # Mt X / Mha to kg X / ha

  # Five cells have 0 "totalLand", which leads to INFs
  nutrientSurplus_perTotalArea <- toolConditionalReplace(x = nutrientSurplus_perTotalArea,
                                                         conditions = "!is.finite()",
                                                         replaceby = 0)

  # Save nutrient surplus intensity to output folder
  nutrientSurplus_perTotalArea <- .formatReport(nutrientSurplus_perTotalArea, "Nutrient surplus intensity, incl natural vegetation") # nolint
  .saveReport(nutrientSurplus_perTotalArea, file = "gridNutrientSurplus_intensity", comment = "kg N / ha")

  # -----------------------------------
  # Exceedance of critical nitrogen surplus (based on Schulte-Uebbing et al. 2022)

  nutrientSurplus_exceedance <- NULL
  criticalNitrogenSurplusPath <- file.path(magpieOutputDir, "criticalNitrogenSurplus_0.5.mz")

  if (file.exists(criticalNitrogenSurplusPath)) {
    criticalNitrogenSurplus <- read.magpie(criticalNitrogenSurplusPath)
    criticalNitrogenSurplus <- collapseDim(criticalNitrogenSurplus, dim = c(2, 3))

    # Calculate exceedance of the critical nitrogen surplus
    nutrientSurplus_exceedance <- -1 * (criticalNitrogenSurplus - nutrientSurplus_perTotalArea)
    nutrientSurplus_exceedance <- .formatReport(nutrientSurplus_exceedance, "Exceedance of Critical Nitrogen Surplus")

    # Save nutrient surplus exceedance to output folder
    .saveReport(nutrientSurplus_exceedance, "exceedanceCriticalNitrogenSurplus_intensity", comment = "kg N / ha")
  }

  # -----------------------------------------------------------------------------------------------------------------
  # Return

  return(list("nutrientSurplus_cropland"   = croplandSurplus,
              "nutrientSurplus_pasture"    = pastureSurplus,
              "nutrientSurplus_manure"     = manureSurplus,
              "nutrientSurplus_nonAgLand"  = nonAgLandSurplus,
              "nutrientSurplus_total"      = total,
              "nutrientSurplus_intensity"  = nutrientSurplus_perTotalArea,
              "nutrientSurplus_exceedance" = nutrientSurplus_exceedance))

}
