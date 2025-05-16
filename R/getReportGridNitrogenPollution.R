#' @title getReportGridNitrogenPollution
#' @description Reports nutrient surplus indicators as well as exceedance of the critical nitrogen surplus at the
#' grid level, including intensities for agricultural land and all land (including non-agricultural areas).
#' @author Michael Crawford
#'
#' @export
#'
#' @param magpieOutputDir a magpie output directory which contains a mapping file (clustermap*.rds) for the
#' disaggregation of grid output
#' @param reportOutputDir a folder name for the output to be written to. If NULL the report is not saved to
#' disk, and only returned to the calling function.
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
  # Nutrient surplus from different land-use types (Mt N)

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

  # Calculate total nutrient surplus from agricultural land only
  total_agri <- mbind(croplandSurplus, pastureSurplus, manureSurplus)
  total_agri <- dimSums(total_agri, dim = 3)
  total_agri <- .formatReport(total_agri, "Nutrient surplus from agricultural land and manure management")

  # Calculate total nutrient surplus including non-agricultural land
  total_allLand <- mbind(croplandSurplus, pastureSurplus, manureSurplus, nonAgLandSurplus)
  total_allLand <- dimSums(total_allLand, dim = 3)
  total_allLand <- .formatReport(total_allLand, "Nutrient surplus from all land and manure management")

  # Save raw surpluses to output folder
  surpluses <- mbind(croplandSurplus, pastureSurplus, manureSurplus, nonAgLandSurplus, total_agri, total_allLand)
  .saveReport(surpluses, file = "gridNitrogenSurplus", comment = "Mt N/yr")

  # -----------------------------------------------------------------------------------------------------------------
  # Calculate intensities (kg N/ha)

  # Total land area (all land) per grid cell
  gridLand <- reportGridLand(gdxPath, dir = magpieOutputDir)
  totalLand <- dimSums(gridLand, dim = 3)

  # Intensity for agricultural land only
  nutrientSurplus_intensity_agri <- (total_agri / totalLand) * 1e3  # Mt N/Mha to kg N/ha
  nutrientSurplus_intensity_agri <- toolConditionalReplace(
    x          = nutrientSurplus_intensity_agri,
    conditions = "!is.finite()",
    replaceby  = 0
  )
  nutrientSurplus_intensity_agri <- .formatReport(
    nutrientSurplus_intensity_agri,
    "Nutrient surplus intensity from agricultural land and manure management"
  )
  .saveReport(
    nutrientSurplus_intensity_agri,
    file    = "gridNutrientSurplus_agri_intensity",
    comment = "kg N / ha"
  )

  # Intensity including non-agricultural land
  nutrientSurplus_intensity_all <- (total_allLand / totalLand) * 1e3  # Mt N/Mha to kg N/ha
  nutrientSurplus_intensity_all <- toolConditionalReplace(
    x          = nutrientSurplus_intensity_all,
    conditions = "!is.finite()",
    replaceby  = 0
  )
  nutrientSurplus_intensity_all <- .formatReport(
    nutrientSurplus_intensity_all,
    "Nutrient surplus intensity from all land and manure management"
  )
  .saveReport(
    nutrientSurplus_intensity_all,
    file    = "gridNutrientSurplus_all_intensity",
    comment = "kg N / ha"
  )

  # -----------------------------------
  # Exceedance of critical nitrogen surplus (based on Schulte-Uebbing et al. 2022)

  nutrientSurplus_exceedance <- NULL
  criticalNitrogenSurplusPath <- file.path(magpieOutputDir, "criticalNitrogenSurplus_0.5.mz")

  if (file.exists(criticalNitrogenSurplusPath)) {
    criticalNitrogenSurplus <- read.magpie(criticalNitrogenSurplusPath)
    criticalNitrogenSurplus <- collapseDim(criticalNitrogenSurplus, dim = c(2, 3))

    # Exceedance based on agricultural intensity
    nutrientSurplus_exceedance <- -1 * (criticalNitrogenSurplus - nutrientSurplus_intensity_agri)
    nutrientSurplus_exceedance <- .formatReport(
      nutrientSurplus_exceedance,
      "Exceedance of critical nitrogen surplus from agriculture and manure management"
    )
    .saveReport(
      nutrientSurplus_exceedance,
      "exceedanceCriticalNitrogenSurplus_agri_intensity",
      comment = "kg N / ha"
    )
  }

  # -----------------------------------------------------------------------------------------------------------------
  # Return
  return(list(
    nutrientSurplus_cropland       = croplandSurplus,
    nutrientSurplus_pasture        = pastureSurplus,
    nutrientSurplus_manure         = manureSurplus,
    nutrientSurplus_nonAgLand      = nonAgLandSurplus,
    nutrientSurplus_total_agri     = total_agri,
    nutrientSurplus_total_all      = total_allLand,
    nutrientSurplus_intensity      = nutrientSurplus_intensity_agri,
    nutrientSurplus_intensity_all  = nutrientSurplus_intensity_all,
    nutrientSurplus_exceedance     = nutrientSurplus_exceedance
  ))
}
