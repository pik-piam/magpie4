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
#' @examples
#'
#'   \dontrun{
#'     x <- getReportGridNitrogenPollution(magpieOutputDir)
#'   }

getReportGridNitrogenPollution <- function(magpieOutputDir, reportOutputDir = NULL, scenario = NULL) {

  # Helper: format and name
  .formatReport <- function(x, name) {
    getSets(x)[c("d1.1", "d1.2", "d1.3")] <- c("x", "y", "iso")
    getSets(x, fulldim = FALSE)[3] <- "variable"
    getNames(x) <- name

    return(x)
  }

  # Helper: save netCDF
  .saveReport <- function(x, file, comment = NULL) {
    if (!is.null(reportOutputDir) && !is.null(scenario)) {
      write.magpie(x,
                   file_name = file.path(reportOutputDir, paste0(scenario, "-", file, ".nc")),
                   comment = comment)
    }
  }

  # --------------------------------------------------------------------------------------------------------------_
  gdxPath <- file.path(magpieOutputDir, "fulldata.gdx")

  # ---- Surpluses (Mt N / yr)

  # 1) Cropland surplus
  croplandBudget   <- reportNitrogenBudgetCropland(gdxPath, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE) # nolint
  croplandSurplus  <- croplandBudget[, , "Nutrient Surplus"]
  croplandSurplus  <- .formatReport(croplandSurplus, "Nitrogen surplus from cropland")

  # 2) Pasture surplus
  pastureBudget    <- reportNitrogenBudgetPasture(gdxPath, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
  pastureSurplus   <- pastureBudget[, , "Nutrient Surplus"]
  pastureSurplus   <- .formatReport(pastureSurplus, "Nitrogen surplus from pasture")

  # 3) Animal waste management (manure losses)
  manureBudget     <- reportGridManureExcretion(gdxPath, dir = magpieOutputDir)
  manureSurplus    <- manureBudget[, , "Manure|Manure In Confinements|+|Losses"]
  manureSurplus    <- .formatReport(manureSurplus, "Nitrogen surplus from animal waste management")

  # 4) Non-agricultural land surplus
  nonAgBudget      <- reportNitrogenBudgetNonagland(gdxPath, grid = TRUE, dir = magpieOutputDir)
  nonAgLandSurplus <- nonAgBudget[, , "Nutrient Surplus"]
  nonAgLandSurplus <- .formatReport(nonAgLandSurplus, "Nitrogen surplus from non-agricultural land")

  # ---- Aggregate surpluses (Mt N / yr)

  # a) Agricultural land only (cropland + pasture)
  surplus_agri <- mbind(croplandSurplus, pastureSurplus)
  surplus_agri <- dimSums(surplus_agri, dim = 3)
  surplus_agri <- .formatReport(surplus_agri, "Nitrogen surplus from agricultural land")

  # b) Agricultural land + manure management
  surplus_agriAWMS <- mbind(croplandSurplus, pastureSurplus, manureSurplus)
  surplus_agriAWMS <- dimSums(surplus_agriAWMS, dim = 3)
  surplus_agriAWMS <- .formatReport(surplus_agriAWMS, "Nitrogen surplus from agricultural land and manure management")

  # c) All land + manure management
  surplus_all <- mbind(croplandSurplus, pastureSurplus, manureSurplus, nonAgLandSurplus)
  surplus_all <- dimSums(surplus_all, dim = 3)
  surplus_all <- .formatReport(surplus_all, "Nitrogen surplus from all land and manure management")

  # bind raw and aggregates into one object
  surpluses <- mbind(
    croplandSurplus,
    pastureSurplus,
    manureSurplus,
    nonAgLandSurplus,
    surplus_agri,
    surplus_agriAWMS,
    surplus_all
  )
  .saveReport(surpluses, file = "gridNitrogenSurplus", comment = "Mt N/yr")

  # ---- intensities (kg N/ha)

  # grid total land area (Mha)
  gridLand  <- reportGridLand(gdxPath, dir = magpieOutputDir)
  totalLand <- dimSums(gridLand, dim = 3)

  # intensity: agricultural land
  intensity_agri <- (surplus_agri / totalLand) * 1e3
  intensity_agri <- madrat::toolConditionalReplace(x = intensity_agri, conditions = "!is.finite()", replaceby = 0)
  intensity_agri <- .formatReport(intensity_agri, "Nitrogen surplus intensity from agricultural land")

  # intensity: agricultural land + manure management
  intensity_agriAWMS <- (surplus_agriAWMS / totalLand) * 1e3
  intensity_agriAWMS <- madrat::toolConditionalReplace(x = intensity_agriAWMS, conditions = "!is.finite()", replaceby = 0) # nolint
  intensity_agriAWMS <- .formatReport(intensity_agriAWMS, "Nitrogen surplus intensity from agricultural land and manure management") # nolint

  # intensity: all land + manure management
  intensity_all <- (surplus_all / totalLand) * 1e3
  intensity_all <- madrat::toolConditionalReplace(x = intensity_all, conditions = "!is.finite()", replaceby = 0)
  intensity_all <- .formatReport(intensity_all, "Nitrogen surplus intensity from all land and manure management")

  intensity <- mbind(intensity_agri, intensity_agriAWMS, intensity_all)
  .saveReport(intensity, file = "gridNitrogenSurplus_intensity", comment = "kg N / ha")

  # ---- Exceedance of Schulte Uebbing et al. (2020) boundary on agricultural land + AWMS
  exceedance <- NULL
  criticalPath <- file.path(magpieOutputDir, "criticalNitrogenSurplus_0.5.mz")
  if (file.exists(criticalPath)) {
    critical <- read.magpie(criticalPath)
    critical <- collapseDim(critical, dim = c(2, 3))

    exceedance <- -1 * (critical - intensity_agriAWMS)
    exceedance <- .formatReport(exceedance, "Exceedance of critical nitrogen surplus from agriculture and manure management") # nolint
    .saveReport(exceedance, "gridNitrogenSurplus_exceedanceAgriAWMS_intensity", comment = "kg N / ha")
  }

  # ---- return list
  return(list(
    nutrientSurplus_cropland           = croplandSurplus,
    nutrientSurplus_pasture            = pastureSurplus,
    nutrientSurplus_manure             = manureSurplus,
    nutrientSurplus_nonAgLand          = nonAgLandSurplus,

    nutrientSurplus_agri               = surplus_agri,
    nutrientSurplus_agriAWMS           = surplus_agriAWMS,
    nutrientSurplus_all                = surplus_all,

    nutrientSurplus_intensity_agri     = intensity_agri,
    nutrientSurplus_intensity_agriAWMS = intensity_agriAWMS,
    nutrientSurplus_intensity_all      = intensity_all,

    nutrientSurplus_exceedance         = exceedance
  ))
}
