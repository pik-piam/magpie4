#' @title getReportGridNitrogenPollution
#' @description Reports nutrient surplus indicators as well as exceedance of the critical nitrogen surplus at the
#' grid level
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
            write.magpie(
                x,
                file_name = file.path(reportOutputDir, paste0(scenario, "-", file, ".nc")),
                comment = comment
            )
        }
    }
    
    # -----------------------------------------------------------------------------------------------------------------
    # Nutrient surplus from different land-use types
    
    gdxPath <- file.path(magpieOutputDir, "fulldata.gdx")
    
    # Cropland
    croplandBudget  <- reportNitrogenBudgetCropland(
        gdxPath,
        grid = TRUE,
        dir = magpieOutputDir,
        include_emissions = TRUE
    )
    croplandSurplus <- croplandBudget[, , "Nutrient Surplus"]
    croplandSurplus <- .formatReport(croplandSurplus, "Nutrient surplus from cropland")
    
    # Pasture
    pastureBudget  <- reportNitrogenBudgetPasture(
        gdxPath,
        grid = TRUE,
        dir = magpieOutputDir,
        include_emissions = TRUE
    )
    pastureSurplus <- pastureBudget[, , "Nutrient Surplus"]
    pastureSurplus <- .formatReport(pastureSurplus, "Nutrient surplus from pasture")
    
    # Manure in confinements
    manureBudget  <- reportGridManureExcretion(gdxPath, dir = magpieOutputDir)
    manureSurplus <- manureBudget[, , "Manure|Manure In Confinements|+|Losses"]
    manureSurplus <- .formatReport(
        manureSurplus,
        "Nutrient surplus from manure losses in confinements"
    )
    
    # Non-agricultural land
    nonAgLandBudget  <- reportNitrogenBudgetNonagland(
        gdxPath,
        grid = TRUE,
        dir = magpieOutputDir
    )
    nonAgLandSurplus <- nonAgLandBudget[, , "Nutrient Surplus"]
    nonAgLandSurplus <- .formatReport(
        nonAgLandSurplus,
        "Nutrient surplus from non-agricultural land"
    )
    
    # Agricultural land (cropland, pasture, AWMS)
    agriAWMS <- mbind(croplandSurplus, pastureSurplus, manureSurplus)
    agriAWMS <- dimSums(agriAWMS, dim = 3)
    agriAWMS <- .formatReport(
        agriAWMS,
        "Nutrient surplus from agricultural land and manure management"
    )
    
    # All land and manure management
    surplusTotal <- mbind(
        croplandSurplus,
        pastureSurplus,
        manureSurplus,
        nonAgLandSurplus
    )
    surplusTotal <- dimSums(surplusTotal, dim = 3)
    surplusTotal <- .formatReport(
        surplusTotal,
        "Nutrient surplus from all land and manure management"
    )
    
    # Save raw surpluses to output folder
    surpluses <- mbind(
        croplandSurplus,
        pastureSurplus,
        manureSurplus,
        nonAgLandSurplus,
        agriAWMS,
        surplusTotal
    )
    .saveReport(surpluses, file = "gridNitrogenSurplus", comment = "Mt N/yr")
    
    # -----------------------------------
    # Surplus intensity (kg N / ha)
    
    # Total land
    gridLand  <- reportGridLand(gdxPath, dir = magpieOutputDir)
    
    # Cropland surplus intensity
    nutrientSurplus_cropland_perHa <- (croplandSurplus / gridLand[, , "Cropland"]) * 1e3
    nutrientSurplus_cropland_perHa[gridLand[, , "Cropland"] < 1e-5] <- 0
    nutrientSurplus_cropland_perHa <- .formatReport(nutrientSurplus_cropland_perHa, "Nutrient surplus intensity from cropland")
    
    # Pasture surplus intensity
    nutrientSurplus_pasture_perHa <- (pastureSurplus / gridLand[, , "Pastures and Rangelands"]) * 1e3
    nutrientSurplus_pasture_perHa[gridLand[, , "Pastures and Rangelands"] < 1e-5] <- 0
    nutrientSurplus_pasture_perHa <- .formatReport(nutrientSurplus_pasture_perHa, "Nutrient surplus intensity from pasture")
    
    # Agricultural Area
    agriArea  <- gridLand[, , "Cropland"] + gridLand[, , "Pastures and Rangelands"]  # Mha
    nutrientSurplus_agriAWMS_perHa <- (agriAWMS / agriArea) * 1e3
    nutrientSurplus_agriAWMS_perHa[agriArea < 1e-5] <- 0
    nutrientSurplus_agriAWMS_perHa <- .formatReport(nutrientSurplus_agriAWMS_perHa, "Nutrient surplus intensity from agricultural land and manure management")
    
    # All land
    totalArea <- dimSums(gridLand, dim = 3)
    nutrientSurplus_perTotalAreaHa <- (surplusTotal / totalArea) * 1e3
    nutrientSurplus_perTotalAreaHa[totalArea < 1e-5] <- 0
    nutrientSurplus_perTotalAreaHa <- .formatReport(nutrientSurplus_perTotalAreaHa, "Nutrient surplus intensity from all land and manure management")
    
    surplusIntensities <- mbind(
        nutrientSurplus_cropland_perHa,
        nutrientSurplus_pasture_perHa,
        nutrientSurplus_agriAWMS_perHa,
        nutrientSurplus_perTotalAreaHa
    )
    
    # Save nutrient surplus intensity to output folder
    .saveReport(
        surplusIntensities,
        file = "gridNitrogenSurplus_intensity",
        comment = "kg N / ha"
    )
    
    # -----------------------------------
    # Exceedance of critical nitrogen surplus (based on Schulte-Uebbing et al. 2022)
    
    surplusExceedances <- NULL
    criticalNitrogenSurplusPath <- file.path(
        magpieOutputDir,
        "criticalNitrogenSurplus_0.5.mz"
    )
    
    if (file.exists(criticalNitrogenSurplusPath)) {
        criticalNitrogenSurplus <- read.magpie(criticalNitrogenSurplusPath)
        criticalNitrogenSurplus <- collapseDim(
            criticalNitrogenSurplus,
            dim = c(2, 3)
        )
        
        # Calculate exceedance of the critical nitrogen surplus
        # Schulte-Uebbing et al. 2022 already subtracts non-agricultural land surplus
        surplusExceedances <- -1 * (
            criticalNitrogenSurplus - nutrientSurplus_agriAWMS_perHa
        )
        surplusExceedances <- .formatReport(
            surplusExceedances,
            "Exceedance of critical nitrogen surplus"
        )
        
        # Save nutrient surplus exceedance to output folder
        .saveReport(
            surplusExceedances,
            "exceedanceGridCriticalNitrogenSurplus_intensity",
            comment = "kg N / ha"
        )
    }
    
    # -----------------------------------------------------------------------------------------------------------------
    # Return
    
    return(list(
        "nutrientSurplus_cropland"   = croplandSurplus,
        "nutrientSurplus_pasture"    = pastureSurplus,
        "nutrientSurplus_manure"     = manureSurplus,
        "nutrientSurplus_nonAgLand"  = nonAgLandSurplus,
        "nutrientSurplus_agriAWMS"   = agriAWMS,
        "nutrientSurplus_total"      = surplusTotal,
        "nutrientSurplus_intensity"  = surplusIntensities,
        "nutrientSurplus_exceedance" = surplusExceedances
    ))
}