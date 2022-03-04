#' @title getReportGridPollutants
#' @description Reports key environmental pollution indicators, including nitrogen budgets for croplands and pastures
#'
#' @export
#'
#' @param gdx GDX file
#' @param reportOutputDir a folder name that the output should be written to using write.report. If NULL the report is not saved to disk, and only returned as a magpie object.
#' @param magpieOutputDir magpie output directory which contains a mapping file (clustermap*.rds) for the disaggregation of grid output
#' @param scenario name of the scenario used for the list-structure of a reporting object (x$scenario$MAgPIE). If NULL the report is not saved to disk, and only returned as a magpie object.
#' @param filter Modelstat filter. Here you have to set the modelstat values for which results should be used. All values for time steps in which the modelstat is different or for which one of the previous modelstats were different are set to NA.
#' @return A list of MAgPIE objects containing the reports
#' @author Michael Crawford
#' @importFrom madrat toolGetMapping
#' @importFrom madrat toolConditionalReplace
#' @examples
#'
#'   \dontrun{
#'     x <- getReportGridPollutants(gdx)
#'   }
#'

getReportGridPollutants <- function(gdx, reportOutputDir, magpieOutputDir, scenario, filter = c(2, 7)) {

    .formatOutput <- function(x, category) {
        getSets(x)[c("d1.1", "d1.2")] <- c("iso", "cell")
        getSets(x, fulldim = FALSE)[3] <- "variable"
        getNames(x) <- paste0(category, getNames(x))

        x <- .filtermagpie(x, gdx, filter = filter)

        # TODO Eventually this mapping should be refined for the FSEC context
        mapping <- toolGetMapping(name = "mappingPIAMtoINMS.csv", type = "sectoral")
        y <- toolAggregate(x = x, rel = mapping,
                           from = "piam", to = "inms",
                           partrel = TRUE,
                           dim = 3.1)

        return(y)
    }

    .saveReport <- function(x, file, comment = NULL) {
        if (!is.null(reportOutputDir) & !is.null(scenario)) {
            write.magpie(x, 
                         file_name = paste0(reportOutputDir, "/gridPollutants-", scenario, "-", file, ".mz"), 
                         comment = comment)
        }
    }

    # -----------------------------------------------------------------------------------------------------------------
    # Single variables ------------------------------------------------------------------------------------------------

    gridLand           <- reportGridLand(gdx, dir = magpieOutputDir)
    gridLand_formatted <- .formatOutput(x = gridLand, category = "Land Cover|")
    .saveReport(gridLand_formatted, file = "LandCover", comment = "Mha X")

    nitrogenBudgetCropland           <- reportNitrogenBudgetCropland(gdx, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
    nitrogenBudgetCropland_formatted <- .formatOutput(x = nitrogenBudgetCropland, category = "Cropland Budget|")
    .saveReport(nitrogenBudgetCropland_formatted, file = "Nitrogen_CroplandBudget", comment = "Mt X")

    nitrogenBudgetPasture           <- reportNitrogenBudgetPasture(gdx, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
    nitrogenBudgetPasture_formatted <- .formatOutput(x = nitrogenBudgetPasture, category = "Pasture Budget|")
    .saveReport(nitrogenBudgetPasture_formatted, file = "Nitrogen_PastureBudget", comment = "Mt X")

    nitrogenBudgetNonAgLand           <- reportNitrogenBudgetNonagland(gdx, grid = TRUE, dir = magpieOutputDir)
    nitrogenBudgetNonAgLand_formatted <- .formatOutput(x = nitrogenBudgetNonAgLand, category = "Nonagland Budget|")
    .saveReport(nitrogenBudgetNonAgLand_formatted, file = "Nitrogen_NonAgriculturalLandBudget", comment = "Mt X")

    gridManureExcretion           <- reportGridManureExcretion(gdx, dir = magpieOutputDir)
    gridManureExcretion_formatted <- .formatOutput(x = gridManureExcretion, category = "")
    .saveReport(gridManureExcretion_formatted, file = "Nitrogen_Manure", comment = "Mt X")

    
    # -----------------------------------------------------------------------------------------------------------------
    # Combined output variables (rates) -------------------------------------------------------------------------------
    
    # TODO Perhaps it would be more appropriate to simply output the multicropping parameter itself, than calculating
    # the intensity datasets here.
    
    # Add multicropping in croplands
    multicropping_parameter <- readGDX(gdx, "f18_multicropping")
    multicropping_parameter <- gdxAggregate(gdx,
                                            x = multicropping_parameter,
                                            to = "grid",
                                            absolute = FALSE,
                                            dir = magpieOutputDir)
    multicropping_parameter <- multicropping_parameter[, getItems(gridLand_formatted)$year, ]
    cropland_withMulticropping <- gridLand_formatted[, , "Cropland"] * multicropping_parameter

    # Remove minuscule values of Cropland (< 10 ha per grid cell)
    cropland_withMulticropping[cropland_withMulticropping < 0.0001] <- 0
    .saveReport(cropland_withMulticropping, file = "cropland_withMulticropping_filtered")

    # Nitrogen budget per unit cropland -------------------------------------------------------------------------------
    nitrogenBudgetCropland_perCropland <- (nitrogenBudgetCropland_formatted / cropland_withMulticropping) * 1000 # Mt X / Mha to kg X / ha
    nitrogenBudgetCropland_perCropland <- toolConditionalReplace(x = nitrogenBudgetCropland_perCropland, conditions = "!is.finite()", replaceby = 0)
    .saveReport(x = nitrogenBudgetCropland_perCropland, file = "NitrogenBudgetCropland_perCropland", comment = "kg X/ha")

    # Nitrogen budget per unit pasture --------------------------------------------------------------------------------
    pasture <- gridLand_formatted[, , "Pasture"]
    pasture[pasture < 0.0001] <- 0
    nitrogenBudgetPasture_perPasture <- (nitrogenBudgetPasture_formatted / pasture[, , "Pasture"]) * 1000 # Mt X / Mha to kg X / ha
    nitrogenBudgetPasture_perPasture <- toolConditionalReplace(x = nitrogenBudgetPasture_perPasture, conditions = "!is.finite()", replaceby = 0)
    .saveReport(x = nitrogenBudgetPasture_perPasture, file = "nitrogenBudgetPasture_perPasture", comment = "kg X/ha")


    # -----------------------------------------------------------------------------------------------------------------

    return(list("gridLand"                           = gridLand_formatted, 
                "nitrogenBudgetCropland"             = nitrogenBudgetCropland_formatted, 
                "nitrogenBudgetPasture"              = nitrogenBudgetPasture_formatted, 
                "nitrogenBudgetNonAgLand"            = nitrogenBudgetNonAgLand_formatted, 
                "gridManureExcretion"                = gridManureExcretion_formatted,
                "cropland_withMulticropping"         = cropland_withMulticropping, 
                "nitrogenBudgetCropland_perCropland" = nitrogenBudgetCropland_perCropland, 
                "nitrogenBudgetPasture_perPasture"   = nitrogenBudgetPasture_perPasture))
    
}
