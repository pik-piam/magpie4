#' @title getReportFSECPollution
#' @description Reports nutrient surplus indicators for the FSEC project
#' @author Michael Crawford
#'
#' @export
#' @param gdx GDX file
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
#'     x <- getReportFSECPollution(gdx)
#'   }
#'

getReportFSECPollution <- function(gdx, reportOutputDir = NULL, scenario = NULL) {

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
                         file_name = file.path(reportOutputDir, paste0(scenario, "-", file, ".mz")),
                         comment = comment)

            write.magpie(x,
                         file_name = file.path(reportOutputDir, paste0(scenario, "-", file, ".nc")),
                         comment = comment)
        }
    }

    # -----------------------------------------------------------------------------------------------------------------
    # Nutrient surplus from different land-use types

    message("getReportFSECPollution: Calculating nutrient surpluses")

    # Cropland
    croplandBudget  <- reportNitrogenBudgetCropland(gdx, level = "grid", include_emissions = TRUE)
    croplandSurplus <- croplandBudget[, , "Nutrient Surplus"]
    croplandSurplus <- .formatReport(croplandSurplus, "Nutrient surplus from cropland")
    .saveReport(croplandSurplus, file = "nutrientSurplus_cropland", comment = "unit: Mt N")

    # Pasture
    pastureBudget  <- reportNitrogenBudgetPasture(gdx, level = "grid", include_emissions = TRUE)
    pastureSurplus <- pastureBudget[, , "Nutrient Surplus"]
    pastureSurplus <- .formatReport(pastureSurplus, "Nutrient surplus from pasture")
    .saveReport(pastureSurplus, file = "nutrientSurplus_pasture", comment = "unit: Mt N")

    # Manure excretion
    manureBudget  <- reportGridManureExcretion(gdx)
    manureSurplus <- manureBudget[, , "Manure|Manure In Confinements|+|Losses"]
    manureSurplus <- .formatReport(manureSurplus, "Nutrient surplus from manure losses in confinements")
    .saveReport(manureSurplus, file = "nutrientSurplus_manure", comment = "unit: Mt N")

    # Non-agricultural land
    nonAgLandBudget <- reportNitrogenBudgetNonagland(gdx, level = "grid")
    nonAgLandSurplus <- nonAgLandBudget[, , "Nutrient Surplus"]
    nonAgLandSurplus <- .formatReport(nonAgLandSurplus, "Nutrient surplus from non-agricultural land")
    .saveReport(nonAgLandSurplus, file = "nutrientSurplus_nonAgLand", comment = "unit: Mt N")

    # Calculate total nutrient surplus
    total <- mbind(croplandSurplus, pastureSurplus, manureSurplus, nonAgLandSurplus)
    total <- dimSums(total, dim = 3)
    total <- .formatReport(total, "Nutrient surplus from land and manure management")
    .saveReport(total, file = "nutrientSurplus_total", comment = "unit: Mt N")

    # -----------------------------------
    # Total land
    gridLand  <- reportGridLand(gdx)
    totalLand <- dimSums(gridLand, dim = 3)

    # Calculate intensity of nutrient surplus
    nutrientSurplus_perTotalArea <- (total / totalLand) * 1000 # Mt X / Mha to kg X / ha

    # Five cells have 0 "totalLand", which leads to INFs
    nutrientSurplus_perTotalArea <- toolConditionalReplace(x = nutrientSurplus_perTotalArea,
                                                           conditions = "!is.finite()",
                                                           replaceby = 0)

    # Save formatted report
    nutrientSurplus_perTotalArea <- .formatReport(nutrientSurplus_perTotalArea, "Nutrient surplus intensity, incl natural vegetation")
    .saveReport(nutrientSurplus_perTotalArea, file = "nutrientSurplus_intensity", comment = "unit: kg N / ha")


    # -----------------------------------------------------------------------------------------------------------------
    # Return

    return(list("nutrientSurplus_cropland"  = croplandSurplus,
                "nutrientSurplus_pasture"   = pastureSurplus,
                "nutrientSurplus_manure"    = manureSurplus,
                "nutrientSurplus_nonAgLand" = nonAgLandSurplus,
                "nutrientSurplus_total"     = total,
                "nutrientSurplus_intensity" = nutrientSurplus_perTotalArea))

}
