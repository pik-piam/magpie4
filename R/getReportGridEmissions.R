#' @title getReportGridEmissions
#' @description Reports all grid-level emissions available for a magpie scenario
#' @author Michael Crawford
#'
#' @export
#'
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
#'     x <- getReportGridEmissions(gdx, magpieOutputDir)
#'   }
#'

getReportGridEmissions <- function(magpieOutputDir, reportOutputDir = NULL, scenario = NULL) {

    # -----------------------------------------------------------------------------------------------------------------
    # Helper functions

    .formatReport <- function(x, name) {
        getSets(x)[c("d1.1", "d1.2")] <- c("iso", "cell")
        getSets(x, fulldim = FALSE)[3] <- "variable"

        currentNames <- getNames(x)
        getNames(x) <- paste0(name, "|", currentNames)

        return(x)
    }

    .saveReport <- function(x, file, comment = NULL) {
        if (!is.null(reportOutputDir) && !is.null(scenario)) {
            write.magpie(x,
                         file_name = file.path(reportOutputDir, paste0(scenario, "-", file, ".mz")),
                         comment = comment)

            # write.magpie(x,
            #              file_name = file.path(reportOutputDir, paste0(scenario, "-", file, ".nc")),
            #              comment = comment)
        }
    }

    gdxPath <- file.path(magpieOutputDir, "fulldata.gdx")

    # -----------------------------------------------------------------------------------------------------------------
    # Nitrogen emissions

    nitrogenEmissionsNames <- c("N2O-N", "NH3-N", "NO2-N", "NO3-N")

    # Cropland
    cropland <- reportNitrogenBudgetCropland(gdxPath, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
    cropland <- cropland[, , nitrogenEmissionsNames]
    cropland <- .formatReport(cropland, name = "Cropland")

    # Pasture lands
    pasture <- reportNitrogenBudgetPasture(gdxPath, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
    pasture <- pasture[, , nitrogenEmissionsNames]
    pasture <- .formatReport(pasture, name = "Pasture")

    # Manure
    manure <- reportGridManureExcretion(gdxPath, dir = magpieOutputDir)

    manureNames <- data.frame(
        currentNames = c("Manure|Manure In Confinements|Losses|N2O-N|Direct",
                         "Manure|Manure In Confinements|Losses|NH3-N",
                         "Manure|Manure In Confinements|Losses|NO2-N",
                         "Manure|Manure In Confinements|Losses|NO3-N"),
        newNames = nitrogenEmissionsNames
    )

    manure <- manure[, , manureNames$currentNames]
    getNames(manure) <- manureNames$newNames

    manure <- .formatReport(manure, name = "Manure")

    # # Non-agricultural land
    # nonagland <- reportNitrogenBudgetNonagland(gdxPath, grid = TRUE, dir = magpieOutputDir)
    # nonagland <- nonagland[, , nitrogenEmissionsNames]
    # nonagland <- .formatReport(nonagland, name = "Non-agricultural land")

    # Combine
    nitrogen <- mbind(cropland, pasture, manure)
    .saveReport(nitrogen, file = "nitrogenEmissionsGridded", comment = "Mt N/yr")


    # -----------------------------------------------------------------------------------------------------------------
    # CO2 emissions

    # -----------------------------------------------------------------------------------------------------------------
    # CH4 emissions

    return(list(
        "Nitrogen" = nitrogen
    ))

}
