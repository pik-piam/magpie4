#' @title getReportFSECPollution
#' @description Reports nutrient surplus indicators for the FSEC project
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
#' @importFrom madrat toolConditionalReplace
#' @importFrom utils download.file
#' @examples
#'
#'   \dontrun{
#'     x <- getReportFSECPollution(gdx, magpieOutputDir)
#'   }
#'

getReportFSECPollution <- function(gdx, magpieOutputDir, reportOutputDir = NULL, scenario = NULL) {

    # -----------------------------------------------------------------------------------------------------------------
    # Helper function

    .formatReport <- function(x, name) {
        getSets(x)[c("d1.1", "d1.2")] <- c("iso", "cell")
        getSets(x, fulldim = FALSE)[3] <- "variable"
        getNames(x) <- name

        return(x)
    }

    .saveNetCDFReport <- function(x, file, comment = NULL) {
        if (!is.null(reportOutputDir) && !is.null(scenario)) {
            write.magpie(x,
                         file_name = file.path(reportOutputDir, paste0(scenario, "-", file, ".nc")),
                         comment = comment)

            write.magpie(x,
                         file_name = file.path(reportOutputDir, paste0(scenario, "-", file, ".mz")),
                         comment = comment)
        }
    }

    # -----------------------------------------------------------------------------------------------------------------
    # Nutrient surplus incl. natural vegetation (kg N / ha)

    message("getReportFSECPollution: Calculating nutrient surplus per total area")

    gdx_path <- file.path(magpieOutputDir, "fulldata.gdx")

    nbCropland        <- reportNitrogenBudgetCropland(gdx_path, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
    nbPasture         <- reportNitrogenBudgetPasture(gdx_path,  grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
    nbManureExcretion <- reportGridManureExcretion(gdx_path, dir = magpieOutputDir)
    nbNonAgLand       <- reportNitrogenBudgetNonagland(gdx_path, grid = TRUE, dir = magpieOutputDir)

    # Combined nutrientSurplus, incl. natural vegetation
    nutrientSurplus <- mbind(nbCropland[, , "Nutrient Surplus"],
                             nbPasture[, , "Nutrient Surplus"],
                             nbManureExcretion[, , "Manure|Manure In Confinements|+|Losses"],
                             nbNonAgLand[, , "Nutrient Surplus"])

    nutrientSurplus <- dimSums(nutrientSurplus, dim = 3)

    # Total land
    gridLand  <- reportGridLand(gdx_path, dir = magpieOutputDir)
    totalLand <- dimSums(gridLand, dim = 3)

    # Calculate intensity of nutrient surplus
    nutrientSurplus_perTotalArea <- (nutrientSurplus / totalLand) * 1000 # Mt X / Mha to kg X / ha

    # Five cells have 0 "totalLand", which leads to INFs
    nutrientSurplus_perTotalArea <- toolConditionalReplace(x = nutrientSurplus_perTotalArea,
                                                           conditions = "!is.finite()",
                                                           replaceby = 0)

    # Save formatted report
    nutrientSurplus_perTotalArea <- .formatReport(nutrientSurplus_perTotalArea, "Nutrient Surplus incl natural vegetation")
    .saveNetCDFReport(nutrientSurplus_perTotalArea,
                      file = "nutrientSurplus",
                      comment = "unit: kg N / ha")


    # -----------------------------------------------------------------------------------------------------------------
    # Nutrient surplus exposure

    message("getReportFSECPollution: Calculating population-weighted nutrient surplus exposure")

    # Read population datasets
    popFile <- file.path(magpieOutputDir, "../../input/FSEC_populationScenarios", "FSEC_populationScenarios_v2_22-08-22.mz")
    pop <- read.magpie(popFile)

    config <- gms::loadConfig(file.path(magpieOutputDir, "config.yml"))
    pop <- pop[, , config$gms$c09_pop_scenario]
    getNames(pop) <- "value"

    # Ensure alignment of years
    yearsPresent <- Reduce(f = intersect, x = Map(getYears, list(nutrientSurplus_perTotalArea, pop)))
    pop <- pop[, yearsPresent, ]

    # Round off projections' fractions of people and use persons rather than millions persons
    pop <- round(pop * 1E6)

    nutrientSurplus_populationExposure <- nutrientSurplus_perTotalArea * pop

    nutrientSurplus_populationExposure <- .formatReport(nutrientSurplus_populationExposure, "nutrientSurplus_populationExposure")
    .saveNetCDFReport(nutrientSurplus_populationExposure,
                      file = "nutrientSurplus_populationExposure",
                      comment = "unit: (kg N / ha) * person")

    # -----------------------------------------------------------------------------------------------------------------
    # Return list -----------------------------------------------------------------------------------------------------

    return(list("nutrientSurplus_perTotalArea"        = nutrientSurplus_perTotalArea,
                "nutrientSurplus_populationExposure"  = nutrientSurplus_populationExposure))

}
