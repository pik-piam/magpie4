#' @title getReportSimonDietz
#' @description Collects reports for Simon Dietz' social welfare function analysis
#'
#' @export
#'
#' @param magpieOutputDir a magpie output directory which contains all the files associate with the given scenario
#' @param reportOutputDir a folder name for the output to be written to. If NULL the report is not saved to
#' disk, and only returned to the calling function.
#' @param scenario the name of the scenario used. If NULL the report is not saved to disk, and only returned to the
#' calling function.
#'
#' @return A list of reports
#' @author Michael Crawford
#' @importFrom dplyr %>% filter
#' @importFrom rlang .data
#' @importFrom madrat toolConditionalReplace
#' @examples
#'
#'   \dontrun{
#'     x <- getReportSimonDietz(magpieOutputDir)
#'   }
#'

getReportSimonDietz <- function(magpieOutputDir, reportOutputDir = NULL, scenario = NULL) {

    # --------------------------------------------------------------------------------
    # Helper functions

    .formatReport <- function(x, name) {
        getSets(x)[c("d1.1", "d1.2")] <- c("iso", "cell")
        getSets(x, fulldim = FALSE)[2] <- "year"
        getSets(x, fulldim = FALSE)[3] <- "variable"
        getNames(x) <- name

        return(x)
    }

    .saveNetCDFReport <- function(x, file, comment = NULL) {
        if (!is.null(reportOutputDir) && !is.null(scenario)) {
            write.magpie(x,
                         file_name = file.path(reportOutputDir, paste0(scenario, "-", file, ".nc")),
                         comment = comment)
        }
    }

    .saveCSVReport <- function(x, file) {
        if (!is.null(reportOutputDir) && !is.null(scenario)) {
            write.csv(x,
                      file = file.path(reportOutputDir, paste0(scenario, "-", file, ".csv")),
                      row.names = FALSE)
        }
    }

    # --------------------------------------------------------------------------------
    # Nutrient surplus

    message("getReportSimonDietz: Calculating nutrient surplus")

    gdx_path <- file.path(magpieOutputDir, "fulldata.gdx")

    tryCatch(
        {
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
        },
        error = function(e)
        {
            message("Error in magpie4::getReportSimonDietz.R: Nutrient surplus failed to calculate. This is often
                    because `cell.land_0.5mz` wasn't created because `magpie4` was corrupted.")
            message("Full error: ", e)
        })


    # --------------------------------------------------------------------------------
    # Biodiversity

    message("getReportSimonDietz: Collecting BII")

    BII_path <- file.path(magpieOutputDir, paste0(scenario, "_cell.bii_0.5.nc"))

    if (file.exists(BII_path)) {
        file.copy(from = BII_path, to = reportOutputDir)
    } else {
        message("Error in magpie4::getReportSimonDietz.R: cell.bii_0.5.nc file not found.")
    }


    # --------------------------------------------------------------------------------
    # Poverty

    message("getReportSimonDietz: Collecting poverty datasets")

    # reportISO_path <- file.path(magpieOutputDir, "report_iso.rds")
    reportISO_path <- file.path(magpieOutputDir, "report_isoPoverty.rds")
    povertyReport  <- readRDS(reportISO_path)

    povertyVariables <- c("Income after Climate Policy (US$05 PPP/cap/yr)",
                          "Gini Coefficient (0-1)",
                          "Fraction of Population below half of Median Income",
                          "Average Income of Lower 40% of Population (US$05 PPP/cap/yr)",
                          "Number of People Below 1.90$/Day (mio people)",
                          "Number of People Below 3.20$/Day (mio people)",
                          "Number of People Below 5.50$/Day (mio people)")

    tryCatch(
        {
            povertyReport <- povertyReport %>% filter(.data$variable %in% povertyVariables)
            colnames(povertyReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
            .saveCSVReport(povertyReport, "poverty")
        },
        error = function(e)
        {
            message("Error in magpie4::getReportSimonDietz.R: The poverty variables weren't found in
                    the report_iso.RDS. Has the povery reporting script been run?")
        }
    )


    # --------------------------------------------------------------------------------
    # Population

    message("getReportSimonDietz: Collecting population datasets")

    pop_path <- file.path(magpieOutputDir, "../../input/FSEC_populationScenarios", "FSEC_populationScenarios_v2_22-08-22.mz")

    if (file.exists(pop_path)) {
        pop <- read.magpie(pop_path)

        config <- gms::loadConfig(file.path(magpieOutputDir, "config.yml"))
        pop <- pop[, , config$gms$c09_pop_scenario]
        getNames(pop) <- "value"

        # Ensure alignment of years
        yearsPresent <- Reduce(f = intersect, x = Map(getYears, list(nutrientSurplus_perTotalArea, pop)))
        pop <- pop[, yearsPresent, ]

        # Round off projections' fractions of people and use persons rather than millions persons
        pop <- round(pop * 1E6)

        pop <- .formatReport(pop, "Population")
        .saveNetCDFReport(pop, file = "population", comment = "unit: Persons")
    } else {
        message("Error in magpie4::getReportSimonDietz.R: Population dataset weren't found.")
    }


    # --------------------------------------------------------------------------------
    # Global Surface Temperature

    message("getReportSimonDietz: Collecting global surface temperature")

    report_path <- file.path(magpieOutputDir, "report.mif")
    report <- read.report(report_path, as.list = FALSE)

    tryCatch(
        {
            globalSurfaceTemperature <- report["GLO", , "Global Surface Temperature (C)"]
            globalSurfaceTemperature <- as.data.frame(globalSurfaceTemperature)
            colnames(globalSurfaceTemperature) <- c("Cell", "Region", "Year", "Scenario", "Model", "Variable", "Value")
            .saveCSVReport(globalSurfaceTemperature, file = "globalSurfaceTemperature")
        },
        error = function(e)
        {
            message("Error in magpie4::getReportSimonDietz.R: Global Surface Temperature
                    wasn't found in the report.mif. Perhaps `blackmagicc` hasn't been run yet?")
        }
    )


    # --------------------------------------------------------------------------------
    # Health impacts

    message("getReportSimonDietz: Collecting country-level dietary impacts")

    reportISO_path <- file.path(magpieOutputDir, "report_iso.rds")
    healthReport <- readRDS(reportISO_path)

    healthVariables <- c("Health|Deaths avoided|Risk|Diet and anthropometrics",
                         "Health|Deaths avoided|Risk|Diet and anthropometrics|+|Female",
                         "Health|Deaths avoided|Risk|Diet and anthropometrics|+|Male",
                         "Health|Year lives lost avoided|Risk|Diet and anthropometrics",
                         "Health|Year lives lost avoided|Risk|Diet and anthropometrics|+|Female",
                         "Health|Year lives lost avoided|Risk|Diet and anthropometrics|+|Male")

    tryCatch(
        {
            healthReport <- healthReport %>% filter(.data$variable %in% healthVariables)
            colnames(healthReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")

            .saveCSVReport(healthReport, "healthImpacts")
        },
        error = function(e)
        {
            message("Error in magpie4::getReportSimonDietz.R: Health impacts weren't found in
                    the report_iso.RDS. Have the health impacts reporting scripts been run?")
        }
    )


    # --------------------------------------------------------------------------------
    # Return

    return(list(nutrientSurplus          = nutrientSurplus_perTotalArea,
                BII                      = BII,
                poverty                  = povertyReport,
                population               = pop,
                globalSurfaceTemperature = globalSurfaceTemperature,
                healthImpacts            = healthReport))
}
