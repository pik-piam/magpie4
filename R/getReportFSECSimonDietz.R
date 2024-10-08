#' @title getReportFSECSimonDietz
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
#' @importFrom dplyr %>% filter rename select pull
#' @importFrom rlang .data
#' @importFrom madrat toolConditionalReplace
#' @importFrom stringr str_detect
#' @examples
#'
#'   \dontrun{
#'     x <- getReportFSECSimonDietz(magpieOutputDir)
#'   }
#'

getReportFSECSimonDietz <- function(magpieOutputDir, reportOutputDir = NULL, scenario = NULL) {

    # --------------------------------------------------------------------------------
    # Helper functions

    .formatGridReport <- function(x, name) {
        getSets(x)[c("d1.1", "d1.2", "d1.3")] <- c("x", "y", "iso")
        getSets(x, fulldim = FALSE)[3] <- "variable"
        getNames(x) <- name
        
        return(x)
    }

    .saveGridReport <- function(x, file, comment = NULL) {
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
    # Setup paths

    gdx_path <- file.path(magpieOutputDir, "fulldata.gdx")

    rootMagpieDir <- file.path(magpieOutputDir, "../../")
    if (stringr::str_detect(string = scenario, pattern = "HR")) {
        rootMagpieDir <- file.path(magpieOutputDir, "../../../")
    }


    # --------------------------------------------------------------------------------
    # Nutrient surplus

    message("getReportFSECSimonDietz: Calculating nutrient surplus")

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
            nutrientSurplus_perTotalArea <- .formatGridReport(nutrientSurplus_perTotalArea, "Nutrient Surplus incl natural vegetation")
            .saveGridReport(nutrientSurplus_perTotalArea,
                              file = "nutrientSurplus",
                              comment = "unit: kg N / ha")
        },
        error = function(e)
        {
            message("Nutrient surplus failed to calculate for the scenario ", scenario)
            message("Full error: ", e)
        })


    # --------------------------------------------------------------------------------
    # Biodiversity

    message("getReportFSECSimonDietz: Collecting BII")

    BII_path <- file.path(magpieOutputDir, "cell.bii_0.5.nc")

    if (file.exists(BII_path)) {
        file.copy(from = BII_path, to = reportOutputDir)
        file.rename(from = file.path(reportOutputDir, "cell.bii_0.5.nc"), 
                    to = file.path(reportOutputDir, paste0(scenario, "-cell.bii_0.5.nc")))
    } else {
        message("BII dataset (cell.bii_0.5.nc) wasn't found for the scenario: ", scenario)
    }


    # --------------------------------------------------------------------------------
    # Poverty

    message("getReportFSECSimonDietz: Collecting poverty datasets")

    reportISO_path <- file.path(magpieOutputDir, "report_iso.rds")
    povertyReport  <- readRDS(reportISO_path)

    povertyVariables <- c("Income|Income after Climate Policy",
                          "Income|Gini Coefficient",
                          "Income|Fraction of Population below half of Median Income",
                          "Income|Average Income of Lower 40% of Population",
                          "Income|Number of People Below 1p90 USDppp11/day",
                          "Income|Number of People Below 3p20 USDppp11/day",
                          "Income|Number of People Below 5p50 USDppp11/day",
                          "Total income after Climate Policy")

    povertyReport <- povertyReport %>% filter(.data$variable %in% povertyVariables)

    colnames(povertyReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
    
    if (nrow(povertyReport) > 0) {
        .saveCSVReport(povertyReport, file = "poverty")
    } else {
        message("The poverty variables weren't found in the report_iso.rds for scenario: ", scenario)
    }


    # --------------------------------------------------------------------------------
    # GDP for poverty-model countries

    GDP <- readGDX(gdx = gdx_path, "i09_gdp_ppp_iso")[, readGDX(gdx_path, "t"), ]

    presentCountries <- povertyReport %>% 
        filter(!is.na(.data$Value)) %>% 
        pull(.data$ISO) %>% 
        droplevels() %>% 
        unique()

    # aggregate GDP over countries from poverty model
    povertyGDP <- as.data.frame(GDP[presentCountries, c(2020, 2050), ]) %>%
        mutate(Unit = "constant 2017 Int$PPP") %>%
        select(.data$Region, .data$Year, .data$Unit, .data$Value)

    colnames(povertyGDP) <- c("ISO", "Year", "Unit", "Value")

    if (nrow(povertyGDP) > 0) {
        .saveCSVReport(povertyGDP, file = "povertyGDP")
    } else {
        message("The GDP for the poverty variables wasn't calculated for scenario: ", scenario)
    }


    # --------------------------------------------------------------------------------
    # Population - grid level

    message("getReportFSECSimonDietz: Collecting grid-level population datasets")

    pop_path <- file.path(rootMagpieDir, "input/FSEC_populationScenarios", "FSEC_populationScenarios_v2_22-08-22.mz")

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

        pop <- .formatGridReport(pop, "Population")
        .saveGridReport(pop, file = "population_grid", comment = "unit: Persons")
    } else {
        message("The population dataset wasn't found for the scenario: ", scenario)
    }


    # --------------------------------------------------------------------------------
    # Population - iso level

    message("getReportFSECSimonDietz: Collecting ISO-level population datasets")

    gdxPath <- file.path(magpieOutputDir, "fulldata.gdx")

    tryCatch(
        {
            pop <- reportPopulation(gdx = gdxPath, level = "iso") %>%
                as.data.frame(pop) %>%
                rename(Unit = .data$Data1) %>%
                select(.data$Region, .data$Year, .data$Unit, .data$Value)
            .saveCSVReport(pop, file = "population_iso")

        },
        error = function(e) {
            message("Failed to save iso-level population data for the scenario: ", scenario)
            message("Full error: ", e)
        }
    )


    # --------------------------------------------------------------------------------
    # Global Surface Temperature

    message("getReportFSECSimonDietz: Collecting global surface temperature")

    report_path <- file.path(magpieOutputDir, "report.mif")
    report <- read.report(report_path, as.list = FALSE)

    tryCatch(
        {
            globalSurfaceTemperature <- report["GLO", , "Global Surface Temperature (C)"]
            globalSurfaceTemperature <- as.data.frame(globalSurfaceTemperature)
            colnames(globalSurfaceTemperature) <- c("Cell", "Region", "Year", "Scenario", "Model", "Variable", "Value")
            .saveCSVReport(globalSurfaceTemperature, file = "globalSurfaceTemperature")
        },
        error = function(e) {
            message("Failed to save global surface temperature for the scenario: ", scenario)
            message("Full error: ", e)
        }
    )


    # --------------------------------------------------------------------------------
    # Health impacts

    message("getReportFSECSimonDietz: Collecting country-level dietary impacts")

    reportISO_path <- file.path(magpieOutputDir, "report_iso.rds")
    healthReport <- readRDS(reportISO_path)

    healthVariables <- c("Health|Years of life lost|Disease")

    healthReport <- healthReport %>% filter(.data$variable %in% healthVariables)

    if (nrow(healthReport) > 0) {
        colnames(healthReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
        .saveCSVReport(healthReport, "healthImpacts")
    } else {
        message("The health variables weren't found in the report_iso.rds for scenario: ", scenario)
    }


    # --------------------------------------------------------------------------------
    # Food system costs

    message("getReportFSECSimonDietz: Collecting food system costs")

    report_path <- file.path(magpieOutputDir, "report.rds")
    costReport <- readRDS(report_path)

    costVariables <- c("Costs Without Incentives")

    costReport <- costReport %>% filter(.data$variable %in% costVariables)

    if (nrow(costReport) > 0) {
        colnames(costReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
        .saveCSVReport(costReport, "costs")
    } else {
        message("The cost variables weren't found in the report.rds for scenario: ", scenario)
    }


    # --------------------------------------------------------------------------------
    # Value - bioeconomy demand

    message("getReportFSECSimonDietz: Collecting value of bioeconomy demand")

    report_path <- file.path(magpieOutputDir, "report.rds")
    bioeconomyReport <- readRDS(report_path)

    bioeconomyVariables <- c("Value|Bioeconomy Demand")

    bioeconomyReport <- bioeconomyReport %>% filter(.data$variable %in% bioeconomyVariables)

    if (nrow(bioeconomyReport) > 0) {
        colnames(bioeconomyReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
        .saveCSVReport(bioeconomyReport, "bioeconomyValue")
    } else {
        message("The bioeconomy variables weren't found in the report.rds for scenario: ", scenario)
    }


    # --------------------------------------------------------------------------------
    # Return - right now this return list isn't actively maintained (it isn't used)

    return(list(nutrientSurplus          = nutrientSurplus_perTotalArea,
                BII                      = BII,
                poverty                  = povertyReport,
                population               = pop,
                globalSurfaceTemperature = globalSurfaceTemperature,
                healthImpacts            = healthReport,
                costs                    = costReport,
                bioeconomy               = bioeconomyReport))
}
