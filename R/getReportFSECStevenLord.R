#' @title getReportFSECStevenLord
#' @description Collects reports for Steven Lord's cost of action / cost of inaction analysis.
#'
#' @export
#'
#' @param magpieOutputDir a magpie output directory which contains a mapping file (clustermap*.rds) for the
#' disaggregation of grid output
#' @param reportOutputDir a folder name for the output to be written to. If NULL the report is not saved to
#' disk, and only returned to the calling function.
#' @param scenario the name of the scenario used. If NULL the report is not saved to disk, and only returned to the
#' calling function.
#' @author Michael Crawford
#' @importFrom magclass write.magpie
#' @importFrom dplyr %>%
#' @examples
#'
#'   \dontrun{
#'     x <- getReportFSECStevenLord(magpieOutputDir)
#'   }
#'

getReportFSECStevenLord <- function(magpieOutputDir, reportOutputDir, scenario) {

  # --------------------------------------------------------------------------------
  # Helper functions

  .formatGridReport <- function(x, name) {
    getSets(x)[c("d1.1", "d1.2")] <- c("iso", "cell")
    getSets(x, fulldim = FALSE)[2] <- "year"
    getSets(x, fulldim = FALSE)[3] <- "variable"
    getNames(x) <- name

    return(x)
  }

  .aggregateToISO <- function(x) {
    x <- gdxAggregate(gdx_path, x, to = "iso", dir = magpieOutputDir)
    x <- as.data.frame(x)
    colnames(x) <- c("Cell", "ISO", "Year", "Variable", "Value")

    return(x)
  }

  .saveCSVReport <- function(x, file) {
    if (!is.null(reportOutputDir) && !is.null(scenario)) {
      write.csv(x,
                file = file.path(reportOutputDir, paste0(scenario, "-", file, ".csv")),
                row.names = FALSE)
    }
  }


  # --------------------------------------------------------------------------------
  # Derive GDX

  gdx_path <- file.path(magpieOutputDir, "fulldata.gdx")

  rootMagpieDir <- file.path(magpieOutputDir, "../../")
  if (stringr::str_detect(string = scenario, pattern = "HR")) {
      rootMagpieDir <- file.path(magpieOutputDir, "../../../")
  }

  # --------------------------------------------------------------------------------
  # Land-use patterns

  message("getReportFSECStevenLord, retrieving land use for scenario: ", scenario)

  tryCatch(
    {
      land <- land(gdx_path, dir = magpieOutputDir, level = "iso")

      cropland <- land[, , "crop"]
      getNames(cropland) <- "cropland"

      pasture <- land[, , "past"]
      getNames(pasture) <- "pasture"

      nonagland <- dimSums(land[, , c("forestry", "primforest", "secdforest", "urban", "other")], dim = 3)
      getNames(nonagland) <- "nonAgLand"

      total <- dimSums(land, dim = 3)
      getNames(total) <- "totalLand"

      landuse <- mbind(cropland, pasture, nonagland, total)

      landuse <- as.data.frame(landuse)
      colnames(landuse) <- c("Cell", "ISO", "Year", "Variable", "Value")
      .saveCSVReport(landuse, file = "Landuse")
    },
    error = function(e)
    {
      message("Land-use disaggregation failed to calculate for the scenario ", scenario)
      message("Full error: ", e)
    })


  # --------------------------------------------------------------------------------
  # Nitrogen Budgets

  message("getReportFSECStevenLord, calculating nutrient surplus for scenario: ", scenario)

  tryCatch(
    {
      # Cropland emissions
      nbCropland <- reportNitrogenBudgetCropland(gdx_path, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
      nbCropland <- nbCropland[, , c("N2O-N", "NH3-N", "NO2-N", "NO3-N")]
      nbCropland <- .formatGridReport(nbCropland, c("N2O_N", "NH3_N", "NO2_N", "NO3_N"))
      nbCropland <- .aggregateToISO(nbCropland)
      nbCropland <- nbCropland %>%
        dplyr::mutate(Type = "Cropland") %>%
        dplyr::rename("Emission" = "Variable") %>%
        dplyr::select(-"Cell")

      # Pasture emissions
      nbPasture <- reportNitrogenBudgetPasture(gdx_path, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
      nbPasture <- nbPasture[, , c("N2O-N", "NH3-N", "NO2-N", "NO3-N")]
      nbPasture <- .formatGridReport(nbPasture, c("N2O_N", "NH3_N", "NO2_N", "NO3_N"))
      nbPasture <- .aggregateToISO(nbPasture)
      nbPasture <- nbPasture %>%
        dplyr::mutate(Type = "Pasture") %>%
        dplyr::rename("Emission" = "Variable") %>%
        dplyr::select(-"Cell")

      # Manure emissions
      # Note to him that N2O-N is direct emissions...
      nbManureExcretion <- reportGridManureExcretion(gdx_path, dir = magpieOutputDir)
      nbManureExcretion <- nbManureExcretion[, , c("Manure|Manure In Confinements|Losses|N2O-N|Direct",
                                                   "Manure|Manure In Confinements|Losses|NH3-N",
                                                   "Manure|Manure In Confinements|Losses|NO2-N",
                                                   "Manure|Manure In Confinements|Losses|NO3-N")]
      nbManureExcretion <- .formatGridReport(nbManureExcretion, c("N2O_N", "NH3_N", "NO2_N", "NO3_N"))
      nbManureExcretion <- .aggregateToISO(nbManureExcretion)
      nbManureExcretion <- nbManureExcretion %>%
        dplyr::mutate(Type = "Manure") %>%
        dplyr::rename("Emission" = "Variable") %>%
        dplyr::select(-"Cell")

      nitrogenPollution <- dplyr::bind_rows(nbCropland, nbPasture, nbManureExcretion)
      .saveCSVReport(nitrogenPollution, file = "NitrogenPollution")
    },
    error = function(e)
    {
      message("Nutrient surplus failed to calculate for the scenario ", scenario)
      message("Full error: ", e)
    })


  # --------------------------------------------------------------------------------
  # Disaggregated CO2 from land-use change


  # --------------------------------------------------------------------------------
  # Disaggregated CH4 from livestock, AWMS, and ricer


  # --------------------------------------------------------------------------------
  # Biodiversity

  message("getReportFSECStevenLord, collecting BII for scenario: ", scenario)

  BII_path <- file.path(magpieOutputDir, paste0(scenario, "_cell.bii_0.5.nc"))

  if (file.exists(BII_path)) {
    file.copy(from = BII_path, to = reportOutputDir)
  } else {
    message("BII dataset (cell.bii_0.5.nc) wasn't found for the scenario: ", scenario)
  }

  # --------------------------------------------------------------------------------
  # Population - iso level

  message("getReportFSECStevenLord: Collecting ISO-level population datasets")

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

  message("getReportFSECStevenLord, collecting global surface temperature for scenario: ", scenario)

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
      message("Failed to save global surface temperature for the scenario: ", scenario)
      message("Full error: ", e)
    }
  )

  # --------------------------------------------------------------------------------
  # Health impacts

  message("getReportFSECStevenLord: Collecting country-level dietary impacts")

  reportISO_path <- file.path(magpieOutputDir, "report_iso.rds")
  healthReport <- readRDS(reportISO_path)

  healthVariables <- c("Health|Years of life lost|Risk|Diet and anthropometrics")

  healthReport <- healthReport %>% filter(.data$variable %in% healthVariables)

  if (nrow(healthReport) > 0) {
      colnames(healthReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
      .saveCSVReport(healthReport, "healthImpacts")
  } else {
      message("The health variables weren't found in the report_iso.rds for scenario: ", scenario)
  }

  # --------------------------------------------------------------------------------
  # Drivers - GDP PPP

  message("getReportFSECStevenLord, collecting GDP (PPP) driver for scenario: ", scenario)

  gdp_path <- file.path(rootMagpieDir, "modules/09_drivers/input/f09_gdp_ppp_iso.csv")
  if (file.exists(gdp_path)) {
    file.copy(from = gdp_path, to = file.path(reportOutputDir, ".."))
  } else {
    message("Error in magpie4::getReportFSECStevenLord.R: f09_gdp_ppp_iso file not found.")
  }


  # --------------------------------------------------------------------------------
  # Drivers - pop ISO

  message("getReportFSECStevenLord, collecting population ISO driver for scenario: ", scenario)

  gdp_path <- file.path(rootMagpieDir, "modules/09_drivers/input/f09_pop_iso.csv")
  if (file.exists(gdp_path)) {
    file.copy(from = gdp_path, to = file.path(reportOutputDir, ".."))
  } else {
    message("Error in magpie4::getReportFSECStevenLord.R: f09_pop_iso.csv file not found.")
  }


  # --------------------------------------------------------------------------------
  # Drivers - demography

  message("getReportFSECStevenLord, collecting demography for scenario: ", scenario)

  gdp_path <- file.path(rootMagpieDir, "modules/09_drivers/input/f09_demography.cs3")
  if (file.exists(gdp_path)) {
    file.copy(from = gdp_path, to = file.path(reportOutputDir, ".."))
  } else {
    message("Error in magpie4::getReportFSECStevenLord.R: f09_demography.cs3 file not found.")
  }

  # --------------------------------------------------------------------------------
  # Region mapping

  message("getReportFSECStevenLord, collecting region mapping for scenario: ", scenario)

  region_path <- file.path(magpieOutputDir, "regionmappingFSEC.csv")
  if (file.exists(region_path)) {
    file.copy(from = region_path, to = file.path(reportOutputDir, ".."))
  } else {
    message("Error in magpie4::getReportFSECStevenLord.R: regionmappingFSEC.csv file not found.")
  }


  # --------------------------------------------------------------------------------
  # GHG

  message("getReportFSECStevenLord, collecting GHG emissions for scenario", scenario)

  report_path <- file.path(magpieOutputDir, "report.rds")
  ghgReport <- readRDS(report_path)

  ghgVariables <- c("Emissions|CH4|Land",
                    "Emissions|N2O|Land",
                    "Emissions|NO2|Land",
                    "Emissions|NO3-|Land",
                    "Emissions|NH3|Land",
                    "Emissions|CO2|Land|+|Land-use Change")

  ghgReport <- ghgReport %>% filter(.data$variable %in% ghgVariables)

  if (nrow(ghgReport) > 0) {
    colnames(ghgReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
    .saveCSVReport(ghgReport, "GHG")
  } else {
    message("The GHG variables weren't found in the report.rds for scenario: ", scenario)
  }


  # --------------------------------------------------------------------------------
  # Tau

  message("getReportFSECStevenLord, collecting Tau for scenario", scenario)

  report_path <- file.path(magpieOutputDir, "report.rds")
  tauReport <- readRDS(report_path)

  tauVariable <- c("Productivity|Landuse Intensity Indicator Tau")

  tauReport <- tauReport %>% filter(.data$variable %in% tauVariable)

  if (nrow(tauReport) > 0) {
    colnames(tauReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
    .saveCSVReport(tauReport, "Tau")
  } else {
    message("Tau wasn't found in the report.rds for scenario: ", scenario)
  }


  # --------------------------------------------------------------------------------
  # Food system costs

  message("getReportFSECStevenLord, collecting food system costs for scenario: ", scenario)

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
  # Dietary indicators

  message("getReportFSECStevenLord, collecting dietary indicators for scenario: ", scenario)

  dietaryIndicators <- getReportDietaryIndicators(gdx_path, scenario)

  caloricIntake     <- dietaryIndicators$caloricSupply
  dietaryIndicators <- dietaryIndicators$dietaryIndicators

  if (nrow(caloricIntake) > 0) {
    .saveCSVReport(caloricIntake, "caloricIntake")
  } else {
    message("The caloric intake variables were unable to calculate for scenario: ", scenario)
  }

  if (nrow(dietaryIndicators) > 0) {
    .saveCSVReport(dietaryIndicators, "dietaryIndicators")
  } else {
    message("The dietary indicators variables were unable to calculate for scenario: ", scenario)
  }

}
