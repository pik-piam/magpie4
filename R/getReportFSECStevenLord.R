#' @title getReportFSECStevenLord
#' @description Collects reports for Steven Lord's cost of action / cost of inaction analysis.
#' Specifically:
#' - Total land (Mh)
#' - Nutrient surplus (Mt N)
#' - Biodiversity (BII)
#' - Population (Persons)
#' - Global Surface Temperature (deg C)
#' - GDP (PPP) driver
#' - Pop ISO driver
#' - Demography driver
#' - GHG emissions (CO2, CH4, NO2, as well as totaled and cumulative in CO2eq)
#' - Food system costs
#' - Tau
#' - Caloric intake
#' - Dietary indicators
#'
#' @export
#'
#' @param magpieOutputDir a magpie output directory which contains a mapping file (clustermap*.rds) for the
#' disaggregation of grid output
#' @param reportOutputDir a folder name for the output to be written to. If NULL the report is not saved to
#' disk, and only returned to the calling function.
#' @param scenario the name of the scenario used. If NULL the report is not saved to disk, and only returned to the
#' calling function.
#' @return A list of MAgPIE objects containing the reports
#' @author Michael Crawford
#' @importFrom magclass write.magpie
#' @examples
#'
#'   \dontrun{
#'     x <- getReportFSECStevenLord(magpieOutputDir)
#'   }
#'

getReportFSECStevenLord <- function(magpieOutputDir, reportOutputDir = NULL, scenario = NULL) {

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
  # Derive GDX

  gdx_path <- file.path(magpieOutputDir, "fulldata.gdx")


  # --------------------------------------------------------------------------------
  # Land-use patterns

  message("In getReportFSECStevenLord, retrieving land use for scenario: ", scenario)

  tryCatch(
    {
      gridLand <- reportGridLand(gdx_path, dir = magpieOutputDir)

      cropland <- gridLand[, , "Cropland"]
      cropland[cropland < 0.0001] <- 0 # Remove minuscule values of cropland (< 10 ha per grid cell)

      # Pastures
      pasture <- gridLand[, , "Pastures and Rangelands"]
      pasture[pasture < 0.0001] <- 0 # Remove minuscule values of Pasture (< 10 ha per grid cell)

      # Non-agricultural land
      nonAgLand <- gridLand[, , c("Managed Forest", "Primary Forest", "Secondary Forest", "Urban Area", "Other Land")]
      nonAgLand <- dimSums(nonAgLand, dim = 3)
      nonAgLand[nonAgLand < 0.0001] <- 0 # Remove minuscule values of non-agricultural land (< 10 ha per grid cell)

      # Total land
      total <- dimSums(gridLand, dim = 3)

      # Combined object
      land <- mbind(cropland, pasture, nonAgLand, total)
      land <- .formatReport(land, c("Cropland", "PasturesAndRangelands", "NonAgriculturalLand", "TotalLand"))
      .saveNetCDFReport(land, file = "Landuse", comment = "unit: Mh")
    },
    error = function(e)
    {
      message("Land-use disaggregation failed to calculate for the scenario ", scenario)
      message("Full error: ", e)
    })


  # --------------------------------------------------------------------------------
  # Nitrogen Budgets

  message("In getReportFSECStevenLord, calculating nutrient surplus for scenario: ", scenario)

  tryCatch(
    {
      # Nitrogen budget per unit cropland
      nbCropland <- reportNitrogenBudgetCropland(gdx_path, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
      nbCropland <- nbCropland[, , c("Fertilizer", "Nutrient Surplus", "N2O-N", "NH3-N", "NO2-N", "NO3-N")]
      nbCropland <- .formatReport(nbCropland, c("Fertilizer", "NutrientSurplus", "N2O_N", "NH3_N", "NO2_N", "NO3_N"))
      .saveNetCDFReport(nbCropland, file = "NitrogenBudget_Cropland", comment = "unit: Mt")

      # Nitrogen budget per unit pasture
      nbPasture <- reportNitrogenBudgetPasture(gdx_path, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
      nbPasture <- nbPasture[, , c("Fertilizer", "Nutrient Surplus", "N2O-N", "NH3-N", "NO2-N", "NO3-N")]
      nbPasture <- .formatReport(nbPasture, c("Fertilizer", "NutrientSurplus", "N2O_N", "NH3_N", "NO2_N", "NO3_N"))
      .saveNetCDFReport(nbPasture, file = "NitrogenBudget_Pasture", comment = "unit: Mt")

      # Nitrogen budget per unit Non-agricultural land
      nbNonAgLand <- reportNitrogenBudgetNonagland(gdx_path, grid = TRUE, dir = magpieOutputDir)
      nbNonAgLand <- nbNonAgLand[, , "Nutrient Surplus"]
      nbNonAgLand <- .formatReport(nbNonAgLand, "NutrientSurplus")
      .saveNetCDFReport(nbNonAgLand, file = "NitrogenBudget_NonAgLand", comment = "unit: Mt")

      # Nitrogen budget per unit total land
      nbManureExcretion <- reportGridManureExcretion(gdx_path, dir = magpieOutputDir)
      nbManureExcretion <- nbManureExcretion[, , c("Manure|Manure In Confinements|+|Losses",
                                                   "Manure|Manure In Confinements|Losses|N2O-N|Direct",
                                                   "Manure|Manure In Confinements|Losses|NH3-N",
                                                   "Manure|Manure In Confinements|Losses|NO2-N",
                                                   "Manure|Manure In Confinements|Losses|NO3-N")]
      nbManureExcretion <- .formatReport(nbManureExcretion, c("ManureInConfinements_TotalLosses",
                                                              "ManureInConfinements_Losses_N2O_N_Direct",
                                                              "ManureInConfinements_Losses_NH3_N",
                                                              "ManureInConfinements_Losses_NO2_N",
                                                              "ManureInConfinements_Losses_NO3_N"))
      .saveNetCDFReport(nbManureExcretion, file = "NitrogenBudget_ManureExcretion", comment = "unit: Mt")
    },
    error = function(e)
    {
      message("Nutrient surplus failed to calculate for the scenario ", scenario)
      message("Full error: ", e)
    })


  # --------------------------------------------------------------------------------
  # Biodiversity

  message("In getReportFSECStevenLord, collecting BII for scenario: ", scenario)

  BII_path <- file.path(magpieOutputDir, paste0(scenario, "_cell.bii_0.5.nc"))

  if (file.exists(BII_path)) {
    file.copy(from = BII_path, to = reportOutputDir)
  } else {
    message("BII dataset (cell.bii_0.5.nc) wasn't found for the scenario: ", scenario)
  }


  # --------------------------------------------------------------------------------
  # Population

  message("In getReportFSECStevenLord, collecting population datasets for scenario: ", scenario)

  pop_path <- file.path(magpieOutputDir, "../../input/FSEC_populationScenarios", "FSEC_populationScenarios_v2_22-08-22.mz")

  if (file.exists(pop_path)) {
    pop <- read.magpie(pop_path)

    config <- gms::loadConfig(file.path(magpieOutputDir, "config.yml"))
    pop <- pop[, , config$gms$c09_pop_scenario]
    getNames(pop) <- "value"

    # Ensure alignment of years
    yearsPresent <- Reduce(f = intersect, x = Map(getYears, list(gridLand, pop)))
    pop <- pop[, yearsPresent, ]

    # Round off projections' fractions of people and use persons rather than millions persons
    pop <- round(pop * 1E6)

    pop <- .formatReport(pop, "Population")
    .saveNetCDFReport(pop, file = "population", comment = "unit: Persons")
  } else {
    message("The population dataset wasn't found for the scenario: ", scenario)
  }


  # --------------------------------------------------------------------------------
  # Global Surface Temperature

  message("In getReportFSECStevenLord, collecting global surface temperature for scenario: ", scenario)

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
  # GDP PPP

  message("In getReportFSECStevenLord, collecting GDP (PPP) driver for scenario: ", scenario)

  gdp_path <- file.path(magpieOutputDir, "../../modules/09_drivers/input/f09_gdp_ppp_iso.csv")
  if (file.exists(gdp_path)) {
    file.copy(from = gdp_path, to = file.path(reportOutputDir, ".."))
  } else {
    message("Error in magpie4::getReportFSECStevenLord.R: f09_gdp_ppp_iso file not found.")
  }


  # --------------------------------------------------------------------------------
  # Drivers - GDP PPP

  message("In getReportFSECStevenLord, collecting GDP (PPP) driver for scenario: ", scenario)

  gdp_path <- file.path(magpieOutputDir, "../../modules/09_drivers/input/f09_gdp_ppp_iso.csv")
  if (file.exists(gdp_path)) {
    file.copy(from = gdp_path, to = file.path(reportOutputDir, ".."))
  } else {
    message("Error in magpie4::getReportFSECStevenLord.R: f09_gdp_ppp_iso file not found.")
  }


  # --------------------------------------------------------------------------------
  # Drivers - pop ISO

  message("In getReportFSECStevenLord, collecting population ISO driver for scenario: ", scenario)

  gdp_path <- file.path(magpieOutputDir, "../../modules/09_drivers/input/f09_pop_iso.csv")
  if (file.exists(gdp_path)) {
    file.copy(from = gdp_path, to = file.path(reportOutputDir, ".."))
  } else {
    message("Error in magpie4::getReportFSECStevenLord.R: f09_pop_iso.csv file not found.")
  }


  # --------------------------------------------------------------------------------
  # Drivers - demography

  message("In getReportFSECStevenLord, collecting demography for scenario: ", scenario)

  gdp_path <- file.path(magpieOutputDir, "../../modules/09_drivers/input/f09_demography.cs3")
  if (file.exists(gdp_path)) {
    file.copy(from = gdp_path, to = file.path(reportOutputDir, ".."))
  } else {
    message("Error in magpie4::getReportFSECStevenLord.R: f09_demography.cs3 file not found.")
  }


  # --------------------------------------------------------------------------------
  # GHG

  message("In getReportFSECStevenLord, collecting GHG emissions for scenario", scenario)

  report_path <- file.path(magpieOutputDir, "report.rds")
  ghgReport <- readRDS(report_path)

  ghgVariables <- c("Emissions|CH4|Land|Agriculture",
                    "Emissions|N2O|Land",
                    "Emissions|CO2|Land|+|Land-use Change",
                    "Emissions|GWP100AR6|Land",
                    "Emissions|GWP100AR6|Land|Cumulative")

  ghgReport <- ghgReport %>% filter(.data$variable %in% ghgVariables)

  if (nrow(ghgReport) > 0) {
    colnames(ghgReport) <- c("Model", "Scenario", "ISO", "Variable", "Unit", "Year", "Value")
    .saveCSVReport(ghgReport, "GHG")
  } else {
    message("The GHG variables weren't found in the report.rds for scenario: ", scenario)
  }


  # --------------------------------------------------------------------------------
  # Tau

  message("In getReportFSECStevenLord, collecting Tau for scenario", scenario)

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

  message("In getReportFSECStevenLord, collecting food system costs for scenario: ", scenario)

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

  message("In getReportFSECStevenLord, collecting dietary indicators for scenario: ", scenario)

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


  # --------------------------------------------------------------------------------
  # Return list

  return(list("Landuse"                        = land,
              "NitrogenBudget_Cropland"        = nbCropland,
              "NitrogenBudget_Pasture"         = nbPasture,
              "NitrogenBudget_NonAgLand"       = nbNonAgLand,
              "NitrogenBudget_ManureExcretion" = nbManureExcretion,
              "GHG"                            = ghgReport,
              "Tau"                            = tauReport,
              "Costs"                          = costReport,
              "caloricIntake"                  = caloricIntake,
              "DietaryIndicators"              = dietaryIndicators))

}
