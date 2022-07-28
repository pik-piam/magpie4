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
#' @importFrom madrat toolGetMapping toolConditionalReplace
#' @importFrom dplyr %>% group_by summarise
#' @importFrom rlang .data
#' @importFrom utils download.file
#' @examples
#'
#'   \dontrun{
#'     x <- getReportFSECPollution(gdx, magpieOutputDir)
#'   }
#'

getReportFSECPollution <- function(gdx, reportOutputDir = NULL, magpieOutputDir, scenario = NULL) {


  # Functions -------------------------------------------------------------------------------------------------------

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

  .saveCSVReport <- function(x, file) {
    if (!is.null(reportOutputDir) && !is.null(scenario)) {
      write.csv(x,
                file = file.path(reportOutputDir, paste0(scenario, "-", file, ".csv")),
                row.names = FALSE)
    }
  }


  # Land use --------------------------------------------------------------------------------------------------------

  gridLand <- reportGridLand(gdx, dir = magpieOutputDir)

  # Add multicropping parameter into croplands
  multicropping_parameter <- readGDX(gdx, "f18_multicropping")
  multicropping_parameter <- gdxAggregate(gdx,
                                          x = multicropping_parameter,
                                          to = "grid",
                                          absolute = FALSE,
                                          dir = magpieOutputDir)
  multicropping_parameter <- multicropping_parameter[, getItems(gridLand)$year, ]

  # Croplands (by area harvested)
  areaHarvested <- gridLand[, , "Cropland"] * multicropping_parameter
  areaHarvested[areaHarvested < 0.0001] <- 0 # Remove minuscule values of Cropland (< 10 ha per grid cell)

  # Pastures
  pasture <- gridLand[, , "Pastures and Rangelands"]
  pasture[pasture < 0.0001] <- 0 # Remove minuscule values of Pasture (< 10 ha per grid cell)

  # Non-agricultural land
  nonAgLand <- gridLand[, , c("Managed Forest", "Primary Forest", "Secondary Forest", "Urban Area", "Other Land")]
  nonAgLand <- dimSums(nonAgLand, dim = 3)
  nonAgLand[nonAgLand < 0.0001] <- 0 # Remove minuscule values of non-agricultural land (< 10 ha per grid cell)

  # Total land
  total <- dimSums(gridLand, dim = 3)


  # Derive intensity datasets ---------------------------------------------------------------------------------------

  # Nitrogen budget per unit cropland
  nbCropland <- reportNitrogenBudgetCropland(gdx, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
  nbCropland_perAreaHarvested <- (nbCropland / areaHarvested) * 1000 # Mt X / Mha to kg X / ha
  nbCropland_perAreaHarvested <- madrat::toolConditionalReplace(x = nbCropland_perAreaHarvested,
                                                                conditions = "!is.finite()", replaceby = 0)

  # Nitrogen budget per unit pasture
  nbPasture <- reportNitrogenBudgetPasture(gdx, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
  nbPasture_perAreaPasture <- (nbPasture / pasture) * 1000 # Mt X / Mha to kg X / ha
  nbPasture_perAreaPasture <- madrat::toolConditionalReplace(x = nbPasture_perAreaPasture,
                                                             conditions = "!is.finite()", replaceby = 0)

  # Nitrogen budget per unit Non-agricultural land
  nbNonAgLand <- reportNitrogenBudgetNonagland(gdx, grid = TRUE, dir = magpieOutputDir)
  nbNonAgLand_perAreaNonAgLand <- (nbNonAgLand / nonAgLand) * 1000 # Mt X / Mha to kg X / ha
  nbNonAgLand_perAreaNonAgLand <- madrat::toolConditionalReplace(x = nbNonAgLand_perAreaNonAgLand,
                                                                 conditions = "!is.finite()", replaceby = 0)

  # Nitrogen budget per unit total land
  nbManureExcretion <- reportGridManureExcretion(gdx, dir = magpieOutputDir)
  nbManureExcretion_perAreaTotalLand <- (nbManureExcretion / total) * 1000  # Mt X / Mha to kg X / ha
  nbManureExcretion_perAreaTotalLand <- madrat::toolConditionalReplace(x = nbManureExcretion_perAreaTotalLand,
                                                                       conditions = "!is.finite()", replaceby = 0)

  # Combined nutrientSurplus, with and without natural vegetation
  nutrientSurplus_noNat <- nbCropland_perAreaHarvested[, , "Nutrient Surplus.Cropland"] +
    nbPasture_perAreaPasture[, , "Nutrient Surplus.Pastures and Rangelands"] +
    nbManureExcretion_perAreaTotalLand[, , "Manure|Manure In Confinements|+|Losses"]

  nutrientSurplus_withNat <- nutrientSurplus_noNat + nbNonAgLand_perAreaNonAgLand[, , "Nutrient Surplus"]

  nutrientSurplus_noNat   <- .formatReport(nutrientSurplus_noNat, "Nutrient Surplus without NatVeg")
  nutrientSurplus_withNat <- .formatReport(nutrientSurplus_withNat, "Nutrient Surplus with NatVeg")


  # Saving datasets -------------------------------------------------------------------------------------------------

  ######################################
  # Unaggregated
  nutrientSurplus_unaggregated <- nutrientSurplus_noNat
  nutrientSurplus_unaggregated <- .formatReport(nutrientSurplus_unaggregated, "nutrientSurplus_perArea")
  .saveNetCDFReport(nutrientSurplus_unaggregated,
                    file = "nutrientSurplus_anthropogenic_unaggregated",
                    comment = "unit: kg N / ha")

  ######################################
  # Country-level aggregation
  nutrientSurplus_country <- as.data.frame(nutrientSurplus_noNat)[, c("Cell", "Region", "Year", "Value")]
  nutrientSurplus_country <- nutrientSurplus_country %>%
    dplyr::group_by(.data$Region, .data$Year) %>%
    dplyr::summarise(Value = round(mean(.data$Value), 2))
  names(nutrientSurplus_country) <- c("Region", "Year", "Nutrient Surplus (kg X / ha)")
  .saveCSVReport(nutrientSurplus_country,  "nutrientSurplus_anthropogenic_country")

  ######################################
  # Planetary boundary --- don't use the average!
  totalNutrientSurplus_noNat <- nbCropland[, , "Nutrient Surplus"] +
    nbPasture[, , "Nutrient Surplus"] +
    nbManureExcretion[, , "Manure|Manure In Confinements|+|Losses"]
  planetaryBoundary <- dimSums(totalNutrientSurplus_noNat, dim = c(1, 3))
  planetaryBoundary <- as.data.frame(planetaryBoundary)[, c("Year", "Value")]
  planetaryBoundary$Value <- round(planetaryBoundary$Value, 2)
  names(planetaryBoundary) <- c("Year", "Nutrient Surplus (Mt X)")
  .saveCSVReport(planetaryBoundary, "planetaryBoundary_anthropogenic")

  ######################################
  # Population aggregations
  popFile <- file.path(magpieOutputDir, "../../input/FSEC_populationScenarios", "FSEC_populationScenarios_v1_12-07-22.mz")
  pop <- read.magpie(popFile)

  config <- gms::loadConfig(file.path(magpieOutputDir, "config.yml"))
  pop <- pop[, , config$gms$c09_pop_scenario]
  getNames(pop) <- "value"

  # Ensure alignment of years
  yearsPresent <- Reduce(f = intersect, x = Map(getYears, list(nutrientSurplus_noNat, pop)))
  pop <- pop[, yearsPresent, ]

  # Round off projections' fractions of people and use persons rather than millions persons
  pop <- round(pop * 1E6)

  ######################################
  # S / P
  totalNutrientSurplus_noNat <- nbCropland[, , "Nutrient Surplus"] +
    nbPasture[, , "Nutrient Surplus"] +
    nbManureExcretion[, , "Manure|Manure In Confinements|+|Losses"]

  nutrientSurplus_perPopulation <- totalNutrientSurplus_noNat / pop
  nutrientSurplus_perPopulation <- madrat::toolConditionalReplace(x = nutrientSurplus_perPopulation,
                                                                  conditions = "!is.finite()", replaceby = 0)
  nutrientSurplus_perPopulation <- .formatReport(nutrientSurplus_perPopulation, "nutrientSurplus_perPopulation")
  .saveNetCDFReport(nutrientSurplus_perPopulation,
                    file = "nutrientSurplus_anthropogenic_perPopulation",
                    comment = "unit: (kg N / ha) / person")

  ######################################
  # S * P
  nutrientSurplus_byPopulation <- nutrientSurplus_noNat * pop
  nutrientSurplus_byPopulation <- madrat::toolConditionalReplace(x = nutrientSurplus_byPopulation,
                                                                 conditions = "!is.finite()", replaceby = 0)
  nutrientSurplus_byPopulation <- .formatReport(nutrientSurplus_byPopulation, "nutrientSurplus_byPopulation")
  .saveNetCDFReport(nutrientSurplus_byPopulation,
                    file = "nutrientSurplus_anthropogenic_byPopulation",
                    comment = "unit: (kg N / ha) * person")

  ######################################
  # Pollution categories
  CountryToCell <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")

  lessthan50     <- nutrientSurplus_withNat < 50
  from50to100    <- nutrientSurplus_withNat >= 50  & nutrientSurplus_withNat < 100
  from100to200   <- nutrientSurplus_withNat >= 100 & nutrientSurplus_withNat < 200
  greaterthan200 <- nutrientSurplus_withNat >= 200

  # Less than 50
  pop_lessthan50 <- pop
  pop_lessthan50[!lessthan50] <- 0
  pop_lessthan50_country <- toolAggregate(pop_lessthan50, rel = CountryToCell, from = "celliso", to = "iso")
  getNames(pop_lessthan50_country) <- "lessthan50"

  # From 50 to 100
  pop_from50to100 <- pop
  pop_from50to100[!from50to100] <- 0
  pop_from50to100_country <- toolAggregate(pop_from50to100, rel = CountryToCell, from = "celliso", to = "iso")
  getNames(pop_from50to100_country) <- "from50to100"

  # From 100 to 200
  pop_from100to200 <- pop
  pop_from100to200[!from100to200] <- 0
  pop_from100to200_country <- toolAggregate(pop_from100to200, rel = CountryToCell, from = "celliso", to = "iso")
  getNames(pop_from100to200_country) <- "from100to200"

  # Greater than 200
  pop_greaterthan200 <- pop
  pop_greaterthan200[!greaterthan200] <- 0
  pop_greaterthan200_country <- toolAggregate(pop_greaterthan200, rel = CountryToCell, from = "celliso", to = "iso")
  getNames(pop_greaterthan200_country) <- "greaterthan200"

  pop_all_country <- mbind(pop_lessthan50_country,
                           pop_from50to100_country,
                           pop_from100to200_country,
                           pop_greaterthan200_country)
  pop_all_country <- as.data.frame(pop_all_country)[, c("Region", "Year", "Data1", "Value")]
  names(pop_all_country) <- c("ISO", "Year", "pollutionCategory", "Population")
  .saveCSVReport(pop_all_country, "population_perNutrientSurplusCategory")


  # Return list -----------------------------------------------------------------------------------------------------

  return(list("nutrientSurplus_anthropogenic_unaggregated"  = nutrientSurplus_unaggregated,
              "nutrientSurplus_anthropogenic_country"       = nutrientSurplus_country,
              "planetaryBoundary_anthropogenic"             = planetaryBoundary,
              "nutrientSurplus_anthropogenic_perPopulation" = nutrientSurplus_perPopulation,
              "nutrientSurplus_anthropogenic_byPopulation"  = nutrientSurplus_byPopulation,
              "population_perNutrientSurplusCategory"       = pop_all_country))

}
