#' @title getReport
#' @description Puts together a report based on a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx      GDX file
#' @param file     a file name the output should be written to using write.report.
#'                 If NULL the report is returned instead as a MAgPIE object.
#' @param scenario Name of the scenario used for the list-structure of a
#'                 reporting object (x$scenario$MAgPIE).
#'                 If NULL the report is returned instead as a MAgPIE object.
#' @param filter   Modelstat filter. Here you have to set the modelstat values
#'                 for which results should be used.
#'                 All values for time steps in which the modelstat is different
#'                 or for which one of the previous modelstats were different are set to NA.
#' @param detail   Crop specific (TRUE) or aggregated outputs (FALSE)
#' @param dir      for gridded intermediate outputs: magpie output directory
#'                 which contains a mapping file (rds or spam)
#' @param ...      additional arguments for write.report.
#'                 Will only be taken into account if argument "file" is not NULL.
#' @return A MAgPIE object containing the report in the case that "file" is NULL.
#' @details Reports are organize with '|' as level delimiter and summation symbols
#'          for grouping subcategories into entities e.g. for stackplots.
#'          Notice the following hints for the summation symbol placement:
#' \itemize{
#'   \item Every name should just contain one summation symbol (mostly '+').
#'   \item The position of the symbol (counted in '|' from left side) will determine the level.
#'   \item Every subitem containing the same summation symbol in the same level
#'         with the same supercategory name will be summed.
#'   \item Items without any summation symbol will ge ignored.
#'   \item Items with different summation symbols will be summed up separately.
#'   \item In most of the cases a summation symbol will be just placed
#'         before the last level (counted in '|' from left side).
#'   \item It is helpful to think about which group of items should be stacked in a stackplot.
#' }
#'   An example how a summation symbol placement could look like:
#'   \preformatted{  Toplevel
#'   Toplevel|+|Item 1
#'   Toplevel|+|Item 2
#'   Toplevel|Item 2|+|Subitem 1
#'   Toplevel|Item 2|+|Subitem 1
#'   Toplevel|++|Item A
#'   Toplevel|++|Item B
#'   Toplevel|Item ?}
#'
#' @author Florian Humpenoeder
#' @importFrom magclass write.report2 getSets add_dimension
#' @importFrom methods is
#' @examples
#' \dontrun{
#' x <- getReport(gdx)
#' }
#'
getReport <- function(gdx, file = NULL, scenario = NULL, filter = c(1, 2, 7),
                      detail = TRUE, dir = ".", ...) {
  tryReport <- function(report, width, gdx) {
    regs <- c(readGDX(gdx, "i"), "GLO")
    years <- readGDX(gdx, "t")
    message("   ", format(report, width = width), appendLF = FALSE)
    t <- system.time(x <- try(eval(parse(text = paste0("suppressMessages(", report, ")"))), silent = TRUE))
    t <- paste0(" ", format(t["elapsed"], nsmall = 2, digits = 2), "s")
    if (is(x, "try-error")) {
      message("ERROR", t)
      x <- NULL
    } else if (is.null(x)) {
      message("no return value", t)
      x <- NULL
    } else if (is.character(x)) {
      message(x, t)
      x <- NULL
    } else if (!is.magpie(x)) {
      message("ERROR - no magpie object", t)
      x <- NULL
    } else if (!setequal(getYears(x), years)) {
      message("ERROR - wrong years", t)
      x <- NULL
    } else if (!setequal(getItems(x, dim = 1), regs)) {
      message("ERROR - wrong regions", t)
      x <- NULL
    } else if (any(grepl(".", getNames(x), fixed = TRUE))) {
      message("ERROR - data names contain dots (.)", t)
      x <- NULL
    } else {
      message("success", t)
    }
    return(x)
  }

  tryList <- function(..., gdx) {
    width <- max(nchar(c(...))) + 1
    return(lapply(unique(list(...)), tryReport, width, gdx))
  }

  message("Start getReport(gdx)...")

  t <- system.time(
    output <- tryList("reportPopulation(gdx)",
      "reportIncome(gdx)",
      "reportPriceGHG(gdx)",
      "reportFoodExpenditure(gdx)",
      "reportKcal(gdx,detail=detail)",
      "reportProtein(gdx,detail=detail)",
      "reportIntakeDetailed(gdx,detail=detail)",
      "reportAnthropometrics(gdx)",
      "reportLivestockShare(gdx)",
      "reportLivestockDemStructure(gdx)",
      "reportVegfruitShare(gdx)",
      "reportPriceShock(gdx)",
      "reportPriceElasticities(gdx)",
      "reportDemand(gdx,detail=detail)",
      "reportDemandBioenergy(gdx,detail=detail)",
      "reportFeed(gdx,detail=detail)",
      "reportProduction(gdx,detail=detail)",
      "reportProductionBioenergy(gdx,detail=detail)",
      "reportTrade(gdx,detail=detail)",
      "reportLandFootprint(gdx)",
      "reportLandUse(gdx)",
      "reportLandUseChange(gdx)",
      "reportNetForestChange(gdx)",
      "reportPeatland(gdx)",
      "reportProtectedArea(gdx)",
      "reportCroparea(gdx,detail=detail)",
      "reportNitrogenBudgetCropland(gdx, dir = dir)",
      "reportNitrogenBudgetPasture(gdx, dir = dir)",
      "reportManure(gdx)",
      "reportNitrogenPollution(gdx, dir = dir)",
      "reportYields(gdx,detail=detail)",
      "reportYieldsCropCalib(gdx,detail=detail)",
      "reportYieldsCropRaw(gdx,detail=detail)",
      "reportTau(gdx)",
      "reportTc(gdx)",
      "reportAgriResearchIntensity(gdx)",
      "reportEmissions(gdx)",
      "reportEmissionsBeforeTechnicalMitigation(gdx)",
      "reportCosts(gdx)",
      "reportCostsPresolve(gdx)",
      "reportCostTransport(gdx)",
      "reportCostsFertilizer(gdx)",
      "reportCostOverall(gdx)",
      "reportCostCapitalStocks(gdx)",
      "reportCostCapitalInvestment(gdx)",
      "reportCostsAEI(gdx)",
      "reportCostInputsCrop(gdx)",
      "reportCostsInputFactors(gdx)",
      "reportCostsAccounting(gdx)",
      "reportCostsProductionCrops(gdx,type = 'investment')",
      "reportCostsWithoutIncentives(gdx)",
      "reportAgGDP(gdx)",
      "reportConsumVal(gdx)",
      "reportPriceFoodIndex(gdx,baseyear = 'y2010')",
      "reportPriceFoodIndex(gdx,baseyear = 'y2020')",
      "reportExpenditureFoodIndex(gdx)",
      "reportPriceAgriculture(gdx)",
      "reportPriceBioenergy(gdx)",
      "reportPriceLand(gdx)",
      "reportPriceWater(gdx)",
      "reportValueTrade(gdx)",
      "reportProcessing(gdx, indicator='primary_to_process')",
      "reportProcessing(gdx, indicator='secondary_from_primary')",
      "reportAEI(gdx)",
      "reportWaterUsage(gdx)",
      "reportWaterAvailability(gdx)",
      "reportAAI(gdx)",
      "reportSOM(gdx)",
      "reportGrowingStock(gdx, indicator='relative')",
      "reportGrowingStock(gdx, indicator='absolute')",
      "reportSDG1(gdx)",
      "reportSDG2(gdx)",
      "reportSDG3(gdx)",
      "reportSDG6(gdx)",
      "reportSDG12(gdx)",
      "reportSDG15(gdx)",
      "reportForestYield(gdx)",
      "reportharvested_area_timber(gdx)",
      "reportPlantationEstablishment(gdx)",
      "reportRotationLength(gdx)",
      "reportTimber(gdx)",
      "reportBII(gdx, dir = dir)",
      "reportCropDiversity(gdx)",
      "reportPriceWoodyBiomass(gdx)",
      "reportCarbonstock(gdx)",
      "reportGrasslandManagement(gdx)",
      "reportGrassStats(gdx)",
      "reportGrasslandYields(gdx)",
      "reportLSUGrasslands(gdx)",
      "reportAgEmployment(gdx, type = 'absolute', detail = TRUE)",
      "reportAgEmployment(gdx, type = 'share', detail = TRUE)",
      "reportHourlyLaborCosts(gdx)",
      "reportRelativeHourlyLaborCosts(gdx)",
      "reportTotalHoursWorked(gdx)",
      "reportOutputPerWorker(gdx)",
      "reportValueMaterialDemand(gdx)",
      "reportFactorCostShares(gdx, type = 'baseline')",
      "reportFactorCostShares(gdx, type = 'optimization')",
      "reportFactorCostShares(gdx, type = 'accounting')",
      "reportWageDevelopment(gdx, baseYear = 2000)",
      "reportWageDevelopment(gdx, baseYear = 2010)",
      "reportWageDevelopment(gdx, baseYear = 2020)",
      "reportWaterIndicators(gdx)",
      "reportBioplasticDemand(gdx)",
      "reportCostsMACCS(gdx)",
      "reportLaborCostsEmpl(gdx)",
      "reportLaborProductivity(gdx)",
      gdx = gdx
    )
  )

  message(paste0("Total runtime:  ", format(t["elapsed"], nsmall = 2, digits = 2), "s"))

  output <- .filtermagpie(mbind(output), gdx, filter = filter)

  getSets(output, fulldim = FALSE)[3] <- "variable"

  if (!is.null(scenario)) {
    output <- add_dimension(output,
      dim = 3.1,
      add = "scenario",
      nm = gsub(".", "_", scenario, fixed = TRUE)
    )
  }
  output <- add_dimension(output, dim = 3.1, add = "model", nm = "MAgPIE")

  missingUnit <- !grepl("\\(.*\\)", getNames(output))
  if (any(missingUnit)) {
    warning("Some units are missing in getReport!")
    warning("Missing units in:", getNames(output)[which(!grepl("\\(.*\\)", getNames(output)) == TRUE)])
    getNames(output)[missingUnit] <- paste(getNames(output)[missingUnit], "( )")
  }
  if (!is.null(file)) {
    write.report2(output, file = file, ...)
  } else {
    return(output)
  }
}
