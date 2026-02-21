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
#' @param level    An aggregation level (currently "regglo" or "iso") or the name of a mapping
#'                 that should be used by default to aggregate the report. The mapping can only map
#'                 from reg to other regions. Not all parts of the report will necessarily adhere to
#'                 this default aggregation level.
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
                      detail = TRUE, level = "regglo", ...) {

  message("Start getReport(gdx)...")

  t <- system.time(
    output <- tryList(
      "reportPopulation(gdx, level = level)",
      "reportWorkingAgePopulation(gdx, level = level)",
      "reportIncome(gdx,type='ppp', level = level)",
      "reportIncome(gdx,type='mer', level = level)",
      "reportPriceGHG(gdx, level = level)",
      "reportFoodExpenditure(gdx, level = level)",
      "reportKcal(gdx, detail = detail, level = level)",
      "reportProtein(gdx, detail = detail, level = level)",
      "reportIntakeDetailed(gdx, detail = detail, level = level)",
      "reportAnthropometrics(gdx, level = level)",
      "reportLivestockShare(gdx, level = level)",
      "reportLivestockDemStructure(gdx, level = level)",
      "reportVegfruitShare(gdx, level = level)",
      "reportPriceShock(gdx, level = level)",
      "reportPriceElasticities(gdx, level = level)",
      "reportDemand(gdx, detail = detail, level = level)",
      "reportDemandBioenergy(gdx, detail = detail, level = level)",
      "reportFeed(gdx, detail = detail, level = level)",
      "reportProduction(gdx, detail = detail, level = level)",
      "reportProductionBioenergy(gdx, detail = detail, level = level)",
      "reportTrade(gdx, detail = detail, level = level)",
      "reportLandUse(gdx, level = level)",
      "reportLandUseChange(gdx, level = level)",
      "reportNetForestChange(gdx, level = level)",
      "reportLandTransitionMatrix(gdx, level = level)",
      "reportPeatland(gdx, level = level)",
      "reportLandConservation(gdx, level = level)",
      "reportCroparea(gdx, detail = detail, level = level)",
      "reportNitrogenBudgetCropland(gdx, level = level)",
      "reportNitrogenBudgetPasture(gdx, level = level)",
      "reportNitrogenEfficiencies(gdx, level = level)",
      "reportManure(gdx, level = level)",
      "reportNitrogenPollution(gdx, level = level)",
      "reportYields(gdx, detail = detail, physical = TRUE, level = level)",
      "reportYields(gdx, detail = detail, physical = FALSE, level = level)",
      "reportYieldsCropCalib(gdx, detail = detail, level = level)",
      "reportYieldsCropRaw(gdx, detail = detail, level = level)",
      "reportFeedConversion(gdx)",
      "reportTau(gdx, level = level)",
      "reportTc(gdx, level = level)",
      "reportAgriResearchIntensity(gdx, level = level)",
      "reportEmissions(gdx, level = level)",
      "reportEmissionsBeforeTechnicalMitigation(gdx, level = level)",
      "reportCosts(gdx, level = level)",
      "reportCostsPresolve(gdx, level = level)",
      "reportCostTransport(gdx, level = level)",
      "reportCostsFertilizer(gdx, level = level)",
      "reportCostOverall(gdx, level = level)",
      "reportCostCapitalStocks(gdx, level = level)",
      "reportCostCapitalInvestment(gdx, level = level)",
      "reportCostsInputFactors(gdx, level = level)",
      "reportCostsAccounting(gdx, level = level)",
      "reportCostsWithoutIncentives(gdx, level = level)",
      "reportAgGDP(gdx, level = level)",
      "reportConsumVal(gdx, level = level)",
      "reportPriceFoodIndex(gdx, baseyear = 'y2010', level = level)",
      "reportPriceFoodIndex(gdx, baseyear = 'y2020', level = level)",
      "reportProducerPriceIndex(gdx, level = level)",
      "reportExpenditureFoodIndex(gdx, level = level)",
      "reportPriceAgriculture(gdx, level = level)",
      "reportPriceBioenergy(gdx, level = level)",
      "reportPriceLand(gdx, level = level)",
      "reportPriceWater(gdx, level = level)",
      "reportValueTrade(gdx, level = level)",
      "reportProcessing(gdx, indicator = 'primary_to_process', level = level)",
      "reportProcessing(gdx, indicator = 'secondary_from_primary', level = level)",
      "reportAEI(gdx, level = level)",
      "reportWaterUsage(gdx, detail = FALSE, level = level)",
      "reportWaterAvailability(gdx, level = level)",
      "reportAAI(gdx, level = level)",
      "reportSOM(gdx, level = level)",
      "reportGrowingStock(gdx, indicator='relative', level = level)",
      "reportGrowingStock(gdx, indicator='absolute', level = level)",
      "reportSDG1(gdx, level = level)",
      "reportSDG2(gdx, level = level)",
      "reportSDG3(gdx, level = level)",
      "reportSDG6(gdx, level = level)",
      "reportSDG12(gdx, level = level)",
      "reportSDG15(gdx, level = level)",
      "reportPBwater(gdx, level = 'regglo')",
      "reportPBland(gdx, level = level)",
      "reportPBbiosphere(gdx, level = level)",
      "reportPBnitrogen(gdx, level = 'regglo')",
      "reportForestYield(gdx, level = level)",
      "reportharvested_area_timber(gdx, level = level)",
      "reportPlantationEstablishment(gdx, level = level)",
      "reportRotationLength(gdx, level = level)",
      "reportTimber(gdx, level = level)",
      "reportBII(gdx, level = level)",
      "reportCropDiversity(gdx, level = level)",
      "reportPriceWoodyBiomass(gdx, level = level)",
      "reportCarbonstock(gdx, level = level)",
      "reportAgEmployment(gdx, type = 'absolute', detail = TRUE, level = level)",
      "reportAgEmployment(gdx, type = 'share', detail = TRUE, level = level)",
      "reportHourlyLaborCosts(gdx, level = level)",
      "reportRelativeHourlyLaborCosts(gdx, level = level)",
      "reportTotalHoursWorked(gdx, level = level)",
      "reportOutputPerWorker(gdx, level = level)",
      "reportValueMaterialDemand(gdx, level = level)",
      "reportFactorCostShares(gdx, type = 'requirements', level = level)",
      "reportFactorCostShares(gdx, type = 'optimization', level = level)",
      "reportFactorCostShares(gdx, type = 'accounting', level = level)",
      "reportWageDevelopment(gdx, baseYear = 2000, level = level)",
      "reportWageDevelopment(gdx, baseYear = 2010, level = level)",
      "reportWageDevelopment(gdx, baseYear = 2020, level = level)",
      "reportWaterIndicators(gdx, level = level)",
      "reportBioplasticDemand(gdx, level = level)",
      "reportCostsMACCS(gdx, level = level)",
      "reportLaborCostsEmpl(gdx, level = level)",
      "reportLaborProductivity(gdx, level = level)",
      "reportRuralDemandShares(gdx, type = 'tradOnly')",
      "reportCostsWholesale(gdx, level = level)",
      "reportFit(gdx, type = 'R2', level = 'grid')",
      "reportFit(gdx, type = 'R2', level = 'cell')",
      "reportExtraResidueEmissions(gdx, level = level)",
      "reportFireEmissions(gdx, level = level)",
      "reportBiochar(gdx, level = level)",
      gdx = gdx,
      level = level
    )
  )

  message(paste0("Total runtime:  ", format(t["elapsed"], nsmall = 2, digits = 2), "s"))

  # Unify regions, as we might have used different levels
  output <- Filter(Negate(is.null), output)
  regList <- lapply(output, function(m) getItems(m, 1))
  allRegs <- Reduce(union, regList)
  output <- lapply(output, function(m) add_columns(m, setdiff(allRegs, getItems(m, 1)), dim = 1))

  # Bind and remove incomplete timesteps
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
