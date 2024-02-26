#' @title getReportAgMIP
#' @description Puts together a report for the Agricultural Model Intercom- parison and Improvement Project
#' (AgMIP) based on a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.report. If NULL the report is returned
#' instead as a MAgPIE object.
#' @param scenario Name of the scenario used for the list-structure of a reporting object (x$scenario$MAgPIE).
#' If NULL the report is returned instead as a MAgPIE object.
#' @param filter Modelstat filter. Here you have to set the modelstat values for which results should be used.
#' All values for time steps in which the modelstat is different or for which one of the previous modelstats
#' were different are set to NA.
#' @param detail Crop specific (TRUE) or aggregated outputs (FALSE)
#' @param ... additional arguments for write.report. Will only be taken into account if argument "file" is not NULL.
#' @return A MAgPIE object containing the report in the case that "file" is NULL.
#' @details Reports are organize with '|' as level delimiter and summation symbols for grouping
#' subcategories into entities e.g. for stackplots. Notice the following hints for the summation
#' symbol placement:
#' \itemize{
#'   \item Every name should just contain one summation symbol (mostly '+').
#'   \item The position of the symbol (counted in '|' from left side) will determine the level.
#'   \item Every subitem containing the same summation symbol in the same level with the same
#'   supercategory name will be summed.
#'   \item Items without any summation symbol will ge ignored.
#'   \item Items with different summation symbols will be summed up separately.
#'   \item In most of the cases a summation symbol will be just placed before the last level
#'   (counted in '|' from left side).
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
#' @author Florian Humpenoeder, Isabelle Weindl, Felicitas Beier
#' @importFrom magclass write.report2 getSets<- getSets add_dimension getCells dimSums mbind new.magpie
#' getNames getYears
#' @importFrom methods is
#' @importFrom gdx readGDX
#' @importFrom madrat toolAggregate
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' x <- getReportAgMIP(gdx)
#' }
#'
getReportAgMIP <- function(gdx, file = NULL, scenario = NULL, filter = c(1, 2, 7),
                           detail = TRUE, ...) {

  message("Start getReportAgMIP(gdx)...")

  output <- tryList("reportPopulation(gdx)",
                    "reportIncome(gdx, type = 'mer')",
                    "reportAgEmployment(gdx, type = 'absolute', detail = TRUE)",
                    "reportAgEmployment(gdx, type = 'share', detail = TRUE)",
                    "reportHourlyLaborCosts(gdx)",
                    "reportProducerPriceIndex(gdx)",
                    "reportPriceGHG(gdx)",
                    #                    "reportFoodExpenditure(gdx)", #nolint
                    "reportKcal(gdx, detail = detail)",
                    "reportIntakeDetailed(gdx, detail = detail)",
                    #                    "reportLivestockShare(gdx)", #nolint
                    #                    "reportLivestockDemStructure(gdx)", #nolint
                    #                    "reportVegfruitShare(gdx)", #nolint
                    #                    "reportHunger(gdx)", #nolint
                    #                    "reportPriceShock(gdx)", #nolint
                    #                    "reportPriceElasticities(gdx)", #nolint
                    "reportProduction(gdx, detail = TRUE, agmip = TRUE)",
                    "reportDemand(gdx, detail = detail, agmip = TRUE)",
                    #                    "reportDemandBioenergy(gdx,detail=detail)", #nolint
                    "reportFeed(gdx, detail = detail)",
                    "reportTrade(gdx, detail = TRUE)",
                    "reportLandUse(gdx)",
                    #                    "reportLandUseChange(gdx)", #nolint
                    #                    "reportProtectedArea(gdx)", #nolint
                    "reportCroparea(gdx, detail = detail)",
                    "reportNitrogenPollution(gdx, dir = dir)",
                    #                    "reportNitrogenBudgetCropland(gdx)", #nolint
                    #                    "reportNitrogenBudgetPasture(gdx)", #nolint
                    #                    "reportManure(gdx)", #nolint
                    "reportYields(gdx, detail = detail)",
                    "reportTau(gdx)",
                    "reportTc(gdx)",
                    "reportYieldShifter(gdx)",
                    "reportEmissions(gdx)",
                    #                    "reportEmisAerosols(gdx)", #nolint
                    #                    "reportEmissionsBeforeTechnicalMitigation(gdx)", #nolint
                    #                    "reportEmisPhosphorus(gdx)", #nolint
                    #                    "reportCosts(gdx)", #nolint
                    #                    "reportCostsPresolve(gdx)", #nolint
                    #                    "reportPriceFoodIndex(gdx)", #nolint
                    #                    "reportPriceAgriculture(gdx)", #nolint
                    #                    "reportPriceBioenergy(gdx)", #nolint
                    #                    "reportPriceLand(gdx)", #nolint
                    #                    "reportPriceWater(gdx)", #nolint
                    "reportValueTrade(gdx, detail = TRUE)",
                    #                    "reportValueConsumption(gdx)", #nolint
                    #                   "reportProcessing(gdx, indicator='primary_to_process')", #nolint
                    #                    "reportProcessing(gdx, indicator='secondary_from_primary')", #nolint
                    #                    "reportAEI(gdx)", #nolint
                    "reportWaterUsage(gdx)",
                    "reportBII(gdx, dir = dir)",
                    #                    "reportAAI(gdx)", #nolint
                    "reportSOM(gdx)",
                    #                    "reportGrowingStock(gdx)", #nolint
                    #                    "reportSDG1(gdx)", #nolint
                    #                    "reportSDG2(gdx)", #nolint
                    #                    "reportSDG3(gdx)", #nolint
                    #                    "reportSDG6(gdx)", #nolint
                    #                    "reportSDG12(gdx)", #nolint
                    #                    "reportSDG15(gdx)", #nolint
                    #                    "reportForestYield(gdx)", #nolint
                    #                    "reportharvested_area_timber(gdx)", #nolint
                    #                    "reportPlantationEstablishment(gdx)", #nolint
                    #                    "reportRotationLength(gdx)", #nolint
                    gdx = gdx)

  x <- .filtermagpie(mbind(output), gdx, filter = filter)
  names(dimnames(x)) <- c("i", "year", "data")

  ### conversion to AgMIP regions in 3 steps
  # Downscaling from MAgPIE regions to country level
  # Aggregation from country level to AgMIP regions
  # Add AgMIP Extra regions

  # Mapping MAgPIE regions to country level
  i2iso <- readGDX(gdx, "i_to_iso")

  # save glo for later
  xGLO <- x["GLO", , ]

  # remove glo for the next steps
  x <- x["GLO", , , invert = TRUE]

  # pop as weight
  pop <- readGDX(gdx, "im_pop_iso")
  pop <- pop[, getYears(x), ]
  pop <- pop * 10^6

  # weight for disaggregation from magpie regions to country level
  w <- new.magpie(getCells(pop), getYears(x), getNames(x), fill = NA, sets = c("iso", "year", "data"))
  w[, , ] <- pop
  tmp <- c("Income", "Nutrition|", "Prices|", "Productivity|", "Trade|Self-sufficiency|")
  w[, , getNames(w[, , tmp, pmatch = "left"])] <- NA

  # do the disaggregation from magpie regions to country level
  y <- toolAggregate(x, i2iso, from = "i", to = "iso", weight = w, mixed_aggregation = TRUE)

  # weight for aggregation from country level to agmip regions
  w <- new.magpie(getCells(y), getYears(y), getNames(y), fill = NA, sets = c("iso", "year", "data"))
  tmp <- c("Income", "Nutrition|", "Prices|", "Productivity|", "Trade|Self-sufficiency|")
  w[, , getNames(w[, , tmp, pmatch = "left"])] <- pop

  # do the aggregation from country level to agmip regions
  mapAgMIP <- read.csv(system.file("extdata", mapping = "regionmappingAgMIP.csv", package = "magpie4"), as.is = TRUE,
                       sep = ";")
  z <- toolAggregate(y, mapAgMIP, from = "CountryCode", to = "RegionCode", weight = w,
                     mixed_aggregation = TRUE)

  ## add AgMIP special regions
  # AgMIP regions + AgMIP Supra regions

  # weight
  pop <- toolAggregate(pop, mapAgMIP, from = "CountryCode", to = "RegionCode")
  w <- new.magpie(getCells(pop), getYears(x), getNames(x), fill = NA, sets = c("i", "year", "data"))
  tmp <- c("Income", "Nutrition|", "Prices|", "Productivity|", "Trade|Self-sufficiency|")
  w[, , getNames(w[, , tmp, pmatch = "left"])] <- pop

  # do the aggregation. Only the AgMIP Supra regions will be added. The default AgMIP regions will remain unchanged.
  mapAgMIPextra <- read.csv(system.file("extdata", mapping = "regionmappingAgMIPextra.csv", package = "magpie4"),
                            as.is = TRUE, sep = ";")
  zz <- toolAggregate(z, mapAgMIPextra, from = "AgMIP", to = "AgMIPext", weight = w, mixed_aggregation = TRUE)

  # add global results als WLD
  getCells(xGLO) <- "WLD"
  zz <- mbind(zz, xGLO)

  # check
  dem <- "Demand (Mt DM/yr)"
  if ((sum(dimSums(x[, , dem], dim = 1) - dimSums(y[, , dem], dim = 1))) > 10e-3) {
    warning("MAgPIE and country level data differ. Check your script and mappings.")
  }
  if ((sum(dimSums(y[, , dem], dim = 1) - dimSums(z[, , dem], dim = 1))) > 10e-3) {
    warning("Country level and AgMIP region data differ. Check your script and mappings.")
  }

  output <- zz

  getSets(output, fulldim = FALSE)[3] <- "variable"

  if (!is.null(scenario)) {
    output <- add_dimension(output, dim = 3.1, add = "scenario", nm = gsub(".", "_", scenario, fixed = TRUE))
  }
  output <- add_dimension(output, dim = 3.1, add = "model", nm = "MAgPIE")

  missingUnit <- !grepl("\\(.*\\)", getNames(output))
  if (any(missingUnit)) {
    warning("Some units are missing in getReportAgMIP!")
    getNames(output)[missingUnit] <- paste(getNames(output)[missingUnit], "( )")
  }
  if (!is.null(file)) write.report2(output, file = file, ...)
  else return(output)
}
