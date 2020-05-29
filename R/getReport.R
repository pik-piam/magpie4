#' @title getReport
#' @description Puts together a report based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.report. If NULL the report is returned instead as a MAgPIE object.
#' @param scenario Name of the scenario used for the list-structure of a reporting object (x$scenario$MAgPIE). If NULL the report is returned instead as a MAgPIE object.
#' @param filter Modelstat filter. Here you have to set the modelstat values for which results should be used. All values for time steps in which the modelstat is different or for which one of the previous modelstats were different are set to NA.
#' @param detail Crop specific (TRUE) or aggregated outputs (FALSE)
#' @param ... additional arguments for write.report. Will only be taken into account if argument "file" is not NULL. 
#' @return A MAgPIE object containing the report in the case that "file" is NULL.
#' @details Reports are organize with '|' as level delimiter and summation symbols for grouping subcategories into entities e.g. for stackplots. Notice the following hints for the summation symbol placement:
#' \itemize{
#'   \item Every name should just contain one summation symbol (mostly '+').
#'   \item The position of the symbol (counted in '|' from left side) will determine the level.
#'   \item Every subitem containing the same summation symbol in the same level with the same supercategory name will be summed.
#'   \item Items without any summation symbol will ge ignored.
#'   \item Items with different summation symbols will be summed up separately.
#'   \item In most of the cases a summation symbol will be just placed before the last level (counted in '|' from left side).
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
#' @importFrom magclass write.report2 getSets<- getSets add_dimension
#' @importFrom methods is
#' @examples
#' 
#'   \dontrun{
#'     x <- getReport(gdx)
#'   }
#' 

getReport <- function(gdx,file=NULL,scenario=NULL,filter=c(1,2,7),detail=TRUE,...) {
  
  tryReport <- function(report, width, gdx) {
    regs  <- c(readGDX(gdx,"i"), "GLO")
    years <- readGDX(gdx,"t")
    message("   ",format(report,width=width),appendLF = FALSE)
    t <- system.time(x <- try(eval(parse(text=paste0("suppressMessages(",report,")"))), silent=TRUE))
    t <- paste0(" ",format(t["elapsed"], nsmall = 2, digits = 2),"s")
    if(is(x,"try-error")) {
      message("ERROR",t)
      x <- NULL
    } else if(is.null(x)) {
      message("no return value",t)  
      x <- NULL
    } else if(!is.magpie(x)) {
      message("ERROR - no magpie object",t)
      x <- NULL      
    } else if(!setequal(getYears(x),years)) {
      message("ERROR - wrong years",t)
      x <- NULL
    } else if(!setequal(getRegions(x),regs)) {
      message("ERROR - wrong regions",t)
      x <- NULL
    } else if(any(grepl(".",getNames(x),fixed=TRUE))){
      message("ERROR - data names contain dots (.)",t)
      x <- NULL
    } else {
      message("success",t)
    }
    return(x)
  }
  
  tryList <- function(..., gdx) {
      width <- max(nchar(c(...))) + 1
      return(lapply(unique(list(...)),tryReport, width, gdx))
  }
  
  message("Start getReport(gdx)...")
  
  t <- system.time(
    output <- tryList("reportPopulation(gdx)",
                    "reportIncome(gdx)",
                    "reportPriceGHG(gdx)",
                    "reportFoodExpenditure(gdx)",
                    "reportKcal(gdx)",
                    "reportIntakeDetailed(gdx,detail=detail)",
                    "reportLivestockShare(gdx)",
                    "reportLivestockDemStructure(gdx)",
                    "reportVegfruitShare(gdx)",
                    "reportPriceShock(gdx)",
                    "reportPriceElasticities(gdx)",
                    "reportDemand(gdx,detail=detail)",
                    "reportDemandBioenergy(gdx,detail=detail)",
                    "reportFeed(gdx,detail=detail)",
                    "reportProduction(gdx,detail=detail)",
                    "reportTrade(gdx,detail=detail)",
                    "reportLandUse(gdx)",
                    "reportLandUseChange(gdx)",
                    "reportProtectedArea(gdx)",
                    "reportCroparea(gdx,detail=detail)",
                    "reportNitrogenBudgetCropland(gdx)",
                    "reportNitrogenBudgetPasture(gdx)",
                    "reportManure(gdx)",
                    "reportYields(gdx,detail=detail)",
                    "reportTau(gdx)",
                    "reportTc(gdx)",
                    "reportEmissions(gdx)",
                    "reportEmissionsBeforeTechnicalMitigation(gdx)",
                    "reportCosts(gdx)",
                    "reportCostsPresolve(gdx)",
                    "reportPriceFoodIndex(gdx)",
                    "reportPriceAgriculture(gdx)",                  
                    "reportPriceBioenergy(gdx)",
                    "reportPriceLand(gdx)",
                    "reportPriceWater(gdx)",
                    "reportValueTrade(gdx)",
                    "reportProcessing(gdx, indicator='primary_to_process')",
                    "reportProcessing(gdx, indicator='secondary_from_primary')",
                    "reportAEI(gdx)",
                    "reportWaterUsage(gdx)",
                    "reportAAI(gdx)",
                    "reportSOM(gdx)",
                    "reportGrowingStock(gdx)",
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
                    "reportTimberDemand(gdx)",
                    "reportTimberProductionVolumetric(gdx)",
                    "reportTimberDemandVolumetric(gdx)",
                    "reportBII(gdx)",
                    gdx=gdx))
  
  message(paste0("Total runtime:  ",format(t["elapsed"], nsmall = 2, digits = 2),"s"))
  
  output <- .filtermagpie(mbind(output),gdx,filter=filter)
  
  getSets(output,fulldim = FALSE)[3] <- "variable"
  
  if(!is.null(scenario)) output <- add_dimension(output, dim=3.1, add="scenario", nm=gsub(".","_",scenario,fixed=TRUE))
  output <- add_dimension(output, dim=3.1, add="model", nm="MAgPIE")
  
  missing_unit <- !grepl("\\(.*\\)",getNames(output))
  if(any(missing_unit)) {
    warning("Some units are missing in getReport!")
    getNames(output)[missing_unit] <- paste(getNames(output)[missing_unit],"( )")
  }
  if(!is.null(file)) write.report2(output,file=file,...)
  else return(output)  
}
