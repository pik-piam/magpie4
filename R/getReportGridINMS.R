#' @title getReportGridINMS
#' @description Generates and saves a list of reports relevant to the INMS context
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param reportOutputDir Directory in which the reports are to be saved. If NULL, a list of reports (MAgPIE objects) is returned instead
#' @param magpieOutputDir Directory containing the MAgPIE run which is to be processed
#' @param scenario Name of the scenario used for the list-structure of a reporting object (x$scenario$MAgPIE). If NULL a list of reports (MAgPIE objects) is returned instead.
#' @param filter Modelstat filter. Here you have to set the modelstat values for which results should be used. All values for time steps in which the modelstat is different or for which one of the previous modelstats were different are set to NA.
#' @return A list of reports (MAgPIE objects)
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder, Michael Crawford
#' @importFrom magclass  getSets getNames<- write.magpie
#' @examples
#' 
#'   \dontrun{
#'     x <- getReportGridINMS(gdx)
#'   }
#' 

getReportGridINMS <- function(gdx, reportOutputDir = NULL, magpieOutputDir, scenario = NULL, filter = c(2, 7)) {
  
  .formatOutput <- function(x, category) {
    getSets(x)[c("d1.1", "d1.2")] <- c("iso", "cell")
    getSets(x, fulldim = FALSE)[3] <- "variable"
    getNames(x) <- paste0(category, getNames(x))
    
    x <- .filtermagpie(x, gdx, filter = filter)
    
    mapping <- toolGetMapping(name = "mappingPIAMtoINMS.csv", type = "sectoral")
    y <- toolAggregate(x = x, rel = mapping,
                       from = "piam", to = "inms",
                       partrel = TRUE,
                       dim = 3.1)
    
    y_prime = y[,,dimnames(y)[[3]][dimSums(as.magpie((!is.na(y))*1), dim = c(1,2)) > 0]]
    if (any(y != y_prime)) {
      y = y_prime
      warning("In magpie4::getReportGridINMS: A sub-dimension with NAs was removed from grid-level output report.")
    }
    
    return(y)
  }
  
  .saveReport <- function(x, file, comment = NULL) {
    if (!is.null(reportOutputDir) & !is.null(scenario)) {
      write.magpie(x, 
                   file_name = paste0(reportOutputDir, "/INMS_output-MAgPIE4-", scenario, "-", file, ".nc"), 
                   comment = comment)
    }
  }
  
  gridLand           <- reportGridLand(gdx, dir = magpieOutputDir)
  gridLand_formatted <- .formatOutput(x = gridLand, category = "Land Cover|")
  .saveReport(gridLand_formatted, file = "LandCover", comment = "unit: Mha X")
  
  # Add multicropping into croplands to calculated harvested area
  multicropping_parameter <- readGDX(gdx, "f18_multicropping")
  multicropping_parameter <- gdxAggregate(gdx,
                                          x = multicropping_parameter,
                                          to = "grid",
                                          absolute = FALSE,
                                          dir = magpieOutputDir)
  multicropping_parameter <- multicropping_parameter[, getItems(gridLand)$year, ]
  
  area_harvested <- gridLand[, , "Cropland"] * multicropping_parameter
  area_harvested <- .formatOutput(area_harvested, category = "Land Cover|")
  area_harvested <- area_harvested[,,"Cropland"]
  getNames(area_harvested) <- "Cropland Area Harvested"
  .saveReport(area_harvested, file = "LandCover_CroplandAreaHarvested", comment = "unit: Mha X")
  
  nitrogenBudgetCropland           <- reportNitrogenBudgetCropland(gdx, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
  nitrogenBudgetCropland_formatted <- .formatOutput(x = nitrogenBudgetCropland, category = "Cropland Budget|")
  .saveReport(nitrogenBudgetCropland_formatted, file = "Nitrogen_CroplandBudget", comment = "unit: Mt X")
  
  nitrogenBudgetPasture           <- reportNitrogenBudgetPasture(gdx, grid = TRUE, dir = magpieOutputDir, include_emissions = TRUE)
  nitrogenBudgetPasture_formatted <- .formatOutput(x = nitrogenBudgetPasture, category = "Pasture Budget|")
  .saveReport(nitrogenBudgetPasture_formatted, file = "Nitrogen_PastureBudget", comment = "unit: Mt X")
  
  nitrogenBudgetNonAgLand           <- reportNitrogenBudgetNonagland(gdx, grid = TRUE, dir = magpieOutputDir)
  nitrogenBudgetNonAgLand_formatted <- .formatOutput(x = nitrogenBudgetNonAgLand, category = "Nonagland Budget|")
  .saveReport(nitrogenBudgetNonAgLand_formatted, file = "Nitrogen_NonAgriculturalLandBudget", comment = "unit: Mt X")
  
  gridManureExcretion           <- reportGridManureExcretion(gdx, dir = magpieOutputDir)
  gridManureExcretion_formatted <- .formatOutput(x = gridManureExcretion, category = "")
  .saveReport(gridManureExcretion_formatted, file = "Nitrogen_Manure", comment = "unit: Mt X")
  
  return(list("gridLand"                = gridLand_formatted,
              "nitrogenBudgetCropland"  = nitrogenBudgetCropland_formatted,
              "nitrogenBudgetPasture"   = nitrogenBudgetPasture_formatted,
              "nitrogenBudgetNonAgLand" = nitrogenBudgetNonAgLand_formatted,
              "gridManureExcretion"     = gridManureExcretion_formatted))
}
