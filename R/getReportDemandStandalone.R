#' @title getReportDemandStandalone
#' @description Puts together a report based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.report. If NULL the report is returned instead as a MAgPIE object.
#' @param scenario Name of the scenario used for the list-structure of a reporting object (x$scenario$MAgPIE). If NULL the report is returned instead as a MAgPIE object.
#' @param detail Crop specific (TRUE) or aggregated outputs (FALSE)
#' @param ... additional arguments for write.report. Will only be taken into account if argument "file" is not NULL. 
#' @return A MAgPIE object containing the report in the case that "file" is NULL.
#' @author Florian Humpenoeder
#' @importFrom magclass write.report
#' @examples
#' 
#'   \dontrun{
#'     x <- getReportDemandStandalone(gdx)
#'   }
#' 

getReportDemandStandalone <- function(gdx,file=NULL,scenario=NULL,detail=FALSE,...) {

  output <- NULL
  
  #Drivers
  output <- mbind(output,reportPopulation(gdx))
  output <- mbind(output,reportIncome(gdx))
  output <- mbind(output,reportFoodExpenditure(gdx))
  
  #Food Demand
  output <- mbind(output,reportKcal(gdx))
  output <- mbind(output,reportLivestockShare(gdx))
  output <- mbind(output,reportVegfruitShare(gdx))
  output <- mbind(output,reportHunger(gdx))
  output <- mbind(output,reportPriceShock(gdx))
  output <- mbind(output,reportPriceElasticities(gdx))
  
  #output <- .filtermagpie(output,gdx,filter=filter)
  
  names(dimnames(output))[3] <- "data"
  
  if(!is.null(scenario)) {
    tmp <- list()
    tmp[[scenario]]$MAgPIE <- output
    output <- tmp
  }
  
  if(!is.null(file)) write.report(output,file=file,...)
  else return(output)  
}
