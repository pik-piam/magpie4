#' @title getReportINMS
#' @description Puts together a report for the INMS project based on a MAgPIE gdx file
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
#' @author Benjamin Bodirsky, Florian Humpenoeder
#' @importFrom magclass write.report2 getSets<- getSets add_dimension is.magpie
#' @importFrom methods is
#' @examples
#' 
#'   \dontrun{
#'     x <- getReport(gdx)
#'   }
#' 

getReportINMS <- function(gdx,file=NULL,scenario=NULL,filter=c(2,7),detail=TRUE,...) {
  
  dir="."
  
  tryReport <- function(report, width, gdx) {
    regs  <- c(readGDX(gdx,"i"), "GLO")
    years <- readGDX(gdx,"t")
    message("   ",format(report,width=width),appendLF = FALSE)
    x <- try(eval(parse(text=paste0(report))), silent=TRUE)
    if(is(x,"try-error")) {
      message("ERROR")
      x <- NULL
    } else if(is.null(x)) {
      message("no return value")  
      x <- NULL
    } else if(!is.magpie(x)) {
      message("ERROR - no magpie object")
      x <- NULL      
    } else if(!setequal(getYears(x),years)) {
      message("ERROR - wrong years")
      x <- NULL
    } else if(!setequal(getRegions(x),regs)) {
      message("ERROR - wrong regions")
      x <- NULL
    } else {
      message("success")
    }
    return(x)
  }
  
  tryList <- function(..., gdx) {
      width <- max(nchar(c(...))) + 1
      return(lapply(list(...),tryReport, width, gdx))
  }
  
  message("Start getReport(gdx)...")
  
  output <- tryList("reportDemandNr(gdx,detail=detail)",
                    "reportProductionNr(gdx,detail=detail)",
                    "reportLandUse(gdx)",
                    "reportNitrogenBudgetCropland(gdx)",
                    "reportNitrogenBudgetPasture(gdx)",
                    "reportNitrogenBudgetNonagland(gdx,dir=dir)",
                    "reportManure(gdx)",
                    "reportEmissions(gdx)",
                    "reportSOM(gdx)",
                    gdx=gdx)
  
  output <- .filtermagpie(mbind(output),gdx,filter=filter)
  
  getSets(output,fulldim = FALSE)[3] <- "variable"
  
  if(!is.null(scenario)) output <- add_dimension(output, dim=3.1, add="scenario", nm=scenario)
  output <- add_dimension(output, dim=3.1, add="model", nm="MAgPIE")
  
  
  if(!is.null(file)) write.report2(output,file=file,...)
  else return(output)  
}

