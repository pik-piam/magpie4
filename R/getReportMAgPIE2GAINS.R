#' @title getReportMAgPIE2GAINS
#' @description Puts together a report for LPJmL or other biophysical models based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param folder a folder name the output should be written to using write.report. If NULL the report is returned instead as a MAgPIE object.
#' @param scenario Name of the scenario used for the list-structure of a reporting object (x$scenario$MAgPIE). If NULL the report is returned instead as a MAgPIE object.
#' @param filter Modelstat filter. Here you have to set the modelstat values for which results should be used. All values for time steps in which the modelstat is different or for which one of the previous modelstats were different are set to NA.
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @param ... additional arguments for write.report. Will only be taken into account if argument "file" is not NULL. 
#' @return A MAgPIE object containing the report in the case that "file" is NULL.
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder
#' @importFrom magclass write.report2 getSets<- getSets add_dimension is.magpie
#' @importFrom methods is
#' @examples
#' 
#'   \dontrun{
#'     x <- getReportMAgPIE2LPJmL(gdx)
#'   }
#' 

getReportMAgPIE2GAINS <- function(gdx,folder=NULL,scenario=NULL,filter=c(2,7),dir=".",spamfiledirectory="",...) {
  
  dir <- getDirectory(dir,spamfiledirectory)
  
  tryReport <- function(reporting, gdx,filter,scenario) {
    file=reporting[[2]]
    report = reporting[[1]]
    regs  <- c(readGDX(gdx,"i"))
    years <- readGDX(gdx,"t")
    message("   ",format(report),appendLF = FALSE)
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

      x <- .filtermagpie(x,gdx,filter=filter)
    
      getSets(x,fulldim = FALSE)[3] <- "variable"
    
      if(!is.null(scenario)) x <- add_dimension(x, dim=3.1, add="scenario", nm=scenario)
      #x <- add_dimension(x, dim=3.1, add="model", nm="MAgPIE")
    
    
      if(!is.null(file)) write.magpie(x,file_name = file)
    }
  }
  
  message("Start getReport(gdx)...")
  
  reporting= list(
    list("reportNitrogenBudgetCropland(gdx,grid=TRUE,dir=dir)",paste0(folder,"NitrogenBudgetCropland.nc")),
    list("reportNitrogenBudgetPasture(gdx,grid=TRUE,dir=dir)",paste0(folder,"NitrogenBudgetPasture.nc")),
    list("reportGridManureExcretion(gdx,grid=TRUE,dir=dir)",paste0(folder,"NitrogenBudgetNonagland.nc"))
  )
  
  output <- lapply(X = reporting, FUN=tryReport, gdx=gdx,filter=filter,scenario=scenario)

}
