#' @title getReportMAgPIE2LPJmL
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
#' @param version versionnumber for the run names
#' @param ... additional arguments for write.report. Will only be taken into account if argument "file" is not NULL. 
#' @return A MAgPIE object containing the report in the case that "file" is NULL.
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder
#' @importFrom magclass write.report2 getSets<- getSets add_dimension is.magpie
#' @importFrom methods is
#' @importFrom iamc RenameAndAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- getReportMAgPIE2LPJmL(gdx)
#'   }
#' 

getReportGridINMS <- function(gdx,folder=NULL,scenario=NULL,filter=c(2,7),dir=".",spamfiledirectory="",version="v8",...) {
  
  dir <- getDirectory(dir,spamfiledirectory)
  
  tryReport <- function(reporting, gdx,filter,scenario) {
    
    report = reporting[[1]]
    file=reporting[[2]]
    category = reporting[[3]]
    
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
      
      mapping="mapping_inms_grid.csv"
      getNames(x)=paste0(category,getNames(x))
      
      y = RenameAndAggregate(data = list(model = list(scenario = x)),mapping = mapping,missing_log = NULL)
      y = y[[1]][[1]]
      y=y[,,dimnames(y)[[3]][dimSums(as.magpie((!is.na(y))*1),dim=c(1,2))>0]]

      if(!is.null(file)) write.magpie(x,file_name = file)
    }
  }
  
  message("Start getReport(gdx)...")
  
  reporting= list(
    list("reportGridLand(gdx,dir=dir)", paste0(folder,scenario,"-","LandCover","-",version,".nc"),"Land Cover|"),
    list("reportNitrogenBudgetCropland(gdx,grid=TRUE,dir=dir,include_emissions=TRUE)",paste0(folder,scenario,"-","Nitrogen_CroplandBudget","-",version,".nc"),"Cropland Budget|"),
    list("reportNitrogenBudgetPasture(gdx,grid=TRUE,dir=dir,include_emissions=TRUE)",paste0(folder,scenario,"-","Nitrogen_PastureBudget","-",version,".nc"),"Pasture Budget|"),
    list("reportNitrogenBudgetNonagland(gdx,grid=TRUE,dir=dir)",paste0(folder,scenario,"-","Nitrogen_NonAgriculturalLandBudget","-",version,".nc"),"Nonagland Budget|"),
    list("reportGridManureExcretion(gdx,grid=TRUE,dir=dir,include_emissions=TRUE)",paste0(folder,scenario,"-","NitrogenManure","-",version,".nc"),"Manure Management|")
  )
  
  output <- lapply(X = reporting, FUN=tryReport, gdx=gdx,filter=filter,scenario=scenario)

}
