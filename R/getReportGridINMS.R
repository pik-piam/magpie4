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
#' @param versionnr versionnumber for the run names
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

getReportGridINMS <- function(gdx,folder=NULL,scenario=NULL,filter=c(2,7),dir=".",spamfiledirectory="",versionnr="v9",...) {
  
  dir <- getDirectory(dir,spamfiledirectory)
  
  tryReport <- function(reporting, gdx,filter,scenario) {
    
    report = reporting[[1]]
    file=reporting[[2]]
    filename=paste0(folder,"INMS_output-MAgPIE4-",scenario,"-",file,"-",versionnr,".nc")
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
      
      mapping=paste0("mapping_inms_",file,".csv")
      getNames(x)=paste0(category,getNames(x))

      y = RenameAndAggregate(data = list(model = list(scenario = x)),mapping = mapping,missing_log = NULL)
      y = y[[1]][[1]]
      y=y[,,dimnames(y)[[3]][dimSums(as.magpie((!is.na(y))*1),dim=c(1,2))>0]]

      if(!is.null(filename)) write.magpie(y,file_name = filename)
    }
  }
  
  
  message("Start getReport(gdx)...")
  
  reporting= list(
    list("reportGridLand(gdx,dir=dir)", file="LandCover","Land Cover|"),
    list("reportNitrogenBudgetCropland(gdx,grid=TRUE,dir=dir,include_emissions=TRUE)",file="Nitrogen_CroplandBudget","Cropland Budget|"),
    list("reportNitrogenBudgetPasture(gdx,grid=TRUE,dir=dir,include_emissions=TRUE)",file="Nitrogen_PastureBudget","Pasture Budget|"),
    list("reportNitrogenBudgetNonagland(gdx,grid=TRUE,dir=dir)",file="Nitrogen_NonAgriculturalLandBudget","Nonagland Budget|"),
    list("reportGridManureExcretion(gdx,dir=dir)",file="Nitrogen_Manure","")
  )
  
  output <- lapply(X = reporting, FUN=tryReport, gdx=gdx,filter=filter,scenario=scenario)

}
