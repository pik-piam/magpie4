#' @title cshare
#' @description Calculates soil carbon share in relation to potential natural vegetation based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param reference default is "actual" (cshare in actual carbon stocks). Other option is "target" (cshare in target carbon stocks).
#' @param noncrop_aggr aggregate non cropland types to 'noncropland' (if FALSE all land types of pools59 will be reported)
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @return A MAgPIE object containing som values
#' @author Kristine Karstens
#' @examples
#' 
#'   \dontrun{
#'     x <- cshare(gdx)
#'   }
#' 

cshare <- function(gdx, file=NULL, level="reg",  reference="actual", noncrop_aggr=TRUE, dir=".",spamfiledirectory=""){
  
  dir <- getDirectory(dir,spamfiledirectory)
  
  nc59      <- readGDX(gdx, "noncropland59", types="sets", react="silent")
  pools59   <- readGDX(gdx, "pools59", types="sets", react="silent")
  
  if(level%in%c("cell","reg","glo","regglo")){
    
    # Load som density for specified level
    som_dens           <- SOM(gdx, level=level, type="density", reference=reference, noncrop_aggr=noncrop_aggr, dir=dir)
    
    # Load reference density
    potential_som_dens <- readGDX(gdx, "f59_topsoilc_density")[,getYears(som_dens),]
    
    #################################################################
    ###  disaggregation to various levels                         ###
    #################################################################
    
    if(level%in%c("reg","glo","regglo")){
      
      land <- land(gdx, level="cell")
      
      if(noncrop_aggr){
        
        land <- mbind(setNames(land[,,"crop"],"cropland"), 
                      setNames(dimSums(land[,,nc59], dim=3),"noncropland"), 
                      setNames(dimSums(land, dim=3),"total"))
        
        potential_som_dens  <- mbind(gdxAggregate(gdx, setNames(potential_som_dens,"cropland"),    weight=land[,,"cropland"],    to=level, absolute=FALSE, dir = dir),
                                     gdxAggregate(gdx, setNames(potential_som_dens,"noncropland"), weight=land[,,"noncropland"], to=level, absolute=FALSE, dir = dir),
                                     gdxAggregate(gdx, setNames(potential_som_dens,"total"),       weight=land[,,"total"],       to=level, absolute=FALSE, dir = dir))
        
      } else {
        
        land <- mbind(land[,,pools59], setNames(dimSums(land, dim=3),"total"))
        
        potential_som_dens  <- mbind(gdxAggregate(gdx, setNames(potential_som_dens, pools59),    weight=land[,,pools59],    to=level, absolute=FALSE, dir = dir),
                                     gdxAggregate(gdx, setNames(potential_som_dens,"total"),     weight=land[,,"total"],    to=level, absolute=FALSE, dir = dir))
        
      }
    } 

    cshare <- som_dens/potential_som_dens
    cshare[is.infinite(cshare)] <- NA
    
  } else if(level=="grid"){
    
    cshare <- gdxAggregate(gdx, cshare(gdx, level="cell",  reference=reference, dir=dir),
                           to="grid", absolute=FALSE, dir=dir)
    
  }
  
  return(cshare)
}