#' @title cshare
#' @description Calculates soil carbon share in relation to potential natural vegetation based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param reference default is "actual" (cshare in actual carbon stocks). Other option is "target" (cshare in target carbon stocks).
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @return A MAgPIE object containing som values
#' @author Kristine Karstens
#' @examples
#' 
#'   \dontrun{
#'     x <- cshare(gdx)
#'   }
#' 

cshare <- function(gdx, file=NULL, level="reg",  reference="actual", spamfiledirectory=""){
  
  if(level%in%c("cell","reg","glo","regglo")){
    
    # Load som density for specified level
    som_dens           <- SOM(gdx, level=level, type="density", reference=reference, spamfiledirectory=spamfiledirectory)
    
    # Load reference density
    potential_som_dens <- readGDX(gdx, "f59_topsoilc_density")[,getYears(som_dens),]
    
    #################################################################
    ###  disaggregation to various levels                         ###
    #################################################################
    
    if(level%in%c("reg","glo","regglo")){
      
      land <- land(gdx, level="cell")
      land <- mbind(setNames(land[,,"crop"],"cropland"), 
                    setNames(dimSums(land[,,"crop",invert=TRUE], dim=3),"noncropland"), 
                    setNames(dimSums(land, dim=3),"total"))
      
      potential_som_dens  <- mbind(gdxAggregate(gdx, setNames(potential_som_dens,"cropland"),    weight=land[,,"cropland"],    to=level, absolute=FALSE, spamfiledirectory = spamfiledirectory),
                                   gdxAggregate(gdx, setNames(potential_som_dens,"noncropland"), weight=land[,,"noncropland"], to=level, absolute=FALSE, spamfiledirectory = spamfiledirectory),
                                   gdxAggregate(gdx, setNames(potential_som_dens,"total"),       weight=land[,,"total"],       to=level, absolute=FALSE, spamfiledirectory = spamfiledirectory))
      
    } 

    cshare <- som_dens/potential_som_dens
    cshare[is.infinite(cshare)] <- NA
    
  } else if(level=="grid"){
    
    cshare <- gdxAggregate(gdx, cshare(gdx, level="cell",  reference=reference, spamfiledirectory=spamfiledirectory),
                           to="grid", absolute=FALSE, spamfiledirectory=spamfiledirectory)
    
  }
  
  return(cshare)
}