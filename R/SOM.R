#' @title SOM
#' @description Calculates soil organic carbon stock size based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @return A MAgPIE object containing som values
#' @author Kristine Karstens
#' @examples
#' 
#'   \dontrun{
#'     x <- SOM(gdx)
#'   }
#' 

SOM <- function(gdx, file=NULL, level="reg", spamfiledirectory=""){
  
  som <- readGDX(gdx, "ov59_som_pool", select=list(type="level"))
  som <- mbind(som, setNames(dimSums(som, dim=3), "total"))
  
  if(level=="grid"){
  
    out <- gdxAggregate(gdx, som[,,"total"], to=level, weight="land", sum=TRUE, absolute=TRUE,
                      spamfiledirectory = spamfiledirectory)
    out <- mbind(out, gdxAggregate(gdx, som[,,"cropland"], to=level, weight="land", types="crop" ,absolute=TRUE,
                                 spamfiledirectory = spamfiledirectory))
    out <- mbind(out, gdxAggregate(gdx, som[,,"noncropland"], to=level, weight="land", types=c("past","forestry","urban","other","primforest","secdforest"), sum=TRUE, absolute=TRUE,
                                 spamfiledirectory = spamfiledirectory))
    
  } else if(level%in%c("reg","glo","regglo")){
    
    out <- gdxAggregate(gdx, som, to=level, absolute=TRUE)
  }
  
  return(out)
}