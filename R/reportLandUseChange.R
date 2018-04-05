#' @title reportLandUseChange
#' @description reports land-use change
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param baseyear baseyear for calculating land-use change
#' @return land-use change as MAgPIE object (million ha wrt to baseyear)
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportLandUseChange(gdx)
#'   }
#' 

reportLandUseChange <- function(gdx,baseyear=1995) {
  
  #get LandUse
  x <- reportLandUse(gdx)
  
  #drop variables
  x <- x[,,"Resources|Land Cover (million ha)",invert=TRUE]
  
  #calc land-use change wrt to baseyear
  x <- x - setYears(x[,baseyear,],NULL)
  
  #rename variable and unit
  getNames(x) <- gsub("\\|Land Cover\\|","\\|Land Cover Change\\|",getNames(x))
  getNames(x) <- gsub("\\(million ha\\)",paste0("\\(million ha wrt ",baseyear,"\\)"),getNames(x))
  
  return(x)
}

