#' @title reportWaterAvailability
#' @description reports water availability
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return water availability as MAgPIE object Unit: see names
#' @author Felicitas Beier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportWaterAvailability(gdx)
#'   }
#' 

reportWaterAvailability <- function(gdx) {
  x           <- water_avail(gdx, file=NULL, level="regglo", sources=NULL, sum=TRUE, digits=3)
  getNames(x) <- "Resources|Water|Availability|Agriculture (km3/yr)"
  return(x)
}


