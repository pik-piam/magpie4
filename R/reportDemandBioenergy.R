#' @title reportDemandBioenergy
#' @description reports Bioenergy Demand in EJ/yr
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Bioenergy demand as MAgPIE object (EJ/yr)
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportDemandBioenergy()
#'   }
#' 

reportDemandBioenergy <- function(gdx){
  out = demandBioenergy(gdx,level="regglo")
  y <- dimSums(out, dim = 3)
  getNames(out) <- paste0("Demand|Bioenergy|++|",getNames(out)," (EJ/yr)")
  getNames(y) <- "Demand|Bioenergy (EJ/yr)"
  out <- mbind(out, y)
  return(out)
}