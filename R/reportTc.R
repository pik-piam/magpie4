#' @title reportTc
#' @description reports Tc
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return tc values as MAgPIE object (%/yr)
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportTc(gdx)
#'   }
#' 

reportTc <- function(gdx) {
  cr = tc(gdx = gdx,level="regglo", type = "crop")
  pt = tc(gdx = gdx,level="regglo", type = "pastr")
  getNames(cr) <- "Productivity|Yield-increasing technological change crops (%/yr)"
  getNames(pt) <- "Productivity|Yield-increasing technological change managed pastures (%/yr)"
  out <- mbind(pt,cr)
  return(out)
}

