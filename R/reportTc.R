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
  pt = NULL
  tau <- readGDX(gdx, "ov_tau", format = "first_found")[, , "level"]
  if(any(grepl("pastr",getItems(tau, dim = 3)))) {
    pt = tc(gdx = gdx,level="regglo", type = "pastr")
    getNames(pt) <- "Productivity|Yield-increasing technological change managed pastures (%/yr)"
  }
  cr = tc(gdx = gdx,level="regglo", type = "crop")
  getNames(cr) <- "Productivity|Yield-increasing technological change crops (%/yr)"
  out <- mbind(pt,cr)
  return(out)
}

