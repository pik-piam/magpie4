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
  out = tc(gdx = gdx,level="regglo")
  getNames(out) <- "Productivity|Yield-increasing technological change (%/yr)"
  return(out)
}

