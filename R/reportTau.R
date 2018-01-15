#' @title reportTau
#' @description reports Tau
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return tau values as MAgPIE object (Index)
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportTau(gdx)
#'   }
#' 

reportTau <- function(gdx) {
  out = tau(gdx = gdx,level="regglo")
  getNames(out) <- "Productivity|Landuse Intensity Indicator Tau (Index)"
  return(out)
}

