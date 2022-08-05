#' @title reportAAI
#' @description reports area actually irrigated
#'
#' @export
#'
#' @param gdx GDX file
#' @return Area actually irrigated as MAgPIE object. Unit: see names
#' @author Stephen Wirth, Anne Biewald
#' @examples
#' \dontrun{
#' x <- reportAEI(gdx)
#' }
#'
reportAAI <- function(gdx) {

  out <- water_AAI(gdx = gdx, level = "regglo")

#  out<-reporthelper(x=x, dim=3.1,level_zero_name = "Area|Area actually irrigated", detail=FALSE)
  getNames(out) <- paste("Resources|Land Cover|Cropland|Area actually irrigated", "(million ha)", sep = " ")
  return(out)
}
