#' @title reportTau
#' @description reports Tau
#'
#' @export
#'
#' @param gdx GDX file
#' @return tau values as MAgPIE object (Index)
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- reportTau(gdx)
#' }
#'
reportTau <- function(gdx) {
  cr <- tau(gdx = gdx, level = "regglo", type = "crop")
  pt <- tau(gdx = gdx, level = "regglo", type = "pastr")
  getNames(pt) <- "Productivity|Landuse Intensity Indicator Tau cropland (Index)"
  getNames(cr) <- "Productivity|Landuse Intensity Indicator Tau managed pastures (Index)"
  out <- mbind(cr, pt)
  return(out)
}
