#' @title reportTau
#' @description reports Tau
#'
#' @param gdx GDX file
#' @return tau values as MAgPIE object (Index)
#' @author Florian Humpenoeder, Patrick v. Jeetze
#' @examples
#' \dontrun{
#' x <- reportTau(gdx)
#' }
#'
#'
#' @section Tau variables:
#' Name | Unit | Meta
#' ---|---|---
#' Productivity\|Landuse Intensity Indicator Tau | Index | Agricultural land-use intensity indicator for crops
#' @md

#' @export
reportTau <- function(gdx, level = "regglo") {
  cr <- tau(gdx = gdx, level = level, type = "crop")
  getNames(cr) <- "Productivity|Landuse Intensity Indicator Tau (Index)"
  out <- cr
  return(out)
}
