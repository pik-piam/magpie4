#' @title reportAAI
#' @description reports area actually irrigated
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @return Area actually irrigated as MAgPIE object. Unit: see names
#' @author Stephen Wirth, Anne Biewald
#' @examples
#' \dontrun{
#' x <- reportAEI(gdx)
#' }
#'
#' @section Area actually irrigated variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover\|Cropland\|Area actually irrigated | million ha | Cropland area actually receiving irrigation
#' @md
reportAAI <- function(gdx, level = "regglo") {
  out <- water_AAI(gdx = gdx, level = level)
  getNames(out) <- paste("Resources|Land Cover|Cropland|Area actually irrigated", "(million ha)", sep = " ")
  return(out)
}
