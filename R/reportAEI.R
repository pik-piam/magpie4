#' @title reportAEI
#' @description reports Area equipped for Irrigation
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @return Area equipped for Irrigation as MAgPIE object. Unit: see names
#' @author Stephen Wirth
#' @examples
#'
#'   \dontrun{
#'     x <- reportAEI(gdx)
#'   }
#'
#' @section Area equipped for irrigation variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover\|Cropland\|Area equipped for irrigation | million ha | Cropland area equipped with irrigation infrastructure
#' @md
reportAEI <- function(gdx, level = "regglo") {
  out <- water_AEI(gdx = gdx, level = level)
  getNames(out) <- paste("Resources|Land Cover|Cropland|Area equipped for irrigation", "(million ha)", sep = " ")
  return(out)
}