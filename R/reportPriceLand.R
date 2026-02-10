#' @title reportPriceLand
#' @description reports land prices (land rent)
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @return land prices as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#'
#'   \dontrun{
#'     x <- reportPriceLand(gdx)
#'   }
#'
#' @section Land price variables:
#' Name | Unit | Meta
#' ---|---|---
#' Prices\|Land\|Cropland | US$2017/ha | Land rent (shadow price of cropland constraint)
#' @md


reportPriceLand <- function(gdx, level = "regglo") {
  x <- land_price(gdx, level = level, ignore_lowbound = TRUE, absolute = FALSE)
  getNames(x) <- "Prices|Land|Cropland (US$2017/ha)"
  return(x)
}