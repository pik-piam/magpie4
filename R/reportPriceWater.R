#' @title reportPriceWater
#' @description reports water prices
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @return water usage as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- reportPriceWater(gdx)
#' }
#'
#' @section Water price variables:
#' Name | Unit | Meta
#' ---|---|---
#' Prices\|Water\|Agriculture | Index 2005=100 | Agricultural water price index
#' @md

#'
reportPriceWater <- function(gdx, level = "regglo") {
  x <- water_price(gdx, level = level,
                   index = TRUE, index_baseyear = 2005, digits = 2)
  getNames(x) <- "Prices|Water|Agriculture (Index 2005=100)"

  return(x)
}
