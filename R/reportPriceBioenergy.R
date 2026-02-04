#' @title reportPriceBioenergy
#' @description reports bioenergy prices
#'
#' @export
#'
#' @param gdx GDX file
#' @return bioenergy price as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#'
#'   \dontrun{
#'     x <- reportPriceBioenergy(gdx)
#'   }
#'
#'
#' @section Bioenergy price variables:
#' Name | Unit | Meta
#' ---|---|---
#' Prices\|Bioenergy | US$2017/GJ | Bioenergy price
#' @md
reportPriceBioenergy <- function(gdx, level = "regglo") {
  x <- prices(gdx, level = level, products = c("begr", "betr"), product_aggr = TRUE, attributes = "ge")
  getNames(x) <- "Prices|Bioenergy (US$2017/GJ)"
  return(x)
}
