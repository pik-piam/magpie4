#' @title reportPriceAgriculture
#' @description reports food commodity prices
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return agricultural commodity prices as MAgPIE object (USD)
#' @author Mishko Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceAgriculture(gdx)
#'   }
#' 
#'
#' @section Agricultural price variables:
#' Name | Unit | Meta
#' ---|---|---
#' Prices\|Agriculture\|Crops | US$2017/tDM | Prices for crop products
#' Prices\|Agriculture\|Livestock products | US$2017/tDM | Prices for livestock products
#' @md

#' @importFrom magpiesets reportingnames

reportPriceAgriculture <- function(gdx){
  out <- prices(gdx, level="regglo", products="kall")
  out <- setNames(out, paste0("Prices|Agriculture|",reportingnames(getNames(out))," (US$2017/tDM)"))
  
  return(out)
}