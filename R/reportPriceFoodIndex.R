#' @title reportPriceFoodIndex
#' @description reports food price index
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Food price index as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceFoodIndex(gdx)
#'   }
#' 

reportPriceFoodIndex <- function(gdx){
  # all food products
  x1 <- priceIndex(gdx,level="regglo", products="kfo", baseyear = "y2010")
  getNames(x1) <- "Prices|Food Price Index (Index 2010=100)"
  
  # plant-based food products
  x2 <- priceIndex(gdx,level="regglo", products="kfo_pp", baseyear = "y2010")
  getNames(x2) <- "Prices|Food Price Index|Plant-based food products (Index 2010=100)"
  
  # livestock food products
  x3 <- priceIndex(gdx,level="regglo", products="kfo_lp", baseyear = "y2010")
  getNames(x3) <- "Prices|Food Price Index|Livestock food products (Index 2010=100)"
  
  x <- mbind(x1, x2,x3)
  # x <- summationhelper(x)
  return(x)
}