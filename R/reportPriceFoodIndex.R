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
  a <- NULL
  
  x <- priceIndex(gdx,level="regglo", products="kfo", baseyear = "y2010")
  getNames(x) <- "Prices|Food Price Index (Index 2010=100)"
  a <- mbind(a,x)

  x <- priceIndex(gdx,level="regglo", products="kfo_pp", baseyear = "y2010")
  getNames(x) <- "Prices|Food Price Index|Plant-based Products (Index 2010=100)"
  a <- mbind(a,x)
  
  x <- priceIndex(gdx,level="regglo", products="kfo_ap", baseyear = "y2010")
  getNames(x) <- "Prices|Food Price Index|Animal-based Products (Index 2010=100)"
  a <- mbind(a,x)
  
  # x <- summationhelper(x)
  return(a)
}