#' @title reportPriceFoodIndex
#' @description reports food price index
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param baseyear baseyear of the price index

#' @return Food price index as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceFoodIndex(gdx)
#'   }
#' 

reportPriceFoodIndex <- function(gdx, baseyear = "y2010"){
  # all food products
  x1 <- priceIndex(gdx,level="regglo", products="kfo", baseyear = baseyear)
  getNames(x1) <- paste0("Prices|Food Price Index (Index ",gsub("\\y","",baseyear),"=100)")
  
  # plant-based food products
  x2 <- priceIndex(gdx,level="regglo", products="kfo_pp", baseyear = baseyear)
  getNames(x2) <- paste0("Prices|Food Price Index|Plant-based food products (Index ",gsub("\\y","",baseyear),"=100)")
  
  # livestock food products
  x3 <- priceIndex(gdx,level="regglo", products="kfo_lp", baseyear = baseyear)
  getNames(x3) <- paste0("Prices|Food Price Index|Livestock food products (Index ",gsub("\\y","",baseyear),"=100)")
  
  x <- mbind(x1, x2,x3)
  # x <- summationhelper(x)
  return(x)
}