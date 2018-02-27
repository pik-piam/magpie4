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
  x <- priceIndex(gdx,level="regglo", products="kfo")
  getNames(x) <- "Prices|Food Price Index (Index 2005=100)"
  # x <- summationhelper(x)
  return(x)
}