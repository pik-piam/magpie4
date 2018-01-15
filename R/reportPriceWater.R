#' @title reportPriceWater
#' @description reports water prices
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return water usage as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceWater(gdx)
#'   }
#' 

reportPriceWater<-function(gdx){
  x <- water_price(gdx,level="regglo",index=TRUE,index_baseyear = 2005,digits = 2)
  getNames(x) <- "Prices|Water|Agriculture (Index 2005=100)"
  
  return(x)
}