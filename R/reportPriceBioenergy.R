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

reportPriceBioenergy<-function(gdx){
  x <- prices(gdx,level="regglo",products = c("begr","betr"),product_aggr = TRUE,attributes = "ge")/1000
  x <- x*(1/0.967) #USD2004 -> USD2005
  getNames(x) <- "Prices|Bioenergy (US$05/GJ)"
  
  return(x)
}