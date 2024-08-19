#' @title reportPriceLand
#' @description reports land prices (land rent)
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return land prices as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceLand(gdx)
#'   }
#' 

reportPriceLand<-function(gdx){
  x <- land_price(gdx, level="regglo", ignore_lowbound=TRUE, absolute=FALSE)
  getNames(x) <- "Prices|Land|Cropland (US$17/ha)"
  
  return(x)
}