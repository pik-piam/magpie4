#' @title reportWaterUsageLivestock
#' @description reports water usage for livestock only
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return water usage as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportWaterUsageLivestock(gdx)
#'   }
#' 

reportWaterUsageLivestock<-function(gdx){
  x <- water_usage(gdx,level="regglo",users="kli",sum=TRUE,digits=3)
  getNames(x) <- "Resources|Water|Withdrawal|Agriculture|Livestock (km3/yr)"
  return(x)
}