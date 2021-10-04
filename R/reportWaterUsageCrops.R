#' @title reportWaterUsageCrops
#' @description reports water usage for crops only
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return water usage as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportWaterUsageCrops(gdx)
#'   }
#' 

reportWaterUsageCrops<-function(gdx){
  x <- water_usage(gdx,level="regglo",users="kcr",sum=TRUE,digits=3)
  getNames(x) <- "Resources|Water|Withdrawal|Agriculture|Crops (km3/yr)"
  return(x)
}