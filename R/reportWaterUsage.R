#' @title reportWaterUsage
#' @description reports water usage
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return water usage as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportWaterUsage(gdx)
#'   }
#' 

reportWaterUsage<-function(gdx){
  x <- water_usage(gdx,level="regglo",users="kcr",sum=TRUE,digits=3)
  getNames(x) <- "Resources|Water|Withdrawal|Agriculture (km3/yr)"
  return(x)
}