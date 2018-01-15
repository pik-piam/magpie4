#' @title reportAEI
#' @description reports Area equipped for Irrigation
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Area equipped for Irrigation as MAgPIE object. Unit: see names
#' @author Stephen Wirth
#' @examples
#' 
#'   \dontrun{
#'     x <- reportAEI(gdx)
#'   }
#' 

reportAEI<-function(gdx){
  out = water_AEI(gdx=gdx, level="regglo")
  
 # out<-reporthelper(x=x,dim=3.1,level_zero_name = "Area|Area equipped for irrigation", detail=FALSE)
  getNames(out) <- paste("Resources|Land Cover|Cropland|Area equipped for irrigation","(million ha)",sep=" ")
  return(out)
}