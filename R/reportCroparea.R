#' @title reportCroparea
#' @description reports croparea
#' 
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#' 
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return Croparea as MAgPIE object (million Ha/yr)
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportCroparea(gdx)
#'   }
#' 

reportCroparea <- function(gdx,detail=FALSE) {
  x <- NULL

  out <- croparea(gdx,level="regglo",products="kcr",product_aggr=FALSE, water_aggr=TRUE)
  out<-reporthelper(x=out,dim=3.1,level_zero_name = "Resources|Land Cover|Cropland",detail = detail)
  getNames(out) <- paste(gsub("\\.","|",getNames(out)),"(million ha)",sep=" ")
  out <- summationhelper(out,sep = "+")
  x <- mbind(x,out)
  
  out <- croparea(gdx,level="regglo",products="kcr",product_aggr=FALSE, water_aggr=FALSE)
  out<-reporthelper(x=out,dim=3.1,level_zero_name = "Resources|Land Cover|Cropland",detail = detail)
  getNames(out) <- paste(gsub("\\.","|",getNames(out)),"(million ha)",sep=" ")
  out <- summationhelper(out, sep = NULL)
  x <- mbind(x,out)
  
  #x <- x[,,sort(getNames(x))]  
  return(x)
}

