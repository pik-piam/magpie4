#' @title reportGrowingStock
#' @description reports Growing stocks for woody materials
#' 
#' @import magpiesets 
#' @export
#' 
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return production as MAgPIE object. Unit: see names
#' @author Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGrowingStock(gdx)
#'   }
#' 

reportGrowingStock<-function(gdx,detail=FALSE){
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    x = GrowingStock(gdx = gdx,level="regglo")
    getNames(x) <- c("Resources|Growing Stock|Managed Forest","Resources|Growing Stock|Secondary Forest","Resources|Growing Stock|Primary Forest","Resources|Growing Stock|Other Land")
    getNames(x) <- paste(getNames(x),"(bio m3/yr)",sep=" ")
    x <- summationhelper(x)
    return(x)
  } else {
    cat("Growing stocks are not reported for current MAgPIE release. WIP.")
    return(NULL)
  }
  
}