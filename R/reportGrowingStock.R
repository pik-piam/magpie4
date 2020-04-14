#' @title reportGrowingStock
#' @description reports Growing stocks for woody materials
#' 
#' @import magpiesets 
#' @export
#' 
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported.
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
    getNames(x) <- paste0("Resources|Growing Stock|", reportingnames(getNames(x,dim=1)))
    getNames(x) <- paste(getNames(x),"(m3/ha)",sep=" ")
    x <- summationhelper(x)
    return(x)
  } else {
    message("Disabled for MAgPIE runs without dynamic forestry.")
    return(NULL) 
  }
  
}