#' @title reportGrowingStockFRA
#' @description reports Growing stocks for woody materials
#' 
#' @import magpiesets 
#' @export
#' 
#' @param gdx GDX file
#' @param indicator If the reported numbers are relative (mio m3/ha) or absolute (mio. m3). Default is relative.
#' @param detail if detail=FALSE, the subcategories of groups are not reported.
#' @return production as MAgPIE object. Unit: see names
#' @author Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGrowingStockFRA(gdx)
#'   }
#' 

reportGrowingStockFRA<-function(gdx,detail=FALSE){
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    x = GrowingStockFRA(gdx = gdx,level="reg")
    unit = "(m3/ha)"
    getNames(x) <- suppressWarnings(paste0("Resources|Growing Stock|","relative","|", reportingnames(getNames(x,dim=1))))
    getNames(x) <- paste(getNames(x),unit,sep=" ")
    x <- summationhelper(x)
    return(x)
  } else {
    message("Disabled for MAgPIE runs without dynamic forestry.")
    return(NULL) 
  }
  
}