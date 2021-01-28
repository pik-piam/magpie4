#' @title reportGrowingStock
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
#'     x <- reportGrowingStock(gdx)
#'   }
#' 

reportGrowingStock<-function(gdx,indicator="relative",detail=FALSE){
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    x = GrowingStock(gdx = gdx,level="regglo",indicator=indicator)
    if(indicator == "relative") unit = "(m3/ha)"
    if(indicator == "absolute") unit = "(Mm3)"
    getNames(x) <- paste0("Resources|Growing Stock|",indicator,"|", reportingnames(getNames(x,dim=1)))
    getNames(x) <- gsub(pattern = "natfor",replacement = "Natural Forest",x=getNames(x))
    getNames(x) <- paste(getNames(x),unit,sep=" ")
    x <- summationhelper(x)
    return(x)
  } else {
    message("Disabled for MAgPIE runs without dynamic forestry.")
    return(NULL) 
  }
  
}