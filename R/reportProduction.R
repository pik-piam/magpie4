#' @title reportProduction
#' @description reports production
#' 
#' @import magpiesets 
#' @export
#' 
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return production as MAgPIE object. Unit: see names
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportProduction(gdx)
#'   }
#' 

reportProduction<-function(gdx,detail=FALSE){
  x = production(gdx = gdx,level="regglo",products = readGDX(gdx,"kall"),product_aggr = FALSE,water_aggr = TRUE)
  out<-reporthelper(x=x,dim=3.1,level_zero_name = "Production",detail = detail)
  getNames(out) <- paste(getNames(out),"(Mt DM/yr)",sep=" ")
  out <- summationhelper(out)
  return(out)
}