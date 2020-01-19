#' @title reportProduction
#' @description reports production
#' 
#' @import magpiesets 
#' @export
#' 
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param agmip if agmip=T, additional sector aggregates required for agmip are reported (e.g. "AGR")
#' @return production as MAgPIE object. Unit: see names
#' @author Benjamin Leon Bodirsky, Isabelle Weindl
#' @examples
#' 
#'   \dontrun{
#'     x <- reportProduction(gdx)
#'   }
#' 

reportProduction<-function(gdx,detail=FALSE,agmip=FALSE){
  x = production(gdx = gdx,level="regglo",products = readGDX(gdx,"kall"),product_aggr = FALSE,water_aggr = TRUE)
  out<-reporthelper(x=x,dim=3.1,level_zero_name = "Production",detail = detail)
  out <- summationhelper(out)
  
  if (agmip==T) {
    agr_group <- c("Production|+|Crops","Production|+|Livestock products","Production|+|Pasture") #"Production|+|Forage"
    agr <- dimSums(out[,,agr_group], dim=3.1)
    getNames(agr) <- "Production|Primary agricultural products"
    out <- mbind(out,agr)
  }
  
  getNames(out)[1]<-"Production"
  getNames(out) <- paste(getNames(out),"(Mt DM/yr)",sep=" ")
  return(out)
}