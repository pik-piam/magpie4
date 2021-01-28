#' @title reportProductionBioenergy
#' @description reports 2nd gen bioenergy production
#' 
#' @import magpiesets 
#' @export
#' 
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return production as MAgPIE object. Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportProductionBioenergy(gdx)
#'   }
#' 

reportProductionBioenergy<-function(gdx,detail=FALSE){
  
  #annual
  x = collapseNames(production(gdx = gdx,level="regglo",products = c("begr","betr"),attributes = "ge",product_aggr = FALSE,water_aggr = TRUE,cumulative = FALSE),collapsedim = "attributes")/1000
  out<-reporthelper(x=x,dim=3.1,level_zero_name = "Production|Bioenergy|2nd generation",detail = detail)
  out <- summationhelper(out,sep="++")
  getNames(out) <- paste(getNames(out),"(EJ/yr)",sep=" ")
  annual <- out
  
  #cumulative
  x = collapseNames(production(gdx = gdx,level="regglo",products = c("begr","betr"),attributes = "ge",product_aggr = FALSE,water_aggr = TRUE,cumulative = TRUE),collapsedim = "attributes")/1000
  out<-reporthelper(x=x,dim=3.1,level_zero_name = "Production|Bioenergy|2nd generation|Cumulative",detail = detail)
  out <- summationhelper(out,sep="++")
  getNames(out) <- paste(getNames(out),"(EJ)",sep=" ")
  cumulative <- out
  
  #combine
  out <- mbind(annual,cumulative)
  
  return(out)
}