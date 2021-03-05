#' @title reportCostTransport
#' @description reports MAgPIE costs
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return consumption value as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportCostTransport(gdx)
#'   }
#' @importFrom magpiesets reporthelper summationhelper

reportCostTransport<-function(gdx){
  
a <- CostTransport(gdx,level = "regglo",sum = FALSE)

a <- reporthelper(a, dim=3.1, level_zero_name = "Costs|Transport", detail=FALSE)
a <- summationhelper(a)

getNames(a) <- paste0(getNames(a)," (million US$05/yr)")


return(a)

#delete Mainsolve also in magpie4 costs

}