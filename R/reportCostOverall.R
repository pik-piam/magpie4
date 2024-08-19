#' @title reportCostOverall
#' @description reports MAgPIE costs
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Magpie object associated with overall costs and value of production
#' @author Edna J. Molina Bacca
#' @examples
#' 
#'   \dontrun{
#'     x <- reportCostOverall(gdx)
#'   }
#' @importFrom magclass getNames

reportCostOverall<-function(gdx){
  
  #Gross Value of production
  x <- CostOverall(gdx,level = "regglo")
  getNames(x)<-"Costs|Gross value of production"
  
  
  getNames(x) <- paste0(getNames(x)," (million US$17/yr)")
  
  return(x)
  

}