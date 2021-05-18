#' @title reportCostCapitalInvestment
#' @description reports MAgPIE capital investments
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Magpie object associated with overall costs and value of production
#' @author Edna J. Molina Bacca
#' @examples
#' 
#'   \dontrun{
#'     x <- reportCostCapitalInvestment(gdx)
#'   }
#' @importFrom magclass getNames

reportCostCapitalInvestment<-function(gdx){
  
  #Capital stocks used in croland per region 
  x <- CostCapital(gdx,level = "regglo",type="investment")
  getNames(x)<-"Costs|Capital Investments"
  
  
  getNames(x) <- paste0(getNames(x)," (million US$05)")
  
  return(x)
  
  
}